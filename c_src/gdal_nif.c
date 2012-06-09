#include "erl_nif.h"

#include "gdalwarper.h"
#include "ogr_srs_api.h"
#include "gdal.h"
#include "cpl_conv.h"
#include "cpl_string.h"
#include "cpl_error.h"

#include "gdal_nif_util.h"

static ErlNifResourceType* gdal_img_RESOURCE = NULL;
static ErlNifResourceType* gdal_tile_RESOURCE = NULL;

typedef struct
{
    GDALDatasetH in_ds;     // Just used to keep and close the original image
    GDALDatasetH out_ds;    // the VRT dataset which warped in_ds for tile projection
    GDALRasterBandH alphaBand;

    int querysize;
    int tilesize;
    int dataBandsCount;
    int tilebands;

    const char* options_resampling;
} gdal_img_handle;

typedef struct
{
    int xoffset;
    int yoffset;
    int xsize;
    int ysize;
} bandregion;

typedef struct
{
    GDALDatasetH dstile;
    bandregion w;
    
    int querysize;
    int tilesize;
    
    int dataBandsCount;
    int tilebands;
    
    const char* options_resampling;

    GByte* data;
    GByte* alpha;
} gdal_tile_handle;

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_NOT_OPEN;

// Prototypes
static ERL_NIF_TERM gdal_nif_build_out_ds_srs_wkt(ErlNifEnv* env, int argc,
                                                  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nif_create_warped_vrt(ErlNifEnv* env, int argc,
                                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nif_close_img(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nif_copyout_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nif_build_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nif_save_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nif_get_meta(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]);


static void destroy_img_handle(gdal_img_handle* handle);
static void gdal_nif_img_resource_cleanup(ErlNifEnv* env, void* arg);
static void gdal_nif_tile_resource_cleanup(ErlNifEnv* env, void* arg);
static void free_temp_data(gdal_tile_handle* hTile);
static ERL_NIF_TERM make_error_msg(ErlNifEnv* env, const char* msg);

static ErlNifFunc nif_funcs[] =
{
    {"build_out_ds_srs_wkt", 1, gdal_nif_build_out_ds_srs_wkt},
    {"create_warped_vrt", 2, gdal_nif_create_warped_vrt},
    {"close_img", 1, gdal_nif_close_img},
    {"copyout_tile", 3, gdal_nif_copyout_tile},
    {"build_tile", 1, gdal_nif_build_tile},
    {"save_tile", 2, gdal_nif_save_tile},
    {"get_meta", 1, gdal_nif_get_meta}
};

// returned WKT string should be free by OGRFree or CPLFree
static char* build_srs_wkt_for(int epsg_code) {
    OGRSpatialReferenceH out_srs = OSRNewSpatialReference(NULL);
    OSRImportFromEPSG(out_srs, epsg_code);
    char* out_srs_wkt;
    OSRExportToWkt(out_srs, &out_srs_wkt);
    return out_srs_wkt;
}

static int get_bandregion_from(ErlNifEnv* env, const ERL_NIF_TERM *pterm, bandregion* pbr) 
{
    int rarity;
    const ERL_NIF_TERM* r;
    int res = enif_get_tuple(env, *pterm, &rarity, &r);
    if (res) {
        enif_get_int(env, r[0], &(pbr->xoffset));
        enif_get_int(env, r[1], &(pbr->yoffset));
        enif_get_int(env, r[2], &(pbr->xsize));
        enif_get_int(env, r[3], &(pbr->ysize));
        //DEBUG("rx=%d, ry=%d, rxsize=%d, rysize=%d\r\n", rx, ry, rxsize, rysize);
    }
    return res;
}

static ERL_NIF_TERM gdal_nif_build_out_ds_srs_wkt(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    int epsg_code;
    if (enif_get_int(env, argv[0], &epsg_code)) {
        char* wkt = build_srs_wkt_for(epsg_code);
        ERL_NIF_TERM res = enif_make_tuple2(env, 
                ATOM_OK,
                enif_make_string(env, wkt ,ERL_NIF_LATIN1));
        OGRFree(wkt);
        return res;
    }
    else {
        return enif_make_badarg(env);
    }
}


// build out_ds
static ERL_NIF_TERM gdal_nif_create_warped_vrt(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    char name[4096];
    size_t name_sz;
    int epsg_code;
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1) &&
        enif_get_int(env, argv[1], &epsg_code)) {
        name_sz = strlen(name);

        GDALDatasetH in_ds = GDALOpenShared(name, GA_ReadOnly);
        if (in_ds != NULL) {
            gdal_img_handle* handle = enif_alloc_resource(
                                                    gdal_img_RESOURCE, 
                                                    sizeof(gdal_img_handle));
            memset(handle, '\0', sizeof(*handle));
            handle->in_ds = in_ds;
            handle->options_resampling = "average";
            handle->querysize = 256 * 4;
            handle->tilesize = 256;

            int rasterCount = GDALGetRasterCount(in_ds);
            if (rasterCount == 0) {
                destroy_img_handle(handle);

                const char* msg = "Input file '%s' has no raster band";
                char errstr[name_sz + strlen(msg) + 1];
                sprintf(errstr, msg, name);
                return make_error_msg(env, errstr);
            }

            GDALRasterBandH hBand = GDALGetRasterBand(in_ds, 1);
            if (GDALGetRasterColorTable(hBand) != NULL) {
                const char* msg = 
                    "Please convert this file to RGB/RGBA and run gdal2tiles on the result.\n" 
                    "From paletted file you can create RGBA file (temp.vrt) by:\n"
                    "gdal_translate -of vrt -expand rgba %s temp.vrt\n"
                    "then run this program: gdal2tiles temp.vrt";
                char errstr[name_sz + strlen(msg) + 1];
                sprintf(errstr, msg, name);
                return make_error_msg(env, errstr);
            }

            double padfTransform[6];
            double errTransform[6] = {0.0, 1.0, 0.0, 0.0, 0.0, 1.0};
            GDALGetGeoTransform(in_ds, padfTransform);
            if (0 == memcmp(padfTransform, errTransform, sizeof(errTransform))
                     && GDALGetGCPCount(in_ds) == 0) {
                return make_error_msg(env, 
                                      "There is no georeference - "
                                      "neither affine transformation (worldfile) nor GCPs");
            }

            const char* in_srs_wkt = GDALGetProjectionRef(in_ds);
            if (in_srs_wkt == NULL && GDALGetGCPCount(in_ds) != 0) {
                in_srs_wkt = GDALGetGCPProjection(in_ds);
            }
            char* out_srs_wkt = build_srs_wkt_for(epsg_code);
            GDALDatasetH out_ds = GDALAutoCreateWarpedVRT(in_ds, 
                                                          in_srs_wkt, 
                                                          out_srs_wkt, 
                                                          GRA_NearestNeighbour, 
                                                          0.0, 
                                                          NULL);
            handle->out_ds = out_ds;
            OGRFree(out_srs_wkt);


            handle->alphaBand = GDALGetMaskBand(GDALGetRasterBand(handle->out_ds, 1));
            rasterCount = GDALGetRasterCount(handle->out_ds);
            unsigned int dataBandsCount;
            if (GDALGetMaskFlags(handle->alphaBand) & GMF_ALPHA || 
                    rasterCount == 4 || rasterCount == 2) {
                dataBandsCount = rasterCount - 1;
            }
            else {
                dataBandsCount = rasterCount;
            }
            handle->dataBandsCount = dataBandsCount;
            handle->tilebands = dataBandsCount + 1;


            ERL_NIF_TERM res = enif_make_resource(env, handle);
            enif_release_resource(handle);

            return enif_make_tuple2(env, ATOM_OK, res);
        }
        else {
            const char* msg = "It is not possible to open the input file '%s'.";
            char errstr[name_sz + strlen(msg) + 1];
            sprintf(errstr, msg, name);
            return make_error_msg(env, errstr);
        }
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM gdal_nif_copyout_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GDALDatasetH ds = NULL;
    gdal_img_handle* hImg = NULL;
    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&hImg)) {
        ds = hImg->out_ds;
    }
    else {
        return enif_make_badarg(env);
    }
        
    bandregion r, w;
    if (!get_bandregion_from(env, argv + 1, &r) || !get_bandregion_from(env, argv + 2, &w)) {
        return enif_make_badarg(env);
    }

    gdal_tile_handle* hTile = enif_alloc_resource(gdal_tile_RESOURCE, sizeof(*hTile));
    *hTile = (gdal_tile_handle) {
        .dstile = NULL,
        .data = NULL,
        .alpha = NULL,
        .w = w,
        .querysize = hImg->querysize,
        .tilesize = hImg->tilesize,
        .dataBandsCount = hImg->dataBandsCount,
        .tilebands = hImg->tilebands,
        .options_resampling = hImg->options_resampling
    };

    ERL_NIF_TERM res = enif_make_resource(env, hTile);
    enif_release_resource(hTile);  // hTile resource now only owned by "Erlang"

    GDALDriverH hMemDriver = GDALGetDriverByName("MEM");
    hTile->dstile = GDALCreate(hMemDriver, "",
                               hImg->tilesize, hImg->tilesize, hImg->tilebands, 
                               GDT_Byte, NULL);
    // read dataset data
    hTile->data = (GByte*)CPLCalloc(w.xsize * w.ysize * hImg->dataBandsCount, sizeof(*hTile->data));

    int panBandMap[hImg->dataBandsCount];
    fill_pband_list(hImg->dataBandsCount, panBandMap);
    CPLErr eErr = GDALDatasetRasterIO(ds, GF_Read, 
                              r.xoffset, r.yoffset, r.xsize, r.ysize, hTile->data, 
                              w.xsize, w.ysize, GDT_Byte, hImg->dataBandsCount, panBandMap, 
                              0, 0, 0);
    if (eErr == CE_Failure) {
//        free_tile(hTile);
        char buf[128] = "DatasetRasterIO read failed: ";
        const char* errmsg = CPLGetLastErrorMsg();
        strncat(buf, errmsg, strlen(errmsg));
        return make_error_msg(env, errmsg);
    }

    // read dataset alpha 
    hTile->alpha = (GByte*)CPLCalloc(w.xsize * w.ysize, sizeof(*hTile->alpha));
    eErr = GDALRasterIO(hImg->alphaBand, GF_Read, 
                        r.xoffset, r.yoffset, r.xsize, r.ysize, 
                        hTile->alpha, w.xsize, w.ysize, 
                        GDT_Byte, 0, 0);
    if (eErr == CE_Failure) {
//        free_tile(hTile);
        char buf[128] = "DatasetRasterIO read failed: ";
        const char* errmsg = CPLGetLastErrorMsg();
        strncat(buf, errmsg, strlen(errmsg));
        return make_error_msg(env, errmsg);
    }

    return enif_make_tuple2(env, ATOM_OK, res);
}

static ERL_NIF_TERM gdal_nif_build_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_tile_handle* hTile;
    if (!enif_get_resource(env, argv[0], gdal_tile_RESOURCE, (void**)&hTile)) {
        return enif_make_badarg(env);
    }

    GDALDatasetH dstile = hTile->dstile;
    GByte* data = hTile->data;
    GByte* alpha = hTile->alpha;
    bandregion w = hTile->w;

    CPLErr eErr = CE_None;
    if (hTile->tilesize == hTile->querysize) {
        int xoffset = w.xoffset;
        int yoffset = w.yoffset;
        int xsize = w.xsize;
        int ysize = w.ysize;
        int dataBandsCount = hTile->dataBandsCount;
        int tilebands = hTile->tilebands;
        eErr = write_data_and_alpha_to_raster(dstile, 
                                              xoffset, yoffset, xsize, ysize, 
                                              data, alpha, dataBandsCount, tilebands);
        if (eErr == CE_Failure) {
            return make_error_msg(env, "write_data_and_alpha_to_raster");
        }
        // Note: For source drivers based on WaveLet compression (JPEG2000, ECW, MrSID)
        // the ReadRaster function returns high-quality raster (not ugly nearest neighbour)
        // TODO: Use directly 'near' for WaveLet files
    }
    else {
        GDALDriverH hMemDriver = GDALGetDriverByName("MEM");
        GDALDatasetH dsquery = GDALCreate(hMemDriver, "", 
                                          hTile->querysize, hTile->querysize, hTile->tilebands, 
                                          GDT_Byte, NULL);
        if (dsquery == NULL) {
            return make_error_msg(env, "create dsquery");
        }

        int xoffset = w.xoffset;
        int yoffset = w.yoffset;
        int xsize = w.xsize;
        int ysize = w.ysize;
        int dataBandsCount = hTile->dataBandsCount;
        int tilebands = hTile->tilebands;
        eErr = write_data_and_alpha_to_raster(dsquery, xoffset, yoffset, xsize, ysize, data, alpha, dataBandsCount, tilebands);
        if (eErr == CE_Failure) {
            GDALClose(dsquery);
            return make_error_msg(env, "write data and alpha to raster");
        }

        CPLErrorReset();
        eErr = scale_query_to_tile(dsquery, dstile, hTile->options_resampling);
        GDALClose(dsquery);
        if (eErr == CE_Failure) {
            return make_error_msg(env, "scale_query_to_tile");
        }
    }

    free_temp_data(hTile);

    return ATOM_OK;
}

static ERL_NIF_TERM gdal_nif_save_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_tile_handle* ti;
    if (!enif_get_resource(env, argv[0], gdal_tile_RESOURCE, (void**)&ti)) {
        return enif_make_badarg(env);
    }
    
    char tilefilename[256] = "";
    if (enif_get_string(env, argv[1], tilefilename, 256, ERL_NIF_LATIN1) <= 0) {
        return enif_make_badarg(env);
    }

    GDALDriverH hOutDriver = GDALGetDriverByName("PNG");
    if ( ! ti->options_resampling || (strcmp("antialias", ti->options_resampling) != 0) ) {
        GDALDatasetH tileDataset = GDALCreateCopy(hOutDriver,
                                                  tilefilename, ti->dstile, 
                                                  FALSE, NULL, NULL, NULL);
        GDALClose(tileDataset);
    }

    return ATOM_OK;
}

static ERL_NIF_TERM gdal_nif_close_img(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_img_handle* handle = NULL;
    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&handle)) {
        gdal_nif_img_resource_cleanup(env, handle);
        return ATOM_OK;
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM gdal_nif_get_meta(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gdal_img_handle* handle;
    if (enif_get_resource(env, argv[0], gdal_img_RESOURCE, (void**)&handle)) {
        GDALDatasetH in_ds = handle->in_ds;
        if (in_ds != NULL) {
            ERL_NIF_TERM terms[8];
            int idx = 0;

            terms[idx++] = enif_make_tuple2(env,
                                enif_make_atom(env, "description"),
                                enif_make_string(env, GDALGetDescription(in_ds), ERL_NIF_LATIN1));

            GDALDriverH hDriver = GDALGetDatasetDriver(in_ds);
            char buf[256];
            sprintf(buf, "%s/%s", 
                    GDALGetDriverShortName(hDriver), GDALGetDriverLongName(hDriver));
            terms[idx++] = enif_make_tuple2(env, 
                                enif_make_atom(env, "driver"),
                                enif_make_string(env, buf, ERL_NIF_LATIN1));

            terms[idx++] = enif_make_tuple2(env, 
                                enif_make_atom(env, "rasterSize"), 
                                enif_make_tuple2(env, 
                                    enif_make_int(env, GDALGetRasterXSize(in_ds)), 
                                    enif_make_int(env, GDALGetRasterYSize(in_ds))));

            terms[idx++] = enif_make_tuple2(env, 
                                    enif_make_atom(env, "rasterCount"),
                                    enif_make_int(env, GDALGetRasterCount(in_ds)));

            double adfGeoTransform[6];
            if( GDALGetGeoTransform( in_ds, adfGeoTransform ) == CE_None ) {
                terms[idx++] = enif_make_tuple2(env,
                                    enif_make_atom(env, "origin"),
                                    enif_make_tuple2(env,
                                        enif_make_double(env, adfGeoTransform[0]), 
                                        enif_make_double(env, adfGeoTransform[3])));

                terms[idx++] = enif_make_tuple2(env,
                                    enif_make_atom(env, "pixelSize"), 
                                    enif_make_tuple2(env, 
                                        enif_make_double(env, adfGeoTransform[1]), 
                                        enif_make_double(env, adfGeoTransform[5])));
            }

            if (GDALGetProjectionRef(in_ds) != NULL) {
                terms[idx++] = enif_make_tuple2(env,
                                    enif_make_atom(env, "projection"), 
                                    enif_make_string(env, 
                                        GDALGetProjectionRef(in_ds), ERL_NIF_LATIN1));
}

            char** fileList = GDALGetFileList(in_ds);
            if (fileList != NULL) {
                ERL_NIF_TERM fileTerms[16];
                int fileIdx = 0;
                char** files = fileList;

                do {
                    fileTerms[ fileIdx++ ] = enif_make_string(env, *files, ERL_NIF_LATIN1);
                } while(*(++files)) ;
                CSLDestroy(fileList);

                terms[idx++] = enif_make_tuple2(env,
                                    enif_make_atom(env, "fileList"),
                                    enif_make_list_from_array(env, fileTerms, fileIdx));
            }

            return enif_make_list_from_array(env, terms, idx);
        }
        else {
            return ATOM_NOT_OPEN;
        }
    }
    else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM make_error_msg(ErlNifEnv* env, const char* msg) {
    return enif_make_tuple2(env, 
                            ATOM_ERROR, 
                            enif_make_string(env, msg, ERL_NIF_LATIN1));
}

static void free_img(gdal_img_handle* handle) {
    if (handle == NULL) {
        return;
    }

    if (handle->out_ds != NULL) {
        GDALClose(handle->out_ds);
        handle->out_ds = NULL;
    }
    if (handle->in_ds != NULL) {
        GDALClose(handle->in_ds);
        handle->in_ds = NULL;
    }
}

// free temp data & alpha binary
static void free_temp_data(gdal_tile_handle* hTile)
{
    if (hTile && hTile->data != NULL) {
        CPLFree(hTile->data);
        hTile->data = NULL;
    }
    if (hTile && hTile->alpha != NULL) {
        CPLFree(hTile->alpha);
        hTile->alpha = NULL;
    }
}

static void free_tile(gdal_tile_handle* hTile) 
{
    if (hTile && hTile->dstile != NULL) {
        GDALClose(hTile->dstile);
        hTile->dstile = NULL;
    }
    
    free_temp_data(hTile);
}
static void destroy_img_handle(gdal_img_handle* handle) {
    free_img(handle);
    enif_release_resource(handle);
}

static void gdal_nif_img_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in gdal_img_handle */
    gdal_img_handle* handle = (gdal_img_handle*)arg;
    free_img(handle);
}

static void gdal_nif_tile_resource_cleanup(ErlNifEnv* env, void* arg)
{
    gdal_tile_handle* ti = (gdal_tile_handle*)arg;
    free_tile(ti);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL, "gdal_img_resource",
                                                     &gdal_nif_img_resource_cleanup,
                                                     flags, NULL);

    if (rt == NULL)
        return -1;

    gdal_img_RESOURCE = rt;

    rt = enif_open_resource_type(env, NULL, "gdal_tile_resource",
                                 &gdal_nif_tile_resource_cleanup,
                                 flags, NULL);

    if (rt == NULL)
        return -1;

    gdal_tile_RESOURCE = rt;

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_NOT_OPEN = enif_make_atom(env, "not_open");

    // Register all known configured GDAL drivers
    GDALAllRegister();

    return 0;
}

ERL_NIF_INIT(gdal_nif, nif_funcs, &on_load, NULL, NULL, NULL);
