#include "erl_nif.h"

#include "gdalwarper.h"
#include "ogr_srs_api.h"
#include "gdal.h"
#include "cpl_conv.h"
#include "cpl_string.h"
#include "cpl_error.h"

static ErlNifResourceType* gdal_nif_RESOURCE = NULL;

typedef struct
{
} gdal_nif_handle;

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_OK;

// Prototypes
static ERL_NIF_TERM gdal_nif_build_out_ds_srs_wkt(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gdal_nif_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"build_out_ds_srs_wkt", 1, gdal_nif_build_out_ds_srs_wkt},
    {"myfunction", 1, gdal_nif_myfunction}
};

static ERL_NIF_TERM gdal_nif_build_out_ds_srs_wkt(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    int epsg_code = 4326;
    if (enif_get_int(env, argv[0], &epsg_code)) {
        OGRSpatialReferenceH out_srs = OSRNewSpatialReference(NULL);
        OSRImportFromEPSG(out_srs, epsg_code);
        char* out_srs_wkt;  // TODO: should be free by OGRFree or CPLFree
        OSRExportToWkt(out_srs, &out_srs_wkt);
        return enif_make_tuple2(env, 
            ATOM_OK,
            enif_make_string(env, out_srs_wkt, ERL_NIF_LATIN1));
    }
    else {
        return enif_make_badarg(env);
    }
}


static ERL_NIF_TERM gdal_nif_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    gdal_nif_handle* handle = enif_alloc_resource(gdal_nif_RESOURCE,
                                                    sizeof(gdal_nif_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static void gdal_nif_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in gdal_nif_handle */
    /* gdal_nif_handle* handle = (gdal_nif_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "gdal_nif_resource",
                                                     &gdal_nif_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    gdal_nif_RESOURCE = rt;

    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_OK = enif_make_atom(env, "ok");

    return 0;
}

ERL_NIF_INIT(gdal_nif, nif_funcs, &on_load, NULL, NULL, NULL);
