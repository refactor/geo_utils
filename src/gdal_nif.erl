-module(gdal_nif).

-export([get_srs_wkt_of/1,
         get_meta/1,
         create_warped_vrtimg/2,
         close_img/1]).

-export([copyout_rawtile/3,
         build_tile/1,
         tile_to_binary/3,
         save_tile/2]).

-export_type([rawtile/0, 
              tile/0,
              img/0]).

-on_load(init/0).

-type rawtile() :: reference().
-type tile()    :: reference().
-type img()     :: reference().

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

-spec get_srs_wkt_of(EpspCode::non_neg_integer()) -> {ok, string()} | {error, string()}.
get_srs_wkt_of(_EpsgCode) ->
    ?nif_stub.

%% @doc build out_ds in global world of EPSP:Code projection
-spec create_warped_vrtimg(ImgFileName::iolist(), VrtEpsgCode::non_neg_integer()) ->
    {ok, img(), tile_grid:img_info()} | {error, string()}.
create_warped_vrtimg(_ImgFileName, _VrtEpsgCode) ->
    ?nif_stub.

-spec close_img(Img::img()) -> ok | {error, string()}.
close_img(_Img) ->
    ?nif_stub.

-spec get_meta(Img::img()) -> any().
get_meta(_ImgRef) ->
    ?nif_stub.

%% @doc just copy the raw data out from img, which prepare for building a tile
-spec copyout_rawtile(img(), tile_grid:bandregion(), tile_grid:bandregion()) -> 
    {ok, rawtile()} | {error, string()}.
copyout_rawtile(_Img, _R, _W) ->
    ?nif_stub.

%% @doc build a tile Memory GDALDataset from raw data
-spec build_tile(RawTile::rawtile()) -> {ok, tile()} | {error, string()}.
build_tile(_RawTile) ->
    ?nif_stub.

%% @doc Depracated
-spec save_tile(Tile::tile(), TileFileName::string()) -> ok | {error, string()}.
save_tile(_Tile, _TileFileName) ->
    ?nif_stub.

%% @doc transform tile to binary as GDAL raster format code:
%% http://gdal.org/formats_list.html
-spec tile_to_binary(Tile::tile(), TileFileName::iolist(), RasterFormatCode::string()) ->
    {ok, binary()} | {error, string()}.
tile_to_binary(_Tile, _TileFileName, _RasterFormatCode) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, SrsWkt} = get_srs_wkt_of(4326),
    ExpectedWkt = "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]",
    ?assertEqual(ExpectedWkt, SrsWkt).

-endif.
