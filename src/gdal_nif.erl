-module(gdal_nif).

-export([build_out_ds_srs_wkt/1,
         get_meta/1,
         create_warped_vrt/2,
         close_img/1]).

-export([copyout_tile/3,
         build_tile/1,
         save_tile/2]).

-on_load(init/0).

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

-spec build_out_ds_srs_wkt(EpspCode::non_neg_integer()) -> {ok, string()} | {error, string()}.
build_out_ds_srs_wkt(_EpsgCode) ->
    ?nif_stub.

%% @doc build out_ds in global world of EPSP:Code projection
-spec create_warped_vrt(ImgFileName::string(), VrtEpsgCode::non_neg_integer()) -> {ok, reference()} | {error, string()}.
create_warped_vrt(_ImgFileName, _VrtEpsgCode) ->
    ?nif_stub.

-spec close_img(Img::reference()) -> ok | {error, string()}.
close_img(_Img) ->
    ?nif_stub.

-spec get_meta(Img::reference()) -> any().
get_meta(_ImgRef) ->
    ?nif_stub.

-spec copyout_tile(reference(), global_grid:bandregion(), global_grid:bandregion()) -> {ok, reference()} | {error, string()}.
copyout_tile(_Img, _R, _W) ->
    ?nif_stub.

-spec build_tile(Tile::reference()) -> ok | {error, string()}.
build_tile(_Tile) ->
    ?nif_stub.

-spec save_tile(Tile::reference(), TileFileName::string()) -> ok | {error, string()}.
save_tile(_Tile, _TileFileName) ->
    ?nif_stub.


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, SrsWkt} = build_out_ds_srs_wkt(4326),
    ExpectedWkt = "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]",
    ?assertEqual(ExpectedWkt, SrsWkt).

-endif.
