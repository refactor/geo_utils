-module(img_tiler, [ProfileMod, RasterImg, RasterInfo]).

-include("global_grid.hrl").

%% API
-export([
         calc_tiles_enclosure/0,
         copyout_tile_for/3
        ]).

-spec copyout_tile_for(integer(), integer(), byte()) -> 
    {ok, reference()} | {error, string()}.
copyout_tile_for(Tx, Ty, Tz) ->
    QuerySize = 4 * ?TILE_SIZE,
    {MinX, MinY, MaxX, MaxY} = ProfileMod:tile_bounds(Tx, Ty, Tz),
    Bound = {MinX, MaxY, MaxX, MinY},
    {Rb, Wb} = global_grid:geo_query(RasterInfo, Bound, QuerySize),
    lager:debug("tx: ~p, ty: ~p, tz: ~p, bound: ~p, rb: ~p, wb: ~p, querysize: ~p", 
                [Tx, Ty, Tz, Bound, Rb, Wb, QuerySize]),
    gdal_nif:copyout_tile(RasterImg, Rb, Wb).


%% @doc calculate the tiles enclosure of the img Raster in a specified zoom level
%% the zoom level is dicided by the img precision which is defined in RasterInfo
%% and used for base_tiles of the img
-spec calc_tiles_enclosure() -> {byte(), global_grid:enclosure()}.
calc_tiles_enclosure() ->
    {_Tminz, Tmaxz} = calc_zoomlevel_range(),
    SpatialEnclosure = global_grid:get_img_coordinates_enclosure(RasterInfo),
    TileEnclosure = global_grid:calc_tminmax(ProfileMod, SpatialEnclosure, Tmaxz),
    {Tmaxz, TileEnclosure}.


%% =============================================================================
%% private functions
%% =============================================================================

%% @doc Get the minimal and maximal zoom level
%% minimal zoom level: map covers area equivalent to one tile
%% maximal zoom level: closest possible zoom level up on the resolution of raster
-spec calc_zoomlevel_range() -> {byte(), byte()}.
calc_zoomlevel_range() ->
    {_OriginX, _OriginY, PixelSizeX, _PixelSizeY, RasterXSize, RasterYSize} = RasterInfo,
    Tminz = ProfileMod:zoom_for_pixelsize( PixelSizeX * max( RasterXSize, RasterYSize) / ?TILE_SIZE ),
    Tmaxz = ProfileMod:zoom_for_pixelsize( PixelSizeX ),
    {Tminz, Tmaxz}.

