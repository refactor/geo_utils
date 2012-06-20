%%% ----------------------------------------------------------------------------
%%% @doc Purpose:  Convert a raster into TMS (Tile Map Service) tiles in a 
%%%             directory or something else as fast as possible.
%%%           - support of global tiles (Spherical Mercator) for compatibility
%%%               with interactive web maps such as Google Maps
%%% 
%%%  this is a clone implementent from gdal2tiles.py, but use erlang/OTP do some
%%%             parallel work for the speed
%%%  gdal2tiles.py is the work of Klokan Petr Pridal, klokan at klokan dot cz
%%%      Web:      http://www.klokan.cz/projects/gdal2tiles/
%%% @end
%%% ----------------------------------------------------------------------------

-module(global_grid).

-include("global_grid.hrl").

-export([behaviour_info/1]).

%% API
-export([geo_query/3,
         calc_tminmax/2,
         quadtree/1]).

-export([zoom_for_pixelsize/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%% export for eunit
-export([
         coordinates_to_tile/4  % Returns tile for given coordinates
        ]).
-endif.

-export_type([bound/0, 
              enclosure/1,
              bandregion/0, 
              tile_info/0,
              img_info/0]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{tile_bounds, 3},          % Returns bounds of the given tile
     {coordinates_to_pixels, 3},% for coordinates_to_tiles which return tile of 
                                % given coordinates in differnent projection or tile profile
     {zoom_for_pixelsize, 1},   % Max scaledown zoom of the pyramid closest to the pixelSize
     {resolution, 1},           % Resolution for given zoom level
     {max_tile_coordinate, 1},  % Calculate max tile-width or tile-height in specified zoom level
                                % there is different calutaions for different profiles
     {epsg_code, 0}];           % EPSG code for Projection 
behaviour_info(_Other) ->
    undefined.


%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = Bound
-type bound() :: {LeftTopX     :: float(), 
                  LeftTopY     :: float(), 
                  RightBottomX :: float(), 
                  RightBottomY :: float()}.

-type enclosure(T) :: {MinX :: T, 
                       MinY :: T, 
                       MaxX :: T, 
                       MaxY :: T}.

%% the region of the band
-type bandregion() :: {
        XOffset :: non_neg_integer(), % pixel offset to the top left corner of the region of the band
        YOffset :: non_neg_integer(), % line offset to the top left corner of the region of the band
        XSize :: non_neg_integer(),   % width of the region of the band to be accessed in pixels
        YSize :: non_neg_integer()}.  % height of the region of the band to be accessed in lines

%% {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize},
-type img_info() :: {OriginX     :: float(), 
                     OriginY     :: float(), 
                     PixelSizeX  :: float(), 
                     PixelSizeY  :: float(), 
                     RasterXSize :: non_neg_integer(), 
                     RasterYSize :: non_neg_integer()}.

-type tile_info() :: {X       :: integer(),
                      Y       :: integer(),
                      Zoom    :: byte()}.

%% @doc For given dataset and query in cartographic coordinates returns parameters for ReadRaster() in 
%% raster coordinates and x/y shifts (for border tiles). If the querysize is not given, the extent is 
%% returned in the native resolution of dataset ds.
%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = _Bound
-spec geo_query(img_info(), bound(), non_neg_integer()) -> {bandregion(), bandregion()}.
geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize}, {Ulx, Uly, Lrx, Lry}, QuerySize) ->
    Rx = erlang:trunc( (Ulx - OriginX) / PixelSizeX + 0.001 ),
    Ry = erlang:trunc( (Uly - OriginY) / PixelSizeY + 0.001 ),
    Rxsize = erlang:trunc( (Lrx - Ulx) / PixelSizeX + 0.5 ),
    Rysize = erlang:trunc( (Lry - Uly) / PixelSizeY + 0.5 ),

    {NewRx, NewWx, ResWxsize, ResRxsize} = adjust_byedge(Rx, Rxsize, RasterXSize, QuerySize),
    {NewRy, NewWy, ResWysize, ResRysize} = adjust_byedge(Ry, Rysize, RasterYSize, QuerySize),

    {{NewRx, NewRy, ResRxsize, ResRysize}, {NewWx, NewWy, ResWxsize, ResWysize}}.


%% =============================================================================
%% protected functions
%% =============================================================================
-spec zoom_for_pixelsize(Resolution::fun(), PixelSize::float(), I::byte()) -> byte().
zoom_for_pixelsize(Resolution, PixelSize, I) ->
    R = Resolution(I),
    if 
        PixelSize > R ->
            cfi(I);
        I < ?MAXZOOMLEVEL ->
            zoom_for_pixelsize(Resolution, PixelSize, I + 1);
        true ->
            I
    end.

cfi(I) ->
    case I of
        0 -> 0;  % We don't want to scale up
        _ -> I - 1
    end.


%% @doc Get the tiles range of the region enclosure for a zoom level, 
%% in a specified projection profile
-spec calc_tminmax(module(), enclosure(float())) -> {byte(), enclosure(integer())}.
calc_tminmax(ProjMod, RasterInfo) ->
    {_Tminz, Tmaxz} = calc_zoomlevel_range(ProjMod, RasterInfo),
    Zoom = Tmaxz,

    {Ominx, Ominy, Omaxx, Omaxy} = calc_img_enclosure(RasterInfo),
    {Tminx, Tminy} = coordinates_to_tile( ProjMod, Ominx, Ominy, Zoom ),
    {Tmaxx, Tmaxy} = coordinates_to_tile( ProjMod, Omaxx, Omaxy, Zoom ),
    MaxTileWidthOrHeight = ProjMod:max_tile_coordinate(Zoom),
    EnclosureOfZoom = {
        max(0, Tminx), max(0, Tminy), 
        min(MaxTileWidthOrHeight, Tmaxx), min(MaxTileWidthOrHeight, Tmaxy)
    },
    {Zoom, EnclosureOfZoom}.

%% @doc Returns tile for given coordinates
%% Returns the tile for zoom which covers given lat/lon coordinates
-spec coordinates_to_tile(module(), float(), float(), byte()) -> {integer(), integer()}.
coordinates_to_tile(ProjMod, Lat, Lon, Zoom) ->
    {Px, Py} = ProjMod:coordinates_to_pixels(Lat, Lon, Zoom),
    pixels_to_tile(Px, Py).


%% @doc Converts TMS tile coordinates to Microsoft QuadTree
%% http://msdn.microsoft.com/en-us/library/bb259689.aspx
-spec(quadtree({TX::integer(), TY::integer(), Zoom::byte()}) -> binary()).
quadtree({TX, TY, Zoom}) ->
    % Ty = (1 bsl Zoom) - 1 - TY, % 2 ** Zoom - TY
    Ty = global_mercator:max_tile_coordinate(Zoom) - TY,
    quadtree({TX, Ty, Zoom}, <<>>).

%% =============================================================================
%% private functions
%% =============================================================================

%% @doc Get the minimal and maximal zoom level
%% minimal zoom level: map covers area equivalent to one tile
%% maximal zoom level: closest possible zoom level up on the resolution of raster
-spec calc_zoomlevel_range(module(), img_info()) -> {byte(), byte()}.
calc_zoomlevel_range(ProjMod, RasterInfo) ->
    {_OriginX, _OriginY, PixelSizeX, _PixelSizeY, RasterXSize, RasterYSize} = RasterInfo,
    Tminz = ProjMod:zoom_for_pixelsize( PixelSizeX * max( RasterXSize, RasterYSize) / ?TILE_SIZE ),
    Tmaxz = ProjMod:zoom_for_pixelsize( PixelSizeX ),
    {Tminz, Tmaxz}.

%% @doc Returns coordinates of the tile covering region in pixel coordinates
pixels_to_tile(PixelX, PixelY) ->
    TileX = erlang:trunc( math_utils:ceiling( PixelX / float(?TILE_SIZE) ) - 1),
    TileY = erlang:trunc( math_utils:ceiling( PixelY / float(?TILE_SIZE) ) - 1),
    {TileX, TileY}.

%% @doc Coordinates should not go out of the bounds of the raster
-spec adjust_byedge(integer(), integer(), non_neg_integer(), non_neg_integer()) -> bandregion().
adjust_byedge(R, Rsize, RasterSize, QuerySize) ->
    Wsize0 = 
        if QuerySize == 0 ->
                Rsize;
            true ->
                QuerySize
        end,

    {NewR, NewW, NewWsize, NewRsize} = 
        if R < 0 ->
                Rshift = abs(R),
                W = trunc( Wsize0 * (Rshift / Rsize) ),
                Wsize = Wsize0 - W,
                {0, W, Wsize, Rsize - trunc(Rsize * (Rshift / Rsize)) };
            true ->
                {R, 0, Wsize0, Rsize}
        end,

    {ResWsize, ResRsize} = 
        if
            R + Rsize > RasterSize ->
                {trunc( NewWsize * (RasterSize - NewR) / NewRsize), RasterSize - NewR};
            true ->
                {NewWsize, NewRsize}
        end,
    {NewR, NewW, ResWsize, ResRsize}.


%% @doc get the geospatial enclosure of an img, in projection coordiates unit
-spec calc_img_enclosure(img_info()) -> enclosure(float()).
calc_img_enclosure(RasterInfo) ->
    {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize} = RasterInfo,
    ExtX = OriginX + PixelSizeX * RasterXSize,
    ExtY = OriginY + PixelSizeY * RasterYSize,
    {min(OriginX, ExtX), min(OriginY, ExtY), max(OriginX, ExtX), max(OriginY, ExtY)}.


-spec quadtree(TileInfo::tile_info(), Quadtree::binary()) -> binary().
quadtree({_TX, _TY, 0}, Quadtree) -> 
    Quadtree;
quadtree({TX, TY, Zoom}, Quadtree) -> 
    Mask = 1 bsl (Zoom - 1),
    Digit = bit_op(TX, TY, Mask),
    quadtree({TX, TY, Zoom - 1}, <<Quadtree/binary, (Digit + $0)>>).

-spec bit_op(TX::integer(), TY::integer(), Mask::byte()) -> 0 | 1 | 2 | 3.
bit_op(TX, TY, Mask) ->
    R1 = 
        if
            TX band Mask =/= 0 ->
                1;
            true ->
                0
        end,
    R2 =
        if
            TY band Mask =/= 0 ->
                2;
            true ->
                0
        end,
    R1 + R2.

%% =============================================================================
%% EUnit tests
%% =============================================================================
-ifdef(TEST).

calc_zoomlevel_range_test() ->
    {OriginX, OriginY} = {117.0439031173947, 35.138356923616534},
    {PixelSizeX, PixelSizeY} = { 2.0080973914208664e-06, -2.0080973914208664e-06},
    {RasterXSize, RasterYSize} = {10933, 8982},
    RasterInfo = {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize},

    {_Tminz, Tmaxz} = calc_zoomlevel_range(global_geodetic, RasterInfo),
    ?assertEqual(18, Tmaxz).

geodetic_geo_query_test() ->
    {OriginX, OriginY} = {117.0439031173947, 35.138356923616534},
    {PixelSizeX, PixelSizeY} = { 2.0080973914208664e-06, -2.0080973914208664e-06},
    {RasterXSize, RasterYSize} = {10933, 8982},
    Bound = {117.06344604492188, 35.11985778808594, 117.06413269042969, 35.12054443359375},
    {B0, B1, B2, B3} = Bound,
    Enclosure = {117.06344604492188,35.12054443359375,117.06413269042969,35.11985778808594},
    {Rb, Wb} = geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize}, Enclosure, 0),
    io:format("rb: ~p, wb: ~p~n", [Rb, Wb]),
    ?assertEqual({9732, 8870, 342, 112}, Rb),
    ?assertEqual({0, 0, 342, 112}, Wb).

mercator_geo_query_test() ->
    {OriginX, OriginY} = {13024084.000533571, 4184269.256414418},
    {PixelSizeX, PixelSizeY} = {0.24473611762142541, -0.24473611762142541},
    {RasterXSize, RasterYSize} = {60352, 62961},
    Enclosure = {865067, 633770, 865453, 633367},
    {Rb, Wb} = geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize}, Enclosure, 0),
    ?assertEqual({0, 14507459, -49680575, -14444498}, Rb),
    ?assertEqual({49682152, 0, -49680575, -14444498}, Wb).

pixels_to_tile_test() ->
    io:format("P2T: ~p~n", [pixels_to_tile(0,0)]),
    math_utils:xy_assert({-1, -1}, pixels_to_tile(0, 0)),
    math_utils:xy_assert({0, 0}, pixels_to_tile(10, 10)),
    math_utils:xy_assert({3, 3}, pixels_to_tile(1000, 1000)),
    math_utils:xy_assert({3, 39}, pixels_to_tile(1000, 10000)).

quadtree_test() ->
    ?assertEqual(<<"2222222">>, quadtree({0, 0, 7})),
    ?assertEqual(<<"113113">>, quadtree({-1, -10, 6})),
    ?assertEqual(<<"2221">>, quadtree({1, 1, 4})),
    ?assertEqual(<<"22221">>, quadtree({1, 1, 5})).

-endif.
