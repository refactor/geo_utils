-module(global_grid).
-behaviour(gen_fsm).

-include("global_grid.hrl").

-export([behaviour_info/1]).

-export([init/1, 
         copying/2, 
         listening/2, 
         handle_event/3, 
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

-export([zoom_for_pixelsize/3, 
         pixels_to_tile/2]).

-export([
         coordinates_to_tile/4  % Returns tile for given coordinates
        ]).

-export_type([world_state/0, bound/0, bandregion/0, rasterinfo/0]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init_world_state, 0},
     {tile_bounds, 3},          % Returns bounds of the given tile
     {coordinates_to_pixels, 3},
     {zoom_for_pixelsize, 1},   % Max scaledown zoom of the pyramid closest to the pixelSize
     {resolution, 1},           % Resolution for given zoom level
     {epsg_code, 0}];           % EPSG code for Projection 
behaviour_info(_Other) ->
    undefined.


-type world_state() :: #w_state{}.

%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = Bound
-type bound() :: {LeftTopX::float(), LeftTopY::float(), RightBottomX::float(), RightBottom::float()}.

-type enclosure() :: {MinX::float(), MinY::float(), MaxX::float(), MaxY::float()}.

%% XOffset: the pixel offset to the top left corner of the region of the band to be accessed
%% YOffset: The line offset to the top left corner of the region of the band to be accessed. 
%% XSize: The width of the region of the band to be accessed in pixels.
%% YSize: The height of the region of the band to be accessed in lines
-type bandregion() :: {XOffset::non_neg_integer(), YOffset::non_neg_integer(), XSize::non_neg_integer(), YSize::non_neg_integer()}.

%% {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize},
-type rasterinfo() :: {float(), float(), float(), float(), non_neg_integer(), non_neg_integer()}.


init({ProjectionMod, DatasetFileName}) ->
    {ok, copying, #w_state{global_projection=ProjectionMod}}.

copying(Event, State) ->
    {next_stage, copying, State}.

listening(Event, State) ->
    {next_stage, listening, State}.

handle_event(Event, StateName, StateData) ->
    {next_stage, copying, StateData}.
    
handle_sync_event(Event, From, StateName, StateData) ->
    {next_stage, copying, StateData}.

handle_info(Info, StateName, StateData) ->
    {next_stage, copying, StateData}.

terminate(Reason, StateName, StateData) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%% @doc For given dataset and query in cartographic coordinates returns parameters for ReadRaster() in 
%% raster coordinates and x/y shifts (for border tiles). If the querysize is not given, the extent is 
%% returned in the native resolution of dataset ds.
%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = _Bound
-spec geo_query(rasterinfo(), bound(), non_neg_integer()) -> {bandregion(), bandregion()}.
geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize}, {Ulx, Uly, Lrx, Lry}, QuerySize) ->
    Rx = trunc( (Ulx - OriginX) / PixelSizeX + 0.001 ),
    Ry = trunc( (Uly - OriginY) / PixelSizeY + 0.001 ),
    Rxsize = trunc( (Lrx - Ulx) / PixelSizeX + 0.5 ),
    Rysize = trunc( (Lry - Uly) / PixelSizeY + 0.5 ),

    {NewRx, NewWx, ResWxsize, ResRxsize} = adjust_byedge(Rx, Rxsize, RasterXSize, QuerySize),
    {NewRy, NewWy, ResWysize, ResRysize} = adjust_byedge(Ry, Rysize, RasterYSize, QuerySize),

    {{NewRx, NewRy, ResRxsize, ResRysize}, {NewWx, NewWy, ResWxsize, ResWysize}}.


%% ===================================================================
%% protected functions
%% ===================================================================
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

%% @doc Get the minimal and maximal zoom level
%% minimal zoom level: map covers area equivalent to one tile
%% maximal zoom level: closest possible zoom level up on the resolution of raster
-spec calc_zoomlevel_range(ProjMod::atom(), rasterinfo()) -> {byte(), byte()}.
calc_zoomlevel_range(ProjMod, RasterInfo) ->
    {_OriginX, _OriginY, PixelSizeX, _PixelSizeY, RasterXSize, RasterYSize} = RasterInfo,
    Tminz = ProjMod:zoom_for_pixelsize( PixelSizeX * max( RasterXSize, RasterYSize) / ?TILE_SIZE ),
    Tmaxz = ProjMod:zoom_for_pixelsize( PixelSizeX ),
    {Tminz, Tmaxz}.

calc_tminmax(ProjMod, {Ominx, Ominy, Omaxx, Omaxy} = _Enclosure, Zoom) ->
    {Tminx, Tminy} = coordinates_to_tile( ProjMod, Ominx, Ominy, Zoom ),
    {Tmaxx, Tmaxy} = coordinates_to_tile( ProjMod, Omaxx, Omaxy, Zoom ),
    Z = trunc(math:pow(2, Zoom)) - 1,
    EnclosureOfZoom = {
        max(0, Tminx), max(0, Tminy), 
        min(Z, Tmaxx), min(Z, Tmaxy)
    },
    EnclosureOfZoom.

%% @doc Returns tile for given coordinates
%% Returns the tile for zoom which covers given lat/lon coordinates
-spec coordinates_to_tile(atom(), float(), float(), byte()) -> {integer(), integer()}.
coordinates_to_tile(ProjMod, Lat, Lon, Zoom) ->
    {Px, Py} = ProjMod:coordinates_to_pixels(Lat, Lon, Zoom),
    pixels_to_tile(Px, Py).


%% ===================================================================
%% private functions
%% ===================================================================

%% @doc Returns coordinates of the tile covering region in pixel coordinates
pixels_to_tile(Px, Py) ->
    Tx = erlang:trunc( math_utils:ceiling( Px / float(?TILE_SIZE) ) - 1),
    Ty = erlang:trunc( math_utils:ceiling( Py / float(?TILE_SIZE) ) - 1),
    {Tx, Ty}.

%% @doc Coordinates should not go out of the bounds of the raster
-spec adjust_byedge(integer(), integer(), non_neg_integer(), non_neg_integer()) -> bandregion().
adjust_byedge(R, Rsize, RasterSize, QuerySize) ->
    if
        QuerySize == 0 ->
            Wsize0 = Rsize;
        true ->
            Wsize0 = QuerySize
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


-spec get_enclosure(rasterinfo()) -> enclosure().
get_enclosure(RasterInfo) ->
    {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize} = RasterInfo,
    ExtX = OriginX + PixelSizeX * RasterXSize,
    ExtY = OriginY + PixelSizeY * RasterYSize,
    {min(OriginX, ExtX), min(OriginY, ExtY), max(OriginX, ExtX), max(OriginY, ExtY)}.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

geo_quert_test() ->
    {OriginX, OriginY} = {13024084.000533571, 4184269.256414418},
    {PixelSizeX, PixelSizeY} = {0.24473611762142541, -0.24473611762142541},
    {RasterXSize, RasterYSize} = {60352, 62961},
    MinMaxBound = {865067, 633770, 865453, 633367},
    {Rb, Wb} = geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize}, MinMaxBound, 0),
    ?assertEqual({0, 14507459, -49680575, -14444498}, Rb),
    ?assertEqual({49682152, 0, -49680575, -14444498}, Wb).

pixels_to_tile_test() ->
    io:format("P2T: ~p~n", [pixels_to_tile(0,0)]),
    math_utils:xy_assert({-1, -1}, pixels_to_tile(0, 0)),
    math_utils:xy_assert({0, 0}, pixels_to_tile(10, 10)),
    math_utils:xy_assert({3, 3}, pixels_to_tile(1000, 1000)),
    math_utils:xy_assert({3, 39}, pixels_to_tile(1000, 10000)).

-endif.
