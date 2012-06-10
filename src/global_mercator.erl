%% @doc TileMap profile: global-mercator
%% which is base on EPSG:3785 ( = EPSG:900913)
%%
-module(global_mercator).
-behaviour(global_grid).
-include("global_grid.hrl").

-export([tile_bounds/3, 
         coordinates_to_pixels/3,
         zoom_for_pixelsize/1, 
         resolution/1, 
         epsg_code/0]).

%-define(EARTH_RADIUS, 6378137).
%-define(PI, math:pi()).
%%	What are zoom level constants (pixels/meter) for pyramid with EPSG:3785 or EPSG:900913?
%%
%%	  whole region is on top of pyramid (zoom=0) covered by 256x256 pixels tile,
%%	  every lower zoom level resolution is always divided by two
%%	  initialResolution = 20037508.342789244 * 2 / 256 = 156543.03392804062
%% -define(INITIAL_RESOLUTION, (2 * ?PI * ?EARTH_RADIUS / ?TILE_SIZE)).
-define(INITIAL_RESOLUTION, 156543.03392804097).

%% -define(ORIGIN_SHIFT, (2 * ?PI * ?EARTH_RADIUS / 2.0)). 
-define(ORIGIN_SHIFT, 20037508.342789244).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% callback functions
%% ===================================================================

%% @doc Returns bounds of the given tile in EPSG:3785 or EPSG:900913 coordinates
-spec tile_bounds(TX::integer(), TY::integer(), Zoom::byte()) -> global_grid:bound().
tile_bounds(TX, TY, Zoom) ->
    {MinX, MinY} = pixels_to_meters(TX * ?TILE_SIZE, TY * ?TILE_SIZE, Zoom),
    {MaxX, MaxY} = pixels_to_meters((TX + 1) * ?TILE_SIZE, (TY + 1) * ?TILE_SIZE, Zoom),
    {MinX, MinY, MaxX, MaxY}.

%% @doc Maximal scaledown zoom of the pyramid closest to the pixelSize.
-spec zoom_for_pixelsize(PixelSize::float()) -> byte().
zoom_for_pixelsize(PixelSize) ->
    global_grid:zoom_for_pixelsize(fun ?MODULE:resolution/1, PixelSize, 0).

%% @doc Resolution (meters/pixel) for given zoom level (measured at Equator)
-spec resolution(Zoom::byte()) -> float().
resolution(Zoom) ->
    ?INITIAL_RESOLUTION / math:pow(2, Zoom).

%% @doc EPSG:3785
-spec epsg_code() -> non_neg_integer().
epsg_code() ->
    3785.

%% ----------------------- protected functions -----------------------
%%
%% @doc Converts EPSG:3785 to pyramid pixel coordinates in given zoom level
%% MetersToPixels, for EPSG:3785
%% This is a intermediate function, called by coordinates_to_tile
-spec coordinates_to_pixels(float(), float(), byte()) -> {float(), float()}.
coordinates_to_pixels(MX, MY, Zoom) ->
    Resolution = resolution(Zoom),
    PX = (MX + ?ORIGIN_SHIFT) / Resolution,
    PY = (MY + ?ORIGIN_SHIFT) / Resolution,
    {PX, PY}.

%% ===================================================================
%% private functions
%% ===================================================================

%% @doc Converts pixel coordinates in given zoom level of pyramid to EPSG:3785 or EPSG:900913
-spec pixels_to_meters(PX::integer(), PY::integer(), Zoom::byte()) -> {float(), float()}.
pixels_to_meters(PX, PY, Zoom) ->
    Resolution = resolution(Zoom),
    MX = PX * Resolution - ?ORIGIN_SHIFT,
    MY = PY * Resolution - ?ORIGIN_SHIFT,
    {MX, MY}.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

tile_bounds_test() ->
    math_utils:swne_assert(
        {-19841829.550379194, -19939668.946584217, -19832045.61075869, -19929885.006963715}, 
        tile_bounds(20, 10, 12)),
    math_utils:swne_assert(
        {-20037508.342789244, -20037508.342789244, 20037508.342789244, 20037508.342789244}, 
        tile_bounds(0, 0, 0)).

zoom_for_pixelsize_test() ->
    ?assertEqual(0, zoom_for_pixelsize(1000000)),
    ?assertEqual(0, zoom_for_pixelsize(100000)),
    ?assertEqual(20, zoom_for_pixelsize(0.1)),
    ?assertEqual(30, zoom_for_pixelsize(0.0000728964)),
    ?assertEqual(3, zoom_for_pixelsize(10000)).

pixels_to_meters_test() ->
    math_utils:xy_assert({762677661.29741549, 762677661.29741549}, 
                         pixels_to_meters(10000, 10000, 1)),
    math_utils:xy_assert({-4383204.9499851465, -4383204.9499851465}, 
                         pixels_to_meters(100, 100, 0)).

coordinates_to_tile_test() ->
    ?assertMatch({0, 0}, 
                 global_grid:coordinates_to_tile(?MODULE, 0, 0, 0)),
    ?assertMatch({0, 0}, 
                 global_grid:coordinates_to_tile(?MODULE, 0, 0, 1)),
    ?assertMatch({131071, 131071}, 
                 global_grid:coordinates_to_tile(?MODULE, 0, 0, 18)),
    ?assertMatch({1956802, 266353}, 
                 global_grid:coordinates_to_tile(?MODULE, 129534670,321750, 19)).

-endif.
