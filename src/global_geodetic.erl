%% @doc TileMap profile: global-geodetic
%% which is base on EPSG:4326
%%
%% ------------------------------------------------------------------------------------------------
%%	TMS Global Geodetic Profile
%% ------------------------------------------------------------------------------------------------
%%
%% Functions necessary for generation of global tiles in Plate Carre projection,
%%	EPSG:4326, "unprojected profile".
%%
%%	Such tiles are compatible with Google Earth (as any other EPSG:4326 rasters)
%%	and you can overlay the tiles on top of OpenLayers base map.
%%	
%%	Pixel and tile coordinates are in TMS notation (origin [0,0] in bottom-left).
%%
%%	What coordinate conversions do we need for TMS Global Geodetic tiles?
%%
%%	  Global Geodetic tiles are using geodetic coordinates (latitude,longitude)
%%	  directly as planar coordinates XY (it is also called Unprojected or Plate
%%	  Carre). We need only scaling to pixel pyramid and cutting to tiles.
%%	  Pyramid has on top level two tiles, so it is not square but rectangle.
%%	  Area [-180,-90,180,90] is scaled to 512x256 pixels.
%%	  TMS has coordinate origin (for pixels and tiles) in bottom-left corner.
%%	  Rasters are in EPSG:4326 and therefore are compatible with Google Earth.
%%
%%	     LatLon      <->      Pixels      <->     Tiles     
%%
%%	 WGS84 coordinates   Pixels in pyramid  Tiles in pyramid
%%	     lat/lon         XY pixels Z zoom      XYZ from TMS 
%%	    EPSG:4326                                           
%%	     .----.                ----                         
%%	    /      \     <->    /--------/    <->      TMS      
%%	    \      /         /--------------/                   
%%	     -----        /--------------------/                
%%	   WMS, KML    Web Clients, Google Earth  TileMapService
%% ------------------------------------------------------------------------------------------------

-module(global_geodetic).
-behaviour(global_grid).
-include("global_grid.hrl").

-export([tile_bounds/3, 
         coordinates_to_pixels/3,
         zoom_for_pixelsize/1, 
         resolution/1, 
         get_max_tilex/1,
         epsg_code/0]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% callback functions
%% ===================================================================

%% @doc Returns bounds of the given tile
-spec tile_bounds(TX::integer(), TY::integer(), Zoom::byte()) -> global_grid:bound().
tile_bounds(TX, TY, Zoom) ->
    Res = 180.0 / math:pow(2, Zoom),
    MinX = TX * Res - 180,
    MinY = TY * Res - 90,
    MaxX = (TX + 1) * Res - 180,
    MaxY = (TY + 1) * Res - 90,
    {MinX, MinY, MaxX, MaxY}.

%% @doc Resolution (arc/pixel) for given zoom level (measured at Equator)
-spec resolution(Zoom::byte()) -> float().
resolution(Zoom) ->
    180.0 / ?TILE_SIZE / math:pow(2, Zoom).

%% @doc EPSG:4326
-spec epsg_code() -> non_neg_integer().
epsg_code() ->
    4326.

get_max_tilex(Zoom) ->
    trunc(math:pow(2, Zoom + 1)) - 1.

%% @doc Maximal scaledown zoom of the pyramid closest to the pixelSize.
-spec zoom_for_pixelsize(PixelSize::float()) -> byte().
zoom_for_pixelsize(PixelSize) ->
    global_grid:zoom_for_pixelsize(fun ?MODULE:resolution/1, PixelSize, 0).

%% ----------------------- protected functions -----------------------
%%
%% @doc Converts lat/lon to pixel coordinates in given zoom of the EPSG:4326 pyramid"
%% LatLonToPixels, for EPSG:4326
%% This is a intermediate function, called by coordinates_to_tile
coordinates_to_pixels(Lat, Lon, Zoom) ->
    Res = resolution(Zoom),
    Px = (180.0 + Lat) / Res,
    Py = (90.0 + Lon) / Res,
    {Px, Py}.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

tile_bounds_test() ->
    math_utils:swne_assert({-180.0, -90.0, 0.0, 90.0}, 
                            tile_bounds(0, 0, 0)),
    math_utils:swne_assert({-178.2421875, -86.484375, -178.06640625, -86.30859375}, 
                            tile_bounds(10, 20, 10)),
    math_utils:swne_assert({117.06344604492188, 35.11985778808594, 117.06413269042969, 35.12054443359375}, 
                            tile_bounds(432630,182219,18)),
    math_utils:swne_assert({117.06619262695313,35.11985778808594,117.06687927246094,35.12054443359375}, 
                            tile_bounds(432634,182219,18)).

resolution_test() ->
    ?assertEqual(0.703125, resolution(0)),
    ?assertEqual(0.3515625, resolution(1)),
    ?assertEqual(0.17578125, resolution(2)),
    ?assertEqual(0.0006866455078125, resolution(10)).

zoom_for_pixelsize_test() ->
    ?assertEqual(0, zoom_for_pixelsize(1000000)),
    ?assertEqual(0, zoom_for_pixelsize(100000)),
    ?assertEqual(0, zoom_for_pixelsize(1)),
    ?assertEqual(2, zoom_for_pixelsize(0.1)),
    ?assertEqual(3, zoom_for_pixelsize(0.07)),
    ?assertEqual(6, zoom_for_pixelsize(0.01)),
    ?assertEqual(9, zoom_for_pixelsize(0.001)).

coordinates_to_pixels_test() ->
    math_utils:xy_assert({256.0, 128.0}, 
                         coordinates_to_pixels(0, 0, 0)),
    math_utils:xy_assert({276707.55555555556, 145635.55555555556}, 
                         coordinates_to_pixels(10, 10, 10)),
    math_utils:xy_assert({110543212.08888888, 48467512.888888888}, 
                         coordinates_to_pixels(116.5, 40, 18)).

coordinates_to_tile_test() ->
    ?assertMatch({0, 0}, 
                 global_grid:coordinates_to_tile(?MODULE, 0, 0, 0)),
    ?assertMatch({1, 0}, 
                 global_grid:coordinates_to_tile(?MODULE, 119.7, 39.9, 0)),
    ?assertMatch({872939, 378361}, 
                 global_grid:coordinates_to_tile(?MODULE, 119.7, 39.9, 19)).

-endif.
