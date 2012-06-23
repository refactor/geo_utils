%% @doc TileMap profile: global-geodetic
%% which is base on EPSG:4326
%%
%% -----------------------------------------------------------------------------
%%	TMS Global Geodetic Profile
%% -----------------------------------------------------------------------------
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
%% -----------------------------------------------------------------------------

-module(global_geodetic).
-behaviour(map_profile).

-include("global_grid.hrl").

-export([epsg_code/0,
         tile_bounds/1, 
         resolution/1, 
         max_tile_extent/1,
         coordinates_to_pixels/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% =============================================================================
%% callback functions
%% =============================================================================

%% @doc WGS84, EPSG:4326
-spec epsg_code() -> non_neg_integer().
epsg_code() ->
    4326.


%% @doc Returns bounds of the given tile
-spec tile_bounds(tile_grid:tile_info()) -> tile_grid:bound().
tile_bounds({TX, TY, Zoom}) ->
    Res = 180.0 / (1 bsl Zoom),  % 180.0 / (2 ** Zoom)
    MinX = TX * Res - 180,
    MinY = TY * Res - 90,
    MaxX = (TX + 1) * Res - 180,
    MaxY = (TY + 1) * Res - 90,
    {MinX, MinY, MaxX, MaxY}.


%% @doc Resolution (arc/pixel) for given zoom level (measured at Equator)
%%  provide <TileSet>s with units-per-pixel
%%  180 / ?TILE_SIZE / (2 ** Zoom)
-spec resolution(Zoom::byte()) -> float().
resolution(Zoom) ->
    180.0 / ?TILE_SIZE / (1 bsl Zoom).


%% @doc calculate max tile-width or tile-height in specified zoom level
%% 2 ** (Zoom + 1) - 1
-spec max_tile_extent(byte()) -> non_neg_integer().
max_tile_extent(Zoom) ->
    (1 bsl  (Zoom + 1)) - 1.


%% ---------------------------- protected functions ----------------------------
%%
%% @doc Converts lon/lat to pixel coordinates in given zoom of the EPSG:4326 pyramid"
%% LatLonToPixels, for EPSG:4326
%% This is a intermediate function, called by coordinates_to_tile
coordinates_to_pixels(Longitude, Latitude, Zoom) ->
    Res = resolution(Zoom),
    Px = (180.0 + Longitude) / Res,
    Py = (90.0 + Latitude) / Res,
    {Px, Py}.


%% =============================================================================
%% EUnit tests
%% =============================================================================
-ifdef(TEST).

tile_bounds_test() ->
    math_utils:swne_assert({-180.0, -90.0, 0.0, 90.0}, 
                            tile_bounds({0, 0, 0})),
    math_utils:swne_assert({-178.2421875, -86.484375, -178.06640625, -86.30859375}, 
                            tile_bounds({10, 20, 10})),
    math_utils:swne_assert({117.06344604492188, 35.11985778808594, 117.06413269042969, 35.12054443359375}, 
                            tile_bounds({432630,182219,18})),
    math_utils:swne_assert({117.06619262695313,35.11985778808594,117.06687927246094,35.12054443359375}, 
                            tile_bounds({432634,182219,18})).

resolution_test() ->
    ?assertEqual(0.703125, resolution(0)),
    ?assertEqual(0.3515625, resolution(1)),
    ?assertEqual(0.17578125, resolution(2)),
    ?assertEqual(0.087890625, resolution(3)),
    ?assertEqual(0.0006866455078125, resolution(10)).

zoom_for_pixelsize_test() ->
    ?assertEqual(0, tile_grid:zoom_for_pixelsize(?MODULE, 1000000)),
    ?assertEqual(0, tile_grid:zoom_for_pixelsize(?MODULE, 100000)),
    ?assertEqual(0, tile_grid:zoom_for_pixelsize(?MODULE, 1)),
    ?assertEqual(2, tile_grid:zoom_for_pixelsize(?MODULE, 0.1)),
    ?assertEqual(3, tile_grid:zoom_for_pixelsize(?MODULE, 0.07)),
    ?assertEqual(6, tile_grid:zoom_for_pixelsize(?MODULE, 0.01)),
    ?assertEqual(9, tile_grid:zoom_for_pixelsize(?MODULE, 0.001)).

coordinates_to_pixels_test() ->
    math_utils:xy_assert({256.0, 128.0}, 
                         coordinates_to_pixels(0, 0, 0)),
    math_utils:xy_assert({276707.55555555556, 145635.55555555556}, 
                         coordinates_to_pixels(10, 10, 10)),
    math_utils:xy_assert({110543212.08888888, 48467512.888888888}, 
                         coordinates_to_pixels(116.5, 40, 18)).

coordinates_to_tile_test() ->
    ?assertMatch({0, 0}, 
                 tile_grid:coordinates_to_tile(?MODULE, 0, 0, 0)),
    ?assertMatch({1, 0}, 
                 tile_grid:coordinates_to_tile(?MODULE, 119.7, 39.9, 0)),
    ?assertMatch({872939, 378361}, 
                 tile_grid:coordinates_to_tile(?MODULE, 119.7, 39.9, 19)).

-endif.
