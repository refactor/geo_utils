%% @doc TileMap profile: global-mercator
%% which is base on EPSG:3785 ( = EPSG:900913)
%%
%% -----------------------------------------------------------------------------
%%      TMS Global Mercator Profile
%% -----------------------------------------------------------------------------
%%
%%	Functions necessary for generation of tiles in Spherical Mercator projection,
%%	EPSG:900913 (EPSG:gOOglE, Google Maps Global Mercator), EPSG:3785, OSGEO:41001.
%%
%%	Such tiles are compatible with Google Maps, Microsoft Virtual Earth, Yahoo Maps,
%%	UK Ordnance Survey OpenSpace API, ...
%%	and you can overlay them on top of base maps of those web mapping applications.
%%	
%%	Pixel and tile coordinates are in TMS notation (origin [0,0] in bottom-left).
%%
%%	What coordinate conversions do we need for TMS Global Mercator tiles::
%%
%%	     LonLat      <->       Meters      <->     Pixels    <->       Tile     
%%
%%	 WGS84 coordinates   Spherical Mercator  Pixels in pyramid  Tiles in pyramid
%%	     lon/lat            XY in metres     XY pixels Z zoom      XYZ from TMS 
%%	    EPSG:4326           EPSG:900913                                         
%%	     .----.              ---------               --                TMS      
%%	    /      \     <->     |       |     <->     /----/    <->      Google    
%%	    \      /             |       |           /--------/          QuadTree   
%%	     -----               ---------         /------------/                   
%%	   KML, public         WebMapService         Web Clients      TileMapService
%%
%%	What is the coordinate extent of Earth in EPSG:900913?
%%
%%	  [-20037508.342789244, -20037508.342789244, 20037508.342789244, 20037508.342789244]
%%	  Constant 20037508.342789244 comes from the circumference of the Earth in meters,
%%	  which is 40 thousand kilometers, the coordinate origin is in the middle of extent.
%%      In fact you can calculate the constant as: 2 * math.pi * 6378137 / 2.0
%%	  $ echo 180 85 | gdaltransform -s_srs EPSG:4326 -t_srs EPSG:900913
%%	  Polar areas with abs(latitude) bigger then 85.05112878 are clipped off.
%%
%%	What are zoom level constants (pixels/meter) for pyramid with EPSG:900913?
%%
%%	  whole region is on top of pyramid (zoom=0) covered by 256x256 pixels tile,
%%	  every lower zoom level resolution is always divided by two
%%	  initialResolution = 20037508.342789244 * 2 / 256 = 156543.03392804062
%%
%%	What is the difference between TMS and Google Maps/QuadTree tile name convention?
%%
%%	  The tile raster itself is the same (equal extent, projection, pixel size),
%%	  there is just different identification of the same raster tile.
%%	  Tiles in TMS are counted from [0,0] in the bottom-left corner, id is XYZ.
%%	  Google placed the origin [0,0] to the top-left corner, reference is XYZ.
%%	  Microsoft is referencing tiles by a QuadTree name, defined on the website:
%%	  http://msdn2.microsoft.com/en-us/library/bb259689.aspx
%%
%%	The lon/lat coordinates are using WGS84 datum, yeh?
%%
%%	  Yes, all lon/lat we are mentioning should use WGS84 Geodetic Datum.
%%	  Well, the web clients like Google Maps are projecting those coordinates by
%%	  Spherical Mercator, so in fact lat/lon coordinates on sphere are treated as if
%%	  the were on the WGS84 ellipsoid.
%%	 
%%	  From MSDN documentation:
%%	  To simplify the calculations, we use the spherical form of projection, not
%%	  the ellipsoidal form. Since the projection is used only for map display,
%%	  and not for displaying numeric coordinates, we don't need the extra precision
%%	  of an ellipsoidal projection. The spherical projection causes approximately
%%	  0.33 percent scale distortion in the Y direction, which is not visually noticable.
%%
%%	How do I create a raster in EPSG:900913 and convert coordinates with PROJ.4?
%%
%%	  You can use standard GIS tools like gdalwarp, cs2cs or gdaltransform.
%%	  All of the tools supports -t_srs 'epsg:900913'.
%%
%%	  For other GIS programs check the exact definition of the projection:
%%	  More info at http://spatialreference.org/ref/user/google-projection/
%%	  The same projection is degined as EPSG:3785. WKT definition is in the official
%%	  EPSG database.
%% -----------------------------------------------------------------------------

-module(global_mercator).
-behaviour(tile_grid).

-include("global_grid.hrl").

-export([tile_bounds/1, 
         coordinates_to_pixels/3,
         resolution/1, 
         max_tile_extent/1,
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

%% =============================================================================
%% callback functions
%% =============================================================================

%% @doc Returns bounds of the given tile in EPSG:3785 or EPSG:900913 coordinates
-spec tile_bounds(tile_grid:tile_inf()) -> tile_grid:bound().
tile_bounds({TX, TY, Zoom}) ->
    {MinX, MinY} = pixels_to_meters(TX * ?TILE_SIZE, TY * ?TILE_SIZE, Zoom),
    {MaxX, MaxY} = pixels_to_meters((TX + 1) * ?TILE_SIZE, (TY + 1) * ?TILE_SIZE, Zoom),
    {MinX, MinY, MaxX, MaxY}.

%% @doc Resolution (meters/pixel) for given zoom level (measured at Equator)
%% INITIAL_RESOLUTION / (2 ** Zoom)
-spec resolution(Zoom::byte()) -> float().
resolution(Zoom) ->
    ?INITIAL_RESOLUTION / (1 bsl Zoom).

%% @doc Spherical Mercator, EPSG:3785
-spec epsg_code() -> non_neg_integer().
epsg_code() ->
    3785.

%% @doc calculate max tile-width or tile-height in specified zoom level
%% 2 ** Zoom - 1
max_tile_extent(Zoom) ->
    (1 bsl Zoom) - 1.

%% ----------------------------- protected functions ---------------------------
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

%% =============================================================================
%% private functions
%% =============================================================================

%% @doc Converts pixel coordinates in given zoom level of pyramid to EPSG:3785 or EPSG:900913
-spec pixels_to_meters(PX::integer(), PY::integer(), Zoom::byte()) -> {float(), float()}.
pixels_to_meters(PX, PY, Zoom) ->
    Resolution = resolution(Zoom),
    MX = PX * Resolution - ?ORIGIN_SHIFT,
    MY = PY * Resolution - ?ORIGIN_SHIFT,
    {MX, MY}.

%% =============================================================================
%% EUnit tests
%% =============================================================================
-ifdef(TEST).

tile_bounds_test() ->
    math_utils:swne_assert(
        {-19841829.550379194, -19939668.946584217, -19832045.61075869, -19929885.006963715}, 
        tile_bounds({20, 10, 12})),
    math_utils:swne_assert(
        {-20037508.342789244, -20037508.342789244, 20037508.342789244, 20037508.342789244}, 
        tile_bounds({0, 0, 0})),
    math_utils:swne_assert(
        {46100394.751242355, 7819049.371403821, 46100547.62529893, 7819202.245460391}, 
        tile_bounds({432630,182219,18})).

resolution_test() ->
    ?assertEqual(78271.51696402048, resolution(1)).

zoom_for_pixelsize_test() ->
    ?assertEqual(0, tile_grid:zoom_for_pixelsize(?MODULE, 1000000)),
    ?assertEqual(0, tile_grid:zoom_for_pixelsize(?MODULE, 100000)),
    ?assertEqual(20, tile_grid:zoom_for_pixelsize(?MODULE, 0.1)),
    ?assertEqual(30, tile_grid:zoom_for_pixelsize(?MODULE, 0.0000728964)),
    ?assertEqual(3, tile_grid:zoom_for_pixelsize(?MODULE, 10000)).

pixels_to_meters_test() ->
    math_utils:xy_assert({762677661.29741549, 762677661.29741549}, 
                         pixels_to_meters(10000, 10000, 1)),
    math_utils:xy_assert({-4383204.9499851465, -4383204.9499851465}, 
                         pixels_to_meters(100, 100, 0)).

coordinates_to_tile_test() ->
    ?assertMatch({0, 0}, 
                 tile_grid:coordinates_to_tile(?MODULE, 0, 0, 0)),
    ?assertMatch({0, 0}, 
                 tile_grid:coordinates_to_tile(?MODULE, 0, 0, 1)),
    ?assertMatch({131071, 131071}, 
                 tile_grid:coordinates_to_tile(?MODULE, 0, 0, 18)),
    ?assertMatch({1956802, 266353}, 
                 tile_grid:coordinates_to_tile(?MODULE, 129534670,321750, 19)).

-endif.
