-module(img_tiler, [ProfileMod, Img, ImgInfo]).

-include("global_grid.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%% export for eunit
-export([
         calc_tiles_enclosure/0
        ]).
-endif.

%% API
-export([new/2, new/3,
         scan_img/0,
         copyout_rawtile_for/3
        ]).

-record(tile_position, {current_tile_x :: integer(), 
                        current_tile_y :: integer(), 
                        tile_zoom      :: byte(),
                        tile_enclosure :: tile_grid:enclosure(integer())}).


%% override parameterized module new function
new(ProfileMod, ImgFileName) ->
    {ok, Img, ImgInfo} = gdal_nif:create_warped_vrtimg(ImgFileName, ProfileMod:epsg_code()),
    instance(ProfileMod, Img, ImgInfo).

%% old depracated 
new(ProfileMod, Img, ImgInfo) ->
    instance(ProfileMod, Img, ImgInfo).


%% @spec scan_img() -> {continue, {integer(),integer(),byte()}, fun()}.
scan_img() ->
    {TileZoom, TileEnclosure} = calc_tiles_enclosure(),
    {StartTileX, StartTileY, _, _} = TileEnclosure,
    {continue, {StartTileX, StartTileY, TileZoom},
               fun() ->
                    scan_img(
                             #tile_position{current_tile_x = StartTileX, 
                                            current_tile_y = StartTileY, 
                                            tile_zoom      = TileZoom,
                                            tile_enclosure = TileEnclosure})
               end}.

%% @spec copyout_rawtile_for(integer(), integer(), byte()) -> {ok, reference()} | {error, string()}.
copyout_rawtile_for(Tx, Ty, Tz) ->
    QuerySize = 4 * ?TILE_SIZE,
%    {MinX, MinY, MaxX, MaxY} = ProfileMod:tile_bounds({Tx, Ty, Tz}),
%    Bound = {MinX, MaxY, MaxX, MinY},
    {Rb, Wb} = tile_grid:geo_query(ProfileMod, ImgInfo, {Tx,Ty,Tz}, QuerySize),
    lager:debug("tx: ~p, ty: ~p, tz: ~p, rb: ~p, wb: ~p, querysize: ~p", 
                [Tx, Ty, Tz, Rb, Wb, QuerySize]),
    gdal_nif:copyout_rawtile(Img, Rb, Wb).


%% =============================================================================
%% private functions
%% =============================================================================

%% @doc Continuation
scan_img(#tile_position{current_tile_x = MaxX,
                        current_tile_y = MaxY,
                        tile_zoom      = _TileZoom,
                        tile_enclosure = {_,_, MaxX,MaxY}}) ->
    done;
scan_img(#tile_position{current_tile_x = MaxX,
                        current_tile_y = Y,
                        tile_zoom      = TileZoom,
                        tile_enclosure ={MinX,MinY,MaxX,MaxY}}) ->
    {continue, {MinX, Y + 1, TileZoom},
                fun() ->
                    scan_img(
                             #tile_position{current_tile_x = MinX,
                                            current_tile_y = Y + 1,
                                            tile_zoom      = TileZoom,
                                            tile_enclosure = {MinX,MinY,MaxX,MaxY}})
                end};
scan_img(#tile_position{current_tile_x = X,
                        current_tile_y = Y,
                        tile_zoom      = TileZoom,
                        tile_enclosure={MinX,MinY,MaxX,MaxY}}) ->
    {continue, {X + 1, Y, TileZoom},
                fun() ->
                    scan_img(
                             #tile_position{current_tile_x = X + 1,
                                            current_tile_y = Y,
                                            tile_zoom      = TileZoom,
                                            tile_enclosure = {MinX,MinY, MaxX, MaxY}})
                end}.


%% =============================================================================
%% private functions
%% =============================================================================

%% @doc calculate the tiles enclosure of the img Raster in a specified zoom level
%% the zoom level is dicided by the img precision which is defined in ImgInfo
%% and used for base_tiles of the img
%% @spec calc_tiles_enclosure() -> {byte(), tile_grid:enclosure(integer())}.
calc_tiles_enclosure() ->
    tile_grid:calc_tminmax(ProfileMod, ImgInfo).


