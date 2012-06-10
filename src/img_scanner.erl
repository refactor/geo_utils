-module(img_scanner).
-behaviour(gen_fsm).

-include("global_grid.hrl").

-export([scan_img/2]).

-export([init/1,
         copyouting/2, 
         listening/2, 
         handle_event/3, 
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

%% API
-export([start_link/2
        ]).

-record(tile_position, {current_tile_x :: integer(), 
                        current_tile_y :: integer(), 
                        tile_zoom      :: byte(),
                        tile_enclosure :: global_grid:encluse()}).

start_link(TileMapProfileMod, ImgFileName) ->
    gen_fsm:start_link(?MODULE, {TileMapProfileMod, ImgFileName}, []).

init({ProfileMod, ImgFileName}) ->
    {ok, copyouting, #w_state{map_profile=ProfileMod, 
                              img_filename=ImgFileName}}.

copyouting({continue, {Img, RasterInfo, TileX, TileY, TileZoom}, Continuation}, State) ->
    {ok, TileRawdata} = global_grid:copyout_tile_for(State#w_state.map_profile, 
                                                     TileY, TileX, TileZoom, 
                                                     Img, RasterInfo),
    tile_builder:start_link(self(), {TileRawdata, {TileX, TileY, TileZoom}}),
    gen_fsm:send_event(self(), Continuation()),
    {next_state, copyouting, State};
copyouting(done, State) ->
%    {next_state, copyouting, State}.
    listening(done, State).

listening(_Event, State) ->
    {next_state, listening, State}.

handle_event(debug, StateName, StateData) ->
    io:format("handle EVENT~n"),
    lager:info("~p", [StateData]),
    {next_state, StateName, StateData};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.
    
handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(start, StateName, StateData = #w_state{map_profile=ProfileMod, img_filename=ImgFileName}) ->
    gen_fsm:send_event(self(), img_scanner:scan_img(ProfileMod, ImgFileName)),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


scan_img(ProfileMod, ImgFileName) ->
    {ok, Img, RasterInfo} = gdal_nif:create_warped_vrt(ImgFileName, 
                                                       ProfileMod:epsg_code()),
    {TileZoom, TileEnclosure} = global_grid:calc_tiles_enclosure(ProfileMod, RasterInfo),
    {StartTileX, StartTileY, _, _} = TileEnclosure,
    {continue, {Img, RasterInfo, StartTileX, StartTileY, TileZoom},
               fun() ->
                    scan_img(ProfileMod, 
                             Img, 
                             RasterInfo, 
                             #tile_position{current_tile_x = StartTileX, 
                                            current_tile_y = StartTileY, 
                                            tile_zoom      = TileZoom,
                                            tile_enclosure = TileEnclosure})
               end}.

scan_img(_ProfileMod, _Img, _RasterInfo, #tile_position{current_tile_x = MaxX,
                                                        current_tile_y = MaxY,
                                                        tile_zoom      = _TileZoom,
                                                        tile_enclosure = {_,_, MaxX,MaxY}}) ->
    done;
scan_img(ProfileMod, Img, RasterInfo, #tile_position{current_tile_x = MaxX,
                                                     current_tile_y = Y,
                                                     tile_zoom      = TileZoom,
                                                     tile_enclosure ={MinX,MinY,MaxX,MaxY}}) ->
    {continue, {Img, RasterInfo, MinX, Y + 1, TileZoom},
                fun() ->
                    scan_img(ProfileMod, 
                             Img, 
                             RasterInfo, 
                             #tile_position{current_tile_x = MinX,
                                            current_tile_y = Y + 1,
                                            tile_zoom      = TileZoom,
                                            tile_enclosure = {MinX,MinY,MaxX,MaxY}})
                end};
scan_img(ProfileMod, Img, RasterInfo, #tile_position{current_tile_x = X,
                                                     current_tile_y = Y,
                                                     tile_zoom      = TileZoom,
                                                     tile_enclosure={MinX,MinY,MaxX,MaxY}}) ->
    {continue, {Img, RasterInfo, X + 1, Y, TileZoom},
                fun() ->
                    scan_img(ProfileMod, 
                             Img, 
                             RasterInfo, 
                             #tile_position{current_tile_x = X + 1,
                                            current_tile_y = Y,
                                            tile_zoom      = TileZoom,
                                            tile_enclosure = {MinX,MinY, MaxX, MaxY}})
                end}.

