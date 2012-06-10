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

-record(tile_position, {current_tile_x, current_tile_y, 
                        tile_zoom :: byte(),
                        tile_enclosure :: global_grid:encluse()}).

start_link(TileMapProfileMod, ImgFileName) ->
    gen_fsm:start_link(?MODULE, {TileMapProfileMod, ImgFileName}, []).

init({ProfileMod, ImgFileName}) ->
    {ok, copyouting, #w_state{map_profile=ProfileMod, 
                              img_filename=ImgFileName}}.

copyouting({continue, {Img, RasterInfo, TileX, TileY, TileZoom}, Continuation}, State) ->
    {ok, TileRawdata} = global_grid:copyout_tile_for(State#w_state.map_profile, TileY, TileX, TileZoom, Img, RasterInfo),
    {next_state, copyouting, State};
copyouting(Event, State) ->
    {next_state, copyouting, State}.

listening(Event, State) ->
    {next_state, listening, State}.

handle_event(debug, StateName, StateData) ->
    io:format("handle EVENT~n"),
    lager:info("~p", [StateData]),
    {next_state, StateName, StateData};
handle_event(Event, StateName, StateData) ->
    {next_state, copyouting, StateData}.
    
handle_sync_event(Event, From, StateName, StateData) ->
    {next_state, copyouting, StateData}.

handle_info(start, StateName, StateData = #w_state{map_profile=ProfileMod, img_filename=ImgFileName}) ->
    gen_fsm:send_event(self(), img_scanner:scan_img(ProfileMod, ImgFileName)),
    {next_state, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
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

scan_img(ProfileMod, Img, RasterInfo, TilePosition) ->
    ok.

