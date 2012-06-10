-module(tile_builder).
-behaviour(gen_server).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ScannerInfo, {Tile, {Tx, Ty, Tz}}) ->
    gen_server:start_link(?MODULE, [ScannerInfo, {Tile, {Tx, Ty, Tz}}], []).

-record(state, {scanner, ref, tile, tileinfo}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{ScannerPid, Ref}, {Tile, {Tx, Ty, Tz}}]) ->
    self() ! start,
    {ok, #state{scanner=ScannerPid,
                ref = Ref,
                tile = Tile, 
                tileinfo={Tx,Ty,Tz}} }.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, State = #state{ref=Ref, tile=Tile, tileinfo={Tx,Ty,Tz}}) ->
    gdal_nif:build_tile(Tile),
    tile_export(Tile, {Tx, Ty, Tz}),
    img_scanner:complete(State#state.scanner, Ref, {Tx,Ty,Tz}),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
tile_export(Tile, {Tx, Ty, Tz}) ->
    SaveTilesToDir = "/tmp/gtiles",
    TilesFileExt = "png",
    TileFilename = filename:join([SaveTilesToDir, 
            integer_to_list(Tz), integer_to_list(Tx), 
            integer_to_list(Ty) ++ "." ++ TilesFileExt]),
    ok = filelib:ensure_dir(TileFilename),
    lager:debug("saved tile(~p)", [TileFilename]),
    gdal_nif:save_tile(Tile, TileFilename),
    ok.
