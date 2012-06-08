-define(MAXZOOMLEVEL, 32).
-define(TILE_SIZE, 256).

-record(w_state, {
    tile_size = 256 :: integer(),
    out_srs_wkt     :: string()
}).


