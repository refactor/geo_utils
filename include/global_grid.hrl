-define(MAXZOOMLEVEL, 32).
-define(TILE_SIZE, 256).

-record(w_state, {
    global_projection,
    out_ds          :: reference(),
    tile_size = 256 :: integer()
}).


