-define(MAXZOOMLEVEL, 32).
-define(TILE_SIZE, 256).

-record(w_state, {
    map_profile,    % tile map profile, such as global-geodetic or global-mercator module
    img_filename    :: string(),
    tile_size = 256 :: integer()
}).


