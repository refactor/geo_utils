-module(img_scanner).

-export([scan_img/2]).

-record(tile_position, {current_tile_x, current_tile_y, 
                        tile_zoom :: byte(),
                        tile_enclosure :: global_grid:encluse()}).

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

