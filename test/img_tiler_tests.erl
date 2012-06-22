-module(img_tiler_tests).
-include("global_grid.hrl").


%% =============================================================================
%% EUnit tests
%% =============================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

calc_tiles_enclosure_test() ->
    {OriginX, OriginY} = {117.0439031173947, 35.138356923616534},
    {PixelSizeX, PixelSizeY} = { 2.0080973914208664e-06, -2.0080973914208664e-06},
    {RasterXSize, RasterYSize} = {10933, 8982},
    ImgInfo = {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize},
    ProfileMod = global_geodetic,
    MT = img_tiler:new(ProfileMod, nil, ImgInfo),
    {MaxZoom, Enclosure} = MT:calc_tiles_enclosure(),
    ?assertEqual({432601, 182219, 432633, 182245}, Enclosure).

-endif.
