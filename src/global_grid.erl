-module(global_grid).

-include("global_grid.hrl").

-export([behaviour_info/1]).

-export([zoom_for_pixelsize/3]).

-export_type([world_state/0, bound/0, bandregion/0, rasterinfo/0]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init_world_state, 0},
     {tile_bounds, 3},
     {zoom_for_pixelsize, 1},
     {resolution, 1},
     {epsg_code, 0}];
behaviour_info(_Other) ->
    undefined.


-type world_state() :: #w_state{}.

%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = Bound
-type bound() :: {LeftTopX::float(), LeftTopY::float(), RightBottomX::float(), RightBottom::float()}.

%% XOffset: the pixel offset to the top left corner of the region of the band to be accessed
%% YOffset: The line offset to the top left corner of the region of the band to be accessed. 
%% XSize: The width of the region of the band to be accessed in pixels.
%% YSize: The height of the region of the band to be accessed in lines
-type bandregion() :: {XOffset::non_neg_integer(), YOffset::non_neg_integer(), XSize::non_neg_integer(), YSize::non_neg_integer()}.

%% {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize},
-type rasterinfo() :: {float(), float(), float(), float(), non_neg_integer(), non_neg_integer()}.


%% @doc For given dataset and query in cartographic coordinates returns parameters for ReadRaster() in 
%% raster coordinates and x/y shifts (for border tiles). If the querysize is not given, the extent is 
%% returned in the native resolution of dataset ds.
%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = _Bound
-spec geo_query(rasterinfo(), bound(), non_neg_integer()) -> {bandregion(), bandregion()}.
geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize}, {Ulx, Uly, Lrx, Lry}, QuerySize) ->
    Rx = trunc( (Ulx - OriginX) / PixelSizeX + 0.001 ),
    Ry = trunc( (Uly - OriginY) / PixelSizeY + 0.001 ),
    Rxsize = trunc( (Lrx - Ulx) / PixelSizeX + 0.5 ),
    Rysize = trunc( (Lry - Uly) / PixelSizeY + 0.5 ),

    {NewRx, NewWx, ResWxsize, ResRxsize} = adjust_byedge(Rx, Rxsize, RasterXSize, QuerySize),
    {NewRy, NewWy, ResWysize, ResRysize} = adjust_byedge(Ry, Rysize, RasterYSize, QuerySize),

    {{NewRx, NewRy, ResRxsize, ResRysize}, {NewWx, NewWy, ResWxsize, ResWysize}}.


%% ===================================================================
%% protected functions
%% ===================================================================
-spec zoom_for_pixelsize(Resolution::fun(), PixelSize::float(), I::byte()) -> byte().
zoom_for_pixelsize(Resolution, PixelSize, I) ->
    R = Resolution(I),
    if 
        PixelSize > R ->
            cfi(I);
        I < ?MAXZOOMLEVEL ->
            zoom_for_pixelsize(Resolution, PixelSize, I + 1);
        true ->
            I
    end.

cfi(I) ->
    case I of
        0 -> 0;  % We don't want to scale up
        _ -> I - 1
    end.

%% ===================================================================
%% private functions
%% ===================================================================

%% @doc Coordinates should not go out of the bounds of the raster
-spec adjust_byedge(integer(), integer(), non_neg_integer(), non_neg_integer()) -> bandregion().
adjust_byedge(R, Rsize, RasterSize, QuerySize) ->
    if
        QuerySize == 0 ->
            Wsize0 = Rsize;
        true ->
            Wsize0 = QuerySize
    end,

    {NewR, NewW, NewWsize, NewRsize} = 
        if R < 0 ->
                Rshift = abs(R),
                W = trunc( Wsize0 * (Rshift / Rsize) ),
                Wsize = Wsize0 - W,
                {0, W, Wsize, Rsize - trunc(Rsize * (Rshift / Rsize)) };
            true ->
                {R, 0, Wsize0, Rsize}
        end,

    {ResWsize, ResRsize} = 
        if
            R + Rsize > RasterSize ->
                {trunc( NewWsize * (RasterSize - NewR) / NewRsize), RasterSize - NewR};
            true ->
                {NewWsize, NewRsize}
        end,
    {NewR, NewW, ResWsize, ResRsize}.


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

geo_quert_test() ->
    {OriginX, OriginY} = {13024084.000533571, 4184269.256414418},
    {PixelSizeX, PixelSizeY} = {0.24473611762142541, -0.24473611762142541},
    {RasterXSize, RasterYSize} = {60352, 62961},
    MinMaxBound = {865067, 633770, 865453, 633367},
    {Rb, Wb} = geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize}, MinMaxBound, 0),
    ?assertEqual({0, 14507459, -49680575, -14444498}, Rb),
    ?assertEqual({49682152, 0, -49680575, -14444498}, Wb).

-endif.
