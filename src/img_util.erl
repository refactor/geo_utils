-module(img_util).

-export([
%        calc_tminmax/1
    ]).



%-spec calc_tminmax(rasterinfo()) -> list().
%calc_tminmax(RasterInfo) ->
%    Enclosure = get_enclosure(RasterInfo),
%    calc_tminmax(Enclosure, [], 0).



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

calc_tminmax(_Enclosure, Tminmax, 32) ->
    lists:reverse(Tminmax);
calc_tminmax({Ominx, Ominy, Omaxx, Omaxy} = Enclosure, Tminmax, Zoom) ->
    {Tminx, Tminy} = mercator_tiles:meters_to_tile( Ominx, Ominy, Zoom ),
    {Tmaxx, Tmaxy} = mercator_tiles:meters_to_tile( Omaxx, Omaxy, Zoom ),
    Z = trunc(math:pow(2, Zoom)) - 1,
    EnclosureOfZoom = {
        max(0, Tminx), max(0, Tminy), 
        min(Z, Tmaxx), min(Z, Tmaxy)
    },
    calc_tminmax(Enclosure, [EnclosureOfZoom|Tminmax], Zoom + 1).


