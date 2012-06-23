-module(map_profile).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{epsg_code, 0},            % EPSG code of tile map Projection 
     {tile_bounds, 1},          % Returns bounds of the given tile
     {resolution, 1},           % Resolution for given zoom level
     {max_tile_extent, 1},      % Calculate max tile-width or tile-height in specified zoom level
                                % there is different calutaions for different profiles
     {coordinates_to_pixels, 3} % for coordinates_to_tiles which return tile of 
                                % given coordinates in differnent projection or tile profile
    ];
behaviour_info(_Other) ->
    undefined.


