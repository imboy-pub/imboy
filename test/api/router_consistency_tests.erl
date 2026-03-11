-module(router_consistency_tests).

-include_lib("eunit/include/eunit.hrl").

route_handler_modules_exist_test() ->
    MissingModules = missing_route_modules(),
    ?assertEqual([], MissingModules).

open_routes_map_to_route_table_test() ->
    RoutePathSet = route_path_set(),
    MissingPaths = missing_paths(imboy_router:open(), RoutePathSet),
    ?assertEqual([], MissingPaths).

option_routes_map_to_route_table_test() ->
    RoutePathSet = route_path_set(),
    MissingPaths = missing_paths(imboy_router:option(), RoutePathSet),
    ?assertEqual([], MissingPaths).

-spec missing_route_modules() -> [atom()].
missing_route_modules() ->
    lists:usort([
        Handler
        || {_Path, Handler, _State} <- all_routes(),
           not handler_exists(Handler)
    ]).

-spec missing_paths([binary()], sets:set(binary())) -> [binary()].
missing_paths(Paths, RoutePathSet) ->
    [
        Path
        || Path <- Paths,
           not sets:is_element(Path, RoutePathSet)
    ].

-spec route_path_set() -> sets:set(binary()).
route_path_set() ->
    Paths = [
        unicode:characters_to_binary(Path)
        || {Path, _Handler, _State} <- all_routes()
    ],
    sets:from_list(Paths).

-spec all_routes() -> [tuple()].
all_routes() ->
    lists:flatmap(
        fun({_Host, Routes}) ->
            Routes
        end,
        imboy_router:get_routes()
    ).

-spec handler_exists(atom()) -> boolean().
handler_exists(cowboy_static) ->
    true;
handler_exists(Handler) when is_atom(Handler) ->
    case code:ensure_loaded(Handler) of
        {module, Handler} ->
            true;
        _ ->
            false
    end.
