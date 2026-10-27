-module(msg_history_backfill_route_tests).

-include_lib("eunit/include/eunit.hrl").

flutter_backfill_route_test_() ->
    [
        {binary_to_list(Query), fun() ->
            ?assertMatch(
                {ok, _, #{handler := msg_handler, handler_opts := #{action := history_backfill}}},
                route(<<"/api/v1/msg">>, Query)
            )
        end}
     || Query <- [
            <<"action=history_backfill&chat_type=c2c&peer_id=102&after_seq=0&limit=50">>,
            <<"chat_type=c2c&peer_id=102&after_seq=0&limit=50">>
        ]
    ].

history_route_keeps_its_action_test() ->
    ?assertMatch(
        {ok, _, #{handler := msg_handler, handler_opts := #{action := history}}},
        route(<<"/api/v1/msg/history">>, <<"action=history_backfill&chat_type=c2c&peer_id=102">>)
    ).

backfill_uses_existing_required_authentication_test() ->
    {ok, _, #{handler_opts := Options}} = route(<<"/api/v1/msg">>, <<>>),
    ?assertEqual(#{action => history_backfill}, Options),
    ?assertNot(lists:member(<<"/api/v1/msg">>, imboy_router:open())),
    ?assertNot(lists:member(<<"/api/v1/msg">>, imboy_router:option())).

route(Path, Query) ->
    Host =
        case config_ds:env(host, '_') of
            '_' -> <<"localhost">>;
            Value when is_binary(Value) -> Value;
            Value when is_list(Value) -> list_to_binary(Value)
        end,
    Request = #{
        host => Host,
        path => Path,
        qs => Query,
        method => <<"GET">>,
        pid => self(),
        streamid => 1,
        headers => #{}
    },
    Dispatch = cowboy_router:compile(imboy_router:get_routes()),
    cowboy_router:execute(Request, #{dispatch => Dispatch}).
