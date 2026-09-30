-module(enterprise_internal_id_validation_tests).
-include_lib("eunit/include/eunit.hrl").

-define(MAX, 9223372036854775807).

id_validation_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun path_ranges/0,
        fun valid_bindings/0,
        fun authentication_precedence/0,
        fun query_ranges/0,
        fun direct_handler_ranges/0,
        fun seat_query_ranges/0
    ]}.

setup() ->
    lists:foreach(
        fun(M) -> meck:new(M, [non_strict, no_link]) end,
        [cowboy_req, enterprise_internal_auth, elib_pg, elib_log]
    ),
    meck:expect(cowboy_req, method, fun(Req) -> maps:get(method, Req) end),
    meck:expect(cowboy_req, path, fun(Req) -> maps:get(path, Req) end),
    meck:expect(cowboy_req, headers, fun(Req) -> maps:get(headers, Req) end),
    meck:expect(cowboy_req, header, fun(Key, Req) ->
        maps:get(Key, maps:get(headers, Req), undefined)
    end),
    meck:expect(cowboy_req, parse_qs, fun(Req) -> maps:to_list(maps:get(qs, Req, #{})) end),
    meck:expect(cowboy_req, reply, fun(Status, _Headers, Body, Req) ->
        Req#{status => Status, body => Body}
    end),
    meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
    meck:expect(elib_pg, with_tx, fun(_) -> error(unexpected_database_access) end),
    meck:expect(enterprise_internal_auth, decide, fun(_, _, _, _) ->
        {ok, #{organization_id => 10, application_id => 20}}
    end).

cleanup(_) ->
    lists:foreach(
        fun meck:unload/1,
        [cowboy_req, enterprise_internal_auth, elib_pg, elib_log]
    ).

request(Bindings) ->
    #{
        method => <<"GET">>,
        path => <<"/api/internal/v1/groups/1">>,
        headers => #{<<"idempotency-key">> => <<"synthetic-id-check">>},
        bindings => Bindings
    }.

invalid_ids() ->
    [
        <<>>,
        <<"0">>,
        <<"-1">>,
        <<"+1">>,
        <<"1.0">>,
        <<" 1">>,
        <<"1a">>,
        <<"9223372036854775808">>,
        binary:copy(<<"9">>, 2048)
    ].

assert_invalid(Reply) ->
    ?assertEqual(400, maps:get(status, Reply)),
    Body = jsone:decode(maps:get(body, Reply)),
    ?assertEqual(<<"invalid_request">>, maps:get(<<"code">>, maps:get(<<"error">>, Body))).

path_ranges() ->
    lists:foreach(
        fun(Key) ->
            lists:foreach(
                fun(Value) ->
                    {stop, Reply} = enterprise_internal_middleware:execute(
                        request(#{Key => Value}), #{handler_opts => #{}}
                    ),
                    assert_invalid(Reply)
                end,
                invalid_ids()
            )
        end,
        [group_id, workspace_id, project_id, channel_id, business_identity_id]
    ),
    ?assertEqual(0, meck:num_calls(elib_pg, with_tx, '_')).

valid_bindings() ->
    lists:foreach(
        fun(Key) ->
            lists:foreach(
                fun({Value, Expected}) ->
                    Req = request(#{
                        Key => Value,
                        opaque => <<"not-a-number">>,
                        delivery_id => <<"intbe02-dlv-0001">>
                    }),
                    {ok, Req, Env} = enterprise_internal_middleware:execute(Req, #{
                        handler_opts => #{action => detail}
                    }),
                    Opts = maps:get(handler_opts, Env),
                    ?assertEqual(Expected, maps:get(Key, Opts)),
                    ?assertEqual(<<"not-a-number">>, maps:get(opaque, Opts)),
                    ?assertEqual(<<"intbe02-dlv-0001">>, maps:get(delivery_id, Opts)),
                    ?assertEqual(detail, maps:get(action, Opts)),
                    ?assertEqual(20, maps:get(application_id, maps:get(enterprise_internal, Opts)))
                end,
                [{<<"1">>, 1}, {integer_to_binary(?MAX), ?MAX}, {<<"0001">>, 1}]
            )
        end,
        [group_id, workspace_id, project_id, channel_id, business_identity_id]
    ),
    lists:foreach(
        fun(DeliveryId) ->
            Req = request(#{delivery_id => DeliveryId}),
            {ok, Req, Env} = enterprise_internal_middleware:execute(Req, #{handler_opts => #{}}),
            ?assertEqual(DeliveryId, maps:get(delivery_id, maps:get(handler_opts, Env)))
        end,
        [<<"0001">>, <<"9223372036854775808">>, <<"intbe02-dlv-0001">>]
    ).

authentication_precedence() ->
    meck:expect(enterprise_internal_auth, decide, fun(_, _, _, _) -> {error, invalid_credential} end),
    {stop, Reply} = enterprise_internal_middleware:execute(request(#{group_id => <<"0">>}), #{}),
    ?assertEqual(401, maps:get(status, Reply)).

query_ranges() ->
    lists:foreach(
        fun({Module, Action}) ->
            lists:foreach(
                fun(Value) ->
                    Req = (request(#{}))#{qs => #{<<"workspace_id">> => Value}},
                    {ok, Reply, _} = Module:init(Req, #{
                        action => Action, enterprise_internal => #{}
                    }),
                    assert_invalid(Reply)
                end,
                invalid_ids()
            ),
            ?assertEqual(0, meck:num_calls(elib_pg, with_tx, '_'))
        end,
        [{enterprise_project_handler, projects}, {enterprise_channel_handler, channels}]
    ).

direct_handler_ranges() ->
    meck:expect(elib_pg, with_tx, fun(Fun) -> Fun(synthetic_connection) end),
    lists:foreach(
        fun({Module, Action, Key}) ->
            lists:foreach(
                fun(Value) ->
                    {ok, Reply, _} = Module:init(
                        request(#{}),
                        #{
                            action => Action,
                            Key => Value,
                            enterprise_internal => #{organization_id => 10}
                        }
                    ),
                    assert_invalid(Reply)
                end,
                invalid_ids() ++ [0, -1, ?MAX + 1]
            )
        end,
        [
            {enterprise_workspace_handler, workspace, workspace_id},
            {enterprise_project_handler, project, project_id},
            {enterprise_channel_handler, channel, channel_id}
        ]
    ).

seat_query_ranges() ->
    lists:foreach(
        fun(Qs) ->
            Req = (request(#{}))#{qs => Qs},
            {ok, Reply, _} = enterprise_cs_seat_handler:init(Req, #{
                action => seats, enterprise_internal => #{}
            }),
            assert_invalid(Reply)
        end,
        [
            #{<<"limit">> => <<"0">>},
            #{<<"limit">> => <<"101">>},
            #{<<"limit">> => true},
            #{<<"organization_id">> => <<"10">>}
        ]
    ),
    ?assertEqual(0, meck:num_calls(elib_pg, with_tx, '_')).
