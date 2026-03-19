-module(messaging_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

req_mock() ->
    cowboy_req_h:new(#{}).

handle_rest_action_offline_returns_expected_shape_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(_Param, _Req, Default) -> {ok, Default} end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 2, fun(_Ts, _Unit) -> <<"1970-01-01T00:00:00Z">> end}
        ]},
        {msg_c2c_repo, [
            {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
        ]},
        {msg_c2g_timeline_repo, [
            {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
        ]},
        {msg_s2c_repo, [
            {'tablename', 0, fun() -> <<"public.msg_s2c">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, [#{<<"count">> => 0}]} end}
        ]},
        {msg_c2c_ds, [
            {'read_msg', 3, fun(_Uid, _Limit, _LastMsgAt) -> [] end}
        ]},
        {msg_c2g_ds, [
            {'read_msg', 3, fun(_Uid, _Limit, _LastMsgAt) -> [] end}
        ]},
        {msg_s2c_ds, [
            {'read_msg', 3, fun(_Uid, _Limit, _LastMsgAt) -> [] end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Req1 = messaging_logic:handle_rest_action(offline, req_mock(), #{current_uid => 12345}),
        ?assertEqual(req_ok, Req1),

        Payload = receive
            {resp_data, Data} -> Data
        after 1000 ->
            timeout
        end,
        ?assertNotEqual(timeout, Payload),
        lists:foreach(
            fun(Type) ->
                TypeMap = maps:get(Type, Payload),
                ?assertEqual(false, maps:get(<<"has_more">>, TypeMap)),
                ?assertEqual(0, maps:get(<<"total">>, TypeMap)),
                ?assertEqual([], maps:get(<<"list">>, TypeMap))
            end,
            [<<"c2c">>, <<"c2g">>, <<"s2c">>]
        )
    end).

route_ws_delegates_c2c_to_existing_logic_modules_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c', 3, fun(MsgId, CurrentUid, Data) ->
            self() ! {c2c_routed, MsgId, CurrentUid, Data},
            ok
        end}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 1001,
        Data = #{<<"payload">> => #{<<"text">> => <<"hello">>}},
        Type = <<"C2C">>,
        OriginalMsg = <<"{\"payload\":{\"text\":\"hello\"}}">>,

        ?assertEqual(ok, messaging_logic:route_ws(MsgId, CurrentUid, Data, Type, OriginalMsg)),
        ?assertEqual(
            {c2c_routed, MsgId, CurrentUid, Data},
            receive
                Routed -> Routed
            after 1000 ->
                timeout
            end
        )
    end).

handle_rest_action_offline_ack_uses_messaging_boundary_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 12345 end}
        ]},
        {elib_log, [
            {'internal_log', 5, fun(_Level, _Fmt, _Args, _Module, _Line) -> ok end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"type">> => <<"C2C">>,
                    <<"msg_ids">> => [<<"m1">>, <<"m2">>]
                }
            end}
        ]},
        {msg_c2c_repo, [
            {'delete_by_msg_ids_and_to_id', 2, fun(MsgIds, 12345) ->
                self() ! {acked_ids, MsgIds},
                2
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Req1 = messaging_logic:handle_rest_action(offline_ack, req_mock(), #{current_uid => 12345}),
        ?assertEqual(req_ok, Req1),
        ?assertEqual(
            {acked_ids, [<<"m1">>, <<"m2">>]},
            receive
                Acked -> Acked
            after 1000 ->
                timeout
            end
        ),
        Payload = receive
            {resp_data, Data} -> Data
        after 1000 ->
            timeout
        end,
        ?assertEqual(<<"c2c">>, maps:get(<<"type">>, Payload)),
        ?assertEqual(2, maps:get(<<"processed_count">>, Payload)),
        ?assertEqual(2, maps:get(<<"msg_ids_count">>, Payload))
    end).

handle_rest_action_reaction_add_uses_messaging_boundary_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'body', 2, fun(Req, _Opts) ->
                {ok,
                 #{
                     <<"msg_id">> => <<"msg-1">>,
                     <<"msg_type">> => <<"c2c">>,
                     <<"emoji">> => <<240, 159, 145, 141>>
                 },
                 Req}
            end}
        ]},
        {msg_reaction_logic, [
            {'add', 4, fun(<<"msg-1">>, <<"c2c">>, 12345, Emoji) ->
                self() ! {reaction_added, Emoji},
                {ok, #{<<"user_id">> => 12345, <<"created_at">> => 1700000000}}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Data, _Msg) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Req1 = messaging_logic:handle_rest_action(reaction_add, req_mock(), #{current_uid => 12345}),
        ?assertEqual(req_ok, Req1),
        ?assertEqual(
            {reaction_added, <<240, 159, 145, 141>>},
            receive
                Added -> Added
            after 1000 ->
                timeout
            end
        ),
        Payload = receive
            {resp_data, Data} -> Data
        after 1000 ->
            timeout
        end,
        ?assertEqual(<<"msg-1">>, maps:get(<<"msg_id">>, Payload)),
        ?assertEqual(12345, maps:get(<<"user_id">>, Payload))
    end).
