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
