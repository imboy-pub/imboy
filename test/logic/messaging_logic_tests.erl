-module(messaging_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%% ARCH-01：offline/2(Req0,State) 已拆分为 handler 层解析 + 本函数纯参数
%% messaging_logic:offline/6，直接调用纯函数，不再需要 cowboy_req/响应 mock。
offline_returns_expected_shape_test_() ->
    ?WITH_MECKS(
        [
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
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end},
                {'count_unread_since', 3, fun(_Uid, _LastMsgAt, _DID) -> 0 end}
            ]},
            {msg_c2g_ds, [
                {'read_msg', 3, fun(_Uid, _Limit, _LastMsgAt) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end},
                {'count_since', 3, fun(_Uid, _LastMsgAt, _DID) -> 0 end}
            ]}
        ],
        fun() ->
            Payload = messaging_logic:offline(12345, 1000, 0, 0, 0, <<>>),
            lists:foreach(
                fun(Type) ->
                    TypeMap = maps:get(Type, Payload),
                    ?assertEqual(false, maps:get(<<"has_more">>, TypeMap)),
                    ?assertEqual(0, maps:get(<<"total">>, TypeMap)),
                    ?assertEqual([], maps:get(<<"list">>, TypeMap))
                end,
                [<<"c2c">>, <<"c2g">>, <<"s2c">>]
            )
        end
    ).

route_ws_delegates_c2c_to_existing_logic_modules_test_() ->
    ?WITH_MECK(
        msg_c2c_logic,
        [
            {'c2c', 3, fun(MsgId, CurrentUid, Data) ->
                self() ! {c2c_routed, MsgId, CurrentUid, Data},
                ok
            end}
        ],
        fun() ->
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
        end
    ).

%% ARCH-01：offline_ack/2(Req0,State) 已拆分为 handler 层解析（含 type 小写化）
%% + 本函数纯参数 messaging_logic:offline_ack/4，Type 传入时已是小写。
offline_ack_uses_messaging_boundary_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 5, fun(_Level, _Fmt, _Args, _Module, _Line) -> ok end}
            ]},
            {msg_c2c_repo, [
                {'delete_by_msg_ids_and_to_id', 2, fun(MsgIds, 12345) ->
                    self() ! {acked_ids, MsgIds},
                    2
                end}
            ]}
        ],
        fun() ->
            {ok, Payload} = messaging_logic:offline_ack(
                12345, <<"c2c">>, [<<"m1">>, <<"m2">>], <<>>
            ),
            ?assertEqual(
                {acked_ids, [<<"m1">>, <<"m2">>]},
                receive
                    Acked -> Acked
                after 1000 ->
                    timeout
                end
            ),
            ?assertEqual(<<"c2c">>, maps:get(<<"type">>, Payload)),
            ?assertEqual(2, maps:get(<<"processed_count">>, Payload)),
            ?assertEqual(2, maps:get(<<"msg_ids_count">>, Payload))
        end
    ).

%% ARCH-01：reaction_add/2(Req0,State) 已拆分为 handler 层解析（elib_req:body）
%% + 本函数纯参数 messaging_logic:reaction_add/4。
reaction_add_uses_messaging_boundary_test_() ->
    ?WITH_MECK(
        msg_reaction_logic,
        [
            {'add', 4, fun(<<"msg-1">>, <<"c2c">>, 12345, Emoji) ->
                self() ! {reaction_added, Emoji},
                {ok, #{<<"user_id">> => 12345, <<"created_at">> => 1700000000}}
            end}
        ],
        fun() ->
            Emoji = <<240, 159, 145, 141>>,
            {ok, Payload, _Msg} = messaging_logic:reaction_add(
                12345, <<"msg-1">>, <<"c2c">>, Emoji
            ),
            ?assertEqual(
                {reaction_added, Emoji},
                receive
                    Added -> Added
                after 1000 ->
                    timeout
                end
            ),
            ?assertEqual(<<"msg-1">>, maps:get(<<"msg_id">>, Payload)),
            ?assertEqual(12345, maps:get(<<"user_id">>, Payload))
        end
    ).
