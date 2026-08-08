-module(msg_c2c_input_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% c2c_input/3（typing 输入状态转发）测试
%%% fire-and-forget：好友且在线直发；离线/非好友静默丢弃。
%%%===================================================================

input_forwards_when_friend_online_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [{'is_friend', 2, fun(7, 5) -> true end}]},
            {user_logic, [{'is_online', 1, fun(7) -> true end}]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(ToId, MsgId, Msg, Type) ->
                    self() ! {sent, ToId, MsgId, maps:get(<<"action">>, Msg), Type},
                    ok
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"7">>,
                <<"action">> => <<"message_input">>,
                <<"payload">> => #{<<"status">> => <<"start">>}
            },
            ?assertEqual(ok, msg_c2c_logic:c2c_input(<<"typ1">>, 5, Data)),
            receive
                {sent, ToId, MsgId, Action, Type} ->
                    ?assertEqual(7, ToId),
                    ?assertEqual(<<"typ1">>, MsgId),
                    ?assertEqual(<<"message_input">>, Action),
                    ?assertEqual(<<"c2c">>, Type)
            after 100 ->
                ?assert(false, "input not forwarded")
            end
        end
    ).

input_forwards_when_online_status_is_hidden_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [{'is_friend', 2, fun(7, 5) -> true end}]},
            {user_logic, [{'is_online', 1, fun(7) -> true end}]},
            {user_setting_ds, [{'chat_state_hide', 1, fun(7) -> true end}]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(ToId, _MsgId, _Msg, _Type) ->
                    self() ! {sent, ToId},
                    ok
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"7">>,
                <<"action">> => <<"message_input">>,
                <<"payload">> => #{<<"status">> => <<"start">>}
            },
            ?assertEqual(ok, msg_c2c_logic:c2c_input(<<"typ-hidden">>, 5, Data)),
            receive
                {sent, ToId} ->
                    ?assertEqual(7, ToId)
            after 100 ->
                ?assert(false, "hidden online status must not block typing delivery")
            end
        end
    ).

input_dropped_when_offline_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [{'is_friend', 2, fun(_, _) -> true end}]},
            {user_logic, [{'is_online', 1, fun(_) -> false end}]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) ->
                    self() ! sent_unexpected,
                    ok
                end}
            ]}
        ],
        fun() ->
            Data = #{<<"to">> => <<"7">>, <<"payload">> => #{}},
            ?assertEqual(ok, msg_c2c_logic:c2c_input(<<"typ2">>, 5, Data)),
            receive
                sent_unexpected -> ?assert(false, "offline should drop")
            after 50 -> ok
            end
        end
    ).

input_dropped_when_not_friend_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [{'is_friend', 2, fun(_, _) -> false end}]},
            {user_logic, [{'is_online', 1, fun(_) -> true end}]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) ->
                    self() ! sent_unexpected,
                    ok
                end}
            ]}
        ],
        fun() ->
            Data = #{<<"to">> => <<"7">>, <<"payload">> => #{}},
            ?assertEqual(ok, msg_c2c_logic:c2c_input(<<"typ3">>, 5, Data)),
            receive
                sent_unexpected -> ?assert(false, "non-friend should drop")
            after 50 -> ok
            end
        end
    ).
