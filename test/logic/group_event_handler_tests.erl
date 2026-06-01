%%% @doc group_event_handler 领域事件订阅者 eunit 测试。
%%% 验证 member_added/member_removed 事件桥接为等价的 S2C 通知投递，
%%% 未知事件被静默忽略。副作用模块（group_ds/user_ds/msg_s2c_ds）经 meck。
-module(group_event_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% member_added → group_member_join 通知（nosave），payload 含 user/sum。
member_added_triggers_join_notice_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(10) -> [2, 3] end},
                {'get_user_id_sum', 1, fun(10) -> 5 end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(1, _Col) ->
                    #{
                        <<"nickname">> => <<"Bob">>,
                        <<"avatar">> => <<"a.png">>,
                        <<"account">> => <<"bob">>
                    }
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(From, To, Action, _MsgType, _E2EE, Payload, Save) ->
                    ?assertEqual(1, From),
                    ?assertEqual([2, 3], To),
                    ?assertEqual(<<"group_member_join">>, Action),
                    ?assertEqual(nosave, Save),
                    ?assertEqual(10, maps:get(<<"gid">>, Payload)),
                    ?assertEqual(5, maps:get(<<"user_id_sum">>, Payload)),
                    ?assertEqual(<<"Bob">>, maps:get(<<"nickname">>, Payload)),
                    ?assertEqual(<<"bob">>, maps:get(<<"account">>, Payload)),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{}},
                group_event_handler:handle_event({member_added, 10, 1}, #{})
            )
        end
    ).

%% member_removed → group_member_leave 通知（save），payload 含 leave_uid。
member_removed_triggers_leave_notice_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(10) -> [2, 3] end},
                {'get_user_id_sum', 1, fun(10) -> 7 end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(From, _To, Action, _MsgType, _E2EE, Payload, Save) ->
                    ?assertEqual(1, From),
                    ?assertEqual(<<"group_member_leave">>, Action),
                    ?assertEqual(save, Save),
                    ?assertEqual(1, maps:get(<<"leave_uid">>, Payload)),
                    ?assertEqual(7, maps:get(<<"user_id_sum">>, Payload)),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{}},
                group_event_handler:handle_event({member_removed, 10, 1}, #{})
            )
        end
    ).

%% 未知事件被静默忽略，状态不变、无副作用。
unknown_event_is_ignored_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {ok, #{}},
            group_event_handler:handle_event({group_dissolved, 10}, #{})
        )
    end).
