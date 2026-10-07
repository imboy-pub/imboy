%%% @doc group_event_handler 领域事件订阅者 eunit 测试。
%%% 验证 member_added/member_removed 事件桥接为等价的 S2C 通知投递，
%%% 未知事件被静默忽略。副作用模块（group_ds/user_ds/msg_s2c_ds）经 meck。
-module(group_event_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% member_added → group_member_join 通知（nosave）。
member_added_triggers_join_notice_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(10) -> [2, 3] end}
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
                    ?assertNot(maps:is_key(<<"user_id_sum">>, Payload)),
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
                {'member_uids', 1, fun(10) -> [2, 3] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(From, _To, Action, _MsgType, _E2EE, Payload, Save) ->
                    ?assertEqual(1, From),
                    ?assertEqual(<<"group_member_leave">>, Action),
                    ?assertEqual(save, Save),
                    ?assertEqual(1, maps:get(<<"leave_uid">>, Payload)),
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

%% owner_transferred → group_transfer 通知（save，离线可补），payload 含新旧群主。
%% Q05/F07/AC10：转让是四场景中唯一"零通知"缺口，本测试锁定补线后的契约。
owner_transferred_triggers_group_transfer_notice_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(10) -> [1, 2, 3] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(From, To, Action, _MsgType, _E2EE, Payload, Save) ->
                    ?assertEqual(1, From),
                    ?assertEqual([1, 2, 3], To),
                    ?assertEqual(<<"group_transfer">>, Action),
                    ?assertEqual(save, Save),
                    ?assertEqual(10, maps:get(<<"gid">>, Payload)),
                    ?assertEqual(1, maps:get(<<"from_owner_uid">>, Payload)),
                    ?assertEqual(3, maps:get(<<"to_owner_uid">>, Payload)),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{}},
                group_event_handler:handle_event({owner_transferred, 10, 1, 3}, #{})
            ),
            %% 恰一次直发（修复前本场景零通知，此断言必失败）
            ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
        end
    ).

%% 被移除者本人追加进 group_member_leave 接收列表（Q05/F07：踢人感知 +
%% 主动退群的多端同步；App 端 memberLeave 已有 leave_uid==self 分支）。
member_removed_notifies_removed_user_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(10) -> [2, 3] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_From, To, _Action, _MsgType, _E2EE, _Payload, Save) ->
                    ?assertEqual([2, 3, 1], To),
                    ?assertEqual(save, Save),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{}},
                group_event_handler:handle_event({member_removed, 10, 1}, #{})
            ),
            %% 被移除者必须收到（修复前不在接收列表，To 断言不会执行）
            ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
        end
    ).

%% 缓存竞态（member_uids 仍含被移除者）不得产生重复接收者。
member_removed_stale_cache_no_duplicate_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(10) -> [1, 2, 3] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_From, To, _Action, _MsgType, _E2EE, _Payload, _Save) ->
                    ?assertEqual([1, 2, 3], To),
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

%% 未知事件被静默忽略，状态不变、无副作用（group_dissolved 仍走直发，不经总线）。
unknown_event_is_ignored_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {ok, #{}},
            group_event_handler:handle_event({group_dissolved, 10}, #{})
        )
    end).
