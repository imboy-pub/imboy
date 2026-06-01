%%% @doc group_member_logic T2.3 领域事件契约 eunit 测试。
%%% 验证 join_group/leave 退化外壳产出正确的成员变更领域事件
%%% （{member_added|member_removed, Gid, Uid}），替代原直调通知。
%%% 通知投递由 group_event_handler（T2.0h）消费事件完成，此处只验证 publish 契约。
-module(group_member_logic_event_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% join_group 成功 → publish {member_added, Gid, Uid}。
join_publishes_member_added_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 999} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {imboy_domain_event, [
                {'publish', 1, fun(Events) ->
                    ?assertEqual([{member_added, 1, 100}], Events),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_member_logic:join_group(<<"invite">>, 100, 1, #{}))
        end
    ).

%% leave 成功 → publish {member_removed, Gid, Uid}。
leave_publishes_member_removed_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'leave', 4, fun(_Conn, _Uid, _Gid, _CurrentUid) -> {ok, 999, #{}} end}
            ]},
            {group_ds, [
                {'leave', 2, fun(_Uid, _Gid) -> ok end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {imboy_domain_event, [
                {'publish', 1, fun(Events) ->
                    ?assertEqual([{member_removed, 1, 100}], Events),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_member_logic:leave(100, 1, 100))
        end
    ).
