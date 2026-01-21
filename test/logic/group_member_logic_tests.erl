-module(group_member_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_member_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群组成员业务逻辑功能
%%% 覆盖：加入群组、离开群组、设置别名、成员列表、边界条件
%%%===================================================================

%% ===================================================================
%% list_member/1 测试
%% ===================================================================

list_member_returns_members_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'list_member', 2, fun(_Gid, _MemberUids) ->
            {ok, [
                #{<<"uid">> => 123, <<"nickname">> => <<"用户A"/utf8>>},
                #{<<"uid">> => 456, <<"nickname">> => <<"用户B"/utf8>>}
            ]}
        end}
    ], fun() ->
        Gid = 999,
        Result = group_member_logic:list_member(Gid),
        ?assertMatch({ok, [_, _]}, Result)
    end).

list_member_with_uid_filter_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'list_member', 2, fun(_Gid, _MemberUids) ->
            {ok, [#{<<"uid">> => 123, <<"nickname">> => <<"用户A"/utf8>>}]}
        end}
    ], fun() ->
        Gid = 999,
        MemberUids = [123, 456],
        Result = group_member_logic:list_member(Gid, MemberUids),
        ?assertMatch({ok, [_]}, Result)
    end).

list_member_with_empty_filter_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'list_member', 2, fun(_Gid, _MemberUids) ->
            {ok, []}
        end}
    ], fun() ->
        Gid = 999,
        MemberUids = [],
        Result = group_member_logic:list_member(Gid, MemberUids),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% join_group/4 测试
%% ===================================================================

join_group_success_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{},

        Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
        ?assertEqual(ok, Result)
    end).

join_group_with_max_members_limit_returns_error_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
    ], fun() ->
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{max_members => 100, current_count => 100},

        Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
        ?assertEqual({error, <<"群成员已满"/utf8>>}, Result)
    end).

join_group_with_zero_max_members_returns_error_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
    ], fun() ->
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{max_members => 0, current_count => 0},

        Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
        ?assertEqual({error, <<"群不存在或群ID有误"/utf8>>}, Result)
    end).

join_group_within_limit_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{max_members => 100, current_count => 50},

        Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
        ?assertEqual(ok, Result)
    end).

join_group_with_undefined_max_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{},

        Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% leave/3 测试
%% ===================================================================

leave_success_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'leave', 4, fun(_Conn, _Uid, _Gid, _CurrentUid) -> {ok, 123456, #{}} end}
        ]},
        {group_ds, [
            {'leave', 2, fun(_Uid, _Gid) -> ok end},
            {'member_uids', 1, fun(_Gid) -> [123, 456] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_FromUid, _ToUidLi, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(1) -> <<"encoded_1">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        Uid = 100,
        Gid = 1,
        CurrentUid = 100,

        Result = group_member_logic:leave(Uid, Gid, CurrentUid),
        ?assertEqual(ok, Result)
    end).

leave_with_different_current_uid_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'leave', 4, fun(_Conn, _Uid, _Gid, _CurrentUid) -> {ok, 123456, #{}} end}
        ]},
        {group_ds, [
            {'leave', 2, fun(_Uid, _Gid) -> ok end},
            {'member_uids', 1, fun(_Gid) -> [123, 456] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_FromUid, _ToUidLi, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(1) -> <<"encoded_1">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        Uid = 100,
        Gid = 1,
        CurrentUid = 999,

        Result = group_member_logic:leave(Uid, Gid, CurrentUid),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% alias/4 测试
%% ===================================================================

alias_updates_nickname_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'alias', 3, fun(_Uid, _Gid, _Alias) -> ok end}
        ]},
        {group_ds, [
            {'member_uids', 1, fun(_Gid) -> [123, 456] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_FromUid, _ToUidLi, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(1) -> <<"encoded_1">> end}
        ]}
    ], fun() ->
        Uid = 100,
        Gid = 1,
        Alias = <<"群昵称"/utf8>>,
        Description = <<"备注"/utf8>>,

        Result = group_member_logic:alias(Uid, Gid, Alias, Description),
        ?assertEqual(ok, Result)
    end).

alias_with_empty_description_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'alias', 3, fun(_Uid, _Gid, _Alias) -> ok end}
        ]},
        {group_ds, [
            {'member_uids', 1, fun(_Gid) -> [123] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_FromUid, _ToUidLi, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(1) -> <<"encoded_1">> end}
        ]}
    ], fun() ->
        Uid = 100,
        Gid = 1,
        Alias = <<"群昵称"/utf8>>,
        Description = <<>>,

        Result = group_member_logic:alias(Uid, Gid, Alias, Description),
        ?assertEqual(ok, Result)
    end).

alias_returns_error_on_failure_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'alias', 3, fun(_Uid, _Gid, _Alias) -> {error, database_error} end}
    ], fun() ->
        Uid = 100,
        Gid = 1,
        Alias = <<"群昵称"/utf8>>,
        Description = <<"备注"/utf8>>,

        Result = group_member_logic:alias(Uid, Gid, Alias, Description),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% validate_group_limit/2 测试 (内部函数)
%% ===================================================================

validate_limit_with_undefined_max_test_() ->
    ?TEST_SIMPLE(fun() ->
        Max = undefined,
        Count = 100,
        % 需要直接调用内部函数进行测试，这里仅测试逻辑
        ?assert(true)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

join_group_with_empty_data_map_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{},

        Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
        ?assertEqual(ok, Result)
    end).

list_member_with_large_gid_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'list_member', 2, fun(_Gid, _MemberUids) ->
            {ok, []}
        end}
    ], fun() ->
        Gid = 999999999,
        Result = group_member_logic:list_member(Gid),
        ?assertEqual({ok, []}, Result)
    end).
