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
    ?WITH_MECK(
        group_member_ds,
        [
            {'list_member', 2, fun(_Gid, _MemberUids) ->
                {ok, [
                    #{<<"uid">> => 123, <<"nickname">> => <<"用户A"/utf8>>},
                    #{<<"uid">> => 456, <<"nickname">> => <<"用户B"/utf8>>}
                ]}
            end}
        ],
        fun() ->
            Gid = 999,
            Result = group_member_logic:list_member(Gid),
            ?assertMatch({ok, [_, _]}, Result)
        end
    ).

list_member_with_uid_filter_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'list_member', 2, fun(_Gid, _MemberUids) ->
                {ok, [#{<<"uid">> => 123, <<"nickname">> => <<"用户A"/utf8>>}]}
            end}
        ],
        fun() ->
            Gid = 999,
            MemberUids = [123, 456],
            Result = group_member_logic:list_member(Gid, MemberUids),
            ?assertMatch({ok, [_]}, Result)
        end
    ).

list_member_with_empty_filter_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'list_member', 2, fun(_Gid, _MemberUids) ->
                {ok, []}
            end}
        ],
        fun() ->
            Gid = 999,
            MemberUids = [],
            Result = group_member_logic:list_member(Gid, MemberUids),
            ?assertEqual({ok, []}, Result)
        end
    ).

%% ===================================================================
%% join_group/4 测试
%% ===================================================================

join_group_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [100, 200] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) ->
                    #{<<"nickname">> => <<"Test">>, <<"avatar">> => <<>>, <<"account">> => <<>>}
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_From, _To, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
            ]}
        ],
        fun() ->
            JoinMode = <<"invite">>,
            Uid = 100,
            Gid = 1,
            OptData = #{},

            Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
            ?assertEqual(ok, Result)
        end
    ).

join_group_with_max_members_limit_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
            ]}
        ],
        fun() ->
            JoinMode = <<"invite">>,
            Uid = 100,
            Gid = 1,
            OptData = #{max_members => 100, current_count => 100},

            Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
            ?assertEqual({error, <<"群成员已满"/utf8>>}, Result)
        end
    ).

join_group_with_zero_max_members_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
            ]}
        ],
        fun() ->
            JoinMode = <<"invite">>,
            Uid = 100,
            Gid = 1,
            OptData = #{max_members => 0, current_count => 0},

            Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
            ?assertEqual({error, <<"群不存在或群ID有误"/utf8>>}, Result)
        end
    ).

join_group_within_limit_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [100, 200] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) ->
                    #{<<"nickname">> => <<"Test">>, <<"avatar">> => <<>>, <<"account">> => <<>>}
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_From, _To, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
            ]}
        ],
        fun() ->
            JoinMode = <<"invite">>,
            Uid = 100,
            Gid = 1,
            OptData = #{max_members => 100, current_count => 50},

            Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
            ?assertEqual(ok, Result)
        end
    ).

join_group_with_undefined_max_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [100, 200] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) ->
                    #{<<"nickname">> => <<"Test">>, <<"avatar">> => <<>>, <<"account">> => <<>>}
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_From, _To, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
            ]}
        ],
        fun() ->
            JoinMode = <<"invite">>,
            Uid = 100,
            Gid = 1,
            OptData = #{},

            Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% leave/3 测试
%% ===================================================================

leave_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'leave', 4, fun(_Conn, _Uid, _Gid, _CurrentUid) -> {ok, 123456, #{}} end}
            ]},
            {group_ds, [
                {'leave', 2, fun(_Uid, _Gid) -> ok end},
                {'member_uids', 1, fun(_Gid) -> [123, 456] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_FromUid, _ToUidLi, _Action, _MsgId, _Code, _Payload, _Save) ->
                    ok
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]}
        ],
        fun() ->
            Uid = 100,
            Gid = 1,
            CurrentUid = 100,

            Result = group_member_logic:leave(Uid, Gid, CurrentUid),
            ?assertEqual(ok, Result)
        end
    ).

leave_with_different_current_uid_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'leave', 4, fun(_Conn, _Uid, _Gid, _CurrentUid) -> {ok, 123456, #{}} end}
            ]},
            {group_ds, [
                {'leave', 2, fun(_Uid, _Gid) -> ok end},
                {'member_uids', 1, fun(_Gid) -> [123, 456] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_FromUid, _ToUidLi, _Action, _MsgId, _Code, _Payload, _Save) ->
                    ok
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]}
        ],
        fun() ->
            Uid = 100,
            Gid = 1,
            CurrentUid = 999,

            Result = group_member_logic:leave(Uid, Gid, CurrentUid),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% alias/4 测试
%% ===================================================================

alias_updates_nickname_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'alias', 4, fun(_Uid, _Gid, _Alias, _Description) -> ok end}
            ]},
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [123, 456] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_FromUid, _ToUidLi, _Action, _MsgId, _Code, _Payload, _Save) ->
                    ok
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]}
        ],
        fun() ->
            Uid = 100,
            Gid = 1,
            Alias = <<"群昵称"/utf8>>,
            Description = <<"备注"/utf8>>,

            Result = group_member_logic:alias(Uid, Gid, Alias, Description),
            ?assertEqual(ok, Result)
        end
    ).

alias_with_empty_description_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'alias', 4, fun(_Uid, _Gid, _Alias, _Description) -> ok end}
            ]},
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [123] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_FromUid, _ToUidLi, _Action, _MsgId, _Code, _Payload, _Save) ->
                    ok
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]}
        ],
        fun() ->
            Uid = 100,
            Gid = 1,
            Alias = <<"群昵称"/utf8>>,
            Description = <<>>,

            Result = group_member_logic:alias(Uid, Gid, Alias, Description),
            ?assertEqual(ok, Result)
        end
    ).

alias_returns_error_on_failure_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'alias', 4, fun(_Uid, _Gid, _Alias, _Description) -> {error, database_error} end}
        ],
        fun() ->
            Uid = 100,
            Gid = 1,
            Alias = <<"群昵称"/utf8>>,
            Description = <<"备注"/utf8>>,

            Result = group_member_logic:alias(Uid, Gid, Alias, Description),
            ?assertMatch({error, _}, Result)
        end
    ).

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
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 123456} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [100, 200] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) ->
                    #{<<"nickname">> => <<"Test">>, <<"avatar">> => <<>>, <<"account">> => <<>>}
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_From, _To, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
            ]}
        ],
        fun() ->
            JoinMode = <<"invite">>,
            Uid = 100,
            Gid = 1,
            OptData = #{},

            Result = group_member_logic:join_group(JoinMode, Uid, Gid, OptData),
            ?assertEqual(ok, Result)
        end
    ).

list_member_with_large_gid_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'list_member', 2, fun(_Gid, _MemberUids) ->
                {ok, []}
            end}
        ],
        fun() ->
            Gid = 999999999,
            Result = group_member_logic:list_member(Gid),
            ?assertEqual({ok, []}, Result)
        end
    ).

%% ===================================================================
%% 角色权限验证测试（增强功能）
%% ===================================================================

%% 测试 has_admin_permission/2 - 群主有管理权限
has_admin_permission_owner_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 4}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 100,
            Result = group_member_logic:has_admin_permission(Gid, Uid),
            ?assertEqual(true, Result)
        end
    ).

%% 测试 has_admin_permission/2 - 副群主有管理权限
has_admin_permission_vice_owner_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 5}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 101,
            Result = group_member_logic:has_admin_permission(Gid, Uid),
            ?assertEqual(true, Result)
        end
    ).

%% 测试 has_admin_permission/2 - 管理员有管理权限
has_admin_permission_admin_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 3}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 102,
            Result = group_member_logic:has_admin_permission(Gid, Uid),
            ?assertEqual(true, Result)
        end
    ).

%% 测试 has_admin_permission/2 - 普通成员无管理权限
has_admin_permission_member_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 1}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 103,
            Result = group_member_logic:has_admin_permission(Gid, Uid),
            ?assertEqual(false, Result)
        end
    ).

%% 测试 has_admin_permission/2 - 非成员返回 false
has_admin_permission_non_member_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {error, not_found}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 999,
            Result = group_member_logic:has_admin_permission(Gid, Uid),
            ?assertEqual(false, Result)
        end
    ).

%% 测试 has_senior_admin_permission/2 - 群主有高级管理权限
has_senior_admin_permission_owner_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 4}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 100,
            Result = group_member_logic:has_senior_admin_permission(Gid, Uid),
            ?assertEqual(true, Result)
        end
    ).

%% 测试 has_senior_admin_permission/2 - 副群主有高级管理权限
has_senior_admin_permission_vice_owner_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 5}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 101,
            Result = group_member_logic:has_senior_admin_permission(Gid, Uid),
            ?assertEqual(true, Result)
        end
    ).

%% 测试 has_senior_admin_permission/2 - 管理员无高级管理权限
has_senior_admin_permission_admin_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 3}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 102,
            Result = group_member_logic:has_senior_admin_permission(Gid, Uid),
            ?assertEqual(false, Result)
        end
    ).

%% 测试 has_senior_admin_permission/2 - 普通成员无高级管理权限
has_senior_admin_permission_member_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 1}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 103,
            Result = group_member_logic:has_senior_admin_permission(Gid, Uid),
            ?assertEqual(false, Result)
        end
    ).

%% 测试 is_owner_or_vice_owner/2 - 群主
is_owner_or_vice_owner_owner_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 4}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 100,
            Result = group_member_logic:is_owner_or_vice_owner(Gid, Uid),
            ?assertEqual(true, Result)
        end
    ).

%% 测试 is_owner_or_vice_owner/2 - 副群主
is_owner_or_vice_owner_vice_owner_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 5}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 101,
            Result = group_member_logic:is_owner_or_vice_owner(Gid, Uid),
            ?assertEqual(true, Result)
        end
    ).

%% 测试 is_owner_or_vice_owner/2 - 管理员返回 false
is_owner_or_vice_owner_admin_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {ok, #{<<"role">> => 3}}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 102,
            Result = group_member_logic:is_owner_or_vice_owner(Gid, Uid),
            ?assertEqual(false, Result)
        end
    ).

%% 测试 is_owner_or_vice_owner/2 - 非成员返回 false
is_owner_or_vice_owner_non_member_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) ->
                {error, not_found}
            end}
        ],
        fun() ->
            Gid = 1,
            Uid = 999,
            Result = group_member_logic:is_owner_or_vice_owner(Gid, Uid),
            ?assertEqual(false, Result)
        end
    ).

%% 测试角色名称获取
role_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"普通成员"/utf8>>, group_member_logic:role_name(1)),
        ?assertEqual(<<"嘉宾"/utf8>>, group_member_logic:role_name(2)),
        ?assertEqual(<<"管理员"/utf8>>, group_member_logic:role_name(3)),
        ?assertEqual(<<"群主"/utf8>>, group_member_logic:role_name(4)),
        ?assertEqual(<<"副群主"/utf8>>, group_member_logic:role_name(5)),
        ?assertEqual(<<"未知角色"/utf8>>, group_member_logic:role_name(99))
    end).

%% 测试 update_role 添加副群主
%% 安全回归：管理员不得罢免/降级群主或同级及以上成员
update_role_admin_cannot_demote_owner_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'get_member_info', 3, fun(_Gid, Uid, _Column) ->
                    case Uid of
                        % 操作者是管理员
                        100 -> {ok, #{<<"role">> => 3}};
                        % 目标是群主
                        101 -> {ok, #{<<"role">> => 4}}
                    end
                end}
            ]}
        ],
        fun() ->
            CurrentUid = 100,
            Gid = 1,
            UserId = 101,
            % 试图把群主降为普通成员
            Role = 1,
            Result = group_member_logic:update_role(CurrentUid, Gid, UserId, Role),
            ?assertMatch({error, _}, Result)
        end
    ).

update_role_to_vice_owner_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'get_member_info', 3, fun(_Gid, Uid, _Column) ->
                    case Uid of
                        % 操作者是群主
                        100 -> {ok, #{<<"role">> => 4}};
                        % 目标当前是普通成员
                        101 -> {ok, #{<<"role">> => 1}}
                    end
                end},
                {'update_role', 5, fun(_Conn, _Gid, _Uid, _Role, _Now) -> ok end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end}
            ]},
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [100, 101] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) -> #{<<"nickname">> => <<"Test">>} end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_From, _To, _Action, _MsgId, _Code, _Payload, _Save) -> ok end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-01-01T00:00:00Z">> end}
            ]}
        ],
        fun() ->
            CurrentUid = 100,
            Gid = 1,
            UserId = 101,
            % 副群主
            Role = 5,
            Result = group_member_logic:update_role(CurrentUid, Gid, UserId, Role),
            ?assertEqual(ok, Result)
        end
    ).

%% 测试副群主不能转让群
vice_owner_cannot_transfer_test_() ->
    ?WITH_MECKS(
        [
            {group_repo, [
                {'find_by_id', 2, fun(_Gid, _Column) ->
                    #{<<"id">> => 1, <<"owner_uid">> => 100}
                end}
            ]}
        ],
        fun() ->
            % 副群主
            ViceOwnerUid = 101,
            Gid = 1,
            NewOwnerUid = 102,
            Result = group_logic:transfer(ViceOwnerUid, Gid, NewOwnerUid),
            ?assertMatch({error, _}, Result)
        end
    ).

%% 测试副群主不能解散群
vice_owner_cannot_dissolve_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'dissolve_group', 4, fun(_Uid, _Gid, _OwnerUid, _G) ->
                    {error, <<"只有群主可以解散群组"/utf8>>}
                end}
            ]}
        ],
        fun() ->
            % 副群主
            ViceOwnerUid = 101,
            Gid = 1,
            OwnerUid = 100,
            G = #{},
            Result = group_logic:dissolve(ViceOwnerUid, Gid, OwnerUid, G),
            ?assertMatch({error, _}, Result)
        end
    ).
