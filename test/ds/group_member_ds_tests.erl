-module(group_member_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_member_ds 模块的 EUnit 测试
%%%
%%% 目标：验证群组成员数据服务功能
%%% 覆盖：成员列表、加入群组、离开群组、设置别名、统计更新
%%%===================================================================

%% ===================================================================
%% list_member/2 测试
%% ===================================================================

list_member_with_empty_list_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {user_repo, [
            {'tablename', 0, fun() -> <<"user">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [Gid, Limit]) ->
                ?assertEqual(1, Gid),
                ?assertEqual(50000, Limit),
                {ok, []}
            end}
        ]}
    ], fun() ->
        Result = group_member_ds:list_member(1, []),
        ?assertEqual({ok, []}, Result)
    end).

list_member_with_empty_list_returns_members_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {user_repo, [
            {'tablename', 0, fun() -> <<"user">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{
                        <<"nickname">> => <<"用户1"/utf8>>,
                        <<"account">> => <<"user1">>,
                        <<"avatar">> => <<"avatar1.jpg">>,
                        <<"sign">> => <<"签名1"/utf8>>,
                        <<"id">> => 1,
                        <<"group_id">> => 1,
                        <<"user_id">> => 100,
                        <<"role">> => 1
                    },
                    #{
                        <<"nickname">> => <<"用户2"/utf8>>,
                        <<"account">> => <<"user2">>,
                        <<"avatar">> => <<"avatar2.jpg">>,
                        <<"sign">> => <<"签名2"/utf8>>,
                        <<"id">> => 2,
                        <<"group_id">> => 1,
                        <<"user_id">> => 101,
                        <<"role">> => 2
                    }
                ]}
            end}
        ]}
    ], fun() ->
        {ok, Members} = group_member_ds:list_member(1, []),
        ?assertEqual(2, length(Members)),
        [Member1, Member2] = Members,
        ?assertEqual(<<"用户1"/utf8>>, maps:get(<<"nickname">>, Member1)),
        ?assertEqual(<<"用户2"/utf8>>, maps:get(<<"nickname">>, Member2))
    end).

list_member_with_member_uids_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {user_repo, [
            {'tablename', 0, fun() -> <<"user">> end}
        ]},
        {elib_pg_sql, [
            {'placeholders', 1, fun(Count) ->
                ?assertEqual(2, Count),
                <<"$2,$3">>
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [Gid | MemberUids]) ->
                ?assertEqual(1, Gid),
                ?assertEqual([100, 101], MemberUids),
                {ok, [
                    #{
                        <<"nickname">> => <<"用户1"/utf8>>,
                        <<"user_id">> => 100
                    }
                ]}
            end}
        ]}
    ], fun() ->
        {ok, Members} = group_member_ds:list_member(1, [100, 101]),
        ?assertEqual(1, length(Members))
    end).

list_member_with_invalid_params_returns_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_member_ds:list_member(1, invalid),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% join_group/5 测试
%% ===================================================================

join_group_success_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) -> #{} end},
            {'add', 2, fun(_Conn, Data) ->
                ?assertEqual(1, maps:get(group_id, Data)),
                ?assertEqual(100, maps:get(user_id, Data)),
                ?assertEqual(1, maps:get(role, Data)),
                ?assertEqual(1, maps:get(is_join, Data)),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_str, [
            {'trunc', 2, fun(Str, _Len) -> Str end}
        ]},
        {group_member_ds, [
            {'update_statistics', 2, fun(_Conn, _Gid) -> {ok, 100} end}
        ]},
        {group_ds, [
            {'join', 2, fun(_Uid, _Gid) -> ok end}
        ]}
    ], fun() ->
        Conn = self(),
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{role => 1},

        Result = group_member_ds:join_group(Conn, JoinMode, Uid, Gid, OptData),
        ?assertEqual({ok, 100}, Result)
    end).

join_group_already_member_returns_zero_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) ->
                #{<<"id">> => 1}
            end}
        ]}
    ], fun() ->
        Conn = self(),
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{},

        Result = group_member_ds:join_group(Conn, JoinMode, Uid, Gid, OptData),
        ?assertEqual({ok, 0}, Result)
    end).

join_group_with_default_role_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) -> #{} end},
            {'add', 2, fun(_Conn, Data) ->
                % 验证默认 role 为 1
                ?assertEqual(1, maps:get(role, Data)),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_str, [
            {'trunc', 2, fun(Str, _Len) -> Str end}
        ]},
        {group_member_ds, [
            {'update_statistics', 2, fun(_Conn, _Gid) -> {ok, 100} end}
        ]},
        {group_ds, [
            {'join', 2, fun(_Uid, _Gid) -> ok end}
        ]}
    ], fun() ->
        Conn = self(),
        JoinMode = <<"apply">>,
        Uid = 100,
        Gid = 1,
        OptData = #{}, % 不指定 role

        Result = group_member_ds:join_group(Conn, JoinMode, Uid, Gid, OptData),
        ?assertEqual({ok, 100}, Result)
    end).

join_group_with_add_error_returns_error_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) -> #{} end},
            {'add', 2, fun(_Conn, _Data) ->
                {error, <<"database_error">>}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_str, [
            {'trunc', 2, fun(Str, _Len) -> Str end}
        ]}
    ], fun() ->
        Conn = self(),
        JoinMode = <<"invite">>,
        Uid = 100,
        Gid = 1,
        OptData = #{},

        Result = group_member_ds:join_group(Conn, JoinMode, Uid, Gid, OptData),
        ?assertEqual({error, <<"database_error">>}, Result)
    end).

join_group_with_statistics_error_returns_error_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) -> #{} end},
            {'add', 2, fun(_Conn, _Data) ->
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_str, [
            {'trunc', 2, fun(Str, _Len) -> Str end}
        ]},
        {group_member_ds, [
            {'update_statistics', 2, fun(_Conn, _Gid) ->
                {error, <<"statistics_error">>}
            end}
        ]}
    ], fun() ->
        Conn = self(),
        JoinMode = <<"qrcode">>,
        Uid = 100,
        Gid = 1,
        OptData = #{},

        Result = group_member_ds:join_group(Conn, JoinMode, Uid, Gid, OptData),
        ?assertEqual({error, <<"statistics_error">>}, Result)
    end).

%% ===================================================================
%% leave/4 测试
%% ===================================================================

leave_success_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) ->
                #{
                    <<"id">> => 1,
                    <<"group_id">> => 1,
                    <<"user_id">> => 100,
                    <<"role">> => 2
                }
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg, [
            {'execute', 3, fun(_Conn, _Sql, [Id]) ->
                ?assertEqual(1, Id),
                {ok, 1}
            end}
        ]},
        {jsone_encode, [
            {'encode', 2, fun(_Data, _Opts) ->
                {ok, <<"{\"id\":1}">>}
            end}
        ]},
        {group_log_repo, [
            {'add', 2, fun(_Conn, LogData) ->
                ?assertEqual(200, maps:get(type, LogData)),
                ?assertEqual(100, maps:get(option_uid, LogData)),
                {ok, 1, #{}}
            end}
        ]},
        {group_member_ds, [
            {'update_statistics', 2, fun(_Conn, _Gid) -> {ok, 200} end}
        ]}
    ], fun() ->
        Conn = self(),
        Uid = 100,
        Gid = 1,
        CurrentUid = 100,

        Result = group_member_ds:leave(Conn, Uid, Gid, CurrentUid),
        ?assertMatch({ok, 200, _}, Result)
    end).

leave_member_not_found_returns_zero_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) -> #{} end}
        ]}
    ], fun() ->
        Conn = self(),
        Uid = 100,
        Gid = 1,
        CurrentUid = 100,

        Result = group_member_ds:leave(Conn, Uid, Gid, CurrentUid),
        ?assertEqual({ok, 0, #{}}, Result)
    end).

leave_kicked_by_admin_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) ->
                #{
                    <<"id">> => 1,
                    <<"group_id">> => 1,
                    <<"user_id">> => 100
                }
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg, [
            {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
        ]},
        {jsone_encode, [
            {'encode', 2, fun(_Data, _Opts) -> {ok, <<"{}">>} end}
        ]},
        {group_log_repo, [
            {'add', 2, fun(_Conn, LogData) ->
                % 验证被踢出的日志类型为 202
                ?assertEqual(202, maps:get(type, LogData)),
                ?assertEqual(999, maps:get(option_uid, LogData)),
                {ok, 1, #{}}
            end}
        ]},
        {group_member_ds, [
            {'update_statistics', 2, fun(_Conn, _Gid) -> {ok, 200} end}
        ]}
    ], fun() ->
        Conn = self(),
        Uid = 100,
        Gid = 1,
        CurrentUid = 999, % 不同的用户ID，表示被踢出

        Result = group_member_ds:leave(Conn, Uid, Gid, CurrentUid),
        ?assertMatch({ok, 200, _}, Result)
    end).

leave_with_statistics_error_returns_error_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) ->
                #{<<"id">> => 1}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg, [
            {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
        ]},
        {jsone_encode, [
            {'encode', 2, fun(_Data, _Opts) -> {ok, <<"{}">>} end}
        ]},
        {group_log_repo, [
            {'add', 2, fun(_Conn, _LogData) -> {ok, 1, #{}} end}
        ]},
        {group_member_ds, [
            {'update_statistics', 2, fun(_Conn, _Gid) ->
                {error, <<"statistics_error">>}
            end}
        ]}
    ], fun() ->
        Conn = self(),
        Uid = 100,
        Gid = 1,
        CurrentUid = 100,

        Result = group_member_ds:leave(Conn, Uid, Gid, CurrentUid),
        ?assertEqual({error, <<"statistics_error">>}, Result)
    end).

%% ===================================================================
%% alias/4 测试
%% ===================================================================

alias_success_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Table, Data, _Where, [Gid, Uid]) ->
                ?assertEqual(1, Gid),
                ?assertEqual(100, Uid),
                ?assertEqual(<<"昵称"/utf8>>, maps:get(alias, Data)),
                ?assertEqual(<<"描述"/utf8>>, maps:get(description, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Uid = 100,
        Gid = 1,
        Alias = <<"昵称"/utf8>>,
        Description = <<"描述"/utf8>>,

        Result = group_member_ds:alias(Uid, Gid, Alias, Description),
        ?assertEqual(ok, Result)
    end).

alias_with_empty_strings_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Table, Data, _Where, _Params) ->
                ?assertEqual(<<>>, maps:get(alias, Data)),
                ?assertEqual(<<>>, maps:get(description, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = group_member_ds:alias(100, 1, <<>>, <<>>),
        ?assertEqual(ok, Result)
    end).

alias_with_error_returns_error_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Table, _Data, _Where, _Params) ->
                {error, <<"update_failed">>}
            end}
        ]}
    ], fun() ->
        Result = group_member_ds:alias(100, 1, <<"昵称"/utf8>>, <<"描述"/utf8>>),
        ?assertEqual({error, <<"update_failed">>}, Result)
    end).

%% ===================================================================
%% update_statistics/2 测试
%% ===================================================================

update_statistics_with_undefined_conn_test_() ->
    ?WITH_MECK(elib_pg, [
        {'with_tx', 1, fun(Fun) ->
            Conn = self(),
            Fun(Conn)
        end}
    ], fun() ->
        ?WITH_MECKS([
            {group_member_repo, [
                {'tablename', 0, fun() -> <<"group_member">> end}
            ]},
            {elib_pg, [
                {'query', 3, fun(_Conn, _Sql, [Gid]) ->
                    ?assertEqual(1, Gid),
                    {ok, [#{
                        <<"user_id_sum">> => 300,
                        <<"member_count">> => 3
                    }]}
                end},
                {'update', 5, fun(_Conn, _Table, Data, _Where, [Gid]) ->
                    ?assertEqual(1, Gid),
                    ?assertEqual(3, maps:get(member_count, Data)),
                    ?assertEqual(300, maps:get(user_id_sum, Data)),
                    {ok, 1}
                end}
            ]},
            {group_repo, [
                {'tablename', 0, fun() -> <<"group">> end}
            ]},
            {ec_cnv, [
                {'to_integer', 1, fun(Val) -> Val end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]}
        ], fun() ->
            Result = group_member_ds:update_statistics(undefined, 1),
            ?assertEqual({ok, 300}, Result)
        end)
    end).

update_statistics_with_conn_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {elib_pg, [
            {'query', 3, fun(_Conn, _Sql, [Gid]) ->
                ?assertEqual(1, Gid),
                {ok, [#{
                    <<"user_id_sum">> => 500,
                    <<"member_count">> => 5
                }]}
            end},
            {'update', 5, fun(_Conn, _Table, _Data, _Where, _Params) ->
                {ok, 1}
            end}
        ]},
        {group_repo, [
            {'tablename', 0, fun() -> <<"group">> end}
        ]},
        {ec_cnv, [
            {'to_integer', 1, fun(Val) -> Val end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Conn = self(),
        Result = group_member_ds:update_statistics(Conn, 1),
        ?assertEqual({ok, 500}, Result)
    end).

update_statistics_with_query_error_returns_error_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {elib_pg, [
            {'query', 3, fun(_Conn, _Sql, _Params) ->
                {error, <<"query_failed">>}
            end}
        ]}
    ], fun() ->
        Conn = self(),
        Result = group_member_ds:update_statistics(Conn, 1),
        ?assertEqual({error, <<"query_failed">>}, Result)
    end).

%% ===================================================================
%% find_by_gid_and_uid/3 测试
%% ===================================================================

find_by_gid_and_uid_delegates_to_repo_test_() ->
    ?WITH_MECK(group_member_repo, [
        {'find', 3, fun(Gid, Uid, Column) ->
            ?assertEqual(1, Gid),
            ?assertEqual(100, Uid),
            ?assertEqual(<<"*">>, Column),
            #{
                <<"id">> => 1,
                <<"group_id">> => 1,
                <<"user_id">> => 100,
                <<"role">> => 1
            }
        end}
    ], fun() ->
        Result = group_member_ds:find_by_gid_and_uid(1, 100, <<"*">>),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

find_by_gid_and_uid_with_specific_column_test_() ->
    ?WITH_MECK(group_member_repo, [
        {'find', 3, fun(_Gid, _Uid, Column) ->
            ?assertEqual(<<"role">>, Column),
            #{<<"role">> => 2}
        end}
    ], fun() ->
        Result = group_member_ds:find_by_gid_and_uid(1, 100, <<"role">>),
        ?assertEqual(#{<<"role">> => 2}, Result)
    end).

find_by_gid_and_uid_not_found_returns_empty_map_test_() ->
    ?WITH_MECK(group_member_repo, [
        {'find', 3, fun(_Gid, _Uid, _Column) -> #{} end}
    ], fun() ->
        Result = group_member_ds:find_by_gid_and_uid(1, 999, <<"*">>),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

list_member_with_large_member_list_test_() ->
    LargeMemberUids = lists:seq(1, 1000),
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {user_repo, [
            {'tablename', 0, fun() -> <<"user">> end}
        ]},
        {elib_pg_sql, [
            {'placeholders', 1, fun(Count) ->
                ?assertEqual(1000, Count),
                <<"$2,$3">> % 简化的占位符
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [Gid | MemberUids]) ->
                ?assertEqual(1, Gid),
                ?assertEqual(1000, length(MemberUids)),
                {ok, []}
            end}
        ]}
    ], fun() ->
        Result = group_member_ds:list_member(1, LargeMemberUids),
        ?assertEqual({ok, []}, Result)
    end).

join_group_with_long_join_mode_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end},
            {'find', 3, fun(_Gid, _Uid, _Column) -> #{} end},
            {'add', 2, fun(_Conn, Data) ->
                % 验证 join_mode 被截断
                JoinMode = maps:get(join_mode, Data),
                ?assert(is_binary(JoinMode)),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_str, [
            {'trunc', 2, fun(Str, Len) ->
                ?assertEqual(100, Len),
                binary:part(Str, 0, min(byte_size(Str), Len))
            end}
        ]},
        {group_member_ds, [
            {'update_statistics', 2, fun(_Conn, _Gid) -> {ok, 100} end}
        ]},
        {group_ds, [
            {'join', 2, fun(_Uid, _Gid) -> ok end}
        ]}
    ], fun() ->
        Conn = self(),
        LongJoinMode = list_to_binary(lists:duplicate(200, $x)),
        Uid = 100,
        Gid = 1,
        OptData = #{},

        Result = group_member_ds:join_group(Conn, LongJoinMode, Uid, Gid, OptData),
        ?assertEqual({ok, 100}, Result)
    end).

update_statistics_with_zero_members_test_() ->
    ?WITH_MECKS([
        {group_member_repo, [
            {'tablename', 0, fun() -> <<"group_member">> end}
        ]},
        {elib_pg, [
            {'query', 3, fun(_Conn, _Sql, _Params) ->
                {ok, [#{
                    <<"user_id_sum">> => 0,
                    <<"member_count">> => 0
                }]}
            end},
            {'update', 5, fun(_Conn, _Table, Data, _Where, _Params) ->
                ?assertEqual(0, maps:get(member_count, Data)),
                ?assertEqual(0, maps:get(user_id_sum, Data)),
                {ok, 1}
            end}
        ]},
        {group_repo, [
            {'tablename', 0, fun() -> <<"group">> end}
        ]},
        {ec_cnv, [
            {'to_integer', 1, fun(Val) -> Val end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Conn = self(),
        Result = group_member_ds:update_statistics(Conn, 1),
        ?assertEqual({ok, 0}, Result)
    end).
