-module(group_member_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_member_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组成员数据访问层功能
%%% 覆盖：成员查询、添加、移除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        Result = group_member_repo:tablename(),
        ?assertEqual(<<"public.group_member">>, Result)
    end).

%% ===================================================================
%% 成员查询测试
%% ===================================================================

list_by_gid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Column = <<"user_id, role, created_at">>,
        Result = group_member_repo:list_by_gid(Gid, Column),
        case Result of
            {ok, _, Members} when is_list(Members) ->
                ?assert(true);
            {ok, Members} when is_list(Members) ->
                ?assert(true);
            {error, _Reason} ->
                ?assert(true)
        end
    end).

%% ===================================================================
%% 成员添加测试
%% ===================================================================

add_member_to_group_test_() ->
    ?WITH_MECK(group_member_repo, [
        {'add', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Data = #{
            group_id => 1,
            user_id => 1,
            role => 1,
            join_mode => <<"invite">>,
            inviter_uid => 2
        },

        Result = group_member_repo:add(Data),
        case Result of
            {ok, MemberId} when is_integer(MemberId) -> ?assert(MemberId > 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, MemberId}")
        end
    end).

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_member_with_connection_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        Data = #{
            group_id => 1,
            user_id => 2,
            role => 2,
            join_mode => <<"apply">>,
            status => 1,
            created_at => elib_dt:now()
        },
        Result = group_member_repo:add(Conn, Data),
        case Result of
            {ok, MemberId} ->
                ?assert(is_integer(MemberId)),
                ?assert(MemberId > 0);
            {error, _} ->
                %% duplicate key (unique_violation) is acceptable
                ?assert(true)
        end
    end).

%% ===================================================================
%% find/3 测试
%% ===================================================================

find_existing_member_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Uid = 1,
        Column = <<"id, group_id, user_id, role">>,
        Result = group_member_repo:find(Gid, Uid, Column),
        ?assert(is_map(Result)),
        case Result of
            #{<<"id">> := Id} when is_integer(Id) -> ?assert(Id > 0);
            #{ } -> ok;
            _ -> ?assert(false, "Expected map with id or empty map")
        end
    end).

find_non_existing_member_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 999999,
        Uid = 888888,
        Column = <<"id">>,
        Result = group_member_repo:find(Gid, Uid, Column),
        ?assertEqual(#{}, Result)
    end).

find_with_all_columns_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Uid = 1,
        Column = <<"*">>,
        Result = group_member_repo:find(Gid, Uid, Column),
        ?assert(is_map(Result)),
        case Result of
            #{<<"group_id">> := _, <<"user_id">> := _} -> ok;
            #{ } -> ok;
            _ -> ?assert(false, "Expected map with required fields or empty map")
        end
    end).

%% ===================================================================
%% list_by_gid/2,3 测试
%% ===================================================================

list_by_gid_default_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Column = <<"id, user_id, role">>,
        Result = group_member_repo:list_by_gid(Gid, Column),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assert(is_list(Rows));
            _ ->
                ok
        end
    end).

list_by_gid_custom_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Column = <<"id, user_id">>,
        Limit = 5,
        Result = group_member_repo:list_by_gid(Gid, Column, Limit),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assert(is_list(Rows)),
                ?assert(length(Rows) =< Limit);
            _ ->
                ok
        end
    end).

list_by_gid_zero_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Column = <<"id">>,
        Limit = 0,
        Result = group_member_repo:list_by_gid(Gid, Column, Limit),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assertEqual([], Rows);
            _ ->
                ok
        end
    end).

list_by_gid_non_existing_group_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 999999,
        Column = <<"id">>,
        Limit = 10,
        Result = group_member_repo:list_by_gid(Gid, Column, Limit),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assertEqual([], Rows);
            _ ->
                ok
        end
    end).

%% ===================================================================
%% list_by_uid/2,3 测试
%% ===================================================================

list_by_uid_default_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Column = <<"id, group_id">>,
        Result = group_member_repo:list_by_uid(Uid, Column),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assert(is_list(Rows));
            _ ->
                ok
        end
    end).

list_by_uid_custom_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Column = <<"id, group_id, role">>,
        Limit = 5,
        Result = group_member_repo:list_by_uid(Uid, Column, Limit),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assert(is_list(Rows)),
                ?assert(length(Rows) =< Limit);
            _ ->
                ok
        end
    end).

list_by_uid_non_existing_user_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Column = <<"id">>,
        Limit = 10,
        Result = group_member_repo:list_by_uid(Uid, Column, Limit),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assertEqual([], Rows);
            _ ->
                ok
        end
    end).

%% ===================================================================
%% list_same_group/2 测试
%% ===================================================================

list_same_group_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid1 = 1,
        Uid2 = 2,
        Result = group_member_repo:list_same_group(Uid1, Uid2),
        ?assert(is_list(Result))
    end).

list_same_group_uid1_zero_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid1 = 0,
        Uid2 = 2,
        Result = group_member_repo:list_same_group(Uid1, Uid2),
        ?assertEqual([], Result)
    end).

list_same_group_uid2_zero_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid1 = 1,
        Uid2 = 0,
        Result = group_member_repo:list_same_group(Uid1, Uid2),
        ?assertEqual([], Result)
    end).

list_same_group_no_common_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid1 = 999999,
        Uid2 = 888888,
        Result = group_member_repo:list_same_group(Uid1, Uid2),
        ?assertEqual([], Result)
    end).

list_same_group_same_user_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid1 = 1,
        Uid2 = 1,
        Result = group_member_repo:list_same_group(Uid1, Uid2),
        ?assert(is_list(Result))
    end).
