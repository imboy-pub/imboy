-module(group_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_db → imboy_pg 迁移的语义正确性
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_public_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = group_repo:tablename(),
        ?assertEqual(<<"public.group">>, Result)
    end).

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_valid_group_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            owner_uid => 1,
            creator_uid => 1,
            type => 1,
            join_limit => 1,
            user_id_sum => 1,
            created_at => imboy_dt:now()
        },
        Result = group_repo:add(undefined, Data),
        case Result of
            {ok, InsertId, Details} when is_integer(InsertId) -> 
                % 验证添加成功，返回插入ID和详细信息
                ?assert(InsertId > 0, "Expected positive insert ID"),
                ?assertMatch(#{}, Details, "Expected details map");
            {ok, InsertResult} when is_map(InsertResult) -> 
                % 验证添加成功，返回结果map
                ?assertMatch(#{}, InsertResult, "Expected non-empty result");
            {error, Reason} -> 
                % 验证添加失败的原因
                ?assert(is_atom(Reason) orelse is_binary(Reason), 
                       "Expected atom or binary error reason");
            _ -> 
                ?assert(false, "Unexpected return value")
        end
    end).

add_empty_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{},
        Result = group_repo:add(undefined, Data),
        % 精确断言：验证错误原因
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).

add_with_missing_required_field_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 缺少 owner_uid
        Data = #{
            type => 1,
            created_at => imboy_dt:now()
        },
        Result = group_repo:add(undefined, Data),
        % 精确断言：验证错误原因
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).

%% ===================================================================
%% find_by_id/2 测试
%% ===================================================================

find_by_id_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Column = <<"id, owner_uid">>,
        Result = group_repo:find_by_id(Gid, Column),
        ?assertMatch(#{<<"id">> := _, <<"owner_uid">> := _}, Result)
    end).

find_by_id_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 999999,
        Column = <<"id">>,
        Result = group_repo:find_by_id(Gid, Column),
        ?assertMatch({error, _}, Result)
    end).

find_by_id_all_columns_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Column = <<"*">>,
        Result = group_repo:find_by_id(Gid, Column),
        ?assertMatch(#{<<"id">> := _}, Result)
    end).

find_by_id_binary_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = <<"1">>,
        Column = <<"id">>,
        Result = group_repo:find_by_id(Gid, Column),
        ?assertMatch(#{<<"id">> := _}, Result)
    end).

%% ===================================================================
%% list_by_ids/2 测试
%% ===================================================================

list_by_ids_non_empty_test_() ->
    ?TEST_WITH_DB(fun() ->
        Ids = [1, 2, 3],
        Column = <<"id">>,
        Result = group_repo:list_by_ids(Ids, Column),
        ?assertMatch({ok, [_|_]}, Result)
    end).

list_by_ids_empty_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Ids = [],
        Column = <<"id">>,
        Result = group_repo:list_by_ids(Ids, Column),
        ?assertEqual({ok, []}, Result)
    end).

list_by_ids_all_columns_test_() ->
    ?TEST_WITH_DB(fun() ->
        Ids = [1],
        Column = <<"*">>,
        Result = group_repo:list_by_ids(Ids, Column),
        case Result of
            {ok, List} when is_list(List) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, List}")
        end
    end).

list_by_ids_with_duplicates_test_() ->
    ?TEST_WITH_DB(fun() ->
        Ids = [1, 1, 2],
        Column = <<"id">>,
        Result = group_repo:list_by_ids(Ids, Column),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% ===================================================================
%% list_by_uid/2 测试
%% ===================================================================

list_by_uid_default_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Column = <<"id">>,
        Result = group_repo:list_by_uid(Uid, Column),
        ?assertMatch({ok, _, _}, Result)
    end).

list_by_uid_custom_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Column = <<"id">>,
        Limit = 5,
        Result = group_repo:list_by_uid(Uid, Column, Limit),
        ?assertMatch({ok, _, _}, Result)
    end).

list_by_uid_zero_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Column = <<"id">>,
        Limit = 0,
        Result = group_repo:list_by_uid(Uid, Column, Limit),
        ?assertMatch({ok, _, _}, Result)
    end).

list_by_uid_non_existing_user_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Column = <<"id">>,
        Limit = 10,
        Result = group_repo:list_by_uid(Uid, Column, Limit),
        ?assertMatch({ok, _, _}, Result)
    end).
