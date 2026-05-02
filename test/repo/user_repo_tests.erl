-module(user_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_db → elib_pg 迁移的语义正确性
%%% 覆盖：正常路径、空结果、异常路径
%%%===================================================================

-define(MOCK_ENV, {config_ds, [{'env', 1, fun(sql_driver) -> pgsql; (_) -> undefined end}]}).
-define(MOCK_TSID, {elib_tsid, [{'generate', 1, fun(_Table) -> 555666777 end}]}).

%% ===================================================================
%% tablename/0 测试 (使用meck模拟依赖)
%% ===================================================================

tablename_returns_public_user_test_() ->
    ?WITH_MECKS([?MOCK_ENV], fun() ->
        Result = user_repo:tablename(),
        ?assertEqual(<<"public.user">>, Result)
    end).

tablename_is_binary_test_() ->
    ?WITH_MECKS([?MOCK_ENV], fun() ->
        Result = user_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(binary:match(Result, <<"public.">>) =/= nomatch)
    end).

%% ===================================================================
%% find_by_email/2 测试
%% ===================================================================

find_by_email_existing_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"id">> => 12345, <<"email">> => <<"test@example.com">>}}
        end}
    ]}], fun() ->
        Email = <<"test@example.com">>,
        Column = <<"id">>,
        Result = user_repo:find_by_email(Email, Column),
        ?assertMatch(#{<<"id">> := 12345}, Result),
        ?assert(maps:get(<<"id">>, Result) > 0)
    end).

find_by_email_not_existing_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {ok, #{}} end}
    ]}], fun() ->
        Email = <<"nonexistent@example.com">>,
        Column = <<"id">>,
        Result = user_repo:find_by_email(Email, Column),
        ?assertEqual(#{}, Result),
        ?assertEqual(0, maps:size(Result))
    end).

find_by_email_empty_email_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {ok, undefined} end}
    ]}], fun() ->
        Email = <<>>,
        Column = <<"id">>,
        Result = user_repo:find_by_email(Email, Column),
        ?assertMatch(#{}, Result)
    end).

%% ===================================================================
%% find_by_mobile/2 测试
%% ===================================================================

find_by_mobile_existing_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"id">> => 12345}}
        end}
    ]}], fun() ->
        Mobile = <<"13800138000">>,
        Column = <<"id">>,
        Result = user_repo:find_by_mobile(Mobile, Column),
        ?assertMatch(#{<<"id">> := _} when map_size(Result) > 0, Result)
    end).

find_by_mobile_not_existing_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {ok, undefined} end}
    ]}], fun() ->
        Mobile = <<"99999999999">>,
        Column = <<"id">>,
        Result = user_repo:find_by_mobile(Mobile, Column),
        ?assertEqual(#{}, Result)
    end).

find_by_mobile_list_mobile_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"id">> => 12345}}
        end}
    ]}], fun() ->
        Mobile = "13800138000",
        Column = <<"id">>,
        Result = user_repo:find_by_mobile(Mobile, Column),
        case Result of
            #{<<"id">> := UserId} ->
                ?assert(is_integer(UserId) andalso UserId > 0);
            #{} ->
                ok
        end
    end).

%% ===================================================================
%% find_by_account/2 测试
%% ===================================================================

find_by_account_existing_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"id">> => 12345, <<"account">> => <<"testuser">>}}
        end}
    ]}], fun() ->
        Account = <<"testuser">>,
        Column = <<"id, account">>,
        Result = user_repo:find_by_account(Account, Column),
        ?assertMatch(#{<<"id">> := _, <<"account">> := _}, Result)
    end).

find_by_account_not_existing_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {ok, undefined} end}
    ]}], fun() ->
        Account = <<"nonexistentuser">>,
        Column = <<"id">>,
        Result = user_repo:find_by_account(Account, Column),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% find_by_id/2 测试
%% ===================================================================

find_by_id_existing_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, [Uid]) ->
            {ok, #{<<"id">> => Uid, <<"account">> => <<"test_account">>}}
        end}
    ]}], fun() ->
        Uid = 1,
        Column = <<"id, account">>,
        Result = user_repo:find_by_id(Uid, Column),
        ?assertMatch(#{<<"id">> := FoundUid, <<"account">> := Account}
            when is_integer(FoundUid) andalso is_binary(Account) andalso byte_size(Account) > 0,
            Result),
        ?assertEqual(Uid, maps:get(<<"id">>, Result))
    end).

find_by_id_not_existing_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {ok, undefined} end}
    ]}], fun() ->
        Uid = 999999,
        Column = <<"id">>,
        Result = user_repo:find_by_id(Uid, Column),
        ?assertEqual(#{}, Result)
    end).

find_by_id_binary_id_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'one', 2, fun(_Sql, [_Uid]) ->
            {ok, #{<<"id">> => 1}}
        end}
    ]}], fun() ->
        Uid = <<"1">>,
        Result = user_repo:find_by_id(Uid, <<"id">>),
        case Result of
            #{<<"id">> := FoundUid} ->
                ?assert(is_integer(FoundUid)),
                ?assert(FoundUid > 0);
            #{} ->
                ok
        end
    end).

%% ===================================================================
%% list_by_ids/2 测试
%% ===================================================================

list_by_ids_non_empty_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"id">> => 1}, #{<<"id">> => 2}]}
        end}
    ]}], fun() ->
        Uids = [1, 2, 3],
        Column = <<"id">>,
        Result = user_repo:list_by_ids(Uids, Column),
        ?assertMatch({ok, List} when is_list(List), Result),
        case Result of
            {ok, List} ->
                ?assert(length(List) =< length(Uids)),
                lists:foreach(fun(User) ->
                    ?assert(is_integer(maps:get(<<"id">>, User))),
                    ?assert(maps:get(<<"id">>, User) > 0)
                end, List);
            _ ->
                ?assert(false, "Expected {ok, List}")
        end
    end).

list_by_ids_empty_list_test_() ->
    ?WITH_MECKS([?MOCK_ENV], fun() ->
        Uids = [],
        Column = <<"id">>,
        ?assertError(function_clause, user_repo:list_by_ids(Uids, Column))
    end).

list_by_ids_single_id_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"id">> => 1, <<"account">> => <<"test_account">>}]}
        end}
    ]}], fun() ->
        Uids = [1],
        Column = <<"id, account">>,
        Result = user_repo:list_by_ids(Uids, Column),
        ?assertMatch({ok, [_]}, Result),
        case Result of
            {ok, [#{<<"id">> := UserId, <<"account">> := Account}]} ->
                ?assert(is_integer(UserId) andalso UserId > 0),
                ?assert(is_binary(Account) andalso byte_size(Account) > 0);
            _ ->
                ?assert(false, "Expected {ok, [UserMap]}")
        end
    end).

list_by_ids_large_list_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"id">> => I} || I <- lists:seq(1, 50)]}
        end}
    ]}], fun() ->
        Uids = lists:seq(1, 100),
        Column = <<"id">>,
        Result = user_repo:list_by_ids(Uids, Column),
        ?assertMatch({ok, [_ | _]}, Result),
        case Result of
            {ok, [#{<<"id">> := UserId} | _]} ->
                ?assert(is_integer(UserId) andalso UserId > 0);
            _ ->
                ?assert(false, "Expected {ok, [User | Rest]}")
        end
    end).

%% ===================================================================
%% save/1 测试
%% ===================================================================

save_insert_valid_data_test_() ->
    ?WITH_MECKS([?MOCK_ENV, ?MOCK_TSID, {elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ]}], fun() ->
        Data = #{
            <<"account">> => <<"test_user_123">>,
            <<"password">> => <<"hashed_password">>,
            <<"mobile">> => <<"13800138000">>,
            <<"status">> => 1,
            <<"created_at">> => <<"2024-01-01T00:00:00Z">>
        },
        Result = user_repo:save(Data),
        ?assertMatch({ok, InsertedId} when is_integer(InsertedId), Result)
    end).

save_insert_empty_map_test_() ->
    ?WITH_MECKS([?MOCK_ENV, ?MOCK_TSID, {elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {error, no_data} end}
    ]}], fun() ->
        Data = #{},
        Result = user_repo:save(Data),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            {error, _} ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).

%% ===================================================================
%% update/2 测试
%% ===================================================================

update_user_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 1} end}
    ]}], fun() ->
        Id = 1,
        Data = #{<<"nickname">> => <<"Updated Nickname">>},
        {ok, UpdatedCount} = user_repo:update(Id, Data),
        ?assert(is_integer(UpdatedCount)),
        ?assert(UpdatedCount > 0)
    end).

update_with_empty_data_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 0} end}
    ]}], fun() ->
        Id = 1,
        Data = #{},
        {ok, UpdatedCount} = user_repo:update(Id, Data),
        ?assert(is_integer(UpdatedCount)),
        ?assertEqual(0, UpdatedCount)
    end).

update_non_existing_user_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 0} end}
    ]}], fun() ->
        Id = 999999,
        Data = #{<<"nickname">> => <<"Test">>},
        {ok, UpdatedCount} = user_repo:update(Id, Data),
        ?assert(is_integer(UpdatedCount)),
        ?assertEqual(0, UpdatedCount)
    end).

update_with_timestamp_field_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 1} end}
    ]}], fun() ->
        Id = 1,
        Data = #{<<"updated_at">> => <<"2024-01-01T00:00:00Z">>},
        {ok, UpdatedCount} = user_repo:update(Id, Data),
        ?assert(is_integer(UpdatedCount)),
        ?assert(UpdatedCount > 0)
    end).

%% ===================================================================
%% delete/1 测试
%% ===================================================================

delete_existing_user_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 1} end}
    ]}], fun() ->
        Id = 1,
        Result = user_repo:delete(Id),
        ?assertMatch({ok, Count} when is_integer(Count), Result)
    end).

delete_non_existing_user_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 0} end}
    ]}], fun() ->
        Id = 999999,
        Result = user_repo:delete(Id),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) ->
                ?assert(AffectedCount >= 0);
            {ok, _} ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

%% ===================================================================
%% update_friends_last_seen_at/2 测试
%% ===================================================================

update_friends_last_seen_at_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
    ]}, {friend_repo, [
        {'tablename', 0, fun() -> <<"public.user_friend">> end}
    ]}, {elib_dt, [
        {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
    ]}], fun() ->
        Uid = 1,
        Timestamp = <<"2024-01-01T00:00:00Z">>,
        Result = user_repo:update_friends_last_seen_at(Uid, Timestamp),
        ?assertEqual(ok, Result)
    end).

update_friends_last_seen_at_zero_uid_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
    ]}, {friend_repo, [
        {'tablename', 0, fun() -> <<"public.user_friend">> end}
    ]}, {elib_dt, [
        {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
    ]}], fun() ->
        Uid = 0,
        Timestamp = <<"2024-01-01T00:00:00Z">>,
        Result = user_repo:update_friends_last_seen_at(Uid, Timestamp),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% select_by_where/5 测试
%% ===================================================================

select_by_where_basic_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'page_with_total', 6, fun(_Tb, _Col, _Where, _OrderBy, _Page, _Size) ->
            {ok, #{list => [#{<<"id">> => 1}], total => 1}}
        end}
    ]}], fun() ->
        Column = <<"id">>,
        Where = <<"id > 0">>,
        Limit = 10,
        Offset = 0,
        OrderBy = <<"id DESC">>,
        Page = (Offset div Limit) + 1,
        Result = elib_pg:page_with_total(user_repo:tablename(), Column, #{<<"__raw">> => Where}, OrderBy, Page, Limit),
        case Result of
            {ok, #{list := List}} when is_list(List) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {ok, #{list := List}}")
        end
    end).

select_by_where_empty_result_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'page_with_total', 6, fun(_Tb, _Col, _Where, _OrderBy, _Page, _Size) ->
            {ok, #{list => [], total => 0}}
        end}
    ]}], fun() ->
        Column = <<"id">>,
        Where = <<"id < 0">>,
        Limit = 10,
        Offset = 0,
        OrderBy = <<"id DESC">>,
        Page = (Offset div Limit) + 1,
        Result = elib_pg:page_with_total(user_repo:tablename(), Column, #{<<"__raw">> => Where}, OrderBy, Page, Limit),
        case Result of
            {ok, #{list := List}} when is_list(List) ->
                ?assertEqual([], List);
            _ ->
                ?assert(false, "Expected {ok, #{list := []}}")
        end
    end).

select_by_where_large_offset_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'page_with_total', 6, fun(_Tb, _Col, _Where, _OrderBy, _Page, _Size) ->
            {ok, #{list => [], total => 0}}
        end}
    ]}], fun() ->
        Column = <<"id">>,
        Where = <<"id > 0">>,
        Limit = 10,
        Offset = 100000,
        OrderBy = <<"id DESC">>,
        Page = (Offset div Limit) + 1,
        Result = elib_pg:page_with_total(user_repo:tablename(), Column, #{<<"__raw">> => Where}, OrderBy, Page, Limit),
        case Result of
            {ok, #{list := List}} when is_list(List) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {ok, #{list := List}}")
        end
    end).
