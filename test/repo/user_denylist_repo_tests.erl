-module(user_denylist_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_denylist_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户黑名单数据访问层功能
%%% 覆盖：黑名单查询、添加、删除
%%%===================================================================

-define(MOCK_ENV, {config_ds, [{'env', 1, fun(sql_driver) -> pgsql; (_) -> undefined end}]}).
-define(MOCK_TSID, {elib_tsid, [{'generate', 1, fun(_Table) -> 999888777 end}]}).

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS([?MOCK_ENV], fun() ->
        Result = user_denylist_repo:tablename(),
        ?assertEqual(<<"public.user_denylist">>, Result)
    end).

%% ===================================================================
%% 黑名单查询测试
%% ===================================================================

page_for_uid_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
    ]}], fun() ->
        Uid = 1,
        Limit = 10,
        Offset = 0,
        Result = user_denylist_repo:page_for_uid(Uid, Limit, Offset),
        ?assertMatch({ok, List} when is_list(List), Result)
    end).

in_denylist_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'pluck_value', 5, fun(_Tb, _Col, _Where, _Opts, _Default) -> 0 end}
    ]}], fun() ->
        Uid = 1,
        DeniedUid = 2,
        Result = user_denylist_repo:in_denylist(Uid, DeniedUid),
        ?assert(is_integer(Result)),
        ?assert(Result >= 0)
    end).

in_denylist_found_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'pluck_value', 5, fun(_Tb, _Col, _Where, _Opts, _Default) -> 1 end}
    ]}], fun() ->
        Uid = 1,
        DeniedUid = 2,
        Result = user_denylist_repo:in_denylist(Uid, DeniedUid),
        ?assertEqual(1, Result)
    end).

%% ===================================================================
%% 黑名单操作测试
%% ===================================================================

add_to_denylist_test_() ->
    ?WITH_MECKS([?MOCK_ENV, ?MOCK_TSID, {elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ]}], fun() ->
        Uid = 1,
        BlockedUid = 2,
        Now = <<"2024-01-01T00:00:00Z">>,
        Result = user_denylist_repo:add(Uid, BlockedUid, Now),
        ?assertMatch({ok, Id} when is_integer(Id), Result)
    end).

remove_from_denylist_test_() ->
    ?WITH_MECKS([?MOCK_ENV, {elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ]}], fun() ->
        Uid = 1,
        BlockedUid = 2,
        Result = user_denylist_repo:remove(Uid, BlockedUid),
        ?assertEqual(ok, Result)
    end).
