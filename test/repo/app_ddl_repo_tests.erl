-module(app_ddl_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_ddl_repo 模块的 EUnit 测试
%%%
%%% 目标：验证应用DDL数据仓库操作
%%% 覆盖：表名获取、添加操作
%%%
%%% 规则：不 mock elib_pg_sql；elib_pg 和 elib_tsid 使用 [no_link]
%%%===================================================================

%% ===================================================================
%% Helper: meck setup/cleanup
%% ===================================================================

setup_pg_tsid_mocks(PgExpectations, TsidExpectations) ->
    catch meck:unload(elib_pg),
    catch meck:unload(elib_tsid),
    meck:new(elib_pg, [no_link]),
    meck:new(elib_tsid, [no_link]),
    lists:foreach(fun({Func, Arity, Fun}) ->
        meck:expect(elib_pg, Func, Arity, Fun)
    end, PgExpectations),
    lists:foreach(fun({Func, Arity, Fun}) ->
        meck:expect(elib_tsid, Func, Arity, Fun)
    end, TsidExpectations).

cleanup_pg_tsid_mocks() ->
    catch meck:unload(elib_pg),
    catch meck:unload(elib_tsid),
    ok.

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_with_app_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        %% eunit_setup sets sql_driver=pgsql, so public_tablename adds "public." prefix
        Result = app_ddl_repo:tablename(),
        ?assertEqual(<<"public.app_ddl">>, Result)
    end).

tablename_is_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = app_ddl_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0),
        ?assert(string:find(binary_to_list(Result), "app_ddl") =/= nomatch)
    end).

%% ===================================================================
%% add/1 测试
%% ===================================================================

add_success_test_() ->
    {setup,
     fun() -> setup_pg_tsid_mocks(
         [{'query', 2, fun(_Sql, _Params) -> {ok, 1} end}],
         [{'generate', 1, fun(app_ddl) -> 99999 end}]
     ) end,
     fun(_) -> cleanup_pg_tsid_mocks() end,
     fun(_) -> ?_test(begin
         Data = #{
             app_key => <<"test_app">>,
             version => <<"1.0.0">>,
             ddl_sql => <<"CREATE TABLE test (id INT);">>,
             status => 1
         },
         Result = app_ddl_repo:add(Data),
         ?assertEqual({ok, 99999}, Result)
     end) end}.

add_error_test_() ->
    {setup,
     fun() -> setup_pg_tsid_mocks(
         [{'query', 2, fun(_Sql, _Params) -> {error, duplicate_key} end}],
         [{'generate', 1, fun(app_ddl) -> 88888 end}]
     ) end,
     fun(_) -> cleanup_pg_tsid_mocks() end,
     fun(_) -> ?_test(begin
         Data = #{
             app_key => <<"test_app">>,
             version => <<"1.0.0">>
         },
         Result = app_ddl_repo:add(Data),
         ?assertEqual({error, duplicate_key}, Result)
     end) end}.

add_with_empty_data_test_() ->
    {setup,
     fun() -> setup_pg_tsid_mocks(
         [{'query', 2, fun(_Sql, _Params) -> {ok, 1} end}],
         [{'generate', 1, fun(app_ddl) -> 77777 end}]
     ) end,
     fun(_) -> cleanup_pg_tsid_mocks() end,
     fun(_) -> ?_test(begin
         Data = #{},
         Result = app_ddl_repo:add(Data),
         %% elib_pg_sql:insert 构建 SQL，elib_pg:query 执行
         %% 空 map 也能构建 INSERT（只有 id 字段）
         ?assertEqual({ok, 77777}, Result)
     end) end}.

add_verifies_id_injected_test_() ->
    {setup,
     fun() -> setup_pg_tsid_mocks(
         [{'query', 2, fun(_Sql, _Params) -> {ok, 1} end}],
         [{'generate', 1, fun(app_ddl) -> 55555 end}]
     ) end,
     fun(_) -> cleanup_pg_tsid_mocks() end,
     fun(_) -> ?_test(begin
         Data = #{app_key => <<"minimal_app">>, version => <<"1.0">>},
         {ok, Id} = app_ddl_repo:add(Data),
         %% 返回的 Id 必须是 elib_tsid:generate 生成的值
         ?assertEqual(55555, Id)
     end) end}.
