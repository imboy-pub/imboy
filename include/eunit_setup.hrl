%%%===================================================================
%%% @doc
%%% EUnit 测试通用头文件
%%%
%%% 使用方法：在测试文件中添加：
%%% -include_lib("eunit_setup.hrl").
%%%
%%% 提供：
%%% - 自动启动应用的 setup 函数
%%% - 自动清理的 cleanup 函数
%%% - 可选的数据库连接支持
%%% - 简化的测试宏
%%%
%%% 注意：辅助函数实现在 eunit_runner 模块中
%%%===================================================================

%% ===================================================================
%% 简化的测试宏
%% ===================================================================

%% @doc 创建一个自动启动应用的测试
%% 用法：
%% TEST_WITH_APP(fun() ->
%%     Result = my_repo:find(1),
%%     ?assertMatch({ok, _}, Result)
%% end).
-define(TEST_WITH_APP(TestFun),
    {setup,
     fun eunit_runner:eunit_setup/0,
     fun eunit_runner:eunit_cleanup/1,
     fun(_State) -> ?_test((TestFun)()) end}
).

%% @doc 创建一个需要数据库的测试
%% 如果数据库不可用，自动跳过
%% 用法：
%% TEST_WITH_DB(fun() ->
%%     Result = my_repo:find(1),
%%     ?assertMatch({ok, _}, Result)
%% end).
-define(TEST_WITH_DB(TestFun),
    {setup,
     fun() ->
         case eunit_runner:eunit_setup_with_db() of
             {ok, Conn} ->
                 {ok, Conn};
             {error, _Reason} ->
                 skip
         end
     end,
     fun({ok, Conn}) -> eunit_runner:eunit_cleanup_db(Conn);
        (skip) -> ok
     end,
     fun({ok, _Conn}) ->
             ?_test((TestFun)());
        (skip) ->
             []
     end}).

%% @doc 创建一个简单的测试（不需要应用）
%% 用法：
%% TEST_SIMPLE(fun() ->
%%     ?assertEqual(<<"public.user">>, elib_pg_sql:public_tablename(<<"user">>))
%% end).
-define(TEST_SIMPLE(TestFun),
    ?_test((TestFun)())
).

%% @doc 创建带数据库连接的测试

%% 提供 Conn 给测试函数

%% 用法：

%% TEST_WITH_CONN(fun(Conn) ->

%%     Result = elib_pg:query(<<"SELECT 1">>, [], Conn),

%%     ?assertMatch({ok, _}, Result)

%% end).

-define(TEST_WITH_CONN(TestFun),

    {setup,

     fun() ->

         case eunit_runner:eunit_setup_with_db() of

             {ok, Conn} -> Conn;

             {error, _} -> skip

         end

     end,

     fun(Conn) when is_pid(Conn) -> eunit_runner:eunit_cleanup_db(Conn);

        (skip) -> ok

     end,

     fun(Conn) when is_pid(Conn) -> ?_test((TestFun)(Conn));

        (skip) -> []

     end}).



%% @doc 创建一个带 Mock 的测试

%% 用法：

%% TEST_WITH_MOCK([

%%     {module, function, arity, fun MockFun end},

%%     {another_module, function, arity, fun MockFun end}

%% ], fun() ->

%%     Result = module:function(Arg),

%%     ?assertExpected(Expected, Result)

%% end).

-define(TEST_WITH_MOCK(MockConfigs, TestFun),

    {setup,

     fun() -> test_helper:setup_mock(MockConfigs) end,

     fun(Mocks) -> test_helper:cleanup_mock(Mocks) end,

     fun(_Mocks) -> ?_test((TestFun)()) end}).



%% ===================================================================

%% Mock 测试宏 (需要 test_helper 模块)

%% ===================================================================



%% @doc 创建带Mock的测试

%% 用法：

%% WITH_MOCK(auth_ds, [{get_token, 3, fun() -> <<"token">> end}], fun() ->

%%     Result = auth_logic:verify_token(<<"token">>),

%%     ?assertEqual(<<"ok">>, Result)

%% end).

-define(WITH_MOCK(Module, Expectations, TestFun),

    {setup,

     fun() -> test_helper:setup_mock(Module, Expectations) end,

     fun(_) -> test_helper:cleanup_mock(Module) end,

     fun(_) -> ?_test((TestFun)()) end}).



%% @doc 创建带多个Mock的测试

%% 用法：

%% WITH_MOCKS([{auth_ds, Exp1}, {user_repo, Exp2}], fun() ->

%%     % 测试代码

%% end).

-define(WITH_MOCKS(MockConfigs, TestFun),

    {setup,

     fun() ->

         lists:foreach(fun({Mod, Exp}) ->

             test_helper:setup_mock(Mod, Exp)

         end, MockConfigs)

     end,

     fun(_) ->

         Modules = [Mod || {Mod, _} <- MockConfigs],

         test_helper:cleanup_mocks(Modules)

     end,

     fun(_) -> ?_test((TestFun)()) end}).

%% @doc 创建带多个 Mock 的测试（使用 meck_helper）
%% 用法：
%% WITH_MECKS([{elib_param, [{'post', 1, fun() -> ... end}]},
%%             {user_repo, [{'find', 1, fun() -> ... end}]}], fun() ->
%%     % 测试代码
%% end).
-define(WITH_MECKS(MockConfigs, TestFun),
    {setup,
     fun() ->
         lists:foreach(fun({Module, Expectations}) ->
             case meck_helper:setup_mock(Module, Expectations) of
                 {ok, _} -> ok;
                 {error, Reason} -> ?debugFmt("Mock setup failed for ~p: ~p", [Module, Reason])
             end
         end, MockConfigs)
     end,
     fun(_) ->
         lists:foreach(fun({Module, _Expectations}) ->
             meck_helper:cleanup_mock(Module)
         end, MockConfigs)
     end,
     fun(_) -> ?_test((TestFun)()) end}).

%% @doc 创建带单个 Mock 的测试（使用 meck_helper）
%% 用法：
%% WITH_MECK(group_repo, [{'find', 1, fun() -> ... end}], fun() ->
%%     % 测试代码
%% end).
-define(WITH_MECK(Module, Expectations, TestFun),
    {setup,
     fun() ->
         case meck_helper:setup_mock(Module, Expectations) of
             {ok, _} -> ok;
             {error, Reason} -> ?debugFmt("Mock setup failed for ~p: ~p", [Module, Reason])
         end
     end,
     fun(_) ->
         meck_helper:cleanup_mock(Module)
     end,
     fun(_) -> ?_test((TestFun)()) end}).

%% ===================================================================
%% 强断言宏（替代标准 EUnit 宏）
%% ===================================================================

%% @doc 强相等断言
-define(ASSERT_EQUAL(Expected, Actual),
    ?assertEqual(Expected, Actual)).

%% @doc 强匹配断言
-define(ASSERT_MATCH(Pattern, Value),
    ?assertMatch(Pattern, Value)).

%% @doc 强 OK 断言
-define(ASSERT_OK(Result),
    ?assertMatch({ok, _}, Result)).

%% @doc 强 ERROR 断言
-define(ASSERT_ERROR(Result),
    ?assertMatch({error, _}, Result)).
