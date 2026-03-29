-module(test_helper).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% 测试辅助模块
%%%
%%% 提供：
%%% - 简化的 meck Mock 管理
%%% - 测试数据生成器
%%% - 数据库测试辅助函数
%%% - 通用断言辅助函数
%%%===================================================================

-export([
    setup_mock/1,
    setup_mock/2,
    setup_mock/3,
    cleanup_mock/1,
    cleanup_mocks/1,
    
    generate_user/0,
    generate_user/1,
    generate_group/0,
    generate_group/1,
    generate_message/0,
    generate_message/1,
    
    assert_ok/1,
    assert_error/1,
    assert_match/2,
    
    with_db/1,
    with_mock/3
]).

%% ===================================================================
%% Meck Mock 管理函数
%% ===================================================================

%% @doc 为单个模块设置Mock
%% Module: 要Mock的模块名
%% Expectations: 期望函数列表，格式为 [{Function, Arity, Fun}]
setup_mock(Module, Expectations) ->
    try
        meck:new(Module, [passthrough]),
        lists:foreach(fun({Func, Arity, Fun}) ->
            meck:expect(Module, Func, Arity, normalize_mock_fun(Fun, Arity))
        end, Expectations)
    catch
        error:{already_started, _} ->
            % 如果已经启动，先卸载再重新创建
            meck:unload(Module),
            meck:new(Module, [passthrough]),
            lists:foreach(fun({Func, Arity, Fun}) ->
                meck:expect(Module, Func, Arity, normalize_mock_fun(Fun, Arity))
            end, Expectations)
    end.

%% @doc 为单个模块设置Mock（带选项）
%% Module: 要Mock的模块名
%% Options: meck选项，如 [passthrough, unstick]
%% Expectations: 期望函数列表
setup_mock(Module, Options, Expectations) ->
    meck:new(Module, Options),
    lists:foreach(fun({Func, Arity, Fun}) ->
        meck:expect(Module, Func, Arity, normalize_mock_fun(Fun, Arity))
    end, Expectations).

%% @doc 设置 Mock 配置
%% @param MockConfigs Mock 配置列表
%% @returns Mock 列表
setup_mock(MockConfigs) ->
    Mocks = lists:map(fun({Module, Function, Arity, MockFun}) ->
        ok = meck:new(Module, [passthrough]),
        ok = meck:expect(Module, Function, Arity, normalize_mock_fun(MockFun, Arity)),
        {Module, Function}
    end, MockConfigs),
    Mocks.

%% @doc 清理单个Mock
cleanup_mock(Module) ->
    try
        case meck:is_loaded(Module) of
            true -> meck:unload(Module);
            false -> ok
        end
    catch
        _:undef -> 
            % meck 可能未加载，忽略错误
            ok
    end.

%% @doc 批量清理Mock
cleanup_mocks(Modules) ->
    lists:foreach(fun cleanup_mock/1, Modules).

%% ===================================================================
%% 测试数据生成器
%% ===================================================================

%% @doc 生成默认用户数据
generate_user() ->
    generate_user(#{}).

%% @doc 生成自定义用户数据
generate_user(Overrides) ->
    Default = #{
        id => 12345,
        account => <<"test_user_12345">>,
        nickname => <<"Test User">>,
        mobile => <<"+8613800138000">>,
        email => <<"test@example.com">>,
        password => <<"hashed_password">>,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% @doc 生成默认群组数据
generate_group() ->
    generate_group(#{}).

%% @doc 生成自定义群组数据
generate_group(Overrides) ->
    Default = #{
        id => 54321,
        name => <<"Test Group">>,
        description => <<"A test group for testing">>,
        creator_id => 12345,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% @doc 生成默认消息数据
generate_message() ->
    generate_message(#{}).

%% @doc 生成自定义消息数据
generate_message(Overrides) ->
    Default = #{
        id => 99999,
        from_uid => 12345,
        to_uid => 67890,
        content => <<"Hello, this is a test message">>,
        msg_type => 1,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% ===================================================================
%% 断言辅助函数
%% ===================================================================

%% @doc 断言结果是 {ok, Value} 并返回 Value
assert_ok({ok, Value}) -> Value;
assert_ok({error, Reason}) -> ?assert(false, {error, Reason});
assert_ok(Result) -> ?assert(false, Result).

%% @doc 断言结果是 {error, Reason} 并返回 Reason
assert_error({error, Reason}) -> Reason;
assert_error({ok, Value}) -> ?assert(false, {ok, Value});
assert_error(Result) -> ?assert(false, Result).

%% @doc 断言匹配指定模式
assert_match(Pattern, Value) ->
    ?assertMatch(Pattern, Value).

%% ===================================================================
%% 测试上下文辅助函数
%% ===================================================================

%% @doc 在数据库上下文中执行测试
with_db(TestFun) ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            try
                TestFun(Conn)
            after
                % 清理测试数据
                cleanup_test_data(Conn),
                eunit_runner:eunit_cleanup_db(Conn)
            end;
        {error, Reason} ->
            ?debugFmt("Database not available: ~p", [Reason]),
            skip
    end.

%% @doc 在Mock上下文中执行测试
with_mock(Module, Expectations, TestFun) ->
    setup_mock(Module, Expectations),
    try
        TestFun()
    after
        cleanup_mock(Module)
    end.

normalize_mock_fun(Fun, Arity) ->
    {arity, FunArity} = erlang:fun_info(Fun, arity),
    case FunArity of
        Arity ->
            Fun;
        N when N =:= Arity - 1 ->
            wrap_drop_first_arg(Fun, Arity);
        _ ->
            Fun
    end.

wrap_drop_first_arg(Fun, 1) ->
    fun(_A1) ->
        Fun()
    end;
wrap_drop_first_arg(Fun, 2) ->
    fun(_A1, A2) ->
        Fun(A2)
    end;
wrap_drop_first_arg(Fun, 3) ->
    fun(_A1, A2, A3) ->
        Fun(A2, A3)
    end;
wrap_drop_first_arg(Fun, 4) ->
    fun(_A1, A2, A3, A4) ->
        Fun(A2, A3, A4)
    end;
wrap_drop_first_arg(Fun, 5) ->
    fun(_A1, A2, A3, A4, A5) ->
        Fun(A2, A3, A4, A5)
    end;
wrap_drop_first_arg(Fun, _) ->
    Fun.

%% ===================================================================
%% 内部辅助函数
%% ===================================================================

%% @doc 清理测试数据
cleanup_test_data(Conn) ->
    % 清理测试用户数据
    TestUsers = [12345, 67890],
    lists:foreach(fun(Uid) ->
        Sql = <<"DELETE FROM public.user WHERE id = $1">>,
        elib_pg:query(Sql, [Uid], Conn)
    end, TestUsers),
    
    % 清理测试群组数据
    TestGroups = [54321],
    lists:foreach(fun(Gid) ->
        Sql = <<"DELETE FROM public.group WHERE id = $1">>,
        elib_pg:query(Sql, [Gid], Conn)
    end, TestGroups),
    
    % 清理测试消息数据
    TestMsgs = [99999],
    lists:foreach(fun(MsgId) ->
        Sql = <<"DELETE FROM public.msg_c2c WHERE id = $1">>,
        elib_pg:query(Sql, [MsgId], Conn)
    end, TestMsgs).

%% ===================================================================
%% EUnit 测试辅助宏
%% ===================================================================

%% @doc 创建带Mock的测试
-define(WITH_MOCK(Module, Expectations, TestFun),
    {setup,
     fun() -> test_helper:setup_mock(Module, Expectations) end,
     fun(_) -> test_helper:cleanup_mock(Module) end,
     fun(_) -> ?_test(TestFun()) end}).

%% @doc 创建带多个Mock的测试
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
     fun(_) -> ?_test(TestFun()) end}).

%% @doc 创建数据库测试
-define(WITH_DB(TestFun),
    {setup,
     fun() -> 
         case eunit_runner:eunit_setup_with_db() of
             {ok, Conn} -> {ok, Conn};
             {error, _} -> skip
         end
     end,
     fun({ok, Conn}) ->
         test_helper:cleanup_test_data(Conn),
         eunit_runner:eunit_cleanup_db(Conn);
        (skip) -> ok
     end,
     fun({ok, Conn}) -> ?_test(TestFun(Conn));
        (skip) -> []
     end}).
