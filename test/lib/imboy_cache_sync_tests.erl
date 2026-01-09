-module(imboy_cache_sync_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("cache.hrl").
-include("chat.hrl").

%% 定义state记录（与源文件保持一致）
-record(state, {}).

%%%===================================================================
%%% @doc
%%% imboy_cache_sync 模块的 EUnit 测试
%%%
%%% 目标：验证缓存同步功能
%%% 覆盖：跨节点缓存同步、消息广播、错误处理
%%%===================================================================

%% 测试常量定义
-define(TEST_KEY, <<"test_key">>).
-define(TEST_VALUE, <<"test_value">>).
-define(TEST_DATA, #{id => 123, name => <<"Test">>}).
-define(TEST_MAX_AGE, 3600).
-define(TEST_DEPEND, [<<"user:123">>]).

%% 测试缓存同步广播功能
broadcast_cache_sync_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        
        % Mock syn:publish/3
        meck:expect(syn, publish, 3, fun(Scope, Group, Message) ->
            ?assertEqual(?CACHE_SCOPE, Scope),
            ?assertEqual(dsync_handler, Group),
            ?assertMatch({cache_sync, _}, Message),
            ok
        end),
        
        try
            TestMessage = #{action => set, key => ?TEST_KEY, value => ?TEST_VALUE},
            
            % 测试广播缓存同步消息
            Result = imboy_cache_sync:broadcast(TestMessage),
            ?assertEqual(ok, Result),
            
            % 验证syn:publish被调用
            ?assert(meck:called(syn, publish, 3)),
            
            % 验证调用参数
            [{_, {Scope, Group, Message}, _}] = meck:history(syn),
            ?assertEqual(?CACHE_SCOPE, Scope),
            ?assertEqual(dsync_handler, Group),
            ?assertMatch({cache_sync, TestMessage}, Message)
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试服务器启动和注册
server_start_and_register_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:new(gen_server, [passthrough, no_link]),
        
        % Mock syn:join/4
        meck:expect(syn, join, 4, fun(Scope, Group, Pid, Meta) ->
            ?assertEqual(?CACHE_SCOPE, Scope),
            ?assertEqual(dsync_handler, Group),
            ?assert(is_pid(Pid)),
            ?assertEqual(#{}, Meta),
            ok
        end),
        
        % Mock gen_server:start_link/4
        meck:expect(gen_server, start_link, 4, fun({local, _Name}, _Module, _Args, _Options) ->
            {ok, self()}
        end),

        try
            % 测试服务器启动
            Result = imboy_cache_sync:start_link(),
            case Result of
                {ok, Pid} when is_pid(Pid) -> ?assert(true);
                {ok, _} -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, Pid}")
            end,

            % 验证gen_server:start_link被调用
            ?assert(meck:called(gen_server, start_link, 4)),

            % 验证syn:join被调用
            ?assert(meck:called(syn, join, 4))
        after
            % 清理Mock
            meck:unload(syn),
            meck:unload(gen_server)
        end
    end).

%% 测试服务器启动失败处理
server_start_failure_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:new(gen_server, [passthrough, no_link]),
        
        % Mock syn:join/4 返回错误
        meck:expect(syn, join, 4, fun(_Scope, _Group, _Pid, _Meta) ->
            {error, register_failed}
        end),
        
        % Mock gen_server:start_link/4
        meck:expect(gen_server, start_link, 4, fun({local, _Name}, _Module, _Args, _Options) ->
            % 模拟服务器启动但注册失败的情况
            {ok, self()}
        end),
        
        try
            % 这里我们直接测试init函数的行为
            % 因为start_link成功但init可能失败
            InitResult = imboy_cache_sync:init([]),
            ?assertMatch({stop, {syn_register_failed, _}}, InitResult)
        after
            % 清理Mock
            meck:unload(syn),
            meck:unload(gen_server)
        end
    end).

%% 测试缓存设置同步消息处理
handle_set_sync_message_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(depcache, [passthrough, no_link]),
        
        % Mock depcache:set/5
        meck:expect(depcache, set, 5, fun(Key, Data, MaxAge, Depend, Server) ->
            ?assertEqual(?TEST_KEY, Key),
            ?assertEqual(?TEST_DATA, Data),
            ?assertEqual(?TEST_MAX_AGE, MaxAge),
            ?assertEqual(?TEST_DEPEND, Depend),
            ?assertEqual(?DEPCACHE_SERVER, Server),
            ok
        end),
        
        try
            SyncMessage = {set, ?TEST_KEY, ?TEST_DATA, ?TEST_MAX_AGE, ?TEST_DEPEND},
            
            % 测试处理设置缓存同步消息
            % 直接调用内部函数
            imboy_cache_sync:handle_sync_message(SyncMessage),
            
            % 验证depcache:set被调用
            ?assert(meck:called(depcache, set, 5))
        after
            % 清理Mock
            meck:unload(depcache)
        end
    end).

%% 测试缓存清空同步消息处理
handle_flush_sync_message_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(depcache, [passthrough, no_link]),
        
        % Mock depcache:flush/2
        meck:expect(depcache, flush, 2, fun(Key, Server) ->
            ?assertEqual(?TEST_KEY, Key),
            ?assertEqual(?DEPCACHE_SERVER, Server),
            ok
        end),
        
        try
            SyncMessage = {flush, ?TEST_KEY},
            
            % 测试处理清空缓存同步消息
            imboy_cache_sync:handle_sync_message(SyncMessage),
            
            % 验证depcache:flush被调用
            ?assert(meck:called(depcache, flush, 2))
        after
            % 清理Mock
            meck:unload(depcache)
        end
    end).

%% 测试全部缓存清空同步消息处理
handle_flush_all_sync_message_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(depcache, [passthrough, no_link]),
        
        % Mock depcache:flush/1
        meck:expect(depcache, flush, 1, fun(Server) ->
            ?assertEqual(?DEPCACHE_SERVER, Server),
            ok
        end),
        
        try
            SyncMessage = flush,
            
            % 测试处理清空所有缓存同步消息
            imboy_cache_sync:handle_sync_message(SyncMessage),
            
            % 验证depcache:flush被调用
            ?assert(meck:called(depcache, flush, 1))
        after
            % 清理Mock
            meck:unload(depcache)
        end
    end).

%% 测试未知消息处理
handle_unknown_message_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试未知消息类型
        UnknownMessages = [
            {unknown_action, ?TEST_KEY, ?TEST_VALUE},
            {invalid},
            <<"binary_message">>,
            12345,
            [],
            #{invalid => <<"message">>}
        ],
        
        lists:foreach(fun(Message) ->
            % 未知消息应该被忽略，不应该抛出异常
            ?assertEqual(ok, imboy_cache_sync:handle_sync_message(Message))
        end, UnknownMessages)
    end).

%% 测试服务器信息处理
server_handle_info_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(depcache, [passthrough, no_link]),
        
        % Mock depcache:set/5
        meck:expect(depcache, set, 5, fun(_Key, _Data, _MaxAge, _Depend, _Server) ->
            ok
        end),
        
        try
            State = #state{},
            SyncMessage = {cache_sync, {set, ?TEST_KEY, ?TEST_DATA, ?TEST_MAX_AGE, ?TEST_DEPEND}},
            
            % 测试处理缓存同步信息
            Result = imboy_cache_sync:handle_info(SyncMessage, State),
            ?assertMatch({noreply, #state{}}, Result),
            
            % 验证depcache:set被调用
            ?assert(meck:called(depcache, set, 5))
        after
            % 清理Mock
            meck:unload(depcache)
        end
    end).

%% 测试服务器终止处理
server_terminate_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        
        % Mock syn:leave/3
        meck:expect(syn, leave, 3, fun(Scope, Group, Pid) ->
            ?assertEqual(?CACHE_SCOPE, Scope),
            ?assertEqual(dsync_handler, Group),
            ?assert(is_pid(Pid)),
            ok
        end),
        
        try
            State = #state{},
            Reason = normal,
            
            % 测试服务器终止处理
            Result = imboy_cache_sync:terminate(Reason, State),
            ?assertEqual(ok, Result),
            
            % 验证syn:leave被调用
            ?assert(meck:called(syn, leave, 3))
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试服务器代码更改处理
server_code_change_test_() ->
    ?TEST_WITH_APP(fun() ->
        State = #state{},
        OldVsn = "1.0.0",
        Extra = #{},
        
        % 测试代码更改处理
        Result = imboy_cache_sync:code_change(OldVsn, State, Extra),
        ?assertMatch({ok, #state{}}, Result)
    end).

%% 测试服务器调用处理
server_handle_call_test_() ->
    ?TEST_WITH_APP(fun() ->
        State = #state{},
        Request = some_request,
        From = self(),
        
        % 测试处理同步调用
        Result = imboy_cache_sync:handle_call(Request, From, State),
        ?assertMatch({reply, ok, #state{}}, Result)
    end).

%% 测试服务器转换处理
server_handle_cast_test_() ->
    ?TEST_WITH_APP(fun() ->
        State = #state{},
        Msg = some_message,
        
        % 测试处理异步调用
        Result = imboy_cache_sync:handle_cast(Msg, State),
        ?assertMatch({noreply, #state{}}, Result)
    end).

%% 测试非缓存同步信息处理
server_handle_non_sync_info_test_() ->
    ?TEST_WITH_APP(fun() ->
        State = #state{},
        NonSyncMessages = [
            some_other_message,
            {timeout, make_ref(), some_data},
            {'DOWN', make_ref(), process, self(), normal}
        ],
        
        lists:foreach(fun(Msg) ->
            % 非缓存同步消息应该被忽略，保持状态不变
            Result = imboy_cache_sync:handle_info(Msg, State),
            ?assertMatch({noreply, #state{}}, Result)
        end, NonSyncMessages)
    end).
