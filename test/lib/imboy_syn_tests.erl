-module(imboy_syn_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("chat.hrl").

%%%===================================================================
%%% @doc
%%% imboy_syn 模块的 EUnit 测试
%%%
%%% 目标：验证 Syn 进程注册功能
%%% 覆盖：进程注册、查找、消息发布、统计功能
%%%===================================================================

%% PR-2γ: init/0 必须把 ?QR_LOGIN_SCOPE (imboy_qr_login) 加入 syn scopes
%% 否则 qr_login_event_ds:notify/2 在生产环境永远走兜底 {ok, 0} 路径
init_registers_qr_login_scope_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, add_node_to_scopes, 1, fun(_Scopes) -> ok end),

        try
            ok = imboy_syn:init(),
            ?assert(meck:called(syn, add_node_to_scopes, 1)),
            %% history 形如 [{Pid, {syn, add_node_to_scopes, [Scopes]}, ok}, ...]
            History = meck:history(syn),
            [{_, {syn, add_node_to_scopes, [Scopes]}, _} | _] = History,
            ?assert(lists:member(imboy_qr_login, Scopes),
                    "imboy_syn:init/0 必须把 imboy_qr_login 注册到 syn scopes")
        after
            meck:unload(syn)
        end
    end).

init_keeps_existing_chat_scope_test_() ->
    %% 防御性：新加 scope 不能误删 ?CHAT_SCOPE
    ?TEST_WITH_APP(fun() ->
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, add_node_to_scopes, 1, fun(_Scopes) -> ok end),

        try
            ok = imboy_syn:init(),
            [{_, {syn, add_node_to_scopes, [Scopes]}, _} | _] = meck:history(syn),
            ?assert(lists:member(?CHAT_SCOPE, Scopes),
                    "imboy_syn:init/0 不能丢弃 ?CHAT_SCOPE"),
            ?assert(lists:member(?ROOM_SCOPE, Scopes)),
            ?assert(lists:member(?CACHE_SCOPE, Scopes))
        after
            meck:unload(syn)
        end
    end).

%% 测试用户加入聊天会话
join_chat_session_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, join, 4, fun(_Scope, _Uid, _Pid, _Meta) -> ok end),

        try
            Uid = 12345,
            DType = <<"macos">>,
            Pid = self(),
            DID = <<"device_123">>,

            % 测试成功加入会话
            Result = imboy_syn:join(Uid, DType, Pid, DID),
            case Result of
                {ok, _} -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, _}")
            end,

            % 验证syn:join被正确调用
            ?assert(meck:called(syn, join, 4)),

            % 验证调用参数
            [{_, {Scope, Uid2, Pid2, Meta}, _}] = meck:history(syn),
            ?assertEqual(?CHAT_SCOPE, Scope),
            ?assertEqual(Uid, Uid2),
            ?assertEqual(Pid, Pid2),
            ?assertEqual({DType, DID}, Meta)
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试用户离开聊天会话
leave_chat_session_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, leave, 3, fun(_Scope, _Uid, _Pid) -> ok end),

        try
            Uid = 12345,
            Pid = self(),

            % 测试成功离开会话
            Result = imboy_syn:leave(Uid, Pid),
            case Result of
                {ok, _} -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, _}")
            end,
            
            % 验证syn:leave被正确调用
            ?assert(meck:called(syn, leave, 3)),
            
            % 验证调用参数
            [{_, {Scope, Uid2, Pid2}, _}] = meck:history(syn),
            ?assertEqual(?CHAT_SCOPE, Scope),
            ?assertEqual(Uid, Uid2),
            ?assertEqual(Pid, Pid2)
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试获取用户在线设备列表
list_by_uid_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, members, 2, fun(_Scope, _Uid) -> 
            [{self(), {<<"macos">>, <<"device_123">>}}] 
        end),
        
        try
            Uid = 12345,
            ExpectedList = [{self(), {<<"macos">>, <<"device_123">>}}],
            
            % 测试获取用户设备列表
            Result = imboy_syn:list_by_uid(Uid),
            ?assertEqual(ExpectedList, Result),
            
            % 验证syn:members被正确调用
            ?assert(meck:called(syn, members, 2)),
            
            % 验证调用参数
            [{_, {Scope, Uid2}, _}] = meck:history(syn),
            ?assertEqual(?CHAT_SCOPE, Scope),
            ?assertEqual(Uid, Uid2)
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试检查用户是否在线（按设备类型）
is_online_by_dtype_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, members, 2, fun(_Scope, _Uid) -> 
            [{self(), {<<"macos">>, <<"device_123">>}}] 
        end),
        
        try
            Uid = 12345,
            
            % 测试设备类型匹配的在线检查
            Result1 = imboy_syn:is_online(Uid, {dtype, <<"macos">>}),
            ?assertEqual(true, Result1),
            
            % 测试设备类型不匹配的在线检查
            Result2 = imboy_syn:is_online(Uid, {dtype, <<"ios">>}),
            ?assertEqual(false, Result2),
            
            % 验证syn:members被调用
            ?assert(meck:called(syn, members, 2))
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试检查用户是否在线（按设备ID）
is_online_by_did_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, members, 2, fun(_Scope, _Uid) -> 
            [{self(), {<<"macos">>, <<"device_123">>}}] 
        end),
        
        try
            Uid = 12345,
            
            % 测试设备ID匹配的在线检查
            Result1 = imboy_syn:is_online(Uid, {did, <<"device_123">>}),
            ?assertEqual(true, Result1),
            
            % 测试设备ID不匹配的在线检查
            Result2 = imboy_syn:is_online(Uid, {did, <<"device_456">>}),
            ?assertEqual(false, Result2),
            
            % 验证syn:members被调用
            ?assert(meck:called(syn, members, 2))
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试获取用户在线设备ID列表
online_dids_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, members, 2, fun(_Scope, _Uid) -> 
            [{self(), {<<"macos">>, <<"device_123">>}},
             {self(), {<<"ios">>, <<"device_456">>}}] 
        end),
        
        try
            Uid = 12345,
            ExpectedDids = [<<"device_123">>, <<"device_456">>],
            
            % 测试获取在线设备ID列表
            Result = imboy_syn:online_dids(Uid),
            ?assertEqual(ExpectedDids, Result),
            
            % 验证syn:members被正确调用
            ?assert(meck:called(syn, members, 2))
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试立即发布消息
publish_immediate_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, members, 2, fun(_Scope, _Uid) -> 
            [{self(), {<<"macos">>, <<"device_123">>}},
             {spawn(fun() -> timer:sleep(1000) end), {<<"ios">>, <<"device_456">>}}] 
        end),
        
        try
            Uid = 12345,
            Message = #{<<"type">> => <<"text">>, <<"content">> => <<"Hello">>},
            
            % 清空当前进程邮箱
            receive _ -> ok after 0 -> ok end,
            
            % 测试立即发布消息
            Result = imboy_syn:publish(Uid, Message),
            ?assertMatch({ok, 2}, Result),
            
            % 验证syn:members被正确调用
            ?assert(meck:called(syn, members, 2)),
            
            % 验证消息被发送到当前进程（立即投递也使用 start_timer，格式为 {timeout, Ref, Msg}）
            receive
                {timeout, _TimerRef, Msg} -> ?assertEqual(Message, Msg)
            after 100 ->
                ?assert(false, "Message not received")
            end
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试延迟发布消息
publish_delayed_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, members, 2, fun(_Scope, _Uid) -> 
            [{self(), {<<"macos">>, <<"device_123">>}}] 
        end),
        
        try
            Uid = 12345,
            Message = #{<<"type">> => <<"text">>, <<"content">> => <<"Delayed Hello">>},
            Delay = 100, % 100ms延迟
            
            % 测试延迟发布消息
            Result = imboy_syn:publish(Uid, Message, Delay),
            ?assertMatch({ok, 1}, Result),
            
            % 验证syn:members被正确调用
            ?assert(meck:called(syn, members, 2)),
            
            % 验证定时器消息被发送
            receive
                {timeout, _TimerRef, Msg} -> ?assertEqual(Message, Msg)
            after 200 ->
                ?assert(false, "Delayed message not received")
            end
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试统计在线用户数
count_user_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, group_count, 1, fun(_Scope) -> 100 end),
        
        try
            % 测试统计在线用户数
            Result = imboy_syn:count_user(),
            ?assertEqual(100, Result),
            
            % 验证syn:group_count被正确调用
            ?assert(meck:called(syn, group_count, 1)),
            
            % 验证调用参数
            [{_, {Scope}, _}] = meck:history(syn),
            ?assertEqual(?CHAT_SCOPE, Scope)
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试统计用户设备数
count_user_devices_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, member_count, 2, fun(_Scope, _Uid) -> 3 end),
        
        try
            Uid = 12345,
            
            % 测试统计用户设备数
            Result = imboy_syn:count_user(Uid),
            ?assertEqual(3, Result),
            
            % 验证syn:member_count被正确调用
            ?assert(meck:called(syn, member_count, 2)),
            
            % 验证调用参数
            [{_, {Scope, Uid2}, _}] = meck:history(syn),
            ?assertEqual(?CHAT_SCOPE, Scope),
            ?assertEqual(Uid, Uid2)
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).

%% 测试统计所有在线设备数
count_all_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn_backbone, [passthrough, no_link]),
        meck:expect(syn_backbone, get_table_name, 2, fun(syn_pg_by_name, _Scope) -> 'test_table' end),
        
        % Mock ets表信息
        meck:new(ets, [unstick, passthrough]),
        meck:expect(ets, info, fun('test_table', size) -> 500 end),
        
        try
            % 测试统计所有在线设备数
            Result = imboy_syn:count(),
            ?assertEqual(500, Result),
            
            % 验证syn_backbone:get_table_name被正确调用
            ?assert(meck:called(syn_backbone, get_table_name, 2)),
            
            % 验证ets:info被正确调用
            ?assert(meck:called(ets, info, 2))
        after
            % 清理Mock
            meck:unload(ets),
            meck:unload(syn_backbone)
        end
    end).

%% 测试按限制获取列表
list_by_limit_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn_backbone, [passthrough, no_link]),
        meck:expect(syn_backbone, get_table_name, 2, fun(syn_pg_by_name, _Scope) -> 'test_table' end),
        
        Limit = 10,
        ExpectedResult = [{uid1, pid1}, {uid2, pid2}],
        
        % Mock ets:select返回结果
        meck:new(ets, [unstick, passthrough]),
        meck:expect(ets, select, fun('test_table', _MatchSpec, 10) -> 
            {ExpectedResult, continuation} 
        end),
        
        try
            % 测试按限制获取列表
            Result = imboy_syn:list_by_limit(Limit),
            ?assertEqual(ExpectedResult, Result),
            
            % 验证syn_backbone:get_table_name被正确调用
            ?assert(meck:called(syn_backbone, get_table_name, 2)),
            
            % 验证ets:select被正确调用
            ?assert(meck:called(ets, select, 3))
        after
            % 清理Mock
            meck:unload(ets),
            meck:unload(syn_backbone)
        end
    end).

%% 测试错误处理
error_handling_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(syn, [passthrough, no_link]),
        meck:expect(syn, join, 4, fun(_Scope, _Uid, _Pid, _Meta) -> 
            throw({error, test_error}) 
        end),
        
        try
            Uid = 12345,
            DType = <<"macos">>,
            Pid = self(),
            DID = <<"device_123">>,
            
            % 测试异常处理
            Result = imboy_syn:join(Uid, DType, Pid, DID),
            ?assertMatch({error, test_error}, Result),
            
            % 验证syn:join被调用
            ?assert(meck:called(syn, join, 4))
        after
            % 清理Mock
            meck:unload(syn)
        end
    end).
