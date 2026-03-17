-module(elib_log_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_log 模块的 EUnit 测试
%%%
%%% 目标：验证日志工具功能
%%% 覆盖：日志级别、格式化输出、错误处理
%%%===================================================================

%% 测试常量定义
-define(TEST_MESSAGE, <<"Test log message">>).
-define(TEST_FORMAT, "Test format: ~s, ~p, ~w").
-define(TEST_ARGS, [<<"arg1">>, 123, {tuple, arg}]).

%% 测试debug级别日志
debug_log_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(Level, Metadata, Message) ->
            ?assertEqual(debug, Level),
            ?assertMatch([_|_], Metadata),
            ?assert(lists:keymember(module, 1, Metadata)),
            ?assert(lists:keymember(line, 1, Metadata)),
            ?assert(lists:keymember(pid, 1, Metadata)),
            ?assertMatch([_|_], Message)
        end),
        
        try
            % 测试debug日志（字符串消息）
            elib_log:debug(?TEST_MESSAGE),
            
            % 验证lager:log被调用
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试info级别日志
info_log_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(Level, Metadata, Message) ->
            ?assertEqual(info, Level),
            ?assertMatch([_|_], Metadata),
            ?assertMatch([_|_], Message)
        end),
        
        try
            % 测试info日志
            elib_log:info(?TEST_MESSAGE),
            
            % 验证lager:log被调用
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试notice级别日志
notice_log_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(Level, Metadata, Message) ->
            ?assertEqual(notice, Level),
            ?assertMatch([_|_], Metadata),
            ?assertMatch([_|_], Message)
        end),
        
        try
            % 测试notice日志
            elib_log:notice(?TEST_MESSAGE),
            
            % 验证lager:log被调用
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试warning级别日志
warning_log_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(Level, Metadata, Message) ->
            ?assertEqual(warning, Level),
            ?assertMatch([_|_], Metadata),
            ?assertMatch([_|_], Message)
        end),
        
        try
            % 测试warning日志
            elib_log:warning(?TEST_MESSAGE),
            
            % 验证lager:log被调用
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试error级别日志
error_log_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(Level, Metadata, Message) ->
            ?assertEqual(error, Level),
            ?assertMatch([_|_], Metadata),
            ?assertMatch([_|_], Message)
        end),
        
        try
            % 测试error日志
            elib_log:error(?TEST_MESSAGE),
            
            % 验证lager:log被调用
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试格式化日志
format_log_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(Level, Metadata, Message) ->
            MessageBin = iolist_to_binary(Message),
            ?assertEqual(info, Level),
            ?assertMatch([_|_], Metadata),
            ?assertMatch([_|_], Message),
            % 验证格式化结果
            ?assert(binary:match(MessageBin, <<"Test format:">>) =/= nomatch)
        end),
        
        try
            % 测试格式化日志
            elib_log:info(?TEST_FORMAT, ?TEST_ARGS),
            
            % 验证lager:log被调用
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试不同消息类型
different_message_types_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(_Level, _Metadata, Message) ->
            ?assertMatch([_|_], Message)
        end),
        
        try
            % 测试二进制消息
            elib_log:info(<<"Binary message">>),
            
            % 测试列表消息
            elib_log:info("String message"),
            
            % 测试原子消息
            elib_log:info(atom_message),
            
            % 测试元组消息
            elib_log:info({tuple, message}),
            
            % 测试映射消息
            elib_log:info(#{key => <<"value">>}),
            
            % 验证所有调用都成功
            ?assertEqual(5, meck:num_calls(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试日志元数据
log_metadata_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(_Level, Metadata, _Message) ->
            ?assertMatch([_|_], Metadata),
            
            % 验证必需的元数据字段
            ?assert(lists:keymember(module, 1, Metadata)),
            ?assert(lists:keymember(line, 1, Metadata)),
            ?assert(lists:keymember(pid, 1, Metadata)),
            
            % 验证元数据值类型
            {module, Module} = lists:keyfind(module, 1, Metadata),
            {line, Line} = lists:keyfind(line, 1, Metadata),
            {pid, Pid} = lists:keyfind(pid, 1, Metadata),
            
            ?assert(is_atom(Module)),
            ?assert(is_integer(Line)),
            ?assert(is_pid(Pid))
        end),
        
        try
            % 测试日志元数据
            elib_log:info(?TEST_MESSAGE),
            
            % 验证lager:log被调用
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试错误处理
error_handling_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3 来捕获错误消息
        meck:expect(lager, log, 3, fun(_Level, _Metadata, Message) ->
            MessageBin = iolist_to_binary(Message),
            % 验证错误消息格式
            ?assertMatch([_|_], Message),
            case MessageBin of
                <<"INVALID_MESSAGE">> ->
                    ok;
                _ ->
                    ?assert(binary:match(MessageBin, <<"INVALID_FORMAT:">>) =/= nomatch)
            end
        end),
        
        try
            % 测试无效格式字符串（会导致格式化失败）
            elib_log:info("Invalid format ~p", [too, many, args]),
            
            % 验证错误处理
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试参数清理
argument_sanitization_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(_Level, _Metadata, Message) ->
            MessageBin = iolist_to_binary(Message),
            ?assertMatch([_|_], Message),
            % 验证二进制参数被转换为字符串
            ?assert(binary:match(MessageBin, <<"<<">>) =:= nomatch)
        end),
        
        try
            % 测试包含二进制参数的格式化日志
            elib_log:info("Binary arg: ~s", [<<"binary_arg">>]),
            
            % 验证参数清理
            ?assert(meck:called(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试内部日志函数
internal_log_functions_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        % Mock lager:log/3
        meck:expect(lager, log, 3, fun(Level, Metadata, Message) ->
            ?assertEqual(debug, Level),
            ?assertMatch([_|_], Metadata),
            ?assertMatch([_|_], Message)
        end),
        
        try
            % 测试内部日志函数（4个参数）
            elib_log:internal_log(debug, ?TEST_MESSAGE, test_module, 123),
            
            % 测试内部日志函数（5个参数）
            elib_log:internal_log(debug, "Format: ~s", [<<"arg">>], test_module, 123),
            
            % 验证所有调用
            ?assertEqual(2, meck:num_calls(lager, log, 3))
        after
            % 清理Mock
            meck:unload(lager)
        end
    end).

%% 测试日志级别过滤
log_level_filtering_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(lager, [passthrough, no_link]),
        
        _CallCount = 0,
        
        % Mock lager:log/3 并计数调用
        meck:expect(lager, log, 3, fun(_Level, _Metadata, _Message) ->
            put(call_count, get(call_count) + 1)
        end),
        
        try
            % 初始化计数器
            put(call_count, 0),
            
            % 测试不同级别的日志（取决于LOG_LEVEL设置）
            elib_log:debug(?TEST_MESSAGE),
            elib_log:info(?TEST_MESSAGE),
            elib_log:notice(?TEST_MESSAGE),
            elib_log:warning(?TEST_MESSAGE),
            elib_log:error(?TEST_MESSAGE),
            
            % 当前实现的日志级别阈值是 debug，5 个入口都会调用
            FinalCount = get(call_count),
            ?assertEqual(5, FinalCount)
        after
            % 清理Mock
            meck:unload(lager),
            erase(call_count)
        end
    end).
