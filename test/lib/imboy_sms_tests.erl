-module(imboy_sms_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_sms 模块的 EUnit 测试
%%%
%%% 目标：验证短信工具功能
%%% 覆盖：短信发送、验证码、手机号过滤
%%%===================================================================

%% 测试常量定义
-define(TEST_MOBILE, <<"+8613800138000">>).
-define(TEST_CONTENT, <<"验证码是1234">>).
-define(TEST_CODE, <<"1234">>).
-define(TEST_TOKEN, <<"test_login_token_12345">>).

%% 测试手机号过滤功能
filter_mobile_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试带+86前缀的手机号
        MobileWith86 = <<"+8613800138000">>,
        Filtered1 = imboy_sms:filter_mobile(MobileWith86),
        ?assertEqual(<<"13800138000">>, Filtered1),
        
        % 测试不带+86前缀的手机号
        MobileWithout86 = <<"13800138000">>,
        Filtered2 = imboy_sms:filter_mobile(MobileWithout86),
        ?assertEqual(<<"13800138000">>, Filtered2),
        
        % 测试其他格式的手机号
        OtherMobile = <<"8613800138000">>,
        Filtered3 = imboy_sms:filter_mobile(OtherMobile),
        ?assertEqual(<<"8613800138000">>, Filtered3)
    end).

%% 测试YJSMS短信发送
send_yjsms_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(config_ds, [no_link]),
        meck:new(elib_dt, [no_link]),
        meck:new(elib_hasher, [no_link]),
        meck:new(elib_log, [no_link]),
        meck:new(elib_req, [no_link]),

        % Mock配置获取
        meck:expect(config_ds, env, fun(yjsms_account, _) -> <<"test_account">>;
                                            (yjsms_secret, _) -> <<"test_secret">>;
                                            (yjsms_url, _) -> <<"https://test.sms.api">> end),

        % Mock时间戳
        meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),

        % Mock MD5计算
        meck:expect(elib_hasher, md5, fun(<<"test_secret">>) -> <<"hashed_secret">>;
                                            (_) -> <<"signature_hash">> end),

        % Mock elib_log 防止 lager 未启动导致 badmatch
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),

        % Mock HTTP请求
        meck:expect(elib_req, post, 3, fun(_URL, _Data, _Headers) ->
            {ok, #{<<"code">> => 0, <<"message">> => <<"success">>}}
        end),

        try
            % 测试YJSMS发送
            Result = imboy_sms:send(?TEST_MOBILE, ?TEST_CONTENT, <<"yjsms">>),
            ?assertMatch({ok, <<"success">>}, Result),

            % 验证HTTP请求被调用
            ?assert(meck:called(elib_req, post, 3))
        after
            % 清理Mock
            meck:unload(config_ds),
            meck:unload(elib_dt),
            meck:unload(elib_hasher),
            meck:unload(elib_log),
            meck:unload(elib_req)
        end
    end).

%% 测试YJSMS发送失败
send_yjsms_failure_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(config_ds, [no_link]),
        meck:new(elib_dt, [no_link]),
        meck:new(elib_hasher, [no_link]),
        meck:new(elib_log, [no_link]),
        meck:new(elib_req, [no_link]),

        % Mock配置获取
        meck:expect(config_ds, env, fun(yjsms_account, _) -> <<"test_account">>;
                                            (yjsms_secret, _) -> <<"test_secret">>;
                                            (yjsms_url, _) -> <<"https://test.sms.api">> end),

        % Mock时间戳
        meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),

        % Mock MD5计算
        meck:expect(elib_hasher, md5, fun(<<"test_secret">>) -> <<"hashed_secret">>;
                                            (_) -> <<"signature_hash">> end),

        % Mock elib_log 防止 lager 未启动导致 badmatch
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),

        % Mock HTTP请求失败
        meck:expect(elib_req, post, 3, fun(_URL, _Data, _Headers) ->
            {ok, #{<<"code">> => 1, <<"message">> => <<"account_empty">>}}
        end),

        try
            % 测试YJSMS发送失败
            Result = imboy_sms:send(?TEST_MOBILE, ?TEST_CONTENT, <<"yjsms">>),
            ?assertMatch({error, <<"account_empty">>}, Result),

            % 验证HTTP请求被调用
            ?assert(meck:called(elib_req, post, 3))
        after
            % 清理Mock
            meck:unload(config_ds),
            meck:unload(elib_dt),
            meck:unload(elib_hasher),
            meck:unload(elib_log),
            meck:unload(elib_req)
        end
    end).

%% 测试极光短信发送
send_jsms_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(config_ds, [no_link]),
        meck:new(elib_log, [no_link]),
        meck:new(elib_req, [no_link]),

        % Mock配置获取
        meck:expect(config_ds, env, fun(jpush_app_key, _) -> <<"test_app_key">>;
                                            (jpush_master_secret, _) -> <<"test_master_secret">> end),

        % Mock elib_log 防止 lager 未启动导致 badmatch
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),

        % Mock HTTP请求
        meck:expect(elib_req, post, 3, fun(_URL, _Data, _Headers) ->
            {ok, #{<<"msg_id">> => <<"123456789">>, <<"send_id">> => <<"987654321">>}}
        end),

        try
            % 测试极光短信发送
            Result = imboy_sms:send(?TEST_MOBILE, ?TEST_CODE, <<"jsms">>),
            ?assertEqual({ok, #{<<"msg_id">> => <<"123456789">>, <<"send_id">> => <<"987654321">>}}, Result),

            % 验证HTTP请求被调用
            ?assert(meck:called(elib_req, post, 3))
        after
            % 清理Mock
            meck:unload(config_ds),
            meck:unload(elib_log),
            meck:unload(elib_req)
        end
    end).

%% 测试极光验证
jverification_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(config_ds, [no_link]),
        meck:new(elib_log, [no_link]),
        meck:new(elib_req, [no_link]),

        % Mock配置获取
        meck:expect(config_ds, env, fun(jpush_app_key, _) -> <<"test_app_key">>;
                                            (jpush_master_secret, _) -> <<"test_master_secret">> end),

        % Mock elib_log 防止 lager 未启动导致 badmatch
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),

        % Mock HTTP请求: 返回 code 8001 (非 8000) 走 error 分支
        meck:expect(elib_req, post, 3, fun(_URL, _Data, _Headers) ->
            {ok, #{<<"phone">> => <<"13800138000">>, <<"code">> => 8001}}
        end),

        try
            % 测试极光验证（非 8000 code 走 error 分支）
            % RespMap has no <<"content">> key, so default <<"unknown">> is used
            Result = imboy_sms:jverification(?TEST_TOKEN),
            ?assertEqual({error, <<"unknown">>}, Result),

            % 验证HTTP请求被调用
            ?assert(meck:called(elib_req, post, 3))
        after
            % 清理Mock
            meck:unload(config_ds),
            meck:unload(elib_log),
            meck:unload(elib_req)
        end
    end).

%% 测试短信参数验证
sms_parameter_validation_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试有效手机号格式
        ValidMobiles = [
            <<"+8613800138000">>,
            <<"+8613912345678">>,
            <<"+8615012345678">>,
            <<"+8618012345678">>
        ],
        
        lists:foreach(fun(Mobile) ->
            Filtered = imboy_sms:filter_mobile(Mobile),
            ?assertMatch(<<_/binary>>, Filtered),
            ?assert(byte_size(Filtered) > 0)
        end, ValidMobiles),
        
        % 测试短信内容格式
        ValidContents = [
            <<"验证码是1234">>,
            <<"您的验证码是：5678，有效期5分钟">>,
            <<"【IMBoy】验证码：9012，请勿泄露">>,
            <<"Code: 3456, valid for 5 minutes">>
        ],
        
        lists:foreach(fun(Content) ->
            ?assertMatch(<<_/binary>>, Content),
            ?assert(byte_size(Content) > 0)
        end, ValidContents),
        
        % 测试验证码格式
        ValidCodes = [
            <<"1234">>,
            <<"5678">>,
            <<"9012">>,
            <<"3456">>
        ],
        
        lists:foreach(fun(Code) ->
            ?assertMatch(<<_/binary>>, Code),
            ?assert(byte_size(Code) =:= 4)
        end, ValidCodes)
    end).

%% 测试短信数据格式化
sms_data_formatting_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(config_ds, [no_link]),
        meck:new(elib_dt, [no_link]),
        meck:new(elib_hasher, [no_link]),
        meck:new(elib_log, [no_link]),
        meck:new(elib_req, [no_link]),

        % Mock配置
        meck:expect(config_ds, env, fun(yjsms_account, _) -> <<"test_account">>;
                                            (yjsms_secret, _) -> <<"test_secret">>;
                                            (yjsms_url, _) -> <<"https://test.sms.api">> end),

        % Mock时间和哈希
        meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),
        meck:expect(elib_hasher, md5, fun(<<"test_secret">>) -> <<"hashed_secret">>;
                                            (_) -> <<"signature_hash">> end),

        % Mock elib_log 防止 lager 未启动导致 badmatch
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),

        % Mock HTTP请求并捕获数据
        meck:expect(elib_req, post, 3, fun(URL, Data, Headers) ->
            ?assertEqual(<<"https://test.sms.api">>, URL),
            ?assert(is_map(Data)),
            ?assertMatch([_|_], Headers),

            % 验证请求数据格式
            ?assert(maps:is_key(<<"userName">>, Data)),
            ?assert(maps:is_key(<<"messageList">>, Data)),
            ?assert(maps:is_key(<<"timestamp">>, Data)),
            ?assert(maps:is_key(<<"sign">>, Data)),

            % 验证消息列表格式
            MessageList = maps:get(<<"messageList">>, Data),
            ?assertMatch([_|_], MessageList),
            ?assert(length(MessageList) > 0),

            [Message | _] = MessageList,
            ?assert(maps:is_key(<<"phone">>, Message)),
            ?assert(maps:is_key(<<"content">>, Message)),

            {ok, #{<<"code">> => 0, <<"message">> => <<"success">>}}
        end),

        try
            % 测试数据格式化
            Result = imboy_sms:send(?TEST_MOBILE, ?TEST_CONTENT, <<"yjsms">>),
            ?assertMatch({ok, <<"success">>}, Result)
        after
            % 清理Mock
            meck:unload(config_ds),
            meck:unload(elib_dt),
            meck:unload(elib_hasher),
            meck:unload(elib_log),
            meck:unload(elib_req)
        end
    end).

%% 测试签名生成
signature_generation_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(config_ds, [no_link]),
        meck:new(elib_dt, [no_link]),
        meck:new(elib_hasher, [no_link]),
        meck:new(elib_log, [no_link]),
        meck:new(elib_req, [no_link]),

        % Mock配置
        meck:expect(config_ds, env, fun(yjsms_account, _) -> <<"test_account">>;
                                            (yjsms_secret, _) -> <<"test_secret">>;
                                            (yjsms_url, _) -> <<"https://test.sms.api">> end),

        % Mock时间戳
        meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),

        % Mock MD5并验证签名计算
        % Source: Sign = elib_hasher:md5(<<Username/binary, TsBin/binary, elib_hasher:md5(Password)/binary>>)
        % First call:  md5(<<"test_secret">>) -> <<"hashed_secret">>
        % Second call: md5(<<"test_account1640995200000hashed_secret">>) -> <<"final_signature">>
        meck:expect(elib_hasher, md5, fun(<<"test_secret">>) -> <<"hashed_secret">>;
                                            (Data) ->
                                                % 验证签名数据格式
                                                ExpectedPattern = <<"test_account1640995200000hashed_secret">>,
                                                ?assertEqual(ExpectedPattern, Data),
                                                <<"final_signature">> end),

        % Mock elib_log 防止 lager 未启动导致 badmatch
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),

        % Mock HTTP请求
        meck:expect(elib_req, post, 3, fun(_URL, Data, _Headers) ->
            % 验证签名
            Sign = maps:get(<<"sign">>, Data),
            ?assertEqual(<<"final_signature">>, Sign),
            {ok, #{<<"code">> => 0, <<"message">> => <<"success">>}}
        end),

        try
            % 测试签名生成
            Result = imboy_sms:send(?TEST_MOBILE, ?TEST_CONTENT, <<"yjsms">>),
            ?assertMatch({ok, <<"success">>}, Result)
        after
            % 清理Mock
            meck:unload(config_ds),
            meck:unload(elib_dt),
            meck:unload(elib_hasher),
            meck:unload(elib_log),
            meck:unload(elib_req)
        end
    end).

%% 测试HTTP头设置
http_headers_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(config_ds, [no_link]),
        meck:new(elib_log, [no_link]),
        meck:new(elib_req, [no_link]),

        % Mock配置
        meck:expect(config_ds, env, fun(jpush_app_key, _) -> <<"test_app_key">>;
                                            (jpush_master_secret, _) -> <<"test_master_secret">> end),

        % Mock elib_log 防止 lager 未启动导致 badmatch
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),

        % Mock HTTP请求并验证头
        meck:expect(elib_req, post, 3, fun(_URL, _Data, Headers) ->
            ?assertMatch([_|_], Headers),

            % 验证Content-Type头
            ?assert(lists:keymember("Content-Type", 1, Headers)),
            {"Content-Type", ContentType} = lists:keyfind("Content-Type", 1, Headers),
            ?assertEqual("application/json", ContentType),

            % 验证Authorization头
            ?assert(lists:keymember("Authorization", 1, Headers)),
            {"Authorization", Auth} = lists:keyfind("Authorization", 1, Headers),
            ?assert(string:find(Auth, "Basic ") =/= nomatch),

            {ok, #{<<"msg_id">> => <<"123456789">>}}
        end),

        try
            % 测试HTTP头设置
            Result = imboy_sms:send(?TEST_MOBILE, ?TEST_CODE, <<"jsms">>),
            ?assertEqual({ok, #{<<"msg_id">> => <<"123456789">>}}, Result)
        after
            % 清理Mock
            meck:unload(config_ds),
            meck:unload(elib_log),
            meck:unload(elib_req)
        end
    end).

%% 测试错误处理
error_handling_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(config_ds, [no_link]),
        meck:new(elib_log, [no_link]),
        meck:new(elib_req, [no_link]),

        % Mock配置
        meck:expect(config_ds, env, fun(jpush_app_key, _) -> <<"test_app_key">>;
                                            (jpush_master_secret, _) -> <<"test_master_secret">> end),

        % Mock elib_log 防止 lager 未启动导致 badmatch
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),

        % Mock HTTP错误
        meck:expect(elib_req, post, 3, fun(_URL, _Data, _Headers) ->
            {error, timeout}
        end),

        try
            % 测试HTTP错误处理
            % 注意：这里可能会抛出异常，取决于elib_req:post的实现
            Result = (catch imboy_sms:send(?TEST_MOBILE, ?TEST_CODE, <<"jsms">>)),
            case Result of
                {'EXIT', _} -> ok;  % 预期的异常
                {error, _} -> ok;    % 预期的错误返回
                _ -> ?assert(false, "Expected error or exception")
            end
        after
            % 清理Mock
            meck:unload(config_ds),
            meck:unload(elib_log),
            meck:unload(elib_req)
        end
    end).
