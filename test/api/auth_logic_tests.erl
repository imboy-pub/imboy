-module(auth_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% auth_logic 模块的 EUnit 测试
%%%
%%% 目标：验证认证业务逻辑功能
%%% 覆盖：verify_for_assets/4, verify_for_open/3
%%%===================================================================

%% ===================================================================
%% verify_for_assets/4 测试
%% ===================================================================

verify_for_assets_with_valid_token_test_() ->
    ?WITH_MOCK(auth_ds, [
        {get_token, 3, fun(assets, _Scene, _Timestamp) ->
            % Mock 返回与输入 token 匹配的值
            <<"valid_token_123">>
        end}
    ], fun() ->
        Scene = <<"upload">>,
        Token = <<"valid_token_123">>,
        Timestamp = imboy_dt:utc(second),
        Path = <<"/upload/file.jpg">>,
        
        Result = auth_logic:verify_for_assets(Scene, Token, Timestamp, Path),
        ?assertEqual(<<"ok">>, Result)
    end).

verify_for_assets_with_invalid_token_test_() ->
    ?WITH_MOCK(auth_ds, [
        {get_token, 3, fun(assets, _Scene, _Timestamp) ->
            % Mock 返回与输入 token 不匹配的值
            <<"different_token_456">>
        end}
    ], fun() ->
        Scene = <<"upload">>,
        Token = <<"invalid_token_789">>,
        Timestamp = imboy_dt:utc(second),
        Path = <<"/upload/file.jpg">>,
        
        Result = auth_logic:verify_for_assets(Scene, Token, Timestamp, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_expired_timestamp_test_() ->
    ?WITH_MOCK(auth_ds, [
        {get_token, 3, fun(assets, _Scene, _Timestamp) ->
            <<"valid_token_123">>
        end}
    ], fun() ->
        Scene = <<"upload">>,
        Token = <<"valid_token_123">>,
        % 使用过期的时间戳（超过7200秒）
        ExpiredTimestamp = imboy_dt:utc(second) - 8000,
        Path = <<"/upload/file.jpg">>,
        
        Result = auth_logic:verify_for_assets(Scene, Token, ExpiredTimestamp, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_assets_with_undefined_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试 undefined 参数的情况
        ?assertEqual(<<"fail">>, auth_logic:verify_for_assets(undefined, <<"token">>, 123, <<"/path">>)),
        ?assertEqual(<<"fail">>, auth_logic:verify_for_assets(<<"scene">>, undefined, 123, <<"/path">>)),
        ?assertEqual(<<"fail">>, auth_logic:verify_for_assets(<<"scene">>, <<"token">>, error, <<"/path">>))
    end).

%% ===================================================================
%% verify_for_open/3 测试
%% ===================================================================

verify_for_open_with_valid_params_test_() ->
    ?WITH_MOCK(auth_ds, [
        {get_token, 3, fun(assets, <<"open">>, _PathWithVal) ->
            % Mock 返回与输入 token 匹配的值
            <<"valid_open_token">>
        end}
    ], fun() ->
        Path = <<"/open/file.jpg">>,
        Token = <<"valid_open_token">>,
        Val = <<"123456">>,
        
        Result = auth_logic:verify_for_open(Path, Token, Val),
        ?assertEqual(<<"ok">>, Result)
    end).

verify_for_open_with_invalid_token_test_() ->
    ?WITH_MOCK(auth_ds, [
        {get_token, 3, fun(assets, <<"open">>, _PathWithVal) ->
            % Mock 返回与输入 token 不匹配的值
            <<"different_open_token">>
        end}
    ], fun() ->
        Path = <<"/open/file.jpg">>,
        Token = <<"invalid_open_token">>,
        Val = <<"123456">>,
        
        Result = auth_logic:verify_for_open(Path, Token, Val),
        ?assertEqual(<<"fail">>, Result)
    end).

verify_for_open_with_undefined_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试 undefined 参数的情况
        ?assertEqual(<<"fail">>, auth_logic:verify_for_open(undefined, <<"token">>, <<"val">>)),
        ?assertEqual(<<"fail">>, auth_logic:verify_for_open(<<"/path">>, undefined, <<"val">>)),
        ?assertEqual(<<"fail">>, auth_logic:verify_for_open(<<"/path">>, <<"token">>, undefined))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

verify_for_assets_boundary_timestamp_test_() ->
    ?WITH_MOCK(auth_ds, [
        {get_token, 3, fun(assets, _Scene, _Timestamp) ->
            <<"boundary_token">>
        end}
    ], fun() ->
        Scene = <<"test">>,
        Token = <<"boundary_token">>,
        % 测试边界时间戳（7199秒，刚好在有效期内）
        BoundaryTimestamp = imboy_dt:utc(second) - 7199,
        Path = <<"/test/path">>,
        
        Result = auth_logic:verify_for_assets(Scene, Token, BoundaryTimestamp, Path),
        ?assertEqual(<<"ok">>, Result)
    end).

verify_for_assets_non_integer_timestamp_test_() ->
    ?WITH_MOCK(auth_ds, [
        {get_token, 3, fun(assets, _Scene, _Timestamp) ->
            <<"non_int_token">>
        end}
    ], fun() ->
        Scene = <<"test">>,
        Token = <<"non_int_token">>,
        % 测试非整数时间戳
        NonIntTimestamp = 123.456,
        Path = <<"/test/path">>,
        
        Result = auth_logic:verify_for_assets(Scene, Token, NonIntTimestamp, Path),
        ?assertEqual(<<"fail">>, Result)
    end).

%% ===================================================================
%% 集成测试
%% ===================================================================

verify_for_assets_integration_test_() ->
    ?WITH_MOCKS([
        {auth_ds, [
            {get_token, 3, fun(assets, Scene, _Timestamp) ->
                % 根据场景返回不同的 token
                case Scene of
                    <<"upload">> -> <<"upload_token_123">>;
                    <<"download">> -> <<"download_token_456">>;
                    _ -> <<"default_token">>
                end
            end}
        ]}
    ], fun() ->
        % 测试不同场景的验证
        UploadResult = auth_logic:verify_for_assets(<<"upload">>, <<"upload_token_123">>, imboy_dt:utc(second), <<"/upload/test.jpg">>),
        DownloadResult = auth_logic:verify_for_assets(<<"download">>, <<"download_token_456">>, imboy_dt:utc(second), <<"/download/test.jpg">>),
        
        ?assertEqual(<<"ok">>, UploadResult),
        ?assertEqual(<<"ok">>, DownloadResult)
    end).
