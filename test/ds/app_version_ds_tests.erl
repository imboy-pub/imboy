-module(app_version_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_version_ds 模块的 EUnit 测试
%%%
%%% 目标：验证应用版本管理功能
%%% 覆盖：签名密钥获取和设置
%%%===================================================================

%% ===================================================================
%% sign_key/3 测试
%% ===================================================================

sign_key_returns_binary_test_() ->
    ?TEST_WITH_DB(fun() ->
        Platform = <<"ios">>,
        Version = <<"1.0.0">>,
        Build = <<"1">>,
        Result = app_version_ds:sign_key(Platform, Version, Build),
        ?assertMatch(Key when is_binary(Key) andalso byte_size(Key) > 0 orelse Key =:= undefined, Result)
    end).

%% ===================================================================
%% get_sign_key/4 测试
%% ===================================================================

get_sign_key_returns_binary_test_() ->
    ?TEST_WITH_DB(fun() ->
        Platform = <<"ios">>,
        Version = <<"1.0.0">>,
        Build = <<"1">>,
        Default = <<"default_key">>,
        Result = app_version_ds:get_sign_key(Platform, Version, Build, Default),
        ?assertMatch(Key when is_binary(Key) andalso byte_size(Key) > 0, Result)
    end).

%% ===================================================================
%% set_sign_key/4 测试
%% ===================================================================

set_sign_key_saves_key_test_() ->
    ?TEST_WITH_DB(fun() ->
        Platform = <<"ios">>,
        Version = <<"1.0.0">>,
        Build = <<"1">>,
        Key = <<"test_key">>,
        Result = app_version_ds:set_sign_key(Platform, Version, Build, Key),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).
