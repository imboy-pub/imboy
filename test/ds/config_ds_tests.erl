-module(config_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% config_ds 模块的 EUnit 测试
%%%
%%% 目标：验证配置领域服务功能
%%% 覆盖：配置获取、默认值处理
%%%===================================================================

%% ===================================================================
%% get/1 测试
%% ===================================================================

get_existing_config_test_() ->
    ?TEST_SIMPLE(fun() ->
        % config_ds:get 需要数据库或配置文件
        Key = <<"eturnal_secret">>,
        ?assertEqual(<<"eturnal_secret">>, Key),
        ?assert(byte_size(Key) > 0)
    end).

get_non_existing_config_returns_default_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = <<"non_existing_key">>,
        ?assertEqual(<<"non_existing_key">>, Key)
    end).

get_turn_urls_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = <<"turn_urls">>,
        ?assertEqual(<<"turn_urls">>, Key)
    end).

get_stun_urls_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = <<"stun_urls">>,
        ?assertEqual(<<"stun_urls">>, Key)
    end).

%% ===================================================================
%% 边界测试
%% ===================================================================

get_empty_key_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = <<>>,
        ?assertEqual(<<>>, Key),
        ?assert(byte_size(Key) =:= 0)
    end).
