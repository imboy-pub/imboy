-module(auth_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% auth_ds 模块的 EUnit 测试
%%%
%%% 目标：验证认证服务功能
%%% 覆盖：Token获取和管理
%%%===================================================================

%% ===================================================================
%% get_token/3 测试
%% ===================================================================

get_token_with_assets_resource_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<"test_upload_key">> end}
    ], fun() ->
        ?WITH_MECK(imboy_hasher, [
            {'md5', 1, fun(_Input) -> <<"1234567890abcdef1234567890abcdef">> end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = 12345,
            
            Result = auth_ds:get_token(ResourceType, Scene, ResourceId),
            % 验证返回的是16字节的二进制token
            ?assertEqual(<<"567890abcdef1234">>, Result),
            ?assertEqual(16, byte_size(Result))
        end)
    end).

get_token_with_string_resource_id_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<"test_upload_key">> end}
    ], fun() ->
        ?WITH_MECK(imboy_hasher, [
            {'md5', 1, fun(_Input) -> <<"1234567890abcdef1234567890abcdef">> end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = "/img/2023/12/test.png",
            
            Result = auth_ds:get_token(ResourceType, Scene, ResourceId),
            ?assertEqual(<<"567890abcdef1234">>, Result),
            ?assertMatch(<<_/binary>>, Result)
        end)
    end).

get_token_different_upload_keys_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(Key) -> 
            case Key of
                <<"upload_key">> -> <<"key1">>;
                _ -> <<"default_key">>
            end
        end}
    ], fun() ->
        ?WITH_MECK(imboy_hasher, [
            {'md5', 1, fun(Input) ->
                case Input of
                    <<"key1test123">> -> <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>;
                    _ -> <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>
                end
            end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = <<"test123">>,
            
            Result1 = auth_ds:get_token(ResourceType, Scene, ResourceId),
            % 验证不同的上传密钥生成不同的token
            ?assertEqual(<<"aaaaaaaaaaaaaaaa">>, Result1),
            ?assertEqual(16, byte_size(Result1))
        end)
    end).

get_token_with_empty_upload_key_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<>> end}
    ], fun() ->
        ?WITH_MECK(imboy_hasher, [
            {'md5', 1, fun(_Input) -> <<"1234567890abcdef1234567890abcdef">> end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = <<"test123">>,
            
            Result = auth_ds:get_token(ResourceType, Scene, ResourceId),
            % 即使上传密钥为空，也应该能生成token
            ?assertEqual(<<"567890abcdef1234">>, Result),
            ?assertMatch(<<_/binary>>, Result)
        end)
    end).

get_token_consistency_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<"consistent_key">> end}
    ], fun() ->
        ?WITH_MECK(imboy_hasher, [
            {'md5', 1, fun(_Input) -> <<"1234567890abcdef1234567890abcdef">> end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = <<"consistent_resource">>,
            
            % 验证相同参数生成相同的token
            Result1 = auth_ds:get_token(ResourceType, Scene, ResourceId),
            Result2 = auth_ds:get_token(ResourceType, Scene, ResourceId),
            ?assertEqual(Result1, Result2),
            ?assertEqual(<<"567890abcdef1234">>, Result1)
        end)
    end).
