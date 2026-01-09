-module(imboy_hashids_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_hashids 模块的 EUnit 测试
%%%
%%% 目标：验证 HashID 编码解码功能
%%% 覆盖：ID 编码、解码
%%%===================================================================

%% ===================================================================
%% encode/1 测试
%% ===================================================================

encode_with_integer_returns_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        Id = 12345,
        Result = imboy_hashids:encode(Id),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0)
    end).

encode_with_different_ids_returns_different_results_test_() ->
    ?TEST_WITH_APP(fun() ->
        Id1 = 12345,
        Id2 = 67890,
        Result1 = imboy_hashids:encode(Id1),
        Result2 = imboy_hashids:encode(Id2),
        ?assertNotEqual(Result1, Result2)
    end).

%% ===================================================================
%% decode/1 测试
%% ===================================================================

decode_with_valid_hash_returns_integer_test_() ->
    ?TEST_WITH_APP(fun() ->
        OriginalId = 12345,
        Encoded = imboy_hashids:encode(OriginalId),
        Result = imboy_hashids:decode(Encoded),
        ?assertEqual(OriginalId, Result)
    end).

decode_encode_roundtrip_test_() ->
    ?TEST_WITH_APP(fun() ->
        OriginalId = 99999,
        Encoded = imboy_hashids:encode(OriginalId),
        Decoded = imboy_hashids:decode(Encoded),
        ?assertEqual(OriginalId, Decoded)
    end).
