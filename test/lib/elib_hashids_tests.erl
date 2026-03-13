-module(elib_hashids_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_hashids 模块的 EUnit 测试
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
        Result = elib_hashids:encode(Id),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0)
    end).

encode_with_different_ids_returns_different_results_test_() ->
    ?TEST_WITH_APP(fun() ->
        Id1 = 12345,
        Id2 = 67890,
        Result1 = elib_hashids:encode(Id1),
        Result2 = elib_hashids:encode(Id2),
        ?assertNotEqual(Result1, Result2)
    end).

%% ===================================================================
%% decode/1 测试
%% ===================================================================

decode_with_valid_hash_returns_integer_test_() ->
    ?TEST_WITH_APP(fun() ->
        OriginalId = 12345,
        Encoded = elib_hashids:encode(OriginalId),
        Result = elib_hashids:decode(Encoded),
        ?assertEqual(OriginalId, Result)
    end).

decode_encode_roundtrip_test_() ->
    ?TEST_WITH_APP(fun() ->
        OriginalId = 99999,
        Encoded = elib_hashids:encode(OriginalId),
        Decoded = elib_hashids:decode(Encoded),
        ?assertEqual(OriginalId, Decoded)
    end).

%% ===================================================================
%% replace_id / replace_fields 稳健性测试
%% ===================================================================

replace_fields_keeps_already_encoded_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        OriginalId = 12345,
        EncodedId = elib_hashids:encode(OriginalId),
        Input = #{<<"id">> => EncodedId, <<"name">> => <<"tester">>},
        Output = elib_hashids:replace_fields(Input, [<<"id">>]),
        ?assertEqual(EncodedId, maps:get(<<"id">>, Output)),
        ?assertEqual(<<"tester">>, maps:get(<<"name">>, Output))
    end).

replace_fields_encodes_numeric_binary_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        Input = #{<<"id">> => <<"42">>},
        Output = elib_hashids:replace_fields(Input, [<<"id">>]),
        ?assertEqual(42, elib_hashids:decode(maps:get(<<"id">>, Output)))
    end).

replace_id_keeps_already_encoded_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        OriginalId = 67890,
        EncodedId = elib_hashids:encode(OriginalId),
        Input = #{<<"id">> => EncodedId},
        Output = elib_hashids:replace_id(Input),
        ?assertEqual(EncodedId, maps:get(<<"id">>, Output))
    end).
