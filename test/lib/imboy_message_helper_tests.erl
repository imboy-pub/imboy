-module(imboy_message_helper_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_message_helper 模块的 EUnit 测试
%%%
%%% 目标：验证消息构建与发送辅助功能
%%% 覆盖：JSON编码、消息构建、消息发送
%%%===================================================================

%% ===================================================================
%% encode_json/1 测试
%% ===================================================================

encode_json_with_simple_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{<<"hello">> => <<"world">>},
        Result = imboy_message_helper:encode_json(Map),
        ?assertEqual(<<"{\"hello\":\"world\"}">>, Result)
    end).

encode_json_with_nested_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{
            <<"user">> => #{
                <<"id">> => 123,
                <<"name">> => <<"测试"/utf8>>
            }
        },
        Result = imboy_message_helper:encode_json(Map),
        ?assert(is_binary(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

encode_json_with_list_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{<<"ids">> => [1, 2, 3]},
        Result = imboy_message_helper:encode_json(Map),
        ?assertEqual(<<"{\"ids\":[1,2,3]}">>, Result)
    end).

encode_json_with_utf8_strings_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{<<"message">> => <<"你好世界"/utf8>>},
        Result = imboy_message_helper:encode_json(Map),
        ?assert(is_binary(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

encode_json_with_empty_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{},
        Result = imboy_message_helper:encode_json(Map),
        ?assertEqual(<<"{}">>, Result)
    end).

encode_json_with_complex_nested_structure_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{
            <<"chat">> => #{
                <<"type">> => <<"group">>,
                <<"members">> => [#{<<"id">> => 1}, #{<<"id">> => 2}]
            }
        },
        Result = imboy_message_helper:encode_json(Map),
        ?assert(is_binary(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

%% ===================================================================
%% build_and_send/4 测试 - 使用默认重试配置
%% ===================================================================

build_and_send_with_retry_type_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, MsgJson, _Intervals) ->
                ?assert(is_binary(MsgJson)),
                ?assertNotEqual(<<>>, MsgJson),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"c2c">>) -> [2000, 5000, 7000] end}
        ]}
    ], fun() ->
        Msg = #{<<"type">> => <<"text">>, <<"body">> => <<"hello"/utf8>>},
        Result = imboy_message_helper:build_and_send(100, <<"msg123">>, Msg, <<"c2c">>),
        ?assertEqual(ok, Result)
    end).

build_and_send_with_different_retry_types_test_() ->
    {foreach,
     fun() ->
         meck:new([message_ds, elib_retry_config]),
         ok
     end,
     fun(_) ->
         meck:unload([message_ds, elib_retry_config])
     end,
     [
      fun(_) ->
          meck:expect(message_ds, send_next, 4, ok),
          meck:expect(elib_retry_config, intervals, fun(<<"c2c">>) -> [1, 2] end),
          Msg = #{<<"t">> => 1},
          ?assertEqual(ok, imboy_message_helper:build_and_send(1, <<"m1">>, Msg, <<"c2c">>))
      end,
      fun(_) ->
          meck:expect(message_ds, send_next, 4, ok),
          meck:expect(elib_retry_config, intervals, fun(<<"c2s">>) -> [3, 4] end),
          Msg = #{<<"t">> => 2},
          ?assertEqual(ok, imboy_message_helper:build_and_send(1, <<"m2">>, Msg, <<"c2s">>))
      end,
      fun(_) ->
          meck:expect(message_ds, send_next, 4, ok),
          meck:expect(elib_retry_config, intervals, fun(<<"s2c">>) -> [5, 6] end),
          Msg = #{<<"t">> => 3},
          ?assertEqual(ok, imboy_message_helper:build_and_send(1, <<"m3">>, Msg, <<"s2c">>))
      end
     ]}.

build_and_send_with_binary_uid_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(ToUid, _MsgId, _MsgJson, _Intervals) ->
                ?assertEqual(100, ToUid),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_) -> [1000] end}
        ]}
    ], fun() ->
        Msg = #{<<"content">> => <<"test"/utf8>>},
        Result = imboy_message_helper:build_and_send(100, <<"msg1">>, Msg, <<"c2c">>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% build_and_send/5 测试 - 支持自定义重试间隔
%% ===================================================================

build_and_send_with_custom_intervals_list_test_() ->
    ?WITH_MECK(message_ds, [
        {'send_next', 4, fun(_ToUid, _MsgId, _MsgJson, Intervals) ->
            ?assertEqual([1000, 2000, 3000], Intervals),
            ok
        end}
    ], fun() ->
        Msg = #{<<"data">> => <<"custom"/utf8>>},
        CustomIntervals = [1000, 2000, 3000],
        Result = imboy_message_helper:build_and_send(
            200,
            <<"msg456">>,
            Msg,
            CustomIntervals,
            []
        ),
        ?assertEqual(ok, Result)
    end).

build_and_send_with_empty_custom_intervals_test_() ->
    ?WITH_MECK(message_ds, [
        {'send_next', 4, fun(_ToUid, _MsgId, _MsgJson, Intervals) ->
            ?assertEqual([], Intervals),
            ok
        end}
    ], fun() ->
        Msg = #{<<"info">> => <<"empty intervals"/utf8>>},
        Result = imboy_message_helper:build_and_send(
            300,
            <<"msg789">>,
            Msg,
            [],
            []
        ),
        ?assertEqual(ok, Result)
    end).

build_and_send_with_long_custom_intervals_test_() ->
    ?WITH_MECK(message_ds, [
        {'send_next', 4, fun(_ToUid, _MsgId, _MsgJson, Intervals) ->
            ?assertEqual([100, 200, 300, 400, 500], Intervals),
            ok
        end}
    ], fun() ->
        Msg = #{<<"x">> => 1},
        LongIntervals = [100, 200, 300, 400, 500],
        Result = imboy_message_helper:build_and_send(
            400,
            <<"msg999">>,
            Msg,
            LongIntervals,
            []
        ),
        ?assertEqual(ok, Result)
    end).

build_and_send_with_binary_retry_type_and_options_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, _MsgJson, _Intervals) ->
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"group">>) -> [4000, 8000] end}
        ]}
    ], fun() ->
        Msg = #{<<"y">> => <<"test"/utf8>>},
        Result = imboy_message_helper:build_and_send(
            500,
            <<"msg111">>,
            Msg,
            <<"group">>,
            []
        ),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% encode_and_send/4 测试 - 使用默认重试配置
%% ===================================================================

encode_and_send_with_default_retry_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, MsgJson, _Intervals) ->
                ?assert(is_binary(MsgJson)),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"c2c">>) -> [2000, 5000] end}
        ]}
    ], fun() ->
        Msg = #{<<"text">> => <<"hello"/utf8>>},
        Result = imboy_message_helper:encode_and_send(600, <<"msg222">>, Msg, <<"c2c">>),
        ?assertEqual(ok, Result)
    end).

encode_and_send_validates_json_encoding_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, MsgJson, _Intervals) ->
                ?assertEqual(<<"{\"text\":\"hello\"}">>, MsgJson),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_) -> [1000] end}
        ]}
    ], fun() ->
        Msg = #{<<"text">> => <<"hello">>},
        Result = imboy_message_helper:encode_and_send(700, <<"msg333">>, Msg, <<"c2s">>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% encode_and_send/5 测试 - 支持自定义重试间隔
%% ===================================================================

encode_and_send_with_custom_intervals_test_() ->
    ?WITH_MECK(message_ds, [
        {'send_next', 4, fun(_ToUid, _MsgId, MsgJson, Intervals) ->
            ?assert(is_binary(MsgJson)),
            ?assertEqual([500, 1000], Intervals),
            ok
        end}
    ], fun() ->
        Msg = #{<<"msg">> => <<"data"/utf8>>},
        Result = imboy_message_helper:encode_and_send(
            800,
            <<"msg444">>,
            Msg,
            [500, 1000],
            []
        ),
        ?assertEqual(ok, Result)
    end).

encode_and_send_with_binary_retry_type_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, MsgJson, _Intervals) ->
                ?assert(is_binary(MsgJson)),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"s2c">>) -> [3000, 6000, 9000] end}
        ]}
    ], fun() ->
        Msg = #{<<"payload">> => <<"content"/utf8>>},
        Result = imboy_message_helper:encode_and_send(
            900,
            <<"msg555">>,
            Msg,
            <<"s2c">>,
            []
        ),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 消息内容测试
%% ===================================================================

encode_json_with_special_characters_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{<<"text">> => <<"测试\"引号\"和\\斜杠"/utf8>>},
        Result = imboy_message_helper:encode_json(Map),
        ?assert(is_binary(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

encode_json_with_unicode_emoji_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{<<"emoji">> => <<"😀😁😂🤣"/utf8>>},
        Result = imboy_message_helper:encode_json(Map),
        ?assert(is_binary(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

encode_json_with_null_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{<<"null_field">> => null},
        Result = imboy_message_helper:encode_json(Map),
        ?assertEqual(<<"{\"null_field\":null}">>, Result)
    end).

encode_json_with_boolean_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{<<"active">> => true, <<"deleted">> => false},
        Result = imboy_message_helper:encode_json(Map),
        ?assertEqual(<<"{\"active\":true,\"deleted\":false}">>, Result)
    end).

encode_json_with_number_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{
            <<"int">> => 42,
            <<"float">> => 3.14,
            <<"negative">> => -10
        },
        Result = imboy_message_helper:encode_json(Map),
        ?assert(is_binary(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

%% ===================================================================
%% 综合场景测试
%% ===================================================================

build_and_send_message_flow_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(ToUid, MsgId, MsgJson, Intervals) ->
                ?assertEqual(1000, ToUid),
                ?assertEqual(<<"msg_test_123">>, MsgId),
                ?assert(is_binary(MsgJson)),
                ?assertMatch([_, _, _], Intervals),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_) -> [2000, 5000, 7000] end}
        ]}
    ], fun() ->
        Msg = message_ds:assemble_msg(<<"C2C">>, 100, 1000, #{<<"t">> => 1}, <<"msg_test_123">>),
        Result = imboy_message_helper:build_and_send(1000, <<"msg_test_123">>, Msg, <<"c2c">>),
        ?assertEqual(ok, Result)
    end).

encode_and_send_with_complex_message_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, MsgJson, _Intervals) ->
                Decoded = jsone:decode(MsgJson),
                ?assertMatch(#{
                    <<"from">> := _,
                    <<"to">> := _,
                    <<"body">> := _
                }, Decoded),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_) -> [1000] end}
        ]}
    ], fun() ->
        Msg = #{
            <<"from">> => 100,
            <<"to">> => 200,
            <<"body">> => <<"复杂消息内容"/utf8>>,
            <<"timestamp">> => 1234567890
        },
        Result = imboy_message_helper:encode_and_send(200, <<"msg_complex">>, Msg, <<"c2c">>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

build_and_send_with_large_message_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, MsgJson, _Intervals) ->
                ?assert(byte_size(MsgJson) > 1000),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_) -> [1000] end}
        ]}
    ], fun() ->
        LargeBody = list_to_binary(lists:duplicate(500, $x)),
        Msg = #{<<"body">> => LargeBody},
        Result = imboy_message_helper:build_and_send(999, <<"msg_large">>, Msg, <<"c2c">>),
        ?assertEqual(ok, Result)
    end).

encode_and_send_with_empty_message_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, MsgJson, _Intervals) ->
                ?assertEqual(<<"{}">>, MsgJson),
                ok
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_) -> [1000] end}
        ]}
    ], fun() ->
        Msg = #{},
        Result = imboy_message_helper:encode_and_send(888, <<"msg_empty">>, Msg, <<"c2c">>),
        ?assertEqual(ok, Result)
    end).
