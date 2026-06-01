%%% @doc message_id_vo 值对象 eunit 测试（零 mock）。
%%% 验证 opaque 构造校验 + value/equal 语义。
-module(message_id_vo_tests).

-include_lib("eunit/include/eunit.hrl").

%% 合法非空 binary 可构造，value/1 原样取回。
new_valid_test() ->
    {ok, Id} = message_id_vo:new(<<"01HXVABC">>),
    ?assertEqual(<<"01HXVABC">>, message_id_vo:value(Id)).

%% 空 binary 拒绝。
new_empty_rejected_test() ->
    ?assertEqual({error, invalid_message_id}, message_id_vo:new(<<>>)).

%% 非 binary 拒绝。
new_non_binary_rejected_test() ->
    ?assertEqual({error, invalid_message_id}, message_id_vo:new(12345)).

%% 相同底层值的两个 VO 相等。
equal_true_test() ->
    {ok, A} = message_id_vo:new(<<"m1">>),
    {ok, B} = message_id_vo:new(<<"m1">>),
    ?assert(message_id_vo:equal(A, B)).

%% 不同底层值的两个 VO 不相等。
equal_false_test() ->
    {ok, A} = message_id_vo:new(<<"m1">>),
    {ok, B} = message_id_vo:new(<<"m2">>),
    ?assertNot(message_id_vo:equal(A, B)).
