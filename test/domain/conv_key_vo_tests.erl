%%% @doc conv_key_vo 值对象 eunit 测试（零 mock）。
%%% 验证 c2c/c2g 格式封装（min:max 排序，权威源 msg_archive_repo:conv_key/3）
%%% 与 parse 解析、type/value/equal 语义。
-module(conv_key_vo_tests).

-include_lib("eunit/include/eunit.hrl").

%% c2c 构造按 min:max 排序（与 authorize_conv 一致）。
c2c_orders_min_max_test() ->
    {ok, K} = conv_key_vo:c2c(9, 2),
    ?assertEqual(<<"c2c:2:9">>, conv_key_vo:value(K)),
    ?assertEqual(c2c, conv_key_vo:type(K)).

%% c2c 入参顺序不影响结果（交换律）。
c2c_symmetric_test() ->
    {ok, A} = conv_key_vo:c2c(2, 9),
    {ok, B} = conv_key_vo:c2c(9, 2),
    ?assert(conv_key_vo:equal(A, B)).

%% c2c 同一用户拒绝（自会话非法）。
c2c_same_uid_rejected_test() ->
    ?assertEqual({error, invalid_c2c_uids}, conv_key_vo:c2c(5, 5)).

%% c2c 非正整数拒绝。
c2c_non_positive_rejected_test() ->
    ?assertEqual({error, invalid_c2c_uids}, conv_key_vo:c2c(0, 9)).

%% c2g 构造格式正确。
c2g_format_test() ->
    {ok, K} = conv_key_vo:c2g(100),
    ?assertEqual(<<"c2g:100">>, conv_key_vo:value(K)),
    ?assertEqual(c2g, conv_key_vo:type(K)).

%% c2g 非正整数拒绝。
c2g_non_positive_rejected_test() ->
    ?assertEqual({error, invalid_gid}, conv_key_vo:c2g(0)).

%% parse 还原 c2c 键。
parse_c2c_test() ->
    {ok, K} = conv_key_vo:parse(<<"c2c:2:9">>),
    ?assertEqual(c2c, conv_key_vo:type(K)),
    ?assertEqual(<<"c2c:2:9">>, conv_key_vo:value(K)).

%% parse 还原 c2g 键。
parse_c2g_test() ->
    {ok, K} = conv_key_vo:parse(<<"c2g:100">>),
    ?assertEqual(c2g, conv_key_vo:type(K)),
    ?assertEqual(<<"c2g:100">>, conv_key_vo:value(K)).

%% parse c2c 乱序输入规范化为 min:max。
parse_c2c_normalizes_test() ->
    {ok, K} = conv_key_vo:parse(<<"c2c:9:2">>),
    ?assertEqual(<<"c2c:2:9">>, conv_key_vo:value(K)).

%% parse 非法格式拒绝。
parse_invalid_test() ->
    ?assertEqual({error, invalid_conv_key}, conv_key_vo:parse(<<"foo">>)),
    ?assertEqual({error, invalid_conv_key}, conv_key_vo:parse(<<"c2c:1">>)).
