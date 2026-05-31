%%% @doc conversation_agg 聚合根 eunit 测试（零 mock，纯函数）。
%%% 验证未读数（>=0）、已读游标（单调不减）、置顶（幂等 toggle）三类不变量
%%% 及其领域事件产出。SOURCE 语义：conversation_logic / conversation_pin_logic。
-module(conversation_agg_tests).

-include_lib("eunit/include/eunit.hrl").

%% 测试夹具：构造一份全新 c2c 会话视图（owner=2，对端=9）。
fresh() ->
    {ok, Key} = conv_key_vo:c2c(2, 9),
    {ok, Conv} = conversation_agg:new(Key, 2),
    Conv.

key_bin() ->
    {ok, Key} = conv_key_vo:c2c(2, 9),
    conv_key_vo:value(Key).

%% ---- 构造 / 访问器 ----

%% 全新会话：未读 0、游标 0、未置顶。
new_defaults_test() ->
    Conv = fresh(),
    ?assertEqual(0, conversation_agg:unread(Conv)),
    ?assertEqual(0, conversation_agg:read_seq(Conv)),
    ?assertNot(conversation_agg:is_pinned(Conv)),
    ?assertEqual(2, conversation_agg:owner(Conv)).

%% 非法 owner 拒绝。
new_invalid_owner_test() ->
    {ok, Key} = conv_key_vo:c2c(2, 9),
    ?assertEqual({error, invalid_owner}, conversation_agg:new(Key, 0)),
    ?assertEqual({error, invalid_owner}, conversation_agg:new(Key, -1)).

%% rehydrate 从持久化裸 map 重建，校验 conv_key。
rehydrate_test() ->
    M = #{
        <<"conv_key">> => key_bin(),
        <<"owner">> => 2,
        <<"unread">> => 3,
        <<"read_seq">> => 100,
        <<"pinned">> => true
    },
    {ok, Conv} = conversation_agg:rehydrate(M),
    ?assertEqual(3, conversation_agg:unread(Conv)),
    ?assertEqual(100, conversation_agg:read_seq(Conv)),
    ?assert(conversation_agg:is_pinned(Conv)).

%% rehydrate 非法会话键拒绝。
rehydrate_invalid_key_test() ->
    M = #{<<"conv_key">> => <<"bogus">>, <<"owner">> => 2},
    ?assertEqual({error, invalid_conv_key}, conversation_agg:rehydrate(M)).

%% ---- 未读数不变量 ----

%% 新消息（seq 超过游标）使未读 +1 并产出事件。
receive_increments_unread_test() ->
    Conv = fresh(),
    {ok, Conv2, Events} = conversation_agg:receive_message(Conv, 10),
    ?assertEqual(1, conversation_agg:unread(Conv2)),
    ?assertEqual([{unread_incremented, key_bin(), 2, 1}], Events).

%% 连续新消息累加未读。
receive_accumulates_test() ->
    Conv = fresh(),
    {ok, C1, _} = conversation_agg:receive_message(Conv, 10),
    {ok, C2, _} = conversation_agg:receive_message(C1, 11),
    ?assertEqual(2, conversation_agg:unread(C2)).

%% 已读游标之内（seq =< read_seq）的消息不增未读、无事件。
receive_below_cursor_noop_test() ->
    Conv = fresh(),
    {ok, Read, _} = conversation_agg:mark_read(Conv, 50),
    {ok, Same, Events} = conversation_agg:receive_message(Read, 50),
    ?assertEqual(0, conversation_agg:unread(Same)),
    ?assertEqual([], Events).

%% 非法 seq 拒绝。
receive_invalid_seq_test() ->
    Conv = fresh(),
    ?assertEqual({error, invalid_seq}, conversation_agg:receive_message(Conv, 0)),
    ?assertEqual({error, invalid_seq}, conversation_agg:receive_message(Conv, -1)).

%% ---- 已读游标单调不变量 ----

%% mark_read 前移游标、清零未读、产出 read 事件。
mark_read_advances_test() ->
    Conv = fresh(),
    {ok, C1, _} = conversation_agg:receive_message(Conv, 10),
    {ok, C2, Events} = conversation_agg:mark_read(C1, 10),
    ?assertEqual(10, conversation_agg:read_seq(C2)),
    ?assertEqual(0, conversation_agg:unread(C2)),
    ?assertEqual([{conversation_read, key_bin(), 2, 10}], Events).

%% 游标回退（Cursor < read_seq）拒绝——单调不变量。
mark_read_stale_rejected_test() ->
    Conv = fresh(),
    {ok, C1, _} = conversation_agg:mark_read(Conv, 100),
    ?assertEqual({error, stale_cursor}, conversation_agg:mark_read(C1, 50)).

%% 游标不变（Cursor =:= read_seq）幂等无事件。
mark_read_same_noop_test() ->
    Conv = fresh(),
    {ok, C1, _} = conversation_agg:mark_read(Conv, 100),
    {ok, C2, Events} = conversation_agg:mark_read(C1, 100),
    ?assertEqual(100, conversation_agg:read_seq(C2)),
    ?assertEqual([], Events).

%% 非法游标拒绝。
mark_read_invalid_cursor_test() ->
    Conv = fresh(),
    ?assertEqual({error, invalid_cursor}, conversation_agg:mark_read(Conv, -1)).

%% ---- 置顶不变量 ----

%% pin 置顶并产出事件。
pin_test() ->
    Conv = fresh(),
    {ok, C1, Events} = conversation_agg:pin(Conv),
    ?assert(conversation_agg:is_pinned(C1)),
    ?assertEqual([{conversation_pinned, key_bin(), 2}], Events).

%% 重复 pin 幂等、无事件（镜像 conversation_pin_logic 已置顶返回 ok 不通知）。
pin_idempotent_test() ->
    Conv = fresh(),
    {ok, C1, _} = conversation_agg:pin(Conv),
    {ok, C2, Events} = conversation_agg:pin(C1),
    ?assert(conversation_agg:is_pinned(C2)),
    ?assertEqual([], Events).

%% unpin 取消置顶并产出事件。
unpin_test() ->
    Conv = fresh(),
    {ok, C1, _} = conversation_agg:pin(Conv),
    {ok, C2, Events} = conversation_agg:unpin(C1),
    ?assertNot(conversation_agg:is_pinned(C2)),
    ?assertEqual([{conversation_unpinned, key_bin(), 2}], Events).

%% 未置顶时 unpin 幂等、无事件。
unpin_idempotent_test() ->
    Conv = fresh(),
    {ok, C1, Events} = conversation_agg:unpin(Conv),
    ?assertNot(conversation_agg:is_pinned(C1)),
    ?assertEqual([], Events).
