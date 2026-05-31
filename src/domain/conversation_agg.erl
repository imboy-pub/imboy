%%% @doc 会话聚合根 / Conversation Aggregate Root
%%%
%%% DDD 充血改造 Phase 1 / T1.3：把「未读数 / 已读游标 / 置顶」三类
%%% 业务不变量内聚到聚合根内部，由纯函数守护，杜绝散落在
%%% conversation_logic / conversation_pin_logic 的 guard 与外部直接赋值。
%%%
%%% Functional-core aggregate guarding conversation invariants:
%%%   - unread count stays >= 0;
%%%   - read cursor (conv_seq) is monotonically non-decreasing;
%%%   - pin/unpin are idempotent toggles.
%%% Operations return {ok, NewState, [event()]} | {error, atom()}; the
%%% imperative shell (logic) persists state and publishes events after commit.
%%% Zero side effects -> eunit without mocks.
%%%
%%% 渐进策略（NOT Building）：本 task 仅新增纯领域层，不改既有 logic；
%%% DS load/save_aggregate 接线与 logic 退化外壳留后续 task。
-module(conversation_agg).

-export([new/2, rehydrate/1]).
-export([receive_message/2, mark_read/2, pin/1, unpin/1]).
-export([owner/1, unread/1, read_seq/1, is_pinned/1, key/1]).

-export_type([t/0, event/0]).

%% 会话视图状态（每用户一份）：
%%   key      会话键值对象（c2c/c2g，conv_key_vo 守护格式）
%%   owner    归属用户 uid（正整数）
%%   unread   未读数，不变量 >= 0
%%   read_seq 已读游标（conv_seq），单调不减
%%   pinned   是否置顶
-record(conversation, {
    key :: conv_key_vo:t(),
    owner :: pos_integer(),
    unread = 0 :: non_neg_integer(),
    read_seq = 0 :: non_neg_integer(),
    pinned = false :: boolean()
}).
-opaque t() :: #conversation{}.

%% 领域事件：会话键以规范化 binary 透传（可序列化、跨进程派发）。
-type event() ::
    {unread_incremented, ConvKey :: binary(), Owner :: pos_integer(), Unread :: pos_integer()}
    | {conversation_read, ConvKey :: binary(), Owner :: pos_integer(), Cursor :: non_neg_integer()}
    | {conversation_pinned, ConvKey :: binary(), Owner :: pos_integer()}
    | {conversation_unpinned, ConvKey :: binary(), Owner :: pos_integer()}.

%% ===================================================================
%% 构造 / 重建
%% ===================================================================

%% @doc 构造一份全新会话视图（未读 0、游标 0、未置顶）。
-spec new(conv_key_vo:t(), Owner :: integer()) ->
    {ok, t()} | {error, invalid_owner}.
new(Key, Owner) when is_integer(Owner), Owner > 0 ->
    {ok, #conversation{key = Key, owner = Owner}};
new(_, _) ->
    {error, invalid_owner}.

%% @doc 从持久化裸 map 重建聚合（DS 层调用），校验会话键与归属。
-spec rehydrate(map()) -> {ok, t()} | {error, invalid_conv_key | invalid_owner}.
rehydrate(M) when is_map(M) ->
    case conv_key_vo:parse(maps:get(<<"conv_key">>, M, <<>>)) of
        {ok, Key} ->
            Owner = maps:get(<<"owner">>, M, 0),
            case is_integer(Owner) andalso Owner > 0 of
                true ->
                    {ok, #conversation{
                        key = Key,
                        owner = Owner,
                        unread = clamp_unread(maps:get(<<"unread">>, M, 0)),
                        read_seq = clamp_seq(maps:get(<<"read_seq">>, M, 0)),
                        pinned = maps:get(<<"pinned">>, M, false) =:= true
                    }};
                false ->
                    {error, invalid_owner}
            end;
        {error, _} ->
            {error, invalid_conv_key}
    end.

%% ===================================================================
%% 未读数不变量
%% ===================================================================

%% @doc 收到一条新消息：若其 seq 超过已读游标则未读 +1 并产出事件，
%% 否则（已被游标覆盖）无变化无事件。幂等去重由外壳负责。
-spec receive_message(t(), Seq :: integer()) ->
    {ok, t(), [event()]} | {error, invalid_seq}.
receive_message(_, Seq) when not is_integer(Seq); Seq =< 0 ->
    {error, invalid_seq};
receive_message(C = #conversation{read_seq = R}, Seq) when Seq =< R ->
    {ok, C, []};
receive_message(C = #conversation{unread = U, owner = O, key = K}, _Seq) ->
    Unread = U + 1,
    {ok, C#conversation{unread = Unread}, [{unread_incremented, conv_key_vo:value(K), O, Unread}]}.

%% ===================================================================
%% 已读游标单调不变量
%% ===================================================================

%% @doc 标记已读至 Cursor：游标只能前移（单调不减）。
%%   Cursor <  read_seq -> {error, stale_cursor}（拒绝回退）
%%   Cursor =:= read_seq -> 幂等无事件
%%   Cursor >  read_seq -> 前移游标、清零未读、产出 read 事件
-spec mark_read(t(), Cursor :: integer()) ->
    {ok, t(), [event()]} | {error, invalid_cursor | stale_cursor}.
mark_read(_, Cursor) when not is_integer(Cursor); Cursor < 0 ->
    {error, invalid_cursor};
mark_read(#conversation{read_seq = R}, Cursor) when Cursor < R ->
    {error, stale_cursor};
mark_read(C = #conversation{read_seq = R}, Cursor) when Cursor =:= R ->
    {ok, C, []};
mark_read(C = #conversation{owner = O, key = K}, Cursor) ->
    {ok, C#conversation{read_seq = Cursor, unread = 0}, [
        {conversation_read, conv_key_vo:value(K), O, Cursor}
    ]}.

%% ===================================================================
%% 置顶不变量（幂等 toggle）
%% ===================================================================

%% @doc 置顶会话。已置顶则幂等无事件（镜像 conversation_pin_logic 现状语义）。
-spec pin(t()) -> {ok, t(), [event()]}.
pin(C = #conversation{pinned = true}) ->
    {ok, C, []};
pin(C = #conversation{owner = O, key = K}) ->
    {ok, C#conversation{pinned = true}, [{conversation_pinned, conv_key_vo:value(K), O}]}.

%% @doc 取消置顶。未置顶则幂等无事件。
-spec unpin(t()) -> {ok, t(), [event()]}.
unpin(C = #conversation{pinned = false}) ->
    {ok, C, []};
unpin(C = #conversation{owner = O, key = K}) ->
    {ok, C#conversation{pinned = false}, [{conversation_unpinned, conv_key_vo:value(K), O}]}.

%% ===================================================================
%% 访问器
%% ===================================================================

-spec owner(t()) -> pos_integer().
owner(#conversation{owner = O}) -> O.

-spec unread(t()) -> non_neg_integer().
unread(#conversation{unread = U}) -> U.

-spec read_seq(t()) -> non_neg_integer().
read_seq(#conversation{read_seq = R}) -> R.

-spec is_pinned(t()) -> boolean().
is_pinned(#conversation{pinned = P}) -> P.

-spec key(t()) -> conv_key_vo:t().
key(#conversation{key = K}) -> K.

%% ===================================================================
%% 内部：持久化数值规整（防御脏数据，维持不变量）
%% ===================================================================

-spec clamp_unread(term()) -> non_neg_integer().
clamp_unread(N) when is_integer(N), N >= 0 -> N;
clamp_unread(_) -> 0.

-spec clamp_seq(term()) -> non_neg_integer().
clamp_seq(N) when is_integer(N), N >= 0 -> N;
clamp_seq(_) -> 0.
