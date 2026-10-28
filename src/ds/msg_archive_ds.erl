-module(msg_archive_ds).
-moduledoc "永久消息存储数据服务层 / permanent message archive data service。".
%%%-------------------------------------------------------------------
%%% @doc  永久消息存储数据服务层
%%%
%%% 封装消息历史查询接口，供 Handler 层调用。
%%% 写入由 msg_store_worker 通过 msg_archive_repo 负责，此模块只负责读取。
%%%
%%% == 调用示例 ==
%%% ```erlang
%%% % C2C 历史消息（增量拉取，首次传 0）
%%% ConvKey = msg_archive_repo:conv_key(<<"c2c">>, FromId, ToId),
%%% {ok, Rows} = msg_archive_ds:history(ConvKey, 0, 50).
%%%
%%% % C2G 历史消息
%%% ConvKey = msg_archive_repo:conv_key(<<"c2g">>, FromId, Gid),
%%% {ok, Rows} = msg_archive_ds:history(ConvKey, LastSeq, 50).
%%% ```
%%% @end
%%%-------------------------------------------------------------------

-export([history/3, history/4]).
-export([history_batch/2]).
-export([message_result/4]).
-export([history_context/7]).
-export([conv_key_c2c/2, conv_key_c2g/1]).
%% R02/D05：安全推进边界、一致快照历史与有界补扫
-export([safe_head/4, safe_heads/2, safe_history/6, backfill/6]).

message_result(ConvKey, MsgId, Sender, MinSeq) ->
    msg_archive_repo:message_result(ConvKey, MsgId, Sender, MinSeq).

-spec history_context(binary(), binary(), pos_integer(), binary(), integer(), integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
history_context(ConvKey, Target, Limit, ChatType, Uid, PeerA, MinSeq) ->
    msg_archive_repo:history_context(ConvKey, Target, Limit, ChatType, Uid, PeerA, MinSeq).

%%-------------------------------------------------------------------
%% @doc  游标拉取历史消息（正序，增量同步）
%%
%% @param ConvKey  会话键（使用 conv_key_c2c/2 或 conv_key_c2g/1 生成）
%% @param AfterSeq 上次最后消息的 conv_seq（首次传 0）
%% @param Limit    每次最多返回条数（建议 50~100）
%% @return {ok, [#{msg_id, chat_type, conv_seq, msg_type, from_id, to_id,
%%                 group_id, e2ee, payload, created_at, server_ts}]} | {error, Reason}
%% @end
%%-------------------------------------------------------------------
-spec history(binary(), integer(), pos_integer()) ->
    {ok, list(map())} | {error, term()}.
history(ConvKey, AfterSeq, Limit) ->
    msg_archive_repo:get_history(ConvKey, AfterSeq, Limit, asc).

%%-------------------------------------------------------------------
%% @doc  批量游标拉取：一次查询多个会话的增量消息(正序)
%%
%% 每项 {ConvKey, AfterSeq} 各按自身游标取最多 Limit 条。返回的行含
%% conv_key 字段，供上层按会话分组。用于多会话同步消除逐会话 N+1。
%%
%% @param Cursors [{ConvKey :: binary(), AfterSeq :: integer()}]
%% @end
%%-------------------------------------------------------------------
-spec history_batch([{binary(), integer()}], pos_integer()) ->
    {ok, list(map())} | {error, term()}.
history_batch(Cursors, Limit) ->
    msg_archive_repo:get_history_batch(Cursors, Limit).

%%-------------------------------------------------------------------
%% @doc  游标拉取历史消息（带排序方向）
%%
%% @param Order asc（向下加载更多）| desc（向上翻页）
%% @end
%%-------------------------------------------------------------------
-spec history(binary(), integer(), pos_integer(), asc | desc) ->
    {ok, list(map())} | {error, term()}.
history(ConvKey, AfterSeq, Limit, Order) ->
    msg_archive_repo:get_history(ConvKey, AfterSeq, Limit, Order).

%%-------------------------------------------------------------------
%% @doc  计算 C2C 会话键
%%
%% @param UidA 任意顺序的两个 user_id（函数内部自动排序）
%% @param UidB
%% @end
%%-------------------------------------------------------------------
-spec conv_key_c2c(integer(), integer()) -> binary().
conv_key_c2c(UidA, UidB) ->
    msg_archive_repo:conv_key(<<"c2c">>, UidA, UidB).

%%-------------------------------------------------------------------
%% @doc  计算 C2G 会话键
%%
%% @param Gid 群组 ID（integer）
%% @end
%%-------------------------------------------------------------------
-spec conv_key_c2g(integer()) -> binary().
conv_key_c2g(Gid) ->
    msg_archive_repo:conv_key(<<"c2g">>, 0, Gid).

%%-------------------------------------------------------------------
%% @doc R02/D05：会话安全推进边界 B（见 msg_archive_repo:safe_head/4）。
%% PeerA/PeerB：C2C 传两端 uid（内部排序）；C2G 传 (Gid, 0)。
%%-------------------------------------------------------------------
-spec safe_head(binary(), binary(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
safe_head(ConvKey, ChatType, PeerA, PeerB) ->
    msg_archive_repo:safe_head(ConvKey, ChatType, PeerA, PeerB).

%% @doc P2（审查发现修复轮）：多会话批量取 B 边界——单事务单 SQL，
%% 消除 offline v2 cursor 逐群 with_tx 串行放大（见 msg_archive_repo:safe_heads/2）。
-spec safe_heads(binary(), [{binary(), integer(), integer()}]) ->
    {ok, #{binary() => non_neg_integer()}} | {error, term()}.
safe_heads(ChatType, Items) ->
    msg_archive_repo:safe_heads(ChatType, Items).

%% @doc R02/D05：一致快照下的增量历史（行 + B 边界 + 终态空洞）。
-spec safe_history(binary(), integer(), pos_integer(), binary(), integer(), integer()) ->
    {ok, list(map()), non_neg_integer(), [{integer(), binary()}]} | {error, term()}.
safe_history(ConvKey, AfterSeq, Limit, ChatType, PeerA, PeerB) ->
    msg_archive_repo:safe_history(ConvKey, AfterSeq, Limit, ChatType, PeerA, PeerB).

%% @doc R02/D05：晚到补齐的有界范围补扫（见 msg_archive_repo:backfill/6）。
-spec backfill(binary(), integer(), pos_integer(), binary(), integer(), integer()) ->
    {ok, list(map()), non_neg_integer(), boolean()} | {error, term()}.
backfill(ConvKey, AfterSeq, Limit, ChatType, PeerA, PeerB) ->
    msg_archive_repo:backfill(ConvKey, AfterSeq, Limit, ChatType, PeerA, PeerB).
