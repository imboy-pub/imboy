-module(msg_operation_ds).
%%%
% msg_operation_ds 是消息操作数据服务层
% 统一封装消息的删除、撤回、编辑、确认等操作
%%%

-include("log.hrl").

-export([delete_c2c_msg/2]).
-export([delete_c2g_msg/3]).
-export([delete_c2c_timeline/2]).
-export([delete_c2g_timeline/2]).
-export([ack_c2c_msg/2, ack_c2c_msg/3]).
-export([ack_c2g_timeline/2]).
-export([ack_s2c_msg/2, ack_s2c_msg/3]).
-export([ack_c2s_msg/2]).
-export([ack_c2c_batch/3]).
-export([ack_s2c_batch/3]).
-export([maybe_clean_delivered/3]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 删除单聊消息（双方删除）
%% @param MsgId 消息ID
%% @param FromId 发送者用户ID
%% @return ok | {error, Reason}
-spec delete_c2c_msg(binary(), integer()) -> ok | {error, term()}.
delete_c2c_msg(MsgId, FromId) ->
    Where = <<"WHERE msg_id = $1 AND from_id = $2">>,
    case msg_c2c_repo:delete_msg(Where, [MsgId, FromId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 删除群聊消息
%% @param Action 操作类型：C2G_DEL_FOR_ME 或 C2G_DEL_EVERYONE
%% @param Uid 用户ID
%% @param MsgId 消息ID
%% @return ok | {error, Reason}
-spec delete_c2g_msg(binary(), integer(), binary()) -> ok | {error, term()}.
delete_c2g_msg(<<"C2G_DEL_FOR_ME">>, Uid, MsgId) ->
    % 只删除用户自己的时间线记录
    case msg_c2g_timeline_repo:delete_timeline(Uid, MsgId) of
        {ok, _} -> ok;
        {error, _} -> ok
    end;
delete_c2g_msg(<<"C2G_DEL_EVERYONE">>, _Uid, MsgId) ->
    % 删除群组消息（所有人）
    Where = <<"WHERE msg_id = $1">>,
    case msg_c2g_repo:delete_msg(Where, [MsgId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end;
delete_c2g_msg(_Action, _Uid, _MsgId) ->
    {error, invalid_action}.

%% @doc 删除单聊时间线记录
%% @param Uid 用户ID
%% @param MsgId 消息ID
%% @return ok
-spec delete_c2c_timeline(integer(), binary()) -> ok.
delete_c2c_timeline(_Uid, _MsgId) ->
    % C2C 消息没有单独的时间线表，消息本身已包含双方信息
    ok.

%% @doc 删除群聊时间线记录
%% @param Uid 用户ID
%% @param MsgId 消息ID
%% @return ok
-spec delete_c2g_timeline(integer(), binary()) -> ok.
delete_c2g_timeline(Uid, MsgId) ->
    case msg_c2g_timeline_repo:delete_timeline(Uid, MsgId) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc C2C 消息 ACK 处理（legacy：无设备维度，按 uid 删行）
%% @param MsgId 消息ID
%% @param Uid 用户ID
%% @return ok
-spec ack_c2c_msg(binary(), integer()) -> ok.
ack_c2c_msg(MsgId, Uid) ->
    case msg_c2c_repo:delete_by_msg_id_and_to_id(MsgId, Uid) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc C2C 消息 ACK 处理（按设备送达，T03/P0-1）
%% DID 有效时只标记"该设备已确认"，主行等全部活跃设备确认后才删，
%% 双端登录场景下另一台离线设备重连仍能拉到该消息。
%% DID 为空（旧客户端）退回 legacy 按 uid 删行语义（返回 ok）。
%% 返回 {ok, N}：N=本次新生效的标记数（重复 ACK 为 0，供指标去虚高，MSG-P2-1）。
-spec ack_c2c_msg(binary(), integer(), binary()) -> ok | {ok, non_neg_integer()}.
ack_c2c_msg(MsgId, Uid, DID) ->
    ack_per_device(<<"c2c">>, [MsgId], Uid, DID, fun() -> ack_c2c_msg(MsgId, Uid) end).

%% @doc C2C 消息批量 ACK（REST offline_ack 按设备送达路径）
-spec ack_c2c_batch([binary()], integer(), binary()) -> ok | {ok, non_neg_integer()}.
ack_c2c_batch(MsgIds, Uid, DID) ->
    ack_per_device(<<"c2c">>, MsgIds, Uid, DID, fun() ->
        _ = msg_c2c_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid),
        ok
    end).

%% @doc C2G 消息 ACK 处理
%% 注意：C2G 不删除离线消息，只标记 timeline
%% @param MsgId 消息ID
%% @param Uid 用户ID
%% @return ok
-spec ack_c2g_timeline(binary(), integer()) -> ok.
ack_c2g_timeline(MsgId, Uid) ->
    case msg_c2g_timeline_repo:client_ack(Uid, MsgId) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc S2C 消息 ACK 处理（legacy：无设备维度，按 uid 删行）
%% @param MsgId 消息ID
%% @param Uid 用户ID
%% @return ok
-spec ack_s2c_msg(binary(), integer()) -> ok.
ack_s2c_msg(MsgId, Uid) ->
    case msg_s2c_repo:delete_by_msg_id_and_to_id(MsgId, Uid) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc S2C 消息 ACK 处理（按设备送达，语义同 ack_c2c_msg/3）
-spec ack_s2c_msg(binary(), integer(), binary()) -> ok | {ok, non_neg_integer()}.
ack_s2c_msg(MsgId, Uid, DID) ->
    ack_per_device(<<"s2c">>, [MsgId], Uid, DID, fun() -> ack_s2c_msg(MsgId, Uid) end).

%% @doc S2C 消息批量 ACK（REST offline_ack 按设备送达路径）
-spec ack_s2c_batch([binary()], integer(), binary()) -> ok | {ok, non_neg_integer()}.
ack_s2c_batch(MsgIds, Uid, DID) ->
    ack_per_device(<<"s2c">>, MsgIds, Uid, DID, fun() ->
        _ = msg_s2c_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid),
        ok
    end).

%% @doc worker 落库成功后的清理钩子（Kind ∈ c2c | s2c）
%% 处理"ACK 先于落库"竞态：接收方在线秒回 ACK 时主行尚未写入，
%% ACK 路径的清理删不到行；落库后再查一次标记，已全端确认则立即清除。
%% 任何异常只记日志，不阻塞 worker 投递流程。
-spec maybe_clean_delivered(atom(), binary(), integer()) -> ok.
maybe_clean_delivered(Kind, MsgId, Uid) when Kind =:= c2c; Kind =:= s2c ->
    KindBin = erlang:atom_to_binary(Kind),
    try
        msg_delivery_repo:delete_delivered(KindBin, MsgId, Uid, delivery_active_days())
    catch
        Class:Reason ->
            ok = ?ERROR_LOG({maybe_clean_delivered_error, Kind, MsgId, Uid, Class, Reason})
    end,
    ok;
maybe_clean_delivered(_Kind, _MsgId, _Uid) ->
    ok.

%% @doc C2S 消息 ACK 处理
%% @param MsgId 消息ID
%% @param Uid 用户ID
%% @return ok
-spec ack_c2s_msg(binary(), integer()) -> ok.
ack_c2s_msg(MsgId, Uid) ->
    Where = <<"msg_id = $1 AND from_id = $2">>,
    case msg_c2s_repo:delete_msg(Where, [MsgId, Uid]) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 按设备 ACK 公共流程：标记 → 全端确认则清主行；DID 为空走 legacy 回调
%% 返回 {ok, N}：N=本次新插入的标记数（重复 ACK 冲突跳过为 0）；legacy 路径返回回调结果。
-spec ack_per_device(binary(), [binary()], integer(), binary(), fun(() -> ok)) ->
    ok | {ok, non_neg_integer()}.
ack_per_device(_Kind, [], _Uid, _DID, _LegacyFun) ->
    {ok, 0};
ack_per_device(Kind, MsgIds, Uid, DID, _LegacyFun) when is_binary(DID), DID =/= <<>> ->
    case msg_delivery_repo:mark_acked_batch(Kind, MsgIds, Uid, DID) of
        {ok, N} ->
            ok = msg_delivery_repo:delete_delivered_batch(
                Kind, MsgIds, Uid, delivery_active_days()
            ),
            {ok, N};
        {error, Reason} ->
            %% 标记失败不删主行（宁可重复投递，不可丢消息），交给下次 ACK/重试
            ok = ?ERROR_LOG({mark_acked_failed, Kind, Uid, DID, Reason}),
            {ok, 0}
    end;
ack_per_device(_Kind, _MsgIds, _Uid, _DID, LegacyFun) ->
    LegacyFun().

%% @doc 设备活跃窗口（天）：超过窗口未活跃的设备不阻塞主行清理，
%% 即离线消息按设备保留期为该窗口（与 C2G timeline 30 天 retention 对齐）
-spec delivery_active_days() -> non_neg_integer().
delivery_active_days() ->
    application:get_env(imboy, msg_delivery_active_days, 30).
