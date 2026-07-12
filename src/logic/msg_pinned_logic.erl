%%%
%  msg_pinned 业务逻辑模块 - 消息置顶功能
%  权限与通知范式 MIRROR msg_reaction_logic（参与者/群成员校验 + msg_s2c_ds:send nosave 广播）
%%%

-module(msg_pinned_logic).

-export([pin/2, unpin/2]).

-include("log.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 消息置顶
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec pin(binary(), integer()) -> ok | {error, any()}.
pin(MsgId, CurrentUid) ->
    set_pinned(MsgId, CurrentUid, true).

%% @doc 取消消息置顶
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec unpin(binary(), integer()) -> ok | {error, any()}.
unpin(MsgId, CurrentUid) ->
    set_pinned(MsgId, CurrentUid, false).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 设置置顶状态：先按 C2C 查消息，查不到再按 C2G 查
-spec set_pinned(binary(), integer(), boolean()) -> ok | {error, any()}.
set_pinned(MsgId, CurrentUid, Pinned) ->
    case msg_c2c_ds:find_msg_by_id(MsgId) of
        {ok, #{<<"from_id">> := FromId, <<"to_id">> := ToId}} ->
            set_pinned_c2c(MsgId, CurrentUid, Pinned, FromId, ToId);
        {error, _} ->
            case msg_c2g_ds:find_msg_by_id(MsgId) of
                {ok, #{<<"to_id">> := Gid}} ->
                    set_pinned_c2g(MsgId, CurrentUid, Pinned, Gid);
                {error, _} ->
                    {error, not_found}
            end
    end.

%% @doc C2C 置顶：调用者必须是消息的发送者或接收者
-spec set_pinned_c2c(binary(), integer(), boolean(), integer(), integer()) ->
    ok | {error, any()}.
set_pinned_c2c(MsgId, CurrentUid, Pinned, FromId, ToId) when
    CurrentUid =:= FromId; CurrentUid =:= ToId
->
    %% msg_c2c 行按接收方存储（update_pinned WHERE to_id），
    %% 必须传消息真实 to_id 而非 CurrentUid：发送者置顶时 CurrentUid 匹配 0 行（旧代码静默无效）
    case msg_c2c_ds:update_pinned(MsgId, ToId, Pinned) of
        {ok, _} ->
            Peer =
                case CurrentUid of
                    FromId -> ToId;
                    _ -> FromId
                end,
            notify_pinned(CurrentUid, [Peer], MsgId, Pinned),
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
set_pinned_c2c(_MsgId, _CurrentUid, _Pinned, _FromId, _ToId) ->
    {error, permission_denied}.

%% @doc C2G 置顶：调用者必须是群成员。
%% ponytail: 任何群成员均可置顶/取消置顶；管理员限制留给产品层
%% （如需角色校验，参照 channel_logic_message:pin_message/3 的 Role < 2 拒绝范式）
-spec set_pinned_c2g(binary(), integer(), boolean(), integer()) -> ok | {error, any()}.
set_pinned_c2g(MsgId, CurrentUid, Pinned, Gid) ->
    case group_member_ds:is_member(Gid, CurrentUid) of
        true ->
            %% msg_c2g 行的 to_id 是群ID（写入时 to_id => Gid），
            %% 必须传 Gid 而非 CurrentUid：旧代码传 CurrentUid 恒匹配 0 行（静默无效）
            case msg_c2g_ds:update_pinned(MsgId, Gid, Pinned) of
                {ok, _} ->
                    Members = [U || U <- group_ds:member_uids(Gid), U =/= CurrentUid],
                    notify_pinned(CurrentUid, Members, MsgId, Pinned),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, permission_denied}
    end.

%% @doc 广播 pin 变更通知（对齐 msg_reaction_logic 的 nosave 语义：
%% pinned 状态已持久化在 pinned 列，离线端重连拉取即可，无需落离线消息）
-spec notify_pinned(integer(), list(), binary(), boolean()) -> ok.
notify_pinned(_CurrentUid, [], _MsgId, _Pinned) ->
    ok;
notify_pinned(CurrentUid, ToUids, MsgId, Pinned) ->
    Payload = #{
        <<"msg_id">> => MsgId,
        <<"pinned">> => Pinned,
        <<"operator">> => CurrentUid
    },
    msg_s2c_ds:send(CurrentUid, ToUids, <<"message_pinned">>, MsgId, null, Payload, nosave),
    ok.
