-module(msg_forward_logic).
%%%
% msg_forward_logic 消息转发业务逻辑模块
%%%

-include("chat.hrl").
-include("log.hrl").
-include("error_code.hrl").

-export([forward/4]).

% 最大批量转发数量
-define(MAX_FORWARD_BATCH, 10).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 转发消息
%% @param MsgIds 要转发的消息ID列表
%% @param CurrentUid 当前用户ID
%% @param ToId 目标会话ID（单聊用户ID或群聊群ID）
%% @param ToType 目标类型（c2c/c2g）
%% @return {ok, ForwardMsgIds} | {error, Reason}
-spec forward([binary()], integer(), integer(), binary()) -> {ok, [binary()]} | {error, term()}.
forward(MsgIds, CurrentUid, ToId, ToType) when is_list(MsgIds), length(MsgIds) > 0 ->
    % 参数验证
    case validate_params(MsgIds, CurrentUid, ToId, ToType) of
        ok ->
            do_forward(MsgIds, CurrentUid, ToId, ToType);
        {error, Reason} ->
            {error, Reason}
    end;
forward(_MsgIds, _CurrentUid, _ToId, _ToType) ->
    {error, {invalid_param, <<"消息ID列表不能为空"/utf8>>}}.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 验证参数
-spec validate_params([binary()], integer(), integer(), binary()) -> ok | {error, term()}.
validate_params(MsgIds, _CurrentUid, _ToId, ToType) ->
    case length(MsgIds) > ?MAX_FORWARD_BATCH of
        true ->
            {error,
                {invalid_param, <<"单次最多转发"/utf8>>, integer_to_binary(?MAX_FORWARD_BATCH),
                    <<"条消息"/utf8>>}};
        false ->
            case ToType of
                <<"c2c">> -> ok;
                <<"c2g">> -> ok;
                _ -> {error, {invalid_param, <<"无效的目标类型"/utf8>>}}
            end
    end.

%% @doc 执行转发
-spec do_forward([binary()], integer(), integer(), binary()) -> {ok, [binary()]} | {error, term()}.
do_forward(MsgIds, CurrentUid, ToId, ToType) ->
    % 验证目标权限
    case validate_target_permission(CurrentUid, ToId, ToType) of
        ok ->
            forward_messages(MsgIds, CurrentUid, ToId, ToType, [], undefined);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 验证目标权限
-spec validate_target_permission(integer(), integer(), binary()) -> ok | {error, term()}.
validate_target_permission(CurrentUid, ToId, <<"c2c">>) ->
    % 检查是否是好友
    % 注：此处 check_relationship 返回 boolean InDenylist，不能委托
    % message_policy:send_decision（其 `InDenylist > 0` quirk 对 boolean false
    % 误判为 in_denylist，见 T1.1 语义留痕）。待 quirk 修复后评审收敛（T4.1 记录）。
    {IsFriend, InDenylist} = friend_ds:check_relationship(ToId, CurrentUid),
    case {IsFriend, InDenylist} of
        {true, false} -> ok;
        {_, true} -> {error, {in_denylist, <<"对方在黑名单中"/utf8>>}};
        {false, _} -> {error, {not_friends, <<"还不是好友"/utf8>>}}
    end;
validate_target_permission(CurrentUid, ToId, <<"c2g">>) ->
    % 检查是否是群成员
    case group_ds:is_member(CurrentUid, ToId) of
        true -> ok;
        false -> {error, {not_group_member, <<"不是群组成员"/utf8>>}}
    end.

%% @doc 转发消息列表
-spec forward_messages([binary()], integer(), integer(), binary(), [binary()], term()) ->
    {ok, [binary()]} | {error, term()}.
forward_messages([], _CurrentUid, _ToId, _ToType, [], LastError) when LastError =/= undefined ->
    {error, LastError};
forward_messages([], _CurrentUid, _ToId, _ToType, Acc, _LastError) ->
    {ok, lists:reverse(Acc)};
forward_messages([MsgId | Rest], CurrentUid, ToId, ToType, Acc, LastError) ->
    case forward_single_message(MsgId, CurrentUid, ToId, ToType) of
        {ok, ForwardMsgId} ->
            forward_messages(Rest, CurrentUid, ToId, ToType, [ForwardMsgId | Acc], LastError);
        {error, Reason} ->
            % 继续转发下一条消息，记录最后一个错误原因
            forward_messages(Rest, CurrentUid, ToId, ToType, Acc, Reason)
    end.

%% @doc 转发单条消息
-spec forward_single_message(binary(), integer(), integer(), binary()) ->
    {ok, binary()} | {error, term()}.
forward_single_message(MsgId, CurrentUid, ToId, ToType) ->
    % 获取原始消息
    case get_original_message(MsgId) of
        {ok, OriginalMsg, OriginalType} ->
            % 验证权限
            case validate_forward_permission(OriginalMsg, CurrentUid) of
                ok ->
                    % 创建转发消息
                    create_forward_message(
                        OriginalMsg, OriginalType, MsgId, CurrentUid, ToId, ToType
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取原始消息
-spec get_original_message(binary()) -> {ok, map(), binary()} | {error, term()}.
get_original_message(MsgId) ->
    % 先尝试从 C2C 消息表查找
    case msg_c2c_ds:find_msg_by_id(MsgId) of
        {ok, Msg} ->
            {ok, Msg, <<"c2c">>};
        {error, not_found} ->
            % 从 C2G 消息表查找
            case msg_c2g_ds:timeline_find_by_msg_id(MsgId) of
                {ok, [Msg]} ->
                    {ok, Msg, <<"c2g">>};
                {ok, []} ->
                    {error, {msg_not_found, <<"消息不存在"/utf8>>}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 验证转发权限
-spec validate_forward_permission(map(), integer()) -> ok | {error, term()}.
validate_forward_permission(Msg, CurrentUid) ->
    % 检查用户是否是消息的发送者或接收者
    FromId = maps:get(<<"from_id">>, Msg, undefined),
    ToId = maps:get(<<"to_id">>, Msg, undefined),
    ToGid = maps:get(<<"to_gid">>, Msg, undefined),

    case {FromId, ToId, ToGid} of
        {CurrentUid, _, _} ->
            % 用户是消息发送者
            ok;
        {_, CurrentUid, _} ->
            % 用户是消息接收者（单聊）
            ok;
        {_, _, GroupId} when is_integer(GroupId), GroupId > 0 ->
            % 群聊消息需校验当前用户是否为群成员
            case group_ds:is_member(CurrentUid, GroupId) of
                true -> ok;
                false -> {error, {permission_denied, <<"无权限转发该消息"/utf8>>}}
            end;
        _ ->
            {error, {permission_denied, <<"无权限转发该消息"/utf8>>}}
    end.

%% @doc 创建转发消息
-spec create_forward_message(map(), binary(), binary(), integer(), integer(), binary()) ->
    {ok, binary()} | {error, term()}.
create_forward_message(OriginalMsg, OriginalType, OriginalMsgId, CurrentUid, ToId, ToType) ->
    % 生成新的消息ID
    ForwardMsgId = integer_to_binary(elib_tsid:generate()),
    NowMs = elib_dt:millisecond(),

    % 构建转发消息数据（复用现有 c2c/c2g 的输入结构）
    To = ToId,
    ForwardData = #{
        <<"to">> => To,
        <<"payload">> => #{
            <<"original_msg_id">> => OriginalMsgId,
            <<"original_type">> => OriginalType
        },
        <<"msg_type">> => <<"forward">>,
        <<"action">> => <<"forward">>,
        <<"e2ee">> => <<>>,
        <<"created_at">> => NowMs
    },
    OriginalFromId = maps:get(<<"from_id">>, OriginalMsg, 0),
    OriginalToId = resolve_original_to_id(OriginalMsg),

    % 根据目标类型发送消息
    case ToType of
        <<"c2c">> ->
            ok = send_forward_message(<<"c2c">>, ForwardMsgId, CurrentUid, ForwardData),
            ok = msg_forward_ds:save_forward_record(
                OriginalMsgId,
                OriginalFromId,
                OriginalToId,
                OriginalType,
                ForwardMsgId,
                CurrentUid,
                ToId,
                ToType
            ),
            {ok, ForwardMsgId};
        <<"c2g">> ->
            ok = send_forward_message(<<"c2g">>, ForwardMsgId, CurrentUid, ForwardData),
            ok = msg_forward_ds:save_forward_record(
                OriginalMsgId,
                OriginalFromId,
                OriginalToId,
                OriginalType,
                ForwardMsgId,
                CurrentUid,
                ToId,
                ToType
            ),
            {ok, ForwardMsgId}
    end.

%% @doc 发送转发消息并归一化返回值
-spec send_forward_message(binary(), binary(), integer(), map()) -> ok | {error, term()}.
send_forward_message(<<"c2c">>, MsgId, CurrentUid, ForwardData) ->
    %% ponytail: c2c reply is a WS frame sent to the caller; treat as ok at this layer
    %% ceiling: c2c/3 returns ok | {reply, ErrFrame}; the error frame is dropped here, so a
    %%   failed forward still reports {ok, ForwardMsgId} to the HTTP caller
    %% upgrade: when forwarding must report per-target success (batch forward, REST result
    %%   list), propagate {reply, _} up instead of discarding it
    _ = msg_c2c_logic:c2c(MsgId, CurrentUid, ForwardData),
    ok;
send_forward_message(<<"c2g">>, MsgId, CurrentUid, ForwardData) ->
    %% ponytail: c2g errors are sent via self() ! {reply,...}; return value is always ok
    %% ceiling: same as the c2c clause — the caller's {ok, ForwardMsgId} says nothing about
    %%   whether the group fan-out actually happened
    %% upgrade: same trigger — per-target forward results (batch forward / REST response)
    %%   require c2g/3 to return a structured result instead of signalling out-of-band
    _ = msg_c2g_logic:c2g(MsgId, CurrentUid, ForwardData),
    ok.

%% @doc 获取原消息接收方ID
-spec resolve_original_to_id(map()) -> integer().
resolve_original_to_id(OriginalMsg) ->
    case maps:find(<<"to_id">>, OriginalMsg) of
        {ok, ToId} when is_integer(ToId) -> ToId;
        _ ->
            case maps:find(<<"to_gid">>, OriginalMsg) of
                {ok, ToGid} when is_integer(ToGid) -> ToGid;
                _ -> 0
            end
    end.
