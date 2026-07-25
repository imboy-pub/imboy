-module(msg_forward_logic).
%%%
% msg_forward_logic 消息转发业务逻辑模块
%%%

-include("chat.hrl").
-include("log.hrl").
-include("error_code.hrl").

-export([forward/4]).

-export_type([forward_result/0]).

%% 批量转发结果：全成功 / 部分成功（按条区分）/ 全失败
-type forward_result() ::
    {ok, [binary()]}
    | {partial, [binary()], [{binary(), term()}]}
    | {error, term()}.

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
%% @return {ok, ForwardMsgIds} 全部成功
%%       | {partial, ForwardMsgIds, [{MsgId, Reason}]} 部分成功（批量转发按条区分成败）
%%       | {error, Reason} 全部失败
-spec forward([binary()], integer(), integer(), binary()) -> forward_result().
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
-spec do_forward([binary()], integer(), integer(), binary()) -> forward_result().
do_forward(MsgIds, CurrentUid, ToId, ToType) ->
    % 验证目标权限
    case validate_target_permission(CurrentUid, ToId, ToType) of
        ok ->
            forward_messages(MsgIds, CurrentUid, ToId, ToType, [], []);
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

%% @doc 转发消息列表：逐条转发，成败分别累积，结果按条可区分
-spec forward_messages(
    [binary()], integer(), integer(), binary(), [binary()], [{binary(), term()}]
) ->
    forward_result().
forward_messages([], _CurrentUid, _ToId, _ToType, Acc, Failed) ->
    build_result(lists:reverse(Acc), lists:reverse(Failed));
forward_messages([MsgId | Rest], CurrentUid, ToId, ToType, Acc, Failed) ->
    case forward_single_message(MsgId, CurrentUid, ToId, ToType) of
        {ok, ForwardMsgId} ->
            forward_messages(Rest, CurrentUid, ToId, ToType, [ForwardMsgId | Acc], Failed);
        {error, Reason} ->
            % 继续转发下一条消息，按原消息ID记录失败原因
            forward_messages(Rest, CurrentUid, ToId, ToType, Acc, [{MsgId, Reason} | Failed])
    end.

%% @doc 汇总批量转发结果
-spec build_result([binary()], [{binary(), term()}]) -> forward_result().
build_result([], [{_MsgId, Reason} | _]) ->
    {error, Reason};
build_result(Acc, []) ->
    {ok, Acc};
build_result(Acc, Failed) ->
    {partial, Acc, Failed}.

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

    % 先发送，成功后才落转发记录：避免"库里有记录但根本没进投递管道"
    case send_forward_message(ToType, ForwardMsgId, CurrentUid, ForwardData) of
        ok ->
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
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 发送转发消息并归一化返回值
%%
%% c2c/3、c2g/3 有两种失败表达，且都不是错误返回值：
%%   1) 返回 {reply, ErrFrame}（c2c 限流/黑名单/非好友）
%%   2) 带外 `self() ! {reply, ErrFrame}`（c2g 限流/禁言/非群成员/引用消息不存在）
%% WS 路径下 self() 是 WS 进程，两种都会真的发回客户端，是既有正常机制，不能改。
%% 转发只经 REST 入口（/api/v1/msg/forward → msg_handler:forward/2），self() 是 HTTP
%% 请求进程，带外帧无人消费 → 调用方零反馈。这里把两种表达统一收敛成返回值。
%%
%% ponytail: ok 的语义是"已通过校验并进入投递管道（staging + enqueue）"，不是"已送达"
%% ceiling: 投递本身是异步的（msg_store_ds:enqueue → worker 扇出），worker 之后的失败
%%   不在本函数可见范围内；这与转发无关，是全局投递可靠性问题，不在本次修复范围
%% upgrade: 真要"已送达"语义，得等 CLIENT_ACK/msg_delivery 落地后回查，属另一条链路
-spec send_forward_message(binary(), binary(), integer(), map()) ->
    ok | {error, {forward_rejected, binary()}}.
send_forward_message(<<"c2c">>, MsgId, CurrentUid, ForwardData) ->
    normalize_send_result(MsgId, msg_c2c_logic:c2c(MsgId, CurrentUid, ForwardData));
send_forward_message(<<"c2g">>, MsgId, CurrentUid, ForwardData) ->
    normalize_send_result(MsgId, msg_c2g_logic:c2g(MsgId, CurrentUid, ForwardData)).

%% @doc 归一化：返回值里的 reply 帧 + 自身邮箱里带外投递的 reply 帧，只要有非 ACK 帧即为失败
-spec normalize_send_result(binary(), ok | {reply, map()}) ->
    ok | {error, {forward_rejected, binary()}}.
normalize_send_result(MsgId, Ret) ->
    RetFrames =
        case Ret of
            {reply, Frame} when is_map(Frame) -> [Frame];
            _ -> []
        end,
    case [F || F <- drain_reply_frames(MsgId, RetFrames), not is_ack_frame(F)] of
        [] -> ok;
        [ErrFrame | _] -> {error, {forward_rejected, frame_reason(ErrFrame)}}
    end.

%% @doc 取走本次转发在自身邮箱里留下的 {reply, Frame}
%% 按 MsgId 精确匹配，不会误吃其他消息；REST 路径下这些帧本就无人消费。
-spec drain_reply_frames(binary(), [map()]) -> [map()].
drain_reply_frames(MsgId, Acc) ->
    receive
        {reply, Frame} when is_map(Frame), map_get(<<"id">>, Frame) =:= MsgId ->
            drain_reply_frames(MsgId, [Frame | Acc])
    after 0 ->
        Acc
    end.

%% @doc SERVER_ACK 是成功帧，其余（S2C 拒绝、C2G_ERROR 等）一律视为失败
-spec is_ack_frame(map()) -> boolean().
is_ack_frame(Frame) ->
    case maps:get(<<"type">>, Frame, <<>>) of
        <<"C2C_SERVER_ACK">> -> true;
        <<"C2G_SERVER_ACK">> -> true;
        _ -> false
    end.

%% @doc 从错误帧里取可读原因
-spec frame_reason(map()) -> binary().
frame_reason(#{<<"error">> := Err}) when is_binary(Err), Err =/= <<>> ->
    Err;
frame_reason(#{<<"action">> := Action}) when is_binary(Action), Action =/= <<>> ->
    Action;
frame_reason(#{<<"type">> := Type}) when is_binary(Type), Type =/= <<>> ->
    Type;
frame_reason(_Frame) ->
    <<"send_failed">>.

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
