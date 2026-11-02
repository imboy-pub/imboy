-module(enterprise_channel_message_logic).

-moduledoc "企业 OA 代发企业频道消息业务逻辑（V3-23 INT-50）—— 频道会话（S3）。".
%%%
% enterprise_channel_message_logic 是 INT-50
% `POST /api/internal/v1/channels/{channel_id}/messages` 的业务逻辑层。
%
% 骨架复用 enterprise_message_logic（SPEC §5 B4）：
%   * sender 解析      = enterprise_message_logic:resolve_sender/3
%   * payload 构造     = enterprise_message_logic:build_payload/5
%     （白名单 text|file|image|link_card|business_action_card 与 S1/S2 同源）
%   * 受理收尾         = enterprise_message_logic:finish_accept/7
%     （origin 账本 kind=channel + 审计 message.enterprise.accepted
%     + webhook 事件同事务；audit/webhook resource_type=<<"msg_channel">>）
%
% 频道目标校验（fail-closed，不泄露存在性——对齐 INT-31 只读口径
% channel_repo:internal_find_tx）：跨 Org / 个人频道 / 非 scope=workspace /
% 非 status=1 / archived Workspace 一律 resource_not_found。
%
% sender 订阅要求（对齐 INT-10 sender_constraints 语义）：
%   application 模式 —— principal 主体，无订阅要求；
%   human 模式      —— 必须是目标频道 active 订阅者（status=1），
%                      否则 organization_boundary_violation。
%
% 不做离线推送：频道消息可见性是订阅者拉取面（与人工 channel 链一致），
% push_notification_logic 无频道形态，V3-23 不扩推送面（偏差已登记）。
%%%

-export([channel_tx/4]).

%%%===================================================================
%%% INT-50 channel
%%%===================================================================

%% @doc OA 代发企业频道消息。
%% Input（atom 键）与 INT-09/10 同源：
%%   sender_mode 必填 application | human；human 必填 sender_user_id；
%%   msg_type ∈ 白名单；content / object_key / 卡片字段按类型必填。
%% 返回 {ok, #{msg_id, webhook_resource_id, sender_kind, sender_user_id,
%% origin_kind, origin_application_id, accepted_at}}；
%% 失败 {error, {Code, Detail}}。
-spec channel_tx(any(), map(), integer(), map()) -> {ok, map()} | {error, {binary(), term()}}.
channel_tx(Conn, Ctx, ChannelId, Input) when is_map(Input), is_integer(ChannelId), ChannelId > 0 ->
    OrgId = maps:get(organization_id, Ctx),
    case enterprise_message_logic:resolve_sender(Conn, Ctx, Input) of
        {ok, SenderKind, SenderUid} ->
            case channel_repo:internal_find_tx(Conn, OrgId, ChannelId) of
                {ok, _Channel} ->
                    case sender_channel_authorized(Conn, SenderKind, SenderUid, ChannelId) of
                        ok ->
                            channel_payload_and_insert(
                                Conn, Ctx, Input, SenderKind, SenderUid, ChannelId
                            );
                        {error, _} = E ->
                            E
                    end;
                {error, not_found} ->
                    {error, {<<"resource_not_found">>, channel_not_found}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, _} = E ->
            E
    end;
channel_tx(_Conn, _Ctx, _ChannelId, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

channel_payload_and_insert(Conn, Ctx, Input, SenderKind, SenderUid, ChannelId) ->
    case enterprise_message_logic:build_payload(Conn, Ctx, Input, SenderKind, SenderUid) of
        {ok, MsgType, Payload} ->
            MsgId = next_msg_id(),
            case
                enterprise_channel_message_repo:insert_tx(
                    Conn,
                    ChannelId,
                    maps:get(organization_id, Ctx),
                    SenderUid,
                    MsgId,
                    MsgType,
                    Payload
                )
            of
                {ok, RowId} ->
                    enterprise_message_logic:finish_accept(
                        Conn, Ctx, SenderKind, SenderUid, <<"msg_channel">>, RowId, MsgId
                    );
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, _} = E ->
            E
    end.

%%%===================================================================
%%% sender 订阅边界
%%%===================================================================

sender_channel_authorized(_Conn, <<"application">>, _SenderUid, _ChannelId) ->
    ok;
sender_channel_authorized(Conn, <<"human">>, SenderUid, ChannelId) ->
    %% 事务连接上读（与业务写同一快照；SQL 口径同
    %% channel_subscription_repo:subscription_status/2，status=1 active）。
    Sql =
        <<
            "SELECT 1 FROM channel_subscription"
            " WHERE channel_id = $1 AND user_id = $2 AND status = 1 LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [ChannelId, SenderUid]) of
        {ok, [_ | _]} ->
            ok;
        {ok, []} ->
            {error, {<<"organization_boundary_violation">>, sender_not_channel_subscriber}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

next_msg_id() ->
    elib_uuid:gen_v7().
