-module(enterprise_channel_message_repo).

-moduledoc "企业频道消息仓储（V3-23 INT-50）—— 企业频道代发/指定员工发送的真源。".
%%%
% enterprise_channel_message_repo 是企业频道消息（migration 00000241 新表
% enterprise_channel_message）的仓储层。
%
% 行语义与 msg_c2c/msg_c2g 同族：
%   * msg_id 为业务消息 ID（uuidv7 binary），UNIQUE；
%   * e2ee 恒 NULL（企业托管非 E2EE，DB CHECK ck_ecm_non_e2ee 强制）；
%   * scope 边界（org/workspace/channel status=1）由 logic 层经
%     channel_repo:internal_find_tx fail-closed 校验后写入，本表不重复
%     组织树 JOIN（写入前已定界）；
%   * 审计/origin/webhook 事件沿用 enterprise_message_logic:finish_accept
%     同事务收尾（msg_channel resource type）。
%%%

-export([insert_tx/7, find_tx/2, count_for_channel_tx/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 事务内插入频道消息行。FromId 为 human 解析 uid 或 application
%% principal uid（与 msg_c2c/msg_c2g 的 from_id 语义一致）。
-spec insert_tx(any(), pos_integer(), pos_integer(), pos_integer(), binary(), binary(), map()) ->
    {ok, pos_integer()} | {error, term()}.
insert_tx(Conn, ChannelId, OrgId, FromId, MsgId, MsgType, Payload) ->
    RowId = elib_tsid:generate(enterprise_channel_message),
    Sql =
        <<
            %% accepted 时刻与 server_ts 同事务 PG 时钟（NOW()，R03/D04 口径）。
            "INSERT INTO enterprise_channel_message"
            " (id, channel_id, organization_id, from_id, msg_id, msg_type, payload,"
            "  e2ee, server_ts, created_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7::jsonb,NULL,NOW(),NOW())"
        >>,
    case
        elib_pg:query(Conn, Sql, [
            RowId, ChannelId, OrgId, FromId, MsgId, MsgType, jsone:encode(Payload)
        ])
    of
        {ok, _} -> {ok, RowId};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按业务消息 ID 读回行（幂等登记 fetch_row_id_tx / 断言用）。
-spec find_tx(any(), binary()) -> {ok, map()} | {error, not_found}.
find_tx(Conn, MsgId) when is_binary(MsgId) ->
    Sql =
        <<
            "SELECT id, channel_id, organization_id, from_id, msg_id, msg_type,"
            " payload::text AS payload, e2ee, server_ts, created_at"
            " FROM enterprise_channel_message WHERE msg_id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [MsgId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} -> {error, not_found}
    end;
find_tx(_Conn, _MsgId) ->
    {error, not_found}.

%% @doc 频道消息计数（断言/诊断用）。
-spec count_for_channel_tx(any(), pos_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
count_for_channel_tx(Conn, ChannelId) when is_integer(ChannelId) ->
    Sql = <<"SELECT COUNT(*) AS n FROM enterprise_channel_message WHERE channel_id = $1">>,
    case elib_pg:query(Conn, Sql, [ChannelId]) of
        {ok, [#{<<"n">> := N} | _]} -> {ok, N};
        {ok, []} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.
