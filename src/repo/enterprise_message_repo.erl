-module(enterprise_message_repo).

%%%
% EPGZ-04 INT-09/10 OA 代发消息仓储（企业托管非 E2EE 真源 + 审计）。
%
% 零新表复用既有存储（本卡禁建 migration）：
%   * msg_c2c / msg_c2g（迁移 5/6）——服务端明文模式（bot 域同款先例：
%     bot_logic:send_message 直写、e2ee 恒 NULL）。OA 代发不触个人
%     E2EE 路径：不走 msg_c2c_logic:c2c human 链、不写 msg_store/
%     msg_store_staging（E2EE 离线真源），payload 顶层无 e2ee 键。
%   * enterprise_audit_event（迁移 119，append-only）——OA 代发事实审计：
%     actor_role='enterprise_application'（审计 actor 是 Application，
%     不伪造成 Human），detail jsonb 持久化 sender_kind / sender_user_id /
%     origin_kind / origin_application_id（列级可查询断言）。
%
% payload 列 = 消息 payload 子对象 JSON（同 human 链存储形态：
% msg_c2c_logic 经 message_policy:encode_payload 落同一形态），origin
% 元数据放 payload 顶层 <<"origin">> 键——A6 Flutter 按 payload.origin
% 渲染「OA 代发 + 非 E2EE」标记，未知字段对旧客户端无害。
%%%

-export([
    insert_direct_tx/6,
    insert_group_tx/6,
    insert_audit_tx/4,
    find_direct_tx/2,
    find_group_tx/2,
    find_audit_tx/3
]).

-include_lib("epgsql/include/epgsql.hrl").

%%%===================================================================
%%% msg_c2c / msg_c2g 服务端明文行
%%%===================================================================

%% @doc OA 代发 direct 消息（同 tx）：
%%   FromId —— human 模式=解析后内部 uid；application 模式=principal user
%%   ToId   —— 收件人解析后内部 uid
%%   e2ee 恒 NULL（企业托管非 E2EE；不写 sender_did——服务端主体无设备）。
-spec insert_direct_tx(
    any(), binary(), integer(), integer(), binary(), map()
) ->
    {ok, pos_integer()} | {error, term()}.
insert_direct_tx(Conn, MsgId, FromId, ToId, MsgType, Payload) ->
    RowId = next_id(msg_c2c),
    Sql =
        <<
            "INSERT INTO msg_c2c (id, from_id, to_id, msg_id, msg_type, e2ee, payload,"
            " server_ts, created_at)"
            " VALUES ($1,$2,$3,$4,$5,NULL,$6::jsonb,NOW(),NOW())"
        >>,
    case elib_pg:query(Conn, Sql, [RowId, FromId, ToId, MsgId, MsgType, jsone:encode(Payload)]) of
        {ok, _} -> {ok, RowId};
        {error, Reason} -> {error, Reason}
    end.

%% @doc OA 代发 group 消息（同 tx）：ToId=群 id（msg_c2g 语义：to_id=群）。
-spec insert_group_tx(
    any(), binary(), integer(), integer(), binary(), map()
) ->
    {ok, pos_integer()} | {error, term()}.
insert_group_tx(Conn, MsgId, FromId, GroupId, MsgType, Payload) ->
    RowId = next_id(msg_c2g),
    Sql =
        <<
            "INSERT INTO msg_c2g (id, topic_id, from_id, to_id, msg_id, msg_type, e2ee,"
            " payload, mentions, server_ts, created_at)"
            " VALUES ($1,0,$2,$3,$4,$5,NULL,$6::jsonb,'[]'::jsonb,NOW(),NOW())"
        >>,
    case
        elib_pg:query(Conn, Sql, [RowId, FromId, GroupId, MsgId, MsgType, jsone:encode(Payload)])
    of
        {ok, _} -> {ok, RowId};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% enterprise_audit_event（append-only 审计真源）
%%%===================================================================

%% @doc OA 代发审计行。Detail 含 origin_kind/origin_application_id/sender_kind/
%% sender_user_id/msg_id（origin_application_id 列级断言点）。
%% actor_role='enterprise_application'：审计 actor 是 Application（principal
%% 可空，仅审计锚点）。resource_type ∈ msg_c2c|msg_c2g。
-spec insert_audit_tx(any(), integer(), map(), map()) ->
    {ok, pos_integer()} | {error, term()}.
insert_audit_tx(Conn, OrgId, Keys, Detail) ->
    EventId = next_id(enterprise_audit_event),
    Sql =
        <<
            "INSERT INTO enterprise_audit_event (id, organization_id, resource_type,"
            " resource_id, action, business_identity_id, actor_user_id, actor_role,"
            " detail, created_at)"
            " VALUES ($1,$2,$3,$4,$5,NULL,$6,'enterprise_application',$7::jsonb,NOW())"
        >>,
    ResourceType = maps:get(resource_type, Keys, <<"msg_c2c">>),
    ResourceId = maps:get(resource_id, Keys, null),
    Action = maps:get(action, Keys, <<"message.enterprise.accepted">>),
    ActorUserId = maps:get(actor_user_id, Keys, null),
    case
        elib_pg:query(Conn, Sql, [
            EventId,
            OrgId,
            ResourceType,
            ResourceId,
            Action,
            ActorUserId,
            jsone:encode(Detail)
        ])
    of
        {ok, _} -> {ok, EventId};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% 查询（消费方断言 / A0 W4 展示用）
%%%===================================================================

-spec find_direct_tx(any(), binary()) -> {ok, map()} | {error, not_found}.
find_direct_tx(Conn, MsgId) ->
    Sql =
        <<
            "SELECT id, from_id, to_id, msg_id, msg_type, e2ee, payload::text AS payload,"
            " server_ts, created_at FROM msg_c2c WHERE msg_id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [MsgId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} -> {error, not_found}
    end.

-spec find_group_tx(any(), binary()) -> {ok, map()} | {error, not_found}.
find_group_tx(Conn, MsgId) ->
    Sql =
        <<
            "SELECT id, topic_id, from_id, to_id, msg_id, msg_type, e2ee, payload::text AS payload,"
            " server_ts, created_at FROM msg_c2g WHERE msg_id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [MsgId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} -> {error, not_found}
    end.

%% @doc 按 (org, resource_type, resource_id) 取审计行——origin_application_id
%% 持久化断言的列级查询入口。
-spec find_audit_tx(any(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
find_audit_tx(Conn, OrgId, ResourceId) ->
    Sql =
        <<
            "SELECT id, resource_type, resource_id, action, actor_user_id, actor_role,"
            " detail::text AS detail, created_at FROM enterprise_audit_event"
            " WHERE organization_id = $1 AND resource_id = $2 ORDER BY created_at"
        >>,
    case elib_pg:query(Conn, Sql, [OrgId, ResourceId]) of
        {ok, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, _} -> {ok, []};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

next_id(Name) ->
    case lists:member(Name, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(Name)
    end,
    elib_tsid:generate(Name).
