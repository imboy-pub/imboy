%%% @doc 企业业务持久化的**冻结 SQL 与行归一化**（EB-03R F1 拆分产物）。
%%%
%%% 依据：EB-03R §2.5 F1/F4 —— `eb_pg_store.erl` 拆分必须**行为保持**：
%%%   * 语句文本逐字保持（原先的 `?SQL_*` 宏改为 `sql/1` 的同文本子句，
%%%     SQL 内容与参数占位符一个字节都没改）；
%%%   * 行归一化（二进制列名 → 原子键、timestamptz → Unix 秒、NULL → undefined）
%%%     与字段规格逐字保持；
%%%   * `statements/0` 返回与原 `eb_pg_store:sql_statements/0` **同一顺序、同一集合**，
%%%     供 `scripts/enterprise_business_db_it.sh` 的「每条语句都带双租户键」机械判据。
%%%
%%% 铁律 6：每一条语句都同时带 `organization_id` 与 `workspace_id`，且前两个业务参数
%%% 是 `$1`/`$2`（OrgId/WorkspaceId）。
-module(eb_pg_store_sql).

-moduledoc "企业业务持久化的冻结 SQL 与行归一化（EB-03R F1 拆分产物）。".
-export([
    sql/1,
    statements/0,
    normalize/2,
    to_unix/1,
    to_atom/1,
    null_to_undefined/1,
    nullify/1,
    ms_strict/1,
    ms_or_null/1,
    sender_type_bin/1,
    status_bin/1,
    normalize_error/1,
    error_constraint/1,
    identity_fields/0,
    conversation_fields/0,
    message_fields/0,
    assignment_fields/0,
    active_assignment_fields/0,
    contact_fields/0,
    contact_identity_fields/0,
    policy_fields/0,
    hold_fields/0,
    delivery_fields/0
]).

-include_lib("epgsql/include/epgsql.hrl").

%% ===================================================================
%% 冻结 SQL：原 `?SQL_*` 宏的逐字迁移（键名 = 原宏名去掉 SQL_ 前缀后小写）
%% ===================================================================

-define(MESSAGE_COLUMNS,
    "id, organization_id, workspace_id, conversation_id, sender_type, sender_contact_id,"
    " sender_business_identity_id, actor_user_id, client_msg_id, body_cipher, key_version,"
    " aad_hash, content_hash, policy_id, policy_version, retention_days, retain_until,"
    " visibility, version, created_at"
).

sql(ack_delivery) ->
    <<
        "INSERT INTO enterprise_message_delivery"
        " (id, organization_id, workspace_id, message_id, recipient_ref, device_id, status,"
        "  acked_at, version)"
        " SELECT $3, $1, $2 AS workspace_id, $4, $5, $6, 'delivered',"
        "        to_timestamp($7::bigint/1000), 1"
        "   FROM enterprise_message m"
        "  WHERE m.organization_id = $1 AND m.workspace_id = $2 AND m.id = $4"
        " ON CONFLICT (organization_id, message_id, recipient_ref, device_id) DO UPDATE"
        "    SET status = 'delivered', updated_at = now(),"
        "        acked_at = to_timestamp($7::bigint/1000),"
        "        version = enterprise_message_delivery.version + 1"
        " RETURNING id, organization_id, workspace_id, message_id, recipient_ref, device_id,"
        "           status, acked_at, version"
    >>;
sql(advance_assignment) ->
    <<
        "UPDATE organization_business_identity_assignment a"
        "   SET status = $5,"
        "       ended_at = CASE WHEN $5 = 'ended' THEN COALESCE(a.ended_at, now()) ELSE NULL END,"
        "       version = a.version + 1,"
        "       updated_at = now()"
        " WHERE a.organization_id = $1"
        "   AND a.business_identity_id = $3"
        "   AND a.status = $4"
        "   AND EXISTS (SELECT w.id AS workspace_id FROM workspace w"
        "                WHERE w.id = $2 AND w.organization_id = $1)"
    >>;
sql(append_message) ->
    <<
        "INSERT INTO enterprise_message"
        " (id, organization_id, workspace_id, conversation_id, sender_type, sender_contact_id,"
        "  sender_business_identity_id, actor_user_id, client_msg_id, body_cipher, key_version,"
        "  aad_hash, content_hash, policy_id, policy_version, retention_days, retain_until,"
        "  visibility, version)"
        " SELECT $3, $1, $2 AS workspace_id, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14,"
        "        $15, $16, to_timestamp($17::bigint/1000), 'visible', 1"
        "   FROM enterprise_conversation c"
        "  WHERE c.organization_id = $1 AND c.workspace_id = $2 AND c.id = $4"
        " ON CONFLICT (organization_id, conversation_id, client_msg_id) DO NOTHING"
        " RETURNING id"
    >>;
sql(exists_contact) ->
    <<
        "SELECT c.id AS contact_id, w.id AS workspace_id"
        "  FROM enterprise_contact c, workspace w"
        " WHERE c.organization_id = $1 AND c.id = $3"
        "   AND w.organization_id = $1 AND w.id = $2"
    >>;
sql(exists_identity) ->
    <<
        "SELECT i.id AS identity_id, w.id AS workspace_id"
        "  FROM organization_business_identity i, workspace w"
        " WHERE i.organization_id = $1 AND i.id = $3"
        "   AND w.organization_id = $1 AND w.id = $2"
    >>;
sql(fetch_contact) ->
    <<
        "SELECT c.id, c.organization_id, w.id AS workspace_id, c.imboy_user_id, c.status,"
        "       c.display_name, c.profile_cipher, c.profile_key_version,"
        "       c.created_by_business_identity_id, c.version"
        "  FROM enterprise_contact c"
        "  JOIN workspace w ON w.id = $2 AND w.organization_id = c.organization_id"
        " WHERE c.organization_id = $1 AND c.id = $3"
    >>;
sql(fetch_contact_identity) ->
    <<
        "SELECT ci.id, ci.organization_id, ci.contact_id, ci.channel, ci.subject_hmac,"
        "       ci.subject_mask, w.id AS workspace_id"
        "  FROM enterprise_contact_identity ci, workspace w"
        " WHERE ci.organization_id = $1 AND ci.id = $3"
        "   AND w.id = $2 AND w.organization_id = $1"
    >>;
sql(fetch_conversation) ->
    <<
        "SELECT id, organization_id, workspace_id, contact_id, business_identity_id, status,"
        "       version, notice_version, consent_at, consent_subject, consent_evidence_kind"
        "  FROM enterprise_conversation"
        " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    >>;
%% EB-01 / A01.36 方案 a：授权层 hint 预取专用只读。**刻意不列入 statements()
%% 冻结清单**（a01_every_statement_carries_both_tenants 双租户门不适用）：无
%% Workspace 语义——会话 id 为 TSID 主键，org + id 双键防跨租户；授权发生在
%% workspace 解析之前，此处只回经办身份、不回任何行数据，org 不匹配即 not_found。
sql(conversation_handler_identity) ->
    <<
        "SELECT business_identity_id"
        "  FROM enterprise_conversation"
        " WHERE organization_id = $1 AND id = $2"
    >>;
sql(fetch_delivery) ->
    <<
        "SELECT d.id, d.organization_id, d.workspace_id, d.message_id, d.recipient_ref,"
        "       d.device_id, d.status, d.acked_at, d.version"
        "  FROM enterprise_message_delivery d"
        "  JOIN workspace w ON w.id = $2 AND w.organization_id = d.organization_id"
        " WHERE d.organization_id = $1 AND d.message_id = $3"
        "   AND d.recipient_ref = $4 AND d.device_id IS NOT DISTINCT FROM $5"
    >>;
sql(fetch_hold) ->
    <<
        "SELECT h.id, h.organization_id, h.workspace_id, h.scope_type, h.scope_conversation_id,"
        "       h.scope_message_id, h.reason_code, h.created_at, h.released_at,"
        "       coalesce(h.scope_conversation_id, m.conversation_id) AS conversation_id,"
        "       h.scope_message_id AS message_id"
        "  FROM enterprise_retention_hold h"
        "  LEFT JOIN enterprise_message m"
        "         ON m.organization_id = h.organization_id AND m.workspace_id = h.workspace_id"
        "        AND m.id = h.scope_message_id"
        "  JOIN workspace w ON w.id = $2 AND w.organization_id = h.organization_id"
        " WHERE h.organization_id = $1 AND h.id = $3"
    >>;
sql(fetch_identity) ->
    <<
        "SELECT i.id, i.organization_id, w.id AS workspace_id, i.function_key, i.display_name,"
        "       i.status, i.version, i.created_by_user_id"
        "  FROM organization_business_identity i"
        "  JOIN workspace w ON w.id = $2 AND w.organization_id = i.organization_id"
        " WHERE i.organization_id = $1 AND i.id = $3"
    >>;
sql(fetch_message) ->
    <<
        "SELECT "
        ?MESSAGE_COLUMNS
        "  FROM enterprise_message"
        " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    >>;
sql(fetch_message_by_client) ->
    <<
        "SELECT "
        ?MESSAGE_COLUMNS
        "  FROM enterprise_message"
        " WHERE organization_id = $1 AND workspace_id = $2 AND conversation_id = $3"
        "   AND client_msg_id = $4"
    >>;
%% BE-PATCH-01（attachment-state-machine append_message）：事务内按 id 批量锁定
%% 资产行（FOR UPDATE），绑定前校验在 canonical 事务的应用侧逐行裁决。
%% 铁律 6：$1/$2 = organization_id/workspace_id。
sql(lock_assets_by_ids) ->
    <<
        "SELECT a.id, a.conversation_id, a.message_id, a.status,"
        " a.uploaded_by_user_id, a.retain_until"
        "  FROM enterprise_asset a"
        " WHERE a.organization_id = $1 AND a.workspace_id = $2"
        "   AND a.id = ANY($3::bigint[])"
        " ORDER BY a.id"
        " FOR UPDATE"
    >>;
%% BE-PATCH-01：绑定（写 message_id）。只接 active+unbound；retain_until 取
%% 「资产现有值与消息保留期的较大者」（只能后移；NULL 资产继承消息保留期——
%% 迁移 118 触发器在 message_id 非空时要求 retain 非空且 >= 消息值，此处预满足）。
sql(bind_asset_message) ->
    <<
        "UPDATE enterprise_asset a"
        "   SET message_id = $4,"
        "       retain_until = GREATEST(a.retain_until, to_timestamp($5::bigint)),"
        "       version = a.version + 1, updated_at = now()"
        " WHERE a.organization_id = $1 AND a.workspace_id = $2 AND a.id = $3"
        "   AND a.status = 'active' AND a.message_id IS NULL"
        " RETURNING a.id"
    >>;
%% BE-PATCH-01：读某消息已绑定的资产 id 集（重放奇偶校验：同一 client_msg_id +
%% asset_ids 重放返回同一 message；其他重放 409）。
sql(fetch_asset_ids_by_message) ->
    <<
        "SELECT a.id"
        "  FROM enterprise_asset a"
        " WHERE a.organization_id = $1 AND a.workspace_id = $2 AND a.message_id = $3"
        " ORDER BY a.id"
    >>;
%% CS-BE-01（历史消息资产投影）：canonical 事务内读某消息已绑定资产的**白名单
%% 投影**（POST 回显用；同事务可见本事务刚写的 message_id 绑定）。列集与
%% eb_pg_message_ext 的批量投影逐字同款——不含 object_key / URL / 上传凭证；
%% 仅 active（deleted 软删不出回显）。
sql(fetch_assets_by_message) ->
    <<
        "SELECT a.id, a.mime, a.size_bytes, a.file_name, a.status"
        "  FROM enterprise_asset a"
        " WHERE a.organization_id = $1 AND a.workspace_id = $2 AND a.message_id = $3"
        "   AND a.status = 'active'"
        " ORDER BY a.id"
    >>;
sql(fetch_policy_by_id) ->
    <<
        "SELECT id, organization_id, workspace_id, data_class, version, retention_days,"
        "       trigger_event, effective_at"
        "  FROM enterprise_retention_policy"
        " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    >>;
sql(insert_contact) ->
    <<
        "WITH scoped AS ("
        "  SELECT w.id AS workspace_id FROM workspace w"
        "   WHERE w.organization_id = $1 AND w.id = $2"
        ")"
        " INSERT INTO enterprise_contact"
        " (id, organization_id, imboy_user_id, status, display_name, profile_cipher,"
        "  profile_key_version, created_by_business_identity_id, version)"
        " SELECT $3, $1, $4, 'active', $5, $6, $7, $8, 1 FROM scoped"
        " ON CONFLICT DO NOTHING"
        " RETURNING id"
    >>;
sql(insert_contact_identity) ->
    <<
        "WITH scoped AS ("
        "  SELECT w.id AS workspace_id FROM workspace w"
        "   WHERE w.organization_id = $1 AND w.id = $2"
        ")"
        " INSERT INTO enterprise_contact_identity"
        " (id, organization_id, contact_id, channel, subject_hmac, subject_mask)"
        " SELECT $3, $1, $4, $5, $6, $7"
        "   FROM enterprise_contact c, scoped s"
        "  WHERE c.organization_id = $1 AND c.id = $4"
        " ON CONFLICT DO NOTHING"
        " RETURNING id, organization_id, contact_id, channel, subject_hmac, subject_mask"
    >>;
sql(insert_conversation) ->
    <<
        "INSERT INTO enterprise_conversation"
        " (id, organization_id, workspace_id, contact_id, business_identity_id, status, version,"
        "  notice_version, consent_at, consent_subject, consent_evidence_kind)"
        " SELECT $3, $1, $2 AS workspace_id, $4, $5, 'active', 1, $6,"
        "        CASE WHEN $7::bigint IS NULL THEN NULL ELSE to_timestamp($7::bigint/1000) END, $8, $9"
        "   FROM workspace w, enterprise_contact c, organization_business_identity i"
        "  WHERE w.id = $2 AND w.organization_id = $1"
        "    AND c.id = $4 AND c.organization_id = $1"
        "    AND i.id = $5 AND i.organization_id = $1"
        " ON CONFLICT DO NOTHING"
        " RETURNING id"
    >>;
sql(insert_hold) ->
    <<
        "WITH scoped AS ("
        "  SELECT w.id AS workspace_id FROM workspace w"
        "   WHERE w.organization_id = $1 AND w.id = $2"
        ")"
        " INSERT INTO enterprise_retention_hold"
        " (id, organization_id, workspace_id, scope_type, scope_conversation_id, scope_message_id,"
        "  reason_code, actor_user_id, version)"
        " SELECT $3, $1, s.workspace_id, $4, $5, $6, $7, $8, 1 FROM scoped s"
        " ON CONFLICT DO NOTHING"
        " RETURNING id"
    >>;
sql(insert_identity) ->
    <<
        "WITH scoped AS ("
        "  SELECT w.id AS workspace_id FROM workspace w"
        "   WHERE w.organization_id = $1 AND w.id = $2"
        ")"
        " INSERT INTO organization_business_identity"
        " (id, organization_id, function_key, display_name, status, version, created_by_user_id)"
        " SELECT $3, $1, $4, $5, 'active', 1, $6 FROM scoped"
        " ON CONFLICT DO NOTHING"
        " RETURNING id"
    >>;
sql(insert_policy) ->
    <<
        "WITH scoped AS ("
        "  SELECT w.id AS workspace_id FROM workspace w"
        "   WHERE w.organization_id = $1 AND w.id = $2"
        ")"
        " INSERT INTO enterprise_retention_policy"
        " (id, organization_id, workspace_id, data_class, version, retention_days, trigger_event,"
        "  effective_at, created_by_user_id)"
        " SELECT $3, $1, s.workspace_id, $4, $5, $6, $7, now(), $8 FROM scoped s"
        " ON CONFLICT DO NOTHING"
        " RETURNING id"
    >>;
sql(latest_policy) ->
    <<
        "SELECT id, organization_id, workspace_id, data_class, version, retention_days,"
        "       trigger_event, effective_at"
        "  FROM enterprise_retention_policy"
        " WHERE organization_id = $1 AND workspace_id = $2 AND data_class = $3"
        " ORDER BY version DESC LIMIT 1"
    >>;
sql(list_active_holds) ->
    <<
        "SELECT h.id, h.organization_id, h.workspace_id, h.scope_type, h.scope_conversation_id,"
        "       h.scope_message_id, h.reason_code, h.created_at, h.released_at,"
        "       coalesce(h.scope_conversation_id, m.conversation_id) AS conversation_id,"
        "       h.scope_message_id AS message_id"
        "  FROM enterprise_retention_hold h"
        "  LEFT JOIN enterprise_message m"
        "         ON m.organization_id = h.organization_id AND m.workspace_id = h.workspace_id"
        "        AND m.id = h.scope_message_id"
        " WHERE h.organization_id = $1 AND h.workspace_id = $2 AND h.released_at IS NULL"
        " ORDER BY h.id"
    >>;
sql(list_assignments) ->
    <<
        "SELECT s.scope_ok, s.workspace_id, a.id AS assignment_id, a.organization_id,"
        "       a.business_identity_id, a.function_key, a.user_id, a.status, a.assigned_at,"
        "       a.ended_at, a.version"
        "  FROM (SELECT (count(*) > 0) AS scope_ok, max(w.id) AS workspace_id FROM workspace w"
        "         WHERE w.id = $2 AND w.organization_id = $1) s"
        "  LEFT JOIN organization_business_identity_assignment a"
        "         ON a.organization_id = $1 AND s.scope_ok"
        " ORDER BY a.id NULLS LAST"
    >>;
sql(list_messages) ->
    <<
        "SELECT "
        ?MESSAGE_COLUMNS
        "  FROM enterprise_message"
        " WHERE organization_id = $1 AND workspace_id = $2 AND conversation_id = $3"
        " ORDER BY id"
    >>;
sql(release_hold) ->
    <<
        "UPDATE enterprise_retention_hold"
        "   SET released_at = to_timestamp($4::bigint/1000), released_by_user_id = $5"
        " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3 AND released_at IS NULL"
    >>;
sql(scope_ok) ->
    <<
        "SELECT w.id AS workspace_id"
        "  FROM workspace w"
        " WHERE w.organization_id = $1 AND w.id = $2"
    >>.

%% @doc 冻结语句清单（与原 eb_pg_store:sql_statements/0 同顺序同集合）。
-spec statements() -> [binary()].
statements() ->
    [
        sql(fetch_identity),
        sql(insert_identity),
        sql(scope_ok),
        sql(fetch_conversation),
        sql(insert_conversation),
        sql(exists_contact),
        sql(exists_identity),
        sql(append_message),
        sql(fetch_message_by_client),
        sql(fetch_message),
        sql(list_messages),
        %% BE-PATCH-01：消息-附件事务绑定（attachment-state-machine）。
        sql(lock_assets_by_ids),
        sql(bind_asset_message),
        sql(fetch_asset_ids_by_message),
        %% CS-BE-01：POST 回显的资产白名单投影（canonical 事务内）。
        sql(fetch_assets_by_message),
        sql(ack_delivery),
        sql(advance_assignment),
        sql(list_assignments),
        sql(insert_contact),
        sql(fetch_contact),
        sql(insert_contact_identity),
        sql(fetch_contact_identity),
        sql(insert_policy),
        sql(fetch_policy_by_id),
        sql(latest_policy),
        sql(insert_hold),
        sql(fetch_hold),
        sql(release_hold),
        sql(fetch_delivery),
        sql(list_active_holds)
    ].

%% ===================================================================
%% 归一化：二进制列名 → 原子键；timestamptz → Unix 秒；NULL → undefined
%% ===================================================================

normalize(Row, Fields) ->
    maps:from_list([
        {AtomKey, convert(Kind, maps:get(BinKey, Row, undefined))}
     || {AtomKey, BinKey, Kind} <- Fields
    ]).

convert(ts, Value) -> to_unix(Value);
convert(int, Value) when is_integer(Value) -> Value;
convert(int, _Other) -> undefined;
convert(bin, Value) -> Value;
convert(atom, Value) -> to_atom(Value);
convert(json, Value) -> Value;
convert(raw, Value) -> null_to_undefined(Value).

to_unix(null) ->
    undefined;
to_unix(undefined) ->
    undefined;
to_unix(Value) when is_integer(Value) ->
    Value;
to_unix(Value) when is_binary(Value) ->
    case elib_dt:rfc3339_to(Value, second) of
        Seconds when is_integer(Seconds) -> Seconds;
        _ -> undefined
    end.

to_atom(null) -> undefined;
to_atom(undefined) -> undefined;
to_atom(<<"active">>) -> active;
to_atom(<<"ended">>) -> ended;
to_atom(<<"retired">>) -> retired;
to_atom(<<"closed">>) -> closed;
to_atom(<<"contact">>) -> contact;
to_atom(<<"business_identity">>) -> business_identity;
to_atom(<<"visible">>) -> visible;
to_atom(<<"hidden">>) -> hidden;
to_atom(<<"pending">>) -> pending;
to_atom(<<"delivered">>) -> delivered;
to_atom(<<"failed">>) -> failed;
to_atom(<<"conversation">>) -> conversation;
to_atom(<<"message">>) -> message;
to_atom(<<"workspace">>) -> workspace;
%% EB-03R 追加：新能力（offboarding 状态机 / asset 生命周期 / 同意证据类别）的取值
%% 映射。**既有映射逐字未动**，只追加显式原子（未列出的仍原样返回，行为保持）。
to_atom(<<"pending_confirm">>) -> pending_confirm;
to_atom(<<"deleted">>) -> deleted;
to_atom(<<"archived">>) -> archived;
to_atom(<<"draft">>) -> draft;
to_atom(<<"frozen">>) -> frozen;
to_atom(<<"transferring">>) -> transferring;
to_atom(<<"verifying">>) -> verifying;
to_atom(<<"completed">>) -> completed;
to_atom(<<"success">>) -> success;
to_atom(<<"synthetic">>) -> synthetic;
to_atom(Other) -> Other.

null_to_undefined(null) -> undefined;
null_to_undefined(Value) -> Value.

nullify(undefined) -> null;
nullify(null) -> null;
nullify(Value) -> Value.

%% retain_until 是消息保留策略的锚点：缺失或非整数一律 fail-closed，不默认任何值。
ms_strict(Seconds) when is_integer(Seconds) ->
    {ok, Seconds * 1000};
ms_strict(Value) ->
    {error, {invalid_retain_until, Value}}.

ms_or_null(undefined) -> null;
ms_or_null(null) -> null;
ms_or_null(Seconds) when is_integer(Seconds) -> Seconds * 1000.

sender_type_bin(contact) -> <<"contact">>;
sender_type_bin(business_identity) -> <<"business_identity">>;
sender_type_bin(Value) -> Value.

status_bin(active) -> <<"active">>;
status_bin(ended) -> <<"ended">>;
status_bin({ended, reopen}) -> <<"ended">>;
status_bin(Value) when is_binary(Value) -> Value.

normalize_error(Reason) ->
    case Reason of
        #error{} = Err -> {sql, Err#error.code, error_constraint(Err#error.extra)};
        Other -> {db, Other}
    end.

error_constraint(Extra) when is_list(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        false -> undefined
    end;
error_constraint(_Other) ->
    undefined.

%% ===================================================================
%% 字段规格
%% ===================================================================

identity_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {function_key, <<"function_key">>, bin},
        {display_name, <<"display_name">>, bin},
        {status, <<"status">>, atom},
        {version, <<"version">>, int},
        {created_by_user_id, <<"created_by_user_id">>, int}
    ].

conversation_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {contact_id, <<"contact_id">>, int},
        {business_identity_id, <<"business_identity_id">>, int},
        {status, <<"status">>, atom},
        {version, <<"version">>, int},
        {notice_version, <<"notice_version">>, bin},
        {consent_at, <<"consent_at">>, ts},
        {consent_subject, <<"consent_subject">>, bin},
        {consent_evidence_kind, <<"consent_evidence_kind">>, atom}
    ].

message_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {conversation_id, <<"conversation_id">>, int},
        {sender_type, <<"sender_type">>, atom},
        {sender_contact_id, <<"sender_contact_id">>, int},
        {sender_business_identity_id, <<"sender_business_identity_id">>, int},
        {actor_user_id, <<"actor_user_id">>, int},
        {client_msg_id, <<"client_msg_id">>, bin},
        {body_cipher, <<"body_cipher">>, bin},
        {key_version, <<"key_version">>, int},
        {aad_hash, <<"aad_hash">>, bin},
        {content_hash, <<"content_hash">>, bin},
        {policy_id, <<"policy_id">>, int},
        {policy_version, <<"policy_version">>, int},
        {retention_days, <<"retention_days">>, int},
        {retain_until, <<"retain_until">>, ts},
        {visibility, <<"visibility">>, atom},
        {version, <<"version">>, int},
        {created_at, <<"created_at">>, ts}
    ].

assignment_fields() ->
    [
        {id, <<"assignment_id">>, int},
        {organization_id, <<"organization_id">>, int},
        {business_identity_id, <<"business_identity_id">>, int},
        {function_key, <<"function_key">>, bin},
        {user_id, <<"user_id">>, int},
        {status, <<"status">>, atom},
        {assigned_at, <<"assigned_at">>, ts},
        {ended_at, <<"ended_at">>, ts},
        {version, <<"version">>, int}
    ].

%% C5（closure run 冻结合同）：identity 列表投影里的 `active_assignment` 对象字段
%% 规格。键集 = `assignment_fields/0` 去掉 organization_id（租户键已在 identity 行上，
%% 不重复进对象）与 ended_at（active 行恒为 NULL，由 ck_obia_ended_at_consistency
%% 保证）——即「字段 = 既有 assignment 白名单、勿扩」。列别名加 `aa_` 前缀：与
%% identity 行同语句 JOIN 时，function_key/status/version 与 identity 自身列同名，
%% 别名既避免 SQL 列歧义，也避免归一化后的原子键互相覆盖。
active_assignment_fields() ->
    [
        {assignment_id, <<"aa_assignment_id">>, int},
        {business_identity_id, <<"aa_business_identity_id">>, int},
        {user_id, <<"aa_user_id">>, int},
        {function_key, <<"aa_function_key">>, bin},
        {status, <<"aa_status">>, atom},
        {assigned_at, <<"aa_assigned_at">>, ts},
        {version, <<"aa_version">>, int}
    ].

contact_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {imboy_user_id, <<"imboy_user_id">>, int},
        {status, <<"status">>, atom},
        {display_name, <<"display_name">>, bin},
        {profile_cipher, <<"profile_cipher">>, bin},
        {profile_key_version, <<"profile_key_version">>, int},
        {created_by_business_identity_id, <<"created_by_business_identity_id">>, int},
        {version, <<"version">>, int}
    ].

contact_identity_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {contact_id, <<"contact_id">>, int},
        {channel, <<"channel">>, bin},
        {subject_hmac, <<"subject_hmac">>, bin},
        {subject_mask, <<"subject_mask">>, bin}
    ].

policy_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {data_class, <<"data_class">>, bin},
        {version, <<"version">>, int},
        {retention_days, <<"retention_days">>, int},
        {trigger_event, <<"trigger_event">>, bin},
        {effective_at, <<"effective_at">>, ts}
    ].

%% 同时给出 DB 口径（scope_*）与 domain 口径（conversation_id / message_id）。
hold_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {scope, <<"scope_type">>, atom},
        {scope_conversation_id, <<"scope_conversation_id">>, int},
        {scope_message_id, <<"scope_message_id">>, int},
        {conversation_id, <<"conversation_id">>, int},
        {message_id, <<"message_id">>, int},
        {reason_code, <<"reason_code">>, bin},
        {created_at, <<"created_at">>, ts},
        {released_at, <<"released_at">>, ts}
    ].

delivery_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {message_id, <<"message_id">>, int},
        {recipient_ref, <<"recipient_ref">>, bin},
        {device_id, <<"device_id">>, bin},
        {status, <<"status">>, atom},
        {acked_at, <<"acked_at">>, ts},
        {version, <<"version">>, int}
    ].
