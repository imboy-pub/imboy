%%% @doc EB-03R P9：企业消息的**只读历史（键集分页）**。
%%%
%%% 依据：EB-06「只读历史」+ R0 分类二——`list_messages` 的 `after_id` 语义**全无**。
%%%
%%% ## 为什么必须是键集（keyset）而不是 offset
%%%
%%% 企业消息是 append-only 且会被 bounded purge 物理删除。`OFFSET n` 分页在
%%% 「读取期间有行被删除或追加」时会**跳行/重复行**：第 2 页会漏掉正好被删掉的那些
%%% 行之后的记录。键集分页把游标固定在**已读到的 id** 上（严格 `id > after_id`），
%%% 删除/追加都不会让后续页漂移。
%%%
%%% 机械可判定（A03/P9 的负例）：
%%%   * 语句里**不得出现 `OFFSET`**（`sql_statements/0` + 静态断言）；
%%%   * `after_id` 必须是绑定参数（`$4`），不得字符串拼接；
%%%   * `limit` 必须是绑定参数（`$5`）且有上界。
-module(eb_pg_message_ext).

-export([list_messages_after/3, default_limit/0, max_limit/0, sql_statements/0]).

-define(DEFAULT_LIMIT, 50).
-define(MAX_LIMIT, 200).

%% 只读：`enterprise_message` 是 canonical 真源，本模块不含任何写语句。
%% 游标语义：`id > $4`（`after_id` 缺省时传 0，等价于首页）。
-define(SQL_LIST_MESSAGES_AFTER, <<
    "SELECT "
    ?MESSAGE_COLUMNS
    "  FROM enterprise_message"
    " WHERE organization_id = $1 AND workspace_id = $2 AND conversation_id = $3"
    "   AND id > $4"
    "   AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id = $1 AND w.id = $2)"
    " ORDER BY id"
    " LIMIT $5"
>>).

-define(MESSAGE_COLUMNS,
    "id, organization_id, workspace_id, conversation_id, sender_type, sender_contact_id,"
    " sender_business_identity_id, actor_user_id, client_msg_id, body_cipher, key_version,"
    " aad_hash, content_hash, policy_id, policy_version, retention_days, retain_until,"
    " visibility, version, created_at"
).

-define(SQL_SCOPE_OK, <<
    "SELECT w.id AS workspace_id"
    "  FROM workspace w"
    " WHERE w.organization_id = $1 AND w.id = $2"
>>).

%% @doc 冻结语句（双租户键机械判据用）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [?SQL_LIST_MESSAGES_AFTER, ?SQL_SCOPE_OK].

-spec default_limit() -> pos_integer().
default_limit() -> ?DEFAULT_LIMIT.

-spec max_limit() -> pos_integer().
max_limit() -> ?MAX_LIMIT.

%% @doc 键集分页读取会话历史。
%%
%% `Query`：
%%   conversation_id  必填；目标会话
%%   after_id         可选（默认 0）；严格 `id > after_id`
%%   limit            可选（默认 50，1..200；越界即 `{error, {invalid_limit, _}}`）
%%
%% 返回 `{ok, Messages}`（按 `id` 升序）或 `{error, Reason}`；跨租户错配返回空列表
%% （不报错、也不返回别人的行）。
-spec list_messages_after(integer(), integer(), map()) -> {ok, [map()]} | {error, term()}.
list_messages_after(OrgId, WorkspaceId, Query) when is_map(Query) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        case query_limit(Query) of
            {error, _} = Err ->
                Err;
            {ok, Limit} ->
                Params = [
                    OrgId,
                    WorkspaceId,
                    maps:get(conversation_id, Query, undefined),
                    after_id(Query),
                    Limit
                ],
                eb_pg_exec:fetch_many(
                    ?SQL_LIST_MESSAGES_AFTER, Params, eb_pg_store_sql:message_fields()
                )
        end
    end);
list_messages_after(_OrgId, _WorkspaceId, _Query) ->
    {error, invalid_query}.

%% 游标缺省 0：`id > 0` 等价于首页；显式负数/非整数一律 fail-closed。
after_id(Query) ->
    case maps:get(after_id, Query, 0) of
        undefined -> 0;
        null -> 0;
        Id when is_integer(Id), Id >= 0 -> Id;
        Other -> Other
    end.

query_limit(Query) ->
    case maps:get(limit, Query, ?DEFAULT_LIMIT) of
        Limit when is_integer(Limit), Limit >= 1, Limit =< ?MAX_LIMIT -> {ok, Limit};
        Other -> {error, {invalid_limit, Other}}
    end.
