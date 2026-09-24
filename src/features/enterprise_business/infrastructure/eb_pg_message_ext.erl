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

-export([
    list_messages_after/3,
    default_limit/0,
    max_limit/0,
    asset_projection_fields/0,
    sql_statements/0
]).

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

%% CS-BE-01（历史消息资产投影）：一页消息的绑定资产**批量**读取（`ANY($3)`），
%% 逐消息 N+1 在此被结构性排除——每页恒定 1 次消息查询 + 1 次资产查询。
%%   * 投影白名单冻结为 {id, mime, size_bytes, file_name, status}（+分组用
%%     message_id，出站前剥离）：**不含** object_key / 任何 URL / 上传凭证；
%%   * 仅 `status = 'active'`：deleted（软删）与 pending_confirm（未绑定，
%%     绑定门只放 active）不出历史投影；
%%   * 双租户键钉在语句里（org = $1 / ws = $2）：跨 Org/Workspace 的资产
%%     行在本语句层即不可见，不是投影层过滤。
-define(SQL_LIST_MESSAGE_ASSETS, <<
    "SELECT a.id, a.message_id, a.mime, a.size_bytes, a.file_name, a.status"
    "  FROM enterprise_asset a"
    " WHERE a.organization_id = $1 AND a.workspace_id = $2"
    "   AND a.message_id = ANY($3::bigint[])"
    "   AND a.status = 'active'"
    " ORDER BY a.id"
>>).

%% @doc 冻结语句（双租户键机械判据用）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [?SQL_LIST_MESSAGES_AFTER, ?SQL_LIST_MESSAGE_ASSETS, ?SQL_SCOPE_OK].

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
                case
                    eb_pg_exec:fetch_many(
                        ?SQL_LIST_MESSAGES_AFTER, Params, eb_pg_store_sql:message_fields()
                    )
                of
                    {ok, Rows} -> attach_page_assets(OrgId, WorkspaceId, Rows);
                    {error, _} = Err -> Err
                end
        end
    end);
list_messages_after(_OrgId, _WorkspaceId, _Query) ->
    {error, invalid_query}.

%% CS-BE-01：把一页消息各自绑定的资产白名单批量装配到行上。
%% 空页零额外查询；非空页恒定再发 1 条批量语句（与页内消息数无关）。
%% 资产查询失败 fail-closed（整页报错，绝不夹带「无附件」的降级行）。
-spec attach_page_assets(integer(), integer(), [map()]) -> {ok, [map()]} | {error, term()}.
attach_page_assets(_OrgId, _WorkspaceId, []) ->
    {ok, []};
attach_page_assets(OrgId, WorkspaceId, Rows) ->
    MessageIds = [maps:get(id, Row) || Row <- Rows],
    case
        eb_pg_exec:fetch_many(
            ?SQL_LIST_MESSAGE_ASSETS, [OrgId, WorkspaceId, MessageIds], asset_projection_fields()
        )
    of
        {ok, AssetRows} ->
            ByMessage = asset_rows_by_message(AssetRows),
            {ok, [
                Row#{assets => maps:get(maps:get(id, Row), ByMessage, [])}
             || Row <- Rows
            ]};
        {error, _} = Err ->
            Err
    end.

asset_rows_by_message(AssetRows) ->
    lists:foldl(
        fun(Asset, Acc) ->
            MessageId = maps:get(message_id, Asset, undefined),
            View = maps:without([message_id], Asset),
            maps:update_with(
                MessageId, fun(Existing) -> Existing ++ [View] end, [View], Acc
            )
        end,
        #{},
        AssetRows
    ).

%% @doc 冻结契约 assets:[{id,mime,size_bytes,file_name,status}] 的行归一化规格
%% （message_id 仅作分组键，装配后剥离；status 归一为原子）。
-spec asset_projection_fields() -> [{atom(), binary(), atom()}].
asset_projection_fields() ->
    [
        {id, <<"id">>, int},
        {message_id, <<"message_id">>, int},
        {mime, <<"mime">>, bin},
        {size_bytes, <<"size_bytes">>, int},
        {file_name, <<"file_name">>, bin},
        {status, <<"status">>, atom}
    ].

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
