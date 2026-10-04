-module(enterprise_webhook_repo).

-moduledoc "企业 Webhook 仓储（EPGZ-04 INT-12/13）—— endpoint 配置与 outbox。".
%%%
% EPGZ-04 INT-12/13 企业 Webhook 仓储（Application endpoint 配置 + outbox）。
%
% 零新表复用既有存储（本卡禁建 migration），ownership 以「Application ->
% principal_user_id <-> bot.user_id」绑定链表达（plan-gz §4.3：Application
% 内部可绑定可信 service-principal user；业务判定只认 Application 绑定）：
%   * bot（迁移 70/92/104）——endpoint 配置载体：webhook_url / events
%     （订阅 jsonb）/ status（启停）/ verify_token_enc（HMAC secret 的
%     AEAD 密文，密钥派生与 bot_repo 同源 postgre_aes_key）。
%     username 固定命名空间 'eapp_<application_key>'（唯一约束防一 App
%     多配置行）；is_public=false；api_token 系列列留空——企业 App 认证
%     只走 enterprise_application_credential，不走 bot token。
%   * bot_delivery / bot_delivery_attempt（迁移 92/104）——durable outbox：
%     bot_id 命名空间 'eapp:<principal_uid>'（非纯数字前缀，与个人 bot 的
%     纯数字 bot_id 物理可分派）；delivery 行快照 webhook_url/host/pin。
%     worker 经 bot_webhook_delivery_worker 的企业分派 hook 执行。
%%%

-export([
    bot_username/1,
    delivery_bot_id/1,
    principal_of_delivery/1,
    find_application_by_principal_tx/3,
    find_bot_config_tx/2,
    upsert_config_tx/5,
    set_secret_tx/3,
    bump_generation_tx/2,
    get_secret/1,
    encrypt_secret/1,
    insert_delivery_tx/2,
    find_delivery_tx/2,
    page_deliveries_tx/6,
    list_deliveries_admin_tx/6,
    delivery_stats_tx/3,
    purgeable_tx/4
]).

-include_lib("epgsql/include/epgsql.hrl").
-include("log.hrl").

-define(DELIVERY_PREFIX, <<"eapp:">>).
%% 列表读面硬上限（无界导出负例：page size 与 page 号都被夹紧）
-define(MAX_PAGE_SIZE, 50).
-define(MAX_PAGE, 1000).
%% 死信/历史保留窗口（天）；purgeable 只读集合的默认口径
-define(DEFAULT_RETENTION_DAYS, 30).

%%%===================================================================
%%% 命名空间
%%%===================================================================

-spec bot_username(binary()) -> binary().
bot_username(ApplicationKey) ->
    <<"eapp_", ApplicationKey/binary>>.

-spec delivery_bot_id(integer()) -> binary().
delivery_bot_id(PrincipalUid) ->
    <<?DELIVERY_PREFIX/binary, (integer_to_binary(PrincipalUid))/binary>>.

%% @doc 从 bot_delivery.bot_id 提取 principal uid（非企业行返回 error）。
-spec principal_of_delivery(binary()) -> {ok, integer()} | error.
principal_of_delivery(<<"eapp:", Rest/binary>>) ->
    try
        {ok, binary_to_integer(Rest)}
    catch
        _:_ -> error
    end;
principal_of_delivery(_) ->
    error.

%%%===================================================================
%%% Application（principal 绑定链）
%%%===================================================================

%% @doc principal -> 同 Org active Application（ownership 判定真源）。
-spec find_application_by_principal_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_application_by_principal_tx(Conn, OrgId, PrincipalUid) ->
    Sql =
        <<
            "SELECT id, application_key, name, principal_user_id, status"
            " FROM enterprise_application"
            " WHERE organization_id = $1 AND principal_user_id = $2 AND status = 'active'"
        >>,
    case elib_pg:query(Conn, Sql, [OrgId, PrincipalUid]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% bot 行（endpoint 配置 + 订阅 + 启停 + secret 密文）
%%%===================================================================

-spec find_bot_config_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_bot_config_tx(Conn, PrincipalUid) ->
    Sql =
        <<
            "SELECT user_id, name, username, webhook_url, verify_token_enc, events::text AS events,"
            " is_public, status, ewh_endpoint_generation FROM bot WHERE user_id = $1"
        >>,
    case elib_pg:query(Conn, Sql, [PrincipalUid]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 端点配置代际 +1（任何配置写入：URL/订阅/启停/轮换都算一代）。
%% 返回新代际；调用方把它写进配置响应，入箱时快照进投递行。
-spec bump_generation_tx(any(), integer()) -> {ok, integer()} | {error, term()}.
bump_generation_tx(Conn, PrincipalUid) ->
    Tb = elib_pg_sql:public_tablename(<<"bot">>),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET ewh_endpoint_generation = ewh_endpoint_generation + 1, updated_at = NOW()"
            " WHERE user_id = $1 RETURNING ewh_endpoint_generation">>,
    case elib_pg:query(Conn, Sql, [PrincipalUid]) of
        {ok, [#{<<"ewh_endpoint_generation">> := Gen} | _]} -> {ok, Gen};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc upsert endpoint 配置（webhook_url / events 订阅 / status 启停）。
%% 首次配置即建行（username 命名空间唯一）；重复配置整体替换。
-spec upsert_config_tx(any(), integer(), map(), [binary()], integer()) ->
    {ok, inserted | updated} | {error, term()}.
upsert_config_tx(Conn, PrincipalUid, Config, Events, Status) ->
    Tb = elib_pg_sql:public_tablename(<<"bot">>),
    Name = maps:get(name, Config, <<"application">>),
    Username = maps:get(username, Config),
    Url = maps:get(webhook_url, Config),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (user_id, name, username, owner_uid, webhook_url, api_token, verify_token,"
            " api_token_digest, api_token_prefix, verify_token_enc, token_migrated,"
            " commands, permissions, events, is_public, status, created_at, updated_at)"
            %% api_token/verify_token 置 NULL：api_token 有 UNIQUE 约束，空串
            %% 会在第二个企业 App 配置时撞唯一（NULL 不参与唯一比较）；企业
            %% App 认证只走 enterprise_application_credential，不走 bot token。
            " VALUES ($1,$2,$3,$1,$4,NULL,NULL,'','', '', true,'[]','[]',$5::jsonb,false,$6,NOW(),NOW())"
            " ON CONFLICT (user_id) DO UPDATE SET webhook_url = EXCLUDED.webhook_url,"
            " events = EXCLUDED.events, status = EXCLUDED.status, updated_at = NOW()"
            " RETURNING (xmax = 0) AS inserted">>,
    case
        elib_pg:query(Conn, Sql, [
            PrincipalUid,
            Name,
            Username,
            Url,
            list_to_json_array(Events),
            Status
        ])
    of
        {ok, [#{<<"inserted">> := true} | _]} ->
            {ok, inserted};
        {ok, [_ | _]} ->
            {ok, updated};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG("enterprise_webhook_repo upsert error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 轮换 HMAC secret：AEAD 密文整列替换（明文只在 logic 返回值出现一次）。
-spec set_secret_tx(any(), integer(), binary()) -> {ok, updated} | {error, no_key | term()}.
set_secret_tx(Conn, PrincipalUid, Secret) ->
    case encrypt_secret(Secret) of
        {error, _} = E ->
            E;
        {ok, Enc} ->
            Tb = elib_pg_sql:public_tablename(<<"bot">>),
            case
                elib_pg:execute(
                    Conn,
                    <<"UPDATE ", Tb/binary,
                        " SET verify_token_enc = $2, verify_token = '', token_migrated = true,"
                        " updated_at = NOW() WHERE user_id = $1">>,
                    [PrincipalUid, Enc]
                )
            of
                {ok, 1} -> {ok, updated};
                {ok, 0} -> {error, not_found};
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc 解密取 HMAC secret（投递签名用；池化）。复用 bot_repo 密钥派生。
-spec get_secret(integer()) -> {ok, binary()} | {error, no_key | notfound | term()}.
get_secret(PrincipalUid) ->
    bot_repo:get_verify_token(PrincipalUid).

%% @doc 与 bot_repo:encrypt_verify_token 同源（postgre_aes_key -> sha256 派生）。
-spec encrypt_secret(binary()) -> {ok, binary()} | {error, no_key | term()}.
encrypt_secret(Secret) when is_binary(Secret) ->
    case derive_aead_key(config_ds:env(postgre_aes_key, <<>>)) of
        {error, no_key} = E ->
            E;
        {ok, Key} ->
            elib_cipher:aes_gcm_encrypt(Secret, Key)
    end.

derive_aead_key(<<>>) ->
    {error, no_key};
derive_aead_key(Key) when is_binary(Key) ->
    {ok, crypto:hash(sha256, Key)};
derive_aead_key(Key) when is_list(Key) ->
    derive_aead_key(list_to_binary(Key));
derive_aead_key(_) ->
    {error, no_key}.

%%%===================================================================
%%% bot_delivery（durable outbox；worker/审计/重试/死信全复用 bot 域）
%%%===================================================================

%% @doc 事件入箱（幂等键 = evt-<event_id>；replay 走独立新行新键）。
%% SQL 与 bot_webhook_delivery_repo:insert_tx 同构（含幂等 ON CONFLICT），
%% 差异：① 不写 agent_hub 审计链（企业事件无 agent task chain）；
%%      ② 额外落 ownership（org/app）、配置代际、replay 来源（FULL-03 账本）。
%% 入箱在调用方事务内（与消息接受/附件确认同 tx 原子提交）。
%% 冲突语义：idempotency_key 命中 = 同事件重复入箱（duplicate）；
%%   uq_ewh_delivery_replay_inflight 命中 = 同原行已有在途重放（同样 duplicate，
%%   由 logic 归一为 idempotency_conflict）——DB 是并发重放的仲裁者。
-spec insert_delivery_tx(any(), map()) -> {ok, inserted | duplicate} | {error, term()}.
insert_delivery_tx(
    Conn,
    #{
        delivery_id := Did,
        bot_id := BotId,
        correlation_id := Corr,
        idempotency_key := Idem,
        webhook_url := WebhookUrl,
        webhook_host := Host,
        pinned_ip := PinnedIP
    } = D
) ->
    Tb = elib_pg_sql:public_tablename(<<"bot_delivery">>),
    EventType = maps:get(event_type, D, <<"message">>),
    Payload = maps:get(payload, D, <<"{}">>),
    OrgId = maps:get(owner_organization_id, D, null),
    AppId = maps:get(owner_application_id, D, null),
    ReplayOf = maps:get(replay_of, D, null),
    Generation = maps:get(endpoint_generation, D, 0),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (delivery_id, bot_id, event_type, payload, reply_context,"
            " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
            " ewh_owner_organization_id, ewh_owner_application_id, ewh_replay_of,"
            " ewh_endpoint_generation)"
            " VALUES ($1,$2,$3,$4::jsonb,'',$5,$6,$7,$8,$9,$10,$11,$12,$13)"
            " ON CONFLICT (idempotency_key) DO NOTHING">>,
    case
        elib_pg:query(Conn, Sql, [
            Did,
            BotId,
            EventType,
            Payload,
            Corr,
            Idem,
            WebhookUrl,
            Host,
            PinnedIP,
            OrgId,
            AppId,
            ReplayOf,
            Generation
        ])
    of
        {ok, [_]} ->
            {ok, inserted};
        {ok, N} when is_integer(N), N > 0 ->
            {ok, inserted};
        {ok, _} ->
            {ok, duplicate};
        {error, #error{code = <<"23505">>}} ->
            %% 在途重放唯一索引命中：并发/重复 replay，另一次已在途
            {ok, duplicate};
        {error, Reason} ->
            ?ERROR_LOG("enterprise_webhook_repo insert_delivery error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 按 delivery_id 取 outbox 行（tx 直连版——replay 在调用方事务内，
%% 必须与业务写入同连接读取：池化连接读不到调用方未提交的状态变更，
%% 且 eunit 直连模式下池不可用）。SQL 含 FULL-03 账本列（ownership/代际/
%% 重放来源/版本/认领时刻）。
-spec find_delivery_tx(any(), binary()) -> {ok, map()} | {error, notfound | term()}.
find_delivery_tx(Conn, DeliveryId) ->
    Tb = elib_pg_sql:public_tablename(<<"bot_delivery">>),
    Sql =
        <<
            "SELECT delivery_id, bot_id, event_type, payload::text AS payload,"
            " reply_context, correlation_id, idempotency_key, status,"
            " attempt_count, next_retry_at, webhook_url, webhook_host, pinned_ip,"
            " ewh_owner_organization_id, ewh_owner_application_id, ewh_replay_of,"
            " ewh_endpoint_generation, ewh_ledger_version, ewh_claimed_at,"
            " created_at, updated_at FROM ",
            Tb/binary,
            " WHERE delivery_id = $1"
        >>,
    case elib_pg:query(Conn, Sql, [DeliveryId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% FULL-03 读面：投递列表 / 统计 / 保留窗口（全部只读，含硬上限）
%%%===================================================================

%% @doc 本 Application 的投递列表一页（CP-CON-02 / DEC-INT23-COMPAT 切
%% CURSOR-V2 keyset；metadata only——**不含 payload**，无 secret/无正文/
%% 无签名 URL）。按 (org, app) 归属过滤（不是 bot_id 前缀推导），可选
%% status 过滤；keyset 前开区间 `(created_at, delivery_id) < After`。
%% 排序冻结 created_at DESC, delivery_id DESC——migration 150 的 partial
%% index bot_delivery_ewh_keyset_idx (org, app, created_at DESC,
%% delivery_id DESC) 服务该序（无 OFFSET、无 COUNT 导出）。
%% After = undefined（首页）| {CreatedAt, DeliveryId}（上一页末行 keyset
%% 值，由 CURSOR-V2 游标验签解出）；Limit 由 logic 传 PageSize+1（多取
%% 一行判 has_more，额外行不进入 items）。
-spec page_deliveries_tx(
    any(),
    integer(),
    integer(),
    undefined | binary(),
    undefined | {binary(), binary()},
    pos_integer()
) ->
    {ok, [map()]} | {error, term()}.
page_deliveries_tx(Conn, OrgId, AppId, Status, After, Limit) ->
    Tb = elib_pg_sql:public_tablename(<<"bot_delivery">>),
    {BaseWhere, BaseParams} = list_where(OrgId, AppId, Status),
    {Where, Params} = keyset_where(BaseWhere, BaseParams, After),
    LimitP = length(Params) + 1,
    Sql =
        <<
            "SELECT delivery_id, event_type, status, attempt_count, webhook_host,"
            " ewh_replay_of, ewh_ledger_version, ewh_claimed_at, created_at, updated_at"
            " FROM ",
            Tb/binary,
            " WHERE ",
            Where/binary,
            " ORDER BY created_at DESC, delivery_id DESC LIMIT $",
            (integer_to_binary(LimitP))/binary
        >>,
    case elib_pg:query(Conn, Sql, Params ++ [Limit]) of
        {ok, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, _N} when is_integer(_N) -> {ok, []};
        {error, Reason} -> {error, Reason}
    end.

%% keyset 前开区间拼接：After 为空即首页；否则行构造器比较
%% (created_at, delivery_id) < ($n::timestamptz, $n+1::text)——
%% delivery_id 兜底排序键保证重复 created_at 的稳定翻页（无重无漏）。
-spec keyset_where(binary(), [term()], undefined | {binary(), binary()}) ->
    {binary(), [term()]}.
keyset_where(BaseWhere, BaseParams, undefined) ->
    {BaseWhere, BaseParams};
keyset_where(BaseWhere, BaseParams, {AfterCreated, AfterId}) ->
    P1 = integer_to_binary(length(BaseParams) + 1),
    P2 = integer_to_binary(length(BaseParams) + 2),
    {
        <<
            BaseWhere/binary,
            " AND (created_at, delivery_id) < ($",
            P1/binary,
            "::timestamptz, $",
            P2/binary,
            "::text)"
        >>,
        BaseParams ++ [AfterCreated, AfterId]
    }.

%% @doc Admin 治理面（FULL-08 / A-13）的投递**元数据**读面。
%% 与 internal keyset 读面（page_deliveries_tx/6）同归属过滤、同排序，只是
%% 分页仍为 offset 夹紧（Admin 治理面非 §10.1 internal list family，
%% CP-CON-02 只迁 INT-23）且**列集更宽**：
%%   * 补 correlation_id / next_retry_at / ewh_endpoint_generation —— Admin 治理
%%     需要这些运维元数据，既有 internal 读面出于「最小暴露」没选；
%%   * 补 `payload->>'event_id'` —— event_id 只存在于事件信封（payload）里，
%%     此处**只取这一个标量**。payload 本体永远不下发到 Admin（业务正文，
%%     plan-full §7：Admin 投递面不得出现 payload / body）。
%% 单独开一个入口而不是改既有 SELECT：既有 internal 读面的列集被 FULL-03 套件
%% 逐键断言，加列会打破既有契约。
-spec list_deliveries_admin_tx(
    any(), integer(), integer(), undefined | binary(), integer(), integer()
) ->
    {ok, [map()]} | {error, term()}.
list_deliveries_admin_tx(Conn, OrgId, AppId, Status, Page0, Size0) ->
    Page = clamp_page(Page0),
    Size = clamp_size(Size0),
    Tb = elib_pg_sql:public_tablename(<<"bot_delivery">>),
    {Where, Params} = list_where(OrgId, AppId, Status),
    LimitP = length(Params) + 1,
    OffsetP = LimitP + 1,
    Sql =
        <<
            "SELECT delivery_id, event_type, status, attempt_count, correlation_id,"
            " next_retry_at, ewh_endpoint_generation, ewh_ledger_version, ewh_replay_of,"
            " created_at, updated_at,"
            " NULLIF(payload->>'event_id', '') AS event_id"
            " FROM ",
            Tb/binary,
            " WHERE ",
            Where/binary,
            " ORDER BY created_at DESC, delivery_id DESC LIMIT $",
            (integer_to_binary(LimitP))/binary,
            " OFFSET $",
            (integer_to_binary(OffsetP))/binary
        >>,
    case elib_pg:query(Conn, Sql, Params ++ [Size, (Page - 1) * Size]) of
        {ok, Rows} when is_list(Rows) -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 归属过滤（org+app 复合；status 可选）——企业与 bot 域行物理混表，
%% 归属列非空才可能被本读面看到（bot 域行 NULL 天然不在集合内）。
list_where(OrgId, AppId, undefined) ->
    {<<"ewh_owner_organization_id = $1 AND ewh_owner_application_id = $2">>, [OrgId, AppId]};
list_where(OrgId, AppId, Status) ->
    {
        <<
            "ewh_owner_organization_id = $1 AND ewh_owner_application_id = $2"
            " AND status = $3"
        >>,
        [OrgId, AppId, Status]
    }.

%% @doc 投递健康度（只读聚合）：状态计数 + 尝试次数 + 重试次数 + 在途重放数。
%% 成功率口径在 logic 层算（success / (success+dead)），此处只给原始计数。
-spec delivery_stats_tx(any(), integer(), integer()) -> {ok, map()} | {error, term()}.
delivery_stats_tx(Conn, OrgId, AppId) ->
    Tb = elib_pg_sql:public_tablename(<<"bot_delivery">>),
    ATb = elib_pg_sql:public_tablename(<<"bot_delivery_attempt">>),
    Sql =
        <<
            "SELECT status, count(*) AS n, coalesce(sum(attempt_count), 0) AS attempts,"
            " coalesce(sum(greatest(attempt_count - 1, 0)), 0) AS retries"
            " FROM ",
            Tb/binary,
            " WHERE ewh_owner_organization_id = $1 AND ewh_owner_application_id = $2"
            " GROUP BY status"
        >>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, Rows} when is_list(Rows) ->
            AttemptSql =
                <<
                    "SELECT count(*) AS n FROM ",
                    ATb/binary,
                    " a JOIN ",
                    Tb/binary,
                    " d ON d.delivery_id = a.delivery_id"
                    " WHERE d.ewh_owner_organization_id = $1 AND d.ewh_owner_application_id = $2"
                >>,
            case elib_pg:query(Conn, AttemptSql, [OrgId, AppId]) of
                {ok, [#{<<"n">> := AttemptRows} | _]} ->
                    {ok, stats_of(Rows, AttemptRows)};
                {ok, _} ->
                    {ok, stats_of(Rows, 0)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, _N} when is_integer(_N) ->
            {ok, stats_of([], 0)};
        {error, Reason} ->
            {error, Reason}
    end.

stats_of(Rows, AttemptRows) ->
    Statuses = maps:from_list([
        {maps:get(<<"status">>, R), maps:get(<<"n">>, R)}
     || R <- Rows
    ]),
    Attempts = lists:sum([maps:get(<<"attempts">>, R, 0) || R <- Rows]),
    Retries = lists:sum([maps:get(<<"retries">>, R, 0) || R <- Rows]),
    #{
        <<"status_counts">> => Statuses,
        <<"attempt_count">> => Attempts,
        <<"retry_count">> => Retries,
        <<"attempt_rows">> => AttemptRows,
        <<"dead_letter_count">> => maps:get(<<"dead">>, Statuses, 0)
    }.

%% @doc 保留窗口只读集合（plan-full §3.1「retention」）：企业投递中**已终结**
%% 且 updated_at 早于 cutoff 的行。在途（pending/retry）永不入选——保留策略
%% 不得成为投递丢失的路径。本函数只读，不做任何删除（物理清理留给运维 runbook）。
-spec purgeable_tx(any(), integer(), integer(), pos_integer() | map()) ->
    {ok, [map()]} | {error, term()}.
purgeable_tx(Conn, OrgId, AppId, Days) ->
    Days1 =
        case Days of
            D when is_integer(D), D > 0 -> D;
            _ -> ?DEFAULT_RETENTION_DAYS
        end,
    Tb = elib_pg_sql:public_tablename(<<"bot_delivery">>),
    Sql =
        <<
            "SELECT delivery_id, status, attempt_count, updated_at FROM ",
            Tb/binary,
            " WHERE ewh_owner_organization_id = $1 AND ewh_owner_application_id = $2"
            " AND status IN ('success','dead')"
            " AND updated_at < NOW() - ($3 || ' days')::interval"
            " ORDER BY updated_at ASC LIMIT $4"
        >>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, integer_to_binary(Days1), ?MAX_PAGE_SIZE]) of
        {ok, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, _N} when is_integer(_N) -> {ok, []};
        {error, Reason} -> {error, Reason}
    end.

clamp_page(Page) when is_integer(Page), Page >= 1 -> min(Page, ?MAX_PAGE);
clamp_page(_) -> 1.

clamp_size(Size) when is_integer(Size), Size >= 1 -> min(Size, ?MAX_PAGE_SIZE);
clamp_size(_) -> 20.

%%%===================================================================
%%% Internal
%%%===================================================================

list_to_json_array(Events) ->
    jsone:encode(Events).
