-module(enterprise_webhook_repo).

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
    get_secret/1,
    encrypt_secret/1,
    insert_delivery_tx/2,
    find_delivery_tx/2
]).

-include_lib("epgsql/include/epgsql.hrl").
-include("log.hrl").

-define(DELIVERY_PREFIX, <<"eapp:">>).

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
            " is_public, status FROM bot WHERE user_id = $1"
        >>,
    case elib_pg:query(Conn, Sql, [PrincipalUid]) of
        {ok, [Row | _]} -> {ok, Row};
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
        {ok, _} ->
            {ok, updated};
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
            _ =
                elib_pg:execute(
                    Conn,
                    <<"UPDATE ", Tb/binary,
                        " SET verify_token_enc = $2, verify_token = '', token_migrated = true,"
                        " updated_at = NOW() WHERE user_id = $1">>,
                    [PrincipalUid, Enc]
                ),
            {ok, updated}
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
%% 差异仅两处：不写 agent_hub 审计链（企业事件无 agent task chain，
%% record_delivery_tx 对无 chain correlation 本就 no_chain 跳过）；
%% 入箱在调用方事务内（与消息接受/附件确认同 tx 原子提交）。
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
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (delivery_id, bot_id, event_type, payload, reply_context,"
            " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip)"
            " VALUES ($1,$2,$3,$4::jsonb,'',$5,$6,$7,$8,$9)"
            " ON CONFLICT (idempotency_key) DO NOTHING">>,
    case
        elib_pg:query(Conn, Sql, [
            Did, BotId, EventType, Payload, Corr, Idem, WebhookUrl, Host, PinnedIP
        ])
    of
        {ok, [_]} ->
            {ok, inserted};
        {ok, N} when is_integer(N), N > 0 -> {ok, inserted};
        {ok, _} ->
            {ok, duplicate};
        {error, Reason} ->
            ?ERROR_LOG("enterprise_webhook_repo insert_delivery error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 按 delivery_id 取 outbox 行（tx 直连版——replay 在调用方事务内，
%% 必须与业务写入同连接读取：池化连接读不到调用方未提交的状态变更，
%% 且 eunit 直连模式下池不可用）。SQL 与
%% bot_webhook_delivery_repo:get_delivery/1 同构（含 payload::text）。
-spec find_delivery_tx(any(), binary()) -> {ok, map()} | {error, notfound | term()}.
find_delivery_tx(Conn, DeliveryId) ->
    Tb = elib_pg_sql:public_tablename(<<"bot_delivery">>),
    Sql =
        <<
            "SELECT delivery_id, bot_id, event_type, payload::text AS payload,"
            " reply_context, correlation_id, idempotency_key, status,"
            " attempt_count, next_retry_at, webhook_url, webhook_host, pinned_ip,"
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
%%% Internal
%%%===================================================================

list_to_json_array(Events) ->
    jsone:encode(Events).
