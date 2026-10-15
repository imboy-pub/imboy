%%% @doc EB-03R P13 + P12：`eb_asset_port` 的**唯一装配实现**。
%%%
%%% 依据：R0-5——`eb_infra_ports:resolve(asset)` 实测返回
%%% `{error, {not_implemented_yet, asset}}`，而 `eb_asset_port` 契约早已冻结；
%%% 本模块让装配与契约对齐（A11 的装配证据：`resolve(asset)` 返回**模块**）。
%%%
%%% ## 两半职责
%%%
%%%   * **object-store 三件**（`put_private/3` / `stream_content/3` / `delete_private/3`）：
%%%     默认经 Garage adapter 实现；本地替身须显式选择；对象 key 只由实现派生
%%%     （`eb_pg_asset_meta:object_key/3`），调用方拿不到 key / bucket / endpoint / 链接。
%%%   * **metadata 生命周期**（`insert_asset/3` / `fetch_asset/3` / `confirm_asset/3` /
%%%     `cleanup_asset/3`）：下沉到 `eb_pg_asset_meta`（PG）。
%%%
%%% ## 作用域（A11-2）
%%%
%%% 每次调用都先用**请求自己的** `(OrgId, WorkspaceId)` 去取元数据：
%%%   * 元数据不存在或不属于该租户 ⇒ `{error, not_found}`（不区分「不存在」与「不是你的」）；
%%%   * 对象 key 由同一对租户键派生 ⇒ 跨 Org 的同 id 资产必然落在不同 key；
%%%   * 替身 adapter 还会独立校验 key 的租户前缀（`out_of_scope`）——纵深两道。
%%%
%%% ## 错误传播（A11-3）
%%%
%%% 任何底层失败（对象不存在 / payload 非法 / 作用域不符 / SQL 守卫拒绝）都映射为
%%% `{error, Reason}` 元组，**不得静默返回 `{ok, _}`**。
%%%
%%% ## 边界声明
%%%
%%% 生产默认 Garage；测试通过 eb_asset_object_store=stub 显式选择替身。
%%% 实际验收须区分替身合同、真实存储与完整附件旅程。
-module(eb_asset_store).

-moduledoc "eb_asset_port 的唯一装配实现（EB-03R P13 + P12）。".
-behaviour(eb_asset_port).

-include_lib("epgsql/include/epgsql.hrl").

-export([
    put_private/3,
    stream_content/3,
    delete_private/3,
    insert_asset/3,
    fetch_asset/3,
    confirm_asset/3,
    cleanup_asset/3,
    cleanup_pending_private/5,
    delete_queued_object/3,
    scope_key/3,
    fetch_session/3,
    session_by_client_id/3,
    renew_session/4,
    register_session/8
]).

%% @doc 由作用域派生对象 key（与元数据 `object_key` 同源，单一真源）。
-spec scope_key(integer(), integer(), integer()) -> binary().
scope_key(OrgId, WorkspaceId, AssetId) ->
    eb_pg_asset_meta:object_key(OrgId, WorkspaceId, AssetId).

%% @doc 先原子预留资产记录，再落企业私有对象（status = `pending_confirm`）。
%%
%% `Descriptor` 必含 `id` / `object_hash` / `payload`（bytes）；可选 `mime` /
%% `size_bytes` / `key_version` / `retain_until` / `conversation_id` / `message_id` /
%% `business_identity_id` / `uploaded_by_user_id`。返回摘要**不含** key / 链接。
-spec put_private(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
put_private(OrgId, WorkspaceId, Descriptor) when is_map(Descriptor) ->
    with_object_store(fun(ObjectStore) ->
        eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
            case {maps:get(id, Descriptor, undefined), maps:get(payload, Descriptor, undefined)} of
                {Id, _} when not is_integer(Id) -> {error, invalid_descriptor};
                {_, Payload} when not is_binary(Payload) ->
                    {error, {object_store, invalid_payload}};
                {_, <<>>} ->
                    {error, {object_store, empty_payload}};
                {_, Payload} ->
                    reserve_and_put(OrgId, WorkspaceId, Descriptor, Payload, ObjectStore)
            end
        end)
    end);
put_private(_OrgId, _WorkspaceId, _Descriptor) ->
    {error, invalid_descriptor}.

reserve_and_put(OrgId, WorkspaceId, Descriptor, Payload, ObjectStore) ->
    Size =
        case maps:get(size_bytes, Descriptor, undefined) of
            N when is_integer(N) -> N;
            _ -> byte_size(Payload)
        end,
    case eb_pg_asset_meta:insert_asset(OrgId, WorkspaceId, Descriptor#{size_bytes => Size}) of
        {ok, Row} ->
            %% V5.5（02 §15）：Descriptor 可携带冻结签名 Context
            %% （绝对 issued_at/expires_at），经 Meta 端到端传到对象适配器；
            %% 无 Context 的调用方（非 V5）保持原 Meta 形状。
            Meta0 = maps:with([mime, size_bytes], Row),
            Meta =
                case maps:get(signature_context, Descriptor, undefined) of
                    Ctx when is_map(Ctx) -> Meta0#{signature_context => Ctx};
                    _ -> Meta0
                end,
            write_reserved(Row, Payload, Meta, ObjectStore);
        {error, _} = Error ->
            Error
    end.

write_reserved(Row, Payload, Meta, ObjectStore) ->
    case ObjectStore:put(maps:get(object_key, Row), Payload, Meta) of
        ok ->
            {ok, #{
                asset_id => maps:get(id, Row),
                status => maps:get(status, Row),
                storage_ref => maps:get(object_key, Row),
                size_bytes => maps:get(size_bytes, Row)
            }};
        {error, Reason} ->
            %% A failed/uncertain write keeps its reserved pending row for bounded cleanup.
            %% Never replay over, or delete, an object owned by an earlier upload.
            {error, {object_store, Reason}}
    end.

%% @doc 鉴权代理取流：返回**流句柄**（不透明），绝不返回链接或 key。
%%
%% 每次调用都重新按请求自己的 `(OrgId, WorkspaceId)` 取元数据（逐请求鉴权的落点）；
%% 跨 Org / 已 deleted 一律 `{error, not_found}`。
-spec stream_content(integer(), integer(), integer()) -> {ok, term()} | {error, term()}.
stream_content(OrgId, WorkspaceId, AssetId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        case eb_pg_asset_meta:fetch_asset(OrgId, WorkspaceId, AssetId) of
            {ok, Row} ->
                case maps:get(status, Row, undefined) of
                    deleted ->
                        {error, not_found};
                    _Other ->
                        read_object(OrgId, WorkspaceId, AssetId, Row)
                end;
            {error, _} = Err ->
                Err
        end
    end).

read_object(OrgId, WorkspaceId, _AssetId, Row) ->
    Key = maps:get(object_key, Row),
    with_object_store(fun(ObjectStore) ->
        case ObjectStore:get(Key, eb_pg_asset_meta:key_prefix(OrgId, WorkspaceId)) of
            {ok, #{bytes := Bytes}} ->
                {ok, {content_stream, Bytes}};
            {error, Reason} ->
                {error, {object_store, Reason}}
        end
    end).

%% @doc 仅由 Org policy 触发；不是 uploader 可自主决定的动作。
%% W4-01 入口收口（02 §4）：不再直接删对象——发企业域删除意图
%% （fn_v5_intent_eb_delete 锁后复核 lifecycle/retain/hold/队列查重；
%% 身份缺失 → blocked job 明示，迁移 186），字节由队列 Worker 删；
%% 消息保留期内的资产拒绝（23510→intent_conflict，消息保留权威优先）。
-spec delete_private(integer(), integer(), integer()) -> ok | {error, term()}.
delete_private(OrgId, WorkspaceId, AssetId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        elib_pg:with_tx(fun(Conn) ->
            case
                elib_pg:query(
                    Conn,
                    <<"SELECT fn_v5_intent_eb_delete($1,$2,$3,'org policy private delete')">>,
                    [OrgId, WorkspaceId, AssetId]
                )
            of
                {ok, _} ->
                    ok;
                {error, #error{code = <<"23505">>}} ->
                    %% job 已在队列：意图已存在即达成目的
                    ok;
                {error, #error{code = <<"23510">>}} ->
                    {error, intent_conflict};
                {error, #error{code = <<"P0002">>}} ->
                    {error, not_found};
                {error, Reason} ->
                    {error, Reason}
            end
        end)
    end).

%% @doc metadata-only 登记（对象已由受控通道就位时使用）。
-spec insert_asset(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_asset(OrgId, WorkspaceId, Descriptor) ->
    eb_pg_asset_meta:insert_asset(OrgId, WorkspaceId, Descriptor).

%% @doc 读取资产元数据。
-spec fetch_asset(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_asset(OrgId, WorkspaceId, AssetId) ->
    eb_pg_asset_meta:fetch_asset(OrgId, WorkspaceId, AssetId).

%% @doc 确认资产（pending_confirm → active）。
-spec confirm_asset(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
confirm_asset(OrgId, WorkspaceId, AssetId) ->
    eb_pg_asset_meta:confirm_asset(OrgId, WorkspaceId, AssetId).

%% @doc 回收资产元数据（→ deleted）。
-spec cleanup_asset(integer(), integer(), integer()) -> ok | {error, term()}.
cleanup_asset(OrgId, WorkspaceId, AssetId) ->
    eb_pg_asset_meta:cleanup_asset(OrgId, WorkspaceId, AssetId).

with_object_store(Fun) ->
    case application:get_env(imboy, eb_asset_object_store, garage) of
        garage -> Fun(eb_asset_object_garage);
        stub -> Fun(eb_asset_object_stub);
        _ -> {error, invalid_object_store}
    end.

cleanup_pending_private(Org, Ws, Id, Now, Ttl) when
    is_integer(Org), is_integer(Ws), is_integer(Id), is_integer(Now), is_integer(Ttl), Ttl >= 0
->
    case eb_pg_asset_meta:fetch_asset(Org, Ws, Id) of
        {ok, #{status := deleted, pending_object_delete := true, object_key := Key}} ->
            delete_pending_object(Org, Ws, Id, Key);
        {ok, #{status := pending_confirm}} ->
            case eb_pg_asset_meta:claim_pending_cleanup(Org, Ws, Id, Now, Ttl) of
                {ok, Key} -> delete_pending_object(Org, Ws, Id, Key);
                {error, _} = Err -> Err
            end;
        {ok, _} ->
            {error, not_eligible};
        {error, _} = Err ->
            Err
    end;
cleanup_pending_private(_, _, _, _, _) ->
    {error, invalid_cleanup_scope}.

%% W4-01 入口收口（02 §4）：pending 字节回收不再直接删对象——发企业域
%% 删除意图（fn_v5_intent_eb_delete：lifecycle→delete_intent + queue job
%% + 审计；资格 claim CAS 已预检，fn 锁内再复核；身份缺失 → blocked job
%% 明示，迁移 186），字节由队列 Worker 删（delete_queued_object/3）。
%% finish 与 fn 同事务同连接：fn 已持该行锁，跨连接 finish 会自等锁超时。
delete_pending_object(Org, Ws, Id, _Key) ->
    elib_pg:with_tx(fun(Conn) ->
        case
            elib_pg:query(
                Conn,
                <<"SELECT fn_v5_intent_eb_delete($1,$2,$3,'pending cleanup')">>,
                [Org, Ws, Id]
            )
        of
            {ok, _} ->
                %% job 已在队列（幂等重放）也照常收口 metadata 终态
                case eb_pg_asset_meta:finish_pending_cleanup_tx(Conn, Org, Ws, Id) of
                    ok -> ok;
                    {error, Reason} -> {error, Reason}
                end;
            {error, Reason} ->
                {error, {intent_failed, Reason}}
        end
    end).

%% Internal durable purge task; caller supplies the committed key after metadata is gone.
delete_queued_object(Org, Ws, Key) ->
    with_object_store(fun(ObjectStore) ->
        ObjectStore:delete(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    end).

fetch_session(Org, Ws, AssetId) -> eb_pg_asset_meta:v5_session(Org, Ws, AssetId).
session_by_client_id(Org, Ws, ClientId) -> eb_pg_asset_meta:v5_session_by_cid(Org, Ws, ClientId).
renew_session(Org, Ws, AssetId, ExpiresAt) ->
    eb_pg_asset_meta:renew_v5(Org, Ws, AssetId, ExpiresAt).

register_session(Org, Ws, Descriptor, ClientId, Registration, Kind, Deadline, ExpiresAt) ->
    case eb_pg_asset_meta:insert_asset(Org, Ws, Descriptor) of
        {ok, _} ->
            AssetId = maps:get(id, Descriptor),
            Key = scope_key(Org, Ws, AssetId),
            eb_pg_asset_meta:register_v5(
                Org,
                Ws,
                AssetId,
                Key,
                ClientId,
                Registration,
                Kind,
                Deadline,
                ExpiresAt
            );
        {error, _} = Error ->
            Error
    end.
