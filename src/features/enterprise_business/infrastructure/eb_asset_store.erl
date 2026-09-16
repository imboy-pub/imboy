%%% @doc EB-03R P13 + P12：`eb_asset_port` 的**唯一装配实现**。
%%%
%%% 依据：R0-5——`eb_infra_ports:resolve(asset)` 实测返回
%%% `{error, {not_implemented_yet, asset}}`，而 `eb_asset_port` 契约早已冻结；
%%% 本模块让装配与契约对齐（A11 的装配证据：`resolve(asset)` 返回**模块**）。
%%%
%%% ## 两半职责
%%%
%%%   * **object-store 三件**（`put_private/3` / `stream_content/3` / `delete_private/3`）：
%%%     经本地替身 adapter（`eb_asset_object_stub`）实现；对象 key 只由实现派生
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
%%% 对象存储侧是**本地替身**验证：`adapter_contract_verified_locally;
%%% real_garage_acceptance=NOT_RUN`。不得据此宣称真实对象存储验收通过。
-module(eb_asset_store).

-behaviour(eb_asset_port).

-export([
    put_private/3,
    stream_content/3,
    delete_private/3,
    insert_asset/3,
    fetch_asset/3,
    confirm_asset/3,
    cleanup_asset/3,
    scope_key/3
]).

%% @doc 由作用域派生对象 key（与元数据 `object_key` 同源，单一真源）。
-spec scope_key(integer(), integer(), integer()) -> binary().
scope_key(OrgId, WorkspaceId, AssetId) ->
    eb_pg_asset_meta:object_key(OrgId, WorkspaceId, AssetId).

%% @doc 把未确认对象落为企业私有对象并登记元数据（status = `pending_confirm`）。
%%
%% `Descriptor` 必含 `id` / `object_hash` / `payload`（bytes）；可选 `mime` /
%% `size_bytes` / `key_version` / `retain_until` / `conversation_id` / `message_id` /
%% `business_identity_id` / `uploaded_by_user_id`。返回摘要**不含** key / 链接。
-spec put_private(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
put_private(OrgId, WorkspaceId, Descriptor) when is_map(Descriptor) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        AssetId = maps:get(id, Descriptor, undefined),
        Payload = maps:get(payload, Descriptor, undefined),
        case is_integer(AssetId) of
            false ->
                {error, invalid_descriptor};
            true ->
                Key = scope_key(OrgId, WorkspaceId, AssetId),
                Meta = #{
                    mime => maps:get(mime, Descriptor, undefined),
                    size_bytes => maps:get(size_bytes, Descriptor, undefined)
                },
                case eb_asset_object_stub:put(Key, Payload, Meta) of
                    ok ->
                        register_metadata(OrgId, WorkspaceId, Descriptor, Payload);
                    {error, Reason} ->
                        %% 底层失败必须显式传播（不得假装成功）
                        {error, {object_store, Reason}}
                end
        end
    end);
put_private(_OrgId, _WorkspaceId, _Descriptor) ->
    {error, invalid_descriptor}.

register_metadata(OrgId, WorkspaceId, Descriptor, Payload) ->
    Size =
        case maps:get(size_bytes, Descriptor, undefined) of
            N when is_integer(N) -> N;
            _ -> byte_size(Payload)
        end,
    case
        eb_pg_asset_meta:insert_asset(OrgId, WorkspaceId, maps:put(size_bytes, Size, Descriptor))
    of
        {ok, Row} ->
            {ok, #{
                asset_id => maps:get(id, Row),
                status => maps:get(status, Row),
                storage_ref => maps:get(object_key, Row),
                size_bytes => maps:get(size_bytes, Row)
            }};
        {error, _} = Err ->
            %% 元数据登记失败 ⇒ 把对象也撤回（不留孤儿对象）
            _ = eb_asset_object_stub:delete(
                scope_key(OrgId, WorkspaceId, maps:get(id, Descriptor, undefined)),
                eb_asset_object_stub:key_prefix(OrgId, WorkspaceId)
            ),
            Err
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
    case eb_asset_object_stub:get(Key, eb_asset_object_stub:key_prefix(OrgId, WorkspaceId)) of
        {ok, #{bytes := Bytes}} ->
            {ok, {content_stream, Bytes}};
        {error, Reason} ->
            {error, {object_store, Reason}}
    end.

%% @doc 仅由 Org policy 或未确认对象回收触发；不是 uploader 可自主决定的动作。
-spec delete_private(integer(), integer(), integer()) -> ok | {error, term()}.
delete_private(OrgId, WorkspaceId, AssetId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        case eb_pg_asset_meta:fetch_asset(OrgId, WorkspaceId, AssetId) of
            {ok, Row} ->
                Key = maps:get(object_key, Row),
                case
                    eb_asset_object_stub:delete(
                        Key, eb_asset_object_stub:key_prefix(OrgId, WorkspaceId)
                    )
                of
                    ok -> ok;
                    {error, Reason} -> {error, {object_store, Reason}}
                end;
            {error, _} = Err ->
                Err
        end
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
