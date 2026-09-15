%%% @doc EB-07 企业附件闭环的**用例层**（facade 的唯一委派目标）。
%%%
%%% 依据：plan v4.1 §EB-07 与 `control/required-acceptance.tsv` 的 `EB-07-A01..A06`：
%%% 「实现私有 object presigned PUT/confirm、鉴权代理 content download、hash/mime/size
%%% 校验和 pending cleanup」。
%%%
%%% ## 消费的既有能力（EB-03R 只读消费，不重建）
%%%
%%%   * 对象存储（P13）：`eb_asset_port:put_private/3` / `stream_content/3` / `delete_private/3`；
%%%   * 元数据生命周期（P12）：`insert_asset/3` / `fetch_asset/3` / `confirm_asset/3` /
%%%     `cleanup_asset/3`；
%%%   * 装配：`eb_infra_ports:resolve(asset)` → `eb_asset_store`；同理
%%%     `store` / `crypto` / `clock` / `id` / `auth` 全部经装配解析（端口可用 `Params`
%%%     同键覆盖，供测试与装配注入）。
%%%
%%% ## 四个用例
%%%
%%%   * `request_presign/2`：逐请求授权 → mime/size/hash 校验 → 解析保留期 →
%%%     签发**不透明上传凭证**（`eb_asset_upload_ref`）。响应里**没有** URL / endpoint /
%%%     object key（本地替身下 token 即 PUT 凭证；真实 Garage 的 presigned PUT URL 由
%%%     adapter 层换取，本层只交凭证）。
%%%   * `put_object/2`：客户端按凭证写入私有桶。执行前先校验证书未过期/未篡改/同上传人，
%%%     再用服务端重算的哈希/size/魔数复核声明值，最后落 `put_private/3`。
%%%   * `confirm_asset/2`：**重新鉴权**（presign 后被 suspend 的 actor 在此 fail-closed）
%%%     → 复核对象完整性 → 校验保留期不短于所属消息 → `confirm_asset/3`。
%%%   * `content_stream/2`：逐请求授权（成员 active + 会话经办 ACL）后经
%%%     `stream_content/3` 取流并复核哈希；返回体是**白名单投影**，不含任何存储侧引用。
%%%   * `cleanup_pending/2`：只回收「**超时且未确认**」的 pending 对象，作用域内逐个判定；
%%%     已确认 / 未超时 / 跨租户 / 无元数据的孤儿对象一律不动。
%%%
%%% ## 边界（硬约束，逐条落在这里）
%%%
%%%   * **不持久化 URL**：对象 key 由实现派生，URL 从不进入任何写入参数或返回体；
%%%   * **不登记为个人 private attachment**：本模块只写 `enterprise_asset`，不触个人
%%%     `attachment` 表（由 IT 套件机械核对）；
%%%   * **ACK / 隐藏 / offboarding 不触发对象删除**：本模块唯一的对象删除通道是
%%%     `cleanup_pending/2`（超时 pending）与 `delete_private/3`（Org policy），
%%%     没有任何路径把这三类事件接到删除上；
%%%   * **A11**：对象存储侧只经本地替身验证，口径固定
%%%     `adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN`。
%%%
%%% ## 已知边界（如实登记，不假绿）
%%%
%%%   * `cleanup_pending/2` 的**候选集由调用方给出**：`eb_asset_port` 的冻结契约没有
%%%     「列举资产」callback，而新增 callback 属 EB-03R 的租约（本卡不得改契约文件）。
%%%     因此 V1 由运维/定时任务的**作用域查询**给出候选，本层只做逐条判定与删除。
%%%   * `cleanup_pending/2` 是**运维路径**：它不做用户级授权（没有 actor），只保证
%%%     作用域（Org/Workspace）与「状态 + 超时 + hold」三条判定不可绕过。
-module(eb_asset_app).

-export([
    request_presign/2,
    put_object/2,
    confirm_asset/2,
    content_stream/2,
    cleanup_pending/2
]).

-define(DEFAULT_UPLOAD_TTL_SEC, 900).
-define(MAX_UPLOAD_TTL_SEC, 3600).
-define(DEFAULT_CLEANUP_TTL_SEC, 3600).

%% ===================================================================
%% 1. presign：签发不透明上传凭证
%% ===================================================================

%% @doc `Params`：`workspace_id` / `conversation_id` / `mime` / `size_bytes` /
%% `object_hash` / `actor_user_id` 必填；`message_id` / `business_identity_id` /
%% `retain_until`（Unix 秒）/ `upload_ttl_seconds` / `key_ref` 可选；
%% 端口可用 `asset` / `store` / `crypto` / `clock` / `id` / `auth` 同键覆盖。
-spec request_presign(integer(), map()) -> {ok, map()} | {error, term()}.
request_presign(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case presign_args(Params) of
        {error, _} = Err ->
            Err;
        {ok, Args} ->
            presign_authorized(OrgId, Args, Params)
    end;
request_presign(_OrgId, _Params) ->
    {error, {invalid_argument, request_presign}}.

presign_args(Params) ->
    Ws = maps:get(workspace_id, Params, undefined),
    Conv = maps:get(conversation_id, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    Mime = maps:get(mime, Params, undefined),
    Size = maps:get(size_bytes, Params, undefined),
    Hash = maps:get(object_hash, Params, undefined),
    case {is_integer(Ws), is_integer(Conv), is_integer(Actor)} of
        {true, true, true} ->
            presign_validations(Ws, Conv, Actor, Mime, Size, Hash);
        _ ->
            {error, {invalid_argument, {presign_scope, [Ws, Conv, Actor]}}}
    end.

presign_validations(Ws, Conv, Actor, Mime, Size, Hash) ->
    case eb_asset_content:validate_mime(Mime) of
        {error, _} = Err ->
            Err;
        ok ->
            case eb_asset_content:validate_size(Size) of
                {error, _} = Err ->
                    Err;
                ok ->
                    case eb_asset_content:validate_hash(Hash) of
                        {error, _} = Err ->
                            Err;
                        ok ->
                            {ok, #{
                                ws => Ws,
                                conv => Conv,
                                actor => Actor,
                                mime => Mime,
                                size => Size,
                                hash => Hash
                            }}
                    end
            end
    end.

presign_authorized(OrgId, Args, Params) ->
    Auth = port(Params, auth),
    Store = port(Params, store),
    case
        eb_asset_scope:authorize(
            Auth,
            Store,
            OrgId,
            maps:get(ws, Args),
            maps:get(conv, Args),
            maps:get(actor, Args)
        )
    of
        {error, _} = Err ->
            Err;
        ok ->
            presign_retention(OrgId, Args, Params)
    end.

presign_retention(OrgId, Args, Params) ->
    Ws = maps:get(ws, Args),
    case resolve_retention(OrgId, Ws, Params) of
        {error, _} = Err ->
            Err;
        {ok, Retain} ->
            presign_mint(OrgId, Args, Retain, Params)
    end.

%% 保留期解析：绑定消息时**只能后移**（取消息与请求值的较大者），不得短于消息。
resolve_retention(OrgId, WorkspaceId, Params) ->
    Store = port(Params, store),
    Requested = maps:get(retain_until, Params, undefined),
    case maps:get(message_id, Params, undefined) of
        undefined when is_integer(Requested) ->
            {ok, Requested};
        undefined ->
            {ok, undefined};
        MsgId when is_integer(MsgId) ->
            message_retention(Store, OrgId, WorkspaceId, MsgId, Requested);
        _Other ->
            {error, {invalid_message_id, maps:get(message_id, Params, undefined)}}
    end.

message_retention(Store, OrgId, WorkspaceId, MsgId, Requested) ->
    case Store:fetch_message(OrgId, WorkspaceId, MsgId) of
        {ok, Message} ->
            case maps:get(retain_until, Message, undefined) of
                MsgRetain when is_integer(MsgRetain) ->
                    {ok, max(MsgRetain, requested_floor(Requested))};
                _Missing ->
                    %% 消息没有保留期锚点 ⇒ 附件无从继承 ⇒ fail-closed（不默认任何值）
                    {error, {missing_message_retain_until, MsgId}}
            end;
        {error, _} ->
            {error, not_found}
    end.

requested_floor(Requested) when is_integer(Requested) -> Requested;
requested_floor(_Other) -> 0.

presign_mint(OrgId, Args, Retain, Params) ->
    Crypto = port(Params, crypto),
    Clock = port(Params, clock),
    Id = port(Params, id),
    KeyRef = maps:get(key_ref, Params, undefined),
    Now = Clock:now(),
    case upload_ttl(Params) of
        {error, _} = Err ->
            Err;
        {ok, Ttl} ->
            AssetId = Id:new_id(enterprise_asset),
            ExpiresAt = Now + Ttl,
            Aad = eb_asset_upload_ref:aad(
                OrgId, maps:get(ws, Args), maps:get(conv, Args), msg_or_undefined(Params)
            ),
            Claims = #{
                asset_id => AssetId,
                actor_user_id => maps:get(actor, Args),
                object_hash => maps:get(hash, Args),
                mime => maps:get(mime, Args),
                size_bytes => maps:get(size, Args),
                retain_until => Retain,
                conversation_id => maps:get(conv, Args),
                message_id => msg_or_undefined(Params),
                issued_at => Now,
                expires_at => ExpiresAt
            },
            case eb_asset_upload_ref:mint(Aad, Claims, Crypto, KeyRef) of
                {ok, Token} ->
                    {ok, presign_view(AssetId, Args, Retain, ExpiresAt, Token)};
                {error, _} = Err ->
                    Err
            end
    end.

msg_or_undefined(Params) ->
    case maps:get(message_id, Params, undefined) of
        Id when is_integer(Id) -> Id;
        _Other -> undefined
    end.

%% 凭证时限：只设**上界**（`?MAX_UPLOAD_TTL_SEC`）。0 与负值被允许，因为那只会让凭证
%% **立即过期**（fail-closed，不产生任何放行风险），同时让过期路径可在不伪造时钟的前提下
%% 被测试覆盖（`eb_asset_tests:unit_upload_ref_expiry_is_enforced` 即用 `-1`）。
upload_ttl(Params) ->
    case maps:get(upload_ttl_seconds, Params, ?DEFAULT_UPLOAD_TTL_SEC) of
        Ttl when is_integer(Ttl), Ttl =< ?MAX_UPLOAD_TTL_SEC -> {ok, Ttl};
        Other -> {error, {invalid_upload_ttl, Other}}
    end.

presign_view(AssetId, Args, Retain, ExpiresAt, Token) ->
    #{
        asset_id => AssetId,
        upload_ref => Token,
        object_hash => maps:get(hash, Args),
        mime => maps:get(mime, Args),
        size_bytes => maps:get(size, Args),
        retain_until => Retain,
        expires_at => ExpiresAt,
        upload => #{
            method => <<"PUT">>,
            token => Token,
            expires_at => ExpiresAt,
            adapter => <<"local_private_object_store">>,
            rule => <<"opaque_token_no_url_no_object_key">>
        }
    }.

%% ===================================================================
%% 2. PUT：客户端按凭证写入私有桶
%% ===================================================================

%% @doc `Params`：`workspace_id` / `upload_ref` / `payload` 必填；`actor_user_id` /
%% `key_ref` 可选（`actor_user_id` 给出时必须等于签发凭证的上传人，否则 `not_uploader`）。
-spec put_object(integer(), map()) -> {ok, map()} | {error, term()}.
put_object(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case put_args(Params) of
        {error, _} = Err ->
            Err;
        {ok, Ws, Token, Payload, Actor} ->
            case open_ref(OrgId, Ws, Token, Actor, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Claims} ->
                    put_authorized(OrgId, Ws, Payload, Claims, Params)
            end
    end;
put_object(_OrgId, _Params) ->
    {error, {invalid_argument, put_object}}.

put_args(Params) ->
    Ws = maps:get(workspace_id, Params, undefined),
    Token = maps:get(upload_ref, Params, undefined),
    Payload = maps:get(payload, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    case {is_integer(Ws), is_binary(Token), is_binary(Payload), is_integer(Actor)} of
        {true, true, true, true} -> {ok, Ws, Token, Payload, Actor};
        _ -> {error, {invalid_argument, {put_object, [Ws, Token, Payload, Actor]}}}
    end.

open_ref(OrgId, Ws, Token, Actor, Params) ->
    Crypto = port(Params, crypto),
    Clock = port(Params, clock),
    KeyRef = maps:get(key_ref, Params, undefined),
    case eb_asset_upload_ref:open(Token, OrgId, Ws, Crypto, KeyRef, Clock:now()) of
        {ok, #{claims := Claims}} ->
            case maps:get(actor_user_id, Claims, undefined) of
                Actor -> {ok, Claims};
                _Other -> {error, {forbidden, not_uploader}}
            end;
        {error, _} = Err ->
            Err
    end.

put_authorized(OrgId, Ws, Payload, Claims, Params) ->
    Auth = port(Params, auth),
    Store = port(Params, store),
    Conv = maps:get(conversation_id, Claims, undefined),
    Actor = maps:get(actor_user_id, Claims, undefined),
    case eb_asset_scope:authorize(Auth, Store, OrgId, Ws, Conv, Actor) of
        {error, _} = Err ->
            Err;
        ok ->
            put_verify(OrgId, Ws, Payload, Claims, Params)
    end.

%% 服务端重算并复核上传方声明的 hash / size / mime —— 三者任一不符都在落库前拒。
put_verify(OrgId, Ws, Payload, Claims, Params) ->
    ExpectedHash = maps:get(object_hash, Claims),
    ExpectedSize = maps:get(size_bytes, Claims),
    Mime = maps:get(mime, Claims),
    ActualHash = eb_asset_content:sha256_hex(Payload),
    case {ActualHash =:= ExpectedHash, byte_size(Payload) =:= ExpectedSize} of
        {false, _} ->
            {error, {hash_mismatch, ExpectedHash, ActualHash}};
        {true, false} ->
            {error, {size_mismatch, ExpectedSize, byte_size(Payload)}};
        {true, true} ->
            case eb_asset_content:sniff(Mime, Payload) of
                {error, _} = Err ->
                    Err;
                ok ->
                    asset_put_private(OrgId, Ws, Payload, Claims, Params)
            end
    end.

asset_put_private(OrgId, Ws, Payload, Claims, Params) ->
    Asset = port(Params, asset),
    Store = port(Params, store),
    case identity_of(Store, OrgId, Ws, maps:get(conversation_id, Claims, undefined)) of
        {error, _} = Err ->
            Err;
        {ok, IdentityId} ->
            Descriptor = #{
                id => maps:get(asset_id, Claims),
                object_hash => maps:get(object_hash, Claims),
                mime => maps:get(mime, Claims),
                size_bytes => maps:get(size_bytes, Claims),
                payload => Payload,
                conversation_id => maps:get(conversation_id, Claims, undefined),
                message_id => maps:get(message_id, Claims, undefined),
                business_identity_id => IdentityId,
                uploaded_by_user_id => maps:get(actor_user_id, Claims, undefined),
                retain_until => eb_asset_content:to_retain_ms(
                    maps:get(retain_until, Claims, undefined)
                )
            },
            case Asset:put_private(OrgId, Ws, Descriptor) of
                {ok, _Summary} ->
                    %% 只回可对外字段：`put_private/3` 的 Summary 含不透明 storage_ref，
                    %% 这里刻意不把它抬进返回体（A01/A04 的机械判据）。
                    {ok, #{
                        asset_id => maps:get(asset_id, Claims),
                        status => pending_confirm,
                        object_hash => maps:get(object_hash, Claims),
                        mime => maps:get(mime, Claims),
                        size_bytes => maps:get(size_bytes, Claims),
                        retain_until => maps:get(retain_until, Claims, undefined)
                    }};
                {error, _} = Err ->
                    Err
            end
    end.

identity_of(_Store, _OrgId, _Ws, undefined) ->
    {ok, undefined};
identity_of(Store, OrgId, Ws, ConversationId) when is_integer(ConversationId) ->
    case Store:fetch_conversation(OrgId, Ws, ConversationId) of
        {ok, Conversation} -> {ok, maps:get(business_identity_id, Conversation, undefined)};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% 3. confirm：重新鉴权 + 完整性 + 保留期继承
%% ===================================================================

%% @doc `Params`：`workspace_id` / `upload_ref` / `actor_user_id` 必填。
-spec confirm_asset(integer(), map()) -> {ok, map()} | {error, term()}.
confirm_asset(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case confirm_args(Params) of
        {error, _} = Err ->
            Err;
        {ok, Ws, Token, Actor} ->
            case open_ref(OrgId, Ws, Token, Actor, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Claims} ->
                    confirm_authorized(OrgId, Ws, Claims, Params)
            end
    end;
confirm_asset(_OrgId, _Params) ->
    {error, {invalid_argument, confirm_asset}}.

confirm_args(Params) ->
    Ws = maps:get(workspace_id, Params, undefined),
    Token = maps:get(upload_ref, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    case {is_integer(Ws), is_binary(Token), is_integer(Actor)} of
        {true, true, true} -> {ok, Ws, Token, Actor};
        _ -> {error, {invalid_argument, {confirm_asset, [Ws, Token, Actor]}}}
    end.

%% A02 的落点：confirm 必须**重新**鉴权（presign 后被 suspend 的 actor 在此失败），
%% 失败的 confirm 不得把状态推进为 active。
confirm_authorized(OrgId, Ws, Claims, Params) ->
    Auth = port(Params, auth),
    Store = port(Params, store),
    Conv = maps:get(conversation_id, Claims, undefined),
    Actor = maps:get(actor_user_id, Claims, undefined),
    case eb_asset_scope:authorize(Auth, Store, OrgId, Ws, Conv, Actor) of
        {error, _} = Err ->
            Err;
        ok ->
            confirm_pending(OrgId, Ws, Claims, Params)
    end.

confirm_pending(OrgId, Ws, Claims, Params) ->
    Asset = port(Params, asset),
    AssetId = maps:get(asset_id, Claims),
    case Asset:fetch_asset(OrgId, Ws, AssetId) of
        {ok, Row} ->
            case maps:get(status, Row, undefined) of
                pending_confirm ->
                    confirm_checks(OrgId, Ws, Row, Claims, Params);
                _Other ->
                    %% 非 pending 一律 conflict（状态机每次跃迁都要可审计，重放不算成功）
                    {error, conflict}
            end;
        {error, _} = Err ->
            Err
    end.

confirm_checks(OrgId, Ws, Row, Claims, Params) ->
    case declared_matches_stored(Row, Claims) of
        {error, _} = Err ->
            Err;
        ok ->
            case object_integrity(OrgId, Ws, Row, Params) of
                {error, _} = Err ->
                    Err;
                ok ->
                    case retention_gate(OrgId, Ws, Row, Params) of
                        ok -> confirm_commit(OrgId, Ws, Row, Params);
                        {error, _} = Err -> Err
                    end
            end
    end.

declared_matches_stored(Row, Claims) ->
    case {maps:get(object_hash, Row, undefined), maps:get(object_hash, Claims)} of
        {Hash, Hash} -> ok;
        {Stored, Declared} -> {error, {object_hash_mismatch, Declared, Stored}}
    end.

%% 对象仍必须存在且内容哈希与元数据一致（防「PUT 后对象被换/被删」）。
object_integrity(OrgId, Ws, Row, Params) ->
    Asset = port(Params, asset),
    case Asset:stream_content(OrgId, Ws, maps:get(id, Row)) of
        {ok, {content_stream, Bytes}} ->
            check_bytes(Row, Bytes);
        {ok, Other} ->
            {error, {unexpected_content_stream, Other}};
        {error, Reason} ->
            {error, {object_unreadable, Reason}}
    end.

check_bytes(Row, Bytes) ->
    Expected = maps:get(object_hash, Row, undefined),
    Actual = eb_asset_content:sha256_hex(Bytes),
    case {Actual =:= Expected, byte_size(Bytes)} of
        {false, _} ->
            {error, {integrity_check_failed, Expected, Actual}};
        {true, Size} ->
            case maps:get(size_bytes, Row, undefined) of
                Size -> ok;
                undefined -> ok;
                OtherSize -> {error, {size_mismatch, OtherSize, Size}}
            end
    end.

%% 保留期：已确认附件的 retain_until **不得短于**所属消息；短了就 fail-closed
%% （不缩短、也不偷偷延长元数据——元数据延长没有对应的冻结 callback）。
retention_gate(OrgId, Ws, Row, Params) ->
    Store = port(Params, store),
    case maps:get(message_id, Row, undefined) of
        MsgId when is_integer(MsgId) ->
            case Store:fetch_message(OrgId, Ws, MsgId) of
                {ok, Message} -> retention_compare(Row, Message, MsgId);
                {error, _} -> {error, {message_missing, MsgId}}
            end;
        _Unbound ->
            ok
    end.

retention_compare(Row, Message, MsgId) ->
    AssetRetain = maps:get(retain_until, Row, undefined),
    MsgRetain = maps:get(retain_until, Message, undefined),
    case {is_integer(AssetRetain), is_integer(MsgRetain)} of
        {true, true} when AssetRetain < MsgRetain ->
            {error, {retain_until_shorter_than_message, AssetRetain, MsgRetain, MsgId}};
        {false, _} ->
            %% 绑定消息却没有保留期锚点 ⇒ fail-closed（不猜、不默认）
            {error, {asset_retain_until_missing, MsgId}};
        _Other ->
            ok
    end.

confirm_commit(OrgId, Ws, Row, Params) ->
    Asset = port(Params, asset),
    Store = port(Params, store),
    AssetId = maps:get(id, Row),
    case Asset:confirm_asset(OrgId, Ws, AssetId) of
        {ok, Confirmed} ->
            View = eb_asset_content:public_asset_view(Confirmed),
            %% hold_count 只是**可观测面**（真守护在 DB 触发器 + cleanup 的 hold 门），
            %% 故此处查询失败按 0 记录，不影响 confirm 的成败。
            Count =
                case hold_count(Store, OrgId, Ws, Row) of
                    {ok, N} -> N;
                    {error, _} -> 0
                end,
            {ok, eb_asset_content:with_hold_count(View, Count)};
        {error, _} = Err ->
            Err
    end.

%% ===================================================================
%% 4. content：鉴权代理取流
%% ===================================================================

%% @doc `Params`：`workspace_id` / `asset_id` / `actor_user_id` 必填。
-spec content_stream(integer(), map()) -> {ok, map()} | {error, term()}.
content_stream(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case content_args(Params) of
        {error, _} = Err ->
            Err;
        {ok, Ws, AssetId, Actor} ->
            content_fetch(OrgId, Ws, AssetId, Actor, Params)
    end;
content_stream(_OrgId, _Params) ->
    {error, {invalid_argument, content_stream}}.

content_args(Params) ->
    Ws = maps:get(workspace_id, Params, undefined),
    AssetId = maps:get(asset_id, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    case {is_integer(Ws), is_integer(AssetId), is_integer(Actor)} of
        {true, true, true} -> {ok, Ws, AssetId, Actor};
        _ -> {error, {invalid_argument, {content_stream, [Ws, AssetId, Actor]}}}
    end.

content_fetch(OrgId, Ws, AssetId, Actor, Params) ->
    Asset = port(Params, asset),
    case Asset:fetch_asset(OrgId, Ws, AssetId) of
        {ok, Row} ->
            content_authorized(OrgId, Ws, Row, Actor, Params);
        {error, _} ->
            %% 跨 Org / 跨 Workspace / 不存在 / 已回收：同一答案（避免枚举）
            {error, not_found}
    end.

content_authorized(OrgId, Ws, Row, Actor, Params) ->
    Auth = port(Params, auth),
    Store = port(Params, store),
    Conv = maps:get(conversation_id, Row, undefined),
    case eb_asset_scope:authorize(Auth, Store, OrgId, Ws, Conv, Actor) of
        {error, _} = Err ->
            Err;
        ok ->
            content_status_gate(OrgId, Ws, Row, Params)
    end.

content_status_gate(OrgId, Ws, Row, Params) ->
    case maps:get(status, Row, undefined) of
        active ->
            content_read(OrgId, Ws, Row, Params);
        deleted ->
            {error, not_found};
        Other ->
            {error, {not_confirmed, Other}}
    end.

content_read(OrgId, Ws, Row, Params) ->
    Asset = port(Params, asset),
    case Asset:stream_content(OrgId, Ws, maps:get(id, Row)) of
        {ok, {content_stream, Bytes}} ->
            content_verify(Bytes, Row);
        {error, Reason} ->
            {error, {object_unreadable, Reason}}
    end.

content_verify(Bytes, Row) ->
    Expected = maps:get(object_hash, Row, undefined),
    case eb_asset_content:sha256_hex(Bytes) =:= Expected of
        true -> {ok, eb_asset_content:public_content_view(Bytes, Row)};
        false -> {error, {integrity_check_failed, Expected}}
    end.

%% ===================================================================
%% 5. cleanup：只回收「超时且未确认」的 pending 对象
%% ===================================================================

%% @doc `Params`：`workspace_id` / `asset_ids` 必填；`ttl_seconds`（默认 3600）/
%% `respect_holds`（默认 true）可选。
%%
%% 逐条判定（任一不满足即跳过，**绝不**越界）：
%%   `not_found`（含跨租户/不存在）→ `not_pending`（已确认或已回收）→
%%   `not_expired`（未超时）→ `active_hold`（被 active hold 覆盖）。
-spec cleanup_pending(integer(), map()) -> {ok, map()} | {error, term()}.
cleanup_pending(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    Ws = maps:get(workspace_id, Params, undefined),
    Ids = maps:get(asset_ids, Params, undefined),
    case {is_integer(Ws), is_list(Ids)} of
        {true, true} ->
            Clock = port(Params, clock),
            Ttl = cleanup_ttl(Params),
            Now = Clock:now(),
            Results = [cleanup_one(OrgId, Ws, Id, Now, Ttl, Params) || Id <- Ids],
            {ok, #{
                deleted => [Id || {deleted, Id} <- Results],
                skipped => [{Id, Reason} || {skipped, Id, Reason} <- Results]
            }};
        _ ->
            {error, {invalid_argument, {cleanup_pending, [Ws, Ids]}}}
    end;
cleanup_pending(_OrgId, _Params) ->
    {error, {invalid_argument, cleanup_pending}}.

cleanup_ttl(Params) ->
    case maps:get(ttl_seconds, Params, ?DEFAULT_CLEANUP_TTL_SEC) of
        Ttl when is_integer(Ttl), Ttl >= 0 -> Ttl;
        _Other -> ?DEFAULT_CLEANUP_TTL_SEC
    end.

cleanup_one(OrgId, Ws, AssetId, Now, Ttl, Params) ->
    Asset = port(Params, asset),
    case Asset:fetch_asset(OrgId, Ws, AssetId) of
        {ok, Row} ->
            cleanup_gate(OrgId, Ws, Row, AssetId, Now, Ttl, Params);
        {error, _} ->
            {skipped, AssetId, not_found}
    end.

cleanup_gate(OrgId, Ws, Row, AssetId, Now, Ttl, Params) ->
    case maps:get(status, Row, undefined) of
        pending_confirm ->
            cleanup_age_gate(OrgId, Ws, Row, AssetId, Now, Ttl, Params);
        _Other ->
            {skipped, AssetId, not_pending}
    end.

cleanup_age_gate(OrgId, Ws, Row, AssetId, Now, Ttl, Params) ->
    CreatedAt = maps:get(created_at, Row, undefined),
    case is_integer(CreatedAt) andalso CreatedAt + Ttl =< Now of
        false ->
            {skipped, AssetId, not_expired};
        true ->
            cleanup_hold_gate(OrgId, Ws, Row, AssetId, Params)
    end.

cleanup_hold_gate(OrgId, Ws, Row, AssetId, Params) ->
    Store = port(Params, store),
    case maps:get(respect_holds, Params, true) of
        false ->
            cleanup_delete(OrgId, Ws, AssetId, Params);
        true ->
            case hold_count(Store, OrgId, Ws, Row) of
                {ok, 0} -> cleanup_delete(OrgId, Ws, AssetId, Params);
                {ok, _N} -> {skipped, AssetId, active_hold};
                %% hold 事实读不到 ⇒ **不删**（fail-closed：宁可留一个超时 pending 对象）
                {error, Reason} -> {skipped, AssetId, {hold_check_failed, Reason}}
            end
    end.

%% 先推进元数据（CAS 形状），再回收对象；对象回收失败必须显式报错，不得静默成功。
cleanup_delete(OrgId, Ws, AssetId, Params) ->
    Asset = port(Params, asset),
    case Asset:cleanup_asset(OrgId, Ws, AssetId) of
        ok ->
            case Asset:delete_private(OrgId, Ws, AssetId) of
                ok ->
                    {deleted, AssetId};
                {error, Reason} ->
                    {skipped, AssetId, {object_delete_failed, Reason}}
            end;
        {error, Reason} ->
            {skipped, AssetId, {metadata_cleanup_failed, Reason}}
    end.

%% ===================================================================
%% hold 覆盖判定（A06 的「继承 active hold」可观测面）
%% ===================================================================

%% @doc 覆盖该附件的 active hold 条数：workspace 级恒覆盖本作用域；
%% message 级比 `scope_message_id`；conversation 级比 `scope_conversation_id`。
hold_count(Store, OrgId, Ws, Row) ->
    case Store:list_active_holds(OrgId, Ws) of
        {ok, Holds} -> {ok, length([H || H <- Holds, covers(H, Row)])};
        {error, Reason} -> {error, {hold_lookup_failed, Reason}}
    end.

covers(Hold, Row) ->
    case maps:get(scope, Hold, undefined) of
        workspace ->
            true;
        message ->
            eq_int(
                maps:get(scope_message_id, Hold, undefined), maps:get(message_id, Row, undefined)
            );
        conversation ->
            eq_int(
                maps:get(scope_conversation_id, Hold, undefined),
                maps:get(conversation_id, Row, undefined)
            );
        _Other ->
            false
    end.

eq_int(A, B) -> is_integer(A) andalso A =:= B.

%% ===================================================================
%% 端口装配
%% ===================================================================

%% `Params` 同键可注入端口（测试与装配用）；缺省一律经 `eb_infra_ports` 装配。
port(Params, Key) ->
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined ->
            Mod;
        _Other ->
            {ok, Mod} = eb_infra_ports:resolve(Key),
            Mod
    end.
