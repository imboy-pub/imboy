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

-moduledoc "企业附件闭环用例层（EB-07）—— facade 的唯一委派目标。".
-export([
    request_presign/2,
    confirm_asset/2,
    content_stream/2,
    edge_authorize_content/1,
    cleanup_pending/2,
    session_status/2,
    session_renew/2
]).

-define(DEFAULT_UPLOAD_TTL_SEC, 900).
-define(MAX_UPLOAD_TTL_SEC, 3600).
%% V5 会话绝对 deadline（02 §143：登记 DB 时间 + 2h，不可延长）。
-define(V5_EB_SESSION_DEADLINE_SEC, 7200).
-define(DEFAULT_CLEANUP_TTL_SEC, 3600).

%% ===================================================================
%% 1b. direct 会话协议（V3-C04）：status 只读回查、renew 换新 attempt
%% ===================================================================

%% @doc 会话回查（direct-v1）：Params 需 `workspace_id` + `object_key`
%% + actor（user/contact 二选一）。归属逐字比对（跨主体/跨空间
%% not_found，C04-02）；只读，响应不含任何写能力。
session_status(OrgId, Params) ->
    session_direct_op(OrgId, Params, fun(Key, Owner) ->
        {ok, direct_status_view(Key, Owner)}
    end).

%% @doc 续期（V3-C04）：GRANTED/FINALIZING → renew_grant（新 attempt +
%% 新 staging descriptor，expected sha 复用冻结声明）；CONFIRMED 幂等
%% 终态投影（无新 attempt、无 presign）。
session_renew(OrgId, Params) ->
    session_direct_op(OrgId, Params, fun(Key, Owner) ->
        case eb_asset_direct:direct_renew(Key) of
            {ok, #{grant := Grant} = Info} ->
                {ok, (direct_status_view(Key, Owner))#{
                    attempt_id => maps:get(attempt_id, Info),
                    upload => #{
                        method => maps:get(method, Grant),
                        transport => maps:get(transport, Grant),
                        url => maps:get(url, Grant),
                        required_headers =>
                            required_headers_js(maps:get(required_headers, Grant)),
                        expires_in => maps:get(expires_in, Grant)
                    }
                }};
            {ok, Info} ->
                %% CONFIRMED 幂等终态（无 grant 键）
                {ok, (direct_status_view(Key, Owner))#{
                    state_confirmed => true,
                    info => Info
                }};
            {error, _} = Err ->
                Err
        end
    end).

%% direct 会话协议用例共用门：actor 二选一 + object_key 定位 + 归属逐字
%% 比对（跨主体/跨空间 not_found——C04-02）。
session_direct_op(OrgId, Params, Fun) ->
    case actor_scope(Params) of
        {error, _} = Err ->
            Err;
        {ok, Actor} ->
            Ws = maps:get(workspace_id, Params, undefined),
            Key = maps:get(object_key, Params, undefined),
            case {is_integer(Ws), is_binary(Key)} of
                {true, true} ->
                    case eb_asset_direct:session_owner(Key) of
                        {ok, Owner} ->
                            case owner_matches(Owner, OrgId, Ws, Actor) of
                                ok -> Fun(Key, Owner);
                                {error, not_found} -> {error, not_found};
                                {error, _} = Err2 -> Err2
                            end;
                        {error, _} ->
                            {error, not_found}
                    end;
                _ ->
                    {error, {invalid_argument, session_identity}}
            end
    end.

%% direct 会话只读投影（status/renew 共用；不含任何写能力）。
direct_status_view(Key, Owner) ->
    State = maps:get(direct_state, Owner),
    #{
        object_key => Key,
        session_state => direct_session_state(State),
        direct_state => State,
        session_deadline => maps:get(session_deadline, Owner, undefined),
        object_hash => maps:get(expected_sha, Owner, undefined),
        mime => maps:get(mime, Owner),
        file_name => maps:get(file_name, Owner, undefined),
        can_resume => (State =:= <<"GRANTED">>) orelse (State =:= <<"FINALIZING">>),
        retry_action =>
            case State of
                <<"GRANTED">> -> <<"resume_same_object_key">>;
                <<"FINALIZING">> -> <<"retry_confirm">>;
                <<"CONFIRMED">> -> <<"retry_confirm">>;
                _ -> <<"none">>
            end
    }.

direct_session_state(<<"CONFIRMED">>) -> <<"confirmed">>;
direct_session_state(<<"FAILED">>) -> <<"rejected">>;
direct_session_state(_) -> <<"registered">>.

registration_subject(Org, Actor, Params) ->
    Base = #{
        <<"kind">> =>
            case maps:get(kind, Actor) of
                contact -> <<"visitor_session">>;
                member -> <<"seat">>
            end,
        <<"principal">> => integer_to_binary(maps:get(id, Actor)),
        <<"org_id">> => integer_to_binary(Org)
    },
    case maps:get(kind, Actor) of
        contact -> Base#{<<"session_id">> => maps:get(visitor_session_id, Params, undefined)};
        member -> Base
    end.

%% ===================================================================
%% 1. presign：签发 s3_put 直传 descriptor（V3-C04）
%% ===================================================================

%% @doc `Params`：`workspace_id` / `conversation_id` / `mime` / `size_bytes` /
%% `object_hash` / `actor_user_id` 必填；`message_id` / `business_identity_id` /
%% `file_name`（CS-BE-01：展示文件名，可选）/ `retain_until`（Unix 秒）/
%% `upload_ttl_seconds` / `key_ref` 可选；
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
    Mime = maps:get(mime, Params, undefined),
    Size = maps:get(size_bytes, Params, undefined),
    Hash = maps:get(object_hash, Params, undefined),
    FileName = maps:get(file_name, Params, undefined),
    case actor_scope(Params) of
        {error, _} = Err ->
            Err;
        {ok, Actor} ->
            case {is_integer(Ws), is_integer(Conv)} of
                {true, true} ->
                    presign_validations(Ws, Conv, Actor, Mime, Size, Hash, FileName);
                _ ->
                    {error, {invalid_argument, {presign_scope, [Ws, Conv]}}}
            end
    end.

%% CSB-02S D6：actor 二选一（主体唯一，不混淆）——
%%   * `actor_user_id`    给出 ⇒ 成员主体（企业面既有口径，走 member+assignee 门）；
%%   * `actor_contact_id` 给出 ⇒ 访客主体（visit token contact，走会话归属门）。
%% 两者皆缺/同时给出 = 作用域形状不成立。
actor_scope(Params) ->
    U = maps:get(actor_user_id, Params, undefined),
    C = maps:get(actor_contact_id, Params, undefined),
    case true of
        _ when is_integer(U), not is_integer(C) ->
            {ok, #{kind => member, id => U}};
        _ when is_integer(C), not is_integer(U) ->
            {ok, #{kind => contact, id => C}};
        _ ->
            {error, {invalid_argument, {presign_scope, [U, C]}}}
    end.

presign_validations(Ws, Conv, Actor, Mime, Size, Hash, FileName) ->
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
                            file_name_arg(Ws, Conv, Actor, Mime, Size, Hash, FileName)
                    end
            end
    end.

%% CS-BE-01：file_name 是可选展示声明——非法值 fail-closed（不静默丢弃，
%% 否则历史投影会出现「上传了却永远无文件名」的半截契约）。
file_name_arg(Ws, Conv, Actor, Mime, Size, Hash, FileName) ->
    case eb_asset_content:validate_file_name(FileName) of
        {error, _} = Err ->
            Err;
        ok ->
            {ok, #{
                ws => Ws,
                conv => Conv,
                actor => Actor,
                mime => Mime,
                size => Size,
                hash => Hash,
                file_name => FileName
            }}
    end.

presign_authorized(OrgId, Args, Params) ->
    Auth = port(Params, auth),
    Store = port(Params, store),
    case presign_scope_check(Auth, Store, OrgId, Args) of
        {error, _} = Err ->
            Err;
        ok ->
            presign_retention(OrgId, Args, Params)
    end.

%% CSB-02S D6：按 actor 主体种类分流授权——成员走 member+assignee 门
%% （既有口径不变）；访客走会话归属门（令牌 contact = 会话 contact）。
presign_scope_check(Auth, Store, OrgId, Args) ->
    case maps:get(kind, maps:get(actor, Args), member) of
        member ->
            eb_asset_scope:authorize(
                Auth,
                Store,
                OrgId,
                maps:get(ws, Args),
                maps:get(conv, Args),
                maps:get(id, maps:get(actor, Args))
            );
        contact ->
            eb_asset_scope:authorize_contact(
                Store,
                OrgId,
                maps:get(ws, Args),
                maps:get(conv, Args),
                maps:get(id, maps:get(actor, Args))
            )
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
    %% V3-C04：正文切 direct-v1（S3-only 硬合同）——
    %% register(fn_dv1_register_eb) → declare → grant 一站式签发 s3_put
    %% staging descriptor。控制门不变（scope/retention/actor 校验已在前置
    %% 步骤通过）；BEAM 零正文，api_byte/upload_ref 正文链一并移除。
    Actor = maps:get(actor, Args),
    Decl = #{
        org_id => OrgId,
        workspace_id => maps:get(ws, Args),
        conversation_id => maps:get(conv, Args),
        message_id => msg_or_undefined(Params),
        client_upload_id => maps:get(client_upload_id, Params, undefined),
        mime => maps:get(mime, Args),
        size_bytes => maps:get(size, Args),
        sha256_hex => maps:get(hash, Args),
        file_name => maps:get(file_name, Args, undefined)
    },
    case
        eb_asset_direct:direct_presign(
            maps:get(kind, Actor), maps:get(id, Actor), Decl
        )
    of
        {ok, Info} ->
            {ok, presign_direct_view(Info, Args, Retain, Params)};
        {error, _} = Err ->
            Err
    end.

msg_or_undefined(Params) ->
    case maps:get(message_id, Params, undefined) of
        Id when is_integer(Id) -> Id;
        _Other -> undefined
    end.

%% direct presign 成功视图（V3-C04）：s3_put descriptor + object_key
%% （direct 会话键，客户端后续 confirm 的定位标识）。不再预分 asset_id、
%% 不再签发 upload_ref（同源正文凭证链已移除）。
presign_direct_view(Info, Args, Retain, _Params) ->
    Grant = maps:get(grant, Info),
    #{
        object_key => maps:get(object_key, Info),
        session_state => maps:get(state, Info),
        attempt_id => maps:get(attempt_id, Info),
        session_deadline => maps:get(session_deadline, Info, undefined),
        object_hash => maps:get(hash, Args),
        mime => maps:get(mime, Args),
        size_bytes => maps:get(size, Args),
        file_name => maps:get(file_name, Args, undefined),
        retain_until => Retain,
        upload => #{
            method => maps:get(method, Grant),
            transport => maps:get(transport, Grant),
            url => maps:get(url, Grant),
            required_headers => required_headers_js(maps:get(required_headers, Grant)),
            expires_in => maps:get(expires_in, Grant)
        }
    }.

%% KV tuple 列表 → JSON 对象数组（jsone 不编码 tuple）
required_headers_js(Headers) when is_list(Headers) ->
    [
        #{key => K, value => V}
     || {K, V} <- Headers
    ];
required_headers_js(_) ->
    [].

%% CSB-02S D6：凭证里的 actor（member/contact 二选一）与请求方主体逐字比对。
actor_claim_value(Claims) ->
    case maps:get(actor_user_id, Claims, undefined) of
        undefined -> {contact, maps:get(actor_contact_id, Claims, undefined)};
        Id -> {member, Id}
    end.

actor_value(#{kind := Kind, id := Id}) ->
    {Kind, Id}.

%% CSB-02S D6：按凭证 actor 主体种类分流授权（member=成员+经办门；
%% contact=访客会话归属门）。
scope_check_by_claims(Auth, Store, OrgId, Ws, Conv, Claims) ->
    case actor_claim_value(Claims) of
        {member, ActorUserId} ->
            eb_asset_scope:authorize(Auth, Store, OrgId, Ws, Conv, ActorUserId);
        {contact, ActorContactId} ->
            eb_asset_scope:authorize_contact(Store, OrgId, Ws, Conv, ActorContactId)
    end.

identity_of(_Store, _OrgId, _Ws, undefined) ->
    {ok, undefined};
identity_of(Store, OrgId, Ws, ConversationId) when is_integer(ConversationId) ->
    case Store:fetch_conversation(OrgId, Ws, ConversationId) of
        {ok, Conversation} -> {ok, maps:get(business_identity_id, Conversation, undefined)};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% 3. confirm：direct-v1 finalize + Worker 校验 + active 绑定（V3-C04）
%% ===================================================================

%% @doc `Params`：`workspace_id` / `upload_ref` / `actor_user_id` 必填；
%% W2-02 恢复：`upload_ref` 可由 `asset_id` 替代（响应丢失后客户端按授权
%% asset_id 恢复 confirm，不要求持久化 upload_ref；授权以行派生 claims +
%% scope 复核为准）。
-spec confirm_asset(integer(), map()) -> {ok, map()} | {error, term()}.
confirm_asset(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    case actor_scope(Params) of
        {error, _} = Err ->
            Err;
        {ok, Actor} ->
            Ws = maps:get(workspace_id, Params, undefined),
            Key = maps:get(object_key, Params, undefined),
            AssetId = maps:get(asset_id, Params, undefined),
            case {is_integer(Ws), is_binary(Key), is_integer(AssetId)} of
                {true, true, _} ->
                    confirm_direct(OrgId, Ws, Key, Params, Actor);
                {true, undefined, true} ->
                    %% W2-02 存量恢复：V5 pending 行按 asset_id 走既有确认链
                    confirm_by_asset_id(OrgId, Ws, AssetId, Actor, Params);
                _ ->
                    {error, {invalid_argument, {confirm_asset, [Ws, Key, AssetId]}}}
            end
    end;
confirm_asset(_OrgId, _Params) ->
    {error, {invalid_argument, confirm_asset}}.

%% 门 1（C04-02）：direct 会话归属逐字比对——跨主体/跨 Org/跨 Workspace
%% 猜 object_key 一律 not_found（不泄露会话存在性）。
confirm_direct(OrgId, Ws, Key, Params, Actor) ->
    case eb_asset_direct:session_owner(Key) of
        {ok, Owner} ->
            case owner_matches(Owner, OrgId, Ws, Actor) of
                {error, _} = Err ->
                    Err;
                ok ->
                    %% 门 2（A02）：confirm **重新**鉴权——presign 后被
                    %% suspend/转接的 actor 在此失败，失败不得推进状态。
                    case confirm_scope_recheck(Params, OrgId, Ws, Owner) of
                        {error, _} = Err2 ->
                            Err2;
                        ok ->
                            confirm_direct_settle(OrgId, Ws, Key, Params, Actor, Owner)
                    end
            end;
        {error, _} ->
            {error, not_found}
    end.

owner_matches(Owner, OrgId, Ws, Actor) ->
    case
        maps:get(kind, Owner) =:= maps:get(kind, Actor) andalso
            maps:get(principal, Owner) =:= maps:get(id, Actor) andalso
            maps:get(org_id, Owner) =:= OrgId andalso
            maps:get(workspace_id, Owner) =:= Ws
    of
        true -> ok;
        false -> {error, not_found}
    end.

confirm_scope_recheck(Params, OrgId, Ws, Owner) ->
    Auth = port(Params, auth),
    Store = port(Params, store),
    Principal = maps:get(principal, Owner),
    Conv = maps:get(conversation_id, Owner),
    case maps:get(kind, Owner) of
        contact ->
            eb_asset_scope:authorize_contact(Store, OrgId, Ws, Conv, Principal);
        _ ->
            eb_asset_scope:authorize(Auth, Store, OrgId, Ws, Conv, Principal)
    end.

%% 状态分派：GRANTED/FINALIZING → finalize+scan；CONFIRMED（响应丢失恢复）
%% → 补 Worker 校验再绑定；其余（RESERVED/DECLARED/FAILED）fail-closed。
confirm_direct_settle(OrgId, Ws, Key, Params, Actor, Owner) ->
    State = maps:get(direct_state, Owner),
    Mime = maps:get(mime, Owner),
    case State of
        S when S =:= <<"GRANTED">> orelse S =:= <<"FINALIZING">> ->
            case
                eb_asset_direct:direct_finalize_scan(
                    Key, attempt_no_param(Params), Mime, 600000
                )
            of
                {ok, Scan} ->
                    confirm_bind(OrgId, Ws, Key, Params, Actor, Owner, Scan);
                {error, _} = Err ->
                    Err
            end;
        <<"CONFIRMED">> ->
            case scan_only(Key, Mime) of
                {ok, Scan} ->
                    confirm_bind(OrgId, Ws, Key, Params, Actor, Owner, Scan);
                {error, _} = Err ->
                    Err
            end;
        _ ->
            {error, conflict}
    end.

attempt_no_param(Params) ->
    max(1, elib_cnv:safe_to_integer(maps:get(attempt_id, Params, 1))).

scan_only(Key, Mime) ->
    case eb_asset_direct:receipt_secret() of
        {ok, Secret} -> eb_asset_direct:scan_confirmed(Key, Mime, Secret, 600000);
        {error, _} = Err -> Err
    end.

%% 绑定：Worker 校验通过后折叠进 enterprise_asset active（meta 层幂等：
%% 同 direct 键回读同一行——重复 confirm 不产生重复引用）。
confirm_bind(OrgId, Ws, Key, Params, Actor, Owner, Scan) ->
    Store = port(Params, store),
    Id = port(Params, id),
    Conv = maps:get(conversation_id, Owner),
    case identity_of(Store, OrgId, Ws, Conv) of
        {error, _} = Err ->
            Err;
        {ok, IdentityId} ->
            case bind_retain(OrgId, Ws, Params, Owner) of
                {error, _} = Err2 ->
                    Err2;
                RetainSec ->
                    Descriptor = #{
                        id => Id:new_id(enterprise_asset),
                        object_hash => maps:get(sha256, Scan),
                        mime => maps:get(mime, Owner),
                        size_bytes => maps:get(size, Scan),
                        conversation_id => Conv,
                        message_id => maps:get(message_id, Owner),
                        uploaded_by_user_id => uploaded_by(Actor),
                        business_identity_id => IdentityId,
                        file_name => maps:get(file_name, Owner),
                        retain_until => eb_asset_content:to_retain_ms(RetainSec),
                        attempt_no => attempt_no_param(Params)
                    },
                    case eb_asset_direct:direct_bind(OrgId, Ws, Key, Descriptor) of
                        {ok, Row} ->
                            confirm_view(Params, OrgId, Ws, Row);
                        {error, _} = Err3 ->
                            Err3
                    end
            end
    end.

%% 保留期：绑定消息时**只能后移**（resolve_retention 消息锚点继承复用）。
bind_retain(OrgId, Ws, Params, Owner) ->
    MsgId = maps:get(message_id, Owner),
    case resolve_retention(OrgId, Ws, Params#{message_id => MsgId}) of
        {error, _} = Err -> Err;
        {ok, Retain} -> Retain
    end.

confirm_view(Params, OrgId, Ws, Row) ->
    Count =
        case hold_count(port(Params, store), OrgId, Ws, Row) of
            {ok, N} -> N;
            {error, _} -> 0
        end,
    {ok, eb_asset_content:with_hold_count(eb_asset_content:public_asset_view(Row), Count)}.

uploaded_by(#{kind := member, id := Id}) -> Id;
uploaded_by(#{kind := seat, id := Id}) -> Id;
uploaded_by(_) -> undefined.

confirm_by_asset_id(OrgId, Ws, AssetId, Actor, Params) ->
    Asset = port(Params, asset),
    case Asset:fetch_asset(OrgId, Ws, AssetId) of
        {ok, Row} ->
            case row_actor_authorized(Row, Actor, Params, OrgId) of
                {error, _} = Err ->
                    Err;
                ok ->
                    Auth = port(Params, auth),
                    Store = port(Params, store),
                    Claims = claims_from_row(Row, Actor),
                    Conv = maps:get(conversation_id, Claims, undefined),
                    case scope_check_by_claims(Auth, Store, OrgId, Ws, Conv, Claims) of
                        {error, _} = Err ->
                            Err;
                        ok ->
                            confirm_dispatch_status(OrgId, Ws, Row, Claims, Params)
                    end
            end;
        {error, _} = Err ->
            Err
    end.

%% 行审计 actor 比对（open_ref 的 actor_claim_value=:=actor_value 同口径）：
%% 成员主体必须与行 uploaded_by_user_id 逐字相等；访客主体（contact）无
%% 行字段可比，交由 scope_check_by_claims 的会话归属门裁决。
row_actor_authorized(Row, Actor, Params, Org) ->
    case maps:get(upload_registration, Row, undefined) of
        undefined ->
            row_actor_authorized(Row, Actor);
        null ->
            row_actor_authorized(Row, Actor);
        Value ->
            Reg =
                case Value of
                    B when is_binary(B) -> jsone:decode(B);
                    M -> M
                end,
            case
                maps:get(<<"subject">>, Reg, undefined) =:= registration_subject(Org, Actor, Params)
            of
                true -> ok;
                false -> {error, not_found}
            end
    end.

row_actor_authorized(Row, Actor) ->
    case {actor_value(Actor), maps:get(uploaded_by_user_id, Row, undefined)} of
        {{member, Id}, RowId} when is_integer(RowId) ->
            case Id =:= RowId of
                true -> ok;
                false -> {error, {forbidden, not_uploader}}
            end;
        {{contact, _}, _} ->
            ok;
        {{member, _}, _} ->
            ok
    end.

claims_from_row(Row, Actor) ->
    Base = #{
        asset_id => maps:get(id, Row),
        object_hash => maps:get(object_hash, Row, undefined),
        size_bytes => maps:get(size_bytes, Row, undefined),
        mime => maps:get(mime_type, Row, maps:get(mime, Row, undefined)),
        conversation_id => maps:get(conversation_id, Row, undefined)
    },
    case actor_value(Actor) of
        {member, Id} -> Base#{actor_user_id => Id};
        {contact, Id} -> Base#{actor_contact_id => Id}
    end.

%% 状态分派：pending_confirm → 校验链；active → 幂等回读（重放返回同一
%% asset 视图与 hold_count，审计/跃迁只在首确认发生一次）；其余 conflict。
confirm_dispatch_status(OrgId, Ws, Row, Claims, Params) ->
    case {maps:get(lifecycle_state, Row, <<"live">>), maps:get(status, Row, undefined)} of
        {<<"live">>, pending_confirm} ->
            confirm_checks(OrgId, Ws, Row, Claims, Params);
        {<<"live">>, active} ->
            replay_view(OrgId, Ws, Row, Params);
        _Other ->
            {error, conflict}
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
    AssetId = maps:get(id, Row),
    case Asset:confirm_asset(OrgId, Ws, AssetId) of
        {ok, Confirmed} ->
            %% hold_count 只是**可观测面**（真守护在 DB 触发器 + cleanup 的 hold 门），
            %% 故此处查询失败按 0 记录，不影响 confirm 的成败。
            Count =
                case hold_count(port(Params, store), OrgId, Ws, Row) of
                    {ok, N} -> N;
                    {error, _} -> 0
                end,
            {ok,
                eb_asset_content:with_hold_count(
                    eb_asset_content:public_asset_view(Confirmed), Count
                )};
        {error, _} = Err ->
            Err
    end.

%% W2-02 幂等重放：已 active 资产返回与首确认相同的视图（同一 asset id、
%% 同一 hold_count 口径），不重复跃迁/审计。
replay_view(OrgId, Ws, Row, Params) ->
    Count =
        case hold_count(port(Params, store), OrgId, Ws, Row) of
            {ok, N} -> N;
            {error, _} -> 0
        end,
    {ok, eb_asset_content:with_hold_count(eb_asset_content:public_asset_view(Row), Count)}.

%% ===================================================================
%% 4. content：鉴权代理取流
%% ===================================================================

%% @doc `Params`：`workspace_id` / `asset_id` 必填；主体二选一：
%% `actor_user_id`（成员，走 member+assignee 门）或 `actor_contact_id`
%% （访客，走会话归属门）；`conversation_id` 可选（**绑定作用域**，服务端
%% 派生——传入时 asset 行的 conversation 必须逐字相等，且访客分支只读
%% linked（message_id 非空）资产。CS 访客内容代理（BE-S01b）用此键表达
%% 「asset 必须绑在本会话的消息上」；既有 EB 坐席调用不传，行为不变）。
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
    %% CSB-02S D6 同款 actor 二选一（主体唯一，不混淆）。
    case actor_scope(Params) of
        {error, _} = Err ->
            Err;
        {ok, Actor} ->
            case {is_integer(Ws), is_integer(AssetId)} of
                {true, true} -> {ok, Ws, AssetId, Actor};
                _ -> {error, {invalid_argument, {content_stream, [Ws, AssetId]}}}
            end
    end.

content_fetch(OrgId, Ws, AssetId, Actor, Params) ->
    Asset = port(Params, asset),
    case Asset:fetch_asset(OrgId, Ws, AssetId) of
        {ok, Row} ->
            case content_binding_gate(Row, Actor, Params) of
                ok ->
                    content_authorized(OrgId, Ws, Row, Actor, Params);
                {error, _} = Err ->
                    Err
            end;
        {error, _} ->
            %% 跨 Org / 跨 Workspace / 不存在 / 已回收：同一答案（避免枚举）
            {error, not_found}
    end.

%% 绑定作用域门（调用方显式传入 conversation_id 时生效）：
%%   * asset 行的 conversation 必须与声明值逐字相等（跨会话与不存在同为
%%     not_found——避免枚举）；
%%   * 访客主体只读 linked（message_id 非空）资产——confirmed_unbound
%%     不出访客内容面（attachment-state-machine binding_invariants）。
content_binding_gate(Row, Actor, Params) ->
    Kind = maps:get(kind, Actor),
    case maps:get(conversation_id, Params, undefined) of
        undefined ->
            ok;
        DeclaredConv ->
            case maps:get(conversation_id, Row, undefined) of
                DeclaredConv when Kind =:= contact ->
                    case maps:get(message_id, Row, undefined) of
                        MsgId when is_integer(MsgId) -> ok;
                        _Other -> {error, not_found}
                    end;
                DeclaredConv ->
                    ok;
                _Other ->
                    {error, not_found}
            end
    end.

content_authorized(OrgId, Ws, Row, Actor, Params) ->
    Auth = port(Params, auth),
    Store = port(Params, store),
    Conv = maps:get(conversation_id, Row, undefined),
    case maps:get(kind, Actor) of
        member ->
            case eb_asset_scope:authorize(Auth, Store, OrgId, Ws, Conv, maps:get(id, Actor)) of
                {error, _} = Err ->
                    Err;
                ok ->
                    content_gated_read(OrgId, Ws, Row, Params)
            end;
        contact ->
            case
                eb_asset_scope:authorize_contact(
                    Store, OrgId, Ws, Conv, maps:get(id, Actor)
                )
            of
                {error, _} = Err ->
                    Err;
                ok ->
                    content_gated_read(OrgId, Ws, Row, Params)
            end
    end.

%% ACL 通过后：status 门 → 票据签发（V3-17：止于 Row，不读字节）。
content_gated_read(OrgId, Ws, Row, Params) ->
    case content_status_gate(OrgId, Ws, Row, Params) of
        {ok, Row} -> content_read(OrgId, Ws, Row, Params);
        {error, _} = Err -> Err
    end.

content_status_gate(_OrgId, _Ws, Row, _Params) ->
    case maps:get(status, Row, undefined) of
        active ->
            {ok, Row};
        deleted ->
            {error, not_found};
        Other ->
            {error, {not_confirmed, Other}}
    end.

%% V3-17（G17）：正文读链切边缘——鉴权链（绑定门/ACL/status 门）全部原样
%% 保留，止于 Row；此后 BEAM **不再读字节**：签 EB 下载票据（claims=本链已
%% 确立的鉴权事实），302 到边缘 `/edge?ticket=`。边缘逐字节请求回调
%% authorize 重放完整 ACL（edge_authorize_content/1）。fail-closed：边缘
%% 未配置或位置非 garage 一律 {error,_}，不回退 BEAM 取流。
content_read(OrgId, Ws, Row, Params) ->
    Asset = port(Params, asset),
    case Asset:download_location(OrgId, Ws, maps:get(id, Row)) of
        {ok, _Location} ->
            case actor_scope(Params) of
                {ok, Actor} ->
                    Claims0 = #{
                        org_id => OrgId,
                        workspace_id => Ws,
                        asset_id => maps:get(id, Row),
                        actor_kind => maps:get(kind, Actor),
                        actor_id => maps:get(id, Actor)
                    },
                    Claims = lists:foldl(
                        fun(K, Acc) ->
                            case maps:get(K, Params, undefined) of
                                undefined -> Acc;
                                V -> Acc#{K => V}
                            end
                        end,
                        Claims0,
                        [conversation_id, visitor_session_id]
                    ),
                    issue_redirect(Claims, Row);
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {object_unreadable, Reason}}
    end.

issue_redirect(Claims, Row) ->
    case eb_asset_download_ticket:issue(Claims, config_ds:env(jwt_key, <<>>)) of
        {ok, Ticket} ->
            case edge_base() of
                <<>> ->
                    {error, edge_not_configured};
                Base ->
                    Url = <<
                        (string:trim(Base, trailing, "/"))/binary,
                        "?ticket=",
                        Ticket/binary
                    >>,
                    {ok, #{
                        redirect_url => Url,
                        asset_id => maps:get(id, Row),
                        object_hash => maps:get(object_hash, Row, undefined),
                        mime => maps:get(mime, Row, <<"application/octet-stream">>),
                        size_bytes => maps:get(size_bytes, Row, undefined)
                    }}
            end;
        _ ->
            {error, {ticket, invalid}}
    end.

%% 边缘对外基址：专用 `edge_base_url` 优先；缺省回退 `base_url` + `/edge`
%% （同域反代形态）。两者皆空 → fail-closed（edge_not_configured）。
edge_base() ->
    case config_ds:env(edge_base_url, <<>>) of
        <<>> ->
            case config_ds:env(base_url, <<>>) of
                <<>> -> <<>>;
                Base -> <<(string:trim(Base, trailing, "/"))/binary, "/edge">>
            end;
        Url ->
            Url
    end.

%% @doc V3-17：边缘 authorize 回调重放——票据 claims 重建 Params，走与
%% content_stream **完全相同**的鉴权链（绑定门/ACL/status 门），通过后解析
%% 精确 winner 并签短时 presign GET（仅回有界 URL，零正文）。撤权（成员
%% 停用/资产删除/会话移交）在下一边缘请求即时生效。
-spec edge_authorize_content(map()) -> {ok, binary()} | {error, term()}.
edge_authorize_content(
    #{
        org_id := OrgId,
        workspace_id := Ws,
        asset_id := AssetId,
        actor_kind := Kind,
        actor_id := ActorId
    } = Claims
) ->
    Params0 = #{workspace_id => Ws, asset_id => AssetId},
    Params1 =
        case Kind of
            member -> Params0#{actor_user_id => ActorId};
            contact -> Params0#{actor_contact_id => ActorId}
        end,
    Params =
        case Claims of
            #{conversation_id := Conv} -> Params1#{conversation_id => Conv};
            _ -> Params1
        end,
    case content_args(Params) of
        {error, _} = Err ->
            Err;
        {ok, Ws, AssetId, Actor} ->
            Asset = port(Params, asset),
            case Asset:fetch_asset(OrgId, Ws, AssetId) of
                {ok, Row} ->
                    case content_binding_gate(Row, Actor, Params) of
                        ok ->
                            Auth = port(Params, auth),
                            Store = port(Params, store),
                            RowConv = maps:get(conversation_id, Row, undefined),
                            case authorize_actor(Auth, Store, OrgId, Ws, RowConv, Actor) of
                                {error, _} = Err ->
                                    Err;
                                ok ->
                                    case content_status_gate(OrgId, Ws, Row, Params) of
                                        {ok, Row} -> edge_presign(Asset, OrgId, Ws, AssetId);
                                        {error, _} = Err -> Err
                                    end
                            end;
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} ->
                    {error, not_found}
            end
    end.

authorize_actor(Auth, Store, OrgId, Ws, Conv, #{kind := member, id := Id}) ->
    eb_asset_scope:authorize(Auth, Store, OrgId, Ws, Conv, Id);
authorize_actor(_Auth, Store, OrgId, Ws, Conv, #{kind := contact, id := Id}) ->
    eb_asset_scope:authorize_contact(Store, OrgId, Ws, Conv, Id).

edge_presign(Asset, OrgId, Ws, AssetId) ->
    case Asset:download_location(OrgId, Ws, AssetId) of
        {ok, I} ->
            attachment_storage_garage:presign_snapshot(I, 600);
        {error, Reason} ->
            {error, {object_unreadable, Reason}}
    end.

%% （V3-17 起正文不再过 BEAM：原 content_verify 的全量字节复核随之退役；
%% 内容完整性由存储侧 checksum proof 与 GC 复核链保证。）

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
        deleted ->
            case maps:get(pending_object_delete, Row, false) of
                true -> cleanup_delete(OrgId, Ws, AssetId, Params);
                false -> {skipped, AssetId, not_pending}
            end;
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

%% Deletion intent survives network failure; eligibility is rechecked atomically by the port.
cleanup_delete(OrgId, Ws, AssetId, Params) ->
    Asset = port(Params, asset),
    Clock = port(Params, clock),
    case Asset:cleanup_pending_private(OrgId, Ws, AssetId, Clock:now(), cleanup_ttl(Params)) of
        ok -> {deleted, AssetId};
        {error, Reason} -> {skipped, AssetId, {cleanup_failed, Reason}}
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
