-module(enterprise_asset_logic).

-moduledoc "企业附件业务逻辑（EPGZ-04 INT-07/08）—— Garage presign → PUT → confirm 流程。".
%%%
% EPGZ-04 INT-07/08 企业附件（Application 域 Garage presign -> PUT -> confirm）。
%
% 链路复用（零新逻辑桩）：presign 纯签名计算（elib_oss:presign_put_for_key
% 复用 lib 层；object key 企业域前缀 eoa/<org>/<app>/ 与个人 u<Uid>/ 物理
% 隔离）；confirm 复用 HEAD 核实模式（elib_oss:head_object，服务端真实值
% 覆盖客户端自报值——与 attach_logic:verify_and_save 同一范式），差异是
% ownership 校验维度从 uid 换成 (org, app)（scope 校验）+ confirm 与
% file.confirmed webhook 事件同事务原子。
%
% 固定非 E2EE：企业托管附件为服务端明文对象（attachment.cipher 恒 null），
% 不触个人附件加密链（attachment_cipher 00000052 的客户端加密语义）。
%%%

-export([presign_tx/3, confirm_tx/3]).

-include("log.hrl").

-define(PUT_EXPIRES, 3600).

%%%===================================================================
%%% INT-07 presign
%%%===================================================================

%% @doc 签发短期上传 URL。
%% Input（atom 键）：
%%   file_name  必填 binary 1..256
%%   mime_type  必填 binary（elib_oss 白名单预检；confirm 以 HEAD 真实值复核）
%%   size_bytes 可选 integer（>0 且 <= 有效上限（min(全局, 应用策略)）预检）
%% 返回 {ok, #{file_id, object_key, put_url, expires_at}}。
-spec presign_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
presign_tx(Conn, Ctx, Input) when is_map(Input) ->
    Operation = maps:get(operation, Input, undefined),
    ClientUploadId = maps:get(client_upload_id, Input, undefined),
    case {Operation, ClientUploadId} of
        {<<"status">>, _} ->
            presign_status_tx(Ctx, Input);
        {<<"renew">>, _} ->
            presign_renew_tx(Conn, Ctx, Input);
        %% V5 register：带 client_upload_id 的新会话走失败关闭协议链
        %% （02 §137：内部应用复用 attach_pending 既有 session 表）；
        %% 旧客户端（无 UUID、无 operation）保留 legacy 链兼容。
        {_, Cid} when Cid =/= undefined, Cid =/= <<>> ->
            presign_register_v5_tx(Conn, Ctx, Input);
        _ ->
            presign_legacy_tx(Conn, Ctx, Input)
    end;
presign_tx(_Conn, _Ctx, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

presign_legacy_tx(Conn, Ctx, Input) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    FileName = maps:get(file_name, Input, undefined),
    MimeType = maps:get(mime_type, Input, undefined),
    SizeHint = maps:get(size_bytes, Input, undefined),
    case enterprise_asset_retention_logic:content_policy_tx(Conn, Ctx, Input) of
        {ok, Policy} ->
            MaxBytes = enterprise_asset_retention_logic:effective_max_bytes(Policy, SizeHint),
            Ok =
                valid_name(FileName) andalso
                    enterprise_asset_retention_logic:mime_allowed(Policy, MimeType) andalso
                    valid_size_hint(SizeHint, MaxBytes),
            case Ok of
                true ->
                    presign_ok(Conn, OrgId, AppId, FileName, MimeType);
                false ->
                    {error, {<<"invalid_request">>, invalid_presign_input}}
            end;
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% V5 恢复 API（02 §137/§143）：register 失败关闭、status 只读、renew 单调
%%% 稳定身份在事务内保存；签发一律由 handler COMMIT 分支完成（F19），
%%% 因此本节返回的 map 绝不含 put_url/upload_capability。
%%%===================================================================

v5_snapshot(Ctx, Input) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    #{
        <<"subject">> => #{
            <<"kind">> => <<"application">>,
            <<"principal">> => integer_to_binary(AppId),
            <<"org_id">> => integer_to_binary(OrgId)
        },
        <<"declaration">> => #{
            <<"filename">> => maps:get(file_name, Input, undefined),
            <<"mime">> => maps:get(mime_type, Input, undefined),
            <<"scope">> => <<"enterprise">>,
            <<"org_id">> => OrgId,
            <<"application_id">> => AppId
        }
    }.

presign_register_v5_tx(Conn, Ctx, Input) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    FileName = maps:get(file_name, Input, undefined),
    MimeType = maps:get(mime_type, Input, undefined),
    SizeHint = maps:get(size_bytes, Input, undefined),
    ClientUploadId = maps:get(client_upload_id, Input),
    case enterprise_asset_retention_logic:content_policy_tx(Conn, Ctx, Input) of
        {ok, Policy} ->
            MaxBytes = enterprise_asset_retention_logic:effective_max_bytes(Policy, SizeHint),
            case
                valid_name(FileName) andalso
                    enterprise_asset_retention_logic:mime_allowed(Policy, MimeType) andalso
                    valid_size_hint(SizeHint, MaxBytes) andalso
                    valid_client_upload_id(ClientUploadId)
            of
                true ->
                    ObjectKey = enterprise_asset_repo:build_object_key(OrgId, AppId, FileName),
                    Bucket = elib_oss:get_bucket(<<"enterprise">>),
                    case
                        attach_pending_repo:register_v5_tx(
                            Conn,
                            ObjectKey,
                            Bucket,
                            <<"enterprise">>,
                            0,
                            ClientUploadId,
                            v5_snapshot(Ctx, Input)
                        )
                    of
                        {ok, Registration} ->
                            {ok, stable_registration(ObjectKey, Bucket, MimeType, Registration)};
                        {error, Reason} ->
                            %% 失败关闭：登记失败无任何能力，COMMIT 回滚由
                            %% handler 以业务错误应答（AC01）。
                            {error, {<<"service_unavailable">>, Reason}}
                    end;
                false ->
                    {error, {<<"invalid_request">>, invalid_presign_input}}
            end;
        {error, _} = Err ->
            Err
    end.

%% 只读回查：无能力、跨主体不泄露存在性（02 §137）。
presign_status_tx(Ctx, Input) ->
    IdOrKey = id_or_key(Input),
    AppId = maps:get(application_id, Ctx),
    case IdOrKey of
        undefined ->
            {error, {<<"invalid_request">>, missing_session_identity}};
        _ ->
            case attach_pending_repo:status_lookup(IdOrKey) of
                {ok, Row} ->
                    case v5_subject_app(Row) =:= AppId of
                        true -> {ok, status_view(Row)};
                        false -> {error, {<<"not_found">>, session_not_found}}
                    end;
                {error, not_found} ->
                    {error, {<<"not_found">>, session_not_found}};
                {error, Reason} ->
                    {error, {<<"service_unavailable">>, Reason}}
            end
    end.

%% renew：单调提交新到期（事务内稳定状态），签发在 handler COMMIT 分支。
presign_renew_tx(Conn, Ctx, Input) ->
    IdOrKey = id_or_key(Input),
    AppId = maps:get(application_id, Ctx),
    case IdOrKey of
        undefined ->
            {error, {<<"invalid_request">>, missing_session_identity}};
        _ ->
            case attach_pending_repo:status_lookup(IdOrKey) of
                {ok, #{<<"session_state">> := <<"registered">>} = Row} ->
                    case v5_subject_app(Row) =:= AppId of
                        false ->
                            {error, {<<"not_found">>, session_not_found}};
                        true ->
                            Deadline = unix_of(maps:get(<<"session_deadline">>, Row)),
                            Now = erlang:system_time(second),
                            case Now >= Deadline of
                                true ->
                                    {error, {<<"gone">>, session_expired}};
                                false ->
                                    Key = maps:get(<<"object_key">>, Row),
                                    NewExpiry = min(Now + ?PUT_EXPIRES, Deadline),
                                    case
                                        attach_pending_repo:renew_capability_tx(
                                            Conn, Key, 0, NewExpiry
                                        )
                                    of
                                        {ok, #{committed_expires_at := Committed}} ->
                                            {ok, #{
                                                <<"object_key">> => Key,
                                                <<"client_upload_id">> =>
                                                    maps:get(<<"client_upload_id">>, Row),
                                                <<"bucket">> => maps:get(<<"bucket">>, Row),
                                                <<"mime_type">> => declared_mime(Row),
                                                <<"session_state">> => <<"registered">>,
                                                <<"deadline">> => Deadline,
                                                <<"expires_at">> => Committed,
                                                <<"renew">> => true
                                            }};
                                        {error, not_renewable} ->
                                            {error, {<<"conflict">>, session_not_renewable}};
                                        {error, Reason} ->
                                            {error, {<<"service_unavailable">>, Reason}}
                                    end
                            end
                    end;
                {ok, #{<<"session_state">> := _Terminal}} ->
                    {error, {<<"conflict">>, session_terminal}};
                {error, not_found} ->
                    {error, {<<"not_found">>, session_not_found}};
                {error, Reason} ->
                    {error, {<<"service_unavailable">>, Reason}}
            end
    end.

id_or_key(Input) ->
    case maps:get(client_upload_id, Input, undefined) of
        undefined -> maps:get(object_key, Input, undefined);
        <<>> -> maps:get(object_key, Input, undefined);
        Cid -> Cid
    end.

valid_client_upload_id(Cid) when is_binary(Cid) ->
    byte_size(Cid) =:= 36 andalso binary:replace(Cid, <<"-">>, <<>>) =/= Cid;
valid_client_upload_id(_) ->
    false.

%% 登记快照里的 subject.principal（application 身份）——授权复核基准。
v5_subject_app(Row) ->
    case maps:get(<<"upload_registration">>, Row, undefined) of
        #{<<"subject">> := #{<<"principal">> := P}} ->
            case
                try
                    binary_to_integer(P)
                catch
                    _:_ -> undefined
                end
            of
                N when is_integer(N) -> N;
                _ -> undefined
            end;
        _ ->
            %% 旧行（legacy add 无快照）不泄露，按不匹配处理
            undefined
    end.

declared_mime(Row) ->
    case maps:get(<<"upload_registration">>, Row, undefined) of
        #{<<"declaration">> := #{<<"mime">> := M}} when is_binary(M), byte_size(M) > 0 -> M;
        _ -> <<"application/octet-stream">>
    end.

unix_of(undefined) ->
    0;
unix_of(Bin) when is_binary(Bin) ->
    case
        try
            calendar:rfc3339_to_system_time(binary_to_list(Bin), [{unit, second}])
        catch
            _:_ -> 0
        end
    of
        N when is_integer(N) -> N;
        _ -> 0
    end.

%% 稳定登记结果（无能力）：handler COMMIT 后据此签发。
stable_registration(ObjectKey, Bucket, MimeType, Registration) ->
    #{
        <<"object_key">> => ObjectKey,
        <<"bucket">> => Bucket,
        <<"mime_type">> => MimeType,
        <<"session_state">> => maps:get(session_state, Registration),
        <<"client_upload_id">> => maps:get(client_upload_id, Registration),
        <<"session_deadline">> => maps:get(deadline, Registration),
        <<"expires_at">> => maps:get(expires_at, Registration)
    }.

status_view(Row) ->
    #{<<"session_state">> := State} = Row,
    Deadline = unix_of(maps:get(<<"session_deadline">>, Row)),
    #{
        <<"client_upload_id">> => maps:get(<<"client_upload_id">>, Row, undefined),
        <<"object_key">> => maps:get(<<"object_key">>, Row),
        <<"session_state">> => State,
        <<"session_deadline">> => Deadline,
        <<"upload_expires_at">> => unix_of(maps:get(<<"upload_expires_at">>, Row)),
        <<"can_resume">> => State =:= <<"registered">>,
        <<"retry_action">> =>
            case State of
                <<"registered">> -> <<"resume_same_id">>;
                _ -> <<"none">>
            end
    }.

%% @doc 签发（内容策略通过后）：object key 前缀 + pending 登记 + 返回 URL。
presign_ok(Conn, OrgId, AppId, FileName, MimeType) ->
    ObjectKey = enterprise_asset_repo:build_object_key(OrgId, AppId, FileName),
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    PutUrl = elib_oss:presign_put_for_key(Bucket, ObjectKey, MimeType, ?PUT_EXPIRES),
    %% pending 登记（迁移 54 生命周期）：失败不阻断签发（与 attach_logic
    %% 同口径——拿不到 URL 是功能中断，漏登记只影响孤儿回收）。
    _ =
        case enterprise_asset_repo:pending_add_tx(Conn, ObjectKey, Bucket, <<"enterprise">>) of
            {ok, inserted} ->
                ok;
            {error, Reason} ->
                ?ERROR_LOG([
                    "enterprise_asset presign pending_add failed: ", ObjectKey, Reason
                ])
        end,
    {ok, #{
        <<"object_key">> => ObjectKey,
        <<"put_url">> => PutUrl,
        <<"expires_at">> => erlang:system_time(second) + ?PUT_EXPIRES
    }}.

%%%===================================================================
%%% INT-08 confirm
%%%===================================================================

%% @doc HEAD/size/hash/scope 校验后确认。
%% Input（atom 键）：
%%   object_key    必填 binary（必须是本 org/app 域 eoa/ 前缀且已 presign 登记）
%%   file_hash256  可选 binary（64 位小写/大写 hex；SHA-256 完整性参考）
%% 返回 {ok, #{file_id, object_key, size, mime_type}}；
%% 失败 {error, {Code, Detail}}，超限/非法类型对象被删除（与 attach_logic 同口径）。
%%
%% 取舍说明：HEAD 核实在调用方事务内执行（网络 IO，上限 10s）——为换
%% attachment 行、pending 销账、file.confirmed 事件的原子性（一期一致性
%% 优先；attach_logic 个人链把 HEAD 放事务外是重试幂等语义，此处 confirm
%% 由幂等键保护重试，等价安全）。
-spec confirm_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
confirm_tx(Conn, Ctx, Input) when is_map(Input) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    ObjectKey = maps:get(object_key, Input, undefined),
    Hash = maps:get(file_hash256, Input, undefined),
    case
        is_binary(ObjectKey) andalso ObjectKey =/= <<>> andalso
            enterprise_asset_repo:owned_key(ObjectKey, OrgId, AppId) andalso
            valid_hash(Hash)
    of
        true ->
            confirm_owned(Conn, Ctx, ObjectKey, Hash);
        false ->
            {error, {<<"invalid_request">>, invalid_confirm_input}}
    end;
confirm_tx(_Conn, _Ctx, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

confirm_owned(Conn, Ctx, ObjectKey, Hash) ->
    case enterprise_asset_repo:pending_find_tx(Conn, ObjectKey) of
        {ok, Pending} ->
            case pending_v5(Pending) of
                true ->
                    %% W2-02 V5 会话：字节级校验 + fn_v5_confirm_pending 原子确认
                    confirm_head_v5(Conn, Ctx, ObjectKey, Hash, Pending);
                false ->
                    confirm_head(Conn, Ctx, ObjectKey, Hash)
            end;
        {error, not_found} ->
            %% W2-02 兼容回读：pending 已销账但附件已转正 → 幂等回读同一
            %% file_id（响应丢失恢复，不新建会话）；从未登记 → 稳定
            %% not_presigned 重传指引，绝不静默删除对象。
            case
                enterprise_asset_repo:find_confirmed_tx(
                    Conn,
                    maps:get(organization_id, Ctx),
                    maps:get(application_id, Ctx),
                    ObjectKey
                )
            of
                {ok, Row} ->
                    {ok, #{
                        <<"file_id">> => maps:get(<<"id">>, Row),
                        <<"object_key">> => maps:get(<<"path">>, Row),
                        <<"size">> => maps:get(<<"size">>, Row),
                        <<"mime_type">> => maps:get(<<"mime_type">>, Row)
                    }};
                {error, not_found} ->
                    {error, {<<"invalid_request">>, not_presigned}}
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% V5 会话判别：client_upload_id 非空（fn_v5_register_pending 登记行）。
pending_v5(#{<<"client_upload_id">> := Cid}) when Cid =/= null -> true;
pending_v5(_) -> false.

%% V5 confirm：HEAD 仅作 MIME/策略预检，字节裁决以 GET 真实读流为准
%% （02 §4「HEAD 本身不充分」）；超限/类型拒绝不删对象——pin 保留供同
%% receipt 重试，物理回收归 W4 GC。
confirm_head_v5(Conn, Ctx, ObjectKey, Hash, Pending) ->
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    case elib_oss:head_object(Bucket, ObjectKey) of
        {error, not_found} ->
            {error, {<<"invalid_request">>, object_not_found}};
        {error, Reason} ->
            ?ERROR_LOG(["enterprise_asset v5 confirm head failed: ", Bucket, ObjectKey, Reason]),
            {error, {<<"internal_error">>, Reason}};
        {ok, #{content_type := RealType}} ->
            case elib_oss:object_sha256(Bucket, ObjectKey) of
                {error, not_found} ->
                    {error, {<<"invalid_request">>, object_not_found}};
                {error, Reason2} ->
                    ?ERROR_LOG([
                        "enterprise_asset v5 confirm read_bytes failed: ",
                        Bucket,
                        ObjectKey,
                        Reason2
                    ]),
                    {error, {<<"internal_error">>, Reason2}};
                {ok, Sha256, ByteSize} ->
                    case enterprise_asset_retention_logic:content_policy_tx(Conn, Ctx, #{}) of
                        {ok, Policy} ->
                            MaxBytes = enterprise_asset_retention_logic:effective_max_bytes(
                                Policy, ByteSize
                            ),
                            confirm_policy_v5(
                                Conn,
                                Ctx,
                                ObjectKey,
                                Hash,
                                ByteSize,
                                RealType,
                                Policy,
                                MaxBytes,
                                Pending,
                                Sha256
                            );
                        {error, _} = Err ->
                            Err
                    end
            end
    end.

confirm_policy_v5(
    Conn, Ctx, ObjectKey, Hash, RealSize, RealType, Policy, MaxBytes, Pending, Sha256
) ->
    MimeOk = enterprise_asset_retention_logic:mime_allowed(Policy, RealType),
    case RealSize > MaxBytes of
        true ->
            {error, {<<"invalid_request">>, file_too_large}};
        false ->
            case MimeOk of
                false ->
                    {error, {<<"invalid_request">>, invalid_file_type}};
                true ->
                    save_confirmed_v5(
                        Conn, Ctx, ObjectKey, Hash, RealSize, RealType, Pending, Sha256
                    )
            end
    end.

save_confirmed_v5(Conn, Ctx, ObjectKey, _Hash, RealSize, RealType, _Pending, Sha256) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    ScopeRef = enterprise_asset_repo:scope_ref(OrgId, AppId),
    %% V5：file_hash256 以服务端字节校验值为准（客户端声明仅完整性参考）
    InfoMap = #{
        scope_ref => ScopeRef,
        origin_kind => <<"enterprise_application">>,
        origin_application_id => AppId,
        file_hash256 => Sha256
    },
    case enterprise_asset_repo:confirm_save_tx(Conn, ObjectKey, RealType, RealSize, InfoMap) of
        {ok, AttId, First} ->
            %% 持锁重查 pin（锁前读数不构成授权，02 §4）后原子确认
            case confirm_v5_link_tx(Conn, ObjectKey, AttId, Sha256, RealSize) of
                ok ->
                    case First of
                        true ->
                            case
                                enterprise_asset_retention_logic:register_retention_tx(
                                    Conn, {AttId, OrgId, AppId}
                                )
                            of
                                ok ->
                                    case audit_mutation(Conn, Ctx, <<"file.confirmed">>, AttId) of
                                        ok ->
                                            ok = confirm_side_effects(Conn, Ctx, AttId),
                                            {ok, #{
                                                <<"file_id">> => AttId,
                                                <<"object_key">> => ObjectKey,
                                                <<"size">> => RealSize,
                                                <<"mime_type">> => RealType
                                            }};
                                        {error, Reason} ->
                                            {error, {<<"internal_error">>, {audit, Reason}}}
                                    end;
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        false ->
                            %% 重放：同一 file_id，审计/留存/副作用均已首确认落一次
                            {ok, #{
                                <<"file_id">> => AttId,
                                <<"object_key">> => ObjectKey,
                                <<"size">> => RealSize,
                                <<"mime_type">> => RealType
                            }}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% confirm 线性化（02 §4）：锁 pending 行重查 pin → fn_v5_confirm_pending
%% registered→confirmed（同 id 重放幂等）。失败按企业稳定码返回，
%% with_idempotency 对 {error, {Code, _}} 统一回滚（插入行一并撤销）。
confirm_v5_link_tx(Conn, ObjectKey, AttId, Sha256, RealSize) ->
    case attach_pending_repo:lock_session_tx(Conn, ObjectKey) of
        {ok, not_found} ->
            {error, {<<"resource_not_found">>, session_vanished}};
        {ok, Session} ->
            case pin_of_session(Session) of
                #{<<"sha256">> := PinSha, <<"size_bytes">> := PinSize} ->
                    case PinSha =:= Sha256 andalso ec_cnv:to_integer(PinSize) =:= RealSize of
                        true ->
                            case attach_pending_repo:confirm_session_tx(Conn, ObjectKey, AttId) of
                                {ok, AttId} ->
                                    ok;
                                {ok, _OtherId} ->
                                    {error, {<<"resource_conflict">>, confirm_id_mismatch}};
                                {error, confirm_conflict} ->
                                    {error, {<<"resource_conflict">>, confirm_conflict}};
                                {error, Reason} ->
                                    {error, {<<"internal_error">>, Reason}}
                            end;
                        false ->
                            {error, {<<"invalid_request">>, content_hash_mismatch}}
                    end;
                _ ->
                    {error, {<<"invalid_request">>, content_not_pinned}}
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

pin_of_session(#{<<"upload_content">> := Content}) when is_map(Content) -> Content;
pin_of_session(_) -> #{}.

confirm_head(Conn, Ctx, ObjectKey, Hash) ->
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    case elib_oss:head_object(Bucket, ObjectKey) of
        {error, not_found} ->
            {error, {<<"invalid_request">>, object_not_found}};
        {error, Reason} ->
            ?ERROR_LOG(["enterprise_asset confirm head failed: ", Bucket, ObjectKey, Reason]),
            {error, {<<"internal_error">>, Reason}};
        {ok, #{size := RealSize, content_type := RealType}} ->
            case enterprise_asset_retention_logic:content_policy_tx(Conn, Ctx, #{}) of
                {ok, Policy} ->
                    MaxBytes = enterprise_asset_retention_logic:effective_max_bytes(
                        Policy, RealSize
                    ),
                    confirm_policy(
                        Conn, Ctx, ObjectKey, Hash, RealSize, RealType, Policy, MaxBytes
                    );
                {error, _} = Err ->
                    Err
            end
    end.

%% @doc confirm 侧内容策略复核（**服务端真实值**）：超有效上限或类型不在策略内
%% 的对象一律删除并拒绝（与广州期 too_large/invalid_type 同口径）。
confirm_policy(Conn, Ctx, ObjectKey, Hash, RealSize, RealType, Policy, MaxBytes) ->
    MimeOk = enterprise_asset_retention_logic:mime_allowed(Policy, RealType),
    case RealSize > MaxBytes of
        true ->
            discard_object(ObjectKey),
            {error, {<<"invalid_request">>, file_too_large}};
        false ->
            case MimeOk of
                false ->
                    discard_object(ObjectKey),
                    {error, {<<"invalid_request">>, invalid_file_type}};
                true ->
                    save_confirmed(Conn, Ctx, ObjectKey, Hash, RealSize, RealType)
            end
    end.

-spec discard_object(binary()) -> ok.
discard_object(ObjectKey) ->
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    _ =
        try
            elib_oss:delete_object(Bucket, ObjectKey)
        catch
            _:_ -> ok
        end,
    ok.

save_confirmed(Conn, Ctx, ObjectKey, Hash, RealSize, RealType) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    ScopeRef = enterprise_asset_repo:scope_ref(OrgId, AppId),
    Info = #{
        scope_ref => ScopeRef,
        origin_kind => <<"enterprise_application">>,
        origin_application_id => AppId
    },
    InfoMap =
        case Hash of
            undefined -> Info#{file_hash256 => <<>>};
            _ -> Info#{file_hash256 => Hash}
        end,
    case enterprise_asset_repo:confirm_save_tx(Conn, ObjectKey, RealType, RealSize, InfoMap) of
        {ok, AttId, First} ->
            ok = enterprise_asset_repo:pending_remove_tx(Conn, ObjectKey),
            %% FULL-02：留存/法务 hold 治理行与附件转正**同事务**登记
            %% （migration 00000140；附件与治理行要么都在、要么都不在）。
            %% W2-02：以下均只在首确认执行一次（重放同一 file_id，不重复
            %% 审计/登记/计量——first audit count=1）。
            case First of
                false ->
                    {ok, #{
                        <<"file_id">> => AttId,
                        <<"object_key">> => ObjectKey,
                        <<"size">> => RealSize,
                        <<"mime_type">> => RealType
                    }};
                true ->
                    case
                        enterprise_asset_retention_logic:register_retention_tx(
                            Conn, {AttId, OrgId, AppId}
                        )
                    of
                        ok ->
                            %% INT-BE-03 冻结政策 INT-08=REQUIRED_AUDIT：转正审计与
                            %% 业务写同事务（审计失败 → error → 调用方整体回滚）。
                            case audit_mutation(Conn, Ctx, <<"file.confirmed">>, AttId) of
                                ok ->
                                    ok = confirm_side_effects(Conn, Ctx, AttId),
                                    {ok, #{
                                        <<"file_id">> => AttId,
                                        <<"object_key">> => ObjectKey,
                                        <<"size">> => RealSize,
                                        <<"mime_type">> => RealType
                                    }};
                                {error, Reason} ->
                                    {error, {<<"internal_error">>, {audit, Reason}}}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc INT-BE-03 冻结政策 REQUIRED_AUDIT 审计接线（本模块私有）：调用
%% enterprise_audit_event_repo:append_tx/3 在调用方事务内落审计行；
%% actor_role 恒为 enterprise_application；detail 只放结构化摘要
%% （application/correlation/object_key），无 secret / 无正文。
-spec audit_mutation(any(), map(), binary(), term()) -> ok | {error, term()}.
audit_mutation(Conn, Ctx, Action, ResourceId) ->
    Detail = #{
        <<"origin_application_id">> => maps:get(application_id, Ctx, null),
        <<"correlation_id">> => maps:get(correlation_id, Ctx, null)
    },
    case
        enterprise_audit_event_repo:append_tx(Conn, maps:get(organization_id, Ctx), #{
            resource_type => <<"attachment">>,
            resource_id => ResourceId,
            action => Action,
            actor_user_id => maps:get(principal_user_id, Ctx, undefined),
            actor_role => <<"enterprise_application">>,
            detail => Detail
        })
    of
        {ok, _AuditId} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc confirm 的旁路副作用（聚合计量 + file.confirmed 事件）：与转正同事务。
%% 事件订阅判定/SSRF guard 在 emit 内；guard 拒绝只跳过事件不阻断 confirm。
-spec confirm_side_effects(any(), map(), pos_integer()) -> ok.
confirm_side_effects(Conn, Ctx, AttId) ->
    _ = enterprise_webhook_logic:emit_event_tx(
        Conn,
        Ctx,
        <<"file.confirmed">>,
        #{resource_type => <<"attachment">>, resource_id => AttId}
    ),
    case
        enterprise_application_usage_repo:bump_tx(
            Conn,
            maps:get(organization_id, Ctx),
            maps:get(application_id, Ctx),
            <<"file.confirmed">>
        )
    of
        ok ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG(["enterprise_asset confirm usage bump failed: ", Reason]),
            ok
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

valid_name(N) when is_binary(N), byte_size(N) > 0, byte_size(N) =< 256 ->
    filename:basename(N) =/= <<>>;
valid_name(_) ->
    false.

%% @doc 尺寸预检（FULL-02 起上限来自**内容策略**：min(全局, 应用配置)）。
valid_size_hint(undefined, _MaxBytes) ->
    true;
valid_size_hint(S, MaxBytes) when is_integer(S), S > 0 ->
    S =< MaxBytes;
valid_size_hint(_, _MaxBytes) ->
    false.

valid_hash(undefined) ->
    true;
valid_hash(H) when is_binary(H), byte_size(H) =:= 64 ->
    HexChars = "0123456789abcdefABCDEF",
    lists:all(fun(C) -> lists:member(C, HexChars) end, binary_to_list(H));
valid_hash(_) ->
    false.
