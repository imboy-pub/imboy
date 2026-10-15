-module(attach_logic).
-compile([nowarn_deprecated_catch]).
%%%
% 附件业务逻辑层（Garage S3 多桶直传链路 + 统一资源访问控制）
%
% 三条职责：
%   presign/5   按 scope 选桶 + 校验上传权 + 生成绑定 uid 的 presigned PUT URL
%   confirm/5   PUT 成功回调：写归属守卫 + 上传权校验 + HEAD 核实 + 落库(含 scope)
%   view_url/2  读鉴权 authorize/2（六分支）后，public 直读公开 URL，受限资源签短时 GET
%
% 归属两件独立的事：
%   写归属(上传权) = object_key 的 u<Uid>/ 前缀 + can_upload/3 的 scope 权限
%   读归属(访问权) = authorize/2 按 scope 分发（public/private/c2c/group/channel/moment）
% 详见 docs/architecture/resource-access-control.md。
%%%

-export([presign/5, presign/6, confirm/5, view_url/2, download/1]).
-export([reauthorize_upload_session/2]).
-export([authorize/2]).

-ifdef(TEST).
%% 纯函数，导出仅为可直接验收「不做套件协商」这一 fail-closed 语义
-export([normalize_cipher/1]).
-endif.

-include("log.hrl").
%% BUILD-00R：moment 未编译时 moment 附件 scope 走 catch-all fail-closed（false）
-include("generated/imboy_product_features.hrl").

%% 上传 URL 有效期：1 小时（足够单文件上传）
-define(PUT_EXPIRES, 3600).
%% 下载 URL 有效期：受限资源统一 600s（不分级，见设计文档第 8 节）
-define(GET_EXPIRES, 600).

%% ===================================================================
%% presign：选桶 + 上传权校验 + 生成 PUT URL
%% ===================================================================

-spec presign(integer(), binary(), binary(), binary(), binary() | undefined) ->
    {ok, map()} | {error, invalid_file_type | forbidden | registration_failed}.
presign(Uid, FileName, MimeType, Scope, ScopeRef) ->
    presign(Uid, FileName, MimeType, Scope, ScopeRef, undefined).

presign(Uid, FileName, MimeType, Scope, ScopeRef, ClientUploadId) ->
    case elib_oss:validate_file_type(MimeType) of
        false ->
            {error, invalid_file_type};
        true ->
            %% Step 10 教学附件：声明 MIME 预检（confirm 阶段以 HEAD 真实值复核）
            case teaching_presign_guard(Scope, MimeType) of
                ok ->
                    presign_authorized(
                        Uid, FileName, MimeType, Scope, ScopeRef, ClientUploadId
                    );
                {error, _} = E ->
                    E
            end
    end.

%% 教学白名单预检；非教学 scope 直通（保持既有行为零变化）
-spec teaching_presign_guard(binary(), binary()) -> ok | {error, invalid_file_type}.
teaching_presign_guard(<<"teaching">>, MimeType) ->
    moya_attach_logic:check_mime(MimeType);
teaching_presign_guard(_Scope, _MimeType) ->
    ok.

%% V5 恢复合同（AC01 / 02 §12）：登记事务 COMMIT 成功后才签发能力。
%% 登记失败、异常或提交结果未知一律不返回可用 put_url —— 签名器调用 0 次。
%% 同 client_upload_id 重试经 fn_v5_register_pending 幂等返回原登记；
%% 快照不一致由数据库拒绝，恢复走 status 而不是重新签发。
-spec presign_authorized(
    integer(),
    binary(),
    binary(),
    binary(),
    binary() | undefined,
    undefined | binary()
) ->
    {ok, map()}
    | {error,
        invalid_file_type
        | forbidden
        | registration_failed}.
presign_authorized(Uid, FileName, MimeType, Scope, ScopeRef, ClientUploadId) ->
    case can_upload(Uid, Scope, ScopeRef) of
        ok ->
            ObjectKey = elib_oss:build_object_key(Uid, Scope, ScopeRef, FileName),
            Bucket = elib_oss:get_bucket(Scope),
            Registration = #{
                <<"filename">> => FileName,
                <<"mime">> => MimeType,
                <<"scope">> => Scope,
                <<"scope_ref">> =>
                    case ScopeRef of
                        undefined -> null;
                        _ -> ScopeRef
                    end
            },
            RegResult =
                try
                    attach_pending_repo:register_v5(
                        ObjectKey, Bucket, Scope, Uid, ClientUploadId, Registration
                    )
                of
                    {ok, _Session} = OK -> OK;
                    {error, RegReason} -> {error, RegReason}
                catch
                    PClass:PReason ->
                        ?ERROR_LOG([
                            "attach_logic presign register_v5 crashed: ",
                            ObjectKey,
                            PClass,
                            PReason
                        ]),
                        {error, registration_crashed}
                end,
            case RegResult of
                {ok, Session} ->
                    legacy_upload_url(
                        ClientUploadId,
                        presign_session(Session, ObjectKey, Bucket, MimeType, Scope)
                    );
                {error, RegReason2} ->
                    %% fail-closed：无登记对象、无 URL；客户端同 ID 重试恢复。
                    ?ERROR_LOG([
                        "attach_logic presign registration failed (no URL issued): ",
                        ObjectKey,
                        RegReason2
                    ]),
                    {error, registration_failed}
            end;
        {error, _} = E ->
            E
    end.

%% Old clients expect an absolute PUT URL and need no knowledge of session UUIDs.
legacy_upload_url(undefined, {ok, #{<<"gateway">> := true, <<"put_url">> := Path} = Data}) ->
    Base = config_ds:env(base_url, <<>>),
    case uri_string:parse(Base) of
        #{scheme := Scheme, host := Host} when
            (Scheme =:= <<"http">> orelse Scheme =:= <<"https">>), Host =/= <<>>
        ->
            Origin = string:trim(Base, trailing, "/"),
            {ok, Data#{<<"put_url">> => <<Origin/binary, Path/binary>>}};
        _ ->
            {error, upload_base_url_required}
    end;
legacy_upload_url(_, Result) ->
    Result.

presign_session(#{client_upload_id := undefined}, ObjectKey, Bucket, MimeType, Scope) ->
    PutUrl = elib_oss:presign_put_for_key(Bucket, ObjectKey, MimeType, ?PUT_EXPIRES),
    Base = #{
        <<"put_url">> => PutUrl,
        <<"object_key">> => ObjectKey,
        <<"expires_at">> => erlang:system_time(second) + ?PUT_EXPIRES
    },
    {ok, maybe_public_url(Scope, ObjectKey, Base)};
presign_session(Session, _ProposedKey, _Bucket, MimeType, Scope) ->
    Key = maps:get(object_key, Session),
    State = maps:get(session_state, Session),
    case State of
        <<"confirmed">> ->
            {ok, #{
                <<"object_key">> => Key,
                <<"session_state">> => State,
                <<"client_upload_id">> => maps:get(client_upload_id, Session),
                <<"can_resume">> => false,
                <<"retry_action">> => <<"none">>
            }};
        <<"registered">> ->
            presign_registered(Session, Key, MimeType, Scope);
        _ ->
            {error, session_not_renewable}
    end.

presign_registered(
    #{
        storage_driver := Driver,
        storage_namespace := Ns,
        storage_container := Container,
        storage_locator_sha256 := Digest
    } = Session,
    Key,
    MimeType,
    Scope
) when
    is_binary(Driver),
    is_binary(Ns),
    is_binary(Container),
    is_binary(Digest),
    byte_size(Ns) > 0,
    byte_size(Container) > 0,
    byte_size(Digest) =:= 64
->
    Expiry = maps:get(expires_at, Session),
    Deadline = maps:get(deadline, Session),
    Now = erlang:system_time(second),
    case
        is_integer(Expiry) andalso is_integer(Deadline) andalso Now < Expiry andalso Now < Deadline
    of
        false ->
            {error, session_expired};
        true ->
            Cid = maps:get(client_upload_id, Session),
            {ok, Capability} = imboy_jwt:issue_upload_capability(Key, MimeType, Expiry),
            Base = #{
                <<"put_url">> => gateway_put_url(Cid, Capability),
                <<"object_key">> => Key,
                <<"expires_at">> => Expiry,
                <<"upload_capability">> => Capability,
                <<"gateway">> => true,
                <<"client_upload_id">> => Cid,
                <<"session_state">> => <<"registered">>
            },
            {ok, maybe_public_url(Scope, Key, Base)}
    end;
presign_registered(_, _, _, _) ->
    {error, historical_location_unverified}.

%% ===================================================================
%% confirm：写归属守卫 + 上传权校验 + HEAD 核实 + 落库
%% ===================================================================

%% Recovery uses the original declaration, never a caller-supplied replacement scope.
reauthorize_upload_session(Uid, #{
    <<"creator_user_id">> := Uid,
    <<"scope">> := Scope,
    <<"upload_registration">> := #{<<"subject">> := Subject, <<"declaration">> := Declaration}
}) when
    is_integer(Uid), Uid > 0, is_map(Subject), is_map(Declaration)
->
    Expected = #{<<"kind">> => <<"user">>, <<"principal">> => integer_to_binary(Uid)},
    case Subject =:= Expected andalso maps:get(<<"scope">>, Declaration, undefined) =:= Scope of
        true ->
            Ref =
                case maps:get(<<"scope_ref">>, Declaration, undefined) of
                    null -> undefined;
                    V -> V
                end,
            can_upload(Uid, Scope, Ref);
        false ->
            {error, forbidden}
    end;
reauthorize_upload_session(_, _) ->
    {error, forbidden}.

-spec confirm(integer(), binary(), binary(), binary() | undefined, map()) ->
    {ok, map()}
    | {error,
        forbidden_key
        | forbidden
        | upload_not_supported
        | invalid_key
        | object_not_found
        | file_too_large
        | invalid_file_type
        | term()}.
confirm(Uid, ObjectKey, Scope, ScopeRef, Meta) ->
    case elib_oss:owner_of_key(ObjectKey) of
        {ok, Uid} ->
            case can_upload(Uid, Scope, ScopeRef) of
                ok ->
                    %% W2-02：V5 会话（client_upload_id 非空，登记时由
                    %% fn_v5_register_pending 固定）走字节校验 + 原子会话确认；
                    %% 旧客户端 pending（无 V5 列）与无 pending 行保持 legacy。
                    case v5_session(ObjectKey) of
                        {ok, true} -> confirm_registered(Uid, ObjectKey, Scope, ScopeRef, Meta);
                        {ok, false} -> verify_and_save(Uid, ObjectKey, Scope, ScopeRef, Meta);
                        {error, _} = E -> E
                    end;
                {error, _} = E ->
                    E
            end;
        {ok, _Other} ->
            {error, forbidden_key};
        {error, R} ->
            {error, R}
    end.

confirm_registered(Uid, Key, Scope, Ref, Meta) ->
    case attach_pending_repo:status_lookup(Key) of
        {ok, Row} ->
            case session_request_matches(Uid, Scope, Ref, Row) of
                true ->
                    case reauthorize_upload_session(Uid, Row) of
                        ok -> verify_and_save_v5(Uid, Key, Scope, Ref, Meta);
                        {error, _} = E -> E
                    end;
                false ->
                    {error, scope_conflict}
            end;
        {error, _} = E ->
            E
    end.

session_request_matches(Uid, Scope, Ref, #{
    <<"creator_user_id">> := Uid,
    <<"scope">> := Scope,
    <<"upload_registration">> := #{<<"subject">> := Subject, <<"declaration">> := Declaration}
}) when
    is_map(Declaration)
->
    Subject =:= #{<<"kind">> => <<"user">>, <<"principal">> => integer_to_binary(Uid)} andalso
        maps:get(<<"scope">>, Declaration, undefined) =:= Scope andalso
        normalize_ref(maps:get(<<"scope_ref">>, Declaration, undefined)) =:= normalize_ref(Ref);
session_request_matches(_, _, _, _) ->
    false.

%% V5 会话判别：仅 client_upload_id 非空的登记行（迁移 178 列集）；
%% 旧 add/4 写入的 pending 行各 V5 列为 NULL，继续 legacy 链。
v5_session(ObjectKey) ->
    case attach_pending_repo:status_lookup(ObjectKey) of
        {ok, #{<<"session_state">> := State, <<"client_upload_id">> := Cid}} when
            State =:= <<"registered">>; State =:= <<"confirmed">>
        ->
            {ok, Cid =/= null};
        {ok, _} ->
            {ok, false};
        {error, not_found} ->
            {ok, false};
        {error, R} ->
            {error, R}
    end.

%% W2-02 V5 confirm：真实字节校验（02 §4「HEAD 本身不充分」）+ 原子会话确认。
%% 与 legacy 的差异：GET 全量字节算 SHA256/size 对照登记会话的 pin 快照；
%% confirm 事务内持锁重查 upload_content（锁前读数不构成授权）；
%% 失败不删对象——pin 保留供同 receipt 重试，物理回收归 W4 GC。
-spec verify_and_save_v5(integer(), binary(), binary(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
verify_and_save_v5(Uid, ObjectKey, Scope, ScopeRef, Meta) ->
    case v5_object_bytes(ObjectKey, Scope) of
        {ok, Sha256, RealSize, RealType} ->
            case v5_meta_guard(Scope, RealType, RealSize, Meta) of
                ok -> do_save_v5(Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType, Sha256);
                {error, _} = E -> E
            end;
        {error, not_found} ->
            {error, object_not_found};
        {error, _} = E ->
            E
    end.

v5_object_bytes(Key, _Scope) ->
    case attachment_storage:session_location(Key) of
        {ok, legacy_unknown} ->
            {error, historical_location_unverified};
        {ok, #{driver := garage, mime := Mime} = I} ->
            case attachment_storage_garage:read_snapshot(I) of
                {ok, Bytes} ->
                    {ok, binary:encode_hex(crypto:hash(sha256, Bytes), lowercase), byte_size(Bytes),
                        Mime};
                {error, _} = E ->
                    E
            end;
        {ok, #{driver := disk, mime := Mime} = I} ->
            case attachment_storage_disk:read_snapshot(I) of
                {ok, Bytes} ->
                    {ok, binary:encode_hex(crypto:hash(sha256, Bytes), lowercase), byte_size(Bytes),
                        Mime};
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E;
        _ ->
            {error, unsupported_driver}
    end.

%% 与 legacy verify_and_save 同口径的白名单/上限/教学复核；V5 失败不删对象。
v5_meta_guard(Scope, RealType, RealSize, Meta) ->
    case RealSize > elib_oss:max_file_size() of
        true ->
            {error, file_too_large};
        false ->
            case elib_oss:validate_file_type(RealType) of
                false ->
                    {error, invalid_file_type};
                true ->
                    teaching_verify_guard(Scope, RealType, RealSize, Meta)
            end
    end.

-spec do_save_v5(
    integer(),
    binary(),
    binary(),
    binary() | undefined,
    map(),
    non_neg_integer(),
    binary(),
    binary()
) ->
    {ok, map()} | {error, term()}.
do_save_v5(Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType, Sha256) ->
    case group_anchor_msg_id(Scope, Meta) of
        {error, attachment_anchor_required} = E ->
            E;
        AnchorMsgId ->
            case normalize_cipher(maps:get(<<"cipher">>, Meta, undefined)) of
                {error, unsupported_cipher} ->
                    {error, unsupported_cipher};
                Cipher ->
                    do_save_v5_1(
                        Uid,
                        ObjectKey,
                        Scope,
                        ScopeRef,
                        Meta,
                        RealSize,
                        RealType,
                        Cipher,
                        AnchorMsgId,
                        Sha256
                    )
            end
    end.

do_save_v5_1(
    Uid, ObjectKey, Scope, ScopeRef, _Meta, RealSize, RealType, Cipher, AnchorMsgId, Sha256
) ->
    Attach = confirm_attach_map(
        ObjectKey, Scope, ScopeRef, RealSize, RealType, Cipher, AnchorMsgId, Sha256
    ),
    Now = elib_dt:now(),
    try
        case
            elib_pg:with_tx(fun(Conn) ->
                confirm_v5_tx(Conn, ObjectKey, Scope, Cipher, Now, Uid, Attach)
            end)
        of
            ok ->
                {ok, maybe_public_url(Scope, ObjectKey, confirm_payload(ObjectKey))};
            {error, SaveReason} ->
                ?ERROR_LOG(["attach_logic v5 confirm tx failed: ", ObjectKey, SaveReason]),
                {error, SaveReason}
        end
    catch
        Class:Reason ->
            ?ERROR_LOG(["attach_logic v5 confirm save failed: ", Class, Reason]),
            {error, Reason}
    end.

%% confirm 行：真实字节 hash 落库（file_hash256=服务端 GET 校验值），
%% 其余列与 legacy save/4 同形状（md5 输入回退仅作用于无字节校验的 legacy）。
confirm_attach_map(ObjectKey, Scope, ScopeRef, RealSize, RealType, Cipher, AnchorMsgId, Sha256) ->
    #{
        <<"file_hash256">> => Sha256,
        <<"cipher">> => Cipher,
        <<"mime_type">> => RealType,
        <<"name">> => filename:basename(ObjectKey),
        <<"path">> => ObjectKey,
        <<"url">> => ObjectKey,
        <<"size">> => RealSize,
        <<"scope">> => Scope,
        <<"scope_ref">> => normalize_ref(ScopeRef),
        <<"anchor_msg_id">> => AnchorMsgId
    }.

%% confirm 线性化事务（02 §4）：workspace 资格 → 锁 pending 行重查 pin 与
%% scope → metadata insert-if-absent → fn_v5_confirm_pending registered→confirmed。
confirm_v5_tx(Conn, ObjectKey, Scope, _Cipher, Now, Uid, Attach) ->
    case attach_scope_target(Scope, maps:get(<<"scope_ref">>, Attach, undefined)) of
        {ok, Target} ->
            ok = workspace_guard:abort_on_error(
                attachment_ds:ensure_upload_scope_tx(Conn, Target, Uid)
            );
        passthrough ->
            ok
    end,
    case attach_pending_repo:lock_session_tx(Conn, ObjectKey) of
        {ok, not_found} ->
            throw({abort_tx, session_vanished});
        {ok, Session} ->
            case
                session_request_matches(
                    Uid, Scope, maps:get(<<"scope_ref">>, Attach, undefined), Session
                )
            of
                true -> ok;
                false -> throw({abort_tx, scope_conflict})
            end,
            %% 持锁重查 pin（锁前哈希结果只用于事务外 GET，此处为裁决基准）
            case pin_of(Session) of
                #{<<"sha256">> := PinSha, <<"size_bytes">> := PinSize} ->
                    PinSizeInt = ec_cnv:to_integer(PinSize),
                    case
                        PinSha =:= maps:get(<<"file_hash256">>, Attach) andalso
                            PinSizeInt =:= ec_cnv:to_integer(maps:get(<<"size">>, Attach))
                    of
                        true -> ok;
                        false -> throw({abort_tx, content_hash_mismatch})
                    end;
                _ ->
                    throw({abort_tx, content_not_pinned})
            end,
            case maps:get(<<"scope">>, Session, null) of
                PendingScope when PendingScope =/= null, PendingScope =/= Scope ->
                    throw({abort_tx, scope_conflict});
                _ ->
                    ok
            end,
            %% metadata insert-if-absent + scope/cipher 冲突复核 + 首审计一次
            %% （save_confirmed_tx 复用：重放不递增 referer，审计只在首行）
            Location = maps:with(
                [
                    <<"storage_driver">>,
                    <<"storage_namespace">>,
                    <<"storage_container">>,
                    <<"storage_locator_sha256">>
                ],
                Session
            ),
            ok = attachment_ds:save_confirmed_tx(Conn, Now, Uid, [maps:merge(Attach, Location)]),
            case attachment_repo:confirmation_row_tx(Conn, ObjectKey) of
                {ok, #{<<"id">> := Id}} ->
                    case attach_pending_repo:confirm_session_tx(Conn, ObjectKey, Id) of
                        {ok, Id} ->
                            ok;
                        {ok, _OtherId} ->
                            throw({abort_tx, confirm_id_mismatch});
                        {error, ConfirmReason} ->
                            throw({abort_tx, ConfirmReason})
                    end;
                {ok, not_found} ->
                    throw({abort_tx, confirm_row_vanished});
                {error, RowReason} ->
                    throw({abort_tx, RowReason})
            end
    end.

pin_of(#{<<"upload_content">> := Content}) when is_map(Content) -> Content;
pin_of(_) -> #{}.

%% @doc 服务端 HEAD 核实真实性后落库（真实值覆盖客户端自报值）
-spec verify_and_save(integer(), binary(), binary(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
verify_and_save(Uid, ObjectKey, Scope, ScopeRef, Meta) ->
    Bucket = elib_oss:get_bucket(Scope),
    case elib_oss:head_object(Bucket, ObjectKey) of
        {error, not_found} ->
            {error, object_not_found};
        {error, R} ->
            %% not_found 之外的 HEAD 异常（如 416 空对象/5xx）必须留痕，
            %% 否则上层只看到笼统的"附件落库失败"400，无法定位。
            ?ERROR_LOG(["attach_logic confirm head_object failed: ", Bucket, ObjectKey, R]),
            {error, R};
        {ok, #{size := RealSize, content_type := RealType}} ->
            case RealSize > elib_oss:max_file_size() of
                true ->
                    reject_upload_object(ObjectKey, <<"confirm reject file_too_large">>),
                    {error, file_too_large};
                false ->
                    case elib_oss:validate_file_type(RealType) of
                        false ->
                            reject_upload_object(ObjectKey, <<"confirm reject invalid_file_type">>),
                            {error, invalid_file_type};
                        true ->
                            %% Step 10 教学附件：教学白名单/大小上限/时长复核
                            %% （video mp4|mov ≤100MB+duration≤60s；photo jpeg|png ≤20MB）
                            case teaching_verify_guard(Scope, RealType, RealSize, Meta) of
                                ok ->
                                    do_save(
                                        Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType
                                    );
                                {error, Reason} ->
                                    reject_upload_object(
                                        ObjectKey,
                                        <<"confirm reject teaching: ",
                                            (ec_cnv:to_binary(Reason))/binary>>
                                    ),
                                    {error, Reason}
                            end
                    end
            end
    end.

%% W4-01 入口收口（02 §4/§5，F11）：内容拒绝不再直接删字节——同事务
%% registered→rejected→delete_intent + ready job + 审计（迁移 184），字节由
%% Worker 删；主动拒绝即写权限撤权，不受能力窗限制。无 pending 行（旧客户端
%% 未登记对象）无意图锚点：ERROR 留痕隔离，绝不绕过队列直接删。
-spec reject_upload_object(binary(), binary()) -> ok.
reject_upload_object(ObjectKey, Reason) ->
    case attach_pending_repo:intent_pending_delete(ObjectKey, <<"reject">>, Reason) of
        ok ->
            ok;
        {error, not_found} ->
            ?ERROR_LOG([
                "attach_logic reject: no pending session anchor, bytes isolated "
                "(not deleted, queue-only reclamation): ",
                ObjectKey,
                Reason
            ]),
            ok;
        {error, Reason2} ->
            %% intent 落败（并发 tombstone/锁冲突等）：对象保留，日志留痕；
            %% 绝不回退到直接删字节。
            ?ERROR_LOG([
                "attach_logic reject: pending delete_intent failed, bytes retained: ",
                ObjectKey,
                Reason,
                Reason2
            ]),
            ok
    end.

%% 教学复核；非教学 scope 直通（既有行为零变化）
-spec teaching_verify_guard(binary(), binary(), non_neg_integer(), map()) ->
    ok | {error, file_too_large | invalid_file_type}.
teaching_verify_guard(<<"teaching">>, RealType, RealSize, Meta) ->
    moya_attach_logic:verify_upload(RealType, RealSize, Meta);
teaching_verify_guard(_Scope, _RealType, _RealSize, _Meta) ->
    ok.

-spec do_save(
    integer(), binary(), binary(), binary() | undefined, map(), non_neg_integer(), binary()
) ->
    {ok, map()} | {error, term()}.
do_save(Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType) ->
    case group_anchor_msg_id(Scope, Meta) of
        {error, attachment_anchor_required} = E ->
            E;
        AnchorMsgId ->
            case normalize_cipher(maps:get(<<"cipher">>, Meta, undefined)) of
                {error, unsupported_cipher} ->
                    %% fail-closed：不做套件协商。落库成 null 会把密文对象**标成明文**，
                    %% 那才是真正危险的降级——日后回迁盘点会漏掉它，读取侧也会当明文直读。
                    {error, unsupported_cipher};
                Cipher ->
                    do_save_1(
                        Uid,
                        ObjectKey,
                        Scope,
                        ScopeRef,
                        Meta,
                        RealSize,
                        RealType,
                        Cipher,
                        AnchorMsgId
                    )
            end
    end.

%% 群附件必须在上传前生成消息 ID；其它 scope 不参与群历史边界。
group_anchor_msg_id(<<"group">>, Meta) ->
    case maps:get(<<"anchor_msg_id">>, Meta, undefined) of
        MsgId when is_binary(MsgId), byte_size(MsgId) > 0, byte_size(MsgId) =< 128 -> MsgId;
        _ -> {error, attachment_anchor_required}
    end;
group_anchor_msg_id(_Scope, _Meta) ->
    null.

%% 只接受 null（明文，含全部旧客户端）与冻结的唯一套件名。
%% 与客户端 AttachmentDescriptor.supportedCipher 保持同一个值。
normalize_cipher(undefined) -> null;
normalize_cipher(null) -> null;
normalize_cipher(<<>>) -> null;
normalize_cipher(<<"AES-256-GCM">>) -> <<"AES-256-GCM">>;
normalize_cipher(_) -> {error, unsupported_cipher}.

do_save_1(Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType, Cipher, AnchorMsgId) ->
    Attach = #{
        %% file_hash256（SHA-256）仅作完整性参考，不作安全边界。
        %% 双读兼容：新客户端传 file_hash256，旧客户端过渡期仍传 md5。
        %% ⚠️ E2EE-061 拍板 ①：cipher 非 NULL 时这里存的是**密文**哈希，
        %% 明文哈希只在客户端加密的 attachment_descriptor 内，永不到达服务端。
        <<"file_hash256">> => maps:get(
            <<"file_hash256">>, Meta, maps:get(<<"md5">>, Meta, <<>>)
        ),
        %% 密文判别位（迁移 00000052）。null = 明文对象，旧客户端语义完全不变。
        <<"cipher">> => Cipher,
        %% mime_type/size 一律采用服务端 HEAD 核实的真实值
        <<"mime_type">> => RealType,
        <<"name">> => filename:basename(ObjectKey),
        %% path/url 存 ObjectKey（受限读取经 view_url 签发，public 直读公开 URL）
        <<"path">> => ObjectKey,
        <<"url">> => ObjectKey,
        <<"size">> => RealSize,
        <<"scope">> => Scope,
        <<"scope_ref">> => normalize_ref(ScopeRef),
        <<"anchor_msg_id">> => AnchorMsgId
    },
    Now = elib_dt:now(),
    try
        SaveResult =
            elib_pg:with_tx(fun(Conn) ->
                %% T7 归档写守卫（A2 收口）：附件转正落库（attachment 元数据写）与
                %% 守卫同事务——scope=group/channel 时按 {group, Ref}/{channel, Ref}
                %% 按 Org/成员 → Workspace 锁序重验资格；archived 拒绝（错误码 980）；
                %% c2c/moment/private/public 为个人域直通（T7 结项：scope 落库不可变
                %% + 读 ACL 恒绑定原 scope，无进 workspace 路径——设计决定非遗漏，
                %% 判定依据详见 workspace_resolver 的 attachment 子句注释）。
                case attach_scope_target(Scope, ScopeRef) of
                    {ok, Target} ->
                        ok = workspace_guard:abort_on_error(
                            attachment_ds:ensure_upload_scope_tx(Conn, Target, Uid)
                        ),
                        attachment_ds:save_confirmed_tx(Conn, Now, Uid, [Attach]);
                    passthrough ->
                        %% W2-02：个人域 confirm 同样 insert-if-absent（重放
                        %% 不递增 referer；真实引用新增仍走 save/4）。
                        case attachment_repo:confirm_if_absent_tx(Conn, Now, Uid, Attach) of
                            {ok, _Id, _First} -> ok;
                            {error, Reason} -> throw({abort_tx, Reason})
                        end
                end
            end),
        case SaveResult of
            {error, SaveReason} ->
                %% 归档拒绝（{980, Msg}）等事务错误原样返回，不吞成 badmatch
                ?ERROR_LOG(["attach_logic confirm save tx failed: ", SaveReason]),
                {error, SaveReason};
            ok ->
                %% #20：转正后销账。放在事务**之后**——销早了若落库失败，
                %% 对象既不在 attachment 表也不在 pending 表，就永久收不回了。
                %% 销账失败也不会误删：attach_pending_repo:list_expired/1 带
                %% `NOT EXISTS (attachment.path = object_key)` 守卫，已转正的对象
                %% 清理器一律不碰。但仍要留痕，残留行会一直被扫到。
                %% 同样必须 try/catch：这段在外层 try 里，pending_remove 抛异常会被
                %% 外层 catch 成 {error, Reason} —— 附件其实**已经落库成功**了，却给
                %% 调用方返回失败，客户端会重试或提示上传失败。销账失败无伤大雅
                %% （list_expired 有 NOT EXISTS 守卫兜底），绝不能反过来污染 confirm 结果。
                _ =
                    try attachment_ds:pending_remove(ObjectKey) of
                        ok ->
                            ok;
                        {error, PendReason} ->
                            ?ERROR_LOG([
                                "attach_logic confirm pending_remove failed: ",
                                ObjectKey,
                                PendReason
                            ])
                    catch
                        PClass:PReason ->
                            ?ERROR_LOG([
                                "attach_logic confirm pending_remove crashed: ",
                                ObjectKey,
                                PClass,
                                PReason
                            ])
                    end,
                %% Step 10：confirm 响应补 attachment_id（字符串，教学契约 TSID；
                %% 保留 object_key 向后兼容；查询失败不阻断——旧字段仍可用）
                {ok, maybe_public_url(Scope, ObjectKey, confirm_payload(ObjectKey))}
        end
    catch
        Class:Reason ->
            ?ERROR_LOG(["attach_logic confirm save failed: ", Class, Reason]),
            {error, Reason}
    end.

%% @doc T7 归档写守卫目标映射：group/channel 附件按 scope_ref 解析目标资源；
%% 其余 scope（c2c/moment/private/public）为个人域直通（T7 结项结论：
%% attachment.scope 落库后不可变、读 ACL 恒绑定原 scope、进群/频道的唯一
%% 途径是上传时即带 group/channel scope——个人域附件不存在落入 workspace
%% 范围的路径，守卫 N/A 是设计决定而非遗漏）。
%% Step 10：confirm 成功 payload = {object_key（兼容保留）, attachment_id（TSID 字符串）}
%% 落库为 upsert（uk path），保存成功后按 path 回读 id；回读失败仅返回旧字段
-spec confirm_payload(binary()) -> map().
confirm_payload(ObjectKey) ->
    Base = #{<<"object_key">> => ObjectKey},
    try attachment_ds:find_by_path(ObjectKey) of
        {ok, #{<<"id">> := AttId}} when is_integer(AttId) ->
            Base#{<<"attachment_id">> => integer_to_binary(AttId)};
        _ ->
            Base
    catch
        _:_ ->
            Base
    end.

-spec attach_scope_target(binary(), binary() | undefined) ->
    {ok, {group | channel, integer() | binary()}} | passthrough.
attach_scope_target(<<"group">>, Ref) when Ref =/= undefined, Ref =/= <<>>, Ref =/= null ->
    {ok, {group, Ref}};
attach_scope_target(<<"channel">>, Ref) when Ref =/= undefined, Ref =/= <<>>, Ref =/= null ->
    {ok, {channel, Ref}};
attach_scope_target(_Scope, _Ref) ->
    passthrough.

%% ===================================================================
%% 上传权校验 can_upload/3（confirm/presign 共用）
%% ===================================================================

-spec can_upload(integer(), binary(), binary() | undefined) ->
    ok | {error, forbidden | upload_not_supported}.
can_upload(_Uid, <<"public">>, _ScopeRef) ->
    ok;
can_upload(_Uid, <<"private">>, _ScopeRef) ->
    ok;
can_upload(_Uid, <<"c2c">>, undefined) ->
    {error, forbidden};
can_upload(Uid, <<"c2c">>, ScopeRef) when is_binary(ScopeRef) ->
    case conv_key_vo:c2c_members(ScopeRef) of
        {ok, {A, B}} when Uid =:= A; Uid =:= B -> ok;
        _ -> {error, forbidden}
    end;
can_upload(Uid, <<"group">>, ScopeRef) ->
    case to_int(ScopeRef) of
        {ok, Gid} ->
            case group_member_ds:is_member(Gid, Uid) of
                true -> ok;
                false -> {error, forbidden}
            end;
        error ->
            {error, forbidden}
    end;
%% channel：发帖人须是该频道订阅者（scope_ref=频道 id），与读鉴权
%% authorize/2 同款判定。此前是 upload_not_supported 占位：客户端频道发帖页
%% 标 scope=channel 后 presign 被拒 → 新图发不出去；不标则落库 private，
%% 订阅者 view_url 授权失败（BUG#124）。
can_upload(Uid, <<"channel">>, ScopeRef) ->
    case to_int(ScopeRef) of
        {ok, ChannelId} ->
            case has_channel_attachment_access(Uid, ChannelId) of
                true -> ok;
                false -> {error, forbidden}
            end;
        error ->
            {error, forbidden}
    end;
%% moment：任何登录用户可为自己的动态上传媒体（scope_ref 发帖时未知，
%% 上传放行；真正可见性在读时由 authorize_moment 按帖子 ACL 卡）。
%% 发帖后 moment_logic:create_post 会把这些附件 scope_ref 回填成 momentId。
can_upload(_Uid, <<"moment">>, _ScopeRef) ->
    ok;
%% Step 10 教学附件：上传人须持有至少一个有效教学身份（active guardian/staff）；
%% 强归属（附件只能进本人提交）在 create_submission 的 validate_assets 把关
can_upload(Uid, <<"teaching">>, _ScopeRef) ->
    case moya_attach_logic:can_upload(Uid) of
        ok -> ok;
        false -> {error, forbidden}
    end;
can_upload(_Uid, _Scope, _ScopeRef) ->
    {error, forbidden}.

%% ===================================================================
%% 读取鉴权 view_url/2 + authorize/2（六分支全就位）
%% ===================================================================

-spec view_url(integer(), binary()) -> {ok, binary()} | {error, forbidden}.
view_url(Uid, ObjectKey) ->
    case attachment_ds:find_by_path(ObjectKey) of
        {ok, Rec} ->
            case authorize(Uid, Rec) of
                true -> authorized_view(Uid, Rec, ObjectKey);
                false -> {error, forbidden}
            end;
        {error, not_found} ->
            {error, forbidden};
        {error, Reason} ->
            %% fail-closed：归属查询失败时拒绝签发（不降级放行），
            %% 避免存储/DB 故障成为越权下载入口。
            ?ERROR_LOG([
                "attach_logic:view_url lookup failed (deny), uid=",
                Uid,
                " key=",
                ObjectKey,
                " reason=",
                Reason
            ]),
            {error, forbidden}
    end.

%% Group/channel/moment URLs are checked again on every HTTP read.
authorized_view(Uid, #{<<"storage_driver">> := <<"disk">>}, ObjectKey) ->
    ticket_view(Uid, ObjectKey);
authorized_view(Uid, #{<<"scope">> := Scope}, ObjectKey) when
    Scope =:= <<"group">>; Scope =:= <<"channel">>; Scope =:= <<"moment">>
->
    ticket_view(Uid, ObjectKey);
authorized_view(_, Rec, ObjectKey) ->
    sign_view(Rec, ObjectKey).

ticket_view(Uid, ObjectKey) ->
    case attachment_download_ticket:issue(Uid, ObjectKey, config_ds:env(jwt_key, <<>>)) of
        {ok, Ticket} ->
            Base = config_ds:env(base_url, <<>>),
            case uri_string:parse(Base) of
                #{scheme := Scheme, host := Host} when
                    (Scheme =:= <<"http">> orelse Scheme =:= <<"https">>), Host =/= <<>>
                ->
                    Query = uri_string:compose_query([{<<"ticket">>, Ticket}]),
                    {ok, <<
                        (string:trim(Base, trailing, "/"))/binary,
                        "/api/v1/attachment/content?",
                        Query/binary
                    >>};
                _ ->
                    {error, forbidden}
            end;
        _ ->
            {error, forbidden}
    end.

download(Ticket) ->
    case attachment_download_ticket:verify(Ticket, config_ds:env(jwt_key, <<>>)) of
        {ok, Uid, ObjectKey} -> download_authorized(Uid, ObjectKey);
        _ -> {error, forbidden}
    end.

download_authorized(Uid, ObjectKey) ->
    case attachment_ds:find_by_path(ObjectKey) of
        {ok, Rec} ->
            case authorize(Uid, Rec) of
                true -> download_target(Rec, ObjectKey);
                false -> {error, forbidden}
            end;
        _ ->
            {error, forbidden}
    end.
download_target(#{<<"storage_driver">> := <<"disk">>} = Rec, Key) ->
    case attachment_storage:frozen_location(Key, Rec) of
        {ok, I} when is_map(I) -> {ok, {disk, I}};
        _ -> {error, forbidden}
    end;
download_target(Rec, Key) ->
    sign_view(Rec, Key).

%% @doc 鉴权通过后出 URL：public 直读公开 URL（不签名），受限资源签短时 GET。
-spec sign_view(map(), binary()) -> {ok, binary()}.
sign_view(#{<<"storage_driver">> := <<"garage">>} = Rec, Key) ->
    case attachment_storage:frozen_location(Key, Rec) of
        {ok, I} when is_map(I) -> attachment_storage_garage:presign_snapshot(I, ?GET_EXPIRES);
        _ -> {error, forbidden}
    end;
sign_view(#{<<"scope">> := <<"public">>}, <<"u", _/binary>> = ObjectKey) ->
    {ok, elib_oss:public_url_for_key(ObjectKey)};
sign_view(Rec, <<"/", _/binary>> = ObjectKey) ->
    %% 旧 go-fastdfs 历史附件：path 以 / 开头，非 Garage object_key（Garage key 恒为 u<Uid>/）。
    %% Garage 无此对象，签 GET 必 404 → 回退返回存储 url（经 nginx 反代可达）；
    %% url 缺失则用 path 本身（相对 fastdfs URL）。
    Url =
        case maps:get(<<"url">>, Rec, null) of
            null -> ObjectKey;
            <<>> -> ObjectKey;
            U -> U
        end,
    {ok, Url};
sign_view(Rec, ObjectKey) ->
    Scope = maps:get(<<"scope">>, Rec, <<"private">>),
    Bucket = elib_oss:get_bucket(Scope),
    {ok, elib_oss:presign_get_for_key(Bucket, ObjectKey, ?GET_EXPIRES)}.

%% @doc 读归属鉴权分发：Rec = #{scope, scope_ref, creator_user_id}
-spec authorize(integer(), map()) -> boolean().
authorize(_, #{<<"status">> := Status}) when not is_integer(Status); Status < 0 ->
    false;
authorize(Uid, Rec) ->
    authorize(maps:get(<<"scope">>, Rec, <<"private">>), Uid, Rec).

-spec authorize(binary(), integer(), map()) -> boolean().
authorize(<<"public">>, _Uid, _Rec) ->
    true;
authorize(<<"private">>, Uid, Rec) ->
    Uid =:= maps:get(<<"creator_user_id">>, Rec, 0);
%% Step 10 教学附件：必须已绑定 submission（未绑定→拒绝）；submission 未撤回；
%% moya_acl:submission_access 复用（guardian can_view_review / 本班 staff，MEDIA-01）
authorize(<<"teaching">>, Uid, Rec) ->
    moya_attach_logic:authorize(Uid, Rec);
authorize(<<"c2c">>, Uid, Rec) ->
    case scope_ref(Rec) of
        ScopeRef when is_binary(ScopeRef) ->
            case conv_key_vo:c2c_members(ScopeRef) of
                {ok, {A, B}} -> Uid =:= A orelse Uid =:= B;
                _ -> false
            end;
        _ ->
            false
    end;
authorize(<<"group">>, Uid, Rec) ->
    case maps:get(<<"path">>, Rec, undefined) of
        ObjectKey when is_binary(ObjectKey), ObjectKey =/= <<>> ->
            attachment_ds:authorize_group_access(ObjectKey, Uid);
        _ ->
            false
    end;
authorize(<<"channel">>, Uid, Rec) ->
    case to_int(scope_ref(Rec)) of
        {ok, ChannelId} -> has_channel_attachment_access(Uid, ChannelId);
        error -> false
    end;
authorize(<<"moment">>, Uid, Rec) ->
    authorize_moment_scope(Uid, Rec);
authorize(_Scope, _Uid, _Rec) ->
    false.

%% BUILD-00R：moment 未编译时 scope 走 fail-closed（false），不引用 moment_ds
-ifdef(IMBOY_FEATURE_MOMENT).
-spec authorize_moment_scope(integer(), map() | list()) -> boolean().
authorize_moment_scope(Uid, Rec) ->
    case to_int(scope_ref(Rec)) of
        {ok, MomentId} -> authorize_moment(Uid, MomentId);
        error -> false
    end.

%% @doc 朋友圈动态：先取 Post 再按可见性规则判定（复用 moment_ds 现成 ACL）
-spec authorize_moment(integer(), integer()) -> boolean().
authorize_moment(Uid, MomentId) ->
    case moment_ds:get_post(MomentId) of
        Post when is_map(Post), map_size(Post) > 0 ->
            moment_ds:can_view_post(Uid, Post);
        _ ->
            false
    end.
-else.
-spec authorize_moment_scope(integer(), map() | list()) -> boolean().
authorize_moment_scope(_Uid, _Rec) ->
    false.
-endif.

%% 频道附件必须跟频道正文使用同一权益模型：免费/私有频道保持原有的
%% 「订阅者可访问」语义；付费频道只接受有效购买或频道管理角色，不能因为
%% 历史脏订阅而绕过正文的购买校验。
%% 频道管理角色（创建者/管理员 role>0）在所有类型下都有附件读写权——
%% 与 ensure_channel_content_access 的角色短路一致：创建者发帖不强制先订阅，
%% 否则创建者发语音/图片会在 presign 被 403（BUG：公开频道创建者发语音
%% 「无权向该范围上传」）。
-spec has_channel_attachment_access(integer(), integer()) -> boolean().
has_channel_attachment_access(Uid, ChannelId) ->
    attachment_ds:authorize_channel_scope(ChannelId, Uid) andalso
        has_channel_attachment_entitlement(Uid, ChannelId).

-spec has_channel_attachment_entitlement(integer(), integer()) -> boolean().
has_channel_attachment_entitlement(Uid, ChannelId) ->
    case channel_logic_common:get_user_role(ChannelId, Uid) of
        Role when is_integer(Role), Role > 0 ->
            true;
        _ ->
            case channel_ds:find_by_id(ChannelId, <<"id,access_type,status">>) of
                #{<<"access_type">> := 1} -> paid_channel_attachment_access(Uid, ChannelId);
                #{<<"access_type">> := _} -> channel_subscription_ds:is_subscribed(ChannelId, Uid);
                _ -> false
            end
    end.

-spec paid_channel_attachment_access(integer(), integer()) -> boolean().
paid_channel_attachment_access(Uid, ChannelId) ->
    case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
        ok -> true;
        _ -> false
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

-spec maybe_public_url(binary(), binary(), map()) -> map().
maybe_public_url(<<"public">>, ObjectKey, Map) ->
    Map#{<<"public_url">> => elib_oss:public_url_for_key(ObjectKey)};
maybe_public_url(_Scope, _ObjectKey, Map) ->
    Map.

%% @doc V5 受控网关 PUT 地址：路径段是会话 client_upload_id（UUID，单段）；
%% 真实 ObjectKey/mime/到期都在 capability token claims 内，网关不信任路径寻址。
%% token 经 query 传递（客户端无 header 能力时也可用），网关侧与
%% x-upload-capability header 等价接受。
-spec gateway_put_url(binary(), binary()) -> binary().
gateway_put_url(ClientUploadId, Capability) ->
    <<"/api/v1/attachment/gateway/", ClientUploadId/binary, "?capability=", Capability/binary>>.

-spec scope_ref(map()) -> binary() | undefined.
scope_ref(Rec) ->
    case maps:get(<<"scope_ref">>, Rec, undefined) of
        null -> undefined;
        V -> V
    end.

-spec to_int(binary() | integer() | undefined) -> {ok, integer()} | error.
to_int(I) when is_integer(I) ->
    {ok, I};
to_int(B) when is_binary(B) ->
    try
        {ok, binary_to_integer(B)}
    catch
        _:_ -> error
    end;
to_int(_) ->
    error.

-spec normalize_ref(binary() | undefined) -> binary() | null.
normalize_ref(undefined) -> null;
normalize_ref(<<>>) -> null;
normalize_ref(R) -> R.
