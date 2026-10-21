-module(attach_direct_logic).
-moduledoc "direct-v1 附件直传会话逻辑（step-6，协议 docs/architecture/attachment-direct-transfer-v1-protocol.md）。".
%%%
% direct-v1 会话状态机编排：
%   register（RESERVED，无写能力/R08，位置四元组冻结）→ declare（expected
%   冻结 DECLARED）→ grant（COMMIT 后签发内容绑定 staging s3_put 描述符 / I02）
%   → [客户端 staging PUT] → finalize（dispatch CAS 先持久化→网络 Copy→
%   candidate proof→短事务 COPY_COMPLETED→PROOF_VERIFIED→winner pin+confirm）
%
% 安全合同落点：
%   * 新协议分派不进入 verify_and_save_v5 → v5_object_bytes → read_snapshot
%     全量 GET 链（R01/G15）；confirm 由 winner_pin（迁移 200）原子完成。
%   * 每可能写入恰一次 dispatch：CREATED→DISPATCHED CAS 在发包前提交；
%     Copy 超时/响应丢失/kill → UNKNOWN_QUARANTINED，本轮无晋升边（§3.2），
%     重试只能新 attempt+新 target。
%   * 网络操作一律在 DB 事务外（I10）；短事务只做状态与授权。
%   * grant URL 绑定 content-type 与 x-amz-checksum-sha256（Base64 原始
%     32 字节摘要，I04）；无 sealed 写/删能力（I05）。
%
% 已知边界（evidence/step-6 limitations）：candidate 内容与 expected sha256
% 的一致性当前由「checksum-PUT 存储端验证 + Copy 服务端逐字节复制」链证明，
% 该链的真实 provider 行为归 A0/5a 认证；A0 未完成前 direct 不得启用（T47）。
%%%

-export([
    register/5,
    register_application/5,
    declare/4,
    grant/2,
    status/2,
    renew_grant/2,
    finalize/2,
    publish_avatar/3,
    reason_bin/1,
    require_application_session/1,
    reconcile/0
]).

-include("log.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% ===== register（RESERVED，无写能力）=====

%% @doc 同 client_upload_id 同声明幂等恢复（T02）；异声明 23510（T03）。
-spec register(
    integer() | binary(),
    binary(),
    binary() | undefined,
    binary() | undefined,
    binary()
) ->
    {ok, map()} | {error, term()}.
register(Uid, Scope, ScopeRef, ClientUploadId, FileName) ->
    ObjectKey = elib_oss:build_object_key(Uid, Scope, ScopeRef, FileName),
    %% elib_oss:default_bucket/0 未导出；同语义直接读 garage 配置
    Bucket = maps:get(bucket, application:get_env(imboy, garage, #{}), <<"imboy">>),
    %% 客户端未提供 id 时落 NULL（与 V5 同语义：该会话无幂等恢复键）
    Uuid =
        case ClientUploadId of
            undefined -> null;
            Bin when is_binary(Bin), byte_size(Bin) =:= 36 -> Bin;
            _ -> null
        end,
    %% 位置四元组在 register 冻结（V5 同合同；grant/winner 不重猜位置）
    case attachment_storage:registration_location(Scope, Bucket, ObjectKey) of
        {ok, #{
            driver := Driver,
            namespace := Ns,
            container := Container,
            locator_digest := Locator
        }} ->
            SafeName = filename:basename(FileName),
            Sql = <<"SELECT fn_dv1_register($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12) AS key">>,
            case
                elib_pg:query(Sql, [
                    ObjectKey,
                    Bucket,
                    Scope,
                    scope_ref_bin(ScopeRef),
                    Uid,
                    Uuid,
                    mime_of(FileName),
                    SafeName,
                    Driver,
                    Ns,
                    Container,
                    Locator
                ])
            of
                {ok, [#{<<"key">> := Key}]} ->
                    deadline_info(Key);
                {ok, []} ->
                    {error, register_not_returned};
                {error, #error{code = <<"23510">>}} ->
                    {error, declaration_conflict};
                {error, #error{code = <<"23514">>}} ->
                    {error, quota_exceeded};
                {error, R} ->
                    {error, R}
            end;
        {error, R} ->
            {error, R}
    end.

%% @doc application 主体变体（Admin AI 头像，协议 §5）：creator=0、
%% subject kind=application——绝不借用 Human 凭据（T28）。scope 通常
%% public（头像公开面；发布前对象仅在私有 winner，匿名不可读）。
-spec register_application(
    integer() | binary(),
    binary() | binary(),
    binary() | binary(),
    binary() | undefined,
    binary()
) ->
    {ok, map()} | {error, term()}.
register_application(AppId, OrgId, Scope, ClientUploadId, FileName) ->
    ObjectKey = application_object_key(AppId, FileName),
    Bucket = maps:get(bucket, application:get_env(imboy, garage, #{}), <<"imboy">>),
    Uuid =
        case ClientUploadId of
            undefined -> null;
            Bin when is_binary(Bin), byte_size(Bin) =:= 36 -> Bin;
            _ -> null
        end,
    case attachment_storage:registration_location(Scope, Bucket, ObjectKey) of
        {ok, #{
            driver := Driver,
            namespace := Ns,
            container := Container,
            locator_digest := Locator
        }} ->
            SafeName = filename:basename(FileName),
            Sql =
                <<"SELECT fn_dv1_register_application($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12) AS key">>,
            case
                elib_pg:query(Sql, [
                    ObjectKey,
                    Bucket,
                    Scope,
                    ec_cnv:to_binary(AppId),
                    ec_cnv:to_binary(OrgId),
                    Uuid,
                    mime_of(FileName),
                    SafeName,
                    Driver,
                    Ns,
                    Container,
                    Locator
                ])
            of
                {ok, [#{<<"key">> := Key}]} ->
                    deadline_info(Key);
                {ok, []} ->
                    {error, register_not_returned};
                {error, #error{code = <<"23510">>}} ->
                    {error, declaration_conflict};
                {error, #error{code = <<"23514">>}} ->
                    {error, quota_exceeded};
                {error, R} ->
                    {error, R}
            end;
        {error, R} ->
            {error, R}
    end.

%% 头像 key：app 前缀 + avatar 段（admin 身份域，不与 u<Uid> 混用）
application_object_key(AppId, FileName) ->
    Ymd = ymd_today(),
    Ext = string:lowercase(filename:extension(FileName)),
    Hex = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"app", (ec_cnv:to_binary(AppId))/binary, "/avatar/", Ymd/binary, "/", Hex/binary,
        Ext/binary>>.

ymd_today() ->
    {{Y, M, D}, _} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B~2..0B~2..0B", [Y, M, D])).

%% @doc avatar 业务发布（公开复制）：仅 application 主体的 CONFIRMED 会话可
%% 发布；私有 winner 字节复制到公开桶，成功才返回公开 URL（失败可重试：
%% winner/candidate 仍在，agent.avatar 由调用方在成功后更新——失败无错误
%% 头像引用）。匿名可见性 = 公开桶对象存在性（I13/协议 §5）。
%% 主体断言（review P1 修复）：Admin 面入口只允许 application 会话——
%% Human 用户会话绝不能经 publish 公开（权限边界：Admin ≠ 读任意用户附件）。
-spec publish_avatar(binary(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
publish_avatar(ObjectKey, AgentRef, _SecretPlaceholder) ->
    case session_row(ObjectKey) of
        {ok, Row = #{<<"direct_state">> := <<"CONFIRMED">>}} ->
            case subject_kind(Row) of
                <<"application">> -> publish_avatar_(Row, ObjectKey, AgentRef);
                _ -> {error, not_application_session}
            end;
        {ok, #{<<"direct_state">> := _}} ->
            {error, not_confirmed};
        {error, R} ->
            {error, R}
    end.

publish_avatar_(Row, ObjectKey, AgentRef) ->
    PublicKey = ObjectKey,
    case attachment_storage:frozen_location(ObjectKey, Row) of
        {ok, I} when is_map(I) ->
            case attachment_storage_garage:read_snapshot(I) of
                {ok, Bytes} ->
                    case
                        elib_oss:put_object(
                            elib_oss:get_bucket(<<"public">>),
                            PublicKey,
                            Bytes,
                            mime_from_row(Row)
                        )
                    of
                        ok ->
                            {ok, #{
                                public_url => elib_oss:public_url_for_key(PublicKey),
                                public_key => PublicKey,
                                agent_ref => AgentRef
                            }};
                        {error, R} ->
                            %% 公开复制失败可恢复：winner 未动，重试即可
                            {error, {publish_failed, R}}
                    end;
                {error, R} ->
                    {error, {winner_read_failed, R}}
            end;
        _ ->
            {error, location_unverified}
    end.

%% @doc Admin 身份域（avatar_direct）会话主体断言：仅 application 主体会话
%% 可经 Admin 面操作。declare/grant/finalize 等通用函数按 key 寻址不校验
%% 主体——Admin 入口必须先过本断言，防止对 Human 用户会话越权推进/发布。
-spec require_application_session(binary()) ->
    ok | {error, not_application_session | not_found | term()}.
require_application_session(ObjectKey) ->
    case session_row(ObjectKey) of
        {ok, Row} ->
            case subject_kind(Row) of
                <<"application">> -> ok;
                _ -> {error, not_application_session}
            end;
        {error, R} ->
            {error, R}
    end.

subject_kind(Row) ->
    case maps:get(<<"upload_registration">>, Row, #{}) of
        #{<<"subject">> := #{<<"kind">> := Kind}} when is_binary(Kind) -> Kind;
        _ -> <<>>
    end.

%% ===== declare（expected 冻结）=====

%% @doc 幂等同值；异值 {error, expected_conflict}（T03）。
-spec declare(binary(), binary(), non_neg_integer(), binary()) -> ok | {error, term()}.
declare(ObjectKey, Sha256Hex, Size, Mime) ->
    %% T08 硬限额：普通附件 104857600，超限在声明点拒绝（checksum 绑定使
    %% 声明即实际上限；不留已上传残留）
    case Size > elib_oss:max_file_size() orelse Size < 0 of
        true -> {error, size_limit_exceeded};
        false -> declare_(ObjectKey, Sha256Hex, Size, Mime)
    end.

declare_(ObjectKey, Sha256Hex, Size, Mime) ->
    case
        elib_pg:query(
            <<"SELECT fn_dv1_declare($1,$2,$3,$4)">>,
            [ObjectKey, Sha256Hex, Size, Mime]
        )
    of
        {ok, _} -> ok;
        {error, #error{code = <<"23510">>}} -> {error, expected_conflict};
        {error, #error{code = <<"P0002">>}} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% ===== grant（COMMIT 后签发）=====

%% @doc DECLARED→GRANTED + attempt#1；返回 s3_put 描述符（仅 PUT、仅 staging）。
-spec grant(binary(), binary()) -> {ok, map()} | {error, term()}.
grant(ObjectKey, Sha256Hex) ->
    case session_row(ObjectKey) of
        {ok, Row = #{<<"direct_state">> := <<"DECLARED">>}} ->
            with_tx(fun(Conn) ->
                grant_tx(Conn, ObjectKey, Row, Sha256Hex, 1)
            end);
        {ok, #{<<"direct_state">> := _}} ->
            {error, illegal_state};
        {error, R} ->
            {error, R}
    end.

%% @doc 重试/续期：新 attempt + 新 target（I05）；预算 ≤3 fail-closed（40904）。
-spec renew_grant(binary(), binary()) -> {ok, map()} | {error, term()}.
renew_grant(ObjectKey, Sha256Hex) ->
    case session_row(ObjectKey) of
        {ok, Row = #{<<"direct_state">> := State}} when
            State =:= <<"GRANTED">> orelse State =:= <<"FINALIZING">>
        ->
            with_tx(fun(Conn) ->
                StagingKey = staging_key(ObjectKey, next_no),
                TargetKey = candidate_key(ObjectKey, next_no),
                case
                    attach_direct_attempt_repo:create_tx(
                        Conn,
                        ObjectKey,
                        StagingKey,
                        TargetKey,
                        maps:get(<<"storage_driver">>, Row),
                        maps:get(<<"storage_namespace">>, Row),
                        maps:get(<<"storage_container">>, Row)
                    )
                of
                    {ok, No} ->
                        {ok, descriptor(ObjectKey, Row, No, StagingKey, Sha256Hex)};
                    {error, R} ->
                        {error, R}
                end
            end);
        {ok, #{<<"direct_state">> := _}} ->
            {error, illegal_state};
        {error, R} ->
            {error, R}
    end.

grant_tx(Conn, ObjectKey, Row, Sha256Hex, _AttemptNo) ->
    StagingKey = staging_key(ObjectKey, next_no),
    TargetKey = candidate_key(ObjectKey, next_no),
    Profile = attach_direct_attempt_repo:direct_profile_digest(),
    case
        elib_pg:query(
            Conn,
            <<"SELECT fn_dv1_grant($1,$2,$3,$4,$5,$6,$7) AS no">>,
            [
                ObjectKey,
                StagingKey,
                TargetKey,
                maps:get(<<"storage_driver">>, Row),
                maps:get(<<"storage_namespace">>, Row),
                maps:get(<<"storage_container">>, Row),
                Profile
            ]
        )
    of
        {ok, [#{<<"no">> := No}]} ->
            {ok, descriptor(ObjectKey, Row, No, StagingKey, Sha256Hex)};
        {ok, []} ->
            {error, grant_not_returned};
        {error, R} ->
            {error, R}
    end.

%% ===== finalize（单次 dispatch → proof → winner pin）=====

%% @doc 对指定 attempt 单次 dispatch；重复调用同 attempt 23510 幂等拒绝。
-spec finalize(binary(), integer()) -> {ok, map()} | {error, term()}.
finalize(ObjectKey, AttemptNo) when is_integer(AttemptNo), AttemptNo >= 1 ->
    case session_row(ObjectKey) of
        {ok, Row} -> do_finalize(ObjectKey, Row, AttemptNo);
        {error, R} -> {error, R}
    end.

do_finalize(ObjectKey, Row, AttemptNo) ->
    DirectState = maps:get(<<"direct_state">>, Row, null),
    case DirectState of
        <<"GRANTED">> ->
            case begin_finalize(ObjectKey, AttemptNo) of
                ok -> after_begin(ObjectKey, Row, AttemptNo);
                {error, R} -> {error, R}
            end;
        <<"FINALIZING">> ->
            after_begin(ObjectKey, Row, AttemptNo);
        _ ->
            {error, illegal_state}
    end.

after_begin(ObjectKey, Row, AttemptNo) ->
    case
        with_tx(fun(Conn) ->
            attach_direct_attempt_repo:get_tx(Conn, ObjectKey, AttemptNo)
        end)
    of
        {ok, #{<<"attempt_state">> := <<"CREATED">>} = Attempt} ->
            dispatch_and_prove(ObjectKey, Row, Attempt);
        {ok, #{<<"attempt_state">> := _Other}} ->
            %% 非 CREATED：或已 dispatch（重复调用）或终态——拒绝再次写授权
            {error, attempt_not_dispatchable};
        {error, R} ->
            {error, R}
    end.

dispatch_and_prove(ObjectKey, Row, Attempt) ->
    AttemptNo = ec_cnv:to_integer(maps:get(<<"attempt_no">>, Attempt)),
    SourceKey = maps:get(<<"source_key">>, Attempt),
    TargetKey = maps:get(<<"target_key">>, Attempt),
    Ns = maps:get(<<"target_namespace">>, Attempt),
    Container = maps:get(<<"target_container">>, Attempt),
    %% 写授权持久化（发包前 CAS，I12）：CREATED→DISPATCHED
    case
        with_tx(fun(Conn) ->
            attach_direct_attempt_repo:mark_dispatched_tx(Conn, ObjectKey, AttemptNo)
        end)
    of
        ok ->
            Outcome = run_copy(Ns, Container, SourceKey, TargetKey),
            settle_outcome(ObjectKey, Row, Attempt, Outcome);
        {error, _CasR} ->
            {error, dispatch_cas_lost}
    end.

%% 网络（无 DB 事务）：Copy → candidate 真 HEAD（§3.1 顺序）
run_copy(Ns, Container, SourceKey, TargetKey) ->
    case attachment_storage_garage:copy_dispatch(Ns, Container, SourceKey, TargetKey) of
        {ok, _CopyInfo} ->
            case attachment_storage_garage:head_bounded(Ns, Container, TargetKey) of
                {ok, #{size := Size}} -> {copy_ok, Size};
                {error, not_found} -> {unknown, candidate_head_missing};
                {error, _} -> {unknown, candidate_head_unverified}
            end;
        {error, not_found} ->
            {rejected, copy_source_missing};
        {error, {storage_status, S}} when S >= 400 -> {rejected, {storage_status, S}};
        {error, copy_xml_error} ->
            {rejected, copy_xml_error};
        {error, copy_no_result} ->
            {unknown, copy_result_missing};
        {error, copy_xml_unrecognized} ->
            {unknown, copy_unrecognized};
        {error, timeout} ->
            {unknown, copy_timeout};
        {error, body_too_large} ->
            {unknown, copy_response_lost};
        {error, _Other} ->
            {unknown, copy_unknown}
    end.

settle_outcome(ObjectKey, Row, Attempt, {copy_ok, Size}) ->
    Expected = maps:get(<<"direct_expected">>, Row, null),
    {ExpSize, ExpSha} = expected_of(Expected),
    AttemptNo = ec_cnv:to_integer(maps:get(<<"attempt_no">>, Attempt)),
    if
        ExpSize =:= undefined orelse ExpSha =:= undefined ->
            quarantine_or_reject(ObjectKey, Row, Attempt, {copy_ok, Size});
        ExpSize =/= Size ->
            %% Copy 实际成功但 size 与 expected 不符：candidate 不能成为 winner
            reject_copy(ObjectKey, AttemptNo, size_mismatch);
        true ->
            confirm_winner(ObjectKey, Row, Attempt, ExpSha, Size)
    end;
settle_outcome(ObjectKey, _Row, Attempt, {rejected, Reason}) ->
    reject_copy(ObjectKey, ec_cnv:to_integer(maps:get(<<"attempt_no">>, Attempt)), Reason);
settle_outcome(ObjectKey, _Row, Attempt, {unknown, Reason}) ->
    %% §3.2：UNKNOWN 只探测/隔离；不重发原 target，不因 HEAD 相同晋升
    AttemptNo = ec_cnv:to_integer(maps:get(<<"attempt_no">>, Attempt)),
    with_tx(fun(Conn) ->
        attach_direct_attempt_repo:record_outcome_tx(
            Conn,
            ObjectKey,
            AttemptNo,
            unknown_quarantined,
            reason_bin(Reason)
        )
    end),
    {ok, #{
        state => <<"FINALIZING">>,
        attempt_id => AttemptNo,
        outcome => <<"UNKNOWN_QUARANTINED">>,
        reason => Reason
    }}.

expected_of(null) ->
    {undefined, undefined};
expected_of(Expected) when is_map(Expected) ->
    {ec_cnv:to_integer(maps:get(<<"size_bytes">>, Expected)), maps:get(<<"sha256">>, Expected)};
expected_of(Expected) when is_binary(Expected) ->
    expected_of(jsone:decode(Expected)).

%% copy_ok 但 expected 缺失（理论不可达：grant 前必须 declare）——保守拒绝
quarantine_or_reject(ObjectKey, _Row, Attempt, {copy_ok, _Size}) ->
    reject_copy(
        ObjectKey,
        ec_cnv:to_integer(maps:get(<<"attempt_no">>, Attempt)),
        expected_missing
    ).

%% DISPATCHED → COPY_COMPLETED → REJECTED（两跳 CAS，白名单强制）
reject_copy(ObjectKey, AttemptNo, Reason) ->
    _ = with_tx(fun(Conn) ->
        attach_direct_attempt_repo:record_outcome_tx(
            Conn,
            ObjectKey,
            AttemptNo,
            copy_completed,
            reason_bin(Reason)
        )
    end),
    case
        with_tx(fun(Conn) ->
            attach_direct_attempt_repo:transition_tx(
                Conn,
                ObjectKey,
                AttemptNo,
                <<"COPY_COMPLETED">>,
                <<"REJECTED">>,
                reason_bin(Reason)
            )
        end)
    of
        ok -> {error, {rejected, Reason}};
        {error, cas_lost} -> {error, {rejected, Reason}};
        {error, R} -> {error, R}
    end.

%% 短事务：DISPATCHED→COPY_COMPLETED→PROOF_VERIFIED→attachment 行→winner pin
confirm_winner(ObjectKey, Row, Attempt, Sha256Hex, Size) ->
    AttemptNo = ec_cnv:to_integer(maps:get(<<"attempt_no">>, Attempt)),
    TargetKey = maps:get(<<"target_key">>, Attempt),
    Uid = ec_cnv:to_integer(maps:get(<<"creator_user_id">>, Row)),
    Scope = maps:get(<<"scope">>, Row),
    Result = with_tx(fun(Conn) ->
        case
            attach_direct_attempt_repo:record_outcome_tx(
                Conn,
                ObjectKey,
                AttemptNo,
                copy_completed,
                undefined
            )
        of
            ok ->
                case
                    attach_direct_attempt_repo:record_proof_tx(
                        Conn,
                        ObjectKey,
                        AttemptNo,
                        Sha256Hex,
                        Size
                    )
                of
                    ok ->
                        confirm_attach_and_pin(
                            Conn,
                            ObjectKey,
                            Row,
                            Attempt,
                            TargetKey,
                            Sha256Hex,
                            Size,
                            Uid,
                            Scope
                        );
                    {error, R} ->
                        {error, R}
                end;
            {error, R} ->
                {error, R}
        end
    end),
    case Result of
        ok ->
            {ok, #{
                state => <<"CONFIRMED">>,
                attempt_id => AttemptNo,
                object_key => ObjectKey,
                size => Size
            }};
        {error, R} ->
            {error, R}
    end.

confirm_attach_and_pin(Conn, ObjectKey, Row, Attempt, TargetKey, Sha256Hex, Size, Uid, Scope) ->
    ScopeRef = scope_ref_from_row(Row),
    %% workspace 资格（与 V5 confirm 同守卫；group/channel 必须存在且成员有效）
    case attach_scope_target(Scope, ScopeRef) of
        {ok, Target} ->
            ok = workspace_guard:abort_on_error(
                attachment_ds:ensure_upload_scope_tx(Conn, Target, Uid)
            );
        passthrough ->
            ok;
        {error, Reason} ->
            throw({abort_tx, Reason})
    end,
    Attach = confirm_attach_map(ObjectKey, Scope, ScopeRef, Size, mime_from_row(Row), Sha256Hex),
    Location = #{
        <<"storage_driver">> => maps:get(<<"storage_driver">>, Row),
        <<"storage_namespace">> => maps:get(<<"storage_namespace">>, Row),
        <<"storage_container">> => maps:get(<<"storage_container">>, Row),
        %% 位置 locator 沿用 register 冻结值（winner_pin 内以 pending 冻结值为准）
        <<"storage_locator_sha256">> => maps:get(<<"storage_locator_sha256">>, Row),
        %% direct-v1：物理键与逻辑键分离（迁移 00000200）
        <<"storage_physical_key">> => TargetKey
    },
    ok = attachment_ds:save_confirmed_tx(
        Conn, elib_dt:now(), Uid, [maps:merge(Attach, Location)]
    ),
    case
        attach_direct_attempt_repo:winner_pin_tx(
            Conn,
            ObjectKey,
            ec_cnv:to_integer(maps:get(<<"attempt_no">>, Attempt)),
            TargetKey,
            Sha256Hex
        )
    of
        ok -> ok;
        {error, winner_race_lost} -> throw({abort_tx, confirm_race});
        {error, R} -> {error, R}
    end.

attach_scope_target(<<"group">>, Ref) when Ref =/= undefined, Ref =/= <<>>, Ref =/= null ->
    {ok, {group, Ref}};
attach_scope_target(<<"channel">>, Ref) when Ref =/= undefined, Ref =/= <<>>, Ref =/= null ->
    {ok, {channel, Ref}};
attach_scope_target(<<"group">>, _Ref) ->
    {error, missing_scope_ref};
attach_scope_target(<<"channel">>, _Ref) ->
    {error, missing_scope_ref};
attach_scope_target(_Scope, _Ref) ->
    passthrough.

confirm_attach_map(ObjectKey, Scope, ScopeRef, RealSize, RealType, Sha256) ->
    #{
        <<"file_hash256">> => Sha256,
        <<"cipher">> => null,
        <<"mime_type">> => RealType,
        <<"name">> => filename:basename(ObjectKey),
        <<"path">> => ObjectKey,
        <<"url">> => ObjectKey,
        <<"size">> => RealSize,
        <<"scope">> => Scope,
        %% 与 V5 confirm_attach_map 同合同：空 ref 归一为 null
        %% （save_confirmed_tx 回读比对 {creator,scope,ref}，undefined≠null）
        <<"scope_ref">> => normalize_ref(ScopeRef),
        <<"anchor_msg_id">> => undefined
    }.

normalize_ref(undefined) -> null;
normalize_ref(<<>>) -> null;
normalize_ref(Bin) when is_binary(Bin) -> Bin.

%% ===== status / reconcile =====

%% @doc 跨主体不泄露：非 owner 返回 not_found。
-spec status(binary(), integer() | binary()) -> {ok, map()} | {error, term()}.
status(ObjectKey, Uid) ->
    UidInt = ec_cnv:to_integer(Uid),
    case session_row(ObjectKey) of
        {ok, Row} ->
            case ec_cnv:to_integer(maps:get(<<"creator_user_id">>, Row)) of
                UidInt ->
                    Attempts =
                        case attach_direct_attempt_repo:list_for_session(ObjectKey) of
                            {ok, ARows} ->
                                %% 不回 target_key（物理键最小化：本接口只反映
                                %% 账本态，attempt/target 映射只存在 attempt 行）
                                [
                                    #{
                                        attempt_id => ec_cnv:to_integer(
                                            maps:get(<<"attempt_no">>, A)
                                        ),
                                        state => maps:get(<<"attempt_state">>, A)
                                    }
                                 || A <- ARows
                                ];
                            {error, _} ->
                                []
                        end,
                    {ok, #{
                        object_key => ObjectKey,
                        state => maps:get(<<"direct_state">>, Row, null),
                        session_state => maps:get(<<"session_state">>, Row),
                        expected => maps:get(<<"direct_expected">>, Row, null),
                        attempts => Attempts
                    }};
                _ ->
                    {error, not_found}
            end;
        {error, R} ->
            {error, R}
    end.

%% @doc 滞留 FINALIZING 的会话/attempt（恢复与 step-7 清理消费）。
-spec reconcile() -> {ok, [map()]} | {error, term()}.
reconcile() ->
    elib_pg:query(<<"SELECT * FROM fn_dv1_reconcile_list($1)">>, [200]).

%% ===== 内部 =====

begin_finalize(ObjectKey, AttemptNo) ->
    case elib_pg:query(<<"SELECT fn_dv1_begin_finalize($1,$2)">>, [ObjectKey, AttemptNo]) of
        {ok, _} -> ok;
        {error, #error{code = <<"23510">>}} -> {error, attempt_not_dispatchable};
        {error, #error{code = <<"P0002">>}} -> {error, not_found};
        {error, R} -> {error, R}
    end.

session_row(ObjectKey) ->
    case
        elib_pg:query(
            <<
                "SELECT object_key, bucket, scope, creator_user_id, session_state, direct_state,"
                " direct_expected, upload_registration, storage_driver, storage_namespace,"
                " storage_container, storage_locator_sha256,"
                " extract(epoch from session_deadline)::bigint AS deadline"
                " FROM attach_pending WHERE object_key=$1"
            >>,
            [ObjectKey]
        )
    of
        {ok, [Row]} -> {ok, decode_reg(Row)};
        {ok, []} -> {error, not_found};
        {error, R} -> {error, R}
    end.

decode_reg(Row) ->
    case maps:get(<<"upload_registration">>, Row, undefined) of
        B when is_binary(B) -> Row#{<<"upload_registration">> => jsone:decode(B)};
        _ -> Row
    end.

scope_ref_from_row(Row) ->
    case maps:get(<<"upload_registration">>, Row, #{}) of
        #{<<"declaration">> := #{<<"scope_ref">> := Ref}} when is_binary(Ref), Ref =/= <<>> -> Ref;
        %% application 主体（Admin AI 头像等）：guard 生成 'org:O/app:A'，
        %% attachment.scope_ref 必须同值（fn_v5_pending_session_guard 比对）
        #{<<"declaration">> := #{<<"application_id">> := App, <<"org_id">> := Org}} when
            is_binary(App), byte_size(App) > 0, is_binary(Org), byte_size(Org) > 0
        ->
            <<"org:", Org/binary, "/app:", App/binary>>;
        _ ->
            undefined
    end.

deadline_info(Key) ->
    case session_row(Key) of
        {ok, Row} ->
            {ok, #{
                object_key => Key,
                state => maps:get(<<"direct_state">>, Row),
                session_state => maps:get(<<"session_state">>, Row),
                deadline => maps:get(<<"deadline">>, Row)
            }};
        {error, R} ->
            {error, R}
    end.

with_tx(Fun) ->
    elib_pg:with_tx(Fun).

%% grant 描述符：s3_put 传输型（协议 §3）；required_headers 之外禁止扩展
%% x-amz-*/ACL/copy-source（T07）。checksum 为原始 32 字节摘要 Base64（I04）。
descriptor(ObjectKey, Row, AttemptNo, StagingKey, Sha256Hex) ->
    Container = maps:get(<<"storage_container">>, Row),
    Cfg = application:get_env(imboy, garage, #{}),
    Endpoint = maps:get(endpoint, Cfg, elib_oss:endpoint()),
    Now = calendar:universal_time(),
    Ttl = 3600,
    Mime = mime_from_row(Row),
    Url = elib_s3_sign:presign_signed_url_at(
        <<"PUT">>,
        Endpoint,
        Container,
        StagingKey,
        [
            {<<"content-type">>, Mime},
            {<<"x-amz-checksum-sha256">>, b64_sha256(Sha256Hex)}
        ],
        Now,
        Ttl,
        Cfg
    ),
    #{
        state => <<"GRANTED">>,
        object_key => ObjectKey,
        attempt_id => AttemptNo,
        grant => #{
            transport => <<"s3_put">>,
            url => Url,
            method => <<"PUT">>,
            container => Container,
            required_headers => [
                {<<"Content-Type">>, Mime},
                {<<"x-amz-checksum-sha256">>, b64_sha256(Sha256Hex)}
            ],
            expires_in => Ttl
        }
    }.

mime_from_row(Row) ->
    case maps:get(<<"upload_registration">>, Row, #{}) of
        #{<<"declaration">> := #{<<"mime">> := Mime}} when is_binary(Mime), byte_size(Mime) > 0 ->
            Mime;
        _ ->
            <<"application/octet-stream">>
    end.

%% sha256hex → 原始 32 字节 → Base64（I04：非 hex 文本）
b64_sha256(HexBin) when is_binary(HexBin), byte_size(HexBin) =:= 64 ->
    base64:encode(binary:decode_hex(HexBin)).

%% staging/candidate key 每 attempt 随机唯一（I05：staging 也是写目标）；
%% attempt_no 与 key 的映射只存在 attempt 行内。
staging_key(ObjectKey, _next_no) ->
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"direct-staging/", ObjectKey/binary, "/", Rand/binary>>.

candidate_key(ObjectKey, _next_no) ->
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"direct-c/", ObjectKey/binary, "/", Rand/binary>>.

scope_ref_bin(undefined) -> null;
scope_ref_bin(Bin) when is_binary(Bin) -> Bin.

mime_of(FileName) ->
    Ext = string:lowercase(filename:extension(FileName)),
    maps:get(Ext, ext_mime_map(), <<"application/octet-stream">>).

ext_mime_map() ->
    #{
        <<".png">> => <<"image/png">>,
        <<".jpg">> => <<"image/jpeg">>,
        <<".jpeg">> => <<"image/jpeg">>,
        <<".gif">> => <<"image/gif">>,
        <<".webp">> => <<"image/webp">>,
        <<".mp4">> => <<"video/mp4">>,
        <<".mov">> => <<"video/quicktime">>,
        <<".mp3">> => <<"audio/mpeg">>,
        <<".m4a">> => <<"audio/mp4">>,
        <<".pdf">> => <<"application/pdf">>,
        <<".txt">> => <<"text/plain">>
    }.

reason_bin(R) when is_atom(R) -> atom_to_binary(R, utf8);
reason_bin({storage_status, S}) -> <<"storage_status_", (integer_to_binary(S))/binary>>;
reason_bin(R) when is_binary(R) -> R;
reason_bin(R) -> iolist_to_binary(io_lib:format("~p", [R])).
