-module(talk_record_attachment_adapter).
-moduledoc "purpose=talk_record 的附件 scope 授权与统一生命周期 adapter（DESIGN §6/§10；T06 卡交付 1/2/4）。".
%%%
% 面聊记录附件接入既有附件系统的唯一授权/引用入口：
%
%   * 附件读取链 = 记录 ACL → 附件授权（authorize/2 挂接 attach_logic
%     authorize 的 talk_record 分支）：记录未删除 +（owner 或活跃 grant）+
%     本记录存在有效 bound 引用，三条件同时成立才放行——禁止通用用户附件
%     授权（scope=private 按 creator 放行）旁路（§10.3 回听行）。
%   * 上传权（can_upload/2 挂接 attach_logic can_upload 的 talk_record 分支）：
%     V1 录音属 owner 单写入者；企业记录还需 organization 活跃 + owner 为
%     活跃成员（§1 单凭传入 org_id 不能授权；scope_ref=record_id）。
%   * audio part 首绑（bind_audio_part_tx/6）：同事务完成"confirmed 真实资产
%     行存在 + 所属上传会话 actor/record/capture 匹配 + 资产 live 无墓碑 +
%     业务关系写入"，首绑锁协议走 attachment_repo:bind_reference_lock_tx
%     （§10.2），business_key = audio_part:<part_id>（重试不重复绑定）。
%   * 释放/processing lease（release_tx / grant_processing_lease_tx / cancel）：
%     删除/移交释放全部 bound 引用；处理任务持有期限引用，取消/超时释放。
%   * 企业治理映射（§10.4）：release/cleanup 前先 evaluate_release_blockers_tx
%     ——record.retention_until 未到期 → blocked_retention（引用保持 bound，
%     GC 谓词继续挡）；EAR hold（如存在行）→ blocked_hold。
%     永久引用首绑原子登记真实 talk_record 来源 EAR；有 workspace 时登记 WAR，
%     无 workspace 仍受企业保留与 hold，不能由 owner 删除绕过治理。
%%%

-export([
    %% attach_logic 挂接点（池连接；隔离测试走 *_tx 形态）
    can_upload/2,
    authorize/2,
    can_upload_tx/3,
    lock_upload_tx/3,
    authorize_tx/3,
    %% 绑定/释放 API（T11/T12 调用）
    bind_audio_part_tx/6,
    bind_artifact_tx/5,
    release_tx/4,
    release_record_tx/2,
    grant_processing_lease_tx/6,
    cancel_processing_lease_tx/4,
    release_expired_leases_tx/1,
    %% 治理映射 + 删除状态（§10.4）
    evaluate_release_blockers_tx/2,
    governance_state_tx/2,
    request_record_cleanup_tx/2,
    %% 工具
    lease_until_from_now/1,
    audio_part_business_key/1
]).

-define(DEFAULT_LEASE_SECONDS, 900).

%% ===================================================================
%% 上传权（attach_logic:can_upload/3 的 talk_record 分支委托）
%% ===================================================================

%% ScopeRef = record_id（文本）。V1 录音：记录 owner 单写入者；企业记录还需
%% organization.status='active' 且 owner 为活跃组织成员。
-spec can_upload(integer(), binary()) -> ok | {error, forbidden | not_found}.
can_upload(Uid, ScopeRef) ->
    elib_pg:with_conn(fun(Conn) -> can_upload_tx(Conn, Uid, ScopeRef) end).

-spec can_upload_tx(any(), integer(), binary()) -> ok | {error, forbidden | not_found}.
can_upload_tx(Conn, Uid, ScopeRef) when is_integer(Uid), Uid > 0, is_binary(ScopeRef) ->
    case scope_ref_record_id(#{<<"scope_ref">> => ScopeRef}) of
        {ok, RecordId} -> can_upload_record_tx(Conn, Uid, RecordId);
        error -> {error, forbidden}
    end;
can_upload_tx(_Conn, _Uid, _ScopeRef) ->
    {error, forbidden}.

can_upload_record_tx(Conn, Uid, RecordId) ->
    Ws = organization_resource_access_repo:workspace_predicate_sql(<<"tr.workspace_id">>, <<"$1">>),
    Sql =
        <<
            "SELECT EXISTS ("
            " SELECT 1 FROM talk_record tr"
            " LEFT JOIN organization o ON o.id = tr.organization_id"
            " LEFT JOIN organization_member om"
            "   ON om.organization_id = tr.organization_id AND om.user_id = tr.owner_user_id"
            " WHERE tr.id = $2"
            "   AND tr.deleted_at IS NULL"
            "   AND tr.owner_user_id = $1"
            "   AND (tr.scope = 'personal'"
            "        OR (tr.scope = 'organization' AND o.status = 'active'"
            "            AND om.status = 'active'"
            "            AND (tr.workspace_id IS NULL OR ",
            Ws/binary,
            ")) )"
            ") AS allowed"
        >>,
    case elib_pg:query(Conn, Sql, [Uid, RecordId]) of
        {ok, [#{<<"allowed">> := true}]} -> ok;
        {ok, _} -> {error, forbidden};
        {error, R} -> {error, R}
    end.

%% Final binding authority is serialized with membership revocation and parent
%% changes. Lock order: organization/member → workspace/member → record.
lock_upload_tx(Conn, Uid, RecordId) ->
    Sql = <<"SELECT scope,organization_id,workspace_id FROM talk_record WHERE id=$1">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, [Scope]} ->
            case lock_upload_parents(Conn, Uid, Scope) of
                ok -> lock_upload_record(Conn, Uid, RecordId, Scope);
                Error -> Error
            end;
        {ok, []} ->
            {error, forbidden};
        Error ->
            Error
    end.

lock_upload_parents(_Conn, _Uid, #{<<"scope">> := <<"personal">>}) ->
    ok;
lock_upload_parents(Conn, Uid, #{<<"organization_id">> := Org, <<"workspace_id">> := Ws}) ->
    Sql = <<
        "SELECT om.role FROM organization o JOIN organization_member om"
        " ON om.organization_id=o.id WHERE o.id=$1 AND om.user_id=$2"
        " AND o.status='active' AND om.status='active' FOR SHARE OF o,om"
    >>,
    case elib_pg:query(Conn, Sql, [Org, Uid]) of
        {ok, [#{<<"role">> := Role}]} -> lock_upload_workspace(Conn, Uid, Org, Ws, Role);
        {ok, []} -> {error, forbidden};
        Error -> Error
    end.

lock_upload_workspace(_Conn, _Uid, _Org, null, _Role) ->
    ok;
lock_upload_workspace(Conn, Uid, Org, Ws, Role) ->
    Sql = <<
        "SELECT id FROM workspace WHERE id=$1 AND organization_id=$2"
        " AND status IN ('active','archived') FOR SHARE"
    >>,
    case elib_pg:query(Conn, Sql, [Ws, Org]) of
        {ok, [_]} when Role =:= <<"owner">>; Role =:= <<"admin">> -> ok;
        {ok, [_]} ->
            case
                elib_pg:query(
                    Conn,
                    <<
                        "SELECT user_id FROM workspace_member"
                        " WHERE workspace_id=$1 AND user_id=$2 AND status='active' FOR SHARE"
                    >>,
                    [Ws, Uid]
                )
            of
                {ok, [_]} -> ok;
                {ok, []} -> {error, forbidden};
                Error -> Error
            end;
        {ok, []} ->
            {error, forbidden};
        Error ->
            Error
    end.

lock_upload_record(Conn, Uid, RecordId, Scope) ->
    Sql = <<
        "SELECT scope,organization_id,workspace_id,owner_user_id,deleted_at"
        " FROM talk_record WHERE id=$1 FOR UPDATE"
    >>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, [#{<<"owner_user_id">> := Uid, <<"deleted_at">> := null} = Row]} ->
            case maps:with(maps:keys(Scope), Row) =:= Scope of
                true -> ok;
                false -> {error, scope_changed}
            end;
        {ok, _} ->
            {error, forbidden};
        Error ->
            Error
    end.

%% ===================================================================
%% 读鉴权（attach_logic:authorize/3 的 talk_record 分支委托）
%% ===================================================================

%% Rec = find_by_path 行（含 id/scope/scope_ref/status）。
%% 链条：记录未删除 → 记录 ACL（owner 或活跃 grant）→ 本记录有效 bound 引用。
%% 无引用（伪造 scope_ref / 引用已释放）一律 false（fail-closed）。
-spec authorize(integer(), map()) -> boolean().
authorize(Uid, Rec) ->
    elib_pg:with_conn(fun(Conn) -> authorize_tx(Conn, Uid, Rec) end).

-spec authorize_tx(any(), integer(), map()) -> boolean().
authorize_tx(Conn, Uid, #{<<"id">> := AttachmentId} = Rec) when
    is_integer(AttachmentId), AttachmentId > 0
->
    case scope_ref_record_id(Rec) of
        {ok, RecordId} ->
            Sql =
                <<
                    "SELECT EXISTS ("
                    " SELECT 1 FROM talk_record tr"
                    " WHERE tr.id = $2"
                    "   AND tr.deleted_at IS NULL"
                    %% Export artifacts have requester/policy/TTL gates which the
                    %% generic attachment endpoint cannot express. Authoritative
                    %% export/ref/derived-source rows deny this read path forever.
                    "   AND NOT EXISTS (SELECT 1 FROM talk_record_export e"
                    "     WHERE e.attachment_id=$3)"
                    "   AND NOT EXISTS (SELECT 1 FROM talk_record_attachment_ref er"
                    "     WHERE er.attachment_id=$3 AND er.role='export')"
                    "   AND NOT EXISTS (SELECT 1 FROM attachment_derived_write dw"
                    "     WHERE dw.attachment_id=$3)"
                    "   AND (tr.owner_user_id = $1"
                    "        OR EXISTS ("
                    "         SELECT 1 FROM talk_record_grant g"
                    "         WHERE g.record_id = tr.id AND g.grantee_user_id = $1"
                    "           AND g.revoked_at IS NULL"
                    "           AND (g.expires_at IS NULL OR g.expires_at > now())))"
                    "   AND EXISTS ("
                    "     SELECT 1 FROM talk_record_attachment_ref trar"
                    "     WHERE trar.record_id = tr.id"
                    "       AND trar.attachment_id = $3"
                    "       AND trar.state = 'bound'"
                    "       AND (trar.lease_until IS NULL OR trar.lease_until > now()))"
                    ") AS allowed"
                >>,
            case elib_pg:query(Conn, Sql, [Uid, RecordId, AttachmentId]) of
                {ok, [#{<<"allowed">> := true}]} -> true;
                _ -> false
            end;
        error ->
            false
    end;
authorize_tx(_Conn, _Uid, _Rec) ->
    false.

scope_ref_record_id(#{<<"scope_ref">> := Ref}) when is_binary(Ref), Ref =/= <<>> ->
    try binary_to_integer(Ref) of
        Id when Id > 0 -> {ok, Id};
        _ -> error
    catch
        _:_ -> error
    end;
scope_ref_record_id(_) ->
    error.

%% ===================================================================
%% audio part 首绑（§10.2 同事务合同）
%% ===================================================================

%% 六条件同事务：capture 属同 record 且 started_by=actor；记录未删除；
%% owner=actor（V1 单写入者）；上传会话 confirmed 且绑定该资产且登记主体=
%% actor；资产 live 无墓碑 + scope/record 归属（repo bind_tx 内复核）；
%% business_key=audio_part:<part_id> 稳定幂等。
-spec bind_audio_part_tx(any(), integer(), integer(), integer(), integer(), integer()) ->
    {ok, integer() | binary(), boolean()} | {error, term()}.
bind_audio_part_tx(Conn, ActorId, RecordId, CaptureId, PartId, AttachmentId) ->
    case verify_capture_tx(Conn, ActorId, RecordId, CaptureId) of
        ok ->
            case verify_record_live_tx(Conn, RecordId) of
                ok ->
                    case verify_upload_session_tx(Conn, ActorId, AttachmentId) of
                        ok ->
                            case verify_part_consistency_tx(Conn, PartId, AttachmentId) of
                                ok ->
                                    talk_record_attachment_ref_repo:bind_tx(Conn, #{
                                        record_id => RecordId,
                                        attachment_id => AttachmentId,
                                        capture_id => CaptureId,
                                        role => <<"audio_part">>,
                                        business_key => audio_part_business_key(PartId)
                                    });
                                {error, R} ->
                                    {error, R}
                            end;
                        {error, R} ->
                            {error, R}
                    end;
                {error, R} ->
                    {error, R}
            end;
        {error, R} ->
            {error, R}
    end.

audio_part_business_key(PartId) when is_integer(PartId) ->
    <<"audio_part:", (integer_to_binary(PartId))/binary>>;
audio_part_business_key(PartId) when is_binary(PartId) ->
    <<"audio_part:", PartId/binary>>.

%% capture 归属 + 单写入者（§2：UNIQUE(record_id,device_session_id)；V1 owner）
verify_capture_tx(Conn, ActorId, RecordId, CaptureId) ->
    Sql =
        <<
            "SELECT c.started_by, tr.owner_user_id FROM talk_record_capture c"
            " JOIN talk_record tr ON tr.id = c.record_id"
            " WHERE c.id = $1 AND c.record_id = $2"
        >>,
    case elib_pg:query(Conn, Sql, [CaptureId, RecordId]) of
        {ok, [#{<<"started_by">> := ActorId, <<"owner_user_id">> := ActorId}]} ->
            ok;
        {ok, [_]} ->
            {error, capture_actor_mismatch};
        {ok, []} ->
            {error, capture_mismatch};
        {error, R} ->
            {error, R}
    end.

verify_record_live_tx(Conn, RecordId) ->
    Sql =
        <<"SELECT deleted_at FROM talk_record WHERE id=$1">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, [#{<<"deleted_at">> := null}]} -> ok;
        {ok, [_]} -> {error, record_deleted};
        {ok, []} -> {error, record_not_found};
        {error, R} -> {error, R}
    end.

%% 所属上传会话：attach_pending confirmed + 资产绑定 + 登记主体=actor
%% （§10.2"所属上传会话 actor+record+capture 匹配"的会话侧；
%%   record/capture 侧由 verify_capture_tx + bind_tx scope_ref 复核）。
verify_upload_session_tx(Conn, ActorId, AttachmentId) ->
    Sql =
        <<
            "SELECT 1 FROM attach_pending p JOIN attachment a ON a.id = $2"
            " WHERE p.object_key = a.path"
            "   AND p.session_state = 'confirmed'"
            "   AND p.confirmed_attachment_id = $2"
            "   AND p.creator_user_id = $1"
        >>,
    case elib_pg:query(Conn, Sql, [ActorId, AttachmentId]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, session_mismatch};
        {error, R} -> {error, R}
    end.

%% 分段行一致性（若 T11 已登记 part 行）：attachment_id 必须一致，防拼接。
verify_part_consistency_tx(Conn, PartId, AttachmentId) ->
    Sql =
        <<
            "SELECT attachment_id FROM talk_record_audio_part WHERE id = $1"
        >>,
    case elib_pg:query(Conn, Sql, [PartId]) of
        {ok, [#{<<"attachment_id">> := AttachmentId}]} -> ok;
        {ok, [_]} -> {error, part_attachment_mismatch};
        {ok, []} -> ok;
        {error, R} -> {error, R}
    end.

%% ===================================================================
%% 服务端产物绑定（manifest/transcoded_audio/export；§10.1 持久产物登记）
%% ===================================================================

-spec bind_artifact_tx(any(), integer(), integer(), binary(), binary()) ->
    {ok, integer() | binary(), boolean()} | {error, term()}.
bind_artifact_tx(Conn, RecordId, AttachmentId, Role, BusinessKey) ->
    case verify_record_live_tx(Conn, RecordId) of
        ok ->
            talk_record_attachment_ref_repo:bind_tx(Conn, #{
                record_id => RecordId,
                attachment_id => AttachmentId,
                role => Role,
                business_key => BusinessKey
            });
        {error, R} ->
            {error, R}
    end.

%% ===================================================================
%% 释放 / processing lease
%% ===================================================================

-spec release_tx(any(), integer(), binary(), binary()) ->
    {ok, released | already_released} | {error, term()}.
release_tx(Conn, RecordId, Role, BusinessKey) ->
    talk_record_attachment_ref_repo:release_tx(Conn, RecordId, Role, BusinessKey).

%% 记录删除/移交释放（治理门先行；blocked 时引用保持 bound，GC 谓词继续挡）。
-spec release_record_tx(any(), integer()) ->
    {ok, released, non_neg_integer()} | {ok, blocked, term()} | {error, term()}.
release_record_tx(Conn, RecordId) ->
    case evaluate_release_blockers_tx(Conn, RecordId) of
        {ok, none} ->
            talk_record_attachment_ref_repo:release_record_tx(Conn, RecordId);
        {ok, Blocker} ->
            {ok, blocked, Blocker};
        {error, R} ->
            {error, R}
    end.

%% 授予/延长处理引用（§10.3：complete/合成/转码任务处理期间）。
%% LeaseSeconds 缺省 900；取消 = cancel_processing_lease_tx；超时由
%% release_expired_leases_tx / GC 谓词时间比较兜底。
-spec grant_processing_lease_tx(
    any(), integer(), integer(), binary(), binary(), pos_integer()
) ->
    {ok, integer() | binary(), boolean()} | {error, term()}.
grant_processing_lease_tx(Conn, RecordId, AttachmentId, Role, BusinessKey, LeaseSeconds) ->
    talk_record_attachment_ref_repo:grant_processing_lease_tx(
        Conn, RecordId, AttachmentId, Role, BusinessKey, lease_until_from_now(LeaseSeconds)
    ).

-spec cancel_processing_lease_tx(any(), integer(), binary(), binary()) ->
    {ok, released | already_released} | {error, term()}.
cancel_processing_lease_tx(Conn, RecordId, Role, BusinessKey) ->
    talk_record_attachment_ref_repo:release_tx(Conn, RecordId, Role, BusinessKey).

-spec release_expired_leases_tx(any()) -> {ok, non_neg_integer()} | {error, term()}.
release_expired_leases_tx(Conn) ->
    talk_record_attachment_ref_repo:release_expired_leases_tx(Conn).

%% ===================================================================
%% 企业治理映射（§10.4）
%% ===================================================================

%% 释放阻断判定：record.retention_until 未到期 → blocked_retention；
%% EAR hold（如治理行存在）→ blocked_hold；否则放行。
%% 【未接线声明】EAR/WAR 登记面不支持 talk_record（见模块头），
%% hold 分支仅在未来治理行出现时生效（defense-in-depth）。
-spec evaluate_release_blockers_tx(any(), integer()) ->
    {ok, none | {blocked_retention, binary()} | {blocked_hold, integer()}} | {error, term()}.
evaluate_release_blockers_tx(Conn, RecordId) ->
    case talk_record_governance_repo:facts_tx(Conn, RecordId) of
        {ok, [#{<<"held_count">> := Held}]} when Held > 0 -> {ok, {blocked_hold, Held}};
        {ok, [#{<<"retained_count">> := N, <<"effective_retention_until">> := Until}]} when N > 0 ->
            {ok, {blocked_retention, Until}};
        {ok, [#{<<"retention_active">> := true, <<"retention_until">> := Until}]} ->
            {ok, {blocked_retention, Until}};
        {ok, [_]} ->
            {ok, none};
        {ok, []} ->
            {error, record_not_found};
        E ->
            E
    end.
%% Actual ledger facts, including enterprise records without a workspace.
governance_state_tx(Conn, RecordId) ->
    case talk_record_governance_repo:facts_tx(Conn, RecordId) of
        {ok, [Row]} ->
            {ok, Row#{
                enterprise_governance_integrated => true,
                retention_policy_scope => <<"instance">>
            }};
        {ok, []} ->
            {error, record_not_found};
        E ->
            E
    end.

%% ===================================================================
%% 删除 → 附件系统清理映射（§10.4 删除状态拆分；T12 purge 协调入口）
%% ===================================================================

%% 只协调引用释放 + 删除意图登记；物理删除由既有 worker 按队列执行。
%% 返回 per-attachment 结果：
%%   intended                    —— 已写 delete_intent 入队
%%   retained_by_other_reference —— 仍有其他引用/上传能力未耗尽（23510 族：
%%                                  收藏/moment/群文件/talk_record 他记录
%%                                  引用/在途写保护，统一按 §10.4 语义展示）
%%   tombstone                   —— 已在队列（幂等重入）
%%   asset_row_missing           —— 资产行不存在（审计异常，不伪报完成）
request_record_cleanup_tx(Conn, RecordId) ->
    case evaluate_release_blockers_tx(Conn, RecordId) of
        {ok, none} ->
            {ok, Attachments} = cleanup_attachments_tx(Conn, RecordId),
            {ok, Released} = talk_record_attachment_ref_repo:release_record_tx(Conn, RecordId),
            Outcomes = [cleanup_one_tx(Conn, Path) || #{<<"path">> := Path} <- Attachments],
            {ok, #{released => Released, outcomes => Outcomes}};
        {ok, Blocker} ->
            {ok, #{blocked => Blocker, outcomes => []}};
        {error, R} ->
            {error, R}
    end.

cleanup_attachments_tx(Conn, RecordId) ->
    Sql =
        <<
            "SELECT DISTINCT a.path FROM attachment a"
            " JOIN talk_record_attachment_ref trar ON trar.attachment_id = a.id"
            " WHERE trar.record_id = $1"
            " UNION SELECT a.path FROM attachment a JOIN attachment_derived_write w ON w.attachment_id=a.id"
            " JOIN talk_record_job j ON j.id=w.source_job_id WHERE j.record_id=$1"
        >>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, Rows} -> {ok, Rows};
        {error, R} -> {error, R}
    end.

cleanup_one_tx(Conn, Path) ->
    case
        elib_pg:query(Conn, <<"SELECT lifecycle_state FROM attachment WHERE path=$1 FOR UPDATE">>, [
            Path
        ])
    of
        {ok, [#{<<"lifecycle_state">> := S}]} when S =:= <<"delete_intent">>; S =:= <<"deleted">> ->
            #{path => Path, outcome => tombstone};
        {ok, [_]} ->
            case attachment_repo:refresh_gc_verified_tx(Conn, Path) of
                {ok, _} -> cleanup_live_tx(Conn, Path);
                {error, R} -> #{path => Path, outcome => {error, R}}
            end;
        {ok, []} ->
            #{path => Path, outcome => asset_row_missing};
        {error, R} ->
            #{path => Path, outcome => {error, R}}
    end.
cleanup_live_tx(Conn, Path) ->
    case attachment_repo:intent_ordinary_delete_tx(Conn, Path, <<"talk_record_purge">>) of
        ok ->
            #{path => Path, outcome => intended};
        {error, intent_conflict} ->
            #{path => Path, outcome => retained_by_other_reference};
        {error, tombstone} ->
            #{path => Path, outcome => tombstone};
        {error, not_found} ->
            #{path => Path, outcome => asset_row_missing};
        {error, R} ->
            #{path => Path, outcome => {error, R}}
    end.

%% ===================================================================
%% 工具
%% ===================================================================

%% 生成 now+Seconds 的 UTC timestamptz 文本（lease 参数用）。
-spec lease_until_from_now(pos_integer()) -> binary().
lease_until_from_now(Seconds) when is_integer(Seconds), Seconds > 0 ->
    Secs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Seconds,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(Secs),
    iolist_to_binary(
        io_lib:format(
            "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B+00",
            [Y, Mo, D, H, Mi, S]
        )
    ).
