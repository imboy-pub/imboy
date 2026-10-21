-module(talk_record_transfer_logic).
-moduledoc "面聊记录企业责任移交逻辑（T19）：POST ownership-transfers。".
%%%
% 责任移交域业务逻辑（§9 企业记录的责任与退出 + §11.2 版本域纪律）。
%
% POST /:id/ownership-transfers（§9）：
%   * 发起人 = **当前 owner 在仍有权时**（visible + owner_user_id ==
%     发起人——owner 已因企业退出失权时 visible 失败 → 404，此时移交
%     属 metadata-only recovery 治理通道，本卡不实现）；
%   * 目标 = 同企业且符合工作区授权的同事（复用 T17 目录校验
%     target_in_directory_tx：同 org active 成员 + ws 资源授权谓词）；
%     目标 = 发起人自己 → 422；跨企业/失格 → 422 target_not_in_directory；
%   * scope=personal → 422（§9：V1 不转移个人归属）；
%   * If-Match（record.revision 域——T21 `record:<revision>` 惯例，兼容
%     纯 revision 形态）+ Idempotency-Key（缺 422/428）；
%   * 事务（§9"服务器事务更新 owner、审计"）：
%     幂等登记（**登记先于门禁**：移交成功后原 owner 失权，网络超时
%     重试同键命中快照直接回放，不因失权 404——§3 幂等动作重放先返回
%     首次结果再校验版本）→ visible 门禁 → 行锁 lock_for_transfer_tx
%     （FOR UPDATE + revision CAS，与并发删除/编辑互斥）→ 目录校验 →
%     keep_access 分支（请求明确决定，§9）：
%       - false：确认无遗留 grant——撤销原 owner 全部活跃授权行（残留
%         行会使原 owner 仍可读，与"不再有权访问"矛盾）；
%       - true：原 owner 补 viewer grant（无活跃行时；已有活跃行保持
%         原样，不降级不重复）；
%     → transfer_owner_tx（owner_user_id 更新 + custody_status=active；
%     **不修改 created_by/scope**——创建事实与归属不变）→
%     acl_revision 推进（§11.2"责任移交时推进"）→ 审计 → 快照回填。
%   * 移交成功后新 owner 到位：custody_status 回 active（handover_
%     required 仅在无有效 owner 时成立，§9）。
%
% 幂等键不随 owner 变化（§11）：创建幂等的业务键是 **created_by +
% client_recording_id**（主表 UNIQUE），移交只改 owner_user_id、不动
% created_by——移交后同 created_by+client_recording_id 的创建重放仍命中
% 原记录，不产生第二份记录（离线补传场景，测试锁定）。
%%%

-export([transfer/5, parse_transfer/1]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% POST /:id/ownership-transfers
%% ===================================================================

%% @doc 责任移交。返回 {ok, created | replay, Payload} | {error, Reason}。
-spec transfer(integer(), integer(), binary() | undefined, binary() | undefined, map()) ->
    {ok, created | replay, map()} | {error, term()}.
transfer(Uid, RecordId, IfMatch, IdemKey, Body) ->
    case talk_record_logic:valid_idempotency_key(IdemKey) of
        false ->
            {error, idempotency_key_required};
        true ->
            transfer_keyed(Uid, RecordId, IfMatch, IdemKey, Body)
    end.

transfer_keyed(Uid, RecordId, IfMatch, IdemKey, Body) ->
    case parse_transfer(Body) of
        {ok, #{target_user_id := TargetUid, keep_access := KeepAccess}} ->
            case parse_record_if_match(IfMatch) of
                {ok, ExpectedRevision} ->
                    transfer_tx(
                        Uid, RecordId, TargetUid, KeepAccess, ExpectedRevision, IdemKey, Body
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec transfer_tx(integer(), integer(), integer(), boolean(), pos_integer(), binary(), map()) ->
    {ok, created | replay, map()} | {error, term()}.
transfer_tx(Uid, RecordId, TargetUid, KeepAccess, ExpectedRevision, IdemKey, Body) ->
    Resource = transfer_resource(RecordId),
    case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
        {ok, Digest} ->
            transfer_digest(
                Uid, RecordId, TargetUid, KeepAccess, ExpectedRevision, IdemKey, Resource, Digest
            );
        {error, non_canonical} ->
            {error, invalid_body}
    end.

-spec transfer_digest(
    integer(),
    integer(),
    integer(),
    boolean(),
    pos_integer(),
    binary(),
    binary(),
    binary()
) -> {ok, created | replay, map()} | {error, term()}.
transfer_digest(Uid, RecordId, TargetUid, KeepAccess, ExpectedRevision, IdemKey, Resource, Digest) ->
    ExpiresAt = elib_dt:add(elib_dt:now(), {idempotency_ttl_seconds(), second}),
    case
        elib_pg:with_tx(
            fun(Conn) ->
                transfer_in_tx(
                    Conn,
                    Uid,
                    RecordId,
                    TargetUid,
                    KeepAccess,
                    ExpectedRevision,
                    IdemKey,
                    Resource,
                    Digest,
                    ExpiresAt
                )
            end,
            [{reraise, false}]
        )
    of
        {ok, Mark, Payload} ->
            {ok, Mark, Payload};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_transfer_rolled_back, RecordId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec transfer_in_tx(
    any(),
    integer(),
    integer(),
    integer(),
    boolean(),
    pos_integer(),
    binary(),
    binary(),
    binary(),
    binary()
) -> {ok, created | replay, map()} | no_return().
transfer_in_tx(
    Conn,
    Uid,
    RecordId,
    TargetUid,
    KeepAccess,
    ExpectedRevision,
    IdemKey,
    Resource,
    Digest,
    ExpiresAt
) ->
    case
        talk_record_idempotency_repo:record_tx(
            Conn, Uid, <<"POST">>, Resource, IdemKey, Digest, ExpiresAt
        )
    of
        {ok, inserted} ->
            transfer_fresh_tx(
                Conn, Uid, RecordId, TargetUid, KeepAccess, ExpectedRevision, IdemKey, Resource
            );
        {ok, existing, Row} ->
            replay_idem(Row, Digest);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_transfer_idem_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @private 移交门禁 + 行锁 + 目录校验 + keep_access 分支 + owner 更新。
-spec transfer_fresh_tx(
    any(),
    integer(),
    integer(),
    integer(),
    boolean(),
    pos_integer(),
    binary(),
    binary()
) -> {ok, created, map()} | no_return().
transfer_fresh_tx(Conn, Uid, RecordId, TargetUid, KeepAccess, ExpectedRevision, IdemKey, Resource) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, #{<<"owner_user_id">> := Uid, <<"scope">> := <<"personal">>}} ->
            %% §9：V1 不转移个人归属（移交接管页 UI 已按 caps 门控）
            throw({talk_record_rollback, {error, personal_not_transferable}});
        {ok, #{<<"owner_user_id">> := Uid} = Row} ->
            ok = lock_transfer_context_tx(Conn, Uid, TargetUid, Row),
            transfer_locked_tx(
                Conn, Uid, RecordId, TargetUid, KeepAccess, ExpectedRevision, IdemKey, Resource
            );
        {ok, _} ->
            %% editor/viewer 不可移交（§1：不可改 ACL 或删除原记录——
            %% 移交是更强的管理动作，同样仅 owner）
            throw({talk_record_rollback, {error, forbidden}});
        {error, Reason} ->
            %% 含 owner 已失权（企业退出后 visible 失败 → 404）：
            %% 该场景的移交属 metadata-only recovery（§9 后续企业治理），
            %% 本卡不放开
            throw({talk_record_rollback, {error, Reason}})
    end.

lock_transfer_context_tx(_Conn, Uid, Uid, _Row) ->
    throw({talk_record_rollback, {error, transfer_target_self}});
lock_transfer_context_tx(Conn, Uid, TargetUid, Row) ->
    case talk_record_context_ds:lock_transfer_context_tx(Conn, Uid, TargetUid, Row) of
        ok -> ok;
        {error, Reason} -> throw({talk_record_rollback, {error, Reason}})
    end.

-spec transfer_locked_tx(
    any(),
    integer(),
    integer(),
    integer(),
    boolean(),
    pos_integer(),
    binary(),
    binary()
) -> {ok, created, map()} | no_return().
transfer_locked_tx(Conn, Uid, RecordId, TargetUid, KeepAccess, ExpectedRevision, IdemKey, Resource) ->
    case talk_record_repo:lock_for_transfer_tx(Conn, RecordId, ExpectedRevision) of
        {ok, #{<<"owner_user_id">> := Uid} = LockedRow} ->
            transfer_target_tx(
                Conn, Uid, RecordId, TargetUid, KeepAccess, LockedRow, IdemKey, Resource
            );
        {ok, _} ->
            %% P2（Q-review）双花防护：行锁 CAS 只核 revision（§11.2 移交
            %% 推 acl_revision 不推 revision）——并发双移交时第二个事务
            %% 拿锁后 revision 仍匹配，若不复核 owner 会把已易主的记录再
            %% 移交一次。锁后当前 owner 已非发起人 → 412（基于过期的
            %% owner 状态，刷新重试）。
            throw({talk_record_rollback, {error, precondition_failed}});
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

-spec transfer_target_tx(
    any(), integer(), integer(), integer(), boolean(), map(), binary(), binary()
) ->
    {ok, created, map()} | no_return().
transfer_target_tx(Conn, Uid, RecordId, TargetUid, KeepAccess, LockedRow, IdemKey, Resource) ->
    case TargetUid =:= Uid of
        true ->
            throw({talk_record_rollback, {error, transfer_target_self}});
        false ->
            transfer_directory_tx(
                Conn, Uid, RecordId, TargetUid, KeepAccess, LockedRow, IdemKey, Resource
            )
    end.

%% @private 目录校验（§9：同企业且符合工作区授权的同事；复用 T17/
%% T15 的 target_in_directory_tx——org 记录=同 org active 成员 + ws
%% 资源授权谓词）。
-spec transfer_directory_tx(
    any(), integer(), integer(), integer(), boolean(), map(), binary(), binary()
) ->
    {ok, created, map()} | no_return().
transfer_directory_tx(Conn, Uid, RecordId, TargetUid, KeepAccess, LockedRow, IdemKey, Resource) ->
    case talk_record_speaker_repo:target_in_directory_tx(Conn, LockedRow, TargetUid) of
        {ok, true} ->
            transfer_commit_tx(Conn, Uid, RecordId, TargetUid, KeepAccess, IdemKey, Resource);
        {ok, false} ->
            throw({talk_record_rollback, {error, target_not_in_directory}});
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

-spec transfer_commit_tx(any(), integer(), integer(), integer(), boolean(), binary(), binary()) ->
    {ok, created, map()} | no_return().
transfer_commit_tx(Conn, Uid, RecordId, TargetUid, KeepAccess, IdemKey, Resource) ->
    %% keep_access 分支（§9：原 owner 是否保留显式 grant 由请求明确决定）
    ok = keep_access_tx(Conn, RecordId, Uid, TargetUid, KeepAccess),
    {ok, NewRow} = talk_record_repo:transfer_owner_tx(Conn, RecordId, TargetUid),
    {ok, NewAcl} = talk_record_repo:bump_acl_revision_tx(Conn, RecordId),
    DisplayName = target_display_name(Conn, TargetUid),
    {ok, _AuditId} = talk_record_repo:insert_audit_tx(
        Conn,
        RecordId,
        Uid,
        <<"ownership_transfer">>,
        TargetUid,
        #{
            <<"from_user_id">> => Uid,
            <<"to_user_id">> => TargetUid,
            <<"keep_access">> => KeepAccess,
            <<"acl_revision">> => NewAcl,
            <<"created_by_unchanged">> => true
        }
    ),
    Payload = #{
        <<"record_id">> => RecordId,
        <<"owner_user_id">> => TargetUid,
        <<"new_owner_user_id">> => TargetUid,
        <<"new_owner_display_name">> => DisplayName,
        <<"keep_access">> => KeepAccess,
        <<"acl_revision">> => NewAcl,
        <<"revision">> => maps:get(<<"revision">>, NewRow),
        <<"created_by">> => maps:get(<<"created_by">>, NewRow),
        <<"scope">> => maps:get(<<"scope">>, NewRow),
        <<"custody_status">> => maps:get(<<"custody_status">>, NewRow)
    },
    Snapshot = jsone:encode(#{<<"payload">> => Payload}),
    case
        talk_record_idempotency_repo:complete_tx(Conn, Uid, <<"POST">>, Resource, IdemKey, Snapshot)
    of
        ok ->
            {ok, created, Payload};
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private keep_access 决定落实：
%%   false → 撤销原 owner 全部活跃 grant（确认无遗留——残留行会使原
%%           owner 仍可读）；owner 在主表的访问随 owner_user_id 更新即失。
%%   true  → 补 viewer grant（无活跃行时；已有活跃行保持原样）。
-spec keep_access_tx(any(), integer(), integer(), integer(), boolean()) -> ok | no_return().
keep_access_tx(Conn, RecordId, OldOwnerUid, _TargetUid, false) ->
    case talk_record_repo:revoke_grantee_grants_tx(Conn, RecordId, OldOwnerUid) of
        {ok, _N} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG([talk_record_transfer_revoke_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end;
keep_access_tx(Conn, RecordId, OldOwnerUid, _TargetUid, true) ->
    case talk_record_repo:ensure_transfer_grant_tx(Conn, RecordId, OldOwnerUid, OldOwnerUid) of
        ok ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG([talk_record_transfer_grant_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @private 新 owner 展示名（user 表 nickname；缺省空——T21
%% TransferOutcome.newOwnerDisplayName 联调用）。
-spec target_display_name(any(), integer()) -> binary().
target_display_name(Conn, TargetUid) ->
    case elib_pg:query(Conn, <<"SELECT nickname FROM \"user\" u WHERE u.id = $1">>, [TargetUid]) of
        {ok, [#{<<"nickname">> := Name}]} when is_binary(Name) -> Name;
        _ -> <<>>
    end.

%% @private 幂等重放（快照形态回放；同键异 body 409）。移交成功后原
%% owner 已失权——重放命中快照直接返回首次结果，不重走门禁（§3）。
-spec replay_idem(map(), binary()) -> {ok, replay, map()} | {error, atom()}.
replay_idem(Row, Digest) ->
    case maps:get(<<"payload_hash">>, Row, <<>>) of
        Digest ->
            case maps:get(<<"result_ref">>, Row, null) of
                SnapBin when is_binary(SnapBin) ->
                    #{<<"payload">> := P} = jsone:decode(SnapBin),
                    {ok, replay, P};
                _ ->
                    {error, idempotency_pending}
            end;
        _ ->
            {error, idempotency_conflict}
    end.

%% ===================================================================
%% 入参解析
%% ===================================================================

%% @doc 移交请求体：target_user_id（正整数）+ keep_access（boolean，
%% 缺省 false——§9/T21 默认不保留）。二进制 ID 兼容（TSID JSON int64
%% 传输惯例为 integer，防御性接受数字串）。
-spec parse_transfer(map()) -> {ok, map()} | {error, atom()}.
parse_transfer(Body) when is_map(Body) ->
    Target = maps:get(<<"target_user_id">>, Body, 0),
    KeepAccess = maps:get(<<"keep_access">>, Body, false),
    case {valid_uid(Target), is_boolean(KeepAccess)} of
        {true, true} ->
            {ok, #{target_user_id => norm_uid(Target), keep_access => KeepAccess}};
        {false, _} ->
            {error, invalid_target_user_id};
        _ ->
            {error, invalid_keep_access}
    end;
parse_transfer(_) ->
    {error, invalid_body}.

-spec valid_uid(term()) -> boolean().
valid_uid(N) when is_integer(N), N > 0 -> true;
valid_uid(B) when is_binary(B) ->
    try
        binary_to_integer(B) > 0
    catch
        _:_ -> false
    end;
valid_uid(_) ->
    false.

-spec norm_uid(integer() | binary()) -> integer().
norm_uid(N) when is_integer(N) -> N;
norm_uid(B) when is_binary(B) -> binary_to_integer(B).

%% ===================================================================
%% 内部
%% ===================================================================

%% @private If-Match（record.revision 域；T21 `record:<rev>` 惯例，兼容
%% 纯 revision——复用内容域解析器，与 acl 域同法）。
-spec parse_record_if_match(binary() | undefined) -> {ok, pos_integer()} | {error, atom()}.
parse_record_if_match(IfMatch) ->
    talk_record_content_common:parse_resource_if_match(<<"record">>, 0, IfMatch).

-spec transfer_resource(integer()) -> binary().
transfer_resource(RecordId) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/ownership-transfers">>.

-spec idempotency_ttl_seconds() -> pos_integer().
idempotency_ttl_seconds() ->
    case config_ds:env(imboy, talk_record_idempotency_ttl_seconds, 604800) of
        N when is_integer(N), N > 0 -> N;
        _ -> 604800
    end.
