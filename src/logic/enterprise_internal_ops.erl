-module(enterprise_internal_ops).

%%%
% enterprise_internal_ops 是企业集成 Application/Credential 的最小运维面
% （EPGZ-02，plan-gz §1.11：不做 Admin 治理 UI，只提供受权限保护的
% module API / shell 命令形态）。
%
% 能力：创建 application、下发/轮换/撤销 credential、查状态、scopes 与
% 启停治理，以及 Application Grant（Org/Workspace Grant）的签发/撤销/降级/
% 边界编辑（FULL-01）。调用方是运维（imboy_ctl/erl shell，W4 接线后可挂
% /api/adm/*），**不是** Application Credential 面。
%
% credential 明文纪律：secret 由本模块生成（32 字节 CSPRNG base64url）
% 或由运维显式提供，只在 issue/rotate 的**返回值出现一次**；库内只存
% SHA-256 digest（enterprise_application_credential_repo）。
%
% Grant 纪律（FULL-01）：签发必须显式给 idempotency_key（同 (org, app) 唯一，
% 重复签发返回 key_conflict 而不是静默再发一份——静默再发会以并集形式扩大
% 授权面）；撤销/降级/边界编辑走 expected-version CAS；授权行禁止物理删除
% （migration 00000139 触发器），撤权即改 status 且下一次请求生效。
%
% _tx 变体供测试（直连连接）与运行时复用；无后缀变体经 elib_pg:with_tx
% 池化执行。scope 只接受 enterprise_internal_scope:all() 固定枚举成员。
%%%

-export([
    create_application_tx/5,
    create_application/4,
    update_scopes_tx/4,
    update_scopes/3,
    set_application_status_tx/4,
    set_application_status/3,
    set_application_status_cas/4,
    update_scopes_cas/4,
    list_credentials/2,
    list_credentials/3,
    issue_credential_tx/5,
    issue_credential_tx/4,
    issue_credential/3,
    rotate_credential_tx/3,
    rotate_credential_tx/4,
    rotate_credential/2,
    revoke_credential_tx/3,
    revoke_credential/2,
    application_status_tx/3,
    application_status/2,
    issue_grant_tx/4,
    issue_grant/3,
    revoke_grant_tx/6,
    revoke_grant/5,
    set_grant_scopes_tx/6,
    set_grant_scopes/5,
    set_grant_workspaces_tx/7,
    set_grant_workspaces/6,
    grant_status_tx/3,
    grant_status/2
]).

-include("log.hrl").

%%%===================================================================
%%% Application 治理
%%%===================================================================

%% @doc 事务内创建 application（scopes 必须全为固定枚举成员）。
-spec create_application_tx(any(), integer(), binary(), binary(), [binary()]) ->
    {ok, map()} | {error, invalid_scope | key_conflict | term()}.
create_application_tx(Conn, OrgId, ApplicationKey, Name, Scopes) ->
    case validate_scopes(Scopes) of
        ok ->
            enterprise_application_repo:create_tx(
                Conn, OrgId, ApplicationKey, Name, {null, Scopes}
            );
        {error, _} = Err ->
            Err
    end.

%% @doc 池化创建 application。
-spec create_application(integer(), binary(), binary(), [binary()]) ->
    {ok, map()} | {error, term()}.
create_application(OrgId, ApplicationKey, Name, Scopes) ->
    pool(fun(Conn) -> create_application_tx(Conn, OrgId, ApplicationKey, Name, Scopes) end).

%% @doc 事务内整体替换 allowed_scopes（固定枚举成员校验）。
-spec update_scopes_tx(any(), integer(), integer(), [binary()]) ->
    ok | {error, invalid_scope | not_found | term()}.
update_scopes_tx(Conn, OrgId, AppId, Scopes) ->
    case validate_scopes(Scopes) of
        ok -> enterprise_application_repo:update_scopes_tx(Conn, OrgId, AppId, Scopes);
        {error, _} = Err -> Err
    end.

%% @doc 池化替换 scopes。
-spec update_scopes(integer(), integer(), [binary()]) -> ok | {error, term()}.
update_scopes(OrgId, AppId, Scopes) ->
    pool(fun(Conn) -> update_scopes_tx(Conn, OrgId, AppId, Scopes) end).

%% @doc 事务内启停 application（active|disabled）。
-spec set_application_status_tx(any(), integer(), integer(), binary()) ->
    ok | {error, not_found | term()}.
set_application_status_tx(Conn, OrgId, AppId, Status) ->
    enterprise_application_repo:update_status_tx(Conn, OrgId, AppId, Status).

%% @doc 池化启停 application。
-spec set_application_status(integer(), integer(), binary()) -> ok | {error, term()}.
set_application_status(OrgId, AppId, Status) ->
    pool(fun(Conn) -> set_application_status_tx(Conn, OrgId, AppId, Status) end).

%% @doc 池化 CAS 替换生命周期（Admin 治理面 A-03；迁移 00000143 的 version 列）。
%% 与 set_application_status/3 的差别：
%%   ① 带 expected_version —— 并发治理写入只有赢家生效，其余 {error, version_conflict}
%%      （plan-full §7「credential/Grant … 下一请求即失效」同口径的乐观锁纪律）；
%%   ② 接受**四值**生命周期 draft/active/disabled/archived（DB ck_ea_status
%%      已同步放宽），而不再只接受启停二值。
%% 非法状态在本层就拒绝（fail-closed，不执行 SQL）；DB 的 CHECK 是第二道闸。
-spec set_application_status_cas(integer(), integer(), pos_integer(), binary()) ->
    ok | {error, invalid_status | not_found | version_conflict | term()}.
set_application_status_cas(OrgId, AppId, ExpectedVersion, Status) ->
    case validate_lifecycle(Status) of
        ok ->
            pool(fun(Conn) ->
                enterprise_application_repo:update_status_cas_tx(
                    Conn, OrgId, AppId, ExpectedVersion, Status
                )
            end);
        {error, _} = Err ->
            Err
    end.

%% @doc 池化 CAS 整体替换 allowed_scopes（Admin 治理面 A-04）。
%% 空集合 ⇒ {error, empty_scopes}（scope downgrade 到「无权限」必须显式表达，
%% 静默清空等于把授权面清零后无人知晓）；非白名单成员 ⇒ {error, invalid_scope}。
-spec update_scopes_cas(integer(), integer(), pos_integer(), [binary()]) ->
    ok | {error, empty_scopes | invalid_scope | not_found | version_conflict | term()}.
update_scopes_cas(_OrgId, _AppId, _ExpectedVersion, []) ->
    {error, empty_scopes};
update_scopes_cas(OrgId, AppId, ExpectedVersion, Scopes) ->
    case validate_scopes(Scopes) of
        ok ->
            pool(fun(Conn) ->
                enterprise_application_repo:update_scopes_cas_tx(
                    Conn, OrgId, AppId, ExpectedVersion, Scopes
                )
            end);
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% Credential 生命周期
%%%===================================================================

%% @doc 事务内签发凭证（secret 由本模块生成，明文只在返回值出现一次）。
-spec issue_credential_tx(any(), integer(), integer(), undefined | binary()) ->
    {ok, #{credential_id := integer(), credential := binary(), expires_at := undefined | binary()}}
    | {error, term()}.
issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) ->
    issue_credential_tx(Conn, OrgId, AppId, generate_secret(), ExpiresAt).

%% @doc 事务内签发凭证（显式 secret——备份恢复等运维场景）。
%% 返回 #{credential_id, credential(明文 ib_int_<id>.<secret>), expires_at}。
-spec issue_credential_tx(any(), integer(), integer(), binary(), undefined | binary()) ->
    {ok, #{credential_id := integer(), credential := binary(), expires_at := undefined | binary()}}
    | {error, term()}.
issue_credential_tx(Conn, OrgId, AppId, Secret, ExpiresAt) when
    is_binary(Secret), Secret =/= <<>>
->
    PreId = enterprise_application_credential_repo:next_id(),
    Prefix = <<"ib_int_", (integer_to_binary(PreId))/binary>>,
    case
        enterprise_application_credential_repo:create_tx(
            Conn, OrgId, AppId, Prefix, Secret, ExpiresAt
        )
    of
        {ok, Row} ->
            %% 行 id 由 repo 生成（与 prefix 中的定位 id 独立）；
            %% credential_id 以行 id 为准（撤销/轮换按行 id 定位）。
            CredId = maps:get(<<"id">>, Row),
            {ok, #{
                credential_id => CredId,
                credential => <<Prefix/binary, ".", Secret/binary>>,
                expires_at => ExpiresAt
            }};
        {error, Reason} ->
            {error, Reason}
    end;
issue_credential_tx(_Conn, _OrgId, _AppId, _Secret, _ExpiresAt) ->
    {error, invalid_secret}.

%% @doc 池化签发凭证（生成式）。
-spec issue_credential(integer(), integer(), undefined | binary()) ->
    {ok, map()} | {error, term()}.
issue_credential(OrgId, AppId, ExpiresAt) ->
    pool(fun(Conn) -> issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) end).

%% @doc 事务内轮换凭证（同事务：签发新凭证 + 撤销旧凭证；
%% 新凭证创建失败时旧凭证保持有效——轮换原子向安全侧倾斜）。
-spec rotate_credential_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
rotate_credential_tx(Conn, OrgId, OldCredId) ->
    rotate_credential_tx(Conn, OrgId, OldCredId, undefined).

%% @doc 事务内轮换凭证（可指定新凭证过期时间）。
-spec rotate_credential_tx(any(), integer(), integer(), undefined | binary()) ->
    {ok, map()} | {error, term()}.
rotate_credential_tx(Conn, OrgId, OldCredId, ExpiresAt) ->
    case credential_application_id(Conn, OrgId, OldCredId) of
        {ok, AppId} ->
            case issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) of
                {ok, _} = Ok ->
                    case enterprise_application_credential_repo:revoke_tx(Conn, OrgId, OldCredId) of
                        ok -> Ok;
                        {error, not_active} -> Ok;
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 池化轮换凭证。
-spec rotate_credential(integer(), integer()) -> {ok, map()} | {error, term()}.
rotate_credential(OrgId, OldCredId) ->
    pool(fun(Conn) -> rotate_credential_tx(Conn, OrgId, OldCredId) end).

%% @doc 事务内撤销凭证（幂等语义见 repo：非 active → {error, not_active}）。
-spec revoke_credential_tx(any(), integer(), integer()) ->
    ok | {error, not_found | not_active | term()}.
revoke_credential_tx(Conn, OrgId, CredId) ->
    enterprise_application_credential_repo:revoke_tx(Conn, OrgId, CredId).

%% @doc 池化撤销凭证。
-spec revoke_credential(integer(), integer()) -> ok | {error, term()}.
revoke_credential(OrgId, CredId) ->
    pool(fun(Conn) -> revoke_credential_tx(Conn, OrgId, CredId) end).

%%%===================================================================
%%% 状态查询（redacted：不含 digest / 明文）
%%%===================================================================

%% @doc 事务内查询 application 状态与凭证列表（仅元数据；
%% 不含 secret_digest / 明文 credential —— redaction 红线）。
-spec application_status_tx(any(), integer(), integer()) -> {ok, map()} | {error, term()}.
application_status_tx(Conn, OrgId, AppId) ->
    case enterprise_application_repo:find_tx(Conn, OrgId, AppId) of
        {ok, App} ->
            Scopes =
                case jsone:decode(maps:get(<<"allowed_scopes">>, App, <<"[]">>)) of
                    L when is_list(L) -> [S || S <- L, is_binary(S)];
                    _ -> []
                end,
            {ok, #{
                application => #{
                    id => maps:get(<<"id">>, App),
                    application_key => maps:get(<<"application_key">>, App),
                    name => maps:get(<<"name">>, App),
                    status => maps:get(<<"status">>, App)
                },
                granted_scopes => Scopes,
                credentials => list_credentials(Conn, OrgId, AppId)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 池化查询 application 状态。
-spec application_status(integer(), integer()) -> {ok, map()} | {error, term()}.
application_status(OrgId, AppId) ->
    pool(fun(Conn) -> application_status_tx(Conn, OrgId, AppId) end).

%%%===================================================================
%%% Application Grant 治理（FULL-01：Org/Workspace Grant）
%%%===================================================================

%% @doc 事务内签发 Grant（Org Grant 或 Workspace Grant）。
%% Spec（map）：
%%   scopes                :: [binary()] 非空且全为固定枚举成员（必填）
%%   workspace_scope_kind  :: none（org 全域，缺省）| explicit（Workspace Grant）
%%   workspace_ids         :: [integer()]（explicit 必填非空；none 必须缺省/空）
%%   expires_at            :: binary RFC3339（必填）
%%   valid_from            :: binary RFC3339（缺省当前时间）
%%   idempotency_key       :: binary 非空（必填；(org, app) 内唯一）
%% 返回 {ok, Grant}（含 scopes / workspace_ids / version=1）。
%% 失败：{error, invalid_scope}（非固定枚举）| {error, missing_idempotency_key} |
%%   {error, empty_scopes} | {error, invalid_workspaces} | {error, key_conflict}
%%   （同键已存在，调用方应 read-back 而不是再发一份）|
%%   {error, application_not_found}（跨 Org / 不存在）|
%%   {error, workspace_not_found}（workspace 不存在或跨 Org）。
%% 语义提醒：第一次签发后该 Application 即「受 Grant 治理」，生效 scope 变为
%% allowed_scopes ∩ Grant scopes——签发比 allowed_scopes 窄的 Grant 会**立即**
%% 收窄该应用的权限面（这是 Grant 的预期语义）。
-spec issue_grant_tx(any(), integer(), integer(), map()) ->
    {ok, map()}
    | {error,
        invalid_scope
        | missing_idempotency_key
        | empty_scopes
        | invalid_workspaces
        | key_conflict
        | application_not_found
        | workspace_not_found
        | term()}.
issue_grant_tx(Conn, OrgId, AppId, Spec) when is_map(Spec) ->
    Scopes = maps:get(scopes, Spec, []),
    case validate_scopes(Scopes) of
        ok ->
            case grant_idempotency_key(Spec) of
                {ok, IdemKey} ->
                    RepoSpec = (maps:without([scopes], Spec))#{
                        scopes => Scopes, idempotency_key => IdemKey
                    },
                    case
                        enterprise_application_grant_repo:create_tx(Conn, OrgId, AppId, RepoSpec)
                    of
                        {ok, _} = Ok -> Ok;
                        {error, Reason} -> {error, grant_error(Reason)}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
issue_grant_tx(_Conn, _OrgId, _AppId, _Other) ->
    {error, invalid_spec}.

%% @doc 池化签发 Grant。
-spec issue_grant(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
issue_grant(OrgId, AppId, Spec) ->
    pool(fun(Conn) -> issue_grant_tx(Conn, OrgId, AppId, Spec) end).

%% @doc 事务内撤销 Grant（CAS：expected version + 仅 active 可撤销）。
%% 撤权立即生效：下一次 auth context 读取就看不到它（受管应用生效 scope 收窄）。
-spec revoke_grant_tx(any(), integer(), integer(), integer(), integer(), integer()) ->
    ok | {error, not_found | version_conflict | already_revoked | term()}.
revoke_grant_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, RevokedByUserId) ->
    enterprise_application_grant_repo:revoke_tx(
        Conn, OrgId, AppId, GrantId, ExpectedVersion, RevokedByUserId
    ).

%% @doc 池化撤销 Grant。
-spec revoke_grant(integer(), integer(), integer(), integer(), integer()) ->
    ok | {error, term()}.
revoke_grant(OrgId, AppId, GrantId, ExpectedVersion, RevokedByUserId) ->
    pool(fun(Conn) ->
        revoke_grant_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, RevokedByUserId)
    end).

%% @doc 事务内整体替换 Grant 的 scope 集合（CAS；scope downgrade 的机制）。
-spec set_grant_scopes_tx(any(), integer(), integer(), integer(), integer(), [binary()]) ->
    ok
    | {error,
        invalid_scope | empty_scopes | not_found | version_conflict | already_revoked | term()}.
set_grant_scopes_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, Scopes) ->
    case validate_scopes(Scopes) of
        ok ->
            enterprise_application_grant_repo:replace_scopes_tx(
                Conn, OrgId, AppId, GrantId, ExpectedVersion, Scopes
            );
        {error, _} = Err ->
            Err
    end.

%% @doc 池化替换 Grant scopes（降级/升级）。
-spec set_grant_scopes(integer(), integer(), integer(), integer(), [binary()]) ->
    ok | {error, term()}.
set_grant_scopes(OrgId, AppId, GrantId, ExpectedVersion, Scopes) ->
    pool(fun(Conn) ->
        set_grant_scopes_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, Scopes)
    end).

%% @doc 事务内整体替换 Grant 的 workspace 边界（CAS）。
%% Kind=none ⇒ org 全域授权（清空 workspace 行）；explicit ⇒ Workspace Grant
%% （workspace_ids 必须非空）。
-spec set_grant_workspaces_tx(
    any(), integer(), integer(), integer(), integer(), none | explicit, [integer()]
) ->
    ok
    | {error,
        invalid_workspaces
        | not_found
        | version_conflict
        | already_revoked
        | workspace_not_found
        | term()}.
set_grant_workspaces_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, Kind, WorkspaceIds) ->
    case
        enterprise_application_grant_repo:set_workspace_scope_tx(
            Conn, OrgId, AppId, GrantId, ExpectedVersion, Kind, WorkspaceIds
        )
    of
        ok -> ok;
        {error, Reason} -> {error, grant_error(Reason)}
    end.

%% @doc 池化替换 Grant workspace 边界。
-spec set_grant_workspaces(integer(), integer(), integer(), integer(), none | explicit, [integer()]) ->
    ok | {error, term()}.
set_grant_workspaces(OrgId, AppId, GrantId, ExpectedVersion, Kind, WorkspaceIds) ->
    pool(fun(Conn) ->
        set_grant_workspaces_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, Kind, WorkspaceIds)
    end).

%% @doc 事务内查询 Grant 治理状态（redacted：只有授权元数据，无 secret）。
%% 返回 #{grant_governed, effective_scopes, grants = [grant 摘要 + effective 标记]}。
-spec grant_status_tx(any(), integer(), integer()) -> {ok, map()} | {error, term()}.
grant_status_tx(Conn, OrgId, AppId) ->
    case enterprise_application_grant_repo:list_tx(Conn, OrgId, AppId) of
        {ok, Grants} ->
            case enterprise_application_grant_repo:grant_governed_tx(Conn, OrgId, AppId) of
                {ok, Governed} ->
                    case
                        enterprise_application_grant_repo:effective_scopes_tx(Conn, OrgId, AppId)
                    of
                        {ok, EffectiveScopes} ->
                            EffectiveIds = effective_ids(Conn, OrgId, AppId),
                            {ok, #{
                                grant_governed => Governed,
                                effective_scopes => EffectiveScopes,
                                grants => [grant_summary(G, EffectiveIds) || G <- Grants]
                            }};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 池化查询 Grant 治理状态。
-spec grant_status(integer(), integer()) -> {ok, map()} | {error, term()}.
grant_status(OrgId, AppId) ->
    pool(fun(Conn) -> grant_status_tx(Conn, OrgId, AppId) end).

-spec effective_ids(any(), integer(), integer()) -> sets:set(integer()).
effective_ids(Conn, OrgId, AppId) ->
    case enterprise_application_grant_repo:effective_grants_tx(Conn, OrgId, AppId) of
        {ok, Rows} -> sets:from_list([maps:get(<<"grant_id">>, R) || R <- Rows]);
        {error, _} -> sets:new()
    end.

-spec grant_summary(map(), sets:set(integer())) -> map().
grant_summary(G, EffectiveIds) ->
    GrantId = maps:get(<<"id">>, G),
    #{
        grant_id => GrantId,
        organization_id => maps:get(<<"organization_id">>, G),
        application_id => maps:get(<<"application_id">>, G),
        workspace_scope_kind => maps:get(<<"workspace_scope_kind">>, G),
        status => maps:get(<<"status">>, G),
        version => maps:get(<<"version">>, G),
        valid_from => maps:get(<<"valid_from">>, G),
        expires_at => maps:get(<<"expires_at">>, G),
        revoked_at => maps:get(<<"revoked_at">>, G),
        scopes => maps:get(<<"scopes">>, G, []),
        workspace_ids => maps:get(<<"workspace_ids">>, G, []),
        effective => sets:is_element(GrantId, EffectiveIds)
    }.

-spec grant_idempotency_key(map()) -> {ok, binary()} | {error, missing_idempotency_key}.
grant_idempotency_key(Spec) ->
    case maps:get(idempotency_key, Spec, undefined) of
        Key when is_binary(Key), Key =/= <<>> -> {ok, Key};
        _ -> {error, missing_idempotency_key}
    end.

%% Grant repo 错误 → 运维面稳定 atom（跨 Org/不存在一律 not_found 语义，不给 oracle）。
-spec grant_error(term()) -> atom() | term().
grant_error({foreign_key_violation, <<"fk_eag_application">>}) ->
    application_not_found;
grant_error({foreign_key_violation, <<"fk_eag_organization">>}) ->
    application_not_found;
grant_error({foreign_key_violation, <<"fk_eagw_workspace">>}) ->
    workspace_not_found;
grant_error({foreign_key_violation, _Other}) ->
    foreign_key_violation;
grant_error({check_violation, _Constraint}) ->
    invalid_grant_value;
grant_error(Reason) ->
    Reason.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec pool(fun()) -> term().
pool(Fun) ->
    case elib_pg:with_tx(Fun) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

%% 生命周期四值（迁移 00000143 放宽 ck_ea_status；与 imboyadmin
%% contracts.ts:APPLICATION_STATUSES 同序）。archived 是终态——合法迁移由上层裁决。
-spec validate_lifecycle(binary()) -> ok | {error, invalid_status}.
validate_lifecycle(S) when
    S =:= <<"draft">>; S =:= <<"active">>; S =:= <<"disabled">>; S =:= <<"archived">>
->
    ok;
validate_lifecycle(_) ->
    {error, invalid_status}.

-spec validate_scopes([binary()]) -> ok | {error, invalid_scope}.
validate_scopes(Scopes) when is_list(Scopes) ->
    Fixed = enterprise_internal_scope:all(),
    case [S || S <- Scopes, not lists:member(S, Fixed)] of
        [] -> ok;
        _Bad -> {error, invalid_scope}
    end;
validate_scopes(_) ->
    {error, invalid_scope}.

%% 32 字节 CSPRNG → base64url（无 padding，43 字符，熵 ≥ 256 bit）。
-spec generate_secret() -> binary().
generate_secret() ->
    Raw = crypto:strong_rand_bytes(32),
    B64 = base64:encode(Raw),
    NoPad = binary:part(B64, 0, byte_size(B64) - 2),
    <<<<(urlsafe(C))/binary>> || <<C>> <= NoPad>>.

urlsafe($+) -> <<"-">>;
urlsafe($/) -> <<"_">>;
urlsafe(C) -> <<C>>.

-spec credential_application_id(any(), integer(), integer()) ->
    {ok, integer()} | {error, not_found | term()}.
credential_application_id(Conn, OrgId, CredId) ->
    Sql =
        <<"SELECT application_id FROM enterprise_application_credential",
            " WHERE organization_id = $1 AND id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, CredId]) of
        {ok, [#{<<"application_id">> := AppId} | _]} ->
            {ok, AppId};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 池化列出 credential **元数据**（Admin 治理面 A-05）。
%% SELECT 列**永不**含 secret_digest / 明文 —— 与 application_status_tx/3 同源的
%% redaction 红线：Admin 读面一旦投影摘要就等于把可离线爆破的凭据哈希下发到浏览器。
-spec list_credentials(integer(), integer()) -> [map()].
list_credentials(OrgId, AppId) ->
    pool(fun(Conn) -> list_credentials(Conn, OrgId, AppId) end).

-spec list_credentials(any(), integer(), integer()) -> [map()].
list_credentials(Conn, OrgId, AppId) ->
    Sql =
        <<"SELECT id, credential_prefix, status, created_at, expires_at, last_used_at, revoked_at",
            " FROM enterprise_application_credential",
            " WHERE organization_id = $1 AND application_id = $2 ORDER BY id">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, Rows} when is_list(Rows) ->
            Rows;
        _ ->
            []
    end.
