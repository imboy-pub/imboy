-module(organization_agent_membership_app).

%% Agent 成员生命周期受控命令（application，ORG-06）。
%%
%% Agent Organization Contract §3/§8：Agent membership 的 create / suspend /
%% restore / remove 必须受 **Human** owner/admin 管理并审计；Agent Domain
%% 不得直写 Organization Core 表——本模块即受控 command 入口。
%%
%% 冻结语义：
%%   * member-only：attach 只以 role='member' 落库（owner/admin 必须 Human；
%%     owner 另有 126/127 DB invariant 兜底；admin 无 DB guard，
%%     由本命令与 facts 侧 fail closed）；
%%   * archived Org 一律拒（409 org_archived，C16 archived 禁新写）；
%%   * 每条命令带 ExpectedVersion（乐观并发：与当前 fact_version 不符 = stale 拒，
%%     undefined 跳过）与 IdempotencyKey（审计承载；状态机天然幂等——
%%     重复命令返回稳定结果、不重复审计）；
%%   * 行不存在时版本按 0 语义处理：带版本的"创建预期"即 stale 拒；
%%   * 锁顺序与既有治理写同口径：组织行先（FOR UPDATE）→ 操作人成员行
%%     （FOR SHARE）→ 目标成员行（FOR UPDATE）。
%%
%% 错误码（稳定口径）：400 形状 / 403 越权或非 Agent / 404 不存在 /
%% 409 org_archived、stale、role_conflict、member_not_active / 500 兜底。

-export([attach/5, suspend/5, restore/5, remove/5]).

-include("log.hrl").

-type idem_key() :: binary().
-type expected_version() :: undefined | non_neg_integer().

%% ===================================================================
%% 命令
%% ===================================================================

%% @doc 授予/恢复 Agent 成员身份（角色恒 member）。
%% 幂等：active member 重复 attach → {ok, unchanged}（不重复审计）；
%% suspended/removed 行 → 显式恢复为 active member。
-spec attach(integer(), integer(), integer(), expected_version(), idem_key()) ->
    {ok, map()} | {error, {integer(), binary()}}.
attach(OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey) ->
    command(attach, OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey).

%% @doc 暂停 Agent 成员（立即撤权事实，可恢复；不删账号）。
-spec suspend(integer(), integer(), integer(), expected_version(), idem_key()) ->
    {ok, map()} | {error, {integer(), binary()}}.
suspend(OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey) ->
    command(suspend, OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey).

%% @doc 恢复暂停中的 Agent 成员（仅 suspended → active；removed 不可 restore）。
-spec restore(integer(), integer(), integer(), expected_version(), idem_key()) ->
    {ok, map()} | {error, {integer(), binary()}}.
restore(OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey) ->
    command(restore, OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey).

%% @doc 移除 Agent 成员（active|suspended → removed；removed 幂等 unchanged）。
-spec remove(integer(), integer(), integer(), expected_version(), idem_key()) ->
    {ok, map()} | {error, {integer(), binary()}}.
remove(OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey) ->
    command(remove, OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec command(
    attach | suspend | restore | remove,
    integer(),
    integer(),
    integer(),
    expected_version(),
    idem_key()
) ->
    {ok, map()} | {error, {integer(), binary()}}.
command(Action, OperatorUid, OrgId, AgentId, ExpectedVersion, IdempotencyKey) ->
    case precheck(Action, OperatorUid, OrgId, AgentId, IdempotencyKey) of
        {error, _} = Error ->
            Error;
        ok ->
            Tx = fun(Conn) ->
                command_tx(Conn, Action, OperatorUid, OrgId, AgentId, ExpectedVersion)
            end,
            case elib_pg:with_tx(Tx) of
                {ok, {changed, Member}} ->
                    %% 状态实际发生变化：审计一次（幂等重放不进此分支）
                    ok =
                        ?INFO_LOG([
                            audit_tag(Action), OrgId, AgentId, OperatorUid, IdempotencyKey
                        ]),
                    {ok, member_result(OrgId, AgentId, Member, changed)};
                {ok, {unchanged, Member}} ->
                    %% 幂等重放：状态未变，返回稳定当前事实，不重复审计
                    {ok, member_result(OrgId, AgentId, Member, unchanged)};
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    {error, {Code, Msg}};
                {rollback, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    {error, {Code, Msg}};
                {rollback, Reason} ->
                    ?ERROR_LOG([organization_agent_member_failed, Action, OrgId, AgentId, Reason]),
                    {error, {500, fallback_msg()}};
                {error, Reason} ->
                    ?ERROR_LOG([organization_agent_member_failed, Action, OrgId, AgentId, Reason]),
                    {error, {500, fallback_msg()}}
            end
    end.

-spec precheck(atom(), integer(), integer(), integer(), idem_key()) ->
    ok | {error, {integer(), binary()}}.
precheck(_Action, OperatorUid, OrgId, AgentId, IdempotencyKey) when
    is_integer(OperatorUid),
    OperatorUid > 0,
    is_integer(OrgId),
    OrgId > 0,
    is_integer(AgentId),
    AgentId > 0,
    is_binary(IdempotencyKey)
->
    ok;
precheck(_, _, _, _, _) ->
    {error, {400, invalid_args_msg()}}.

%% 事务体：锁序 组织行 → 操作人行 → 目标身份 → 目标成员行 → 裁决 → 写。
-spec command_tx(
    any(),
    attach | suspend | restore | remove,
    integer(),
    integer(),
    integer(),
    expected_version()
) ->
    {ok, {changed | unchanged, map()}} | no_return().
command_tx(Conn, Action, OperatorUid, OrgId, AgentId, ExpectedVersion) ->
    %% 1) 组织行锁 + 生命周期门禁（archived fail closed）
    case organization_owner_store:lock_organization_tx(Conn, OrgId) of
        {ok, #{<<"status">> := <<"active">>}} ->
            ok;
        {ok, _Archived} ->
            abort(409, org_archived_msg());
        {error, not_found} ->
            abort(404, org_not_found_msg());
        {error, _Reason1} ->
            abort(500, fallback_msg())
    end,
    %% 2) 操作人必须是 active Human owner/admin（合同 §3）
    case organization_agent_membership_pg:lock_operator_tx(Conn, OrgId, OperatorUid) of
        {ok, #{<<"role">> := Role, <<"account_type">> := AccountType}} when
            Role =:= <<"owner">>; Role =:= <<"admin">>
        ->
            case
                organization_agent_boundary:ensure_human_identity(
                    elib_cnv:safe_to_integer(AccountType)
                )
            of
                ok ->
                    ok;
                {error, not_human} ->
                    abort(403, operator_not_human_msg())
            end;
        {ok, _} ->
            abort(403, operator_not_authorized_msg());
        {error, not_found} ->
            abort(403, operator_not_authorized_msg());
        {error, _Reason2} ->
            abort(500, fallback_msg())
    end,
    %% 3) 目标必须是 Agent 身份（account_type=1；member-only 命令入口）
    case organization_agent_membership_pg:subject_account_type_tx(Conn, AgentId) of
        {ok, AccountType2} ->
            case organization_agent_boundary:ensure_agent_identity(AccountType2) of
                ok ->
                    ok;
                {error, not_agent} ->
                    abort(403, not_agent_msg())
            end;
        {error, not_found} ->
            abort(404, subject_not_found_msg());
        {error, _Reason3} ->
            abort(500, fallback_msg())
    end,
    %% 4) 目标成员行锁（行不存在按版本 0 语义）
    Subject =
        case organization_agent_membership_pg:lock_subject_tx(Conn, OrgId, AgentId) of
            {ok, Row} ->
                Row;
            {error, not_found} ->
                #{<<"role">> => null, <<"status">> => null, <<"fact_version">> => 0};
            {error, _Reason4} ->
                abort(500, fallback_msg())
        end,
    %% 5) 乐观并发：ExpectedVersion 与当前不符 = stale 拒
    case
        organization_agent_boundary:stale_version(
            ExpectedVersion, elib_cnv:safe_to_integer(maps:get(<<"fact_version">>, Subject))
        )
    of
        stale ->
            abort(409, stale_version_msg());
        _FreshOrNone ->
            ok
    end,
    %% 6) 状态机裁决（member-only；裁决语义在 domain/本层，SQL 只执行）
    transition_tx(Conn, Action, OperatorUid, OrgId, AgentId, Subject).

-spec transition_tx(
    any(),
    attach | suspend | restore | remove,
    integer(),
    integer(),
    integer(),
    map()
) ->
    {ok, {changed | unchanged, map()}}.
transition_tx(Conn, attach, OperatorUid, OrgId, AgentId, Subject) ->
    Status = maps:get(<<"status">>, Subject),
    case Status of
        null ->
            case
                organization_agent_membership_pg:insert_member_tx(Conn, OrgId, AgentId, OperatorUid)
            of
                {ok, Row} ->
                    {ok, {changed, Row}};
                {error, _Reason} ->
                    abort(500, fallback_msg())
            end;
        <<"active">> ->
            case maps:get(<<"role">>, Subject) of
                <<"member">> ->
                    {ok, {unchanged, Subject}};
                _OwnerOrAdmin ->
                    %% 治理角色行拒绝静默改写（fail closed）
                    abort(409, role_conflict_msg())
            end;
        _SuspendedOrRemoved ->
            case
                organization_agent_membership_pg:activate_member_tx(
                    Conn, OrgId, AgentId, OperatorUid
                )
            of
                {ok, Row} ->
                    {ok, {changed, Row}};
                {error, _Reason} ->
                    abort(500, fallback_msg())
            end
    end;
transition_tx(Conn, suspend, _OperatorUid, OrgId, AgentId, Subject) ->
    move_tx(Conn, OrgId, AgentId, Subject, <<"active">>, <<"suspended">>);
transition_tx(Conn, restore, _OperatorUid, OrgId, AgentId, Subject) ->
    move_tx(Conn, OrgId, AgentId, Subject, <<"suspended">>, <<"active">>);
transition_tx(Conn, remove, _OperatorUid, OrgId, AgentId, Subject) ->
    %% remove 接受 active 与 suspended 两个来源态（两步离场 EB-08 语义）；
    %% removed 幂等 unchanged；行不存在 / 其他 → 409。
    case maps:get(<<"status">>, Subject) of
        <<"removed">> ->
            {ok, {unchanged, Subject}};
        <<"active">> ->
            do_move_tx(Conn, OrgId, AgentId, <<"active">>, <<"removed">>);
        <<"suspended">> ->
            do_move_tx(Conn, OrgId, AgentId, <<"suspended">>, <<"removed">>);
        _Null ->
            abort(409, member_not_active_msg())
    end.

%% suspend/restore 的统一迁移：目标态一致 → unchanged；可迁移 → changed；
%% 其余（removed / 行不存在）→ 409。
-spec move_tx(any(), integer(), integer(), map(), binary(), binary()) ->
    {ok, {changed | unchanged, map()}}.
move_tx(Conn, OrgId, AgentId, Subject, ExpectedStatus, TargetStatus) ->
    case maps:get(<<"status">>, Subject) of
        TargetStatus ->
            {ok, {unchanged, Subject}};
        ExpectedStatus ->
            do_move_tx(Conn, OrgId, AgentId, ExpectedStatus, TargetStatus);
        _Other ->
            abort(409, member_not_active_msg())
    end.

%% 执行精确单列状态迁移（WHERE 带期望来源态，双保险）。
-spec do_move_tx(any(), integer(), integer(), binary(), binary()) ->
    {ok, {changed, map()}}.
do_move_tx(Conn, OrgId, AgentId, ExpectedStatus, TargetStatus) ->
    case
        organization_agent_membership_pg:set_status_tx(
            Conn, OrgId, AgentId, ExpectedStatus, TargetStatus
        )
    of
        {ok, Row} ->
            {ok, {changed, Row}};
        {error, _Reason} ->
            abort(500, fallback_msg())
    end.

-spec member_result(integer(), integer(), map(), changed | unchanged) -> map().
member_result(OrgId, AgentId, Row, StatusTag) ->
    #{
        status_tag => StatusTag,
        organization_id => OrgId,
        subject_user_id => AgentId,
        status => maps:get(<<"status">>, Row),
        role => maps:get(<<"role">>, Row),
        fact_version => elib_cnv:safe_to_integer(maps:get(<<"fact_version">>, Row, 0))
    }.

-spec audit_tag(attach | suspend | restore | remove) -> atom().
audit_tag(attach) ->
    organization_agent_member_attached;
audit_tag(suspend) ->
    organization_agent_member_suspended;
audit_tag(restore) ->
    organization_agent_member_restored;
audit_tag(remove) ->
    organization_agent_member_removed.

-spec abort(integer(), binary()) -> no_return().
abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).

-spec org_not_found_msg() -> binary().
org_not_found_msg() ->
    <<"Organization 不存在"/utf8>>.

-spec org_archived_msg() -> binary().
org_archived_msg() ->
    <<"Organization 已归档，Agent 成员管理被拒绝"/utf8>>.

-spec operator_not_authorized_msg() -> binary().
operator_not_authorized_msg() ->
    <<"仅 Organization Human Owner 或 Admin 可管理 Agent 成员"/utf8>>.

-spec operator_not_human_msg() -> binary().
operator_not_human_msg() ->
    <<"Agent 成员管理操作人必须是 Human（account_type=0）"/utf8>>.

-spec not_agent_msg() -> binary().
not_agent_msg() ->
    <<"目标主体必须是 Agent 身份（account_type=1）"/utf8>>.

-spec subject_not_found_msg() -> binary().
subject_not_found_msg() ->
    <<"目标主体用户不存在"/utf8>>.

-spec stale_version_msg() -> binary().
stale_version_msg() ->
    <<"成员事实版本已过期（stale fact_version），请重新读取后重试"/utf8>>.

-spec role_conflict_msg() -> binary().
role_conflict_msg() ->
    <<"目标行持有治理角色，拒绝改写为 Agent member"/utf8>>.

-spec member_not_active_msg() -> binary().
member_not_active_msg() ->
    <<"Agent 成员不存在或已移除"/utf8>>.

-spec invalid_args_msg() -> binary().
invalid_args_msg() ->
    <<"参数必须是正整数，幂等键必须是 binary"/utf8>>.

-spec fallback_msg() -> binary().
fallback_msg() ->
    <<"操作失败，请稍后重试"/utf8>>.
