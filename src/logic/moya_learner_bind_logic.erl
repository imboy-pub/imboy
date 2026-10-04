-module(moya_learner_bind_logic).
-moduledoc "墨芽学员账号绑定逻辑（Step 16）—— 管理侧最小动作，无自助 UI。".
%%%
% 墨芽学员账号绑定逻辑（Step 16：管理侧最小动作，无自助 UI，计划 §6.4）
% Learner account binding logic
%
% 设计：
%   - bind_learner/3 / unbind_learner/2 —— 管理侧动作，Operator 必须是该
%     Organization owner 或该学员所在班 class manager（deny-by-default）
%   - 绑定不复制/不迁移 submission/teacher_review（BIND-01：历史恒归 learner_id）
%   - 解绑仅置 user_id=NULL（与 user 注销 FK SET NULL 语义一致），账号本人
%     立即失去历史入口（BIND-02）；learner/submission/review 全部保留
%   - 审计：learner 行 account_bound_at/by + 结构化日志（无 PII：不落
%     display_name；审计用户匿名化策略遵循 STEP-08-DB sentinel 建议，
%     见 handoff —— DB 审计表需求登记，不自行建表）
%   - 跨 Org 同 user 绑定独立 learner 允许（uk_learner_org_user 部分唯一，
%     00000096 已支持）；同 Org 重复绑定 → duplicate_bind_in_org
%
% 路由：本波不挂路由（禁改 imboy_router.erl）；所需路由段见 STEP-16 handoff。
%%%

-export([bind_learner/3, unbind_learner/2]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 绑定学员档案与 IMBoy 账号（管理侧）。
%% 错误：learner_not_found / learner_inactive / not_authorized /
%%       duplicate_bind_in_org / invalid_target_user / db_error
%% 成功动作在**同一事务**内写 teaching_admin_audit（00000099，Step 16 接线）；
%% 被拒动作不入 DB 审计（日志级 audit_rejected 已覆盖）。
-spec bind_learner(integer(), integer(), integer()) ->
    {ok, map()} | {error, atom()}.
bind_learner(OperatorUid, LearnerId, TargetUserId) ->
    Tx = fun(Conn) ->
        case moya_learner_bind_repo:operator_role_tx(Conn, OperatorUid, LearnerId) of
            {error, Reason} ->
                ?LOG_ERROR("learner_bind guard db error ~p", [Reason]),
                {error, db_error};
            unauthorized ->
                {error, not_authorized};
            Role ->
                case moya_learner_bind_repo:bind_tx(Conn, LearnerId, TargetUserId, OperatorUid) of
                    {ok, Row} ->
                        ok = audit_admin_tx(
                            Conn,
                            bind_learner,
                            Role,
                            OperatorUid,
                            LearnerId,
                            TargetUserId
                        ),
                        audit(bind, Role, OperatorUid, LearnerId, TargetUserId),
                        {ok, Row};
                    {error, duplicate_bind_in_org} = E ->
                        audit_rejected(
                            bind,
                            Role,
                            OperatorUid,
                            LearnerId,
                            TargetUserId,
                            duplicate_bind_in_org
                        ),
                        E;
                    {error, Reason} ->
                        {error, reason_atom(Reason)}
                end
        end
    end,
    tx_run(Tx).

%% @doc 解绑（管理侧）：仅清 user_id；不删任何教学数据。
-spec unbind_learner(integer(), integer()) -> {ok, map()} | {error, atom()}.
unbind_learner(OperatorUid, LearnerId) ->
    Tx = fun(Conn) ->
        case moya_learner_bind_repo:operator_role_tx(Conn, OperatorUid, LearnerId) of
            {error, Reason} ->
                ?LOG_ERROR("learner_unbind guard db error ~p", [Reason]),
                {error, db_error};
            unauthorized ->
                {error, not_authorized};
            Role ->
                case moya_learner_bind_repo:unbind_tx(Conn, LearnerId, OperatorUid) of
                    {ok, Row} ->
                        ok = audit_admin_tx(
                            Conn,
                            unbind_learner,
                            Role,
                            OperatorUid,
                            LearnerId,
                            null
                        ),
                        audit(unbind, Role, OperatorUid, LearnerId, null),
                        {ok, Row};
                    {error, not_bound} = E ->
                        audit_rejected(unbind, Role, OperatorUid, LearnerId, null, not_bound),
                        E;
                    {error, Reason} ->
                        {error, reason_atom(Reason)}
                end
        end
    end,
    tx_run(Tx).

%%%===================================================================
%%% Internal
%%%===================================================================

%% DB 审计（00000099 teaching_admin_audit）：成功动作与业务变更同事务落一行。
%% 列：id(TSID) / action(枚举 bind_learner|unbind_learner) / operator_uid(裸列，
%% 0=sentinel) / learner_id / target_user_id(unbind 为 NULL=本无目标) /
%% detail(含操作时角色)。被拒动作不落行（日志级审计覆盖）。
-spec audit_admin_tx(
    any(),
    bind_learner | unbind_learner,
    atom(),
    integer(),
    integer(),
    integer() | null
) -> ok | no_return().
audit_admin_tx(Conn, Action, Role, OperatorUid, LearnerId, TargetUserId) ->
    Sql =
        <<"INSERT INTO ", (elib_pg_sql:public_tablename(<<"teaching_admin_audit">>))/binary,
            " (id, action, operator_uid, learner_id, target_user_id, detail) "
            "VALUES ($1, $2, $3, $4, $5, $6)">>,
    Detail = jsone:encode(#{<<"role">> => atom_to_binary(Role, utf8)}),
    case
        elib_pg:execute(
            Conn,
            Sql,
            [
                elib_tsid:generate(),
                atom_to_binary(Action, utf8),
                OperatorUid,
                LearnerId,
                TargetUserId,
                Detail
            ]
        )
    of
        {ok, 1} ->
            ok;
        {error, Reason} ->
            %% 审计与业务变更同事务：审计写失败则整体回滚（fail-closed）
            ?LOG_ERROR("teaching_admin_audit insert failed ~p", [Reason]),
            error({audit_insert_failed, Reason})
    end.

%% with_tx = epgsql:with_transaction：透传 Tx fun 的原始返回值（{ok,_} 或
%% 业务 {error, ReasonAtom}），仅异常/rollback 折叠为 db_error。
%% 修复（R6，handler 契约测试暴露）：原实现 `case {ok, Result} -> Result`
%% 会把 {ok, Row} 再解一层（调用方拿到裸 Row 而非 {ok, Row}），且把业务
%% 错误原子（not_authorized 等）误折叠为 db_error。
tx_run(Tx) ->
    case elib_pg:with_tx(Tx, [{reraise, false}]) of
        {rollback, Reason} ->
            ?LOG_ERROR("learner_bind tx rollback ~p", [Reason]),
            {error, db_error};
        Result ->
            Result
    end.

%% 审计日志：只含 ID 与事件，不含 display_name / PII（计划 §9.3）。
audit(Event, Role, OperatorUid, LearnerId, TargetUserId) ->
    ?LOG_INFO(
        "[teaching_learner_bind] event=~p role=~p operator_uid=~p learner_id=~p target=~p",
        [Event, Role, OperatorUid, LearnerId, TargetUserId]
    ).

audit_rejected(Event, Role, OperatorUid, LearnerId, TargetUserId, Why) ->
    ?LOG_INFO(
        "[teaching_learner_bind] event=~p rejected=~p role=~p operator_uid=~p learner_id=~p target=~p why=~p",
        [Event, true, Role, OperatorUid, LearnerId, TargetUserId, Why]
    ).

%% repo 侧 Reason 可能是原子（已分类）或 epgsql map（未分类）——只原子化已知形态。
reason_atom(duplicate_bind_in_org) -> duplicate_bind_in_org;
reason_atom(invalid_target_user) -> invalid_target_user;
reason_atom(learner_not_found) -> learner_not_found;
reason_atom(learner_inactive) -> learner_inactive;
reason_atom(not_bound) -> not_bound;
reason_atom(_) -> db_error.
