-module(moya_learner_bind_repo).
-moduledoc "墨芽学员账号绑定数据仓库（Step 16）。".
%%%
% 墨芽学员账号绑定数据仓库（Step 16：管理侧最小动作，无自助 UI）
% Learner account binding repository
%
% 职责：
%   - bind/unbind 的事务内 SQL（_tx 变体直传连接，测试与生产同一代码路径）
%   - 操作者守卫查询（Org owner 或该学员所在班 class manager —— deny-by-default）
%   - 23505（uk_learner_org_user）错误分类
%
% 审计（无 PII）：
%   - learner 行自带 account_bound_at / account_bound_by（00000096 §6.4）
%   - logic 层结构化日志；不写 display_name / 任何儿童信息
%   - 主动解绑保留最近一次绑定的 account_bound_* 字段作为痕迹
%     （account_bound_by 指向最近一次绑定的操作人；user_id=NULL 即当前未绑定）
%
% 禁改清单遵守：本模块不依赖 B 的文件；表结构 00000096/97/98 只读。
%%%

-export([tablename/1]).
-export([find_tx/2, bind_tx/4, unbind_tx/3]).
-export([operator_role_tx/3]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec tablename(binary()) -> binary().
tablename(Tb) ->
    elib_pg_sql:public_tablename(Tb).

%% @doc learner 行（含绑定字段）。不存在 → {ok, undefined}。
-spec find_tx(any(), integer()) -> {ok, map() | undefined} | {error, term()}.
find_tx(Conn, LearnerId) ->
    Sql = <<
        "SELECT id, organization_id, display_name, user_id, "
        "account_bound_at, account_bound_by, status "
        "FROM ",
        (tb(learner))/binary,
        " WHERE id = $1"
    >>,
    case elib_pg:query(Conn, Sql, [LearnerId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 操作者守卫：Org owner 或 学员所在任一 active 班的 class manager。
%% deny-by-default：查不到关系即 unauthorized（不区分细节）。
%% 返回 owner | manager | unauthorized。
-spec operator_role_tx(any(), integer(), integer()) ->
    owner | manager | unauthorized | {error, term()}.
operator_role_tx(Conn, OperatorUid, LearnerId) ->
    OwnerSql =
        <<"SELECT o.owner_id FROM ", (tb(organization))/binary,
            " o "
            "JOIN ", (tb(learner))/binary,
            " l ON l.organization_id = o.id "
            "WHERE l.id = $1 AND o.owner_id = $2">>,
    case elib_pg:query(Conn, OwnerSql, [LearnerId, OperatorUid]) of
        {ok, [_ | _]} ->
            owner;
        {ok, []} ->
            ManagerSql =
                <<"SELECT 1 FROM ", (tb(class_staff))/binary,
                    " cs "
                    "JOIN ", (tb(class_enrollment))/binary,
                    " e "
                    " ON e.group_id = cs.group_id AND e.status = 'active' "
                    "WHERE e.learner_id = $1 AND cs.user_id = $2 "
                    "AND cs.role = 'manager' AND cs.status = 'active' LIMIT 1">>,
            case elib_pg:query(Conn, ManagerSql, [LearnerId, OperatorUid]) of
                {ok, [_ | _]} -> manager;
                {ok, []} -> unauthorized;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 绑定：UPDATE learner SET user_id/at/by。
%% 23505 → duplicate_bind_in_org（同 Org 同 user 已绑其他 learner；跨 Org 独立绑定不受影响——
%% uk_learner_org_user 为 (organization_id, user_id) 部分唯一索引，00000096）。
-spec bind_tx(any(), integer(), integer(), integer()) ->
    {ok, map()} | {error, duplicate_bind_in_org | learner_not_found | learner_inactive | term()}.
bind_tx(Conn, LearnerId, TargetUserId, OperatorUid) ->
    case find_tx(Conn, LearnerId) of
        {ok, undefined} ->
            {error, learner_not_found};
        {ok, #{<<"status">> := <<"active">>}} ->
            Sql =
                <<"UPDATE ", (tb(learner))/binary,
                    " SET user_id = $2, account_bound_at = now(), account_bound_by = $3, "
                    "updated_at = now() "
                    "WHERE id = $1 RETURNING id, organization_id, user_id, "
                    "account_bound_at, account_bound_by, status">>,
            case elib_pg:query(Conn, Sql, [LearnerId, TargetUserId, OperatorUid]) of
                {ok, [Row | _]} -> {ok, Row};
                {ok, []} -> {error, learner_not_found};
                {error, Reason} -> {error, classify_bind_error(Reason)}
            end;
        {ok, _} ->
            {error, learner_inactive};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 解绑：仅置 user_id = NULL（与 user 表 FK SET NULL 语义一致）。
%% 不删 learner / submission / review（BIND-02）；保留 account_bound_* 为最近绑定痕迹。
-spec unbind_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, learner_not_found | not_bound | term()}.
unbind_tx(Conn, LearnerId, _OperatorUid) ->
    case find_tx(Conn, LearnerId) of
        {ok, undefined} ->
            {error, learner_not_found};
        {ok, #{<<"user_id">> := null}} ->
            {error, not_bound};
        {ok, _} ->
            Sql =
                <<"UPDATE ", (tb(learner))/binary,
                    " SET user_id = NULL, updated_at = now() "
                    "WHERE id = $1 RETURNING id, organization_id, user_id, "
                    "account_bound_at, account_bound_by, status">>,
            case elib_pg:query(Conn, Sql, [LearnerId]) of
                {ok, [Row | _]} -> {ok, Row};
                {ok, []} -> {error, learner_not_found};
                {error, Reason} -> {error, classify_bind_error(Reason)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc epgsql 错误分类：23505+uk_learner_org_user → duplicate_bind_in_org；
%% 23503（FK）→ invalid_target_user；其余透传。
%% 兼容 epgsql 两种错误形态：map（新接口）与 {error_error, Sev, Code, Name, Msg, Extra} tuple（旧接口）。
-spec classify_bind_error(term()) -> atom() | term().
classify_bind_error(#{code := <<"23505">>, constraint := <<"uk_learner_org_user">>}) ->
    duplicate_bind_in_org;
classify_bind_error(#{code := 23505, constraint := <<"uk_learner_org_user">>}) ->
    duplicate_bind_in_org;
classify_bind_error({error, _, <<"23505">>, unique_violation, _, Extra}) when
    is_list(Extra)
->
    case proplists:get_value(constraint_name, Extra) of
        <<"uk_learner_org_user">> -> duplicate_bind_in_org;
        _ -> duplicate_bind_in_org
    end;
classify_bind_error({error_error, _, <<"23505">>, unique_violation, _, Extra}) when
    is_list(Extra)
->
    case proplists:get_value(constraint_name, Extra) of
        <<"uk_learner_org_user">> -> duplicate_bind_in_org;
        _ -> duplicate_bind_in_org
    end;
classify_bind_error(#{code := <<"23503">>}) ->
    invalid_target_user;
classify_bind_error(#{code := 23503}) ->
    invalid_target_user;
classify_bind_error({error, _, <<"23503">>, foreign_key_violation, _, _}) ->
    invalid_target_user;
classify_bind_error({error_error, _, <<"23503">>, foreign_key_violation, _, _}) ->
    invalid_target_user;
classify_bind_error(Reason) ->
    Reason.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 与 B 的 moya_context_repo 同构：先转 binary（直连测试模式下
%% config_ds:env 未初始化，public_tablename 可能原样返回 atom，atom 不能
%% 直接进 binary 拼接段）。
tb(Tb) when is_atom(Tb) ->
    tablename(ec_cnv:to_binary(Tb));
tb(Tb) ->
    tablename(Tb).
