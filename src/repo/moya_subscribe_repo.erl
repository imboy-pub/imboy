-module(moya_subscribe_repo).
-moduledoc "墨芽订阅消息一次性授权额度数据仓库（W3 服务端依赖之一）。".
%%%
% 墨芽订阅消息一次性授权额度数据仓库（W3 服务端依赖之一）
% Moya subscribe-message one-shot grant repository
%
% 职责：
%   - 客户端授权上报落库（每次 accept 一行 pending 额度）
%   - 发布回评时的额度消费：条件 UPDATE 防并发双发（两处发布同时触发
%     下发时，UPDATE ... WHERE status='pending' 的行锁保证只有一方
%     拿到额度；另一方 affected=0 视为无额度，静默跳过）
%   - 不存 openid（PII）：send 时由 logic 层经 sso_identity 反查
%   - 下发侧只读查询：notification_context（提交 → 学员名 + 作业标题，
%     消息占位符数据源）与 view_guardians（可看回评的 active 监护人 uid 列表）；
%     SELECT 列不含 openid / 联系方式（P0-2 同款纪律）
%
% 表：moya_subscribe_grant（迁移 00000152）。消费链路不需要 _tx 变体：
%   额度消费与微信外呼不能同事务（provider HTTP 在事务内的教训见
%   moya_ai_worker / PREVIEW_ENV_v1：20~30s 长事务），故 consume 走
%   单语句条件 UPDATE 自带原子性，无需外层事务。
%%%

-export([tablename/0]).
-export([insert_grants/2, pending_grant/2, consume_grant/3]).
-export([notification_context/1, view_guardians/1]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moya_subscribe_grant">>).

%% @doc 授权上报落库：每个模板一行 pending。
%% 幂等性说明：客户端可能因网络重试重复上报——多记一行额度是可接受的
%% 过授权（最坏情况 = 家长多收到一条通知），比加去重窗口的复杂度低；
%% 且上报入口只接受当前配置的模板 ID（logic 层白名单校验）。
-spec insert_grants(integer(), [binary()]) -> {ok, non_neg_integer()} | {error, term()}.
insert_grants(_Uid, []) ->
    {ok, 0};
insert_grants(Uid, TemplateIds) ->
    %% created_at 走表 DEFAULT CURRENT_TIMESTAMP；unnest 批量写法与
    %% enterprise_application_grant_repo 同款
    Sql = <<
        "INSERT INTO ",
        (tablename())/binary,
        " (uid, template_id)"
        " SELECT $1, t FROM unnest($2::text[]) AS t(template_id)"
    >>,
    case elib_pg:query(Sql, [Uid, TemplateIds]) of
        {ok, _} ->
            {ok, length(TemplateIds)};
        {error, Reason} ->
            ?LOG_ERROR("moya_subscribe_repo:insert_grants error ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 取 (uid, template_id) 最近一条 pending 额度。
%% {ok, Id} | {ok, undefined}（无额度，静默跳过下发）。
-spec pending_grant(integer(), binary()) -> {ok, integer() | undefined} | {error, term()}.
pending_grant(Uid, TemplateId) ->
    Sql = <<
        "SELECT id FROM ",
        (tablename())/binary,
        " WHERE uid = $1 AND template_id = $2 AND status = 'pending'"
        " ORDER BY id DESC LIMIT 1"
    >>,
    case elib_pg:query(Sql, [Uid, TemplateId]) of
        {ok, [#{<<"id">> := Id} | _]} ->
            {ok, Id};
        {ok, []} ->
            {ok, undefined};
        {error, Reason} ->
            ?LOG_ERROR("moya_subscribe_repo:pending_grant error ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 条件消费一条额度（pending → consumed）。
%% 返回 true=抢到额度可下发；false=已被并发消费（跳过）。
%% 微信 send 失败时**不回滚**额度（rollback 需要事务且引入「HTTP 在事务内」
%% 反模式；一次性授权被 errcode 拒绝的场景——如用户已关闭接收——额度本身
%% 已不可用，消耗掉是正确语义）。
-spec consume_grant(integer(), integer(), integer()) -> boolean().
consume_grant(GrantId, Uid, SubmissionId) ->
    Sql = <<
        "UPDATE ",
        (tablename())/binary,
        " SET status = 'consumed', consumed_at = NOW(),"
        " consumed_by_submission = $3"
        " WHERE id = $1 AND uid = $2 AND status = 'pending'"
    >>,
    case elib_pg:query(Sql, [GrantId, Uid, SubmissionId]) of
        {ok, _} ->
            true;
        {error, Reason} ->
            ?LOG_ERROR("moya_subscribe_repo:consume_grant error ~p", [Reason]),
            false
    end.

%% @doc 下发上下文：submission → {learner_id, 学员名, 作业标题}。
%% 订阅消息 data 占位符（{learner_name}/{task_title}）的数据源。
%% 联表链 homework_submission → learner（学员名）与
%% homework_submission → group_task_assignment → group_task（作业标题，
%% 与 moya_context_repo:submission_scope 同款 a.task_id = gt.task_id 跳法）。
%% 任一跳缺失（INNER JOIN 无行）→ {ok, undefined}。
%% 行键：learner_id（integer）/ learner_name / task_title（binary）。
-spec notification_context(integer()) -> {ok, map() | undefined} | {error, term()}.
notification_context(SubmissionId) ->
    Sql = <<
        "SELECT hs.learner_id, l.display_name AS learner_name, "
        "gt.title AS task_title "
        "FROM ",
        (tb(<<"homework_submission">>))/binary,
        " hs "
        "JOIN ",
        (tb(<<"learner">>))/binary,
        " l ON l.id = hs.learner_id "
        "JOIN ",
        (tb(<<"group_task_assignment">>))/binary,
        " a ON a.id = hs.assignment_id "
        "JOIN ",
        (tb(<<"group_task">>))/binary,
        " gt ON gt.task_id = a.task_id "
        "WHERE hs.id = $1 LIMIT 1"
    >>,
    case elib_pg:query(Sql, [SubmissionId]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {ok, undefined};
        {error, Reason} ->
            ?LOG_ERROR("moya_subscribe_repo:notification_context error ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 可看回评的 active 监护人 uid 列表（下发对象）。
%% 与 moya_acl:resolve_guardian(view_review) 同口径：status='active' 且
%% can_view_review=true——没有回评查看权的监护人不应收到「已有点评」通知。
%% SELECT 列只有 guardian_uid（不含 relation 等细节，最小取数）。
-spec view_guardians(integer()) -> {ok, [integer()]} | {error, term()}.
view_guardians(LearnerId) ->
    Sql = <<
        "SELECT guardian_uid FROM ",
        (tb(<<"guardian_learner">>))/binary,
        " WHERE learner_id = $1 AND status = 'active'"
        " AND can_view_review = true"
    >>,
    case elib_pg:query(Sql, [LearnerId]) of
        {ok, Rows} ->
            {ok, [maps:get(<<"guardian_uid">>, R) || R <- Rows]};
        {error, Reason} ->
            ?LOG_ERROR("moya_subscribe_repo:view_guardians error ~p", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 其他业务表名包裹（本表走 tablename/0）。入参用 binary 字面量书写，
%% public_tablename 恒返回 binary，可直接进 SQL 拼接段
%% （atom 兜底分支参见 moya_learner_bind_repo:tb/1，本模块无 atom 调用方）。
-spec tb(binary()) -> binary().
tb(Tb) ->
    elib_pg_sql:public_tablename(Tb).
