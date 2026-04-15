-module(report_ds).
%%%
%% report_ds — 举报数据服务层 / Report data service layer
%%
%% 职责：封装 report_ticket_repo / report_action_log_repo 的访问，
%%       让 report_logic 回归纯业务编排，遵循 Handler→Logic→DS→Repo 单向架构。
%% G3 治理：report_logic 不再直调 repo / G3 remediation.
%%%

-export([create_ticket/5, find_ticket_by_id/1, page_admin_tickets/3, resolve_ticket/4]).
-export([create_action_log/4]).

%% @doc 创建举报单 / Create a report ticket.
-spec create_ticket(integer(), integer(), integer(), binary(), binary()) ->
        {ok, integer()} | {error, term()}.
create_ticket(TargetType, TargetId, ReporterUid, Reason, Desc) ->
    report_ticket_repo:create(TargetType, TargetId, ReporterUid, Reason, Desc).

%% @doc 按 ID 查询举报单 / Find ticket by id.
-spec find_ticket_by_id(integer()) -> map() | undefined.
find_ticket_by_id(ReportId) ->
    report_ticket_repo:find_by_id(ReportId).

%% @doc 管理员分页查询 / Admin paged list.
-spec page_admin_tickets(map(), pos_integer(), pos_integer()) ->
        {ok, map()} | {error, term()}.
page_admin_tickets(Filter, Page, Size) ->
    report_ticket_repo:page_admin(Filter, Page, Size).

%% @doc 处置举报单 / Resolve ticket.
-spec resolve_ticket(integer(), binary(), binary(), integer()) ->
        {ok, integer()} | {error, term()}.
resolve_ticket(ReportId, Result, Note, AdmUid) ->
    report_ticket_repo:resolve(ReportId, Result, Note, AdmUid).

%% @doc 记录处置动作日志 / Record action log.
-spec create_action_log(integer(), integer(), binary(), binary()) ->
        {ok, integer()} | {error, term()}.
create_action_log(ReportId, OperatorUid, Result, Note) ->
    report_action_log_repo:create(ReportId, OperatorUid, Result, Note).
