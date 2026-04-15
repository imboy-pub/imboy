-module(report_ticket_ds).
%%%
% report_ticket_ds — G3 架构治理：report_logic 不应直调 report_ticket_repo
% G3: thin DS wrapper for report tickets
%%%

-include("log.hrl").

%% ==================== API ====================
-export([create/5]).
-export([page_admin/3]).
-export([find_by_id/1]).
-export([resolve/4]).

-spec create(binary(), integer(), integer(), binary(), binary()) -> {ok, integer()} | {error, any()}.
create(Type, TargetId, ReporterUid, Reason, Desc) ->
    report_ticket_repo:create(Type, TargetId, ReporterUid, Reason, Desc).

-spec page_admin(map(), integer(), integer()) -> {ok, map()} | {error, any()}.
page_admin(RepoFilter, Page, Size) ->
    report_ticket_repo:page_admin(RepoFilter, Page, Size).

-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(ReportId) ->
    report_ticket_repo:find_by_id(ReportId).

-spec resolve(integer(), integer(), binary(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
resolve(ReportId, Result, Note, AdmUid) ->
    report_ticket_repo:resolve(ReportId, Result, Note, AdmUid).
