-module(report_action_log_ds).
%%%
% report_action_log_ds — G3 架构治理：report_logic 不应直调 report_action_log_repo
% G3: thin DS wrapper for report action logs
%%%

-include("log.hrl").

%% ==================== API ====================
-export([create/4]).

-spec create(integer(), integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
create(ReportId, AdmUid, Result, Note) ->
    report_action_log_repo:create(ReportId, AdmUid, Result, Note).
