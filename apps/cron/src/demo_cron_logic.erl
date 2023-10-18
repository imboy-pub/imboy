-module(demo_cron_logic).
%%%
% demo_cron 业务逻辑模块
% demo_cron business logic module
%%%

-export([add_every_4am_job/0]).
-export([inspect/1]).
-export([delete_job/1]).
-export([job_stats/1,
         all_job_stats/0,
         predict_datetime_by_spec/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").


%% ===================================================================
%% API
%% ===================================================================

%% Add job run at 04:00 everyday.
%% by ecron:add/3
% demo_cron_logic:add_every_4am_job()
add_every_4am_job() ->
    JobName = every_4am_job,
    MFA = {?MODULE, insepct, ["at 04:00 everyday."]},
    {ok, JobName} = ecron:add(JobName, "0 4 * * *", MFA).


%% MFA
inspect(Format) ->
    io:format(calendar:system_time_to_rfc3339(erlang:system_time(second)) ++ " : " ++ Format ++ "\n").


%% Delete a specific task
delete_job(JobName) ->
    ok = ecron:delete(JobName).


%% Inspect specific statistic
job_stats(JobName) ->
    ecron:statistic(JobName).


%% Inspect all statistic
all_job_stats() ->
    ecron:statistic().


%% Predict latest N datetime.
predict_datetime_by_spec(Spec, N) ->
    ecron:parse_spec(Spec, N).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
