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

%% 类型定义
-type job_name() :: atom().
-type cron_spec() :: string().
-type format_string() :: string().
-type datetime_list() :: [calendar:datetime()].
-type job_stats() :: map().

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
%% 添加一个在每天凌晨4点执行的定时任务
%%
%% 此函数创建一个名为 every_4am_job 的定时任务，该任务每天凌晨4点执行一次。
%% 任务执行时会调用 inspect/1 函数，输出当前时间和描述信息。
%%
%% @returns {ok, JobName} - 成功时返回 {ok, JobName}，其中 JobName 为任务名称
%%
%% 示例:
%% ```
%% > demo_cron_logic:add_every_4am_job().
%% {ok, every_4am_job}
%% ```
% demo_cron_logic:add_every_4am_job()
-spec add_every_4am_job() -> {ok, job_name()}.
add_every_4am_job() ->
    JobName = every_4am_job,
    MFA = {?MODULE, inspect, ["at 04:00 everyday."]},
    {ok, JobName} = ecron:add(JobName, "0 4 * * *", MFA).


%% MFA
%% 输出格式化的调试信息
%%
%% 此函数用于定时任务执行时输出调试信息，会打印当前时间（RFC3339格式）
%% 和传入的格式字符串。主要用于演示定时任务的执行情况。
%%
%% @param Format - 要输出的格式字符串
%% @returns ok - io:format/1 的返回值
%%
%% 示例:
%% ```
%% > demo_cron_logic:inspect("任务执行").
%% "2024-01-01T04:00:00Z : 任务执行\n"
%% ok
%% ```
-spec inspect(Format :: format_string()) -> ok.
inspect(Format) ->
    io:format(calendar:system_time_to_rfc3339(erlang:system_time(second)) ++ " : " ++ Format ++ "\n").


%% Delete a specific task
%% 删除指定的定时任务
%%
%% 此函数用于删除一个已经存在的定时任务。如果任务不存在，
%% ecron:delete/1 会返回错误，这里使用 = 进行模式匹配，
%% 确保删除操作成功完成。
%%
%% @param JobName - 要删除的任务名称
%% @returns ok - 成功删除时返回 ok
%%
%% 示例:
%% ```
%% > demo_cron_logic:delete_job(every_4am_job).
%% ok
%% ```
-spec delete_job(JobName :: job_name()) -> ok.
delete_job(JobName) ->
    ok = ecron:delete(JobName).


%% Inspect specific statistic
%% 检查指定任务的统计信息
%%
%% 此函数用于获取指定定时任务的详细统计信息，包括：
%% - 任务创建时间
%% - 任务上次执行时间
%% - 任务下次执行时间
%% - 任务执行次数
%% - 任务执行状态等
%%
%% @param JobName - 要查询的任务名称
%% @returns JobStats - 包含任务统计信息的映射(map)
%%
%% 示例:
%% ```
%% > demo_cron_logic:job_stats(every_4am_job).
%% #{job_name => every_4am_job,
%%   created_at => {{2024,1,1},{3,59,59}},
%%   last_run => {{2024,1,1},{4,0,0}},
%%   next_run => {{2024,1,2},{4,0,0}},
%%   run_count => 1}
%% '''
-spec job_stats(JobName :: job_name()) -> job_stats().
job_stats(JobName) ->
    ecron:statistic(JobName).


%% Inspect all statistic
%% 检查所有任务的统计信息
%%
%% 此函数用于获取系统中所有定时任务的统计信息列表，
%% 返回一个包含所有任务详情的列表，每个任务的信息
%% 都是一个包含详细统计数据的映射。
%%
%% @returns [JobStats] - 所有任务统计信息的列表
%%
%% 示例:
%% ```
%% > demo_cron_logic:all_job_stats().
%% [#{job_name => every_4am_job,
%%    created_at => {{2024,1,1},{3,59,59}},
%%    last_run => {{2024,1,1},{4,0,0}},
%%    next_run => {{2024,1,2},{4,0,0}},
%%    run_count => 1},
%%  #{job_name => hourly_job,
%%    created_at => {{2024,1,1},{0,0,0}},
%%    last_run => {{2024,1,1},{3,0,0}},
%%    next_run => {{2024,1,1},{4,0,0}},
%%    run_count => 3}]
%% '''
-spec all_job_stats() -> [job_stats()].
all_job_stats() ->
    ecron:statistic().


%% Predict latest N datetime.
%% 根据 cron 规范预测未来 N 次执行时间
%%
%% 此函数根据给定的 cron 表达式规范，预测该任务在未来
%% N 次执行的具体时间点。返回的时间为标准的 Erlang datetime 格式。
%%
%% @param Spec - cron 表达式字符串，如 "0 4 * * *" 表示每天凌晨4点
%% @param N - 要预测的执行次数，必须为正整数
%% @returns DatetimeList - 包含 N 个预测执行时间的列表
%%
%% 示例:
%% ```
%% > demo_cron_logic:predict_datetime_by_spec("0 4 * * *", 3).
%% [{{2024,1,1},{4,0,0}}, {{2024,1,2},{4,0,0}}, {{2024,1,3},{4,0,0}}]
%% '''
-spec predict_datetime_by_spec(Spec :: cron_spec(), N :: non_neg_integer()) -> datetime_list().
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
