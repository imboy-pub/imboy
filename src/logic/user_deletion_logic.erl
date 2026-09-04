-module(user_deletion_logic).
%%%===================================================================
%%% @doc 账号注销删除编排器（Implementation Plan Task D-03）
%%%
%%% 周期（或手动）驱动三阶段：
%%%   1. ensure_jobs：为"宽限期已满且无任务"的注销请求补建 pending 任务
%%%   2. claim：FOR UPDATE SKIP LOCKED 原子认领（多 worker 安全）
%%%   3. execute：钱包余额门 → 所有权转移 → 主删除事务 → Garage 入队
%%%
%%% 重试：失败回 pending（attempts < max），达上限转 failed 终态。
%%% 幂等：DB 步骤对已删行 no-op；Garage 删除失败可重试（引用已清）。
%%%
%%% 配置 (sys.config):
%%% - {user_deletion_enabled, true}              % 启用自动删除（默认 false）
%%% - {user_deletion_interval, 86400000}          % 扫描间隔，默认 24 小时
%%% - {user_deletion_retention_days, 60}          % 宽限期，默认 60 天
%%% - {user_deletion_batch_size, 10}              % 每轮处理上限
%%% - {user_deletion_max_attempts, 5}             % 重试上限（默认 5）
%%%===================================================================

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/0]).
-export([cleanup_now/0]).
-export([get_status/0]).
-export([export_user_data/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% 24 小时
-define(DEFAULT_INTERVAL, 86400000).
-define(DEFAULT_RETENTION_DAYS, 60).
-define(DEFAULT_BATCH_SIZE, 10).

-record(state, {
    interval :: non_neg_integer(),
    retention_days :: pos_integer(),
    batch_size :: non_neg_integer(),
    last_cleanup :: erlang:timestamp() | undefined,
    total_deleted :: non_neg_integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 手动触发清理
-spec cleanup_now() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_now() ->
    gen_server:call(?MODULE, cleanup_now, 60000).

%% @doc 获取清理状态
-spec get_status() -> map().
get_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc 导出用户数据（用于注销前的数据快照）
%% @param Uid 用户ID
%% @return {ok, ExportData} | {error, Reason}
-spec export_user_data(integer()) -> {ok, map()} | {error, term()}.
export_user_data(Uid) ->
    user_ds:export_data(Uid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Enabled = application:get_env(imboy, user_deletion_enabled, false),
    case Enabled of
        true ->
            Interval = application:get_env(imboy, user_deletion_interval, ?DEFAULT_INTERVAL),
            RetDays = application:get_env(
                imboy, user_deletion_retention_days, ?DEFAULT_RETENTION_DAYS
            ),
            BatchSize = application:get_env(imboy, user_deletion_batch_size, ?DEFAULT_BATCH_SIZE),
            State = #state{
                interval = Interval,
                retention_days = RetDays,
                batch_size = BatchSize,
                last_cleanup = undefined,
                total_deleted = 0
            },
            %% 首次延迟 60 秒执行
            erlang:send_after(60000, self(), do_cleanup),
            ok = ?INFO_LOG([
                user_deletion_logic,
                started,
                #{
                    interval => Interval,
                    retention_days => RetDays,
                    batch_size => BatchSize
                }
            ]),
            {ok, State};
        false ->
            ok = ?INFO_LOG([user_deletion_logic, disabled]),
            {ok, #state{
                interval = 0,
                retention_days = ?DEFAULT_RETENTION_DAYS,
                batch_size = 0,
                last_cleanup = undefined,
                total_deleted = 0
            }}
    end.

handle_call(cleanup_now, _From, State) ->
    {Result, NewState} = do_cleanup_internal(State),
    {reply, Result, NewState};
handle_call(get_status, _From, State) ->
    Status = #{
        interval => State#state.interval,
        retention_days => State#state.retention_days,
        batch_size => State#state.batch_size,
        last_cleanup => State#state.last_cleanup,
        total_deleted => State#state.total_deleted
    },
    {reply, Status, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(do_cleanup, #state{interval = 0} = State) ->
    {noreply, State};
handle_info(do_cleanup, State) ->
    {_Result, NewState} = do_cleanup_internal(State),
    erlang:send_after(State#state.interval, self(), do_cleanup),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

do_cleanup_internal(State) ->
    try
        %% 每轮现读配置而非 init 冻结 State：supervisor 常驻的 disabled 实例
        %% （batch_size=0）在运行期改配置后无需重启即可生效
        RetDays = application:get_env(imboy, user_deletion_retention_days, 60),
        BatchSize = application:get_env(imboy, user_deletion_batch_size, 10),
        {ok, Deleted} = run_cycle(RetDays, BatchSize),
        case Deleted > 0 of
            true ->
                ok = ?INFO_LOG([
                    user_deletion_cleanup, #{deleted => Deleted, retention_days => RetDays}
                ]);
            false ->
                ok
        end,
        NewState = State#state{
            last_cleanup = erlang:timestamp(),
            total_deleted = State#state.total_deleted + Deleted
        },
        {{ok, Deleted}, NewState}
    catch
        _:Error ->
            ok = ?ERROR_LOG([user_deletion_cleanup_error, Error]),
            {{error, Error}, State}
    end.

%% @doc 一轮编排：补建任务 → 认领 → 执行，返回完成数
-spec run_cycle(pos_integer(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
run_cycle(RetentionDays, BatchSize) ->
    io:format(user, "~nDBG_CYCLE ret=~p batch=~p~n", [RetentionDays, BatchSize]),
    ok = ensure_jobs(RetentionDays, BatchSize),
    ClaimedBy = unicode:characters_to_binary(
        "node|" ++ atom_to_list(node())
    ),
    claim_loop(RetentionDays, ClaimedBy, BatchSize, 0).

%% 阶段 1：为过期请求补建任务（幂等，ON CONFLICT DO NOTHING）
ensure_jobs(RetentionDays, BatchSize) ->
    case user_ds:find_expired_logout_users(RetentionDays, BatchSize) of
        {ok, Rows} ->
            io:format(user, "~nDBG_ENSURE rows=~p~n", [Rows]),
            lists:foreach(
                fun(#{<<"id">> := Uid}) ->
                    ensure_job(Uid)
                end,
                Rows
            ),
            ok;
        {error, Reason} ->
            ?ERROR_LOG([user_deletion_ensure_jobs_failed, Reason]),
            ok
    end.

ensure_job(Uid) ->
    elib_pg:with_tx(fun(Conn) ->
        Account =
            case
                elib_pg:query(
                    Conn,
                    <<"SELECT account FROM public.\"user\" WHERE id = $1">>,
                    [Uid]
                )
            of
                {ok, [#{<<"account">> := A}]} -> A;
                _ -> <<>>
            end,
        {ok, _} = user_deletion_job_repo:ensure_job_tx(Conn, Uid, Account),
        ok
    end).

%% 阶段 2+3：认领并执行，直到无可认领或达批量上限
claim_loop(_RetentionDays, _ClaimedBy, 0, Done) ->
    {ok, Done};
claim_loop(RetentionDays, ClaimedBy, Budget, Done) ->
    case user_deletion_job_repo:claim_pending_expired(RetentionDays, ClaimedBy) of
        {ok, none} ->
            {ok, Done};
        {ok, Job} ->
            case execute_job(Job) of
                ok ->
                    claim_loop(RetentionDays, ClaimedBy, Budget - 1, Done + 1);
                {error, Reason} ->
                    ?ERROR_LOG([user_deletion_execute_failed, Reason]),
                    {ok, Done}
            end;
        {error, Reason} ->
            ?ERROR_LOG([user_deletion_claim_failed, Reason]),
            {ok, Done}
    end.

%% 单任务执行：钱包门 → 转移 + 主事务 → Garage 入队 → 终态
execute_job(Job) ->
    #{<<"id">> := JobId, <<"user_id">> := Uid} = Job,
    MaxAttempts = application:get_env(imboy, user_deletion_max_attempts, 5),
    case balance_gate(Uid) of
        ok ->
            case execute_deletion(Uid) of
                {ok, Keys} ->
                    enqueue_garage_deletes(Keys),
                    _ = user_deletion_job_repo:mark_completed(JobId),
                    ok;
                {error, Reason} ->
                    _ = user_deletion_job_repo:mark_failed(
                        JobId,
                        elib_cnv:safe_to_binary(Reason),
                        MaxAttempts
                    ),
                    ok
            end;
        {error, Reason} ->
            %% 余额门未过：转 pending 重试，达上限转 failed 终态（运营介入）
            _ = user_deletion_job_repo:mark_failed(
                JobId,
                elib_cnv:safe_to_binary(Reason),
                MaxAttempts
            ),
            ok
    end.

%% 钱包余额门（D-02：余额必须为 0）
balance_gate(Uid) ->
    case elib_pg:query(<<"SELECT balance FROM public.wallet WHERE user_id = $1">>, [Uid]) of
        {ok, []} ->
            ok;
        {ok, [#{<<"balance">> := 0}]} ->
            ok;
        {ok, [#{<<"balance">> := Balance}]} when is_integer(Balance), Balance =/= 0 ->
            {error, {balance_not_zero, Balance}};
        {ok, [#{<<"balance">> := Balance}]} when is_binary(Balance), Balance =/= <<"0">> ->
            {error, {balance_not_zero, Balance}};
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% 执行删除：附件收集 → 转移 + 主事务（同一事务），返回待删对象键
execute_deletion(Uid) ->
    elib_pg:with_tx(fun(Conn) ->
        {ok, Keys} = user_deletion_executor:collect_attachment_keys_tx(Conn, Uid),
        ok = user_deletion_executor:transfer_ownerships_tx(Conn, Uid),
        ok = user_deletion_executor:execute_main_tx(Conn, Uid),
        {ok, Keys}
    end).

%% Garage S3 对象删除（外部资源，事务后 best-effort；
%% 失败仅记日志——DB 引用已清，重跑由对象键幂等兜底）
enqueue_garage_deletes(Keys) ->
    lists:foreach(
        fun(Key) ->
            try
                ok = elib_oss:delete_object(Key)
            catch
                _:Err ->
                    ?ERROR_LOG([user_deletion_garage_delete_failed, Key, Err])
            end
        end,
        Keys
    ).
