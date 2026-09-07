-module(credential_retention_worker).
-behaviour(gen_server).
%%%===================================================================
%%% @doc 过期凭证行保留清理定时 worker（T-02，注册表类 sessions_tokens）。
%%%
%%% verification_code 表（id=手机号/邮箱，PII）过期行长期滞留违反保留
%%% 口径（retention-policy.yml sessions_tokens: event-driven→delete）。
%%% 本 worker 周期分批删除已过期验证码行。
%%%
%%% 设计对齐 olm_otk_cleanup_worker：
%%%   - 默认禁用（credential_retention_enabled=false），运维显式启用；
%%%   - bounded batches：单轮按 LIMIT 分批删，批间累计直到不足一批；
%%%   - dry-run：credential_retention_dry_run=true 时只统计不删除；
%%%   - 删除按 age 幂等，多节点并发重复扫描无副作用（行锁串行化）；
%%%   - 单轮失败只 WARN，不 crash、不影响下一轮；
%%%   - 日志只记行数，不落任何 PII。
%%%
%%% 配置（sys.config imboy app env）：
%%%   {credential_retention_enabled, true}                    %% 默认 false
%%%   {credential_retention_interval_ms, 86400000}            %% 默认每日
%%%   {credential_retention_verification_code_days, 1}        %% 过期宽限，默认 1 天
%%%   {credential_retention_batch_limit, 5000}                %% 单批上限
%%%   {credential_retention_dry_run, true}                    %% 默认 false
%%% @end
%%%===================================================================

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

-define(CHECK_INTERVAL_MS, 86400000).
-define(INITIAL_DELAY_MS, 60000).
-define(DEFAULT_RETENTION_DAYS, 1).
-define(DEFAULT_BATCH_LIMIT, 5000).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(?INITIAL_DELAY_MS, self(), run),
    {ok, #{}}.

handle_info(run, State) ->
    _ = run_cleanup(),
    erlang:send_after(interval_ms(), self(), run),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% @doc 启用才清理；dry-run 只统计。失败只 WARN（tick 已重排，不影响下一轮）。
-spec run_cleanup() -> ok.
run_cleanup() ->
    case enabled() of
        false ->
            ok;
        true ->
            Cutoff = cutoff_rfc3339(),
            case dry_run() of
                true ->
                    case verification_code_ds:count_expired(Cutoff) of
                        {ok, N} ->
                            ?INFO_LOG(
                                "[credential_retention] dry-run: ~p expired verification "
                                "code row(s) eligible (no rows deleted)",
                                [N]
                            ),
                            ok;
                        {error, Reason} ->
                            ok = ?WARN_LOG(
                                "[credential_retention] dry-run count failed: ~p", [Reason]
                            ),
                            ok
                    end;
                false ->
                    purge_in_batches(Cutoff, 0)
            end
    end.

%% bounded batches：单轮循环删到不足一批为止；单轮总量有上界（批数×limit），
%% 批内 LIMIT 防长事务/大锁。
purge_in_batches(Cutoff, Acc) ->
    Limit = batch_limit(),
    case verification_code_ds:purge_expired(Cutoff, Limit) of
        {ok, N} when N >= Limit ->
            ?INFO_LOG("[credential_retention] purged ~p row(s), continuing", [N]),
            purge_in_batches(Cutoff, Acc + N);
        {ok, N} ->
            ?INFO_LOG(
                "[credential_retention] purged ~p expired verification code row(s) "
                "(total ~p this round)",
                [N, Acc + N]
            ),
            ok;
        {error, Reason} ->
            ok = ?WARN_LOG(
                "[credential_retention] purge failed after ~p row(s): ~p", [Acc, Reason]
            ),
            ok
    end.

-spec cutoff_rfc3339() -> binary().
cutoff_rfc3339() ->
    CutoffMs = elib_dt:millisecond() - retention_days() * 86400000,
    elib_dt:to_rfc3339(CutoffMs).

-spec enabled() -> boolean().
enabled() ->
    application:get_env(imboy, credential_retention_enabled, false).

-spec dry_run() -> boolean().
dry_run() ->
    application:get_env(imboy, credential_retention_dry_run, false).

-spec interval_ms() -> pos_integer().
interval_ms() ->
    application:get_env(imboy, credential_retention_interval_ms, ?CHECK_INTERVAL_MS).

-spec retention_days() -> pos_integer().
retention_days() ->
    application:get_env(
        imboy, credential_retention_verification_code_days, ?DEFAULT_RETENTION_DAYS
    ).

-spec batch_limit() -> pos_integer().
batch_limit() ->
    application:get_env(imboy, credential_retention_batch_limit, ?DEFAULT_BATCH_LIMIT).
