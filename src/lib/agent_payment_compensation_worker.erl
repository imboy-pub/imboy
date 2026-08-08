-module(agent_payment_compensation_worker).
-behaviour(gen_server).

%% Agent 支付预留补偿 worker。
%% 只处理持久化 outbox，不在进程内保存任何恢复所需状态。

-export([start_link/0, process_once/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

-define(INITIAL_DELAY_MS, 5000).
-define(INTERVAL_MS, 10000).
-define(BATCH_SIZE, 20).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, #{}}.
init([]) ->
    erlang:send_after(?INITIAL_DELAY_MS, self(), run),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(run, State) ->
    _ = process_once(),
    erlang:send_after(?INTERVAL_MS, self(), run),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec process_once() -> ok.
process_once() ->
    case agent_payment_compensation_ds:claim_pending(?BATCH_SIZE) of
        {ok, Rows} ->
            lists:foreach(fun process_row/1, Rows),
            ok;
        {error, Reason} ->
            ok = ?WARN_LOG({agent_payment_compensation_claim_failed, Reason}),
            ok
    end.

-spec process_row(map()) -> ok.
process_row(#{<<"id">> := CompensationId, <<"attempts">> := Attempts}) ->
    case agent_payment_compensation_ds:release(CompensationId) of
        ok ->
            ok;
        {error, Reason} ->
            DelaySecs = min(300, 1 bsl min(Attempts, 8)),
            ok = ?WARN_LOG({agent_payment_compensation_release_failed, CompensationId, Reason}),
            _ = agent_payment_compensation_ds:mark_retry(CompensationId, DelaySecs, Reason),
            ok
    end;
process_row(_) ->
    ok.
