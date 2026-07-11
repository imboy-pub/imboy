-module(billing_invoice_worker).
-behaviour(gen_server).
%%%===================================================================
%%% @doc SaaS 账单生成定时 worker（金钱相邻）。
%%%
%%% 每日扫描一次到期订阅，对每个到期订阅调 billing_logic:generate_invoice/1
%%% （依赖 (subscription_id, period_start, period_end) 唯一约束幂等）。
%%%
%%% ⚠️ 只「生成账单」，不自动扣款、不自动续费：扣款/续费是外向不可逆动作，
%%%   须走独立的、有人工/凭证介入的支付流程。
%%%
%%% 默认禁用（billing_invoice_enabled=false）：上线需运维在 sys.config 显式启用：
%%%   {billing_invoice_enabled, true}          %% 默认 false
%%%   {billing_invoice_interval_ms, 86400000}  %% 默认每日
%%% @end
%%%===================================================================

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

%% 每日扫描；启动后延迟首扫，等 DB/迁移就绪与时钟稳定
-define(CHECK_INTERVAL_MS, 86400000).
-define(INITIAL_DELAY_MS, 60000).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(?INITIAL_DELAY_MS, self(), run),
    {ok, #{}}.

handle_info(run, State) ->
    _ = run_billing(),
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

%% @doc 启用才扫描；对每个到期订阅幂等生成账单。单个失败只 WARN，不中断整批。
-spec run_billing() -> ok.
run_billing() ->
    case enabled() of
        false ->
            ok;
        true ->
            Now = erlang:system_time(millisecond),
            Subs = billing_subscription_ds:list_expiring(Now),
            lists:foreach(fun gen_invoice/1, Subs),
            ?INFO_LOG("[billing_invoice] scanned ~p expiring subscription(s)", [length(Subs)]),
            ok
    end.

-spec gen_invoice(map()) -> ok.
gen_invoice(Sub) ->
    case maps:get(<<"id">>, Sub, undefined) of
        SubId when is_integer(SubId) ->
            case billing_logic:generate_invoice(SubId) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    ok = ?WARN_LOG(
                        "[billing_invoice] generate failed sub=~p reason=~p", [SubId, Reason]
                    ),
                    ok
            end;
        _ ->
            ok
    end.

-spec enabled() -> boolean().
enabled() ->
    application:get_env(imboy, billing_invoice_enabled, false).

-spec interval_ms() -> pos_integer().
interval_ms() ->
    application:get_env(imboy, billing_invoice_interval_ms, ?CHECK_INTERVAL_MS).
