-module(olm_otk_cleanup_worker).
-behaviour(gen_server).
%%%===================================================================
%%% @doc Olm one-time key 已消费审计行清理定时 worker。
%%%
%%% claim OTK 语义为「UPDATE status='claimed' 不删」（ADR 03 §8.3），保留消费痕迹
%%% 供审计；本 worker 周期删除超保留期的 claimed 行，防表膨胀。
%%%
%%% 分层：Worker -> olm_identity_logic -> olm_identity_ds -> olm_identity_repo。
%%% 配置单位 days，Logic 层换算 seconds；Repo 收 seconds。
%%%
%%% 并发安全（cleanup vs claim_one_time_key，工作集互斥）：
%%%   - claim 只 SELECT/UPDATE status='available' 的行（选中后转为 'claimed'）；
%%%   - cleanup 只 DELETE status='claimed' 且 consumed_at 超保留期的行；
%%%   - 刚 claim 的行 consumed_at=now() 不满足 `< now()-retention`（retention>0），
%%%     故两者按 (status, consumed_at 年龄) 严格分区，永不触碰同一行；
%%%   - 即便理论同行竞争，PG 行锁串行化，且 claim 用 FOR UPDATE SKIP LOCKED 不阻塞。
%%%   详见 ADR 03 §8.3。
%%%
%%% 默认禁用（olm_otk_cleanup_enabled=false）：上线需运维在 sys.config 显式启用：
%%%   {olm_otk_cleanup_enabled, true}          %% 默认 false，禁用时不删任何审计行
%%%   {olm_otk_cleanup_interval_ms, 86400000}  %% 默认每日
%%%   {olm_otk_retention_days, 7}              %% 默认保留 7 天
%%% @end
%%%===================================================================

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

%% 每日扫描；启动后延迟首扫，等 DB/迁移就绪与时钟稳定
-define(CHECK_INTERVAL_MS, 86400000).
-define(INITIAL_DELAY_MS, 60000).
-define(DEFAULT_RETENTION_DAYS, 7).

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

%% @doc 启用才清理。单次失败只 WARN，不 crash、不影响下一轮（handle_info 已重排）。
%%  TODO(metrics): 预留埋点 —— (1) 删除行数 (2) 单次耗时(ms) (3) 累计失败次数
%%                 (4) 末次成功运行时间戳。本 Slice 不实现，不接线 elib_metric。
-spec run_cleanup() -> ok.
run_cleanup() ->
    case enabled() of
        false ->
            ok;
        true ->
            case olm_identity_logic:cleanup_consumed_one_time_keys(retention_days()) of
                {ok, N} ->
                    ?INFO_LOG("[olm_otk_cleanup] deleted ~p consumed OTK audit row(s)", [N]),
                    ok;
                {error, Reason} ->
                    ok = ?WARN_LOG("[olm_otk_cleanup] cleanup failed: ~p", [Reason]),
                    ok
            end
    end.

-spec enabled() -> boolean().
enabled() ->
    application:get_env(imboy, olm_otk_cleanup_enabled, false).

-spec interval_ms() -> pos_integer().
interval_ms() ->
    application:get_env(imboy, olm_otk_cleanup_interval_ms, ?CHECK_INTERVAL_MS).

-spec retention_days() -> pos_integer().
retention_days() ->
    application:get_env(imboy, olm_otk_retention_days, ?DEFAULT_RETENTION_DAYS).
