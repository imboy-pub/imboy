-module(qr_login_cleanup_worker).
-moduledoc "QR 登录过期会话行清理定时 worker。".
-behaviour(gen_server).
%%%===================================================================
%%% @doc QR 登录过期会话行清理定时 worker（LEE-5）。
%%%
%%% cache 权威删除后 public.qr_login_session 是 QR 会话唯一存储，过期行
%%% 须周期清扫防表膨胀（设计对齐 olm_otk_cleanup_worker）。
%%%
%%% 并发安全（cleanup vs claim/transition，工作集互斥）：
%%%   - claim/状态机全部语句内联 expires_at > now() 谓词——**过期安全从不
%%%     依赖清扫**，清扫缺席或延迟最多造成行残留，绝不造成过期凭据交付；
%%%   - cleanup 只 DELETE expires_at <= now() 的行（PG 侧时钟裁决），与
%%%     claim 的谓词按 (expires_at vs now()) 严格分区；即便理论同行竞争，
%%%     PG 行锁串行化：claim 先行则行已 consumed、清扫按过期照删（凭据已
%%%     NULL），cleanup 先行则 claim 0 行 → not_claimable（fail closed）。
%%%   - 单批限量（默认 500，LIMIT 子查询），失败仅 WARN 不 crash、不影响
%%%     下一轮（handle_info 重排），worker 崩溃不影响 QR 登录（独立进程，
%%%     不在 readiness 必需清单）。
%%%
%%% 与 olm_otk_cleanup_worker 不同：默认启用（QR 行为 60s 短生命周期数据，
%%% 无审计保留价值，不清扫则必然膨胀）；可用 sys.config 覆盖：
%%%   {qr_login_cleanup_enabled, true}          %% 默认 true
%%%   {qr_login_cleanup_interval_ms, 300000}    %% 默认每 5 分钟
%%%   {qr_login_cleanup_batch, 500}             %% 默认单批 500 行
%%% @end
%%%===================================================================

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

%% 默认每 5 分钟一批；启动后延迟首扫，等 DB/迁移就绪与时钟稳定
-define(CHECK_INTERVAL_MS, 300000).
-define(INITIAL_DELAY_MS, 60000).
-define(DEFAULT_BATCH, 500).

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

%% @doc 单次失败只 WARN，不 crash、不影响下一轮（handle_info 已重排）。
%% 分层：Worker -> qr_login_session_ds -> qr_login_session_repo。
-spec run_cleanup() -> ok.
run_cleanup() ->
    case enabled() of
        false ->
            ok;
        true ->
            case qr_login_session_ds:delete_expired(batch()) of
                {ok, N} when N > 0 ->
                    ?INFO_LOG("[lee-5] qr_login cleanup deleted ~p expired row(s)", [N]),
                    ok;
                {ok, 0} ->
                    ok;
                {error, Reason} ->
                    ok = ?WARN_LOG("[lee-5] qr_login cleanup failed: ~p", [Reason]),
                    ok
            end
    end.

-spec enabled() -> boolean().
enabled() ->
    application:get_env(imboy, qr_login_cleanup_enabled, true).

-spec interval_ms() -> pos_integer().
interval_ms() ->
    application:get_env(imboy, qr_login_cleanup_interval_ms, ?CHECK_INTERVAL_MS).

-spec batch() -> pos_integer().
batch() ->
    application:get_env(imboy, qr_login_cleanup_batch, ?DEFAULT_BATCH).
