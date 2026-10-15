%%% @doc V5 普通队列删除 worker（W4-04）：gen_server 壳 + 生命周期调和。
%%%
%%% 协议（迁移 179 fn + 02 §6）：claim（SKIP LOCKED/lease 120s/attempts<8）
%%% → driver 删字节 → ack(ok/failed)。调和（02 §6）：
%%%   * 启动调和：reaper 回收过期租约（零删除，禁全桶扫）；
%%%   * 周期 tick：有限批（≤100）+ reaper；
%%%   * blocked 终态不轮询（fn 候选天然排除——无第 9 次）；
%%%   * DB 故障有界唤醒：tick 间隔指数退避（上限 10×），不无限快轮询；
%%%   * 重启不重置 attempts（持久于行）。
%%%
%%% 默认禁用（{attachment_delete_worker_enabled, false}）；启用后由
%%% supervisor 挂接。删除按 registry driver 派发（attachment_storage，
%%% W4-03 边界）。
-module(attachment_delete_worker).

-behaviour(gen_server).

-export([start_link/0, run_batch/1, run_batch/2, reconcile/0, reconcile/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("log.hrl").

-define(BATCH_LIMIT, 100).
-define(TICK_MS, 10000).
-define(MAX_TICK_MS, 100000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Opts) ->
    case application:get_env(imboy, attachment_delete_worker_enabled, false) of
        true ->
            ok = reconcile(),
            erlang:send_after(?TICK_MS, self(), tick),
            {ok, #{enabled => true, fails => 0}};
        false ->
            erlang:send_after(?TICK_MS, self(), tick),
            {ok, #{enabled => false, fails => 0}}
    end.

handle_info(tick, State = #{enabled := true}) ->
    case run_batch(fun default_delete/4) of
        {ok, _} ->
            erlang:send_after(?TICK_MS, self(), tick),
            {noreply, State#{fails := 0}};
        {error, _} ->
            %% DB 故障有界唤醒：退避翻倍（上限 MAX_TICK_MS），不无限快轮询
            Fails = maps:get(fails, State, 0) + 1,
            Delay = min(?TICK_MS * (1 bsl min(Fails, 4)), ?MAX_TICK_MS),
            erlang:send_after(Delay, self(), tick),
            {noreply, State#{fails := Fails}}
    end;
handle_info(tick, State) ->
    erlang:send_after(?TICK_MS, self(), tick),
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 批处理：池连接上循环 claim→删除→ack（≤BATCH_LIMIT）；claim 错误即止
%% （fn 已退避；DB 故障由 tick 退避承载）。
run_batch(DeleteFun) when is_function(DeleteFun, 4) ->
    elib_pg:with_conn(
        fun(Conn) ->
            batch_loop(Conn, DeleteFun, ?BATCH_LIMIT, 0)
        end,
        30000
    ).

%% 测试/调用方自带连接（marker 库）时使用。
run_batch(Conn, DeleteFun) when is_function(DeleteFun, 4) ->
    batch_loop(Conn, DeleteFun, ?BATCH_LIMIT, 0).

batch_loop(_Conn, _Del, 0, Done) ->
    {ok, Done};
batch_loop(Conn, Del, N, Done) ->
    Token = new_token(),
    case attachment_gc_repo:v5_claim(Conn, Token) of
        {ok, Row} ->
            case delete_job(Conn, Del, Row, Token) of
                ok ->
                    batch_loop(Conn, Del, N - 1, Done + 1);
                {error, _} ->
                    {ok, Done}
            end;
        {error, empty} ->
            {ok, Done};
        {error, R} ->
            ?ERROR_LOG(["attachment_delete_worker claim failed: ", R]),
            {error, R}
    end.

delete_job(Conn, Del, Row, Token) ->
    Path = maps:get(<<"out_path">>, Row),
    Bucket = maps:get(<<"out_bucket">>, Row, <<"b">>),
    Ns = maps:get(<<"out_storage_namespace">>, Row, <<>>),
    Container = maps:get(<<"out_storage_container">>, Row, <<>>),
    case Del(Ns, Container, Path, Bucket) of
        ok ->
            attachment_gc_repo:v5_ack(Conn, Path, Token, true);
        {error, not_found} ->
            %% 精确位置 not_found 视为成功（02 §6：at-least-once）
            attachment_gc_repo:v5_ack(Conn, Path, Token, true);
        {error, R} ->
            ?ERROR_LOG(["attachment_delete_worker delete failed: ", Path, R]),
            attachment_gc_repo:v5_ack(Conn, Path, Token, false)
    end.

%% 生产删除副作用：按 registry driver 派发（W4-03 storage boundary）。
default_delete(Ns, Container, Key, _Bucket) ->
    case attachment_storage:identity(Ns, Container, Key) of
        {ok, #{driver := Driver, namespace := CanonNs, container := CanonC}} ->
            Mod = attachment_storage:driver_module(
                atom_to_binary(Driver, utf8)
            ),
            Mod:delete(CanonNs, CanonC, Key);
        {error, unknown} ->
            {error, unknown_namespace};
        {error, R} ->
            {error, R}
    end.

%% 启动/周期调和：reaper 过期租约（零删除——02 §6 禁全桶扫陌生对象）。
reconcile() ->
    elib_pg:with_conn(fun(Conn) -> reconcile(Conn) end, 30000).

reconcile(Conn) ->
    attachment_gc_repo:v5_reap(Conn, 100).

new_token() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.
