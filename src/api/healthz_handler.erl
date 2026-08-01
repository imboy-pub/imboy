-module(healthz_handler).
%%%===================================================================
%%% @doc 健康检查端点 / Liveness + readiness probe
%%%
%%% C-49：`/healthz` 此前**只在 throttle_middleware 的白名单里出现过**，
%%% 却从来没有对应路由 —— 实际请求返回 404。于是：
%%%   - compose/helm 的 healthcheck 配了也永远不健康
%%%   - deploy.sh 的就绪判断只能退化成"端口通不通"，而端口通 ≠ 服务可用
%%%
%%% 语义（刻意区分，不合并成一个"活着"）：
%%%   200 = 依赖就绪，可以接流量
%%%   503 = 进程活着但**依赖不可用**（当前只看 PG），不应被灌流量
%%% 判据要求的正是"PG 挂掉返 503" —— 若 PG 挂了仍返 200，蓝绿切流会把流量
%%% 切到一个连不上库的节点上，比不切更糟。
%%%
%%% 这个端点在**每次探活**时被打，因此：
%%%   - 不查业务表，只做一次最轻的连通性探测
%%%   - 结果短时缓存，避免探活频率变成对 PG 的压力
%%%   - 任何异常都收敛成 503，绝不 500（探针拿到 500 与拿到 503 的处置不同）
%%% @end
%%%===================================================================

-export([init/2]).

-ifdef(TEST).
-export([probe_db/0, cache_ttl_ms/0]).
-endif.

%% 探测结果缓存时长：探活通常 5~10s 一次，2s 缓存足以削掉重复探测，
%% 又不会让"PG 刚挂"这件事被掩盖太久。
-define(CACHE_TTL_MS, 2000).
-define(CACHE_KEY, {healthz, db}).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State) ->
    {Code, Body} =
        case cached_db_ok() of
            true -> {200, <<"{\"status\":\"ok\",\"db\":\"up\"}">>};
            false -> {503, <<"{\"status\":\"degraded\",\"db\":\"down\"}">>}
        end,
    Req = cowboy_req:reply(
        Code,
        #{
            <<"content-type">> => <<"application/json; charset=utf-8">>,
            %% 探针结果绝不能被任何中间层缓存，否则挂了还一直返 200
            <<"cache-control">> => <<"no-store">>
        },
        Body,
        Req0
    ),
    {ok, Req, State}.

%% @doc 带短时缓存的 DB 探测。缓存放 persistent_term 而不是 ETS/进程：
%% 无需额外进程、读是无锁的，写频率被 TTL 限制在每 2 秒一次。
-spec cached_db_ok() -> boolean().
cached_db_ok() ->
    Now = erlang:monotonic_time(millisecond),
    case persistent_term:get(?CACHE_KEY, undefined) of
        {Ok, At} when is_integer(At), Now - At < ?CACHE_TTL_MS ->
            Ok;
        _ ->
            Ok = probe_db(),
            persistent_term:put(?CACHE_KEY, {Ok, Now}),
            Ok
    end.

%% @doc 最轻的连通性探测：`SELECT 1`。
%% 不查任何业务表 —— 业务表为空是合法状态，不该被判成不健康。
-spec probe_db() -> boolean().
probe_db() ->
    try elib_pg:query(<<"SELECT 1">>, []) of
        {ok, [_ | _]} -> true;
        {ok, _} -> true;
        _ -> false
    catch
        %% 连接池耗尽 / 无连接时 elib_pg 是**抛异常**而不是返回 {error,_}
        %% （attach presign 那次已经踩过同一个坑），这里必须 catch，
        %% 否则 PG 挂掉时探针返回 500 而不是判据要求的 503。
        _:_ -> false
    end.

-spec cache_ttl_ms() -> pos_integer().
cache_ttl_ms() -> ?CACHE_TTL_MS.
