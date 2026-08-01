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
%%%   - 不查业务表，只做一次最轻的连通性探测（SELECT 1）
%%%   - 任何异常都收敛成 503，绝不 500（探针拿到 500 与拿到 503 的处置不同）
%%%
%%% ⚠️ 这里**刻意不做缓存**。初版用 persistent_term 缓存 2s，是错的：
%%%   persistent_term:put/2 每次写会触发**全局 GC**（扫描所有进程寻找旧值引用），
%%%   代价与进程数成正比 —— 本节点每个 WS 连接一个进程，量级上千。
%%%   而探活恰好在**部署期**最密集（deploy.sh 每 2s 探一次），等于在系统最吃紧
%%%   的时刻反复触发全局 GC。
%%%   缓存本来要省的那点开销：SELECT 1 对 PG 是白菜价，探活间隔 30s、
%%%   部署轮询总共也就 20 次。**为省一个不存在的开销引入一个真实的抖动源。**
%%% @end
%%%===================================================================

-export([init/2]).
%% probe_db/0 也是**容器 healthcheck 的入口**（C-50）：
%% 运行镜像是 debian-slim，没有 curl/wget，探不了 HTTP；
%% 但 release 自带 `bin/imboy eval`，直接调这个函数即可，无需额外工具。
-export([probe_db/0]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State) ->
    %% ⚠️ 版本号**只对内网可见**。nginx 对 /metrics 返 403，但**没有拦 /healthz**，
    %% 所以本端点是公网可达且匿名的；向匿名者精确报版本号等于替攻击者做 CVE 匹配。
    %% 外部 LB 需要的只是 200/503，不需要版本。
    %% C-51 的部署探活走 ssh 到 127.0.0.1，属内网，照样拿得到版本。
    Vsn =
        case is_internal(Req0) of
            true -> app_vsn();
            false -> <<"hidden">>
        end,
    {Code, Body} =
        case probe_db() of
            true ->
                {200, <<"{\"status\":\"ok\",\"db\":\"up\",\"version\":\"", Vsn/binary, "\"}">>};
            false ->
                {503,
                    <<"{\"status\":\"degraded\",\"db\":\"down\",\"version\":\"", Vsn/binary,
                        "\"}">>}
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

%% @doc 请求是否来自内网。复用 metrics_handler 里已有的判定，不另写一份 ——
%% 两份网段判定迟早会漂（B-26 那类"同一知识抄多份"的坑）。
-spec is_internal(cowboy_req:req()) -> boolean().
is_internal(Req) ->
    try
        {Ip, _Port} = cowboy_req:peer(Req),
        metrics_handler:is_internal_ip(Ip)
    catch
        %% 取不到 peer（测试桩/异常代理）时按**外网**处理：宁可少报版本，
        %% 不可因判定失败而泄露。
        _:_ -> false
    end.

%% @doc 上报自身版本。C-51 的部署就绪判断要用它区分"端口通了"和
%% "**我要的这个版本**通了" —— 目标色端口上残留着上一版进程时，
%% 只探端口会让部署误判成功，把流量切到旧二进制上。
-spec app_vsn() -> binary().
app_vsn() ->
    case application:get_key(imboy, vsn) of
        {ok, V} when is_list(V) -> list_to_binary(V);
        {ok, V} when is_binary(V) -> V;
        _ -> <<"unknown">>
    end.
