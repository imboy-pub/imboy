-module(healthz_handler).
-moduledoc "健康检查端点 / Liveness + readiness probe。".
%%%===================================================================
%%% @doc 健康检查端点 / Liveness + readiness probe
%%%
%%% C-49：`/healthz` 此前**只在 throttle_middleware 的白名单里出现过**，
%%% 却从来没有对应路由 —— 实际请求返回 404。于是：
%%%   - compose/helm 的 healthcheck 配了也永远不健康
%%%   - deploy.sh 的就绪判断只能退化成"端口通不通"，而端口通 ≠ 服务可用
%%%
%%% TSID-06 起语义三分（计划 §4.1）：
%%%   /livez  —— liveness：BEAM/Cowboy 能执行到这里即活着；依赖故障
%%%              （TSID guard fenced / PG 挂）不触发容器重启风暴
%%%   /readyz —— readiness：聚合 PostgreSQL 与 TSID guard；不可生成 ID
%%%              或连不上库时 503，不接流量
%%%   /healthz —— /readyz 的兼容别名（保持既有响应字段并追加 tsid）
%%%
%%% 判据：
%%%   200 = 依赖就绪，可以接流量
%%%   503 = 进程活着但**依赖不可用**，不应被灌流量
%%%
%%% 这个端点在**每次探活**时被打，因此：
%%%   - 不查业务表，只做一次最轻的连通性探测（SELECT 1）
%%%   - 任何异常都收敛成 503，绝不 500（探针拿到 500 与拿到 503 的处置不同）
%%%
%%% ⚠️ 这里**刻意不做缓存**。初版用 persistent_term 缓存 2s，是错的：
%%%   persistent_term:put/2 每次写会触发**全局 GC**（扫描所有进程寻找旧值引用），
%%%   代价与进程数成正比 —— 本节点每个 WS 连接一个进程，量级上千。
%%%   而探活恰好在**部署期**最密集（deploy.sh 每 2s 探一次），等于在系统最吃紧的
%%%   时刻反复触发全局 GC。
%%%   缓存本来要省的那点开销：SELECT 1 对 PG 是白菜价，探活间隔 30s、
%%%   部署轮询总共也就 20 次。**为省一个不存在的开销引入一个真实的抖动源。**
%%% @end
%%%===================================================================

-export([init/2]).
%% probe_db/0 也是**容器 healthcheck 的入口**（C-50）：
%% 运行镜像是 debian-slim，没有 curl/wget，探不了 HTTP；
%% 但 release 自带 `bin/imboy eval`，直接调这个函数即可，无需额外工具。
%% probe_db/1 供测试注入独立预算（T02：探针 deadline 与 elib_pg 重试解耦）。
-export([probe_db/0, probe_db/1]).
%% tsid_readiness/0 供探针/部署脚本直调；probe/2 为纯决策
%% 函数，EUnit 合同测试与端点逻辑共用同一判据。
-export([tsid_readiness/0, probe/2]).

%% T02 探针独立预算：elib_pg:with_conn/2 的第二参数是**初始重试 sleep**
%% （T01 语义），池耗尽时公共路径会累计约 1+2+3 秒等待；epgsql 的
%% sync_command 又是 infinity 超时。探针必须在自己的明确预算内返回
%% （计划 §4.2.6 / §6 验收"取连接失败/池耗尽"行），因此不走 elib_pg，
%% 直接向 pooler 申请连接并自带总 deadline。两段预算分开定义（计划
%% §4.1.5 的探针局部落地），可用 application env 覆盖：
%%   {imboy, readyz_take_conn_timeout_ms}  取连接等待上限（默认 1000ms）
%%   {imboy, readyz_query_timeout_ms}      查询执行上限（默认 2000ms）
-define(READYZ_TAKE_CONN_TIMEOUT_MS, 1000).
-define(READYZ_QUERY_TIMEOUT_MS, 2000).

%% @doc /livez：liveness 恒 200（BEAM 活着即可，不看任何依赖）
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, #{mode := live} = State) ->
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"application/json; charset=utf-8">>,
            %% 探针结果绝不能被任何中间层缓存，否则挂了还一直返 200
            <<"cache-control">> => <<"no-store">>
        },
        <<"{\"status\":\"live\"}">>,
        Req0
    ),
    {ok, Req, State};
%% @doc /readyz 与 /healthz（兼容别名）：PG + TSID guard 聚合
init(Req0, State) ->
    %% ⚠️ 版本号**只对内网可见**。nginx 对 /metrics 返 403，但**没有拦 /healthz**，
    %% 所以本端点是公网可达且匿名的；向匿名者精确报版本号等于替攻击者做 CVE 匹配。
    %% 外部 LB 需要的只是 200/503，不需要版本。
    %% C-51 的部署探活走 ssh 到 127.0.0.1，属内网，照样拿得到版本。
    %% 节点列表则按运维可见性决策**公网可见**（owner 2026-09-18）：Erlang dist
    %% 端口只绑 127.0.0.1，节点名不构成可达路径，泄露面仅限侦察信息。
    Vsn =
        case is_internal(Req0) of
            true -> app_vsn();
            false -> <<"hidden">>
        end,
    Nodes = nodes_json(lists:usort([node() | nodes()])),
    Tsid = tsid_readiness(),
    {Code, Body} = probe_body(probe_db(), Tsid, Vsn, Nodes, imboy_app:readiness()),
    Req = cowboy_req:reply(
        Code,
        #{
            <<"content-type">> => <<"application/json; charset=utf-8">>,
            <<"cache-control">> => <<"no-store">>
        },
        Body,
        Req0
    ),
    {ok, Req, State}.

probe_body(Db, Tsid, Vsn, Nodes, Runtime) ->
    {Code, _} = probe(ready, #{db => Db, tsid => Tsid, runtime => Runtime}),
    StatusBin =
        case Code of
            200 -> <<"ok">>;
            503 -> <<"degraded">>
        end,
    DbBin =
        case Db of
            true -> <<"up">>;
            false -> <<"down">>
        end,
    TsidBin = atom_to_binary(Tsid, utf8),
    RuntimeBin = atom_to_binary(Runtime, utf8),
    {Code,
        <<"{\"status\":\"", StatusBin/binary, "\",\"db\":\"", DbBin/binary, "\",\"tsid\":\"",
            TsidBin/binary, "\",\"runtime\":\"", RuntimeBin/binary, "\",\"nodes\":", Nodes/binary,
            ",\"version\":\"", Vsn/binary, "\"}">>}.

%% @doc TSID readiness：V10 无 guard 进程，runtime 已发布（sup init 内
%% 完成）即 ready；未初始化即 not_initialized（不可发号，503）。
-spec tsid_readiness() -> ready | not_initialized.
tsid_readiness() ->
    elib_tsid:readiness().

%% @doc 纯决策函数（EUnit 合同测试专用）：live → 恒 200；
%% ready → 聚合 db、tsid 与运行时状态；HTTP 端点共用此判据。
%% runtime 为可选键：live 判据不依赖任何后端（计划 §4.2.6），缺省按
%% ready 视，仅供纯函数测试复刻旧调用形态。
-spec probe(live | ready, #{
    db := boolean(),
    tsid := ready | not_initialized,
    runtime => ready | starting | draining | degraded
}) ->
    {200 | 503, live | ready}.
probe(live, _) ->
    {200, live};
probe(ready, #{db := Db, tsid := Tsid} = Inputs) ->
    case {Db, Tsid, maps:get(runtime, Inputs, ready)} of
        {true, ready, ready} -> {200, ready};
        _ -> {503, ready}
    end.

%% @doc 最轻的连通性探测：`SELECT 1`。
%% 不查任何业务表 —— 业务表为空是合法状态，不该被判成不健康。
%% 预算取 application env（默认宏），总上界 ≈ 取连接 1s + 查询 2s。
-spec probe_db() -> boolean().
probe_db() ->
    probe_db(#{
        take_conn_timeout_ms =>
            application:get_env(imboy, readyz_take_conn_timeout_ms, ?READYZ_TAKE_CONN_TIMEOUT_MS),
        query_timeout_ms =>
            application:get_env(imboy, readyz_query_timeout_ms, ?READYZ_QUERY_TIMEOUT_MS)
    }).

%% @doc 带显式预算的连通性探测（测试/运维直调入口）。
%% 池耗尽 → 在取连接预算内返回 false，不再沿 elib_pg 的 1+2+3 秒
%% 重试长挂；查询超时 → kill 等待进程并淘汰状态不可知的连接
%% （T01 语义：状态未确认的连接不得复用，return_member fail 交池剔除）。
-spec probe_db(#{take_conn_timeout_ms := non_neg_integer(), query_timeout_ms := non_neg_integer()}) ->
    boolean().
probe_db(#{take_conn_timeout_ms := TakeMs, query_timeout_ms := QueryMs}) ->
    try
        case pooler:take_member(pgsql, TakeMs) of
            error_no_members ->
                false;
            Conn when is_pid(Conn) ->
                case is_process_alive(Conn) of
                    false ->
                        _ = pooler:return_member(pgsql, Conn, fail),
                        false;
                    true ->
                        probe_query(Conn, QueryMs)
                end
        end
    catch
        %% 任何底层异常（池未启动/epgsql 崩溃等）都收敛成 false：
        %% 探针的契约是"明确预算内的 503 判据"，绝不 500。
        _:_ -> false
    end.

%% 拿到连接后的有界查询：SELECT 1 在健康连接上毫秒级完成；只有 PG
%% 无响应/半死连接才会吃满 QueryMs 预算。
probe_query(Conn, QueryMs) ->
    case run_probe_query_bounded(Conn, QueryMs) of
        ok ->
            ok = return_probe_conn(Conn, ok),
            true;
        {error, probe_deadline_exceeded} ->
            %% 等待进程被 kill，连接协议状态不可知：淘汰而非归还
            %% （run_probe_query_bounded 内已 fail 归还）。淘汰是运维
            %% 关心事件（PG 慢/半死连接），必须留痕。
            logger:warning(
                "[readyz] probe deadline exceeded (~p ms); "
                "connection discarded for rebuild",
                [QueryMs]
            ),
            false;
        {error, {probe_runner_exit, _Other}} ->
            %% Runner 系统级崩溃：连接协议状态不可知（命令可能仍在途），
            %% 与超时路径同语义——fail 淘汰，不按 ok 归还（R2 review P3）。
            logger:warning(
                "[readyz] probe runner exited unexpectedly; "
                "connection discarded for rebuild"
            ),
            ok = return_probe_conn(Conn, fail),
            false;
        {error, _Other} ->
            %% 查询明确失败但连接进程存活（如 PG 报错）：正常归还。
            ok = return_probe_conn(Conn, ok),
            false
    end.

return_probe_conn(Conn, Status) ->
    case is_process_alive(Conn) of
        true -> pooler:return_member(pgsql, Conn, Status);
        false -> pooler:return_member(pgsql, Conn, fail)
    end.

%% epgsql 的 sync_command 是 gen_server:call(..., infinity)：查询本身
%% 没有内建 deadline。这里用独立监控进程执行查询，到期 kill 等待者。
%% 注意：被 kill 的只是等待进程，epgsql 连接进程可能仍在执行命令，
%% 因此超时路径必须以 fail 归还连接让 pooler 剔除重建（探针低频，
%% 偶发淘汰一条连接的成本远小于探针长挂导致漏报 503）。
run_probe_query_bounded(Conn, QueryMs) ->
    {Runner, Mon} = spawn_monitor(fun() ->
        Result =
            try
                epgsql:equery(Conn, <<"SELECT 1">>, [])
            catch
                _:_ -> {error, probe_query_failed}
            end,
        exit({probe_done, Result})
    end),
    receive
        {'DOWN', Mon, process, Runner, {probe_done, Result}} ->
            case element(1, Result) of
                ok -> ok;
                _ -> {error, Result}
            end;
        {'DOWN', Mon, process, Runner, Other} ->
            {error, {probe_runner_exit, Other}}
    after QueryMs ->
        exit(Runner, kill),
        receive
            {'DOWN', Mon, process, Runner, killed} -> ok
        after 0 -> ok
        end,
        _ = pooler:return_member(pgsql, Conn, fail),
        {error, probe_deadline_exceeded}
    end.

%% @doc 在线 Erlang 节点列表（本节点 + 已建立 dist 连接的节点）编码为 JSON 数组。
%% 单机蓝绿部署时即当前服务节点；未来横向扩展后各节点互连，此处即为全量成员。
-spec nodes_json([node()]) -> binary().
nodes_json(Nodes) ->
    iolist_to_binary([
        $[,
        lists:join($,, [<<$", (atom_to_binary(N, utf8))/binary, $">> || N <- Nodes]),
        $]
    ]).

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
