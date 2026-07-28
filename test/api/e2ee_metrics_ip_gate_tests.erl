%%% `/metrics` 的**内网 IP 门**接线实证。
%%%
%%% == 为什么需要它 ==
%%%
%%% E2EE-062 的耗尽指标（`olm_otk_exhausted_total` /
%%% `olm_prekey_unavailable_total`）经 `/metrics` 导出。这些计数**会泄漏攻击活动**
%%% （某段时间内耗尽发生得多频繁），同时该端点还带 License/用户数/系统指标。
%%%
%%% 现有防护有两层：
%%%   1. nginx 入口 `location ~ ^/(api/)?(v1/)?metrics$ { return 403; }`
%%%      —— 已核实存在，且注释明写「后端 is_internal_ip 校验 TCP peer，
%%%      反代后 peer 是内网 IP 会被放行，故必须入口拦截」；
%%%   2. 后端 `metrics_handler:init/2` 的 `is_internal_ip` 门。
%%%
%%% ⚠️ **第 2 层在某些部署里是唯一防线**：`deploy/helm/values.yaml` 给 Pod 加了
%%% `prometheus.io/path: "/metrics"` 注解，k8s 抓取路径**不经过 nginx**。
%%% 而这一层此前**没有任何测试**——被重构掉不会有人发现。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. 【对照组】`is_internal_ip/1` 对已知内/外网地址分类正确。
%%%    它红说明判据本身就错了，后面两条的结论都不成立；
%%% 2. 外网 peer → **403**，且**不得触达指标读取**（不泄漏一个字节）；
%%% 3. 【正向可用性】内网 peer → **照常提供指标**。
%%%    一个"一律 403"的实现在"不泄漏"这个指标上恒得满分，必须被这条否掉。
-module(e2ee_metrics_ip_gate_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% 1. 对照组：纯谓词分类正确
%% ===================================================================

internal_ip_classification_test() ->
    %% 内网
    ?assert(metrics_handler:is_internal_ip({127, 0, 0, 1})),
    ?assert(metrics_handler:is_internal_ip({10, 1, 2, 3})),
    ?assert(metrics_handler:is_internal_ip({172, 16, 0, 1})),
    ?assert(metrics_handler:is_internal_ip({172, 31, 255, 254})),
    ?assert(metrics_handler:is_internal_ip({192, 168, 1, 1})),
    ?assert(metrics_handler:is_internal_ip({0, 0, 0, 0, 0, 0, 0, 1})),
    %% 外网（含 172.15 / 172.32 这两个紧贴 RFC-1918 边界的地址）
    ?assertNot(metrics_handler:is_internal_ip({8, 8, 8, 8})),
    ?assertNot(metrics_handler:is_internal_ip({172, 15, 0, 1})),
    ?assertNot(metrics_handler:is_internal_ip({172, 32, 0, 1})),
    ?assertNot(metrics_handler:is_internal_ip({193, 168, 1, 1})).

%% ===================================================================
%% 2. 外网 peer → 403，且不得触达指标读取
%% ===================================================================

external_peer_gets_403_and_no_metrics_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'peer', 1, fun(_Req) -> {{8, 8, 8, 8}, 12345} end},
                {'reply', 4, fun(Code, _Headers, Body, _Req) -> {replied, Code, Body} end},
                {'header', 3, fun(_N, _Req, Default) -> Default end}
            ]},
            {elib_metric, [
                {'get_all_metrics', 0, fun() ->
                    erlang:error(must_not_read_metrics_for_external_peer)
                end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = metrics_handler:init(cowboy_req_ok, #{}),
            ?assertEqual(
                {replied, 403, <<>>},
                Result,
                "外网可读 /metrics 会泄漏 License/用户数，以及"
                "E2EE-062 的耗尽计数（等于把攻击进展告诉攻击者）"
            )
        end
    ).

%% ===================================================================
%% 3. 正向可用性：内网 peer 必须照常拿到指标
%% ===================================================================

%% 一个"一律 403"的实现在"不泄漏"上恒得满分；没有这条，
%% 把整个端点关掉也能让上一条通过。
internal_peer_is_served_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'peer', 1, fun(_Req) -> {{127, 0, 0, 1}, 12345} end},
                {'reply', 4, fun(Code, _Headers, Body, _Req) -> {replied, Code, Body} end},
                {'header', 3, fun(_N, _Req, _Default) -> <<"text/plain">> end}
            ]},
            {elib_metric, [
                {'get_all_metrics', 0, fun() ->
                    #{counters => #{gate_probe_counter => 3}, histograms => #{}}
                end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = metrics_handler:init(cowboy_req_ok, #{}),
            ?assertMatch({replied, 200, _}, Result),
            {replied, 200, Body} = Result,
            ?assertNotEqual(
                nomatch,
                binary:match(iolist_to_binary(Body), <<"gate_probe_counter">>),
                "内网必须真的拿到指标内容，而不是一个空的 200"
            )
        end
    ).
