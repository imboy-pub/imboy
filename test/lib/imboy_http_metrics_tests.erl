-module(imboy_http_metrics_tests).

%%%===================================================================
%%% @doc B-26：HTTP 请求指标产出 + 指标名与 deploy 配置的契约。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(elib_metric, [passthrough, non_strict]),
    meck:expect(elib_metric, increment, fun(_N, _V, _L) -> ok end),
    ok.

cleanup(_) ->
    catch meck:unload(elib_metric),
    ok.

labels() ->
    [
        L
     || {_P, {elib_metric, increment, [imboy_http_requests_total, _V, L]}, _R} <-
            meck:history(elib_metric)
    ].

observe_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun records_method_and_status/0,
        fun unknown_method_is_bucketed/0,
        fun missing_status_is_not_dropped/0,
        fun never_raises/0
    ]}.

records_method_and_status() ->
    ok = imboy_http_metrics:observe(#{req => #{method => <<"POST">>}, resp_status => 500}),
    ?assertEqual([#{method => <<"POST">>, status => <<"500">>}], labels()).

%% 方法名不得原样透传：任意 method 会把 Prometheus 序列数撑爆（标签基数爆炸）
unknown_method_is_bucketed() ->
    ok = imboy_http_metrics:observe(#{req => #{method => <<"PROPFIND">>}, resp_status => 200}),
    ?assertEqual([#{method => <<"OTHER">>, status => <<"200">>}], labels()).

%% 连接中断等 cowboy 不给状态的情形记 "000" 而不是丢弃 ——
%% 丢掉会让分母偏小、5xx 错误率虚高
missing_status_is_not_dropped() ->
    ok = imboy_http_metrics:observe(#{req => #{method => <<"GET">>}}),
    ?assertEqual([#{method => <<"GET">>, status => <<"000">>}], labels()).

%% 跑在每个请求的收尾路径上，任何输入都不许抛
never_raises() ->
    ?assertEqual(ok, imboy_http_metrics:observe(not_a_map)),
    ?assertEqual(ok, imboy_http_metrics:observe(#{})),
    ?assertEqual(ok, imboy_http_metrics:observe(#{req => broken, resp_status => <<"x">>})).

%% ===================================================================
%% 指标名契约：deploy 里引用的应用侧指标名必须在 src/ 里真的产出
%%
%% B-26 的根因就是这条契约没人守：面板和告警引用 imboy_/erlang_vm_ 开头的名字，
%% 而 metric_name/1 直接 atom_to_binary 导出的是另一套 —— 9 个面板 7 个永久
%% "No data"、6 条告警永远不触发，且**没有任何东西会报错**。
%% 这个测试就是那把尺子：改名字时它先红。
%% ===================================================================

%% 应用侧（非 exporter/非 pushgateway）指标名白名单。
%% node_/pg_ 开头的来自 exporter（B-24），imboy_backup_ 来自 pushgateway（B-23），
%% 都不由本仓 src/ 产出，故不在此列。
app_metric_names() ->
    [
        <<"erlang_vm_memory_bytes_total">>,
        <<"erlang_vm_port_count">>,
        <<"erlang_vm_process_count">>,
        <<"imboy_http_requests_total">>,
        <<"imboy_msg_sent_total">>,
        <<"imboy_ws_connections_total">>,
        <<"process_uptime_seconds">>
    ].

metric_name_contract_test() ->
    SrcText = read_src_text(),
    lists:foreach(
        fun(Name) ->
            ?assertMatch(
                {Name, true},
                {Name, binary:match(SrcText, Name) =/= nomatch}
            )
        end,
        app_metric_names()
    ).

%% 把 src/ 下所有 .erl 拼成一坨文本做包含判断。粗暴但足够：
%% 我们要证的是"这个名字在生产代码里出现过"，不是它的语法位置。
read_src_text() ->
    Files = filelib:wildcard("src/**/*.erl"),
    ?assert(length(Files) > 0),
    iolist_to_binary([
        case file:read_file(F) of
            {ok, Bin} -> Bin;
            _ -> <<>>
        end
     || F <- Files
    ]).
