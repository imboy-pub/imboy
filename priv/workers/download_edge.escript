#!/usr/bin/env escript
%%! -noshell +Bd
%%%
%% V3-08 独立下载边缘（长驻 OS 进程，S3-only 合同 §5.2/§7）：
%%   * 客户端 GET/HEAD /edge?ticket=…（保持业务 URL 语义，不暴露 origin）；
%%   * 逐请求授权：每个请求先回调主 BEAM authorize 端点（loopback + 内网
%%     共享密钥），deny 即统一 404 隐藏——撤权即时生效；
%%   * 字节仅在本进程流动：Garage presigned URL → 流式转发客户端；
%%     Range/HEAD/206/416/404 语义由 attachment_edge_proxy_handler 处理；
%%   * 主 BEAM 只发有界 JSON（url/错误），零正文（BEAM=0）。
%% 环境变量：EDGE_PORT、EDGE_AUTHORIZE_URL、EDGE_INTERNAL_TOKEN、
%%           EDGE_EBIN、EDGE_CODE_PATHS（:分隔）
%%!
main(_) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    code:add_patha(os:getenv("EDGE_EBIN", ".")),
    [code:add_patha(P) || P <- string:split(os:getenv("EDGE_CODE_PATHS", ""), ":", all), P =/= ""],
    {ok, _} = application:ensure_all_started(cowboy),
    Port = list_to_integer(os:getenv("EDGE_PORT", "13990")),
    AuthorizeUrl = os:getenv("EDGE_AUTHORIZE_URL"),
    true = AuthorizeUrl =/= false andalso AuthorizeUrl =/= "",
    Token = os:getenv("EDGE_INTERNAL_TOKEN", ""),
    Ctx = {AuthorizeUrl, Token},
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/edge", attachment_edge_proxy_handler, Ctx},
            {"/api/v1/talk-records/client-audio-source", attachment_edge_proxy_handler,
                {AuthorizeUrl, Token, client_audio}},
            {"/_avatar/:alias", attachment_edge_proxy_handler, {AuthorizeUrl, Token, avatar}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(edge_http, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    io:format(standard_error, "EDGE_READY port=~p~n", [Port]),
    timer:sleep(infinity).
