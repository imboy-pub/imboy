-module(cors_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% @doc CORS 中间件
%% 处理跨域资源共享 (CORS) 预检请求和响应头
%%
%% 支持前端从 localhost 或其他域名访问 API
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return 中间件执行结果
%% @end
-spec execute(cowboy_req:req(), map()) ->
                 {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req0, Env) ->
    Method = cowboy_req:method(Req0),
    Origin = cowboy_req:header(<<"origin">>, Req0),

    % 设置 CORS 响应头
    Req1 = case Origin of
        undefined ->
            % 没有 Origin 头，可能是同源请求或非浏览器请求
            Req0;
        _ ->
            % 允许任何来源（生产环境建议设置具体域名）
            cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req0)
    end,

    Req2 = cowboy_req:set_resp_header(
        <<"access-control-allow-methods">>,
        <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>,
        Req1
    ),

    Req3 = cowboy_req:set_resp_header(
        <<"access-control-allow-headers">>,
        <<"content-type, authorization, accept, origin, x-requested-with, *">>,
        Req2
    ),

    Req4 = cowboy_req:set_resp_header(
        <<"access-control-expose-headers">>,
        <<"content-type, content-length, authorization">>,
        Req3
    ),

    Req5 = cowboy_req:set_resp_header(
        <<"access-control-max-age">>,
        <<"3600">>,
        Req4
    ),

    Req6 = cowboy_req:set_resp_header(
        <<"access-control-allow-credentials">>,
        <<"true">>,
        Req5
    ),

    % 处理 OPTIONS 预检请求
    case Method of
        <<"OPTIONS">> ->
            % 预检请求直接返回 204 No Content
            ReqFinal = cowboy_req:reply(204, Req6),
            {stop, ReqFinal};
        _ ->
            % 其他请求继续处理
            {ok, Req6, Env}
    end.
