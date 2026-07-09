-module(mcp_handler).

%%%
% MCP Server cowboy handler / MCP Streamable HTTP 同步桥接（Phase 3 T3.2）
%
% 把 vendored barrel_mcp 协议引擎（传输无关）桥接到 imboy 自己的 cowboy：
%   POST body(JSON-RPC) → barrel_mcp_protocol:decode → handle/2 → encode → JSON
% MVP 只做同步 request/response（initialize/tools.list/tools.call）；SSE/会话/
% Last-Event-ID 重放留 T3.6（复用 qr_login_sse_handler 的 cowboy_loop 模式）。
%
% 路由 /api/v1/mcp（进 imboy_router 静态 ApiV1Routes，不走 imboy_router_registry）。
% JWT 注入（AuthInfo）留 T3.3：现以 #{} 作 handle/2 的 State。
%%%

-export([init/2]).
%% process/1 为可测纯桥接（JSON binary → {StatusCode, RespBody}）
-export([process/1]).

-include("barrel_mcp.hrl").

%% 请求体上限 1MB（MCP 单条 JSON-RPC 足够；防超大 body）
-define(MAX_BODY, 1048576).

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = read_body(Req0, <<>>),
            {Code, Resp} = process(Body),
            Req = cowboy_req:reply(
                Code,
                #{<<"content-type">> => <<"application/json">>},
                Resp,
                Req1
            ),
            {ok, Req, State};
        _ ->
            {ok, cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0), State}
    end.

%% @doc 纯桥接：JSON-RPC binary → {HTTP 状态码, 响应体 JSON binary}
%% no_response（通知类，无 id）→ 202 空体；正常/async → 200 + JSON；解析失败 → 400 + -32700
-spec process(binary()) -> {non_neg_integer(), binary()}.
process(Body) ->
    case barrel_mcp_protocol:decode(Body) of
        {ok, Request} ->
            case barrel_mcp_protocol:handle(Request, #{}) of
                no_response ->
                    {202, <<>>};
                {async, Map} when is_map(Map) ->
                    {200, barrel_mcp_protocol:encode(Map)};
                Map when is_map(Map) ->
                    {200, barrel_mcp_protocol:encode(Map)}
            end;
        {error, _} ->
            Err = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => null,
                <<"error">> => #{
                    <<"code">> => ?JSONRPC_PARSE_ERROR,
                    <<"message">> => <<"Parse error">>
                }
            },
            {400, barrel_mcp_protocol:encode(Err)}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec read_body(cowboy_req:req(), binary()) -> {ok, binary(), cowboy_req:req()}.
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0, #{length => ?MAX_BODY}) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.
