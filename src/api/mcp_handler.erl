-module(mcp_handler).
-behavior(cowboy_loop).

%%%
% MCP Server cowboy handler / MCP Streamable HTTP 桥接（Phase 3 T3.2 + T3.6）
%
% 把 vendored barrel_mcp 协议引擎（传输无关）桥接到 imboy 自己的 cowboy：
%   POST body(JSON-RPC) → barrel_mcp_protocol:decode → handle/2 → encode → JSON
%   GET  → SSE 长连接（server→client 推送 list_changed / progress 通知），
%          支持 Mcp-Session-Id 会话与 Last-Event-ID 断线重放（T3.6，
%          cowboy_loop 模式，样板见 qr_login_sse_handler）。
%
% 同一 init/2 按 method 分派：POST 返回 {ok,...}（普通 handler），
% GET 返回 {cowboy_loop,...}（升级为长连接）。二者同 handler 合法。
%
% 路由 /api/v1/mcp（进 imboy_router 静态 ApiV1Routes，不走 imboy_router_registry）。
% JWT 注入（AuthInfo）见 T3.3：取 current_uid 作 handle/2 的 AuthInfo。
%%%

-export([init/2, info/3, terminate/3]).
%% process/1,2,3 为可测纯桥接（JSON binary → {StatusCode, RespBody[, SessionId]}）
-export([process/1, process/2, process/3]).
%% sse_frame/2 为可测纯函数（EventId + JSON → SSE 帧含 id: 行）
-export([sse_frame/2]).

-include("barrel_mcp.hrl").

%% 请求体上限 1MB（MCP 单条 JSON-RPC 足够；防超大 body）
-define(MAX_BODY, 1048576).
%% tool 同步执行超时
-define(TOOL_TIMEOUT, 30000).
%% Heartbeat 间隔，cowboy 默认 idle_timeout=60s，30s 心跳留一倍余量
-define(HEARTBEAT_INTERVAL_MS, 30000).

-spec init(cowboy_req:req(), any()) ->
    {ok, cowboy_req:req(), any()} | {cowboy_loop, cowboy_req:req(), any()}.
init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = read_body(Req0, <<>>),
            %% T3.3：auth_middleware 已把 current_uid 注入 handler opts；
            %% 取调用者 uid 作 AuthInfo 线程进 tool Ctx（未认证=0）。
            AuthInfo = auth_ds:current_uid(State),
            %% T3.6：接 Mcp-Session-Id（无则 initialize 时惰性创建）。
            SessionId0 = cowboy_req:header(<<"mcp-session-id">>, Req1),
            {Code, Resp, SessionId} = process(Body, AuthInfo, SessionId0),
            Headers = maybe_session_header(
                #{<<"content-type">> => <<"application/json">>}, SessionId
            ),
            Req = cowboy_req:reply(Code, Headers, Resp, Req1),
            {ok, Req, State};
        <<"GET">> ->
            init_sse(Req0);
        _ ->
            {ok, cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0), State}
    end.

%% @doc 升级为 SSE 长连接（GET /api/v1/mcp）。
%% 读 mcp-session-id（无则新建）；读 last-event-id（有则重放增量）；
%% 绑定本 loop 进程为 session 的 sse_pid；响应头回带 Mcp-Session-Id。
-spec init_sse(cowboy_req:req()) -> {cowboy_loop, cowboy_req:req(), map()}.
init_sse(Req0) ->
    SessionId = resolve_sse_session(cowboy_req:header(<<"mcp-session-id">>, Req0)),
    Req1 = cowboy_req:stream_reply(
        200,
        #{
            <<"content-type">> => <<"text/event-stream">>,
            <<"cache-control">> => <<"no-cache">>,
            <<"connection">> => <<"keep-alive">>,
            <<"mcp-session-id">> => SessionId
        },
        Req0
    ),
    %% 重放先于 set_sse_pid：确保 last-event-id 之后、绑定之前无实时消息插入，
    %% 保证客户端收到的事件严格有序（重放 → 实时）。
    Req2 = maybe_replay(
        SessionId, cowboy_req:header(<<"last-event-id">>, Req1), Req1
    ),
    ok = barrel_mcp_session:set_sse_pid(SessionId, self()),
    Timer = erlang:send_after(?HEARTBEAT_INTERVAL_MS, self(), mcp_sse_heartbeat),
    {cowboy_loop, Req2, #{
        session_id => SessionId,
        heartbeat_timer => Timer
    }}.

%% 解析/新建 SSE 会话：头缺失或指向不存在的会话 → 新建。
resolve_sse_session(Sid) when is_binary(Sid), byte_size(Sid) > 0 ->
    case barrel_mcp_session:get(Sid) of
        {ok, _} -> Sid;
        {error, _} -> new_session()
    end;
resolve_sse_session(_) ->
    new_session().

new_session() ->
    {ok, Sid} = barrel_mcp_session:create(#{}),
    Sid.

%% 无 Last-Event-ID → 不重放；有则取增量重放；越界（truncated）→ 提示客户端全量重拉。
maybe_replay(_SessionId, undefined, Req) ->
    Req;
maybe_replay(SessionId, LastId, Req) ->
    case barrel_mcp_session:events_since(SessionId, LastId) of
        {ok, Events} ->
            lists:foldl(
                fun({EventId, Payload}, ReqAcc) ->
                    Json = barrel_mcp_protocol:encode(Payload),
                    cowboy_req:stream_body(
                        sse_frame(EventId, Json), nofin, ReqAcc
                    )
                end,
                Req,
                Events
            );
        _ ->
            %% truncated / not_found：缓冲已滚出窗口，让客户端全量重拉。
            Resync = barrel_mcp_protocol:encode_notification(
                <<"notifications/mcp/resync">>,
                #{<<"reason">> => <<"last_event_id_expired">>}
            ),
            cowboy_req:stream_body(
                <<"data: ", (barrel_mcp_protocol:encode(Resync))/binary, "\n\n">>,
                nofin,
                Req
            )
    end.

%% @doc SSE loop info：心跳 / server→client 推送 / 会话终止。
-spec info(any(), cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req(), map()}.
info(mcp_sse_heartbeat, Req0, State) ->
    Req = cowboy_req:stream_body(<<": heartbeat\n\n">>, nofin, Req0),
    NewTimer = erlang:send_after(?HEARTBEAT_INTERVAL_MS, self(), mcp_sse_heartbeat),
    {ok, Req, State#{heartbeat_timer => NewTimer}};
info({sse_send_message, Msg}, Req0, #{session_id := SessionId} = State) when
    is_map(Msg)
->
    Json = barrel_mcp_protocol:encode(Msg),
    %% 每帧唯一递增 EventId，记入 ring buffer 供 Last-Event-ID 重放。
    EventId = integer_to_binary(erlang:unique_integer([monotonic, positive])),
    _ = barrel_mcp_session:record_sse_event(SessionId, EventId, Msg),
    Req = cowboy_req:stream_body(sse_frame(EventId, Json), nofin, Req0),
    {ok, Req, State};
info(session_terminated, Req, State) ->
    {stop, Req, State};
info(_Other, Req, State) ->
    {ok, Req, State}.

%% @doc 连接关闭：cancel 心跳 timer + 解绑 sse_pid（仅当仍指向本进程）。
-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_Reason, _Req, State) when is_map(State) ->
    _ =
        case maps:get(heartbeat_timer, State, undefined) of
            undefined -> ok;
            Timer -> erlang:cancel_timer(Timer)
        end,
    case maps:get(session_id, State, undefined) of
        undefined ->
            ok;
        SessionId ->
            %% 仅当本进程仍是登记的 sse_pid 才解绑，避免抹掉重连后的新连接。
            case barrel_mcp_session:get_sse_pid(SessionId) of
                {ok, Self} when Self =:= self() ->
                    _ = barrel_mcp_session:set_sse_pid(SessionId, undefined),
                    ok;
                _ ->
                    ok
            end
    end;
terminate(_Reason, _Req, _State) ->
    ok.

%% @doc SSE 事件帧：id: 行让客户端记录 Last-Event-ID，data: 行携带 JSON。
-spec sse_frame(binary(), binary()) -> binary().
sse_frame(EventId, Json) ->
    <<"id: ", EventId/binary, "\ndata: ", Json/binary, "\n\n">>.

%% 有 SessionId 才回带响应头（POST 无会话场景不加）。
maybe_session_header(Headers, undefined) ->
    Headers;
maybe_session_header(Headers, SessionId) when is_binary(SessionId) ->
    Headers#{<<"mcp-session-id">> => SessionId}.

%% @doc 无 auth 上下文的桥接（测试/无认证场景）
-spec process(binary()) -> {non_neg_integer(), binary()}.
process(Body) ->
    process(Body, undefined).

%% @doc 无会话桥接：JSON-RPC binary + AuthInfo → {HTTP 状态码, 响应体 JSON binary}
%% no_response（通知类）→ 202 空体；tools/call 的 {async,Plan} 经 drive_async_plan/3
%% 同步驱动（把 AuthInfo 注入 Ctx.auth_info，tool 内即可拿到调用者 uid 做越权校验）；
%% 其余同步 → 200 + JSON；解析失败 → 400 + -32700。
-spec process(binary(), term()) -> {non_neg_integer(), binary()}.
process(Body, AuthInfo) ->
    case barrel_mcp_protocol:decode(Body) of
        {ok, Request} ->
            dispatch(Request, AuthInfo, #{});
        {error, _} ->
            {400, parse_error()}
    end.

%% @doc 带会话桥接（T3.6）：额外接 Mcp-Session-Id（SessionId0，缺失=undefined）。
%% 无会话头且方法为 initialize 时惰性新建会话（MCP 规范：会话在 initialize 分配）；
%% 把 session_id 线程进 handle State（供 capabilities/订阅/日志级别等消费）；
%% 返回三元组，多带最终 SessionId 供响应头回带（无会话则 undefined）。
-spec process(binary(), term(), binary() | undefined) ->
    {non_neg_integer(), binary(), binary() | undefined}.
process(Body, AuthInfo, SessionId0) ->
    case barrel_mcp_protocol:decode(Body) of
        {ok, Request} ->
            SessionId = resolve_post_session(Request, SessionId0),
            State =
                case SessionId of
                    undefined -> #{};
                    _ -> #{session_id => SessionId}
                end,
            {Code, Resp} = dispatch(Request, AuthInfo, State),
            {Code, Resp, SessionId};
        {error, _} ->
            {400, parse_error(), SessionId0}
    end.

%% 头带会话 → 沿用；无头且 initialize → 新建；无头且其他方法 → 无会话（无状态调用）。
resolve_post_session(_Request, Sid) when is_binary(Sid), byte_size(Sid) > 0 ->
    Sid;
resolve_post_session(Request, _) ->
    case maps:get(<<"method">>, Request, undefined) of
        <<"initialize">> -> new_session();
        _ -> undefined
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% 已解码请求 → {HTTP 状态码, 响应体}。共享给 process/2、process/3。
dispatch(Request, AuthInfo, State) ->
    case barrel_mcp_protocol:handle(Request, State) of
        no_response ->
            {202, <<>>};
        {async, Plan} when is_map(Plan) ->
            Resp = barrel_mcp_protocol:drive_async_plan(
                Plan, ?TOOL_TIMEOUT, AuthInfo
            ),
            {200, barrel_mcp_protocol:encode(Resp)};
        Map when is_map(Map) ->
            {200, barrel_mcp_protocol:encode(Map)}
    end.

parse_error() ->
    Err = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => null,
        <<"error">> => #{
            <<"code">> => ?JSONRPC_PARSE_ERROR,
            <<"message">> => <<"Parse error">>
        }
    },
    barrel_mcp_protocol:encode(Err).

-spec read_body(cowboy_req:req(), binary()) -> {ok, binary(), cowboy_req:req()}.
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0, #{length => ?MAX_BODY}) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.
