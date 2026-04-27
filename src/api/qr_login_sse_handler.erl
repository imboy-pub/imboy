%%%-------------------------------------------------------------------
%%% @doc QR 登录 SSE Handler（Phase 2 PR-3α，cowboy_loop behavior）
%%%
%%% 端点：GET /v1/passport/qr_login/subscribe?session_token=<binary>
%%%
%%% 协议：Server-Sent Events (text/event-stream)
%%%   - 客户端用 EventSource 长连接
%%%   - 服务端每收到 syn:publish 的 Event map 推一帧 `data: <json>\n\n`
%%%   - confirmed / expired 后服务端主动 stop 关闭连接（终态语义）
%%%   - waiting / scanned / cancelled 持续保持连接
%%%
%%% 与 PR-2β 后端事件流的接续：
%%%   `qr_login_event_ds:notify(SessionToken, Event)`
%%%      → syn:publish(?QR_LOGIN_SCOPE, {qr_login, SessionToken}, Event)
%%%      → cowboy_loop info/3 收到 Event → 推 SSE chunk
%%%
%%% 风险登记：本模块为 imboy 项目首个 cowboy_loop 用例。
%%% 设计原则（actor model + Let It Crash）：
%%%   - init 失败立即 crash supervisor 自愈（不 try/catch 包 stream_reply）
%%%   - info 收到非 map 消息容错（兼容 syn 多版本）
%%%   - terminate 必须 syn:leave 防僵尸订阅
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_sse_handler).
-behavior(cowboy_loop).

-export([init/2, info/3, terminate/3]).

%% Heartbeat 间隔，cowboy 默认 idle_timeout=60s，30s 心跳留一倍余量
%% Heartbeat interval; cowboy default idle_timeout=60s, 30s leaves 2x margin
-define(HEARTBEAT_INTERVAL_MS, 30000).

%% ===================================================================
%% cowboy_loop callbacks
%% ===================================================================

%% @doc 升级为 SSE 长连接。
%%
%% 校验 query 参数 session_token：
%%   - 合法 → stream_reply 200 + SSE headers + subscribe → {cowboy_loop, ...}
%%   - 非法/缺失 → reply 400 → {ok, ...}（普通 handler 退出，不进 loop）
-spec init(cowboy_req:req(), any()) ->
    {ok, cowboy_req:req(), any()} | {cowboy_loop, cowboy_req:req(), any()}.
init(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    case proplists:get_value(<<"session_token">>, Qs) of
        SessionToken when is_binary(SessionToken), byte_size(SessionToken) > 0 ->
            Req = cowboy_req:stream_reply(200, #{
                <<"content-type">> => <<"text/event-stream">>,
                <<"cache-control">> => <<"no-cache">>,
                <<"connection">> => <<"keep-alive">>
            }, Req0),
            %% 加入 syn group。失败也继续 loop（订阅丢失 ≈ 0 投递，与 PR-2α
            %% notify 兜底语义一致；客户端有降级轮询兜底）。
            _ = qr_login_event_ds:subscribe(SessionToken, self()),
            %% 启动 heartbeat 防 cowboy idle_timeout 自动断连
            %% Start heartbeat to prevent cowboy idle_timeout disconnect
            Timer = erlang:send_after(?HEARTBEAT_INTERVAL_MS, self(), qr_sse_heartbeat),
            {cowboy_loop, Req, #{session_token => SessionToken,
                                 heartbeat_timer => Timer}};
        _ ->
            Req = cowboy_req:reply(400, #{}, <<"missing session_token">>, Req0),
            {ok, Req, #{}}
    end.

%% @doc 接收 syn:publish 投递的 Event map，推 SSE chunk。
%%
%% Event 来自 qr_login_event_ds:event/2 构造，约束字段：
%%   - <<"status">> :: binary()  必须存在（waiting|scanned|confirmed|expired|cancelled）
%%   - <<"token">>  :: binary()  仅 confirmed 时存在
%%
%% 终态判断：
%%   - status=confirmed → stop（Web 端已拿到 token，无需保持连接）
%%   - status=expired   → stop（QR 失效，Web 端应刷新）
%%   - 其他             → 继续 loop
-spec info(any(), cowboy_req:req(), any()) ->
    {ok, cowboy_req:req(), any()} |
    {ok, cowboy_req:req(), any(), hibernate} |
    {stop, cowboy_req:req(), any()}.
info(qr_sse_heartbeat, Req0, State) ->
    %% Heartbeat：发 SSE comment "\: heartbeat\n\n"（浏览器 EventSource 自动过滤
    %% 不触发 onmessage），重启 timer。维持 TCP keepalive 防 cowboy idle_timeout。
    %% Heartbeat: send SSE comment, browsers auto-filter (no onmessage triggered),
    %% then reschedule timer to maintain TCP keepalive.
    Req = cowboy_req:stream_body(<<": heartbeat\n\n">>, nofin, Req0),
    NewTimer = erlang:send_after(?HEARTBEAT_INTERVAL_MS, self(), qr_sse_heartbeat),
    {ok, Req, State#{heartbeat_timer => NewTimer}};
info(Event, Req0, State) when is_map(Event) ->
    Json = jsx:encode(Event),
    Chunk = <<"data: ", Json/binary, "\n\n">>,
    Req = cowboy_req:stream_body(Chunk, nofin, Req0),
    case maps:get(<<"status">>, Event, undefined) of
        <<"confirmed">> -> {stop, Req, State};
        <<"expired">>   -> {stop, Req, State};
        _               -> {ok, Req, State}
    end;
info(_Other, Req, State) ->
    %% 非 map 消息（system / tcp / 其他）容错：保持 loop，不中断
    {ok, Req, State}.

%% @doc 连接关闭时 syn:leave 释放订阅，防僵尸 group 成员。
%%
%% 调用时机：客户端断开 / 服务端 stop / 异常 crash 前。
%% State 中可能没 session_token（init 失败路径），用 maps:get 默认 undefined 防御。
-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_Reason, _Req, State) when is_map(State) ->
    %% 先 cancel heartbeat timer 防止心跳消息发到已死进程
    %% Cancel heartbeat timer first to prevent send-to-dead-process
    case maps:get(heartbeat_timer, State, undefined) of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    case maps:get(session_token, State, undefined) of
        undefined -> ok;
        SessionToken ->
            _ = qr_login_event_ds:unsubscribe(SessionToken, self()),
            ok
    end;
terminate(_Reason, _Req, _State) ->
    %% init 失败路径 state=[] 或其他类型，无 session_token 可清理
    ok.
