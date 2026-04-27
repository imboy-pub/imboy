%%%-------------------------------------------------------------------
%%% @doc QR 登录会话事件 DS（Phase 2 推送基础设施 PR-1，纯函数契约）
%%%
%%% 锚定 `imboy/src/api/qr_login_handler.erl` 的会话状态机，定义两类纯函数：
%%%
%%%   1. `topic_for/1`：根据 SessionToken 构造 syn 广播 topic 名（base64 安全）
%%%   2. `event/2`：根据 Status + 可选 Token 构造客户端可消费的事件 map
%%%
%%% 本 PR 是**纯加法**：不动任何现有路由 / handler / cache / syn 调用。
%%% 后续 PR-2 才会在 `qr_login_handler:handle_scan/2` 等位点调用 `event/2`
%%% 并通过 syn 广播；PR-3 引入 `cowboy_loop` SSE handler 接收事件下推。
%%%
%%% 设计原则（actor model + Let It Crash）：
%%%   - 本模块零进程、零 ETS、零 syn 副作用，仅纯函数 → 易测、易复用
%%%   - 事件 schema 与 `lib/page/passport/qr_login_response_rules.dart` 的
%%%     `parseQrStatusResponse` 保持兼容（status 字段语义对齐）
%%%   - 协议违反（如 confirmed 但 token 为空）由调用方拦截，本模块只做格式
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_event_ds).

-include("chat.hrl").

-export([topic_for/1]).
-export([event/2]).
-export([subscribe/2, unsubscribe/2, notify/2]).

%% ===================================================================
%% Topic 命名
%% ===================================================================

%% @doc 由 SessionToken 构造 syn 广播 topic（atom 形式）。
%%
%% syn:join/syn:publish 要求 topic 为 atom 或 {atom, term()}，本函数返回
%% `{qr_login, SessionToken}` 元组，与现有 `imboy_cache:set({qr_login, ...})`
%% 的 key 结构同源，便于排查。
%%
%% **关键守卫**：SessionToken 必须为非空 binary，否则 throw `badarg`（Let It
%% Crash —— 调用方不应传非法值，正常路径不触发）。
-spec topic_for(SessionToken :: binary()) -> {qr_login, binary()}.
topic_for(SessionToken) when is_binary(SessionToken), byte_size(SessionToken) > 0 ->
    {qr_login, SessionToken};
topic_for(_) ->
    erlang:error(badarg).

%% ===================================================================
%% 事件构造
%% ===================================================================

%% @doc 根据状态 + 可选 token 构造客户端可消费的事件 map。
%%
%% 与 `qr_login_handler:handle_status/1` 的响应 payload 结构对齐：
%%   - `<<"status">>`：waiting | scanned | confirmed | cancelled | expired
%%   - `<<"token">>`：仅 status=confirmed 时附 JWT，其他状态省略字段
%%
%% **守卫**：
%%   - status=confirmed 但 Token 为空 binary → throw `protocol_violation`
%%     （由调用方在调 `event/2` 前已校验，此处兜底防御）
%%   - 未知 status → throw `unknown_status`
%%
%% 与客户端 `parseQrStatusResponse` 字段对齐（dart sealed `QrStatusEvent`）：
%%   - waiting/scanned/expired/cancelled → 单 status 字段
%%   - confirmed → status + token 两字段
-spec event(
    Status :: waiting | scanned | confirmed | cancelled | expired,
    Token :: binary() | undefined
) -> map().
event(confirmed, Token) when is_binary(Token), byte_size(Token) > 0 ->
    #{<<"status">> => <<"confirmed">>, <<"token">> => Token};
event(confirmed, _) ->
    erlang:error(protocol_violation);
event(waiting, _) ->
    #{<<"status">> => <<"waiting">>};
event(scanned, _) ->
    #{<<"status">> => <<"scanned">>};
event(expired, _) ->
    #{<<"status">> => <<"expired">>};
event(cancelled, _) ->
    #{<<"status">> => <<"cancelled">>};
event(_, _) ->
    erlang:error(unknown_status).

%% ===================================================================
%% syn 集成（带 try/catch 兜底，scope 未注册时 silent 返回 ok / 0）
%% ===================================================================

%% @doc PR-3 SSE handler 调用，加入 SessionToken 对应的 group。
%%
%% 失败安全：scope 未注册 / Pid 已死 / 网络分区 → 返回 {error, Reason}，
%% 由调用方决定是否重试。
-spec subscribe(SessionToken :: binary(), Pid :: pid()) ->
    ok | {error, term()}.
subscribe(SessionToken, Pid) ->
    Topic = topic_for(SessionToken),
    try syn:join(?QR_LOGIN_SCOPE, Topic, Pid) of
        ok -> ok;
        {error, _} = Err -> Err;
        Other -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.

%% @doc PR-3 SSE handler 在 terminate 时调用，离开 group 防止僵尸订阅。
-spec unsubscribe(SessionToken :: binary(), Pid :: pid()) ->
    ok | {error, term()}.
unsubscribe(SessionToken, Pid) ->
    Topic = topic_for(SessionToken),
    try syn:leave(?QR_LOGIN_SCOPE, Topic, Pid) of
        ok -> ok;
        {error, _} = Err -> Err;
        Other -> {error, Other}
    catch
        _:Reason -> {error, Reason}
    end.

%% @doc qr_login_handler 在 scan/confirm 后调用，向所有订阅者广播事件。
%%
%% 失败安全：scope 未注册 → 返回 {ok, 0}（与 imboy_syn:publish 行为一致），
%% 不影响 handler 主流程。Event 应由 event/2 构造，确保格式合法。
-spec notify(SessionToken :: binary(), Event :: map()) ->
    {ok, non_neg_integer()}.
notify(SessionToken, Event) ->
    Topic = topic_for(SessionToken),
    try syn:publish(?QR_LOGIN_SCOPE, Topic, Event) of
        {ok, N} when is_integer(N), N >= 0 -> {ok, N};
        _Other -> {ok, 0}
    catch
        _:_ -> {ok, 0}
    end.
