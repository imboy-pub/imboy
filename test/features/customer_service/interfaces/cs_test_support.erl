%%% @doc CS-02 三套件的公共支撑（test-only，无 DB、无业务断言）。
%%%
%%%   1. **真 HTTP（零 DB）**：在临时端口（`{port, 0}`）起一个只挂**单条真路由**
%%%      的监听器，用**真 cowboy route Opts**（从 `imboy_router:get_routes/0` 取，
%%%      含 CS-02 注入的 surface/feature/auth_facts），再叠加测试注入：会话键
%%%      （`current_uid` / `adm_user_id`）与事实装配（`auth_facts => cs_fake_facts`）。
%%%      `current_uid`/`adm_user_id` 生产由 auth_middleware_api_v1 / adm_auth_middleware
%%%      写入 handler_opts；本套件测 handler 及其下游（真 cs_auth + **meck 的 facade**），
%%%      故由测试扮演中间件注入角色。facade 用 meck 打桩——纪律：纯套件不触真库。
%%%   2. **请求**：裸 TCP 发原始 HTTP，解析状态行/头/定长或 chunked 体。
%%%   3. **路由访问**：按面 + 动作取 `{Pattern, Opts}`（路由漂移即 crash）。
-module(cs_test_support).

-export([
    handler_of/1,
    listener_for/3,
    with_listener/4,
    stop/1,
    request/4,
    request/5,
    stream_request/6,
    parse/1,
    status/1,
    json/1,
    code/1,
    msg/1,
    payload/1,
    route_opt/2,
    path/3,
    cs_routes/1,
    seat_state/2,
    visit_state/2,
    admin_state/2,
    platform_state/2
]).

-define(TIMEOUT, 10000).

%% ===================================================================
%% 监听器
%% ===================================================================

%% @doc 面 → handler 映射（三面：租户 / widget / 平台）。
-spec handler_of(atom()) -> module().
handler_of(platform) -> cs_platform_handler;
handler_of(widget) -> cs_widget_handler;
handler_of(_Tenant) -> cs_tenant_handler.

%% @doc 按面 + 动作起监听器：从真路由表取该动作的 path 与 Opts，叠加注入
%% （auth_facts 换成 cs_fake_facts；current_uid/adm_user_id 扮演中间件）。
-spec listener_for(atom(), atom(), map()) -> {ok, atom(), integer()}.
listener_for(Surface, Action, Inject) ->
    {Pattern, Opts} = route_opt(Surface, Action),
    PatternBin = to_bin(Pattern),
    Handler = handler_of(Surface),
    Name = list_to_atom("cs02_http_" ++ integer_to_list(erlang:unique_integer([positive]))),
    Dispatch = cowboy_router:compile([
        {'_', [{binary_to_list(PatternBin), Handler, maps:merge(Opts, Inject)}]}
    ]),
    case cowboy:start_clear(Name, [{port, 0}], #{env => #{dispatch => Dispatch}}) of
        {ok, _Pid} -> {ok, Name, ranch:get_port(Name)};
        {error, Reason} -> erlang:error({listener_failed, Reason})
    end.

%% @doc 起监听器 → 执行 → 必定停掉（端口不泄漏）。
-spec with_listener(atom(), atom(), map(), fun((integer()) -> term())) -> term().
with_listener(Surface, Action, Inject, Fun) ->
    {ok, Name, Port} = listener_for(Surface, Action, Inject),
    try
        Fun(Port)
    after
        stop(Name)
    end.

-spec stop(atom()) -> ok.
stop(Name) ->
    _ = cowboy:stop_listener(Name),
    ok.

%% ===================================================================
%% 路由表访问
%% ===================================================================

-spec route_opt(atom(), atom()) -> {binary(), map()}.
route_opt(Surface, Action) ->
    Handler = handler_of(Surface),
    Matches = [
        {b(Path), Opts}
     || {Path, H, Opts} <- cs_routes(all),
        H =:= Handler,
        maps:get(action, Opts, undefined) =:= Action
    ],
    case Matches of
        [Found] -> Found;
        [] -> erlang:error({route_not_registered, Surface, Action});
        _Many -> erlang:error({route_registered_more_than_once, Surface, Action})
    end.

%% @doc 用真路由 pattern 拼出实际请求路径（`:name` → 给定取值）。
-spec path(atom(), atom(), map()) -> binary().
path(Surface, Action, Bindings) ->
    {Pattern, _Opts} = route_opt(Surface, Action),
    maps:fold(
        fun(Name, Value, Acc) ->
            re:replace(
                Acc,
                <<":", (atom_to_binary(Name, utf8))/binary>>,
                to_bin(Value),
                [{return, binary}]
            )
        end,
        Pattern,
        Bindings
    ).

%% @doc 客服面路由（租户 + widget + 平台），从真路由表筛出。
-spec cs_routes(atom()) -> [{binary(), module(), map()}].
cs_routes(_Scope) ->
    [{_Host, Routes}] = imboy_router:get_routes(),
    [
        {b(Path), H, Opts}
     || {Path, H, Opts} <- Routes,
        H =:= cs_tenant_handler orelse H =:= cs_platform_handler orelse H =:= cs_widget_handler
    ].

%% ===================================================================
%% State 便捷构造（对应三类典型主体；凭证在头/会话键里）
%% ===================================================================

seat_state(OrgId, Uid) ->
    #{organization_id => OrgId, current_uid => Uid}.

visit_state(OrgId, Uid) ->
    #{organization_id => OrgId, current_uid => Uid}.

admin_state(OrgId, Uid) ->
    #{organization_id => OrgId, current_uid => Uid}.

platform_state(OrgId, AdmId) ->
    #{organization_id => OrgId, adm_user_id => AdmId}.

%% ===================================================================
%% 原始 HTTP
%% ===================================================================

-spec request(integer(), binary(), binary(), term()) -> map().
request(Port, Method, Path0, Body) ->
    request(Port, Method, Path0, Body, #{}).

-spec request(integer(), binary(), binary(), term(), map()) -> map().
request(Port, Method, Path0, Body, Headers0) ->
    BodyBin = encode_body(Body),
    Headers = maps:merge(
        #{
            <<"host">> => <<"localhost">>,
            <<"connection">> => <<"close">>,
            <<"content-length">> => integer_to_binary(byte_size(BodyBin))
        },
        Headers0
    ),
    HeaderBin = iolist_to_binary([
        [K, <<": ">>, V, <<"\r\n">>]
     || {K, V} <- maps:to_list(Headers)
    ]),
    Req = iolist_to_binary([
        Method, <<" ">>, Path0, <<" HTTP/1.1\r\n">>, HeaderBin, <<"\r\n">>, BodyBin
    ]),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], ?TIMEOUT),
    ok = gen_tcp:send(Socket, Req),
    Raw = recv_all(Socket, []),
    ok = gen_tcp:close(Socket),
    parse(Raw).

encode_body(Body) when is_binary(Body) -> Body;
encode_body(Body) when is_map(Body) -> jsx:encode(Body).

%% @doc 流式响应读取（SSE）：发请求后**限时**收字节，超时或对端关闭即返回
%% 已收内容（原始 binary）——绝不等到连接关闭（SSE 不关）。
-spec stream_request(integer(), binary(), binary(), term(), map(), integer()) -> binary().
stream_request(Port, Method, Path0, Body, Headers0, ReadMs) ->
    BodyBin = encode_body(Body),
    Headers = maps:merge(
        #{
            <<"host">> => <<"localhost">>,
            <<"connection">> => <<"close">>,
            <<"content-length">> => integer_to_binary(byte_size(BodyBin))
        },
        Headers0
    ),
    HeaderBin = iolist_to_binary([
        [K, <<": ">>, V, <<"\r\n">>]
     || {K, V} <- maps:to_list(Headers)
    ]),
    Req = iolist_to_binary([
        Method, <<" ">>, Path0, <<" HTTP/1.1\r\n">>, HeaderBin, <<"\r\n">>, BodyBin
    ]),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], ?TIMEOUT),
    ok = gen_tcp:send(Socket, Req),
    Raw = recv_until(Socket, <<>>, erlang:monotonic_time(millisecond) + ReadMs),
    ok = gen_tcp:close(Socket),
    Raw.

recv_until(Socket, Acc, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= Deadline of
        true ->
            Acc;
        false ->
            case gen_tcp:recv(Socket, 0, max(1, Deadline - Now)) of
                {ok, Data} -> recv_until(Socket, <<Acc/binary, Data/binary>>, Deadline);
                {error, closed} -> Acc;
                {error, _} -> Acc
            end
    end.

recv_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Data} -> recv_all(Socket, [Data | Acc]);
        {error, closed} -> iolist_to_binary(lists:reverse(Acc));
        {error, _Timeout} -> iolist_to_binary(lists:reverse(Acc))
    end.

%% @doc 解析原始响应：`#{status, headers, body, raw}`（定长与 chunked 都支持）。
-spec parse(binary()) -> map().
parse(Raw) ->
    case binary:split(Raw, <<"\r\n\r\n">>) of
        [Head, Body] ->
            [StatusLine | HeaderLines] = binary:split(Head, <<"\r\n">>, [global]),
            [_, StatusBin | _] = binary:split(StatusLine, <<" ">>, [global]),
            #{
                raw => Raw,
                status => binary_to_integer(StatusBin),
                headers => headers(HeaderLines),
                body => decode_chunked(Body, headers(HeaderLines))
            };
        [_Only] ->
            #{raw => Raw, status => 0, headers => #{}, body => <<>>}
    end.

%% @doc HTTP 状态码便捷读取。
-spec status(map()) -> integer().
status(#{status := Status}) ->
    Status.

headers(Lines) ->
    maps:from_list([
        {string:lowercase(K), V}
     || Line <- Lines,
        [K, V] <- [binary:split(Line, <<": ">>)],
        K =/= <<>>
    ]).

decode_chunked(Body, #{<<"transfer-encoding">> := <<"chunked">>}) ->
    chunked(Body, []);
decode_chunked(Body, _Headers) ->
    Body.

chunked(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
chunked(Bin, Acc) ->
    case binary:split(Bin, <<"\r\n">>) of
        [SizeBin, Rest] ->
            case to_int(SizeBin) of
                0 ->
                    iolist_to_binary(lists:reverse(Acc));
                Size when is_integer(Size), Size > 0 ->
                    <<Chunk:Size/binary, _CRLF:2/binary, Tail/binary>> = Rest,
                    chunked(Tail, [Chunk | Acc]);
                _Other ->
                    iolist_to_binary(lists:reverse(Acc))
            end;
        _Incomplete ->
            iolist_to_binary(lists:reverse(Acc))
    end.

to_int(Bin) ->
    try binary_to_integer(Bin, 16) of
        Int -> Int
    catch
        _:_ -> -1
    end.

%% 响应信封（elib_response）：`#{code, msg, payload}`。
-spec json(map()) -> map().
json(#{body := Body}) ->
    jsx:decode(Body, [return_maps]).

-spec code(map()) -> integer().
code(Resp) ->
    maps:get(<<"code">>, json(Resp)).

-spec msg(map()) -> binary().
msg(Resp) ->
    maps:get(<<"msg">>, json(Resp)).

-spec payload(map()) -> term().
payload(Resp) ->
    maps:get(<<"payload">>, json(Resp)).

%% ===================================================================
%% 内部
%% ===================================================================

b(Bin) when is_binary(Bin) -> Bin;
b(List) when is_list(List) -> unicode:characters_to_binary(List).

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8).
