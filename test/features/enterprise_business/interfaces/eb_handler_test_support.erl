%%% @doc EB-09 handler 套件的测试支撑（test-only，不进任何 release）。
%%%
%%% 提供三件事：
%%%
%%%   1. **真 HTTP**：在临时端口（`{port, 0}`，天然串行、不撞其它 worker 的监听）
%%%      起一个只用**单条真路由**的监听器，用**真 cowboy route Opts**（从
%%%      `imboy_router:get_routes/0` 取，含 EB-09 注入的 surface/feature/auth_facts）
%%%      加上本测试注入的会话键（`current_uid` / `adm_user_id`）与事实装配。
%%%      为什么注入会话键：`current_uid` / `adm_user_id` 在**生产**由
%%%      `auth_middleware_api_v1` / `adm_auth_middleware` 写入 `handler_opts`，
%%%      是中间件的职责、不是 handler 的职责；本套件测的是 handler 及其下游
%%%      （真 facade + 真 PG），故由测试扮演中间件的注入角色，并**不**伪造凭证语义
%%%      （handler 仍按 `credential_class` 校验类别）。
%%%   2. **请求**：裸 TCP 发原始 HTTP（带 Content-Length 的定长请求），按
%%%      `content-length` 读取定长响应（`reply_content/2` 也固定带 content-length）。
%%%   3. **合成租户**：直接复用 EB-03 的 `eb_pg_test_fixture`（随机 TSID 隔离，
%%%      不 TRUNCATE、不碰共享库的既有行）与最小 SQL 断言辅助。
%%%
%%% 不含任何业务逻辑；不做断言（断言在套件里）。
-module(eb_handler_test_support).

-export([
    listener/2,
    listener/3,
    listener_for/3,
    stop/1,
    request/3,
    request/4,
    request/5,
    parse/1,
    json/1,
    code/1,
    msg/1,
    payload/1,
    raw/1,
    route_opt/2,
    path/3,
    with_listener/4,
    enterprise_routes/1,
    set_session/1,
    facts/1,
    real_facts/0,
    sql/2,
    scalar/2,
    scalar/3
]).

-define(TIMEOUT, 15000).

%% ===================================================================
%% 监听器
%% ===================================================================

%% @doc 起一个只用单条路由的临时监听器；`Opts` 即 handler 的初始 State。
-spec listener(binary(), map()) -> {ok, atom(), integer()} | {error, term()}.
listener(Pattern, Opts) ->
    listener(Pattern, Opts, handler(Opts)).

%% @doc 同上，但显式指定 handler 模块（供响应构造器的直接探测使用）。
-spec listener(binary(), map(), module()) -> {ok, atom(), integer()} | {error, term()}.
listener(Pattern, Opts, Handler) ->
    Name = listener_name(),
    Dispatch = cowboy_router:compile([{'_', [{binary_to_list(Pattern), Handler, Opts}]}]),
    case cowboy:start_clear(Name, [{port, 0}], #{env => #{dispatch => Dispatch}}) of
        {ok, _Pid} -> {ok, Name, ranch:get_port(Name)};
        {error, Reason} -> {error, Reason}
    end.

handler(#{surface := platform}) -> eb_platform_handler;
handler(_Opts) -> eb_tenant_handler.

%% @doc 按**动作**起监听器：从真路由表取该动作的 path 与 Opts，再叠加会话/装配注入。
-spec listener_for(atom(), map(), map()) -> {ok, atom(), integer()}.
listener_for(Surface, Action, Inject) ->
    {Pattern, Opts} = route_opt(Surface, Action),
    listener(Pattern, maps:merge(Opts, Inject)).

-spec stop(atom()) -> ok.
stop(Name) ->
    _ = cowboy:stop_listener(Name),
    ok.

listener_name() ->
    list_to_atom("eb09_http_" ++ integer_to_list(erlang:unique_integer([positive]))).

%% ===================================================================
%% 真路由表访问
%% ===================================================================

%% @doc 从 `imboy_router:get_routes/0` 取该面上指定动作的 `{Pattern, Opts}`。
%% 找不到即 crash（路由漂移必须立刻可见）。
-spec route_opt(atom(), atom()) -> {binary(), map()}.
route_opt(Surface, Action) ->
    Matches = [
        {Path, Opts}
     || {Path, H, Opts} <- enterprise_routes(Surface),
        H =:= handler_for(Surface),
        maps:get(action, Opts, undefined) =:= Action
    ],
    case Matches of
        [Found] -> Found;
        [] -> erlang:error({route_not_registered, Surface, Action});
        _Many -> erlang:error({route_registered_more_than_once, Surface, Action})
    end.

handler_for(platform) -> eb_platform_handler;
handler_for(_Surface) -> eb_tenant_handler.

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

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8).

%% @doc 起监听器 → 执行 → 必定停掉监听器（端口不泄漏）。
-spec with_listener(atom(), atom(), map(), fun((integer()) -> term())) -> term().
with_listener(Surface, Action, Inject, Fun) ->
    {ok, Name, Port} = listener_for(Surface, Action, Inject),
    try
        Fun(Port)
    after
        stop(Name)
    end.

%% cowboy 路由表里的 path 是 Erlang **字符串**（list）；本套件统一转成 binary，
%% 使下游的 binary 匹配 / re 匹配 / 拼接都成立。
b(Bin) when is_binary(Bin) -> Bin;
b(List) when is_list(List) -> unicode:characters_to_binary(List).

%% @doc 企业面路由（租户 + 平台），从真路由表筛出。
-spec enterprise_routes(atom() | all) -> [{binary(), module(), map()}].
enterprise_routes(Scope) ->
    [{_Host, Routes}] = imboy_router:get_routes(),
    [
        {b(Path), H, Opts}
     || {Path, H, Opts} <- Routes,
        is_enterprise_handler(H, Scope)
    ].

is_enterprise_handler(eb_tenant_handler, all) -> true;
is_enterprise_handler(eb_platform_handler, all) -> true;
is_enterprise_handler(H, tenant) -> H =:= eb_tenant_handler;
is_enterprise_handler(H, platform) -> H =:= eb_platform_handler;
is_enterprise_handler(_H, _Scope) -> false.

%% ===================================================================
%% 会话 / 事实注入（扮演生产中间件与装配的角色）
%% ===================================================================

%% @doc 注入会话键：租户面 `current_uid`，平台面 `adm_user_id`。
-spec set_session(map()) -> map().
set_session(Inject) ->
    Inject.

%% @doc 选择事实装配：`real`（生产装配）/ `{probe, Permissions}`（测试装配，
%% 见 `eb09_facts_probe`）。
-spec facts(term()) -> module().
facts(real) -> real_facts();
facts({probe, _Permissions}) -> eb09_facts_probe.

real_facts() -> eb_pg_auth_facts.

%% ===================================================================
%% 原始 HTTP
%% ===================================================================

-spec request(integer(), binary(), binary()) -> map().
request(Port, Method, Path) ->
    request(Port, Method, Path, <<>>, #{}).

-spec request(integer(), binary(), binary(), term()) -> map().
request(Port, Method, Path, Body) ->
    request(Port, Method, Path, Body, #{}).

-spec request(integer(), binary(), binary(), term(), map()) -> map().
request(Port, Method, Path, Body, Headers0) ->
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
        Method, <<" ">>, Path, <<" HTTP/1.1\r\n">>, HeaderBin, <<"\r\n">>, BodyBin
    ]),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], ?TIMEOUT),
    ok = gen_tcp:send(Socket, Req),
    Raw = recv_all(Socket, []),
    ok = gen_tcp:close(Socket),
    parse(Raw).

encode_body(Body) when is_binary(Body) -> Body;
encode_body(Body) when is_map(Body) -> jsx:encode(Body);
encode_body(Body) when is_list(Body) -> jsx:encode(Body).

recv_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Data} -> recv_all(Socket, [Data | Acc]);
        {error, closed} -> iolist_to_binary(lists:reverse(Acc));
        {error, _Timeout} -> iolist_to_binary(lists:reverse(Acc))
    end.

%% @doc 解析原始响应：`#{status, headers, body, raw}`。支持定长与 chunked。
-spec parse(binary()) -> map().
parse(Raw) ->
    case binary:split(Raw, <<"\r\n\r\n">>) of
        [Head, Body] ->
            [StatusLine | HeaderLines] = binary:split(Head, <<"\r\n">>, [global]),
            [_, StatusBin | _] = binary:split(StatusLine, <<" ">>, [global]),
            Headers = headers(HeaderLines),
            #{
                raw => Raw,
                status => binary_to_integer(StatusBin),
                headers => Headers,
                body => decode_chunked(Body, Headers)
            };
        [_Only] ->
            #{raw => Raw, status => 0, headers => #{}, body => <<>>}
    end.

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

-spec json(map()) -> map().
json(#{body := Body}) ->
    jsx:decode(Body, [return_maps]).

%% 响应信封（`elib_response`）：`#{code, msg, payload}`。
-spec code(map()) -> integer().
code(Resp) ->
    maps:get(<<"code">>, json(Resp)).

-spec msg(map()) -> binary().
msg(Resp) ->
    maps:get(<<"msg">>, json(Resp)).

-spec payload(map()) -> term().
payload(Resp) ->
    maps:get(<<"payload">>, json(Resp)).

-spec raw(map()) -> binary().
raw(Resp) ->
    maps:get(raw, Resp).

%% ===================================================================
%% 合成租户 / SQL
%% ===================================================================

-spec sql(binary(), [term()]) -> ok.
sql(Sql, Params) ->
    ok = eb_pg_test_fixture:exec(Sql, Params).

-spec scalar(term(), iodata()) -> term().
scalar(Default, Sql) ->
    scalar(Default, Sql, []).

-spec scalar(term(), iodata(), [term()]) -> term().
scalar(Default, Sql, Params) ->
    case eb_pg_test_fixture:scalar(Sql, Params) of
        undefined -> Default;
        null -> Default;
        Value -> Value
    end.
