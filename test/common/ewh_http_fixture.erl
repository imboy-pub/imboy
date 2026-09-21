-module(ewh_http_fixture).

%%%===================================================================
% 出站 Webhook 的**真实 socket** 对端夹具（FULL-03，test/common）。
%
% 用途：不 mock 任何网络原语地验证 sender 的 timeout / redirect / response cap
%   / 线上签名头——bot_webhook_delivery_sender 只被 meck 时会漏掉整个
%   「连出去」的语义，而 FULL-03 的安全硬门（plan-full §7）要的正是它。
%
% 用法：
%   H = ewh_http_fixture:start(fun(Req) -> {reply, 200, [], <<"ok">>} end),
%   Port = ewh_http_fixture:port(H),
%   ... bot_webhook_delivery_sender:post({127,0,0,1}, Port, false, <<"/hook">>,
%                                        <<"127.0.0.1">>, Headers, Body) ...
%   Requests = ewh_http_fixture:requests(H),   %% 收到的原始请求（逐条）
%   ewh_http_fixture:stop(H).
%
% Responder(Request) 返回：
%   {reply, Code, Headers, Body}  正常响应（自动补 content-length + connection: close）
%   {raw, IoData}                 原样写字节（大响应头/cap 负例用）
%   hang                          收下请求后**不响应**（timeout 负例）
%
% Request :: #{method, path, headers :: [{binary(), binary()}], body :: binary(),
%              raw :: binary()}
%%%===================================================================

-export([start/1, stop/1, port/1, requests/1, request_count/1]).

-record(fixture, {lsock, port, pid, table}).

-spec start(fun((map()) -> term())) -> #fixture{}.
start(Responder) when is_function(Responder, 1) ->
    {ok, LSock} = gen_tcp:listen(0, [
        binary, {active, false}, {reuseaddr, true}, {backlog, 16}
    ]),
    {ok, Port} = inet:port(LSock),
    Table = ets:new(ewh_http_fixture_reqs, [public, ordered_set]),
    Pid = spawn(fun() -> accept_loop(LSock, Responder, Table, 0) end),
    #fixture{lsock = LSock, port = Port, pid = Pid, table = Table}.

-spec stop(#fixture{}) -> ok.
stop(#fixture{lsock = LSock, pid = Pid, table = Table}) ->
    _ = (catch gen_tcp:close(LSock)),
    _ = (catch exit(Pid, kill)),
    _ = (catch ets:delete(Table)),
    ok.

-spec port(#fixture{}) -> inet:port_number().
port(#fixture{port = Port}) -> Port.

-spec requests(#fixture{}) -> [map()].
requests(#fixture{table = Table}) ->
    [Req || {_, Req} <- lists:sort(ets:tab2list(Table))].

-spec request_count(#fixture{}) -> non_neg_integer().
request_count(#fixture{table = Table}) -> ets:info(Table, size).

%%%===================================================================
%%% Internal
%%%===================================================================

accept_loop(LSock, Responder, Table, N) ->
    case gen_tcp:accept(LSock, 30000) of
        {ok, Sock} ->
            _ = handle(Sock, Responder, Table, N),
            accept_loop(LSock, Responder, Table, N + 1);
        {error, closed} ->
            ok;
        {error, timeout} ->
            accept_loop(LSock, Responder, Table, N);
        {error, _} ->
            ok
    end.

handle(Sock, Responder, Table, N) ->
    case read_request(Sock, <<>>) of
        {ok, Raw} ->
            Req = parse_request(Raw),
            ets:insert(Table, {N, Req}),
            _ = respond(Responder(Req), Sock),
            close_quiet(Sock);
        {error, _} ->
            close_quiet(Sock)
    end.

read_request(Sock, Acc) ->
    %% 只读到请求头结束；再按 content-length 读正文（夹具用于断言线上签名）。
    case read_until_headers(Sock, Acc) of
        {ok, HeadBin, Rest} ->
            Len = content_length(HeadBin),
            read_body(Sock, Rest, Len, HeadBin);
        {error, Reason} ->
            {error, Reason}
    end.

read_until_headers(Sock, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        {Pos, 4} ->
            {ok, binary:part(Acc, 0, Pos + 4), binary:part(Acc, Pos + 4, byte_size(Acc) - Pos - 4)};
        nomatch when byte_size(Acc) < 262144 ->
            case gen_tcp:recv(Sock, 0, 5000) of
                {ok, Data} -> read_until_headers(Sock, <<Acc/binary, Data/binary>>);
                {error, Reason} -> {error, Reason}
            end;
        nomatch ->
            {error, head_too_large}
    end.

read_body(_Sock, Acc, Len, HeadBin) when byte_size(Acc) >= Len ->
    {ok, <<HeadBin/binary, (binary:part(Acc, 0, Len))/binary>>};
read_body(Sock, Acc, Len, HeadBin) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Data} -> read_body(Sock, <<Acc/binary, Data/binary>>, Len, HeadBin);
        {error, Reason} -> {error, Reason}
    end.

parse_request(Raw) ->
    [HeadBin, Body] = split_once(Raw, <<"\r\n\r\n">>),
    [RequestLine | HeaderLines] = binary:split(HeadBin, <<"\r\n">>, [global]),
    [Method, Path | _] = binary:split(RequestLine, <<" ">>, [global]),
    Headers = [
        {lower(K), trim(V)}
     || Line <- HeaderLines,
        {K, V} <- [split_once(Line, <<":">>)],
        K =/= <<>>
    ],
    #{
        method => Method,
        path => Path,
        headers => Headers,
        body => Body,
        raw => Raw
    }.

split_once(Bin, Sep) ->
    case binary:split(Bin, Sep) of
        [A, B] -> [A, B];
        [A] -> [A, <<>>]
    end.

lower(B) -> list_to_binary(string:lowercase(binary_to_list(trim(B)))).

trim(B) ->
    list_to_binary(string:trim(binary_to_list(B))).

content_length(HeadBin) ->
    case re:run(HeadBin, <<"content-length:\\s*(\\d+)">>, [caseless, {capture, [1], binary}]) of
        {match, [LenBin]} ->
            try binary_to_integer(LenBin) of
                N when N >= 0 -> N
            catch
                _:_ -> 0
            end;
        _ ->
            0
    end.

respond({reply, Code, Headers, Body}, Sock) ->
    Hdrs =
        Headers ++
            [
                {<<"content-length">>, integer_to_binary(byte_size(Body))},
                {<<"connection">>, <<"close">>}
            ],
    send(Sock, [
        <<"HTTP/1.1 ">>,
        integer_to_binary(Code),
        <<" OK\r\n">>,
        [[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- Hdrs],
        <<"\r\n">>,
        Body
    ]);
respond({raw, IoData}, Sock) ->
    send(Sock, IoData);
respond(hang, _Sock) ->
    ok;
respond(_, _Sock) ->
    ok.

send(Sock, Data) ->
    _ = (catch gen_tcp:send(Sock, Data)),
    ok.

close_quiet(Sock) ->
    _ = (catch gen_tcp:close(Sock)),
    ok.
