-module (wsc1).

-export ([start/2]).
-export ([ws_start/2]).

%

% erl +Q 1048576 +P 1048576 -env ERL_MAX_PORTS 1048576 -env ERTS_MAX_PORTS 1048576 -pa ../ebin ../deps/cowlib/ebin
% netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'

% 46340
% c(wsc1).
% wsc1:start(1, 65000).
% wsc1:start(65000, 65000 + 65000).
% wsc1:start(130000, 130000 + 65000).
% wsc1:start(195000, 195000 + 65000).
% wsc1:start(260000, 260000 + 65000).

% wsc1:start(325000, 325000 + 65000).
% wsc1:start(390000, 390000 + 65000).
% wsc1:start(455000, 455000 + 65000).
% wsc1:start(520000, 520000 + 65000).
% wsc1:start(585000, 585000 + 65000).

% wsc1:start(650000, 650000 + 65000).
% wsc1:start(715000, 715000 + 65000).
% wsc1:start(780000, 780000 + 65000).
% wsc1:start(845000, 845000 + 65000).
% wsc1:start(910000, 910000 + 65000).

% wsc1:start(975000, 975000 + 65000).
% wsc1:start(1040000, 1040000 + 65000).

%%  erlang:system_info(process_limit).
% erlang:system_info(process_count).
% erlang:memory().
% length(chat_store_repo:lookall()).

-include("common.hrl").

start(Begin, End) ->
    % code:add_path("../ebin"),
    Max = erlang:system_info(process_limit),
    io:format("Maxmium allowed process is ~p ~n", [Max]),
    statistics(runtime),
    statistics(wall_clock),
    for(Begin, End, fun(Begin) ->
            receive
            after
                1 ->
                    spawn(wsc1, ws_start, [Begin, 0])
            end
    end),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    % lists:foreach(fun(Pid) -> Pid ! die end, L),
    U1 = Time1 * 1000 / End,
    U2 = Time2 * 1000 /End,
    io:format("Process spawn time=~p (~p) microseconds ~n", [U1, U2]).

for(N, N, F) -> [F(N)];
for(I, N, F) -> [F(I)|for(I+1, N, F)].

ws_start(Id, Index) when Index > 1000 ->
    ?LOG([Id, 'over 1000']),
    ok;
ws_start(Id, Index) ->
    % Host = <<"localhost">>,
    % Port = <<"9800">>,
    % Host = <<"demo.imboy.leeyi.net">>,
    % Port = <<"80">>,

    Host = <<"119.23.238.99">>,
    Port = <<"80">>,

    % Host = <<"local.imboy.leeyi.net">>,
    % Port = <<"9800">>,
    % CPid = self(),
    try
        case gen_tcp:connect(binary_to_list(Host) , list_to_integer(binary_to_list(Port)), [{packet, 0}, {active, false}, {reuseaddr, true}]) of
                {ok, Socket} ->
                    gen_tcp:controlling_process(Socket, self()),
                    Token = token_ds:encrypt_token(Id),
                    Token2 = binary:replace(Token, <<"+">>, <<"%2B">>, [global]),
                    % Token2 = list_to_binary(integer_to_list(Id)),
                    Header = [
                        <<"GET /stress_testing?token=", Token2/binary, " HTTP/1.1\r\n">>,
                        % <<"GET /websocket/?token=", Token2/binary, " HTTP/1.1\r\n">>,
                        <<"Host: ", Host/binary, ":", Port/binary, "\r\n">>,
                        <<"Connection: Upgrade\r\n">>,
                        <<"Pragma: no-cache\r\n">>,
                        <<"Cache-Control: no-cache\r\n">>,
                        <<"Upgrade: websocket\r\n">>,
                        <<"Origin: http://", Host/binary, ":", Port/binary, "\r\n">>,
                        <<"Sec-WebSocket-Protocol: text\r\n">>,
                        <<"Sec-WebSocket-Version: 13\r\n">>,
                        <<"User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36\r\n">>,
                        <<"Accept-Encoding: gzip, deflate, sdch\r\n">>,
                        <<"Accept-Language: zh-CN,zh;q=0.8\r\n">>,
                        <<"Cookie: location_area_code=; location_name=\r\n">>,
                        <<"Sec-WebSocket-Key: u5uqxRXPut2megmbeLqEsQ==\r\n">>,
                        <<"Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits\r\n">>,
                        <<"\r\n">>],
                    % ?LOG([Id, Header]),
                    gen_tcp:send(Socket, Header),
                    % gen_tcp:send(Socket, cow_ws:masked_frame(ping, "")),
                    loop(Socket, Id);
                {error, Reason} ->
                    throw({connect_error, Reason})
        end
    of
        _ ->
         ok
    catch
        _:_ ->
            ok
        % ErrCode2:ErrorMsg2 ->
        %     ?LOG(["ws_start try catch: ", ErrCode2, ErrorMsg2, Id, Index, self(), CPid])
    end,
    % erlang:garbage_collect(self()),
    timer:sleep(3000 + Index * 100),
    ws_start(Id, Index + 1).

loop(Socket, Id) ->
    receive
    after
        5000 ->
            case gen_tcp:recv(Socket, 0) of
                {ok, _Msg} ->
                    gen_tcp:send(Socket, cow_ws:masked_frame(pong, ""));
                {error,closed} ->
                    gen_tcp:close(Socket),
                    throw({closed, Socket, Id});
                Err ->
                    ?LOG(Err),
                    throw(Err)
            end
    end,
    receive
    after
        5000 ->
            gen_tcp:send(Socket, cow_ws:masked_frame(ping, ""))
    end,
    loop(Socket, Id).
