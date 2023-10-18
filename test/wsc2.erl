-module(wsc2).
-behavior(gen_server).

-export([start_link/1,
         init/1,
         terminate/2,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         code_change/3]).

-export ([start/2]).
-export ([start_link/0]).
-export ([connect_ws/3]).

-export ([stop/0]).
-export ([stop/1]).
-export ([stop/2]).
% cd(test). c(cow_ws). c(wsc2).

% wsc2:start(83235, 83235 + 12000)
% wsc2:start(113235, 113235 + 30000).
% wsc2:start(113235, 113235 + 60000).

% wsc2:stop(83235, 83235 + 12000)
% wsc2:stop(113235, 113235 + 60000).

-include_lib("imlib/include/log.hrl").

start_link() ->
    start_link({1, 0, {}}).

stop() ->
    gen_server:call(?MODULE, stop).

stop(Id) ->
    gen_server:call(sname(Id), stop).

stop(Begin, End) ->
    for(Begin, End, fun(Begin1) ->
        stop(Begin1)
    end).

sname(Id) ->
    BinId = list_to_binary(integer_to_list(Id)),
    M = <<"wsc2_", BinId/binary>>,
    binary_to_atom(M, latin1).

start_link({Id, Index, Ip}) ->
    gen_server:start_link({local, sname(Id)}, ?MODULE, {Id, Index, Ip}, []);
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init({Id, Index, Ip}) ->
    % ?LOG({Id, Index, Ip}),
    case connect_ws(Id, Index, Ip) of
        {ok, Socket, Id, Index} ->
            gen_tcp:controlling_process(Socket, self()),
            gen_server:cast(sname(Id), {receive_msg, Socket, Id, Index, Ip});
        {Id, Index} ->
            timer:sleep(3000 + Index * 100),
            init({Id, Index + 1, Ip})
    end,
    {ok, []};
init(Args) ->
    ?LOG(Args),
    {ok, []}.

handle_info(Info, State) ->
    ?LOG([Info, State]),
    {noreply, State}.

handle_cast({receive_msg, Socket, Id, Index, Ip}, State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, _Msg} ->
            gen_tcp:send(Socket, cow_ws:masked_frame(pong, "")),
            gen_server:cast(sname(Id), {receive_msg, Socket, Id, Index, Ip});
        {error, closed} ->
            gen_tcp:close(Socket),
            timer:sleep(3000 + Index * 100),
            init({Id, Index + 1, Ip});
        Err ->
            ?LOG(Err),
            timer:sleep(3000 + Index * 100),
            init({Id, Index + 1, Ip})
    end,
    {noreply, State, hibernate};
handle_cast(Msg, State) ->
    ?LOG([Msg, State]),
    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(Msg, From, State) ->
    ?LOG([Msg, From, State]),
    {reply, ok, State}.

code_change(OldVsn, State, Extra) ->
    ?LOG([OldVsn, State, Extra]),
    {ok, State}.

terminate(Reason, Data) ->
    ?LOG([Reason, Data]),
    ok.

start(Begin, End) ->
    % code:add_path("../ebin"),
    L = for(Begin, End, fun(Begin1) ->
            receive
            after
                5 ->
                    start_link({Begin1, 0, {}})
            end
    end),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    ?LOG(L),
    U1 = Time1 * 1000 / End,
    U2 = Time2 * 1000 /End,
    io:format("Process spawn time=~p (~p) microseconds ~n", [U1, U2]).

for(N, N, F) -> [F(N)];
for(I, N, F) -> [F(I)|for(I+1, N, F)].

-spec connect_ws(Id::integer(), Index::integer(), Ip::any()) ->
    {ok, Socket::any(), Id::integer(), Index::integer()} |
    {Id::integer(), Index::integer()}.
connect_ws(Id, Index, _Ip) ->
    % Host = <<"localhost">>,
    % Port = <<"9800">>,
    Host = <<"demo.imboy.leeyi.net">>,
    Port = <<"80">>,
    % Host = <<"local.imboy.leeyi.net">>,
    % Port = <<"9800">>,
    Opts = [{packet, 0}, {active, false}, {reuseaddr, true}],
    case gen_tcp:connect(binary_to_list(Host) , list_to_integer(binary_to_list(Port)), Opts) of
            {ok, Socket} ->
                gen_tcp:controlling_process(Socket, self()),
                % Token = token_ds:encrypt_token(Id),
                % Token2 = binary:replace(Token, <<"+">>, <<"%2B">>, [global]),
                Token2 = list_to_binary(integer_to_list(Id)),
                Header = [
                    % <<"GET /websocket/?token=", Token2/binary, " HTTP/1.1\r\n">>,
                    <<"GET /test_ws/?current_uid=", Token2/binary, " HTTP/1.1\r\n">>,
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
                % ?LOG([Id, Token, Header]),
                gen_tcp:send(Socket, Header),
                {ok, Socket, Id, Index};
            {error, Reason} ->
                ?LOG([error, Reason, Id, Index]),
                {Id, Index}
    end.
