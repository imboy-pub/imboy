-module(probe_debug).
-export([main/1]).

%% one-shot debug: open ONE gun websocket against the probe server and
%% print every message the owner receives for 3 seconds.

main(Args0) ->
    Args = [to_list(A) || A <- Args0],
    [PortS | _] = Args,
    Port = list_to_integer(PortS),
    {ok, _} = application:ensure_all_started(gun),
    Ret = gun:open("127.0.0.1", Port, #{
        protocols => [http],
        retry => 0,
        connect_timeout => 5000,
        tcp_opts => [{nodelay, true}]
    }),
    io:format("gun:open -> ~0p~n", [Ret]),
    {ok, Pid} = Ret,
    Up = gun:await_up(Pid, 5000),
    io:format("gun:await_up -> ~0p~n", [Up]),
    Ref = gun:ws_upgrade(Pid, "/api/v1/ws", [], #{}),
    io:format("gun:ws_upgrade -> ~0p~n", [Ref]),
    gun:ws_send(Pid, Ref, {text, <<"ping">>}),
    loop(3000),
    erlang:halt(0).

loop(Deadline) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true -> ok;
        false ->
            receive
                M -> io:format("MSG ~0p~n", [M]), loop(Deadline)
            after 100 -> loop(Deadline)
            end
    end.

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_binary(A) -> binary_to_list(A);
to_list(A) when is_list(A) -> A.
