%% Single-owner liveness is proven by a nonce-bound loopback challenge.
-module(imboy_quality_task_owner).
-export([start/2, stop/1, shutdown/2, verify/3, challenge/3]).

start(Token, Worktree) when
    is_binary(Token),
    byte_size(Token) =:= 64,
    is_binary(Worktree),
    byte_size(Worktree) > 0
->
    case token_valid(Token) of
        true -> start_valid(Token, Worktree);
        false -> {error, invalid_input}
    end;
start(_, _) ->
    {error, invalid_input}.

start_valid(Token, Worktree) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() -> init(Parent, Ref, Token, Worktree) end),
    receive
        {Ref, Result} -> Result
    after 3000 ->
        exit(Pid, kill),
        {error, start_timeout}
    end.

init(Parent, Ref, Token, Worktree) ->
    case
        gen_tcp:listen(0, [
            binary,
            {ip, {127, 0, 0, 1}},
            {active, false},
            {packet, 4},
            {packet_size, 4096},
            {reuseaddr, false}
        ])
    of
        {ok, L} ->
            ParentMonitor = erlang:monitor(process, Parent),
            {ok, Port} = inet:port(L),
            {ok, Host} = inet:gethostname(),
            Start = string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(32))),
            Identity = #{
                <<"host">> => unicode:characters_to_binary(Host),
                <<"pid">> => list_to_integer(os:getpid()),
                <<"start_identity">> => Start,
                <<"owner_port">> => Port,
                <<"token">> => Token
            },
            Parent ! {Ref, {ok, self(), Identity}},
            loop(L, Token, Worktree, Identity, #{}, ParentMonitor);
        {error, Reason} ->
            Parent ! {Ref, {error, Reason}}
    end.

loop(L, Token, Worktree, Identity, Seen, ParentMonitor) ->
    receive
        stop -> gen_tcp:close(L);
        {'DOWN', ParentMonitor, process, _, _} -> gen_tcp:close(L)
    after 0 ->
        case gen_tcp:accept(L, 200) of
            {ok, Socket} ->
                handle(Socket, Token, Worktree, Identity, Seen, L, ParentMonitor);
            {error, timeout} ->
                loop(L, Token, Worktree, Identity, Seen, ParentMonitor);
            {error, closed} ->
                ok
        end
    end.

handle(Socket, Token, Worktree, Identity, Seen, L, ParentMonitor) ->
    Action =
        try
            {ok, Data} = gen_tcp:recv(Socket, 0, 1000),
            {Kind, Nonce, Mac} = binary_to_term(Data, [safe]),
            true = Kind =:= request orelse Kind =:= shutdown,
            true = is_binary(Nonce) andalso byte_size(Nonce) =:= 32,
            false = maps:is_key(Nonce, Seen),
            true = map_size(Seen) < 4096,
            true = crypto:hash_equals(Mac, mac(Token, Kind, Nonce, Worktree, Identity)),
            Reply = mac(Token, {response, Kind}, Nonce, Worktree, Identity),
            ok = gen_tcp:send(Socket, term_to_binary({response, Reply})),
            {Kind, Seen#{Nonce => true}}
        catch
            _:_ -> {request, Seen}
        end,
    gen_tcp:close(Socket),
    case Action of
        {shutdown, _} -> gen_tcp:close(L);
        {request, Next} -> loop(L, Token, Worktree, Identity, Next, ParentMonitor)
    end.

stop(Pid) when is_pid(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 3000 -> {error, stop_timeout}
    end.

verify(State, Supplied, Repo) ->
    try
        #{
            <<"owner">> := Identity = #{
                <<"token">> := Token,
                <<"host">> := Host,
                <<"pid">> := Pid,
                <<"start_identity">> := Start,
                <<"owner_port">> := Port
            },
            <<"worktree">> := Repo
        } = State,
        true = token_valid(Token),
        true = is_binary(Supplied) andalso crypto:hash_equals(Token, Supplied),
        true = is_binary(Repo) andalso byte_size(Repo) > 0,
        true = is_integer(Pid) andalso Pid > 0,
        true = is_binary(Start) andalso byte_size(Start) >= 32,
        true = is_integer(Port) andalso Port > 0 andalso Port < 65536,
        {ok, LocalHost} = inet:gethostname(),
        true = short_host(binary_to_list(Host)) =:= short_host(LocalHost),
        challenge(Identity, Repo, crypto:strong_rand_bytes(32))
    catch
        _:_ -> false
    end.

%% Exported for deterministic replay-negative tests; production always generates a fresh nonce.
challenge(Identity, Worktree, Nonce) ->
    exchange(request, Identity, Worktree, Nonce).

shutdown(Identity, Worktree) ->
    exchange(shutdown, Identity, Worktree, crypto:strong_rand_bytes(32)).

exchange(Kind, Identity, Worktree, Nonce) ->
    try
        Token = maps:get(<<"token">>, Identity),
        true = token_valid(Token),
        Port = maps:get(<<"owner_port">>, Identity),
        {ok, S} = gen_tcp:connect(
            {127, 0, 0, 1},
            Port,
            [binary, {active, false}, {packet, 4}, {packet_size, 4096}],
            1000
        ),
        try
            Req = {Kind, Nonce, mac(Token, Kind, Nonce, Worktree, Identity)},
            ok = gen_tcp:send(S, term_to_binary(Req)),
            {ok, Data} = gen_tcp:recv(S, 0, 1000),
            {response, Reply} = binary_to_term(Data, [safe]),
            crypto:hash_equals(Reply, mac(Token, {response, Kind}, Nonce, Worktree, Identity))
        after
            gen_tcp:close(S)
        end
    catch
        _:_ -> false
    end.

mac(Token, Kind, Nonce, Worktree, Identity) ->
    Binding =
        {imboy_quality_owner_v1, Kind, Nonce, Worktree, maps:get(<<"host">>, Identity),
            maps:get(<<"pid">>, Identity), maps:get(<<"start_identity">>, Identity),
            maps:get(<<"owner_port">>, Identity)},
    crypto:mac(hmac, sha256, Token, term_to_binary(Binding)).

short_host(Host) -> hd(string:split(Host, ".", all)).

token_valid(Token) when is_binary(Token), byte_size(Token) =:= 64 ->
    lists:all(
        fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end,
        binary_to_list(Token)
    );
token_valid(_) ->
    false.
