-module(stats_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req =
        case cowboy_req:path_info(Req0) of
            [<<"stop">>] ->
                spawn(fun() -> timer:sleep(150), erlang:halt(0) end),
                reply(Req0, <<"stopping\n">>);
            _ ->
                reply(Req0, stats())
        end,
    {ok, Req, State}.

reply(Req, Body) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req).

stats() ->
    Ref = probe,
    S = ranch:info(Ref),
    MaxConns = ranch:get_max_connections(Ref),
    SpawnProbe =
        try
            spawn(fun() -> ok end) of
                P when is_pid(P) -> <<"ok">>
        catch
            C:R -> iolist_to_binary(io_lib:format("~p:~0p", [C, R]))
        end,
    iolist_to_binary(
        [
            <<"{\"process_count\":">>, int(erlang:system_info(process_count)),
            <<",\"port_count\":">>, int(erlang:system_info(port_count)),
            <<",\"process_limit\":">>, int(erlang:system_info(process_limit)),
            <<",\"port_limit\":">>, int(erlang:system_info(port_limit)),
            <<",\"schedulers_online\":">>, int(erlang:system_info(schedulers_online)),
            <<",\"memory_total\":">>, int(erlang:memory(total)),
            <<",\"active_connections\":">>, int(maps:get(active_connections, S, -1)),
            <<",\"all_connections\":">>, int(maps:get(all_connections, S, -1)),
            <<",\"max_connections_per_conns_sup\":">>, term_enc(MaxConns),
            <<",\"conns_sups\":">>, int(length(ranch_server:get_connections_sups(Ref))),
            <<",\"acceptors\":">>, int(length(ranch:procs(Ref, acceptors))),
            <<",\"spawn_probe\":">>, term_enc(SpawnProbe),
            <<",\"probe_counters\":">>, counters(),
            <<",\"initial_calls\":">>, calls(),
            <<"}\n">>
        ]
    ).

counters() ->
    case ets:whereis(probe_counters) of
        undefined -> ets:new(probe_counters, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    Joined = lists:join(<<",">>, [
        [atom_to_binary(K, utf8), <<":">>, integer_to_binary(V)]
     || {K, V} <- lists:sort(ets:tab2list(probe_counters))
    ]),
    [<<"{">>, Joined, <<"}">>].

calls() ->
    Counts = lists:foldl(
        fun(P, Acc) ->
            case process_info(P, dictionary) of
                {dictionary, Dic} ->
                    case lists:keyfind('$initial_call', 1, Dic) of
                        {'$initial_call', {M, _, _}} ->
                            maps:update_with(M, fun(V) -> V + 1 end, 1, Acc);
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end,
        #{},
        processes()
    ),
    Top = lists:sublist(lists:reverse(lists:keysort(2, maps:to_list(Counts))), 14),
    Joined = lists:join(<<",">>, [
        [atom_to_binary(M, utf8), <<":">>, integer_to_binary(N)] || {M, N} <- Top
    ]),
    [<<"{">>, Joined, <<"}">>].

int(I) when is_integer(I) -> integer_to_binary(I);
int(_) -> <<"-999">>.

term_enc(infinity) -> <<"\"infinity\"">>;
term_enc(B) when is_binary(B) -> [$", B, $"];
term_enc(I) when is_integer(I) -> integer_to_binary(I).
