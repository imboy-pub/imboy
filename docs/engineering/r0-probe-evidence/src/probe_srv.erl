-module(probe_srv).
-export([main/1]).

%% R0 capacity probe server: loopback Cowboy listener based on imboy_app:start_listener/2.
%%
%% usage (erl -s probe_srv main -- Args):
%%   DepsRoot Port ShapeSpec ReadyFile LifetimeMs
%%   ShapeSpec "imboy"            -> default Ranch limits with loopback binding
%%   ShapeSpec "cap=N,sups=K,acc=K" -> explicit ranch transport options

main(Args0) ->
    Args = [to_list(A) || A <- Args0],
    [DepsRoot, PortS, ShapeS, ReadyFile, LifeS | _] = Args,
    Port = list_to_integer(PortS),
    Life = list_to_integer(LifeS),
    [
        code:add_patha(filename:join([DepsRoot, D, "ebin"]))
     || D <- [cowlib, ranch, cowboy]
    ],
    {ok, _Started} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/v1/ws", ws_probe, []},
            {"/stats/[...]", stats_handler, []}
        ]}
    ]),
    %% Same ProtoOpts keys as imboy_app:start_listener/2, minus imboy middlewares.
    ProtoOpts = #{
        env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, cowboy_handler],
        metrics_callback => fun(_) -> ok end,
        stream_handlers => [cowboy_metrics_h, cowboy_compress_h, cowboy_stream_h]
    },
    TransportOpts = transport_opts(ShapeS, Port),
    {ok, _} = cowboy:start_clear(probe, TransportOpts, ProtoOpts),
    Effective = maps:remove(socket_opts, ranch:get_transport_options(probe)),
    io:format("EFFECTIVE transport_opts=~0p~n", [Effective]),
    ConnsSups = length(ranch_server:get_connections_sups(probe)),
    io:format(
        "EFFECTIVE max_connections_per_conns_sup=~0p conns_sups=~0p total_cap=~0p~n",
        [
            ranch:get_max_connections(probe),
            ConnsSups,
            case ranch:get_max_connections(probe) of
                infinity -> infinity;
                Max -> Max * ConnsSups
            end
        ]
    ),
    ok = file:write_file(ReadyFile, <<"ready\n">>),
    timer:apply_after(Life, erlang, halt, [0]),
    receive
    after infinity -> ok
    end.

transport_opts("imboy", Port) ->
    %% Match the product listener's list form while restricting the probe address.
    [{ip, {127, 0, 0, 1}}, {port, Port}];
transport_opts(Spec, Port) ->
    lists:foldl(
        fun(Token, Acc) ->
            [K, V] = string:split(Token, "="),
            Key =
                case K of
                    "cap" -> max_connections;
                    "sups" -> num_conns_sups;
                    "acc" -> num_acceptors
                end,
            Acc#{Key => list_to_integer(V)}
        end,
        #{socket_opts => [{ip, {127, 0, 0, 1}}, {port, Port}]},
        string:split(Spec, ",", all)
    ).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_binary(A) -> binary_to_list(A);
to_list(A) when is_list(A) -> A.
