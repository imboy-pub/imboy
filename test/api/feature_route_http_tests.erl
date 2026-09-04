-module(feature_route_http_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, feature_route_http_test_listener).

compiled_route_http_contract_test_() ->
    {timeout, 15, fun compiled_route_http_contract/0}.

compiled_route_http_contract() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile(imboy_router:get_routes()),
    {ok, _Pid} = cowboy:start_clear(
        ?LISTENER,
        [{port, 0}],
        #{env => #{dispatch => Dispatch}}
    ),
    try
        Port = ranch:get_port(?LISTENER),
        ManifestResponse = request(Port, <<"/api/v1/app/manifest">>),
        ?assertMatch(<<"HTTP/1.1 200", _/binary>>, ManifestResponse),
        ?assertNotEqual(nomatch, binary:match(ManifestResponse, imboy_feature:manifest_hash())),
        assert_optional_route_contract(Port)
    after
        ok = cowboy:stop_listener(?LISTENER)
    end.

assert_optional_route_contract(Port) ->
    case imboy_feature:compiled(channel) of
        false ->
            Response = request(Port, <<"/api/v1/channels/discover">>),
            ?assertMatch(<<"HTTP/1.1 404", _/binary>>, Response);
        true ->
            [{_Host, Routes}] = imboy_router:get_routes(),
            ?assert(lists:keymember("/api/v1/channels/discover", 1, Routes))
    end.

request(Port, Path) ->
    {ok, Socket} = gen_tcp:connect(
        {127, 0, 0, 1},
        Port,
        [binary, {active, false}],
        5000
    ),
    ok = gen_tcp:send(Socket, [
        <<"GET ">>, Path, <<" HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n">>
    ]),
    receive_all(Socket, []).

receive_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} -> receive_all(Socket, [Data | Acc]);
        {error, closed} -> iolist_to_binary(lists:reverse(Acc))
    end.
