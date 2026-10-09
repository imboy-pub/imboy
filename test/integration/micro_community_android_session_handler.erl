-module(micro_community_android_session_handler).
-export([init/2]).

%% Test-only bootstrap on a separate IPv4-loopback listener. No business stubs.
init(Req, #{secret := Secret, consumed := Consumed, issue := Issue} = State) ->
    {Ip, _} = cowboy_req:peer(Req),
    Header = cowboy_req:header(<<"x-mc-owned-bootstrap">>, Req, <<>>),
    Allowed =
        Ip =:= {127, 0, 0, 1} andalso cowboy_req:method(Req) =:= <<"GET">> andalso
            cowboy_req:path(Req) =:= <<"/mc-owned/android/session">> andalso
            byte_size(Header) =:= byte_size(Secret) andalso
            crypto:hash_equals(crypto:hash(sha256, Header), crypto:hash(sha256, Secret)),
    case Allowed of
        true -> authorized(Req, State, Consumed, Issue);
        false -> respond(403, #{code => 403}, Req, State)
    end.

authorized(Req, State, Consumed, Issue) ->
    Query = cowboy_req:parse_qs(Req),
    Did = proplists:get_value(<<"did">>, Query, <<>>),
    ValidDid =
        is_binary(Did) andalso byte_size(Did) > 0 andalso byte_size(Did) =< 128 andalso
            lists:all(fun(C) -> C >= 33 andalso C =< 126 end, binary_to_list(Did)),
    case ValidDid andalso atomics:compare_exchange(Consumed, 1, 0, 1) =:= ok of
        true ->
            try Issue(Did) of
                Payload when is_map(Payload) ->
                    respond(200, #{code => 0, payload => Payload}, Req, State)
            catch
                _:_ -> respond(500, #{code => 500}, Req, State)
            end;
        false ->
            respond(409, #{code => 409}, Req, State)
    end.

respond(Status, Body, Req, State) ->
    Reply = cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json">>, <<"cache-control">> => <<"no-store">>},
        jsone:encode(Body),
        Req
    ),
    {ok, Reply, State}.
