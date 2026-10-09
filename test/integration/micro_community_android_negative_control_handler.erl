-module(micro_community_android_negative_control_handler).
-export([init/2]).

init(Req, #{role := Role, secret := Secret, probe := Probe} = State) ->
    {Ip, _} = cowboy_req:peer(Req),
    {Path, HeaderName} = endpoint(Role),
    Header = cowboy_req:header(HeaderName, Req, <<>>),
    Allowed =
        Ip =:= {127, 0, 0, 1} andalso cowboy_req:path(Req) =:= Path andalso
            byte_size(Header) =:= byte_size(Secret) andalso
            crypto:hash_equals(crypto:hash(sha256, Header), crypto:hash(sha256, Secret)),
    case Allowed of
        true -> dispatch(cowboy_req:method(Req), Role, Req, State, Probe);
        false -> reply(403, #{code => 403}, Req, State)
    end.

endpoint(phase) -> {<<"/mc-owned/android/phase">>, <<"x-mc-owned-phase">>};
endpoint(control) -> {<<"/mc-owned/android/control">>, <<"x-mc-owned-control">>}.

dispatch(<<"GET">>, phase, Req, State, Probe) ->
    call(Probe, snapshot, Req, State);
dispatch(<<"POST">>, Role, Req, State, Probe) ->
    case cowboy_req:read_body(Req, #{length => 1024, period => 5000}) of
        {ok, Bytes, Req1} when byte_size(Bytes) =< 1024 ->
            try jsone:decode(Bytes, [{object_format, map}]) of
                #{<<"action">> := Action, <<"actual_msg_id">> := Id} = Body when
                    Role =:= phase,
                    map_size(Body) =:= 2,
                    is_binary(Id),
                    Action =:= <<"chat_denied">>;
                    Role =:= phase,
                    map_size(Body) =:= 2,
                    is_binary(Id),
                    Action =:= <<"queued_denied">>
                ->
                    call(Probe, {phase, Action, Id}, Req1, State);
                #{<<"action">> := Action} = Body when map_size(Body) =:= 1, is_binary(Action) ->
                    call(Probe, {Role, Action}, Req1, State);
                _ ->
                    reply(400, #{code => 400}, Req1, State)
            catch
                _:_ -> reply(400, #{code => 400}, Req1, State)
            end;
        {_, _, Req1} ->
            reply(413, #{code => 413}, Req1, State)
    end;
dispatch(_, _, Req, State, _) ->
    reply(405, #{code => 405}, Req, State).

call(Probe, Command, Req, State) ->
    try gen_server:call(Probe, Command, 90000) of
        {ok, Payload} -> reply(200, #{code => 0, payload => Payload}, Req, State);
        {error, Code} when is_integer(Code) -> reply(Code, #{code => Code}, Req, State)
    catch
        _:_ -> reply(500, #{code => 500}, Req, State)
    end.

reply(Status, Body, Req, State) ->
    {ok,
        cowboy_req:reply(
            Status,
            #{
                <<"content-type">> => <<"application/json">>,
                <<"cache-control">> => <<"no-store">>
            },
            jsone:encode(Body),
            Req
        ),
        State}.
