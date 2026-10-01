%%% Actual persisted token expiry, default Widget polling and production HTTP.
-module(customer_service_widget_expiry_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").
-define(FIX, cs_pg_test_fixture).

run(H) ->
    {ok, _} = application:ensure_all_started(gun),
    [run_case(H, Kind) || Kind <- [expiry, revocation]],
    ok.

run_case(H, Kind) ->
    S = ?FIX:new_scope(),
    try
        journey(H, S, Kind)
    after
        cleanup(S),
        ?FIX:cleanup(S)
    end.

journey(H, S, Kind) ->
    Org = maps:get(org_id, S),
    Installation = ?FIX:id(),
    Secret = crypto:strong_rand_bytes(32),
    %% Header-safe synthetic secret; only its digest is persisted.
    Header = binary:encode_hex(Secret),
    {ok, _} = cs_pg_widget:insert_widget_installation(Org, #{
        id => Installation,
        public_widget_id => integer_to_binary(Installation),
        display_name => <<"synthetic-expiry">>,
        allowed_origins => [],
        branding => #{},
        consent_version => <<"synthetic-v1">>
    }),
    Ttl =
        case Kind of
            expiry -> 8;
            revocation -> 3600
        end,
    Expires = os:system_time(second) + Ttl,
    {ok, _} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
        id => ?FIX:id(),
        contact_id => maps:get(contact_id, S),
        token_digest => cs_widget_support:token_digest(#{}, Header),
        expires_at => Expires,
        widget_installation_id => Installation
    }),
    {ok, Session} = cs_session_app:open_session(Org, #{
        workspace_id => maps:get(workspace_id, S),
        contact_id => maps:get(contact_id, S),
        conversation_id => maps:get(conversation_id, S),
        at => os:system_time(second)
    }),
    Path =
        <<"/api/v1/cs/widget/sessions/", (integer_to_binary(maps:get(id, Session)))/binary,
            "/events?installation_id=", (integer_to_binary(Installation))/binary>>,
    Headers = [{<<"x-cs-visit-token">>, Header}],
    case Kind of
        expiry -> expiry(H, Path, Headers, Expires);
        revocation -> revocation(H, S, Path, Headers, Installation)
    end.

expiry(H, Path, Headers, Expires) ->
    {ok, P} = gun:open({127, 0, 0, 1}, maps:get(port, H), #{protocols => [http]}),
    try
        {ok, http} = gun:await_up(P, 5000),
        Ref = gun:get(P, Path, Headers),
        ?assertMatch({response, nofin, 200, _}, gun:await(P, Ref, 5000)),
        {data, nofin, Initial} = gun:await(P, Ref, 5000),
        ?assert(binary:match(Initial, <<"event: state">>) =/= nomatch),
        ?assert(os:system_time(second) < Expires),
        Raw = finish(P, Ref, Initial, erlang:monotonic_time(millisecond) + 22000),
        ?assert(os:system_time(second) >= Expires),
        ?assert(binary:match(Raw, <<"event: message">>) =:= nomatch),
        Reconnect = gun:get(P, Path, Headers),
        ?assertMatch({response, _, 401, _}, gun:await(P, Reconnect, 5000)),
        {ok, Body} = gun:await_body(P, Reconnect, 5000),
        ?assertEqual(<<"token_expired">>, maps:get(<<"msg">>, jsone:decode(Body))),
        save("widget-expiry-sse.txt", Raw),
        save("widget-expiry-reconnect.json", Body)
    after
        gun:close(P)
    end.

finish(P, Ref, Raw, Deadline) ->
    Left = Deadline - erlang:monotonic_time(millisecond),
    ?assert(Left > 0),
    case gun:await(P, Ref, Left) of
        {data, fin, Chunk} -> <<Raw/binary, Chunk/binary>>;
        {data, nofin, Chunk} -> finish(P, Ref, <<Raw/binary, Chunk/binary>>, Deadline);
        Other -> error({widget_expiry_stream_did_not_finish, Other})
    end.

cleanup(S) ->
    Org = maps:get(org_id, S),
    ok = ?FIX:exec(
        <<"DELETE FROM customer_service_visit_token WHERE organization_id=$1 AND widget_installation_id IS NOT NULL">>,
        [Org]
    ),
    ?FIX:exec(<<"DELETE FROM customer_service_widget_installation WHERE organization_id=$1">>, [Org]).

save(Name, Bytes) ->
    file:write_file(filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), Name), Bytes).

revocation(H, S, Path, Headers, Installation) ->
    {ok, P} = gun:open({127, 0, 0, 1}, maps:get(port, H), #{protocols => [http]}),
    try
        {ok, http} = gun:await_up(P, 5000),
        Ref = gun:get(P, Path, Headers),
        ?assertMatch({response, nofin, 200, _}, gun:await(P, Ref, 5000)),
        {data, nofin, Initial} = gun:await(P, Ref, 5000),
        ?assert(binary:match(Initial, <<"event: state">>) =/= nomatch),
        {ok, _} = cs_widget_app:revoke_installation(maps:get(org_id, S), #{
            id => Installation,
            workspace_id => maps:get(workspace_id, S),
            actor_user_id => maps:get(owner_user_id, S),
            at => os:system_time(second)
        }),
        History = binary:replace(Path, <<"/events?">>, <<"/messages?">>),
        denied(H, History, Headers),
        denied(H, History, Headers, post),
        ?assertEqual(0, ?FIX:count(maps:get(org_id, S), messages)),
        Raw = finish(P, Ref, Initial, erlang:monotonic_time(millisecond) + 22000),
        ?assert(binary:match(Raw, <<"event: message">>) =:= nomatch),
        denied(H, Path, Headers),
        save("widget-installation-revoked-sse.txt", Raw)
    after
        gun:close(P)
    end.

denied(H, Path, Headers) -> denied(H, Path, Headers, get).

denied(H, Path, Headers, Method) ->
    {ok, P} = gun:open({127, 0, 0, 1}, maps:get(port, H), #{protocols => [http]}),
    try
        {ok, http} = gun:await_up(P, 5000),
        Ref =
            case Method of
                get ->
                    gun:get(P, Path, Headers);
                post ->
                    gun:post(
                        P,
                        Path,
                        [{<<"content-type">>, <<"application/json">>} | Headers],
                        jsone:encode(#{
                            body => <<"synthetic-after-revoke">>,
                            client_msg_id => <<"synthetic-revoked-write">>
                        })
                    )
            end,
        ?assertMatch({response, _, 403, _}, gun:await(P, Ref, 5000)),
        {ok, Body} = gun:await_body(P, Ref, 5000),
        ?assertEqual(<<"installation_revoked">>, maps:get(<<"msg">>, jsone:decode(Body))),
        save("widget-installation-revoked-" ++ atom_to_list(Method) ++ "-response.json", Body)
    after
        gun:close(P)
    end.
