%%% Production middleware, facts, facade and database; synthetic JWT/device key only.
-module(customer_service_seat_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").
-define(FIX, cs_pg_test_fixture).
run(H) ->
    S = ?FIX:new_scope(),
    Old = [{K, application:get_env(imboy, K)} || K <- [jwt_key, eb_enterprise_keyring]],
    application:set_env(imboy, jwt_key, <<"synthetic-seat-http-key-only">>),
    application:set_env(imboy, eb_enterprise_keyring, #{
        active_version => 1, keys => #{1 => binary:copy(<<"01">>, 32)}
    }),
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>, <<"seat-test">>, <<"synthetic.seat">>, <<"synthetic-seat-device-key">>
    ),
    try
        journey(H, S)
    after
        ?FIX:cleanup(S),
        [
            case V of
                {ok, X} -> application:set_env(imboy, K, X);
                undefined -> application:unset_env(imboy, K)
            end
         || {K, V} <- Old
        ]
    end.
journey(H, S) ->
    Org = maps:get(org_id, S),
    Actor = maps:get(actor_user_id, S),
    Service = maps:get(service_identity_id, S),
    ok = ?FIX:exec(
        <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,function_key='customer_service' WHERE organization_id=$1 AND user_id=$2">>,
        [Org, Actor, Service]
    ),
    {ok, Session} = cs_session_app:open_session(Org, #{
        workspace_id => maps:get(workspace_id, S),
        contact_id => maps:get(contact_id, S),
        conversation_id => maps:get(conversation_id, S),
        at => os:system_time(second)
    }),
    Id = maps:get(id, Session),
    ?assertEqual(403, maps:get(status, send(H, S, <<"synthetic-unclaimed">>))),
    R = post(H, S, cs_path(S, Id, <<"claim">>), #{
        workspace_id => maps:get(workspace_id, S), expected_version => 1
    }),
    ?assertEqual(200, maps:get(status, R)),
    suspended_check(H, S),
    Sent = send(H, S, <<"synthetic-http-first">>),
    ?assertEqual(200, maps:get(status, Sent)),
    ?assertEqual(1, ?FIX:count(Org, messages)),
    ?assertEqual(200, maps:get(status, send(H, S, <<"synthetic-http-first">>))),
    ?assertEqual(1, ?FIX:count(Org, messages)),
    ?assertEqual(
        1,
        ?FIX:scalar(
            <<"SELECT count(*) AS n FROM customer_service_event WHERE organization_id=$1 AND action='message.appended'">>,
            [Org],
            -1
        )
    ),
    Next = transfer_check(H, S, Id),
    finish(H, Next, Id).

finish(H, S, Id) ->
    Closed = post(H, S, cs_path(S, Id, <<"close">>), #{
        workspace_id => maps:get(workspace_id, S), expected_version => 3
    }),
    ?assertEqual(200, maps:get(status, Closed)),
    ?assertEqual(409, maps:get(status, send(H, S, <<"synthetic-http-after-close">>))),
    ?assertEqual(2, ?FIX:count(maps:get(org_id, S), messages)).

transfer_check(H, S, Id) ->
    Next = second_seat(S),
    Ws = maps:get(workspace_id, S),
    Bid = maps:get(service_identity_id, Next),
    ?assertEqual(
        403,
        maps:get(
            status,
            post(H, Next, cs_path(S, Id, <<"close">>), #{workspace_id => Ws, expected_version => 2})
        )
    ),
    ?assertEqual(
        403,
        maps:get(
            status,
            post(H, Next, cs_path(S, Id, <<"transfer">>), #{
                workspace_id => Ws, expected_version => 2, to_identity_id => Bid
            })
        )
    ),
    R = post(H, S, cs_path(S, Id, <<"transfer">>), #{
        workspace_id => Ws, expected_version => 2, to_identity_id => Bid
    }),
    ?assertEqual(200, maps:get(status, R)),
    stale_control_check(S, Id),
    ?assertEqual(403, maps:get(status, send(H, S, <<"synthetic-old-seat">>))),
    ?assertEqual(
        403,
        maps:get(
            status,
            post(H, S, cs_path(S, Id, <<"close">>), #{workspace_id => Ws, expected_version => 3})
        )
    ),
    ?assertEqual(200, maps:get(status, send(H, Next, <<"synthetic-new-seat">>))),
    Next.

%% Even a guessed new version cannot commit an old handler snapshot.
stale_control_check(S, Id) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Old = maps:get(service_identity_id, S),
    Event = #{
        workspace_id => Ws,
        session_id => Id,
        business_identity_id => Old,
        actor_user_id => maps:get(actor_user_id, S),
        actor_kind => <<"seat">>,
        action => <<"session.transferred">>,
        detail => #{<<"from">> => Old}
    },
    ?assertEqual(
        {error, conflict},
        cs_pg_session:transfer_session(Org, Ws, Id, Old, 3, os:system_time(second), Event)
    ),
    ?assertEqual(
        {error, conflict},
        cs_pg_session:close_session(Org, Ws, Id, undefined, 3, os:system_time(second), Event#{
            action => <<"session.closed">>
        })
    ).

second_seat(S) ->
    Org = maps:get(org_id, S),
    Peer = maps:get(peer_user_id, S),
    Bid = ?FIX:id(),
    Owner = maps:get(owner_user_id, S),
    ok = ?FIX:exec(
        <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
        [Org, Peer]
    ),
    ok = ?FIX:exec(
        <<"INSERT INTO organization_business_identity(id,organization_id,function_key,display_name,status,version,created_by_user_id) VALUES($1,$2,'customer_service','synthetic-second-seat','active',1,$3)">>,
        [Bid, Org, Owner]
    ),
    ok = ?FIX:exec(
        <<"INSERT INTO organization_business_identity_assignment(id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version) VALUES($1,$2,$3,'customer_service',$4,'active',$5,1)">>,
        [?FIX:id(), Org, Bid, Peer, Owner]
    ),
    {ok, _} = cs_seat_app:create_seat(Org, #{
        workspace_id => maps:get(workspace_id, S),
        business_identity_id => Bid,
        created_by_user_id => Owner
    }),
    S#{actor_user_id => Peer, service_identity_id => Bid}.

suspended_check(H, S) ->
    Params = #{
        workspace_id => maps:get(workspace_id, S),
        business_identity_id => maps:get(service_identity_id, S),
        at => os:system_time(second)
    },
    Org = maps:get(org_id, S),
    {ok, _} = cs_seat_app:suspend_seat(Org, Params),
    ?assertEqual(403, maps:get(status, send(H, S, <<"synthetic-http-disabled">>))),
    ?assertEqual(0, ?FIX:count(Org, messages)),
    {ok, _} = cs_seat_app:resume_seat(Org, Params).

cs_path(S, Id, Action) ->
    <<"/api/v1/cs/organizations/", (integer_to_binary(maps:get(org_id, S)))/binary, "/sessions/",
        (integer_to_binary(Id))/binary, "/", Action/binary>>.
send(H, S, Client) ->
    Path =
        <<"/api/v1/enterprise/organizations/", (integer_to_binary(maps:get(org_id, S)))/binary,
            "/conversations/", (integer_to_binary(maps:get(conversation_id, S)))/binary,
            "/messages">>,
    post(H, S, Path, #{
        workspace_id => maps:get(workspace_id, S),
        client_msg_id => Client,
        sender_type => <<"business_identity">>,
        body => <<"synthetic HTTP seat message">>
    }).
post(H, S, Path, Body) ->
    Token = token_ds:encrypt_token(maps:get(actor_user_id, S)),
    Headers = maps:merge(intbe02_http_support:auth(Token), #{
        <<"cos">> => <<"synthetic">>,
        <<"vsn">> => <<"seat-test">>,
        <<"pkg">> => <<"synthetic.seat">>,
        <<"did">> => <<"synthetic-device">>,
        <<"method">> => <<"sha256">>,
        <<"sign">> => elib_hasher:hmac_sha256(
            <<"synthetic-device|seat-test|synthetic|synthetic.seat">>,
            <<"synthetic-seat-device-key">>
        )
    }),
    R = intbe02_http_support:http(maps:get(port, H), <<"POST">>, Path, Body, Headers),
    File = filename:join(os:getenv("IMBOY_GATE_RUN_DIR", "/tmp"), "seat-http-responses.jsonl"),
    ok = file:write_file(
        File,
        [
            jsone:encode(#{
                path => Path,
                status => maps:get(status, R),
                response => jsone:decode(maps:get(body, R))
            }),
            <<"\n">>
        ],
        [append]
    ),
    R.
