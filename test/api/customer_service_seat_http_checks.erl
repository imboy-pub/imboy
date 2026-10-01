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
    Closed = post(H, S, cs_path(S, Id, <<"close">>), #{
        workspace_id => maps:get(workspace_id, S), expected_version => 2
    }),
    ?assertEqual(200, maps:get(status, Closed)),
    Late = send(H, S, <<"synthetic-http-after-close">>),
    ?assertEqual(409, maps:get(status, Late)),
    ?assertEqual(1, ?FIX:count(Org, messages)).

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
