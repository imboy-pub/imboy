%%% Disposable production HTTP fixture for the built Widget browser journey.
-module(customer_service_browser_fixture).
-export([run/0]).
-define(FIX, cs_pg_test_fixture).
-include_lib("eunit/include/eunit.hrl").

run() ->
    H = intbe02_http_support:setup_all(),
    try
        setup_keys(),
        S = ?FIX:new_scope(),
        Next = customer_service_seat_http_checks:second_seat(S),
        Org = maps:get(org_id, S),
        ok = ?FIX:exec(
            <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,function_key='customer_service' WHERE organization_id=$1 AND user_id=$2">>,
            [Org, maps:get(actor_user_id, S), maps:get(service_identity_id, S)]
        ),
        Installation = ?FIX:id(),
        Origin = list_to_binary(os:getenv("CSWW_E2E_HOST_ORIGIN")),
        {ok, _} = cs_pg_widget:insert_widget_installation(Org, #{
            id => Installation,
            public_widget_id => integer_to_binary(Installation),
            display_name => <<"synthetic-browser-service">>,
            allowed_origins => [Origin],
            branding => #{},
            consent_version => <<"synthetic-browser-v1">>
        }),
        Console = ?FIX:id(),
        {ok, _} = cs_pg_seat_console:insert_seat_console(Org, #{
            id => Console,
            workspace_id => maps:get(workspace_id, S),
            public_seat_console_id => integer_to_binary(Console),
            allowed_origins => [Origin],
            created_by_user_id => maps:get(owner_user_id, S)
        }),
        ok = save("browser-fixture.json", #{
            public_seat_console_id => integer_to_binary(Console),
            port => maps:get(port, H),
            organization_id => integer_to_binary(Org),
            workspace_id => integer_to_binary(maps:get(workspace_id, S)),
            public_widget_id => integer_to_binary(Installation),
            seat_a => seat(S),
            seat_b => seat(Next)
        }),
        ok = await_done(erlang:monotonic_time(millisecond) + 180000),
        save_proof(Org)
    after
        intbe02_http_support:teardown_all(H),
        inttest_marker_db:release(H)
    end.

setup_keys() ->
    ok = elib_tsid:register(user_device),
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_qr_login]),
    application:set_env(imboy, jwt_key, <<"synthetic-seat-http-key-only">>),
    application:set_env(imboy, cs_widget_subject_key, <<"synthetic-browser-subject-key-only">>),
    application:set_env(imboy, eb_enterprise_keyring, #{
        active_version => 1, keys => #{1 => binary:copy(<<"01">>, 32)}
    }),
    app_version_ds:set_sign_key(
        <<"synthetic">>, <<"seat-test">>, <<"synthetic.seat">>, <<"synthetic-seat-device-key">>
    ).

seat(S) ->
    #{
        identity_id => integer_to_binary(maps:get(service_identity_id, S)),
        headers => customer_service_seat_http_checks:headers(S)
    }.

await_done(Deadline) ->
    case filelib:is_file(filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "browser.done")) of
        true ->
            ok;
        false ->
            true = erlang:monotonic_time(millisecond) < Deadline,
            receive
            after 100 -> await_done(Deadline)
            end
    end.

save_proof(Org) ->
    {ok, Sessions} = elib_pg:query(
        <<"SELECT id::text,status,version,rating,conversations.conversation_id::text FROM customer_service_session conversations WHERE organization_id=$1">>,
        [Org]
    ),
    {ok, Messages} = elib_pg:query(
        <<"SELECT id::text,sender_type,client_msg_id FROM enterprise_message WHERE organization_id=$1 ORDER BY id">>,
        [Org]
    ),
    {ok, Events} = elib_pg:query(
        <<"SELECT action FROM customer_service_event WHERE organization_id=$1 ORDER BY id">>, [Org]
    ),
    ?assertMatch([#{<<"status">> := <<"closed">>, <<"rating">> := 5}], Sessions),
    ?assertEqual(4, length(Messages)),
    ?assertEqual(2, length([M || M <- Messages, maps:get(<<"sender_type">>, M) =:= <<"contact">>])),
    SeatMessages = [M || M <- Messages, maps:get(<<"sender_type">>, M) =:= <<"business_identity">>],
    ?assertEqual(2, length(SeatMessages)),
    ?assertEqual(4, length(lists:usort([maps:get(<<"client_msg_id">>, M) || M <- Messages]))),
    Actions = [maps:get(<<"action">>, E) || E <- Events],
    ?assertEqual(1, length([A || A <- Actions, A =:= <<"session.claimed">>])),
    [
        ?assert(lists:member(A, Actions))
     || A <- [
            <<"session.opened">>,
            <<"session.claimed">>,
            <<"session.transferred">>,
            <<"session.closed">>,
            <<"session.rated">>
        ]
    ],
    save("browser-db-proof.json", #{sessions => Sessions, messages => Messages, events => Events}).

save(Name, Value) ->
    file:write_file(filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), Name), jsone:encode(Value)).
