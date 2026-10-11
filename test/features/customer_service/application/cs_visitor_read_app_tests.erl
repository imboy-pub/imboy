-module(cs_visitor_read_app_tests).
-include_lib("eunit/include/eunit.hrl").

invalid_wire_batch_test_() ->
    [
        fun() ->
            ?assertEqual(
                {error, {invalid_argument, message_ids}}, cs_visitor_read:message_ids(Value)
            )
        end
     || Value <- [
            undefined,
            null,
            [],
            <<>>,
            [1],
            [1.0],
            [<<"0">>],
            [<<"-1">>],
            [<<"01">>],
            [<<"1.0">>],
            [<<"9223372036854775808">>],
            [<<"中文😀"/utf8>>],
            lists:duplicate(201, <<"1">>),
            lists:duplicate(10001, <<"1">>)
        ]
    ].
wire_sort_dedup_and_boundaries_test() ->
    ?assertEqual(
        {ok, [1, 20, 9223372036854775807]},
        cs_visitor_read:message_ids([<<"20">>, <<"1">>, <<"20">>, <<"9223372036854775807">>])
    ),
    ?assertEqual({ok, [1]}, cs_visitor_read:message_ids(lists:duplicate(200, <<"1">>))).

patch_case_and_derived_fields_test() ->
    {ok, Entry} = cs_actions:widget(widget_session_messages),
    {ok, Case} = cs_actions:case_for(Entry, <<"PATCH">>),
    ?assertEqual(widget_confirm_reads, maps:get(facade, Case)),
    ?assert(lists:member(highest_confirmed_id, maps:get(client_forbidden, Entry))),
    ?assert(lists:member(read_at, maps:get(client_forbidden, Entry))).

seat_projection_preserves_enterprise_source_test() ->
    setup_seat(),
    try
        Message = #{
            id => 10,
            sender_type => business_identity,
            visibility => visible,
            body => <<"enterprise decrypted source">>,
            assets => [#{id => 90}]
        },
        Contact = #{id => 20, sender_type => contact, visibility => visible},
        meck:expect(enterprise_business_facade, list_messages, fun(1, _) ->
            {ok, [Message, Contact]}
        end),
        meck:expect(cs_pg_store, fetch_visitor_message_reads, fun(1, 2, Scope, [10]) ->
            ?assertEqual(3, maps:get(session_id, Scope)),
            {ok, [#{message_id => 10, read_at => <<"first">>}]}
        end),
        ?assertEqual(
            {ok, [Message#{read_at => <<"first">>}, Contact]},
            cs_visitor_read_app:list_messages(1, seat_params())
        )
    after
        cleanup_seat()
    end.
seat_transferred_history_reader_test() ->
    setup_seat(),
    try
        meck:expect(cs_pg_store, fetch_conversation_session, fun(1, 2, 4) ->
            {ok, #{id => 3, conversation_id => 4, contact_id => 5, business_identity_id => 99}}
        end),
        meck:expect(enterprise_business_facade, list_messages, fun(1, _) ->
            {ok, [#{id => 10, sender_type => business_identity, visibility => visible}]}
        end),
        meck:expect(cs_pg_store, fetch_visitor_message_reads, fun(1, 2, _, [10]) ->
            {ok, [#{message_id => 10, read_at => <<"first">>}]}
        end),
        ?assertMatch(
            {ok, [#{read_at := <<"first">>}]}, cs_visitor_read_app:list_messages(1, seat_params())
        )
    after
        cleanup_seat()
    end.
seat_history_without_cs_session_test() ->
    setup_seat(),
    try
        Message = #{id => 10, body => <<"history">>, read_at => <<"untrusted">>},
        meck:expect(enterprise_business_facade, list_messages, fun(1, _) -> {ok, [Message]} end),
        meck:expect(cs_pg_store, fetch_conversation_session, fun(1, 2, 4) -> {error, not_found} end),
        ?assertEqual(
            {ok, [maps:remove(read_at, Message)]},
            cs_visitor_read_app:list_messages(1, seat_params())
        ),
        ?assertEqual(0, meck:num_calls(cs_pg_store, fetch_visitor_message_reads, '_'))
    after
        cleanup_seat()
    end.
seat_session_storage_failure_propagates_test() ->
    setup_seat(),
    try
        meck:expect(enterprise_business_facade, list_messages, fun(1, _) -> {ok, []} end),
        meck:expect(cs_pg_store, fetch_conversation_session, fun(1, 2, 4) ->
            {error, session_storage_failure}
        end),
        ?assertEqual(
            {error, session_storage_failure}, cs_visitor_read_app:list_messages(1, seat_params())
        ),
        ?assertEqual(1, meck:num_calls(enterprise_business_facade, list_messages, '_'))
    after
        cleanup_seat()
    end.
seat_enterprise_failure_propagates_test() ->
    setup_seat(),
    try
        meck:expect(enterprise_business_facade, list_messages, fun(_, _) ->
            {error, enterprise_failure}
        end),
        ?assertEqual(
            {error, enterprise_failure}, cs_visitor_read_app:list_messages(1, seat_params())
        ),
        ?assertEqual(0, meck:num_calls(cs_pg_store, fetch_visitor_message_reads, '_'))
    after
        cleanup_seat()
    end.
seat_receipt_storage_failure_is_not_unread_test() ->
    setup_seat(),
    try
        meck:expect(enterprise_business_facade, list_messages, fun(_, _) ->
            {ok, [#{id => 10, sender_type => business_identity, visibility => visible}]}
        end),
        meck:expect(cs_pg_store, fetch_visitor_message_reads, fun(_, _, _, _) ->
            {error, receipt_storage_failure}
        end),
        ?assertEqual(
            {error, receipt_storage_failure}, cs_visitor_read_app:list_messages(1, seat_params())
        )
    after
        cleanup_seat()
    end.
setup_seat() ->
    meck:new(cs_pg_store, [non_strict, no_link]),
    meck:new(enterprise_business_facade, [non_strict, no_link]),
    meck:expect(cs_pg_store, fetch_conversation_session, fun(1, 2, 4) ->
        {ok, #{id => 3, conversation_id => 4, contact_id => 5, business_identity_id => 6}}
    end),
    meck:expect(cs_pg_store, fetch_seat, fun(1, 6) -> {ok, #{enabled => true}} end).
cleanup_seat() ->
    meck:unload(cs_pg_store),
    meck:unload(enterprise_business_facade).
seat_params() ->
    #{workspace_id => 2, conversation_id => 4, business_identity_id => 6, store => cs_pg_store}.

confirm_application_derives_credential_scope_test() ->
    meck:new(cs_widget_session_app, [non_strict, no_link]),
    meck:new(cs_pg_store, [non_strict, no_link]),
    try
        meck:expect(cs_widget_session_app, visitor_session_scope, fun(1, _) ->
            {ok, #{
                workspace_id => 2,
                contact_id => 5,
                token => #{id => 7, widget_installation_id => 6},
                session => #{id => 3, conversation_id => 4}
            }}
        end),
        meck:expect(cs_pg_store, fetch_widget_installation, fun(1, 6) ->
            {ok, #{status => active, allowed_origins => [<<"https://widget.test">>]}}
        end),
        meck:expect(cs_pg_store, confirm_visitor_message_reads, fun(1, 2, Scope, [10, 20], 123) ->
            ?assertEqual(
                #{
                    session_id => 3,
                    conversation_id => 4,
                    contact_id => 5,
                    installation_id => 6,
                    token_id => 7,
                    origin => <<"https://widget.test">>,
                    request_host => undefined
                },
                Scope
            ),
            {ok, confirmed}
        end),
        ?assertEqual(
            {ok, confirmed},
            cs_visitor_read_app:confirm(1, #{
                store => cs_pg_store,
                message_ids => [<<"20">>, <<"10">>, <<"10">>],
                at => 123,
                origin => <<"https://widget.test">>
            })
        )
    after
        meck:unload(cs_widget_session_app),
        meck:unload(cs_pg_store)
    end.
read_hint_only_invalidates_no_read_fact_payload_test() ->
    Envelope = cs_seat_event_app:envelope(
        #{
            id => 30,
            action => <<"message.read">>,
            workspace_id => 2,
            session_id => 3,
            detail => #{<<"message_id">> => 10},
            created_at => 123
        },
        1
    ),
    ?assertMatch(
        #{
            type := <<"message.appended">>,
            resource_type := <<"message">>,
            resource_id := 10,
            reason := <<"updated">>
        },
        Envelope
    ),
    ?assertEqual(false, maps:is_key(read_at, Envelope)).

confirm_wrong_origin_refuses_before_transaction_test() ->
    meck:new(cs_widget_session_app, [non_strict, no_link]),
    meck:new(cs_pg_store, [non_strict, no_link]),
    try
        meck:expect(cs_widget_session_app, visitor_session_scope, fun(_, _) ->
            {ok, #{
                workspace_id => 2,
                contact_id => 5,
                token => #{id => 7, widget_installation_id => 6},
                session => #{id => 3, conversation_id => 4}
            }}
        end),
        meck:expect(cs_pg_store, fetch_widget_installation, fun(_, _) ->
            {ok, #{status => active, allowed_origins => [<<"https://widget.test">>]}}
        end),
        ?assertMatch(
            {error, _},
            cs_visitor_read_app:confirm(1, #{
                store => cs_pg_store,
                message_ids => [<<"10">>],
                at => 123,
                origin => <<"https://foreign.test">>
            })
        ),
        ?assertEqual(0, meck:num_calls(cs_pg_store, confirm_visitor_message_reads, '_'))
    after
        meck:unload(cs_widget_session_app),
        meck:unload(cs_pg_store)
    end.

fake_port_receipts_fail_closed_without_canonical_rows_test() ->
    cs_fake_store:init(),
    try
        ?assertMatch(
            {error, _},
            cs_fake_store:confirm_visitor_message_reads(
                1,
                2,
                #{
                    session_id => 3,
                    conversation_id => 4,
                    contact_id => 5,
                    installation_id => 6,
                    token_id => 7
                },
                [10],
                123
            )
        ),
        ?assertEqual(
            {ok, []},
            cs_fake_store:fetch_visitor_message_reads(
                1, 2, #{session_id => 3, conversation_id => 4, contact_id => 5}, [10]
            )
        )
    after
        cs_fake_store:destroy()
    end.
