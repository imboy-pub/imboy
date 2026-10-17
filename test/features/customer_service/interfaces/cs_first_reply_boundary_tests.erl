-module(cs_first_reply_boundary_tests).
-include_lib("eunit/include/eunit.hrl").

forged_measurement_fields_test_() ->
    Keys = [
        origin,
        response_origin,
        response_fact,
        response_measurement_version,
        accepted_at,
        first_visitor_message_id,
        first_visitor_message_at,
        first_human_reply_id,
        first_human_reply_at,
        first_ai_reply_id,
        first_ai_reply_at,
        first_reply,
        first_response_at
    ],
    [{atom_to_list(Key), fun() -> assert_rejected(Key) end} || Key <- Keys].

assert_rejected(Key) ->
    Bin = atom_to_binary(Key, utf8),
    ?assertMatch({error, _}, cs_http:check_forbidden(#{Key => 1}, [], [Key])),
    ?assertMatch({error, _}, cs_http:check_forbidden(#{Bin => 1}, [], [Key])),
    ?assertMatch({error, _}, cs_http:check_forbidden(#{}, [{Bin, <<"1">>}], [Key])),
    {ok, CsEntry} = cs_actions:tenant(session_messages),
    [CsCase] = maps:get(cases, CsEntry),
    ?assertMatch(
        {error, _},
        cs_http:build_params(
            CsEntry,
            CsCase,
            #{qs => <<>>},
            #{Bin => 1},
            #{}
        )
    ),
    ?assertMatch(
        {error, _},
        cs_http:build_params(
            CsEntry,
            CsCase,
            #{qs => <<Bin/binary, "=1">>},
            #{},
            #{}
        )
    ),
    Entry = #{owner => tenant},
    ?assertMatch(
        {error, _},
        eb_enterprise_http:build_params(
            Entry,
            #{},
            #{qs => <<>>},
            #{Bin => 1},
            #{}
        )
    ),
    ?assertMatch(
        {error, _},
        eb_enterprise_http:build_params(
            Entry,
            #{},
            #{qs => <<Bin/binary, "=1">>},
            #{},
            #{}
        )
    ).

trusted_context_routes_test_() ->
    {foreach,
        fun() ->
            meck:new(customer_service_facade, [passthrough]),
            meck:expect(
                customer_service_facade,
                append_conversation_message,
                fun(_, _, Context) -> {classified, Context} end
            ),
            meck:expect(
                customer_service_facade,
                append_conversation_message,
                fun(_, _) -> unclassified end
            ),
            meck:expect(
                customer_service_facade,
                append_session_message,
                fun(_, _, Context) -> {classified, Context} end
            ),
            meck:expect(
                customer_service_facade,
                append_session_message,
                fun(_, _) -> unclassified end
            )
        end,
        fun(_) -> meck:unload(customer_service_facade) end, [
            fun(_) -> fun enterprise_context/0 end,
            fun(_) -> fun visitor_context/0 end
        ]}.

enterprise_context() ->
    P = #{
        caller_function_key => <<"customer_service">>,
        actor_user_id => 20,
        caller_identity_id => 30,
        identity_id => 30,
        sender_type => <<"business_identity">>
    },
    C = #{
        auth_context => enterprise_member,
        organization_id => 10,
        function_key => <<"customer_service">>,
        user_id => 20,
        business_identity_id => 30
    },
    ?assertEqual(
        {classified, {authenticated_human, 20, 30}},
        eb_enterprise_facade_call:call(append_message, 10, P, C)
    ),
    lists:foreach(
        fun(Bad) ->
            ?assertEqual(unclassified, eb_enterprise_facade_call:call(append_message, 10, P, Bad))
        end,
        [
            C#{user_id => 21},
            C#{business_identity_id => 31},
            C#{organization_id => 11},
            C#{function_key => <<"sales">>},
            C#{auth_context => enterprise_owner_admin},
            C#{auth_context => cs_shop_key},
            #{}
        ]
    ),
    ?assertEqual(
        unclassified,
        eb_enterprise_facade_call:call(
            append_message,
            10,
            P#{response_origin => human, first_reply => true}
        )
    ).

visitor_context() ->
    P = #{contact_id => 30},
    C = #{auth_context => cs_visit, organization_id => 10, contact_id => 30},
    ?assertEqual({classified, visitor}, cs_facade_call:call(append_session_message, 10, P, C)),
    ?assertEqual(
        unclassified,
        cs_facade_call:call(
            append_session_message,
            10,
            P,
            C#{auth_context => cs_shop_key}
        )
    ),
    ?assertEqual(
        {error, invalid_response_origin},
        cs_facade_call:call(
            append_session_message, 10, P, C#{contact_id => 31}
        )
    ).
