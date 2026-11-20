-module(enterprise_history_isolation_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).

enterprise_history_isolation_pg_test_() ->
    {setup, fun setup/0, fun eunit_runner:eunit_cleanup_db/1, fun(_) ->
        {timeout, 60, fun history_isolation/0}
    end}.

setup() ->
    {ok, Conn} = eunit_runner:eunit_setup_with_db(),
    Conn.

history_isolation() ->
    Scope = ?FIX:new_scope(),
    try
        check_history(Scope)
    after
        ?FIX:cleanup(Scope)
    end.

check_history(Scope) ->
    #{org_id := Org, workspace_id := Ws, conversation_id := Conv, contact_id := Contact} = Scope,
    Account = maps:get(peer_user_id, Scope),
    ok = ?FIX:exec(
        <<"UPDATE enterprise_contact SET imboy_user_id=$1 WHERE id=$2 AND organization_id=$3">>,
        [Account, Contact, Org]
    ),
    Key = ?FIX:keyring_ref(Scope),
    Text = <<"mh05 synthetic enterprise history">>,
    Now = eb_system_clock:now(),
    Read = #{workspace_id => Ws, conversation_id => Conv, key_ref => Key},
    {ok, Created} = eb_message_app:append_message(Org, Read#{
        client_msg_id => <<"mh05-isolation">>,
        body => Text,
        sender_type => contact,
        contact_id => Contact,
        accepted_at => Now,
        notify => fun(_) -> ok end
    }),
    MessageId = maps:get(message_id, Created),
    {ok, History} = eb_message_app:list_messages(Org, Read),
    ?assertMatch([#{body := Text}], History),
    {ok, Canonical} = eb_pg_store:list_messages(Org, Ws, Conv),
    AuditCount = ?FIX:count(Org, Ws, audits),
    Ack = #{
        workspace_id => Ws,
        message_id => MessageId,
        recipient_ref => <<"contact:", (integer_to_binary(Contact))/binary>>,
        device_id => <<"mh05-device">>,
        acked_at => Now
    },
    AssertUnchanged = fun() ->
        ?assertEqual({ok, History}, eb_message_app:list_messages(Org, Read)),
        ?assertEqual({ok, Canonical}, eb_pg_store:list_messages(Org, Ws, Conv)),
        ?assertEqual(AuditCount, ?FIX:count(Org, Ws, audits)),
        ?assertEqual(1, ?FIX:count(Org, Ws, deliveries))
    end,
    try
        ?assertEqual(0, personal_state_count(Account)),
        {ok, First} = eb_message_app:ack_delivery(Org, Ack),
        ?assertMatch(#{canonical_unchanged := true, delivery := #{status := delivered}}, First),
        AssertUnchanged(),
        set_personal_state(Account, <<"ON">>, 1),
        AssertUnchanged(),
        {ok, Repeated} = eb_message_app:ack_delivery(Org, Ack),
        #{delivery := #{id := DeliveryId}} = First,
        ?assertMatch(#{canonical_unchanged := true, delivery := #{id := DeliveryId}}, Repeated),
        AssertUnchanged(),
        set_personal_state(Account, <<"OFF_PURGE_PENDING">>, 2),
        AssertUnchanged()
    after
        ?FIX:exec(<<"DELETE FROM public.personal_history_authorization WHERE account_id=$1">>, [
            Account
        ])
    end.

personal_state_count(Account) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM public.personal_history_authorization WHERE account_id=$1">>,
        [Account]
    ).

%% The application switch belongs to MH-02. These rows model its stored states.
set_personal_state(Account, State, Generation) ->
    ?FIX:exec(
        <<
            "INSERT INTO public.personal_history_authorization(account_id,domain,state,generation) "
            "VALUES ($1,'personal',$2,$3) ON CONFLICT(account_id,domain) DO UPDATE "
            "SET state=EXCLUDED.state,generation=EXCLUDED.generation"
        >>,
        [Account, State, Generation]
    ).
