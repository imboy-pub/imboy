-module(cs_seat_body_limit_tests).

-include_lib("eunit/include/eunit.hrl").

seat_body_limit_test_() ->
    [
        {"ASCII overflow", fun() -> reject(binary:copy(<<"a">>, 4001), body_too_long) end},
        {"Chinese overflow", fun() -> reject(binary:copy(<<"中"/utf8>>, 4001), body_too_long) end},
        {"Astral overflow", fun() ->
            reject(binary:copy(<<16#1F600/utf8>>, 4001), body_too_long)
        end},
        {"Combining marks", fun() ->
            reject(binary:copy(<<"e", 16#0301/utf8, "\n">>, 1334), body_too_long)
        end},
        {"Invalid UTF-8", fun() -> reject(<<16#FF>>, body_invalid_utf8) end},
        {"Incomplete UTF-8", fun() -> reject(<<16#E4, 16#B8>>, body_invalid_utf8) end}
    ].

boundary_test_() ->
    [
        {"4000 ASCII code points", fun() -> accept(binary:copy(<<"a">>, 4000)) end},
        {"4000 Chinese code points", fun() -> accept(binary:copy(<<"中"/utf8>>, 4000)) end},
        {"4000 astral code points", fun() -> accept(binary:copy(<<16#1F600/utf8>>, 4000)) end},
        {"Attachment-only remains allowed", fun() -> accept(<<>>) end},
        {"Visitor contract unchanged", fun() ->
            ?assertEqual(
                ok,
                cs_seat_message_body:validate(#{
                    sender_type => <<"contact">>, body => binary:copy(<<"a">>, 4001)
                })
            )
        end}
    ].

accept(Body) ->
    ?assertEqual(
        ok, cs_seat_message_body:validate(#{sender_type => <<"business_identity">>, body => Body})
    ).

reject(Body, Reason) ->
    ok = cs_fake_store:init(),
    ok = cs_fake_canonical_tx:reset(),
    Session = #{
        id => 12,
        organization_id => 1,
        workspace_id => 2,
        conversation_id => 3,
        contact_id => 4,
        business_identity_id => 5,
        status => active,
        version => 1,
        queued_at => 1700000000
    },
    ok = cs_fake_store:put_session_for_list(Session),
    Params = #{
        workspace_id => 2,
        session_id => 12,
        business_identity_id => 5,
        actor_user_id => 6,
        client_msg_id => <<"body-limit-test">>,
        body => Body,
        store => cs_fake_store,
        id => cs_fake_id,
        org_lifecycle => cs_fake_org_lifecycle,
        canonical_tx => cs_fake_canonical_tx,
        key_ref => #{key => <<"k">>, key_version => 1},
        accepted_at => 1700000010
    },
    try
        ?assertEqual(
            {error, {invalid_argument, Reason}}, cs_session_app:append_session_message(1, Params)
        ),
        ?assertEqual([], cs_fake_canonical_tx:calls()),
        ?assertEqual([], cs_fake_store:events())
    after
        cs_fake_store:destroy(),
        cs_fake_canonical_tx:reset()
    end.
