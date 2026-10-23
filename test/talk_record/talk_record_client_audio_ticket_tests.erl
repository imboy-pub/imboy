-module(talk_record_client_audio_ticket_tests).
-include_lib("eunit/include/eunit.hrl").

valid_fixed_window_ticket_test() ->
    {B, Now} = binding(),
    {ok, T, Exp} = talk_record_client_audio_ticket:issue(B, secret(), Now),
    ?assertEqual(maps:get(<<"source_expires_at">>, B), Exp),
    ?assertEqual({ok, B}, talk_record_client_audio_ticket:verify(T, secret(), Now)),
    %% Reconstructing the same source authorization cannot extend or rebind it.
    ?assertEqual({ok, T, Exp}, talk_record_client_audio_ticket:issue(B, secret(), Now + 30)),
    ?assertEqual(denied(), talk_record_client_audio_ticket:verify(T, secret(), Exp)),
    ?assertEqual(denied(), talk_record_client_audio_ticket:issue(B, secret(), Exp)).

wrong_purpose_key_and_tampering_are_rejected_test() ->
    {B, Now} = binding(),
    {ok, T, _} = talk_record_client_audio_ticket:issue(B, secret(), Now),
    ?assertEqual(denied(), talk_record_client_audio_ticket:verify(T, other_secret(), Now)),
    ?assertEqual(
        denied(), talk_record_client_audio_ticket:verify(<<T/binary, "tampered">>, secret(), Now)
    ),
    Derived = crypto:mac(hmac, sha256, secret(), <<"imboy:talk-record-client-audio:v1">>),
    Claims = B#{
        <<"aud">> => <<"attachment_download">>,
        <<"sub">> => <<"attachment_download">>,
        <<"iat">> => maps:get(<<"source_issued_at">>, B),
        <<"exp">> => maps:get(<<"source_expires_at">>, B)
    },
    Foreign = imboy_jwt:sign(Claims, Derived),
    ?assertEqual(denied(), talk_record_client_audio_ticket:verify(Foreign, secret(), Now)),
    {ok, General} = attachment_download_ticket:issue(10, <<"synthetic">>, secret()),
    ?assertEqual(denied(), talk_record_client_audio_ticket:verify(General, secret(), Now)).

unknown_claims_and_long_lifetimes_rejected_test() ->
    {B, Now} = binding(),
    lists:foreach(
        fun(Changed) ->
            ?assertEqual(denied(), talk_record_client_audio_ticket:issue(Changed, secret(), Now))
        end,
        [
            B#{<<"api_key">> => <<"synthetic-secret-must-not-be-returned">>},
            B#{<<"url">> => <<"https://arbitrary">>},
            B#{<<"source_expires_at">> => maps:get(<<"source_issued_at">>, B) + 301},
            B#{<<"deployment_id">> => <<"invalid">>}
        ]
    ),
    ?assertEqual(
        denied(), talk_record_client_audio_ticket:verify(binary:copy(<<"x">>, 8193), secret(), Now)
    ),
    ?assertEqual(denied(), talk_record_client_audio_ticket:issue(B, <<"short">>, Now)).

deployment_context_change_changes_capability_test() ->
    {B, Now} = binding(),
    {ok, T, _} = talk_record_client_audio_ticket:issue(B, secret(), Now),
    {ok, OtherDeployment} = talk_record_client_audio_contract:origin(<<"https://other.example">>),
    Other = B#{<<"deployment_id">> => OtherDeployment},
    {ok, OtherToken, _} = talk_record_client_audio_ticket:issue(Other, secret(), Now),
    ?assertNotEqual(T, OtherToken),
    ?assertEqual({ok, Other}, talk_record_client_audio_ticket:verify(OtherToken, secret(), Now)).

binding() ->
    Now = erlang:system_time(second),
    A = talk_record_client_audio_contract_tests:authority(),
    {ok, B} = talk_record_client_audio_contract:authorize(A, Now),
    {B, Now}.
secret() -> binary:copy(<<"x">>, 32).
other_secret() -> binary:copy(<<"y">>, 32).
denied() -> {error, audio_source_denied}.
