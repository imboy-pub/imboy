-module(talk_record_provider_policy_tests).
-include_lib("eunit/include/eunit.hrl").

existing_minimax_policy_stays_compatible_test() ->
    with_config(#{}, <<>>, fun(Public) ->
        ?assertMatch({ok, #{provider := <<"minimax">>}}, request(Public, <<"transcribe">>)),
        Providers = maps:get(<<"providers">>, Public),
        Selected = maps:get(<<"transcribe">>, Providers),
        Forged = Public#{
            <<"providers">> => Providers#{
                <<"transcribe">> =>
                    Selected#{<<"provider">> => <<"arbitrary">>}
            }
        },
        ?assertEqual({error, client_schema_invalid}, request(Forged, <<"transcribe">>))
    end).

doubao_requires_source_authority_before_admission_test() ->
    lists:foreach(
        fun({Extra, Origin}) ->
            with_config(Extra#{doubao_enabled => true}, Origin, fun(Public) ->
                ?assertEqual({error, client_schema_invalid}, request(Public, <<"transcribe">>)),
                ?assertMatch({ok, #{provider := <<"zhipu">>}}, request(Public, <<"summarize">>))
            end)
        end,
        [
            {#{}, <<"https://audio.example">>},
            {#{audio_source_enabled => true}, <<"http://audio.example">>}
        ]
    ).

explicit_ready_doubao_uses_pinned_async_policy_test() ->
    with_config(
        #{doubao_enabled => true, audio_source_enabled => true},
        <<"https://audio.example">>,
        fun(Public) ->
            ?assertMatch({ok, #{provider := <<"doubao">>}}, request(Public, <<"transcribe">>))
        end
    ).

request(Public, Kind) ->
    Provider = maps:get(Kind, maps:get(<<"providers">>, Public)),
    Pin = (maps:with([<<"policy_version">>, <<"config_version">>], Public))#{
        <<"execution_mode">> => <<"client_byok">>,
        <<"provider">> => maps:get(<<"provider">>, Provider),
        <<"model">> => maps:get(<<"model">>, Provider)
    },
    talk_record_client_contract:request(
        Pin#{
            <<"mode">> => Kind,
            <<"processing_consent">> => Pin#{<<"confirmed">> => true}
        },
        Public
    ).

with_config(Extra, Origin, Fun) ->
    pure_mock_fixture:run(fun() ->
        ok = meck:new(config_ds, [passthrough, no_link]),
        try
            C = Extra#{
                enabled => true,
                policy_version => <<"synthetic-test-v1">>,
                signing_key => binary:copy(<<"x">>, 32)
            },
            meck:expect(config_ds, env, fun(imboy, talk_record_client_processing, _) -> C end),
            meck:expect(config_ds, env, fun(base_url, _) -> Origin end),
            {ok, #{public := Public}} = talk_record_client_policy:get(),
            Fun(Public)
        after
            meck:unload(config_ds)
        end
    end).
