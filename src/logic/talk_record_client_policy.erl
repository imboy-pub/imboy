-module(talk_record_client_policy).
-moduledoc "Explicit deployment BYOK allow policy, separate from local model configuration.".
-export([get/0, public/0, allowed/2, token/5, for_record/1]).

get() ->
    Config = config_ds:env(imboy, talk_record_client_processing, #{}),
    case Config of
        #{enabled := true, signing_key := Key, policy_version := PV} when
            is_binary(Key),
            byte_size(Key) >= 32,
            is_binary(PV),
            byte_size(PV) > 0,
            byte_size(PV) =< 64
        ->
            Providers = #{
                <<"transcribe">> => #{
                    <<"provider">> => <<"minimax">>, <<"model">> => <<"asr-1.0">>
                },
                <<"summarize">> => #{
                    <<"provider">> => <<"zhipu">>, <<"model">> => <<"glm-5.3-flash">>
                }
            },
            Public = #{
                <<"available">> => true,
                <<"execution_mode">> => <<"client_byok">>,
                <<"policy_version">> => PV,
                <<"providers">> => Providers
            },
            {ok, Config#{
                public => Public#{<<"config_version">> => talk_record_client_contract:hash(Public)}
            }};
        _ ->
            {error, processing_unavailable}
    end.

public() ->
    case ?MODULE:get() of
        {ok, P} -> maps:get(public, P);
        _ -> #{<<"available">> => false}
    end.

allowed(#{<<"scope">> := <<"personal">>}, Config) ->
    maps:get(personal_enabled, Config, true) =:= true;
allowed(#{<<"scope">> := <<"organization">>, <<"organization_id">> := Org}, Config) ->
    Orgs = maps:get(enterprise_organizations, Config, []),
    is_list(Orgs) andalso lists:member(Org, Orgs);
allowed(_, _) ->
    false.

token(Key, RunId, Attempt, Uid, Did) ->
    Digest = crypto:mac(
        hmac,
        sha256,
        Key,
        term_to_binary({talk_record_client_execution_v1, RunId, Attempt, Uid, Did})
    ),
    binary:replace(
        binary:replace(base64:encode(Digest), <<"+">>, <<"-">>, [global]),
        <<"/">>,
        <<"_">>,
        [global]
    ).

for_record(Record) ->
    case ?MODULE:get() of
        {ok, Config} -> (maps:get(public, Config))#{<<"available">> => allowed(Record, Config)};
        _ -> #{<<"available">> => false}
    end.
