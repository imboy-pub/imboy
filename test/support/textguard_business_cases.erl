-module(textguard_business_cases).
-export([manifest/0]).

manifest() ->
    Common = [
        {channel_allow, [<<"AC-078">>]},
        {channel_block, [<<"AC-078">>]},
        {channel_review, [<<"AC-079">>]},
        {channel_duplicate, [<<"AC-079">>]},
        {channel_edit_block, [<<"AC-078">>]},
        {channel_edit_review, [<<"AC-079">>]},
        {channel_denied, [<<"AC-082">>]},
        {private_channel, [<<"AC-082">>]},
        {encrypted_channel, [<<"AC-082">>]},
        {bad_payload_json, [<<"AC-082">>]},
        {binary_payload_json, [<<"AC-081">>]},
        {incoming_binary_payload_rejected, [<<"AC-082">>]},
        {historical_profile_version, [<<"AC-080">>]},
        {moment_block, [<<"AC-078">>]},
        {moment_review, [<<"AC-079">>]},
        {private_moment, [<<"AC-082">>]},
        {profile_block, [<<"AC-078">>]},
        {profile_review, [<<"AC-079">>]},
        {profile_field_scope, [<<"AC-081">>]},
        {multi_field, [<<"AC-081">>]},
        {alias_review, [<<"AC-083">>]},
        {comma_metadata, [<<"AC-083">>]},
        {original_text, [<<"AC-081">>]},
        {stale_channel_review, [<<"AC-080">>]},
        {stale_profile_review, [<<"AC-080">>]},
        {channel_aba, [<<"AC-080">>]},
        {profile_aba, [<<"AC-080">>]},
        {legacy_profile, [<<"AC-081">>]},
        {explicit_unchecked, [<<"AC-079">>]},
        {matching_channel_review, [<<"AC-080">>]},
        {matching_profile_review, [<<"AC-080">>]},
        {canary_truncation, [<<"AC-079">>, <<"AC-083">>]},
        {second_publication, [<<"AC-083">>]},
        {profile_trigger, [<<"AC-080">>]},
        {migration_roundtrip, [<<"AC-083">>]},
        {{private_e2ee, c2c}, [<<"AC-082">>]},
        {{private_e2ee, c2g}, [<<"AC-082">>]}
    ],
    Basic = [entry(Transport, Kind, Acs) || Transport <- [http, grpc], {Kind, Acs} <- Common],
    Queue = [
        entry(Transport, {queue_failure, Surface, Mode}, [<<"AC-079">>])
     || Transport <- [http, grpc],
        Surface <- [channel, moment, profile],
        Mode <- [id_generation, query_throw, pg_reject]
    ],
    Errors = [
        entry(Transport, {failure, Mode, Code}, [<<"AC-078">>, <<"AC-079">>])
     || Transport <- [http, grpc],
        Mode <- [availability_compat, strict_reject],
        Code <- [
            overloaded,
            not_ready,
            connection_failed,
            timeout,
            invalid_argument,
            invalid_utf8,
            text_too_large,
            normalization_limit,
            match_budget_exceeded,
            unsupported_ruleset,
            protocol_error,
            normalizer_incompatible,
            unauthenticated,
            permission_denied,
            internal_error
        ]
    ],
    Races = [
        entry(http, {race, Kind}, [<<"AC-080">>])
     || Kind <-
            [profile_value, channel_content, channel_timestamp, channel_revoked, channel_status]
    ],
    Deadlines = [
        entry(Transport, {deadline, Policy}, [<<"AC-078">>, <<"AC-079">>])
     || Transport <- [http, grpc], Policy <- [availability_compat, strict_reject]
    ],
    Aba = [
        entry(Transport, {profile_external_aba, Kind}, [<<"AC-080">>])
     || Transport <- [http, grpc], Kind <- [raw_sql, generic_repo, legacy]
    ],
    Bindings = [
        entry(Transport, {review_binding, Surface, Variant}, [<<"AC-080">>, <<"AC-083">>])
     || Transport <- [http, grpc],
        Surface <- [channel, profile],
        Variant <- [
            null_version,
            missing_version,
            number_version,
            short_version,
            wrong_schema,
            malformed_json,
            json_null,
            json_array,
            legacy_empty,
            legacy_missing,
            legacy_json
        ]
    ],
    ProfileEmpty = [
        entry(
            Transport,
            {review_binding, profile, empty_version},
            [<<"AC-080">>, <<"AC-083">>]
        )
     || Transport <- [http, grpc]
    ],
    Basic ++ Queue ++ Errors ++ Races ++ Deadlines ++ Aba ++ Bindings ++ ProfileEmpty.

entry(Transport, Kind, Acceptance) ->
    #{
        id => iolist_to_binary(io_lib:format("~p/~p", [Transport, Kind])),
        transport => Transport,
        kind => Kind,
        acceptance => Acceptance
    }.
