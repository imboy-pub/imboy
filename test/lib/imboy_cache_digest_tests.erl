%% Fingerprint optimization must preserve wire compatibility and live policy changes.
-module(imboy_cache_digest_tests).
-include_lib("eunit/include/eunit.hrl").

fingerprints_test_() ->
    {setup, fun save_env/0, fun restore_env/1, fun(_) ->
        [
            ?_test(budget_matches_public_limits()),
            ?_test(live_changes_are_immediate()),
            ?_test(cache_storage_is_bounded()),
            ?_test(large_inputs_bypass_storage()),
            ?_test(invalid_budget_values_keep_defaults()),
            ?_test(replication_authorization_is_not_cached())
        ]
    end}.

save_env() ->
    [{K, application:get_env(imboy, K)} || K <- env_keys()].
restore_env(Saved) ->
    lists:foreach(fun
        ({K, undefined}) -> application:unset_env(imboy, K);
        ({K, {ok, V}}) -> application:set_env(imboy, K, V)
    end, Saved).
env_keys() -> [cache_replication, cache_registry_overrides, cache_fenced_namespaces].
reset() ->
    application:set_env(imboy, cache_replication, #{}),
    application:set_env(imboy, cache_registry_overrides, #{}),
    application:set_env(imboy, cache_fenced_namespaces, []).

budget_matches_public_limits() ->
    reset(),
    Configs = [#{}, #{n_max => 100000, max_ttl_sec => 1000},
        #{recovery_window_sec => 900, gen_max_bytes => 17000, peer_max => 3},
        #{n_max => 0, p_max => -1, b_max_bytes => invalid}],
    lists:foreach(fun(Config) ->
        application:set_env(imboy, cache_replication, Config),
        ?assertEqual(reference_budget(), imboy_cache_policy:budget_digest()),
        ?assertEqual(reference_budget(), imboy_cache_policy:budget_digest())
    end, Configs).

%% Independently uses the existing public getters, including derived defaults.
reference_budget() ->
    Names = [n_max, n_max_bytes, t_max, t_max_bytes, j_max, grace_max, peer_max,
        j_max_bytes, b_max, b_max_bytes, p_max, gen_max_bytes, p_max_bytes,
        r_max_rounds, d_max_ms, max_value_bytes, protocol_meta_max_bytes,
        max_ttl_sec, high_watermark, grace_window_ms],
    Parts = [{Name, apply(imboy_cache_policy, Name, [])} || Name <- Names],
    <<Digest:16/binary, _/binary>> = crypto:hash(sha256,
        term_to_binary(Parts, [{minor_version, 2}])),
    Digest.

live_changes_are_immediate() ->
    reset(),
    Original = fingerprints(),
    lists:foreach(fun({Key, Value}) ->
        reset(),
        _ = fingerprints(),
        application:set_env(imboy, Key, Value),
        Changed = fingerprints(),
        ?assertNotEqual(Original, Changed),
        ?assertEqual(Changed, fingerprints()),
        reset(),
        ?assertEqual(Original, fingerprints())
    end, [{cache_replication, #{max_ttl_sec => 2000}},
        {cache_replication, #{whitelist => [digest_test]}},
        {cache_fenced_namespaces, [digest_test]},
        {cache_registry_overrides, #{digest_test => {replicate, false}}}]).

cache_storage_is_bounded() ->
    reset(),
    lists:foreach(fun(I) ->
        application:set_env(imboy, cache_replication, #{n_max => I}),
        application:set_env(imboy, cache_registry_overrides,
            #{digest_test => {local, I rem 2 =:= 0}}),
        _ = fingerprints()
    end, lists:seq(1, 100)),
    Slots = [K || {K = {imboy_cache_policy, digest, _}, _} <- erlang:get()],
    ?assertEqual(2, length(Slots)),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, budget})),
    %% Retain only a code token rather than a local fun.
    [?assert(is_binary(CodeVersion)) || K <- Slots,
        {_, CodeVersion, _} <- [erlang:get(K)]].

large_inputs_bypass_storage() ->
    reset(),
    _ = fingerprints(),
    %% Unknown fields do not affect the wire digest, but must not be retained
    %% merely because they occur in an otherwise valid configuration map.
    Original = fingerprints(),
    application:set_env(imboy, cache_replication,
        #{unused_large_field => binary:copy(<<0>>, 8192)}),
    ?assertEqual(Original, fingerprints()),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, budget})),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, config})),
    %% ETF string packing must not admit a large caller-retained list heap.
    application:set_env(imboy, cache_replication,
        #{unused_compact_string => lists:duplicate(3000, $x)}),
    ?assertEqual(Original, fingerprints()),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, budget})),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, config})),
    %% Small sub-binaries must not retain oversized backing binaries.
    Backing = binary:copy(<<0>>, 65536),
    Slice = binary:part(Backing, 0, 128),
    ?assert(binary:referenced_byte_size(Slice) > 4096),
    application:set_env(imboy, cache_replication, #{unused_slice => Slice}),
    ?assertEqual(Original, fingerprints()),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, budget})),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, config})),
    %% A large, valid list independently bypasses storage.
    application:set_env(imboy, cache_replication, #{}),
    application:set_env(imboy, cache_fenced_namespaces, lists:duplicate(4096, digest_test)),
    Changed = fingerprints(),
    ?assertNotEqual(Original, Changed),
    ?assertEqual(Changed, fingerprints()),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, config})),
    reset(),
    %% Registry hashing preserves the entire override map, even unused keys.
    application:set_env(imboy, cache_registry_overrides,
        #{binary:copy(<<"unused">>, 1000) => {local, false}}),
    RegistryChanged = fingerprints(),
    ?assertNotEqual(Original, RegistryChanged),
    ?assertEqual(RegistryChanged, fingerprints()),
    ?assertEqual(undefined, erlang:get({imboy_cache_policy, digest, registry})),
    reset(),
    ?assertEqual(Original, fingerprints()).

invalid_budget_values_keep_defaults() ->
    reset(),
    Original = fingerprints(),
    application:set_env(imboy, cache_replication,
        #{n_max => 0, max_ttl_sec => -1, p_max => invalid}),
    ?assertEqual(Original, fingerprints()).

replication_authorization_is_not_cached() ->
    reset(),
    Key = {digest_test, 1},
    application:set_env(imboy, cache_registry_overrides,
        #{digest_test => {replicate, false}}),
    application:set_env(imboy, cache_replication, #{whitelist => [digest_test]}),
    _ = fingerprints(),
    ?assert(imboy_cache_policy:replicate_allowed(Key)),
    application:set_env(imboy, cache_replication, #{}),
    ?assertNot(imboy_cache_policy:replicate_allowed(Key)),
    application:set_env(imboy, cache_replication, #{whitelist => [digest_test]}),
    application:set_env(imboy, cache_registry_overrides,
        #{digest_test => {replicate, true}}),
    ?assertNot(imboy_cache_policy:replicate_allowed(Key)).

fingerprints() ->
    {imboy_cache_policy:budget_digest(), imboy_cache_policy:config_hash(),
        imboy_cache_policy:registry_hash()}.
