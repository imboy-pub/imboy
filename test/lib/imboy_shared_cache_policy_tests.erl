%% The public shared_cache namespace is reconstructible, opt-in and non-sensitive.
-module(imboy_shared_cache_policy_tests).
-include_lib("eunit/include/eunit.hrl").

shared_cache_test_() ->
    {setup, fun() ->
        [{K, application:get_env(imboy, K)} || K <- [cache_replication, cache_registry_overrides]]
    end, fun(Saved) ->
        lists:foreach(fun
            ({K, undefined}) -> application:unset_env(imboy, K);
            ({K, {ok, V}}) -> application:set_env(imboy, K, V)
        end, Saved)
    end, fun(_) -> [
        ?_test(opt_in_without_registry_override()),
        ?_test(bounds_and_types()),
        ?_test(other_namespaces_stay_protected())
    ] end}.

reset() ->
    application:set_env(imboy, cache_registry_overrides, #{}),
    application:set_env(imboy, cache_replication, #{}).

opt_in_without_registry_override() ->
    reset(),
    K = {shared_cache, <<"rebuildable-search-result">>},
    ?assertEqual(replicate, imboy_cache_policy:policy(K)),
    ?assertNot(imboy_cache_policy:is_sensitive(K)),
    ?assertEqual({error, replicate_not_authorized}, imboy_cache_policy:validate_replicate(K, <<1>>, 30)),
    application:set_env(imboy, cache_replication, #{whitelist => [shared_cache]}),
    ?assertEqual(ok, imboy_cache_policy:validate_replicate(K, <<1>>, 30)),
    application:set_env(imboy, cache_replication, #{}),
    ?assertNot(imboy_cache_policy:replicate_allowed(K)).

bounds_and_types() ->
    reset(),
    application:set_env(imboy, cache_replication,
        #{whitelist => [shared_cache], max_ttl_sec => 60, max_value_bytes => 64}),
    K = {shared_cache, bounded},
    ?assertEqual(ok, imboy_cache_policy:validate_replicate(K, <<1>>, 60)),
    ?assertEqual({error, ttl_out_of_range}, imboy_cache_policy:validate_replicate(K, <<1>>, 61)),
    ?assertEqual({error, ttl_out_of_range}, imboy_cache_policy:validate_replicate(K, <<1>>, 0)),
    ?assertEqual({error, value_too_large}, imboy_cache_policy:validate_replicate(K, binary:copy(<<0>>, 65), 60)),
    ?assertEqual({error, key_too_large}, imboy_cache_policy:validate_replicate({shared_cache, binary:copy(<<0>>, 65)}, <<1>>, 60)),
    ?assertEqual({error, value_type_forbidden}, imboy_cache_policy:validate_replicate(K, self(), 60)),
    ?assertEqual({error, invalid_value}, imboy_cache_policy:validate_replicate(K, undefined, 60)).

other_namespaces_stay_protected() ->
    reset(),
    Names = [qr_login, qr_login_qr, auth_session_epoch, local_cache, unregistered],
    application:set_env(imboy, cache_replication, #{whitelist => [shared_cache | Names]}),
    lists:foreach(fun(NS) ->
        ?assertNot(imboy_cache_policy:replicate_allowed({NS, probe}))
    end, Names),
    ?assertEqual(blocked, imboy_cache_policy:policy({qr_login, probe})),
    ?assertEqual(local, imboy_cache_policy:policy({local_cache, probe})),
    ?assertEqual(invalidate, imboy_cache_policy:policy({auth_session_epoch, probe})).
