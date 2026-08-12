-module(imboy_env_policy_tests).

-include_lib("eunit/include/eunit.hrl").

sales_policy_env_override_test() ->
    AppKeys = [product_profile, capabilities, features],
    EnvKeys = [
        "IMBOY_PRODUCT_PROFILE",
        "IMBOY_E2EE_MODE",
        "IMBOY_FEATURE_E2EE",
        "IMBOY_FEATURE_CHANNEL",
        "IMBOY_FEATURE_CHANNEL_ORDER"
    ],
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- AppKeys],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- EnvKeys],
    try
        application:set_env(imboy, capabilities, #{e2ee_mode => optional}),
        application:set_env(imboy, features, #{}),
        os:putenv("IMBOY_PRODUCT_PROFILE", "community"),
        os:putenv("IMBOY_E2EE_MODE", "required"),
        os:putenv("IMBOY_FEATURE_E2EE", "true"),
        os:putenv("IMBOY_FEATURE_CHANNEL", "1"),
        os:putenv("IMBOY_FEATURE_CHANNEL_ORDER", "true"),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, community}, application:get_env(imboy, product_profile)),
        ?assertEqual(
            required,
            maps:get(e2ee_mode, application:get_env(imboy, capabilities, #{}))
        ),
        Features = application:get_env(imboy, features, #{}),
        ?assertEqual(#{enabled => true}, maps:get(e2ee, Features)),
        ?assertEqual(#{enabled => true}, maps:get(channel, Features)),
        ?assertEqual(#{enabled => true}, maps:get(channel_order, Features))
    after
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

restore_app_env([]) ->
    ok;
restore_app_env([{Key, undefined} | Rest]) ->
    application:unset_env(imboy, Key),
    restore_app_env(Rest);
restore_app_env([{Key, {ok, Value}} | Rest]) ->
    application:set_env(imboy, Key, Value),
    restore_app_env(Rest).

restore_os_env([]) ->
    ok;
restore_os_env([{Key, false} | Rest]) ->
    os:unsetenv(Key),
    restore_os_env(Rest);
restore_os_env([{Key, Value} | Rest]) ->
    os:putenv(Key, Value),
    restore_os_env(Rest).
