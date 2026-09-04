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

auto_migrate_env_override_test() ->
    SavedApp = [{auto_migrate, application:get_env(imboy, auto_migrate)}],
    SavedEnv = [{"IMBOY_AUTO_MIGRATE", os:getenv("IMBOY_AUTO_MIGRATE")}],
    try
        lists:foreach(
            fun({Raw, Expected}) ->
                os:putenv("IMBOY_AUTO_MIGRATE", Raw),
                ok = imboy_env:override_from_env(),
                ?assertEqual({ok, Expected}, application:get_env(imboy, auto_migrate))
            end,
            [{"true", true}, {"1", true}, {"false", false}, {"0", false}]
        ),
        os:putenv("IMBOY_AUTO_MIGRATE", "enabled"),
        ?assertError(
            {invalid_env, "IMBOY_AUTO_MIGRATE", "enabled"},
            imboy_env:override_from_env()
        )
    after
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

smtp_and_sms_env_override_test() ->
    AppKeys = [smtp_option, sms, yjsms_url, jsms_temp_id, jsms_sign_id],
    EnvKeys = [
        "IMBOY_SMTP_RELAY",
        "IMBOY_SMTP_PORT",
        "IMBOY_SMTP_SSL",
        "IMBOY_SMTP_FROM",
        "IMBOY_SMS_SWITCH",
        "IMBOY_SMS_PLATFORM",
        "IMBOY_YJSMS_URL",
        "IMBOY_JSMS_TEMP_ID",
        "IMBOY_JSMS_SIGN_ID"
    ],
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- AppKeys],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- EnvKeys],
    try
        application:set_env(imboy, smtp_option, []),
        application:set_env(imboy, sms, [{switch, <<"off">>}, {platform, <<"yjsms">>}]),
        os:putenv("IMBOY_SMTP_RELAY", "smtp.example.com"),
        os:putenv("IMBOY_SMTP_PORT", "465"),
        os:putenv("IMBOY_SMTP_SSL", "true"),
        os:putenv("IMBOY_SMTP_FROM", "noreply@example.com"),
        os:putenv("IMBOY_SMS_SWITCH", "on"),
        os:putenv("IMBOY_SMS_PLATFORM", "jsms"),
        os:putenv("IMBOY_YJSMS_URL", "https://sms.example.com"),
        os:putenv("IMBOY_JSMS_TEMP_ID", "template-1"),
        os:putenv("IMBOY_JSMS_SIGN_ID", "sign-1"),
        ok = imboy_env:override_from_env(),
        Smtp = application:get_env(imboy, smtp_option, []),
        ?assertEqual("smtp.example.com", proplists:get_value(relay, Smtp)),
        ?assertEqual(465, proplists:get_value(port, Smtp)),
        ?assertEqual(true, proplists:get_value(ssl, Smtp)),
        ?assertEqual("noreply@example.com", proplists:get_value(from, Smtp)),
        Sms = application:get_env(imboy, sms, []),
        ?assertEqual(<<"on">>, proplists:get_value(switch, Sms)),
        ?assertEqual(<<"jsms">>, proplists:get_value(platform, Sms)),
        ?assertEqual({ok, <<"https://sms.example.com">>}, application:get_env(imboy, yjsms_url)),
        ?assertEqual({ok, <<"template-1">>}, application:get_env(imboy, jsms_temp_id)),
        ?assertEqual({ok, <<"sign-1">>}, application:get_env(imboy, jsms_sign_id))
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
