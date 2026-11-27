-module(imboy_env_policy_tests).

-include_lib("eunit/include/eunit.hrl").

delivery_profile_env_override_test() ->
    SavedApp = [{product_profile, application:get_env(imboy, product_profile)}],
    SavedEnv = [{"IMBOY_PRODUCT_PROFILE", os:getenv("IMBOY_PRODUCT_PROFILE")}],
    try
        lists:foreach(
            fun({Raw, Expected}) ->
                os:putenv("IMBOY_PRODUCT_PROFILE", Raw),
                ok = imboy_env:override_from_env(),
                ?assertEqual({ok, Expected}, application:get_env(imboy, product_profile))
            end,
            [
                {"phase1_delivery", phase1_delivery},
                {" PHASE1_DELIVERY ", phase1_delivery},
                {"overseas_baseline", overseas_baseline},
                {"standard", standard},
                {"audited", audited},
                {"enterprise", audited}
            ]
        ),
        os:putenv("IMBOY_PRODUCT_PROFILE", "phase1_typo"),
        ?assertError(
            {invalid_env, "IMBOY_PRODUCT_PROFILE", "phase1_typo"},
            imboy_env:override_from_env()
        )
    after
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

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
        ?assertEqual({ok, standard}, application:get_env(imboy, product_profile)),
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

cluster_nodes_env_override_test() ->
    SavedApp = [{cluster_nodes, application:get_env(imboy, cluster_nodes)}],
    SavedEnv = [{"IMBOY_CLUSTER_NODES", os:getenv("IMBOY_CLUSTER_NODES")}],
    Configured = ['existing1@cluster.example', 'existing2@cluster.example'],
    try
        application:set_env(imboy, cluster_nodes, Configured),
        os:unsetenv("IMBOY_CLUSTER_NODES"),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, Configured}, application:get_env(imboy, cluster_nodes)),
        os:putenv("IMBOY_CLUSTER_NODES", ""),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, Configured}, application:get_env(imboy, cluster_nodes)),
        os:putenv("IMBOY_CLUSTER_NODES", " [] "),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, []}, application:get_env(imboy, cluster_nodes)),
        os:putenv("IMBOY_CLUSTER_NODES", "[\"node1@127.0.0.1\",\"node2@cluster.example\"]"),
        ok = imboy_env:override_from_env(),
        ?assertEqual(
            {ok, ['node1@127.0.0.1', 'node2@cluster.example']},
            application:get_env(imboy, cluster_nodes)
        ),
        application:set_env(imboy, cluster_nodes, Configured),
        lists:foreach(
            fun(Raw) ->
                os:putenv("IMBOY_CLUSTER_NODES", Raw),
                ?assertError(
                    {invalid_env, "IMBOY_CLUSTER_NODES", Raw},
                    imboy_env:override_from_env()
                ),
                ?assertEqual({ok, Configured}, application:get_env(imboy, cluster_nodes))
            end,
            [
                "not-json",
                "{}",
                "null",
                "[1]",
                "[\"\"]",
                "[\"node\"]",
                "[\"@host\"]",
                "[\"node@\"]",
                "[\"node@host@other\"]",
                "[\"node @host\"]"
            ]
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

%% 公开资源 URL 必须使用本实例地址；S3 签名地址与存储桶保持独立。
%% Public resource URLs use this instance's origin, independently of S3 signing.
garage_public_base_url_env_override_test() ->
    EnvKeys = [
        "IMBOY_GARAGE_ENDPOINT",
        "IMBOY_GARAGE_PUBLIC_ENDPOINT",
        "IMBOY_GARAGE_PUBLIC_BASE_URL",
        "IMBOY_GARAGE_BUCKET",
        "IMBOY_GARAGE_PUBLIC_BUCKET",
        "IMBOY_GARAGE_ACCESS_KEY",
        "IMBOY_GARAGE_SECRET_KEY"
    ],
    SavedApp = [{garage, application:get_env(imboy, garage)}],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- EnvKeys],
    Garage = #{
        endpoint => <<"http://garage:3900">>,
        public_endpoint => <<"http://api.localhost:8080/s3">>,
        public_base_url => <<"https://s3.example.com">>,
        bucket => <<"imboy">>,
        public_bucket => <<"imboy-public">>
    },
    try
        lists:foreach(fun os:unsetenv/1, EnvKeys),
        application:set_env(imboy, garage, Garage),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, Garage}, application:get_env(imboy, garage)),
        os:putenv("IMBOY_GARAGE_PUBLIC_BASE_URL", "http://s3.localhost:8080"),
        ok = imboy_env:override_from_env(),
        ?assertEqual(
            {ok, Garage#{public_base_url := <<"http://s3.localhost:8080">>}},
            application:get_env(imboy, garage)
        ),
        ?assertEqual(
            <<"http://s3.localhost:8080/public/avatar.png">>,
            elib_oss:public_url_for_key(<<"public/avatar.png">>)
        ),
        %% Empty optional env values must preserve sys.config, as for other Garage keys.
        os:putenv("IMBOY_GARAGE_PUBLIC_BASE_URL", ""),
        application:set_env(imboy, garage, Garage),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, Garage}, application:get_env(imboy, garage))
    after
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

%% ===================================================================
%% BE-W01 A01：IMBOY_CS_WIDGET_* 映射 + _FILE（0600）fail-closed 合同
%% ===================================================================

cs_widget_env_keys() ->
    [
        cs_widget_subject_key,
        cs_widget_identity_keys,
        cs_widget_intake_business_identity_id
    ].

cs_widget_os_keys() ->
    [
        "IMBOY_CS_WIDGET_SUBJECT_KEY",
        "IMBOY_CS_WIDGET_SUBJECT_KEY_FILE",
        "IMBOY_CS_WIDGET_IDENTITY_KEYS",
        "IMBOY_CS_WIDGET_IDENTITY_KEYS_FILE",
        "IMBOY_CS_WIDGET_INTAKE_BUSINESS_IDENTITY_ID"
    ].

cs_widget_direct_env_override_test() ->
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- cs_widget_env_keys()],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- cs_widget_os_keys()],
    try
        os:putenv("IMBOY_CS_WIDGET_SUBJECT_KEY", "subject-hmac-material"),
        os:putenv("IMBOY_CS_WIDGET_IDENTITY_KEYS", "1:k-one,2:k-two"),
        os:putenv("IMBOY_CS_WIDGET_INTAKE_BUSINESS_IDENTITY_ID", "424242"),
        ok = imboy_env:override_from_env(),
        ?assertEqual(
            {ok, <<"subject-hmac-material">>},
            application:get_env(imboy, cs_widget_subject_key)
        ),
        %% identity keys 严格解析为 [{Version, Key}] proplist（消费方
        %% cs_identity_assertion:provisioned_keys/2 双形态均可吃）。
        ?assertEqual(
            {ok, [{1, <<"k-one">>}, {2, <<"k-two">>}]},
            application:get_env(imboy, cs_widget_identity_keys)
        ),
        ?assertEqual(
            {ok, 424242},
            application:get_env(imboy, cs_widget_intake_business_identity_id)
        )
    after
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

cs_widget_identity_keys_garbage_fails_fast_test() ->
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- cs_widget_env_keys()],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- cs_widget_os_keys()],
    try
        %% 非法条目必须启动期吵闹（消费方对未知条目是静默忽略——fail-open，
        %% 装配层必须把住 fail-closed 关）。
        os:putenv("IMBOY_CS_WIDGET_IDENTITY_KEYS", "1:ok,not-a-pair"),
        ?assertError(
            {invalid_env, "IMBOY_CS_WIDGET_IDENTITY_KEYS", _},
            imboy_env:override_from_env()
        )
    after
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

temp_secret_file(Name, Content, Mode) ->
    Path = filename:join(
        "/tmp", Name ++ "-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:write_file(Path, Content),
    ok = file:change_mode(Path, Mode),
    Path.

edge_internal_token_override_test() ->
    Keys = ["IMBOY_EDGE_INTERNAL_TOKEN", "IMBOY_EDGE_INTERNAL_TOKEN_FILE"],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- Keys],
    SavedApp = [{edge_internal_token, application:get_env(imboy, edge_internal_token)}],
    Path = temp_secret_file("imboy-edge-token", <<"test-token\n">>, 8#600),
    try
        [os:unsetenv(Key) || Key <- Keys],
        os:putenv("IMBOY_EDGE_INTERNAL_TOKEN_FILE", Path),
        ok = imboy_env_overrides:override_edge_token(),
        ?assertEqual({ok, <<"test-token">>}, application:get_env(imboy, edge_internal_token)),
        os:putenv("IMBOY_EDGE_INTERNAL_TOKEN", "other"),
        ?assertError(
            {conflicting_env, "IMBOY_EDGE_INTERNAL_TOKEN", "IMBOY_EDGE_INTERNAL_TOKEN_FILE"},
            imboy_env_overrides:override_edge_token()
        )
    after
        _ = file:delete(Path),
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

cs_widget_file_variant_reads_0600_content_test() ->
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- cs_widget_env_keys()],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- cs_widget_os_keys()],
    Path = temp_secret_file("imboy-csww-subject.key", <<"file-material-1\n">>, 8#600),
    try
        os:putenv("IMBOY_CS_WIDGET_SUBJECT_KEY_FILE", Path),
        ok = imboy_env:override_from_env(),
        %% 文件内容为值（尾部换行剥除；值不进日志/错误项）。
        ?assertEqual(
            {ok, <<"file-material-1">>},
            application:get_env(imboy, cs_widget_subject_key)
        )
    after
        _ = file:delete(Path),
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

cs_widget_file_variant_rejects_loose_permissions_test() ->
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- cs_widget_env_keys()],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- cs_widget_os_keys()],
    Path = temp_secret_file("imboy-csww-subject-loose.key", <<"x">>, 8#644),
    try
        os:putenv("IMBOY_CS_WIDGET_SUBJECT_KEY_FILE", Path),
        ?assertError(
            {secret_file_permissions, "IMBOY_CS_WIDGET_SUBJECT_KEY_FILE", _},
            imboy_env:override_from_env()
        )
    after
        _ = file:delete(Path),
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

cs_widget_file_variant_missing_file_fails_fast_test() ->
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- cs_widget_env_keys()],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- cs_widget_os_keys()],
    try
        os:putenv("IMBOY_CS_WIDGET_SUBJECT_KEY_FILE", "/tmp/imboy-csww-definitely-missing.key"),
        ?assertError(
            {secret_file_unreadable, "IMBOY_CS_WIDGET_SUBJECT_KEY_FILE", _},
            imboy_env:override_from_env()
        )
    after
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

cs_widget_direct_and_file_conflict_fails_closed_test() ->
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- cs_widget_env_keys()],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- cs_widget_os_keys()],
    Path = temp_secret_file("imboy-csww-both.key", <<"x">>, 8#600),
    try
        os:putenv("IMBOY_CS_WIDGET_SUBJECT_KEY", "direct-value"),
        os:putenv("IMBOY_CS_WIDGET_SUBJECT_KEY_FILE", Path),
        %% 双源同给 = 装配歧义，fail-closed 报错（不猜优先级）。
        ?assertError(
            {conflicting_env, "IMBOY_CS_WIDGET_SUBJECT_KEY", "IMBOY_CS_WIDGET_SUBJECT_KEY_FILE"},
            imboy_env:override_from_env()
        )
    after
        _ = file:delete(Path),
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

cs_widget_identity_keys_file_variant_test() ->
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- cs_widget_env_keys()],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- cs_widget_os_keys()],
    Path = temp_secret_file("imboy-csww-identity.keys", <<"3:k-three">>, 8#600),
    try
        os:putenv("IMBOY_CS_WIDGET_IDENTITY_KEYS_FILE", Path),
        ok = imboy_env:override_from_env(),
        ?assertEqual(
            {ok, [{3, <<"k-three">>}]},
            application:get_env(imboy, cs_widget_identity_keys)
        )
    after
        _ = file:delete(Path),
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

%% BE-W01：CORS 分面 origin 名单的 env 注入（逗号分隔）。
cors_face_origins_env_keys() ->
    [cors_widget_origins, cors_admin_origins].

cors_face_origins_env_override_test() ->
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- cors_face_origins_env_keys()],
    SavedEnv = [
        {Key, os:getenv(Key)}
     || Key <- ["IMBOY_CORS_WIDGET_ORIGINS", "IMBOY_CORS_ADMIN_ORIGINS"]
    ],
    try
        os:putenv("IMBOY_CORS_WIDGET_ORIGINS", "https://cs.example.com, https://cs2.example.com"),
        os:putenv("IMBOY_CORS_ADMIN_ORIGINS", "https://adm.example.com"),
        ok = imboy_env:override_from_env(),
        ?assertEqual(
            {ok, [<<"https://cs.example.com">>, <<"https://cs2.example.com">>]},
            application:get_env(imboy, cors_widget_origins)
        ),
        ?assertEqual(
            {ok, [<<"https://adm.example.com">>]},
            application:get_env(imboy, cors_admin_origins)
        )
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
