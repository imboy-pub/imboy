-module(imboy_secret_policy_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% Task 12 / LT-06：三类 secret（jwt_key / postgre_aes_key / adm_cookie_secret）
%%% 统一校验矩阵。验收 LT-06-A01：交付（strict）profile 下缺失/相同/过短一律 fail-closed；
%%% 开发 profile 由 imboy_app:ensure_* 派生 node-local 值，但不得回落公开常量。

-define(MIN, imboy_secret_policy:min_length()).

long_a() ->
    binary:copy(<<"a">>, ?MIN).

long_b() ->
    binary:copy(<<"b">>, ?MIN).

long_c() ->
    binary:copy(<<"c">>, ?MIN).

%% 缺失：键未设置
missing_key_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:unset_env(imboy, jwt_key),
        set(postgre_aes_key, long_a()),
        set(adm_cookie_secret, long_b()),
        ?assertEqual(
            {error, {missing_required_config, jwt_key}},
            imboy_secret_policy:validate_strict()
        )
    end).

%% 缺失：空串占位（sys.runtime.config 形态）视同未配置
empty_key_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        set(jwt_key, <<>>),
        set(postgre_aes_key, long_a()),
        set(adm_cookie_secret, long_b()),
        ?assertEqual(
            {error, {missing_required_config, jwt_key}},
            imboy_secret_policy:validate_strict()
        )
    end).

%% 过短：三类 key 逐一 < min_length 拒绝
too_short_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        Short = binary:part(long_a(), 0, ?MIN - 1),
        lists:foreach(
            fun(Key) ->
                Others = [K || K <- required(), K =/= Key],
                set(hd(Others), long_b()),
                set(lists:last(Others), long_c()),
                set(Key, Short),
                ?assertEqual(
                    {error, {secret_too_short, Key, ?MIN - 1}},
                    imboy_secret_policy:validate_strict()
                )
            end,
            required()
        )
    end).

%% 相同：两两相等均拒绝（三对逐一）
pairwise_equal_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        [K1, K2, K3] = required(),
        set(K1, long_a()),
        set(K2, long_a()),
        set(K3, long_c()),
        ?assertEqual(
            {error, {secrets_must_differ, K1, K2}},
            imboy_secret_policy:validate_strict()
        ),
        set(K2, long_b()),
        set(K3, long_b()),
        ?assertEqual(
            {error, {secrets_must_differ, K2, K3}},
            imboy_secret_policy:validate_strict()
        ),
        set(K3, long_a()),
        set(K2, long_c()),
        ?assertEqual(
            {error, {secrets_must_differ, K1, K3}},
            imboy_secret_policy:validate_strict()
        )
    end).

%% 合格：三者齐备、达长、互异 → ok
distinct_long_keys_accepted_test_() ->
    ?TEST_SIMPLE(fun() ->
        set(jwt_key, long_a()),
        set(postgre_aes_key, long_b()),
        set(adm_cookie_secret, long_c()),
        ?assertEqual(ok, imboy_secret_policy:validate_strict())
    end).

%% RED 复现位（修复前）：adm_cookie_secret 未配置时 signing_key/0 回落公开常量
%% <<"imboy-adm-cookie">> —— 公开已知 HMAC 密钥可伪造任意管理员会话。
%% 修复后：启动期 ensure 派生 node-local 值；signing_key 对"仍为空"fail-loud。
dev_signing_key_never_public_constant_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:unset_env(imboy, adm_cookie_secret),
        ?assertNotEqual(<<"imboy-adm-cookie">>, adm_auth_middleware:signing_key()),
        ?assertNotEqual(<<>>, adm_auth_middleware:signing_key())
    end).

%% 启动期 dev 派生：三值齐备且互异（不同的派生盐），且均非公开常量
dev_defaults_derived_and_distinct_test_() ->
    ?TEST_SIMPLE(fun() ->
        lists:foreach(fun(K) -> application:unset_env(imboy, K) end, required()),
        ok = imboy_secret_policy:ensure_dev_defaults(),
        [Jwt, Aes, Adm] = [env(K) || K <- required()],
        ?assertNotEqual(<<>>, Jwt),
        ?assertNotEqual(<<>>, Aes),
        ?assertNotEqual(<<>>, Adm),
        ?assertNotEqual(Jwt, Aes),
        ?assertNotEqual(Jwt, Adm),
        ?assertNotEqual(Aes, Adm),
        ?assertEqual(ok, imboy_secret_policy:validate_strict())
    end).

%% 启动链缺席（仅测试/降级环境可达）：signing_key 派生 node-local 非公开稳定值
unconfigured_signing_key_derives_nonpublic_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:unset_env(imboy, adm_cookie_secret),
        K1 = adm_auth_middleware:signing_key(),
        ?assertNotEqual(<<>>, K1),
        ?assertNotEqual(<<"imboy-adm-cookie">>, K1),
        %% 落 env 后同进程稳定：再次读取为同一值（签名可复现验证的前提）
        ?assertEqual(K1, adm_auth_middleware:signing_key())
    end).

%% ===================================================================
%% Internal
%% ===================================================================

required() ->
    imboy_secret_policy:required_keys().

set(Key, Value) ->
    application:set_env(imboy, Key, Value).

env(Key) ->
    case application:get_env(imboy, Key) of
        {ok, V} -> V;
        undefined -> <<>>
    end.
