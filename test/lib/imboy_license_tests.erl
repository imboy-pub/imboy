-module(imboy_license_tests).
%%%===================================================================
%%% @doc imboy_license EUnit 测试
%%%
%%% 覆盖：配额 gate（用户/节点）、运行时访问器、以及「签发→加载→验签→校验」
%%% 全链路（用临时生成的 RSA 密钥对 + 临时 license 文件，经 IMBOY_LICENSE_*
%%% 环境变量注入），含篡改/过期降级用例。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(PT_KEY, {imboy_license, state}).

%%%===================================================================
%%% 测试编排
%%%===================================================================

license_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun check_user_quota_/0,
        fun check_node_quota_/0,
        fun accessors_/0,
        fun valid_license_roundtrip_/0,
        fun tampered_license_downgrade_/0,
        fun expired_license_downgrade_/0,
        fun trial_fresh_/0,
        fun trial_expired_/0,
        fun trial_file_init_/0
    ]}.

setup() ->
    _ = application:ensure_all_started(crypto),
    _ = application:ensure_all_started(public_key),
    ok.

cleanup(_) ->
    catch persistent_term:erase(?PT_KEY),
    ok.

%%%===================================================================
%%% 配额 gate
%%%===================================================================

check_user_quota_() ->
    put_state(base_state(#{max_users => 500})),
    ?assertEqual(ok, imboy_license:check_user_quota(0)),
    ?assertEqual(ok, imboy_license:check_user_quota(499)),
    ?assertEqual({error, quota_exceeded}, imboy_license:check_user_quota(500)),
    ?assertEqual({error, quota_exceeded}, imboy_license:check_user_quota(1000)),
    %% max_users=0 表示不限量
    put_state(base_state(#{max_users => 0})),
    ?assertEqual(ok, imboy_license:check_user_quota(999999)).

check_node_quota_() ->
    %% 不限量
    put_state(base_state(#{max_nodes => 0})),
    ?assertEqual(ok, imboy_license:check_node_quota()),
    %% 非分布式测试节点：当前节点数=1，max_nodes>=1 均放行
    put_state(base_state(#{max_nodes => 1})),
    ?assertEqual(ok, imboy_license:check_node_quota()),
    put_state(base_state(#{max_nodes => 10})),
    ?assertEqual(ok, imboy_license:check_node_quota()).

accessors_() ->
    put_state(#{
        valid => true,
        status => valid,
        edition => <<"professional">>,
        max_users => 500,
        max_nodes => 3,
        licensee => <<"测试公司"/utf8>>,
        expires_at => 123,
        reason => <<>>
    }),
    ?assertEqual(true, imboy_license:is_valid()),
    ?assertEqual(<<"professional">>, imboy_license:edition()),
    ?assertEqual(500, imboy_license:max_users()),
    ?assertEqual(3, imboy_license:max_nodes()),
    ?assertEqual(<<"测试公司"/utf8>>, imboy_license:licensee()),
    ?assertEqual(123, imboy_license:expires_at()),
    ?assertEqual(#{max_users => 500, max_nodes => 3}, imboy_license:limits()).

%%%===================================================================
%%% 全链路：签发 → 加载 → 验签 → 校验
%%%===================================================================

valid_license_roundtrip_() ->
    {Priv, PubPem} = gen_keypair(),
    Now = erlang:system_time(millisecond),
    Payload = encode_payload(#{
        <<"edition">> => <<"professional">>,
        <<"max_users">> => 500,
        <<"max_nodes">> => 3,
        <<"domains">> => [],
        <<"licensee">> => <<"测试公司"/utf8>>,
        <<"issued_at">> => Now,
        <<"expires_at">> => Now + 365 * 86400000
    }),
    License = make_license(Payload, Priv),
    with_license(PubPem, License, fun() ->
        ok = imboy_license:load_and_validate(),
        ?assertEqual(true, imboy_license:is_valid()),
        ?assertEqual(<<"professional">>, imboy_license:edition()),
        ?assertEqual(500, imboy_license:max_users()),
        ?assertEqual(<<"测试公司"/utf8>>, imboy_license:licensee()),
        %% 规模 gate 随 license 生效
        ?assertEqual(ok, imboy_license:check_user_quota(499)),
        ?assertEqual({error, quota_exceeded}, imboy_license:check_user_quota(500))
    end).

tampered_license_downgrade_() ->
    {Priv, PubPem} = gen_keypair(),
    Now = erlang:system_time(millisecond),
    Good = encode_payload(#{
        <<"edition">> => <<"professional">>,
        <<"max_users">> => 500,
        <<"domains">> => [],
        <<"licensee">> => <<"X">>,
        <<"expires_at">> => Now + 86400000
    }),
    Sig = public_key:sign(Good, sha256, Priv),
    %% 篡改 payload（提权 max_users）但沿用旧签名 → 验签必败
    Tampered = encode_payload(#{
        <<"edition">> => <<"max">>,
        <<"max_users">> => 999999,
        <<"domains">> => [],
        <<"licensee">> => <<"X">>,
        <<"expires_at">> => Now + 86400000
    }),
    License = <<(base64:encode(Tampered))/binary, ".", (base64:encode(Sig))/binary>>,
    with_license(PubPem, License, fun() ->
        ok = imboy_license:load_and_validate(),
        ?assertEqual(false, imboy_license:is_valid()),
        ?assertEqual(<<"community">>, imboy_license:edition())
    end).

expired_license_downgrade_() ->
    {Priv, PubPem} = gen_keypair(),
    Now = erlang:system_time(millisecond),
    %% 过期超过 7 天宽限期（8 天前过期）→ 降级社区版
    Payload = encode_payload(#{
        <<"edition">> => <<"professional">>,
        <<"max_users">> => 500,
        <<"domains">> => [],
        <<"licensee">> => <<"X">>,
        <<"expires_at">> => Now - 8 * 86400000
    }),
    License = make_license(Payload, Priv),
    with_license(PubPem, License, fun() ->
        ok = imboy_license:load_and_validate(),
        ?assertEqual(false, imboy_license:is_valid()),
        ?assertEqual(<<"community">>, imboy_license:edition())
    end).

%%%===================================================================
%%% 试用期自动签发（无 license 文件时）
%%%===================================================================

trial_fresh_() ->
    Now = erlang:system_time(millisecond),
    S = imboy_license:evaluate_trial(Now, Now),
    ?assertEqual(true, maps:get(valid, S)),
    ?assertEqual(<<"trial">>, maps:get(edition, S)),
    %% 试用配额高于社区版（>100）
    ?assert(maps:get(max_users, S) > 100).

trial_expired_() ->
    Now = erlang:system_time(millisecond),
    %% 起始时间在 9999 天前 → 必然超过试用期 → 降级社区版
    S = imboy_license:evaluate_trial(Now, Now - 9999 * 86400000),
    ?assertEqual(false, maps:get(valid, S)),
    ?assertEqual(<<"community">>, maps:get(edition, S)).

trial_file_init_() ->
    Tmp = write_tmp("trial", <<>>),
    catch file:delete(Tmp),
    Old = os:getenv("IMBOY_TRIAL_FILE"),
    os:putenv("IMBOY_TRIAL_FILE", Tmp),
    try
        {ok, S1} = imboy_license:trial_start_ms(),
        %% 二次读回一致（不重置起始时间）
        {ok, S2} = imboy_license:trial_start_ms(),
        ?assertEqual(S1, S2),
        ?assert(S1 > 0)
    after
        restore_env("IMBOY_TRIAL_FILE", Old),
        catch file:delete(Tmp)
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

put_state(M) ->
    persistent_term:put(?PT_KEY, M).

base_state(Over) ->
    maps:merge(
        #{
            valid => true,
            status => valid,
            edition => <<"professional">>,
            max_users => 500,
            max_nodes => 3,
            licensee => <<>>,
            expires_at => 0,
            reason => <<>>
        },
        Over
    ).

encode_payload(Map) ->
    jsone:encode(Map, [native_utf8]).

make_license(Payload, Priv) ->
    Sig = public_key:sign(Payload, sha256, Priv),
    <<(base64:encode(Payload))/binary, ".", (base64:encode(Sig))/binary>>.

gen_keypair() ->
    Priv = public_key:generate_key({rsa, 2048, 65537}),
    Pub = #'RSAPublicKey'{
        modulus = Priv#'RSAPrivateKey'.modulus,
        publicExponent = Priv#'RSAPrivateKey'.publicExponent
    },
    PubPem = public_key:pem_encode([public_key:pem_entry_encode('SubjectPublicKeyInfo', Pub)]),
    {Priv, PubPem}.

write_tmp(Prefix, Content) ->
    Dir =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            D -> D
        end,
    File = filename:join(
        Dir,
        "imboy_lic_test_" ++ Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:write_file(File, Content),
    File.

with_license(PubPem, License, Fun) ->
    PubFile = write_tmp("pub", PubPem),
    LicFile = write_tmp("lic", License),
    OldPub = os:getenv("IMBOY_LICENSE_PUBKEY"),
    OldLic = os:getenv("IMBOY_LICENSE_FILE"),
    os:putenv("IMBOY_LICENSE_PUBKEY", PubFile),
    os:putenv("IMBOY_LICENSE_FILE", LicFile),
    try
        Fun()
    after
        restore_env("IMBOY_LICENSE_PUBKEY", OldPub),
        restore_env("IMBOY_LICENSE_FILE", OldLic),
        catch file:delete(PubFile),
        catch file:delete(LicFile)
    end.

restore_env(K, false) -> os:unsetenv(K);
restore_env(K, V) -> os:putenv(K, V).
