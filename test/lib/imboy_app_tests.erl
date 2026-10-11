-module(imboy_app_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    Previous = application:get_env(imboy, auto_migrate),
    meck:new(imboy_migrate, [passthrough, no_link]),
    Previous.

cleanup(Previous) ->
    meck:unload(imboy_migrate),
    case Previous of
        undefined -> application:unset_env(imboy, auto_migrate);
        {ok, Value} -> application:set_env(imboy, auto_migrate, Value)
    end,
    ok.

auto_migrate_gate_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun enabled_runs_migration/0,
        fun disabled_skips_migration/0,
        fun invalid_config_fails_fast/0
    ]}.

enabled_runs_migration() ->
    application:set_env(imboy, auto_migrate, true),
    meck:expect(imboy_migrate, migrate, fun() -> ok end),
    ?assertEqual(ok, imboy_app:maybe_migrate()),
    ?assertEqual(1, meck:num_calls(imboy_migrate, migrate, [])).

disabled_skips_migration() ->
    application:set_env(imboy, auto_migrate, false),
    meck:expect(imboy_migrate, migrate, fun() -> ok end),
    ?assertEqual(ok, imboy_app:maybe_migrate()),
    ?assertEqual(0, meck:num_calls(imboy_migrate, migrate, [])).

invalid_config_fails_fast() ->
    application:set_env(imboy, auto_migrate, enabled),
    ?assertError(
        {invalid_config, auto_migrate, enabled},
        imboy_app:maybe_migrate()
    ).

%% ------------------------------------------------------------------
%% N-5 回归：jverification_rsa_priv_key_file 是**可选能力**（仅极光一键登录
%% 手机号解密使用，运行期本就 try/catch 降级），绝不能在 boot 期因文件缺失
%% badmatch 拖垮整节点 —— 旧实现 `{ok,P}=file:read_file` 正是如此。
%% 三态：空→复用 login 私钥；已配置可读→用自身；已配置不可读→告警+回退 login。
%% ------------------------------------------------------------------
ensure_rsa_keys_jv_test_() ->
    {setup, fun rsa_setup/0, fun rsa_cleanup/1, fun rsa_cases/1}.

rsa_cases(#{jv := Jv}) ->
    [
        {"jv 留空 → 复用 login 私钥", fun() ->
            ok = application:set_env(imboy, jverification_rsa_priv_key_file, ""),
            ?assertEqual(ok, imboy_app:ensure_rsa_keys()),
            ?assertEqual(
                {ok, <<"PRIVPEM">>},
                application:get_env(imboy, jverification_rsa_priv_key)
            )
        end},
        {"jv 已配置且可读 → 使用独立私钥", fun() ->
            ok = application:set_env(imboy, jverification_rsa_priv_key_file, Jv),
            ?assertEqual(ok, imboy_app:ensure_rsa_keys()),
            ?assertEqual(
                {ok, <<"JVPEM">>},
                application:get_env(imboy, jverification_rsa_priv_key)
            )
        end},
        {"jv 已配置但不可读 → 回退 login 私钥，绝不崩溃启动（N-5）", fun() ->
            ok = application:set_env(
                imboy,
                jverification_rsa_priv_key_file,
                "/tmp/imboy_definitely_missing_jv_priv.pem"
            ),
            ?assertEqual(ok, imboy_app:ensure_rsa_keys()),
            ?assertEqual(
                {ok, <<"PRIVPEM">>},
                application:get_env(imboy, jverification_rsa_priv_key)
            )
        end}
    ].

rsa_setup() ->
    Keys = [
        login_rsa_pub_key_file,
        login_rsa_priv_key_file,
        jverification_rsa_priv_key_file,
        login_rsa_pub_key,
        login_rsa_priv_key,
        jverification_rsa_priv_key
    ],
    Saved = [{K, application:get_env(imboy, K)} || K <- Keys],
    Dir = filename:join(
        "/tmp",
        "imboy_app_rsa_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    Pub = filename:join(Dir, "login_pub.pem"),
    Priv = filename:join(Dir, "login_priv.pem"),
    Jv = filename:join(Dir, "jv_priv.pem"),
    %% ensure_rsa_keys/0 只做 read_file + set_env，不校验 PEM 结构 —— 用可辨识
    %% 的短字节串即可精确断言"取了哪一个"，避免测试里生成真实 RSA 密钥。
    ok = file:write_file(Pub, <<"PUBPEM">>),
    ok = file:write_file(Priv, <<"PRIVPEM">>),
    ok = file:write_file(Jv, <<"JVPEM">>),
    ok = application:set_env(imboy, login_rsa_pub_key_file, Pub),
    ok = application:set_env(imboy, login_rsa_priv_key_file, Priv),
    #{saved => Saved, dir => Dir, jv => Jv}.

rsa_cleanup(#{saved := Saved, dir := Dir}) ->
    [
        case V of
            undefined -> application:unset_env(imboy, K);
            {ok, Val} -> application:set_env(imboy, K, Val)
        end
     || {K, V} <- Saved
    ],
    _ = file:delete(filename:join(Dir, "login_pub.pem")),
    _ = file:delete(filename:join(Dir, "login_priv.pem")),
    _ = file:delete(filename:join(Dir, "jv_priv.pem")),
    _ = file:del_dir(Dir),
    ok.
