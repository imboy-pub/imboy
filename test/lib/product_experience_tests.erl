-module(product_experience_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- effective/0：默认 / 显式 / 非法值降级 ------------------------------

effective_defaults_to_chat_test() ->
    SavedApp = saved_app_env(),
    try
        application:unset_env(imboy, product_experience),
        ?assertEqual(chat, product_experience:effective()),
        ?assertEqual(<<"chat">>, product_experience:effective_binary())
    after
        restore_app_env(SavedApp)
    end.

effective_explicit_workspace_test() ->
    SavedApp = saved_app_env(),
    try
        application:set_env(imboy, product_experience, workspace),
        ?assertEqual(workspace, product_experience:effective()),
        ?assertEqual(<<"workspace">>, product_experience:effective_binary())
    after
        restore_app_env(SavedApp)
    end.

effective_unknown_value_falls_back_to_chat_test() ->
    SavedApp = saved_app_env(),
    try
        lists:foreach(
            fun(Garbage) ->
                application:set_env(imboy, product_experience, Garbage),
                ?assertEqual(chat, product_experience:effective()),
                ?assertEqual(<<"chat">>, product_experience:effective_binary())
            end,
            [undefined, <<"bogus">>, <<"CHAT">>, 42, pro, {error, x}, [1 | 2]]
        )
    after
        restore_app_env(SavedApp)
    end.

%% ---- imboy_env 注入链（镜像 imboy_env_policy_tests 写法）------------------

env_override_injection_test() ->
    AppKeys = [product_experience],
    EnvKeys = ["IMBOY_PRODUCT_EXPERIENCE"],
    SavedApp = [{Key, application:get_env(imboy, Key)} || Key <- AppKeys],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- EnvKeys],
    try
        os:putenv("IMBOY_PRODUCT_EXPERIENCE", "workspace"),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, workspace}, application:get_env(imboy, product_experience)),

        %% 大小写与首尾空白归一
        os:putenv("IMBOY_PRODUCT_EXPERIENCE", "  CHAT  "),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, chat}, application:get_env(imboy, product_experience)),

        %% 非法值 fail-safe 为 chat（与 product_profile 的 erlang:error 拒启
        %% 语义刻意不同：体验开关错误值降级而非阻断启动）
        os:putenv("IMBOY_PRODUCT_EXPERIENCE", "workspce"),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, chat}, application:get_env(imboy, product_experience)),

        %% 缺失值默认 chat
        os:unsetenv("IMBOY_PRODUCT_EXPERIENCE"),
        ok = imboy_env:override_from_env(),
        ?assertEqual({ok, chat}, application:get_env(imboy, product_experience))
    after
        restore_app_env(SavedApp),
        restore_os_env(SavedEnv)
    end.

%% ---- config_version 摘要 --------------------------------------------------

config_version_stable_test() ->
    ?assertEqual(
        product_experience:digest(chat, <<"1.0.0">>),
        product_experience:digest(chat, <<"1.0.0">>)
    ),
    %% experience 归一化后参与摘要：binary 输入与原子输入同值同输出
    ?assertEqual(
        product_experience:digest(workspace, <<"1.0.0">>),
        product_experience:digest(<<"workspace">>, <<"1.0.0">>)
    ).

config_version_changes_on_experience_test() ->
    ?assertNotEqual(
        product_experience:digest(chat, <<"1.0.0">>),
        product_experience:digest(workspace, <<"1.0.0">>)
    ).

config_version_changes_on_vsn_test() ->
    ?assertNotEqual(
        product_experience:digest(chat, <<"1.0.0">>),
        product_experience:digest(chat, <<"1.0.1">>)
    ).

%% sha256("experience=<E>;app=<V>") 前 16 个小写 hex 字符（shasum -a 256 复算）
config_version_golden_test() ->
    ?assertEqual(<<"aabbb4be82f3ec06">>, product_experience:digest(chat, <<"1.0.0">>)),
    ?assertEqual(
        <<"684f363bd3176f1f">>, product_experience:digest(workspace, <<"1.0.0">>)
    ),
    ?assertEqual(
        <<"49654a9fffa39c0d">>, product_experience:digest(chat, <<"1.0.0-alpha.69">>)
    ).

config_version_format_test() ->
    Version = product_experience:config_version(),
    ?assertEqual(16, byte_size(Version)),
    ?assert(
        lists:all(
            fun(C) -> lists:member(C, "0123456789abcdef") end,
            binary_to_list(Version)
        )
    ).

%% ---- helpers（镜像 imboy_env_policy_tests）--------------------------------

saved_app_env() ->
    [{Key, application:get_env(imboy, Key)} || Key <- [product_experience]].

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
