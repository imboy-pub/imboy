%%% @doc BE-W01 A02：企业密钥环 `_FILE` 装载合同（eb_keyring_file）。
%%%
%%% 冻结合同（cors-auth-matrix / plan :519）：企业消息 keyring 支持
%%% `IMBOY_EB_ENTERPRISE_KEYRING_FILE` 文件加载，文件权限必须 0600，
%%% 装配失败启动 fail-fast；密钥内容不进任何错误项。
%%%
%%% 文件格式：JSON `{"active_version": 1, "keys": {"1": "<64-hex>"}}`
%%% （密管系统渲染挂载的常规形态）；解码复用 eb_env_keyring:decode/1
%%% 的全部严格判据（hex/32 字节/active 存在性）。
-module(eb_keyring_file_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ENV_VAR, "IMBOY_EB_ENTERPRISE_KEYRING_FILE").

hex_of(Bytes) ->
    binary:encode_hex(Bytes, lowercase).

valid_keyring_json() ->
    jsone:encode(
        #{
            <<"active_version">> => 2,
            <<"keys">> => #{
                <<"1">> => hex_of(crypto:strong_rand_bytes(32)),
                <<"2">> => hex_of(crypto:strong_rand_bytes(32))
            }
        },
        [native_utf8]
    ).

temp_keyring_file(Name, Content, Mode) ->
    Path = filename:join(
        "/tmp", Name ++ "-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:write_file(Path, Content),
    ok = file:change_mode(Path, Mode),
    Path.

saved_state() ->
    {
        application:get_env(imboy, eb_enterprise_keyring),
        os:getenv(?ENV_VAR)
    }.

restore_state({AppEnv, EnvVar}) ->
    case AppEnv of
        undefined -> _ = application:unset_env(imboy, eb_enterprise_keyring);
        {ok, V} -> ok = application:set_env(imboy, eb_enterprise_keyring, V)
    end,
    case EnvVar of
        false -> os:unsetenv(?ENV_VAR);
        EnvValue -> os:putenv(?ENV_VAR, EnvValue)
    end,
    ok.

%% JSON → 密钥环形状：键字符串版本号转正整数，其余严格判据复用 decode/1。
decode_json_valid_test() ->
    Json = valid_keyring_json(),
    {ok, EnvShape} = eb_keyring_file:decode_json(Json),
    {ok, #{keys := Ring, key_version := 2}} = eb_env_keyring:decode(EnvShape),
    ?assertEqual(2, map_size(Ring)),
    [?assertEqual(32, byte_size(K)) || K <- maps:values(Ring)].

decode_json_invalid_test() ->
    %% 非 JSON / 非 object / 缺键 / 版本号非正整数 —— 全部 fail-closed。
    ?assertMatch({error, _}, eb_keyring_file:decode_json(<<"not-json">>)),
    ?assertMatch({error, _}, eb_keyring_file:decode_json(<<"[1,2]">>)),
    ?assertMatch(
        {error, _}, eb_keyring_file:decode_json(jsone:encode(#{<<"active_version">> => 1}))
    ),
    ?assertMatch(
        {error, _},
        eb_keyring_file:decode_json(
            jsone:encode(
                #{<<"active_version">> => 1, <<"keys">> => #{<<"x">> => hex_of(<<0:256>>)}}, [
                    native_utf8
                ]
            )
        )
    ).

%% 未设置 env：no-op，不碰 application env。
load_env_file_unset_is_noop_test() ->
    Saved = saved_state(),
    try
        os:unsetenv(?ENV_VAR),
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        ok = eb_keyring_file:load_env_file(),
        ?assertEqual(undefined, application:get_env(imboy, eb_enterprise_keyring))
    after
        restore_state(Saved)
    end.

%% 0600 文件装载成功：application env 被覆写为解码前形状，可解析出 KeyRef。
load_env_file_reads_0600_file_test() ->
    Saved = saved_state(),
    Path = temp_keyring_file("imboy-csww-keyring.json", valid_keyring_json(), 8#600),
    try
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        os:putenv(?ENV_VAR, Path),
        ok = eb_keyring_file:load_env_file(),
        {ok, Shape} = application:get_env(imboy, eb_enterprise_keyring),
        {ok, _KeyRef} = eb_env_keyring:decode(Shape)
    after
        _ = file:delete(Path),
        restore_state(Saved)
    end.

%% 权限过宽（0644）：启动 fail-fast，绝不读取内容。
load_env_file_rejects_loose_permissions_test() ->
    Saved = saved_state(),
    Path = temp_keyring_file("imboy-csww-keyring-loose.json", valid_keyring_json(), 8#644),
    try
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        os:putenv(?ENV_VAR, Path),
        ?assertError({keyring_file_permissions, _}, eb_keyring_file:load_env_file()),
        ?assertEqual(undefined, application:get_env(imboy, eb_enterprise_keyring))
    after
        _ = file:delete(Path),
        restore_state(Saved)
    end.

%% 文件缺失/内容非法：启动 fail-fast；错误项不含密钥内容。
load_env_file_missing_or_bad_content_fails_fast_test() ->
    Saved = saved_state(),
    BadJson = temp_keyring_file("imboy-csww-keyring-bad.json", <<"{oops">>, 8#600),
    try
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        os:putenv(?ENV_VAR, "/tmp/imboy-csww-keyring-missing.json"),
        ?assertError({keyring_file_unreadable, _}, eb_keyring_file:load_env_file()),
        os:putenv(?ENV_VAR, BadJson),
        ?assertError({keyring_file_invalid, _}, eb_keyring_file:load_env_file()),
        ?assertEqual(undefined, application:get_env(imboy, eb_enterprise_keyring))
    after
        _ = file:delete(BadJson),
        restore_state(Saved)
    end.

%% 双源歧义：application env（sys.config）与 _FILE 同给 → fail-closed。
load_env_file_conflicts_with_app_env_test() ->
    Saved = saved_state(),
    Path = temp_keyring_file("imboy-csww-keyring-both.json", valid_keyring_json(), 8#600),
    try
        ok = application:set_env(imboy, eb_enterprise_keyring, #{active_version => 1}),
        os:putenv(?ENV_VAR, Path),
        ?assertError(keyring_source_conflict, eb_keyring_file:load_env_file())
    after
        _ = file:delete(Path),
        restore_state(Saved)
    end.
