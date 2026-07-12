-module(sso_config_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% sso_config_ds 模块的 EUnit 测试
%%%
%%% 目标：验证 jsonb 编解码与 provider -> 配置对象组装，以及 upsert 的
%%%       enabled 提取。全部 mock sso_config_repo，不触达数据库。
%%%===================================================================

%% ===================================================================
%% get_all/0
%% ===================================================================

%% @doc 多行 -> 以 provider 为键组装，jsonb 二进制被解码为 map
get_all_builds_provider_map_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'select_all', 0, fun() ->
                    {ok, [
                        #{
                            <<"provider">> => <<"ldap">>,
                            <<"enabled">> => true,
                            <<"config">> => <<"{\"host\":\"h\",\"enabled\":true}">>
                        },
                        #{
                            <<"provider">> => <<"saml">>,
                            <<"enabled">> => false,
                            <<"config">> => <<"{\"entity_id\":\"e\"}">>
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, M} = sso_config_ds:get_all(),
            ?assertEqual(<<"h">>, maps:get(<<"host">>, maps:get(<<"ldap">>, M))),
            ?assertEqual(<<"e">>, maps:get(<<"entity_id">>, maps:get(<<"saml">>, M))),
            ?assertNot(maps:is_key(<<"oauth2">>, M))
        end
    ).

%% @doc 空表 -> 空 map
get_all_empty_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'select_all', 0, fun() -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, #{}}, sso_config_ds:get_all())
        end
    ).

%% ===================================================================
%% upsert/2
%% ===================================================================

%% @doc 从配置对象提取 enabled，编码为 JSON 后交给 repo
upsert_extracts_enabled_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'select_all', 0, fun() -> {ok, []} end},
                {'upsert', 3, fun(Provider, Enabled, Json) ->
                    ?assertEqual(<<"oauth2">>, Provider),
                    ?assertEqual(true, Enabled),
                    ?assert(is_binary(Json)),
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            Cfg = #{
                <<"provider">> => <<"oauth2">>, <<"enabled">> => true, <<"client_id">> => <<"c">>
            },
            ?assertEqual({ok, #{}}, sso_config_ds:upsert(<<"oauth2">>, Cfg))
        end
    ).

%% @doc repo 出错 -> 返回友好错误
upsert_repo_error_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'upsert', 3, fun(_, _, _) -> {error, db_down} end},
                {'select_all', 0, fun() -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, sso_config_ds:upsert(<<"ldap">>, #{<<"enabled">> => false}))
        end
    ).

%% ===================================================================
%% 敏感字段加密 / 脱敏哨兵 / 解密往返
%% ===================================================================

-define(TEST_AES_KEY, <<"0123456789abcdef0123456789abcdef">>).

set_aes_key() ->
    application:set_env(imboy, postgre_aes_key, ?TEST_AES_KEY).

%% @doc upsert 落库的 JSON 中敏感字段必须是 enc:v1: 密文（行内无明文）
upsert_encrypts_secret_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'select_all', 0, fun() -> {ok, []} end},
                {'upsert', 3, fun(_, _, Json) ->
                    Stored = jsone:decode(Json, [{object_format, map}]),
                    Sec = maps:get(<<"client_secret">>, Stored),
                    ?assertMatch(<<"enc:v2:", _/binary>>, Sec),
                    ?assertEqual(nomatch, binary:match(Json, <<"topsecret">>)),
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            set_aes_key(),
            Cfg = #{
                <<"provider">> => <<"oauth2">>,
                <<"enabled">> => true,
                <<"client_id">> => <<"cid">>,
                <<"client_secret">> => <<"topsecret">>
            },
            ?assertEqual({ok, #{}}, sso_config_ds:upsert(<<"oauth2">>, Cfg))
        end
    ).

%% @doc get_provider 解密往返 == 原文
get_provider_decrypt_roundtrip_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'select_all', 0, fun() -> {ok, []} end},
                {'upsert', 3, fun(_, _, _) -> {ok, [#{<<"id">> => 1}]} end}
            ]}
        ],
        fun() ->
            set_aes_key(),
            Cfg = #{
                <<"provider">> => <<"oauth2">>,
                <<"client_secret">> => <<"roundtrip_secret">>
            },
            %% 捕获 upsert 实际落库的 JSON，再让 select_all 返回它
            Self = self(),
            meck:expect(sso_config_repo, upsert, fun(_, _, Json) ->
                Self ! {stored, Json},
                {ok, [#{<<"id">> => 1}]}
            end),
            {ok, _} = sso_config_ds:upsert(<<"oauth2">>, Cfg),
            Json =
                receive
                    {stored, J} -> J
                after 1000 -> erlang:error(no_stored_json)
                end,
            meck:expect(sso_config_repo, select_all, fun() ->
                {ok, [
                    #{
                        <<"provider">> => <<"oauth2">>,
                        <<"enabled">> => true,
                        <<"config">> => Json
                    }
                ]}
            end),
            {ok, Out} = sso_config_ds:get_provider(<<"oauth2">>),
            ?assertEqual(<<"roundtrip_secret">>, maps:get(<<"client_secret">>, Out))
        end
    ).

%% @doc 哨兵：入参 *** 或空 -> 保留库中已存密文不变
upsert_sentinel_keeps_existing_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'select_all', 0, fun() ->
                    {ok, [
                        #{
                            <<"provider">> => <<"oauth2">>,
                            <<"enabled">> => true,
                            <<"config">> =>
                                <<"{\"client_secret\":\"enc:v1:aXY=:existing_cipher\"}">>
                        }
                    ]}
                end},
                {'upsert', 3, fun(_, _, Json) ->
                    Stored = jsone:decode(Json, [{object_format, map}]),
                    ?assertEqual(
                        <<"enc:v1:aXY=:existing_cipher">>,
                        maps:get(<<"client_secret">>, Stored)
                    ),
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            set_aes_key(),
            Cfg = #{<<"provider">> => <<"oauth2">>, <<"client_secret">> => <<"***">>},
            ?assertEqual({ok, #{}}, sso_config_ds:upsert(<<"oauth2">>, Cfg))
        end
    ).

%% @doc 历史明文行（无 enc:v1: 前缀）兼容读：get_provider 原样返回
get_provider_legacy_plaintext_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'select_all', 0, fun() ->
                    {ok, [
                        #{
                            <<"provider">> => <<"ldap">>,
                            <<"enabled">> => true,
                            <<"config">> => <<"{\"bind_password\":\"legacy_plain\"}">>
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            set_aes_key(),
            {ok, Out} = sso_config_ds:get_provider(<<"ldap">>),
            ?assertEqual(<<"legacy_plain">>, maps:get(<<"bind_password">>, Out))
        end
    ).

%% @doc 未配置 provider -> not_found
get_provider_not_found_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_repo, [
                {'select_all', 0, fun() -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_found}, sso_config_ds:get_provider(<<"oauth2">>))
        end
    ).
