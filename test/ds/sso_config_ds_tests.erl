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
                {'upsert', 3, fun(_, _, _) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, sso_config_ds:upsert(<<"ldap">>, #{<<"enabled">> => false}))
        end
    ).
