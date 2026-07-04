-module(sso_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% sso_logic 模块的 EUnit 测试
%%%
%%% 目标：验证 SSO 配置的 provider 分派、保存校验与 test 连通性校验分支。
%%% 说明：test_connection 的真实网络探测（TCP/HTTP）不在单测内触发，
%%%       仅覆盖字段完整性校验的失败分支（不发起连接）。
%%%===================================================================

%% ===================================================================
%% save_config/1
%% ===================================================================

%% @doc 合法 provider -> 透传到 ds:upsert
save_config_valid_provider_test_() ->
    ?WITH_MECKS(
        [
            {sso_config_ds, [
                {'upsert', 2, fun(Provider, Cfg) ->
                    ?assertEqual(<<"ldap">>, Provider),
                    ?assertEqual(true, maps:get(<<"enabled">>, Cfg)),
                    {ok, #{}}
                end}
            ]}
        ],
        fun() ->
            Cfg = #{<<"provider">> => <<"ldap">>, <<"enabled">> => true, <<"host">> => <<"h">>},
            ?assertEqual({ok, #{}}, sso_logic:save_config(Cfg))
        end
    ).

%% @doc 非法 provider -> 拒绝，不触达 ds
save_config_invalid_provider_test() ->
    ?assertMatch({error, _}, sso_logic:save_config(#{<<"provider">> => <<"bogus">>})).

%% @doc 缺 provider -> 拒绝
save_config_missing_provider_test() ->
    ?assertMatch({error, _}, sso_logic:save_config(#{<<"enabled">> => true})).

%% ===================================================================
%% test_connection/2 —— 校验失败分支（不发起网络）
%% ===================================================================

%% @doc LDAP host 空 -> 校验失败
test_ldap_empty_host_test() ->
    ?assertMatch({false, _}, sso_logic:test_connection(<<"ldap">>, #{<<"port">> => 389})).

%% @doc LDAP port 非法 -> 校验失败
test_ldap_bad_port_test() ->
    Cfg = #{<<"host">> => <<"ldap.example.com">>, <<"port">> => 0},
    ?assertMatch({false, _}, sso_logic:test_connection(<<"ldap">>, Cfg)).

%% @doc SAML metadata_url 非法 -> 校验失败
test_saml_bad_url_test() ->
    Cfg = #{<<"metadata_url">> => <<"ftp://x">>},
    ?assertMatch({false, _}, sso_logic:test_connection(<<"saml">>, Cfg)).

%% @doc OAuth2 缺 token_url -> 校验失败
test_oauth2_missing_token_url_test() ->
    Cfg = #{<<"auth_url">> => <<"https://idp/authorize">>},
    ?assertMatch({false, _}, sso_logic:test_connection(<<"oauth2">>, Cfg)).

%% @doc 未知 provider -> 校验失败
test_unknown_provider_test() ->
    ?assertMatch({false, _}, sso_logic:test_connection(<<"bogus">>, #{})).
