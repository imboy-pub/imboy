%%% @doc CSB-02R 装配层套件（零 DB、零 socket；application:set_env 注入
%%% 配置，用后恢复——不污染同节点其它套件）。
%%%
%%% 覆盖：
%%%   * **cs_identity_assertion:verify/2**：digest 锚定（材料摘要 = DB 行
%%%     digest）、HMAC 签名复核（canonical = 键名字典序 JSON）、claims 归一
%%%     （binary 键 → 冻结白名单原子）、逐类失败可区分
%%%     （identity_key_not_configured 422 / identity_key_digest_mismatch 500 /
%%%     assertion_signature_mismatch 401）。
%%%   * **cs_widget_env 合并语义**：Params 注入恒优先；env 解析不出 = 不合并
%%%     （透传缺参，application 既有 fail-closed 语义保留）；assertion 归一
%%%     （binary 嵌套键 → 原子）与坏形状 422。
-module(cs_widget_env_tests).

-include_lib("eunit/include/eunit.hrl").

env_test_() ->
    {foreach,
        fun() ->
            %% 快照注入面：用例内 set_env，用后按快照恢复（含未配置 = undefined）。
            [
                {cs_widget_subject_key, application:get_env(imboy, cs_widget_subject_key)},
                {cs_widget_identity_keys, application:get_env(imboy, cs_widget_identity_keys)},
                {cs_widget_intake_business_identity_id,
                    application:get_env(imboy, cs_widget_intake_business_identity_id)}
            ]
        end,
        fun(Snapshot) ->
            lists:foreach(
                fun({Key, Old}) ->
                    case Old of
                        undefined -> application:unset_env(imboy, Key);
                        {ok, V} -> application:set_env(imboy, Key, V)
                    end
                end,
                Snapshot
            ),
            ok
        end,
        [fun assertion_verifier_cases/1, fun merge_cases/1]}.

%% ===================================================================
%% cs_identity_assertion:verify/2
%% ===================================================================

assertion_verifier_cases(_) ->
    Key = <<"secret-material-1">>,
    Claims = #{
        <<"iss">> => <<"mall">>,
        <<"aud">> => <<"wgt_pub">>,
        <<"exp">> => 9999999999,
        <<"iat">> => 1760000000,
        <<"jti">> => <<"jti-1">>,
        <<"sub">> => <<"u-1">>
    },
    Canonical = jsone:encode(lists:sort(maps:to_list(Claims)), [native_utf8]),
    %% lowercase 与生产 encode_hex/2 同口径（4fea324f：digest 惯例小写；
    %% 缺省大写会先在 digest 锚定处失配，掩盖签名断言本意）。
    Sig = binary:encode_hex(crypto:mac(hmac, sha256, Key, Canonical), lowercase),
    GoodDigest = binary:encode_hex(crypto:hash(sha256, Key), lowercase),
    {ok, Verifier} = cs_widget_env:assertion_verifier_fun(),
    [
        {"verifier accepts a well-signed assertion and normalizes claim keys", fun() ->
            application:set_env(imboy, cs_widget_identity_keys, [{1, Key}]),
            {ok, Normalized} = Verifier(
                #{key_version => 1, claims => Claims, sig => Sig}, GoodDigest
            ),
            ?assertEqual(<<"mall">>, maps:get(iss, Normalized)),
            ?assertEqual(9999999999, maps:get(exp, Normalized)),
            ?assertEqual(<<"jti-1">>, maps:get(jti, Normalized))
        end},

        {"verifier rejects unknown key version (identity_key_not_configured)", fun() ->
            application:set_env(imboy, cs_widget_identity_keys, [{1, Key}]),
            ?assertEqual(
                {error, identity_key_not_configured},
                Verifier(#{key_version => 9, claims => Claims, sig => Sig}, GoodDigest)
            )
        end},

        {"verifier rejects digest drift (identity_key_digest_mismatch)", fun() ->
            application:set_env(imboy, cs_widget_identity_keys, [{1, <<"other-material">>}]),
            ?assertEqual(
                {error, identity_key_digest_mismatch},
                Verifier(#{key_version => 1, claims => Claims, sig => Sig}, GoodDigest)
            )
        end},

        {"verifier rejects a bad signature (assertion_signature_mismatch)", fun() ->
            application:set_env(imboy, cs_widget_identity_keys, [{1, Key}]),
            ?assertEqual(
                {error, assertion_signature_mismatch},
                Verifier(
                    #{key_version => 1, claims => Claims, sig => <<"deadbeef">>}, GoodDigest
                )
            )
        end},

        {"verifier is fail-closed on unprovisioned config", fun() ->
            application:set_env(imboy, cs_widget_identity_keys, []),
            ?assertEqual(
                {error, identity_key_not_configured},
                Verifier(#{key_version => 1, claims => Claims, sig => Sig}, GoodDigest)
            )
        end}
    ].

%% ===================================================================
%% cs_widget_env 合并语义
%% ===================================================================

merge_cases(_) ->
    [
        {"merge_subject_key fills only when env resolves; Params injection wins", fun() ->
            application:set_env(imboy, cs_widget_subject_key, <<"sk-material">>),
            {ok, Merged} = cs_widget_env:merge_bootstrap(1, #{public_widget_id => <<"w">>}),
            ?assertEqual(<<"sk-material">>, maps:get(subject_key, Merged)),
            %% Params 注入恒优先。
            {ok, Kept} = cs_widget_env:merge_bootstrap(1, #{
                subject_key => <<"injected">>
            }),
            ?assertEqual(<<"injected">>, maps:get(subject_key, Kept))
        end},

        {"merge skips missing env keys (fail-closed passthrough, no placeholders)", fun() ->
            application:set_env(imboy, cs_widget_subject_key, <<>>),
            {ok, Untouched} = cs_widget_env:merge_bootstrap(1, #{public_widget_id => <<"w">>}),
            ?assertNot(is_map_key(subject_key, Untouched))
        end},

        {"merge_identity_exchange normalizes the HTTP assertion object (binary keys)", fun() ->
            application:set_env(imboy, cs_widget_subject_key, <<"sk-material">>),
            {ok, Merged} = cs_widget_env:merge_identity_exchange(1, #{
                installation_id => 810001,
                assertion => #{
                    <<"key_version">> => 1,
                    <<"claims">> => #{<<"jti">> => <<"j">>},
                    <<"sig">> => <<"ab">>
                }
            }),
            Assertion = maps:get(assertion, Merged),
            ?assertEqual(1, maps:get(key_version, Assertion)),
            ?assertEqual(#{<<"jti">> => <<"j">>}, maps:get(claims, Assertion)),
            ?assertEqual(<<"ab">>, maps:get(sig, Assertion))
        end},

        {"merge_identity_exchange rejects a malformed assertion with 422 vocabulary", fun() ->
            ?assertEqual(
                {error, {invalid_argument, identity_exchange}},
                cs_widget_env:merge_identity_exchange(1, #{
                    installation_id => 810001,
                    assertion => #{<<"key_version">> => <<"x">>}
                })
            )
        end}
    ].
