-module(adm_session_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% Task 12 / LT-06：管理后台 cookie 生命周期矩阵。
%%% 覆盖卡片 Step1 冻结清单：Secure/HttpOnly/SameSite/Max-Age 可断言（cookie_opts）；
%%% 过期拒绝；登出 bump 后服务端拒绝（旧 cookie 复用无效）；legacy 裸 HMAC 拒绝；
%%% store 不可用 fail-closed；epoch 持久化与跨账号隔离。

%% unique_integer 是 VM 级单调：每轮 eunit 新 VM 都从低位重复同一 ID 序列，
%% 与 adm_auth_epoch 跨轮残留行确定性碰撞（be-02 两轮实证：issue/bump 写过
%% 的 admin_id 本轮再查即非缺行默认 1）。混入本 VM 启动熵做基址，VM 内仍
%% 单调互不重号，跨轮不复用历史 admin_id。
uid() ->
    erlang:unique_integer([positive]) rem 100000000 + uid_base().

uid_base() ->
    case persistent_term:get({?MODULE, uid_base}, undefined) of
        undefined ->
            B = 1000 + erlang:phash2(erlang:system_time(nanosecond)) rem 899000000,
            persistent_term:put({?MODULE, uid_base}, B),
            B;
        B ->
            B
    end.

uid_bin() ->
    ec_cnv:to_binary(uid()).

%% 缺行默认 epoch=1（已知默认态）
current_epoch_missing_row_defaults_to_1_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        AdmId = uid(),
        ?assertEqual({ok, 1}, adm_session_ds:current_epoch(AdmId))
    end).

%% bump：单调递增；缓存失效后以 DB 为权威
bump_advances_and_survives_cache_flush_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        AdmId = uid(),
        ?assertEqual({ok, 1}, adm_session_ds:current_epoch(AdmId)),
        ok = adm_session_ds:bump(AdmId),
        ok = adm_session_ds:bump(AdmId),
        imboy_cache:flush(),
        ?assertEqual({ok, 3}, adm_session_ds:current_epoch(AdmId))
    end).

%% 跨账号隔离：bump 只吊销 A 的既有 cookie，B 的 cookie 不受影响
cross_account_isolation_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        A = uid(),
        B = uid(),
        {ok, SigA} = adm_session_ds:issue(ec_cnv:to_binary(A)),
        {ok, SigB} = adm_session_ds:issue(ec_cnv:to_binary(B)),
        ?assertMatch({ok, _}, adm_session_ds:verify(ec_cnv:to_binary(B), SigB)),
        ok = adm_session_ds:bump(A),
        ?assertEqual({error, revoked}, adm_session_ds:verify(ec_cnv:to_binary(A), SigA)),
        ?assertMatch({ok, _}, adm_session_ds:verify(ec_cnv:to_binary(B), SigB))
    end).

%% store 不可用 fail-closed：epoch 现势无法确认 ⇒ 签名一律拒绝
fail_closed_when_store_unavailable_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        AdmId = uid(),
        {ok, Sig} = adm_session_ds:issue(ec_cnv:to_binary(AdmId)),
        meck:new(elib_pg, [passthrough]),
        try
            meck:expect(elib_pg, query, fun(_Sql, _Params) ->
                {error, pool_unavailable}
            end),
            %% issue 在 meck 前已写入 60s 正向 memo；清掉才能落到被 meck 的 DB 层
            imboy_cache:flush(),
            ?assertEqual({error, unavailable}, adm_session_ds:current_epoch(AdmId)),
            ?assertEqual(
                {error, store_unavailable}, adm_session_ds:verify(ec_cnv:to_binary(AdmId), Sig)
            )
        after
            _ =
                try
                    meck:unload(elib_pg)
                catch
                    _:_ -> ok
                end
        end
    end).

%% 端到端：新签发 cookie 有效
fresh_sig_grants_access_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        UidBin = uid_bin(),
        Sig = adm_auth_middleware:sign_admin_cookie(UidBin),
        ?assertMatch({ok, _}, adm_session_ds:verify(UidBin, Sig))
    end).

%% 过期拒绝：exp 已过的 sig 一律拒绝（即便签名正确、epoch 未变）
expired_sig_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        AdmId = uid(),
        UidBin = ec_cnv:to_binary(AdmId),
        Past = erlang:system_time(second) - 1,
        Sig = adm_session_ds:issue_with(UidBin, 1, Past),
        ?assertEqual({error, expired}, adm_session_ds:verify(UidBin, Sig))
    end).

%% 登出语义：bump 后旧 sig 服务端拒绝；重新登录（新签发）恢复
logout_bump_revokes_server_side_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        AdmId = uid(),
        UidBin = ec_cnv:to_binary(AdmId),
        Sig = adm_auth_middleware:sign_admin_cookie(UidBin),
        ?assertMatch({ok, _}, adm_session_ds:verify(UidBin, Sig)),
        ok = adm_session_ds:bump(AdmId),
        ?assertEqual({error, revoked}, adm_session_ds:verify(UidBin, Sig)),
        Sig2 = adm_auth_middleware:sign_admin_cookie(UidBin),
        ?assertMatch({ok, _}, adm_session_ds:verify(UidBin, Sig2))
    end).

%% legacy 裸 HMAC（旧格式）拒绝：升级后存量 cookie 全部失效（fail-closed，重登一次）
legacy_bare_hmac_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        UidBin = uid_bin(),
        Legacy = elib_hasher:hmac_sha256(UidBin, adm_session_ds:signing_key()),
        ?assertEqual({error, malformed}, adm_session_ds:verify(UidBin, Legacy))
    end).

%% 篡改拒绝：claims 与签名不一致
tampered_sig_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        UidBin = uid_bin(),
        {ok, Sig} = adm_session_ds:issue(UidBin),
        %% 把合法 sig 的 exp 段替换成远期，签名必不匹配
        [_V, _E, _X, SigB64] = binary:split(Sig, <<":">>, [global]),
        Future = erlang:system_time(second) + 3600,
        Forged = <<"v1:1:", (ec_cnv:to_binary(Future))/binary, ":", SigB64/binary>>,
        ?assertEqual({error, bad_signature}, adm_session_ds:verify(UidBin, Forged))
    end).

%% cookie 安全属性可断言（LT-06-A02）：HttpOnly/SameSite/Max-Age/path；
%% secure 随 start_mode（tls/http_tls 才置位，与既有口径一致）
cookie_security_attributes_test_() ->
    ?TEST_SIMPLE(fun() ->
        Opts = adm_session_ds:cookie_opts(3600),
        ?assertEqual(true, maps:get(http_only, Opts)),
        ?assertEqual(lax, maps:get(same_site, Opts)),
        ?assertEqual(3600, maps:get(max_age, Opts)),
        ?assertEqual(<<"/">>, maps:get(path, Opts)),
        ?assertEqual(false, maps:get(secure, Opts)),
        application:set_env(imboy, start_mode, tls),
        ?assertEqual(true, maps:get(secure, adm_session_ds:cookie_opts(3600))),
        application:unset_env(imboy, start_mode)
    end).

%% config_ds:env/2 mock — 与 adm_auth_middleware_tests 同款口径
config_ds_mock() ->
    {config_ds, [
        {'env', 2, fun
            (adm_cookie_secret, _Default) -> <<"test-cookie-secret">>;
            (adm_ip_allowlist, _Default) -> [];
            (adm_auth_legacy_cookie_enabled, _Default) -> false;
            (start_mode, http) -> http;
            (_, Default) -> Default
        end}
    ]}.

mock_request() ->
    #{
        method => <<"GET">>,
        version => 'HTTP/1.1',
        scheme => <<"http">>,
        host => <<"localhost">>,
        port => 8080,
        path => <<"/api/adm/dashboard">>,
        qs => <<>>,
        headers => #{},
        peer => {{127, 0, 0, 1}, 12345},
        body_length => 0
    }.

%% 中间件集成：登出 bump 后，持旧 cookie 的请求被 401 拒绝
middleware_rejects_after_logout_bump_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        AdmId = uid(),
        UidBin = ec_cnv:to_binary(AdmId),
        Sig = adm_auth_middleware:sign_admin_cookie(UidBin),
        ?WITH_MECKS(
            [
                config_ds_mock(),
                {cowboy_req, [
                    {'path', 1, fun(_Req) -> <<"/api/adm/dashboard">> end},
                    {'method', 1, fun(_Req) -> <<"GET">> end},
                    {'set_resp_cookie', 4, fun(_Name, _Value, Req, _Opts) -> Req end},
                    {'reply', 4, fun(Code, Headers, Body, Req) ->
                        Req#{
                            response_status => Code,
                            response_headers => Headers,
                            response_body => Body
                        }
                    end}
                ]},
                {elib_req, [
                    {'cookie', 2, fun
                        (<<"adm_user_id">>, _Req) -> UidBin;
                        (<<"adm_user_sig">>, _Req) -> Sig;
                        (_, _) -> false
                    end}
                ]}
            ],
            fun() ->
                Req = mock_request(),
                Env = #{handler_opts => #{}},
                %% bump 前放行（grant_access）
                {ok, _, #{handler_opts := #{adm_user_id := AdmId}}} =
                    adm_auth_middleware:execute(Req, Env),
                %% 登出 bump：旧 cookie 立即服务端失效
                ok = adm_session_ds:bump(AdmId),
                {stop, _} = adm_auth_middleware:execute(Req, Env)
            end
        )
    end).
