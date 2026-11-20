%%%===================================================================
%%% @doc SEC-05 后端认证安全策略独立验证（S-04 政策层）
%%%
%%% 覆盖 PLANNED 政策断言（卡 S-04）：
%%%   1) 验证码一次消费/短效 fail-closed（真库 verification_code_ds）
%%%   2) 找回密码未知账号不泄露内部状态（不枚举）
%%%   3) 找回成功 = bump + kick_all_sessions（旧凭据全失效）
%%%   4) DS 失败 fail-closed：不半吊销、不发凭据、不假成功
%%%   5) 账户域锁定有恢复途径（record_login_success 解锁）
%%%   6) 锁定详情 map 不含 PII（计数/时间戳白名单键）
%%%
%%% 纪律：D/S 本体不 mock（真库/真 owner）；仅 find_password 链路按
%%% 卡许可 meck user_ds/verification_code_ds/auth_session_ds 合成边界。
%%%===================================================================
-module(auth_security_policy_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 916004).
-define(EMAIL, <<"s04-policy@e2e.test">>).

%% ------------------------------------------------------------------
%% 1. 验证码一次消费（真库）
%% ------------------------------------------------------------------
consume_code_single_consumption_test_() ->
    ?TEST_WITH_DB(fun() ->
        Now = elib_dt:now(),
        ValidityAt = elib_dt:add(Now, {10, minute}),
        Id = <<"s04-consume-once@e2e.test">>,
        Code = <<"654321">>,
        {ok, _} = verification_code_ds:save(Id, Code, ValidityAt, Now),
        ?assertMatch({ok, _}, verification_code_ds:consume(Id, Code)),
        %% 同码二次消费必须拒绝：并发兑换/重放不得绕过
        ?assertMatch({error, _}, verification_code_ds:consume(Id, Code))
    end).

%% ------------------------------------------------------------------
%% 2. 验证码过期 fail-closed（真库；strict <：到期即拒绝）
%% ------------------------------------------------------------------
consume_code_expired_fail_closed_test_() ->
    ?TEST_WITH_DB(fun() ->
        Now = elib_dt:now(),
        Past = elib_dt:minus(Now, {5, minute}),
        Id = <<"s04-expired@e2e.test">>,
        Code = <<"314159">>,
        {ok, _} = verification_code_ds:save(Id, Code, Past, Past),
        ?assertMatch({error, _}, verification_code_ds:consume(Id, Code))
    end).

%% ------------------------------------------------------------------
%% 3. 找回密码：未知账号（Id=0）错误为普通 binary，不泄露内部状态
%% ------------------------------------------------------------------
find_password_unknown_account_no_state_leak_test_() ->
    ?WITH_MECKS(
        [
            {user_ds, [
                {'lookup', 3, fun(_Field, _Value, _Sel) ->
                    {ok, #{}}
                end}
            ]},
            {verification_code_ds, [
                {'consume', 2, fun(_Id, _Code) ->
                    {error, <<"验证码无效"/utf8>>}
                end}
            ]}
        ],
        fun() ->
            Result = passport_logic:find_password(
                <<"email">>, ?EMAIL, <<"NewPwd#01">>, <<"000001">>, #{}
            ),
            %% 未知/已删账号：普通 binary 错误（不存在或已删除合并表述，
            %% 不区分两态=不枚举），无内部 map/状态/PII 泄露
            ?assertMatch({error, Msg} when is_binary(Msg), Result)
        end
    ).

%% ------------------------------------------------------------------
%% 4. 找回成功 = 凭据版本提升 + 全端会话吊销（旧 Cookie/JWT 失效）
%% ------------------------------------------------------------------
find_password_success_bumps_and_kicks_test_() ->
    ?WITH_MECKS(
        [
            {user_ds, [
                {'lookup', 3, fun
                    (email, ?EMAIL, <<"id">>) ->
                        {ok, #{<<"id">> => ?UID}};
                    (_F, _V, _S) ->
                        {ok, #{}}
                end},
                {'update_password', 2, fun(?UID, _Pwd) ->
                    {ok, 1}
                end}
            ]},
            {verification_code_ds, [
                {'consume', 2, fun(?EMAIL, _Code) ->
                    {ok, <<"consumed">>}
                end}
            ]},
            {auth_session_ds, [
                {'bump', 1, fun(?UID) -> ok end},
                {'kick_all_sessions', 1, fun(?UID) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{}},
                passport_logic:find_password(
                    <<"email">>, ?EMAIL, <<"NewPwd#01">>, <<"123456">>, #{}
                )
            ),
            ?assertEqual(1, meck:num_calls(auth_session_ds, bump, 1)),
            ?assertEqual(1, meck:num_calls(auth_session_ds, kick_all_sessions, 1))
        end
    ).

%% ------------------------------------------------------------------
%% 5. 密码写库失败 fail-closed：错误返回、不半吊销（bump/kick 均不触发）
%% ------------------------------------------------------------------
find_password_ds_failure_no_half_revocation_test_() ->
    ?WITH_MECKS(
        [
            {user_ds, [
                {'lookup', 3, fun
                    (email, ?EMAIL, <<"id">>) ->
                        {ok, #{<<"id">> => ?UID}};
                    (_F, _V, _S) ->
                        {ok, #{}}
                end},
                {'update_password', 2, fun(_Id, _Pwd) ->
                    {error, unavailable}
                end}
            ]},
            {verification_code_ds, [
                {'consume', 2, fun(?EMAIL, _Code) ->
                    {ok, <<"consumed">>}
                end}
            ]},
            {auth_session_ds, [
                {'bump', 1, fun(_) -> ok end},
                {'kick_all_sessions', 1, fun(_) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, unavailable},
                passport_logic:find_password(
                    <<"email">>, ?EMAIL, <<"NewPwd#01">>, <<"123456">>, #{}
                )
            ),
            %% fail-closed：未完成密码变更前不得吊销任何既有会话
            ?assertEqual(0, meck:num_calls(auth_session_ds, bump, 1)),
            ?assertEqual(0, meck:num_calls(auth_session_ds, kick_all_sessions, 1))
        end
    ).

%% ------------------------------------------------------------------
%% 6/7. 账户域锁定：真实阈值锁定 + 成功登录即解锁（恢复途径）
%% ------------------------------------------------------------------
-define(S04_ID, {user, ?UID}).

lock_has_recovery_path_and_no_pii_test_() ->
    {timeout, 30,
        ?TEST_WITH_APP(fun() ->
            ok = limiter_test_support:ensure_owner(),
            try
                lists:foreach(
                    fun(_) -> {ok, _} = login_security_logic:record_login_failure(?S04_ID) end,
                    lists:seq(1, 5)
                ),
                {error, locked, LockInfo} =
                    login_security_logic:check_login_allowed(?S04_ID),
                ?assertEqual(true, maps:get(<<"is_locked">>, LockInfo)),
                %% 锁定详情仅含计数/时间类白名单键（不携带账号/凭据/PII）
                %% 锁定详情仅含计数/时间/策略类白名单键（不携带账号/凭据/PII）
                Allowed = [
                    <<"is_locked">>,
                    <<"attempts">>,
                    <<"remaining_attempts">>,
                    <<"max_attempts">>,
                    <<"locked_until">>
                ],
                ?assertEqual(
                    [],
                    lists:filter(
                        fun(K) -> not lists:member(K, Allowed) end, maps:keys(LockInfo)
                    )
                ),
                %% 恢复途径：合法成功登录重置锁定（不依赖人工/短信）
                ok = login_security_logic:record_login_success(?S04_ID),
                ?assertEqual(
                    {ok, true},
                    login_security_logic:check_login_allowed(?S04_ID)
                )
            after
                catch limiter_test_support:ensure_owner()
            end
        end)}.
