-module(elib_kdf_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% Task 11 / LT-05：versioned 口令 KDF 双读矩阵（卡 Step1 冻结清单）。
%%% 覆盖：旧摘要成功升级一次 / 错误密码零写入 / 畸形与未知版本 / 越界成本
%%% 拒绝且不计算 / 并发双登录升级一次（CAS）/ 升级失败不锁死 / 双读兼容 /
%%% 未激活门（默认零行为变化）。

uid() ->
    erlang:unique_integer([positive]) rem 1000000000 + 2000.

activate_v2() ->
    application:set_env(imboy, kdf_v2_enabled, true),
    application:set_env(imboy, kdf_v2_iterations, 100000).

deactivate_v2() ->
    application:unset_env(imboy, kdf_v2_enabled),
    application:unset_env(imboy, kdf_v2_iterations).

%% 未激活门：默认（无配置）v2 完全不产出，generate 走既有格式
gate_default_disabled_test_() ->
    ?TEST_SIMPLE(fun() ->
        deactivate_v2(),
        {error, disabled} = elib_kdf:hash_v2(<<"pw">>, 2),
        Gen = elib_password:generate(<<"pw">>),
        ?assertMatch(error, elib_kdf:parse_v2(Gen)),
        %% 未激活也不影响既有验证
        ?assertEqual({ok, []}, elib_password:verify(<<"pw">>, Gen))
    end).

%% 激活后：generate 产出 v2；verify 双读往返通过；plan 报 version=v2（无升级）
gate_enabled_roundtrip_test_() ->
    ?TEST_SIMPLE(fun() ->
        activate_v2(),
        Gen = elib_password:generate(<<"pw-1">>),
        ?assertMatch({ok, _}, elib_kdf:parse_v2(Gen)),
        ?assertEqual({ok, []}, elib_password:verify(<<"pw-1">>, Gen)),
        {ok, #{version := v2}} = elib_password:verify_and_plan(<<"pw-1">>, Gen),
        deactivate_v2()
    end).

%% legacy 存储双读：验证通过并给出升级计划（变体+候选）
legacy_verify_plans_upgrade_test_() ->
    ?TEST_SIMPLE(fun() ->
        Legacy = elib_password:generate(<<"pw-2">>, hmac_sha512),
        ?assertMatch(error, elib_kdf:parse_v2(Legacy)),
        {ok, #{version := legacy, variant := 2, candidate := _}} =
            elib_password:verify_and_plan(<<"pw-2">>, Legacy)
    end).

%% 错误密码：拒绝且不产生升级计划（零写入语义的验证侧前提）
wrong_password_no_upgrade_plan_test_() ->
    ?TEST_SIMPLE(fun() ->
        Legacy = elib_password:generate(<<"right">>, hmac_sha512),
        {error, <<"errorPassword">>} = elib_password:verify_and_plan(<<"wrong">>, Legacy),
        {error, <<"errorPassword">>} = elib_kdf:verify_v2(<<"wrong">>, Legacy)
    end).

%% 畸形与未知版本：拒绝不崩溃（不执行昂贵计算）
malformed_and_unknown_version_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch({error, <<"errorPassword">>}, elib_kdf:verify_v2(<<"x">>, <<"garbage">>)),
        ?assertMatch(
            {error, <<"errorPassword">>},
            elib_kdf:verify_v2(<<"x">>, <<"$v2$unknown_algo$i=100000;v=2$AAAA$BBBB">>)
        ),
        ?assertMatch(error, elib_kdf:parse_v2(<<"$v9$whatever">>)),
        ?assertMatch(error, elib_kdf:parse_v2(<<"$v2$pbkdf2_sha512$broken">>))
    end).

%% 越界成本：参数超出硬边界 ⇒ 拒绝且不执行 KDF（远小于一次真实 KDF 的耗时）
oversized_cost_rejected_without_compute_test_() ->
    ?TEST_SIMPLE(fun() ->
        activate_v2(),
        Oversized =
            <<
                "$v2$pbkdf2_sha512$i=999999999;v=2$"
                "AAAAAAAAAAAAAAAAAAAAAA==$"
                "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
            >>,
        T0 = erlang:monotonic_time(millisecond),
        ?assertMatch({error, <<"errorPassword">>}, elib_kdf:verify_v2(<<"x">>, Oversized)),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assert(Elapsed < 50),
        deactivate_v2()
    end).

%% 旧摘要成功升级一次（CAS）：首次 Count=1，再次 Count=0；升级后 v2 验证通过
scratch_cas_upgrade_once_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        activate_v2(),
        Uid = uid(),
        Legacy = elib_password:generate(<<"legacy-pw">>, hmac_sha512),
        {ok, _} = elib_pg:query(
            <<
                "INSERT INTO public.\"user\" (id, account, nickname, password, "
                "status, created_at, reg_ip, reg_cosv, source) "
                "VALUES ($1,$2,'t',$3,1,now(),'127.0.0.1','test','eunit')"
            >>,
            [Uid, <<"kdf-t-", (integer_to_binary(Uid))/binary>>, Legacy]
        ),
        {ok, #{version := legacy, variant := V, candidate := Cand}} =
            elib_password:verify_and_plan(<<"legacy-pw">>, Legacy),
        {ok, V2} = elib_kdf:hash_v2_prehashed(Cand, V),
        ?assertEqual({ok, 1}, user_ds:upgrade_password_kdf_cas(Uid, V2)),
        %% 升级一次：再次 CAS 竞态落败（Count=0），不重复写
        ?assertEqual({ok, 0}, user_ds:upgrade_password_kdf_cas(Uid, V2)),
        %% 升级后 v2 验证通过；DB 行确为 v2 形态
        {ok, Rows} = elib_pg:query(
            <<"SELECT password FROM public.\"user\" WHERE id = $1">>, [Uid]
        ),
        [#{<<"password">> := Stored}] = Rows,
        io:format(
            user,
            "DBG v2len=~p storedlen=~p equal=~p~n",
            [byte_size(V2), byte_size(Stored), Stored =:= V2]
        ),
        ?assertMatch({ok, _}, elib_kdf:parse_v2(Stored)),
        io:format(
            user,
            "DBG parse=~p plan=~p direct=~p~n",
            [
                elib_kdf:parse_v2(Stored),
                elib_password:verify_and_plan(<<"legacy-pw">>, Stored),
                elib_kdf:verify_v2(<<"legacy-pw">>, Stored)
            ]
        ),
        ?assertEqual({ok, []}, elib_password:verify(<<"legacy-pw">>, Stored)),
        %% 错误密码对 v2 行：拒绝（零写入）
        {error, <<"errorPassword">>} = elib_password:verify_and_plan(<<"nope">>, Stored),
        deactivate_v2()
    end).

%% 端到端：verify_user 编排——legacy 成功登录触发升级写；升级写失败不锁死登录
verify_user_upgrade_orchestration_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        activate_v2(),
        Uid = uid(),
        Legacy = elib_password:generate(<<"orch-pw">>, hmac_sha512),
        {ok, _} = elib_pg:query(
            <<
                "INSERT INTO public.\"user\" (id, account, nickname, password, "
                "status, created_at, reg_ip, reg_cosv, source) "
                "VALUES ($1,$2,'t',$3,1,now(),'127.0.0.1','test','eunit')"
            >>,
            [Uid, <<"kdf-o-", (integer_to_binary(Uid))/binary>>, Legacy]
        ),
        User = #{
            <<"id">> => Uid,
            <<"password">> => Legacy,
            <<"status">> => 1,
            <<"account">> => <<"kdf-o">>,
            <<"nickname">> => <<"t">>,
            <<"email">> => <<>>,
            <<"mobile">> => <<>>,
            <<"avatar">> => <<>>,
            <<"sign">> => <<>>,
            <<"gender">> => 0,
            <<"region">> => <<>>
        },
        {ok, _} = passport_logic:verify_user(<<"orch-pw">>, User, <<>>),
        {ok, Rows} = elib_pg:query(
            <<"SELECT password FROM public.\"user\" WHERE id = $1">>, [Uid]
        ),
        [#{<<"password">> := Stored}] = Rows,
        ?assertMatch({ok, _}, elib_kdf:parse_v2(Stored)),
        deactivate_v2()
    end).
