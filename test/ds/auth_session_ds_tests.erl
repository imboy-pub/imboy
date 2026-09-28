-module(auth_session_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% Task 10 / LT-04：持久化会话吊销（session epoch）矩阵。
%%% 覆盖卡片 Step1 的冻结清单：
%%%   改密/全端登出 bump → 旧 token 失效；账号禁用同机制；
%%%   单端登出走设备管道（回归位）；restart 持久化（DB 权威）；
%%%   跨账号隔离；store 不可用 fail-closed；legacy/malformed claim 语义。

uid() ->
    %% A1e（CP-TD-A02 run2 实证）：unique_integer rem 1e9 + 1000 落在小整数域，
    %% 与其他套件的固定小 uid（adm/adm_session 测试用 2350、3951 等）共享
    %% {auth_session_epoch, Uid} 缓存键空间；并行套件 bump 后的 60s memo
    %% 会毒化本套件 missing-row 断言（current_epoch 得 {ok,2}，表内实无行）。
    %% 改用 TSID：本轮唯一、不与小 uid 域相交（同 group_user_id_sum_p0_tests
    %% 的防撞先例）；user_auth_epoch.user_id 为 bigint，TSID 容纳无虞。
    elib_tsid:generate().

%% 缺行默认 epoch=1（已知默认态，非未知态）
current_epoch_missing_row_defaults_to_1_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        Uid = uid(),
        ?assertEqual({ok, 1}, auth_session_ds:current_epoch(Uid)),
        ?assertEqual(false, auth_session_ds:revoked(Uid, 1))
    end).

%% bump：旧 epoch 的 token 立即吊销，新 epoch 的 token 有效
bump_advances_and_revokes_old_tokens_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        Uid = uid(),
        ?assertEqual({ok, 1}, auth_session_ds:current_epoch(Uid)),
        ok = auth_session_ds:bump(Uid),
        ?assertEqual({ok, 2}, auth_session_ds:current_epoch(Uid)),
        ?assertEqual(true, auth_session_ds:revoked(Uid, 1)),
        ?assertEqual(false, auth_session_ds:revoked(Uid, 2))
    end).

%% restart 持久化：epoch 以 DB 为权威，清空进程内缓存后仍是 bump 后的值
epoch_survives_cache_flush_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        Uid = uid(),
        ok = auth_session_ds:bump(Uid),
        ok = auth_session_ds:bump(Uid),
        imboy_cache:flush(),
        ?assertEqual({ok, 3}, auth_session_ds:current_epoch(Uid))
    end).

%% 跨账号隔离：bump 一个用户不影响其他用户的会话有效性
cross_account_isolation_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        UidA = uid(),
        UidB = uid(),
        ok = auth_session_ds:bump(UidA),
        ?assertEqual(true, auth_session_ds:revoked(UidA, 1)),
        ?assertEqual(false, auth_session_ds:revoked(UidB, 1))
    end).

%% store 不可用 fail-closed：epoch 现势无法确认 ⇒ 一律判已吊销
fail_closed_when_store_unavailable_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        Uid = uid(),
        meck:new(elib_pg, [passthrough]),
        try
            meck:expect(elib_pg, query, fun(_Sql, _Params) ->
                {error, pool_unavailable}
            end),
            ?assertEqual({error, unavailable}, auth_session_ds:current_epoch(Uid)),
            ?assertEqual(true, auth_session_ds:revoked(Uid, 1))
        after
            _ = (catch meck:unload(elib_pg))
        end
    end).

%% legacy 语义：无 ep claim（undefined）豁免；malformed ep fail-closed
claim_semantics_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, auth_session_ds:revoked(1, undefined)),
        ?assertEqual(true, auth_session_ds:revoked(1, malformed)),
        ?assertEqual(true, auth_session_ds:revoked(1, <<>>))
    end).

%% 端到端形态：did 绑定 token 携带 ep；空 did（legacy 形态）不携带
token_carries_ep_when_did_bound_test_() ->
    ?TEST_SIMPLE(fun() ->
        Token = token_ds:encrypt_token(424242, <<"did-t10">>),
        {ok, 424242, _Exp, <<"tk">>, <<"did-t10">>, Ep} = token_ds:decrypt_token(Token),
        ?assert(Ep =:= 1 orelse is_integer(Ep)),
        %% 空 did：legacy 形态，无 ep
        Legacy = token_ds:encrypt_token(424242),
        {ok, 424242, _Exp2, <<"tk">>, <<>>, undefined} = token_ds:decrypt_token(Legacy)
    end).
