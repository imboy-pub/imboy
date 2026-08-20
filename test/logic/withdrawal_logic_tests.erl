-module(withdrawal_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc withdrawal_logic:withdraw/4 提现安全契约测试。
%%%
%%% 对应路由 POST /v1/wallet/withdraw（P0）。验证：① 正常提现返回新余额；
%%%   ② 余额不足 → 拒绝（依赖 repo 的 balance+$1>=0 透支守卫，此处 meck 模拟）；
%%%   ③ 金额非法（< 100）→ 拒绝。user_id 来自 JWT（无 IDOR）。
%%%   手法：meck wallet_ds，绝不触真实 PG。
%%% @end
%%%===================================================================

-define(UID, 4301).

setup() ->
    application:set_env(imboy, env, test),
    meck:new(wallet_ds, [no_link, passthrough]),
    meck:new(elib_tsid, [no_link, passthrough]),
    meck:expect(wallet_ds, ensure_wallet, fun(?UID) ->
        #{<<"id">> => 7001, <<"user_id">> => ?UID, <<"balance">> => 5000}
    end),
    %% gen_ref_no/0 调 elib_tsid:generate/1，需 register；meck 规避
    meck:expect(elib_tsid, generate, fun(_) -> 99001 end),
    ok.

cleanup(_) ->
    meck:unload(elib_tsid),
    meck:unload(wallet_ds),
    ok.

withdrawal_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun withdraw_success_returns_balance/0,
        fun withdraw_insufficient_balance_rejected/0,
        fun withdraw_amount_too_small_rejected/0,
        fun withdraw_generic_rollback_rejected/0
    ]}.

%% 正常路径：余额充足 → 扣款成功返回新余额
withdraw_success_returns_balance() ->
    meck:expect(wallet_ds, atomic_balance_change, fun(-300, ?UID, _TxData, _RefNo) ->
        {ok, 4700}
    end),
    {ok, Result} = withdrawal_logic:withdraw(?UID, 300, <<"alipay">>, <<"user@example">>),
    ?assertEqual(4700, maps:get(<<"balance">>, Result)),
    ?assertEqual(4700 / 100.0, maps:get(<<"balance_yuan">>, Result)),
    ?assert(is_binary(maps:get(<<"reference_no">>, Result))).

%% 非法输入：余额不足（repo 守卫触发）→ 拒绝
%% 回归：wallet_repo:atomic_balance_change 真实余额不足时抛 {rollback, insufficient_balance}
%% （经 elib_pg:with_tx 原样返回），不是 {error, insufficient_balance}；
%% 曾错误按后者匹配，导致提现余额不足时必现 case_clause 崩溃而非友好错误。
withdraw_insufficient_balance_rejected() ->
    meck:expect(wallet_ds, atomic_balance_change, fun(-99999, ?UID, _TxData, _RefNo) ->
        {rollback, insufficient_balance}
    end),
    ?assertEqual(
        {error, <<"钱包余额不足"/utf8>>},
        withdrawal_logic:withdraw(?UID, 99999, <<"wechat">>, <<"wx_id">>)
    ).

%% 非法输入：金额低于下限（0 < 1，下限=最小货币单位 1 分）→ 拒绝（不触 ds）
withdraw_amount_too_small_rejected() ->
    ?assertEqual(
        {error, <<"提现参数不合法"/utf8>>},
        withdrawal_logic:withdraw(?UID, 0, <<"alipay">>, <<"user@example">>)
    ).

%% 回归：事务内非余额不足的 DB 故障，with_tx 原样返回 {rollback, Reason}（非
%% insufficient_balance）。旧代码只匹配 {rollback, insufficient_balance}/{error, _}，
%% 缺通用 {rollback, _} 分支 → case_clause 崩溃（500）。修复后应归一为友好可重试错误。
withdraw_generic_rollback_rejected() ->
    meck:expect(wallet_ds, atomic_balance_change, fun(-300, ?UID, _TxData, _RefNo) ->
        {rollback, {pgsql_error, #{code => <<"40001">>}}}
    end),
    ?assertEqual(
        {error, <<"提现失败，请稍后再试"/utf8>>},
        withdrawal_logic:withdraw(?UID, 300, <<"alipay">>, <<"user@example">>)
    ).
