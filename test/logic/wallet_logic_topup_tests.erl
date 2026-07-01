-module(wallet_logic_topup_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc wallet_logic:topup_enabled_for_env/1 环境门禁测试。
%%%
%%% 防护不变量：/v1/wallet/topup（mock 充值）仅非生产环境允许；
%%%   生产（pro/prod/production）及未配置（<<>>，按生产对待）一律拒绝，
%%%   防止生产环境凭空生成钱包余额。纯函数，无需 meck/DB。
%%% @end
%%%===================================================================

%% 生产及未配置环境 → 禁用（非法/危险输入）
prod_envs_disabled_test_() ->
    [
        ?_assertNot(wallet_logic:topup_enabled_for_env(E))
     || E <- [<<"pro">>, <<"prod">>, <<"production">>, <<>>]
    ].

%% 非生产环境 → 允许（正常路径）
dev_envs_enabled_test_() ->
    [
        ?_assert(wallet_logic:topup_enabled_for_env(E))
     || E <- [<<"local">>, <<"test">>, <<"dev">>]
    ].
