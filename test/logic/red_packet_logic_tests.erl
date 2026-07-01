-module(red_packet_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc red_packet_logic:send/5 余额不足回归测试。
%%%
%%% wallet_repo:atomic_balance_change 余额不足时抛 {rollback, insufficient_balance}
%%% （经 elib_pg:with_tx 原样返回），不是 {error, insufficient_balance}；
%%% send/5 曾错误按后者匹配，导致发红包余额不足时必现 case_clause 崩溃而非友好错误。
%%% 手法：meck wallet_ds，绝不触真实 PG。
%%% @end
%%%===================================================================

-define(UID, 5201).

setup() ->
    application:set_env(imboy, env, test),
    meck:new(wallet_ds, [no_link, passthrough]),
    meck:new(elib_tsid, [no_link, passthrough]),
    meck:expect(wallet_ds, ensure_wallet, fun(?UID) ->
        #{<<"id">> => 8001, <<"user_id">> => ?UID, <<"balance">> => 100}
    end),
    %% gen_ref_no/0 调 elib_tsid:generate/1，需 register；meck 规避
    meck:expect(elib_tsid, generate, fun(_) -> 88001 end),
    ok.

cleanup(_) ->
    meck:unload(elib_tsid),
    meck:unload(wallet_ds),
    ok.

red_packet_send_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun send_insufficient_balance_rejected/0
    ]}.

send_insufficient_balance_rejected() ->
    meck:expect(wallet_ds, atomic_balance_change, fun(-100000, ?UID, _TxData, _RefNo) ->
        {rollback, insufficient_balance}
    end),
    ?assertEqual(
        {error, <<"钱包余额不足"/utf8>>},
        red_packet_logic:send(?UID, <<"fixed">>, 100000, 1, <<"恭喜发财"/utf8>>)
    ).
