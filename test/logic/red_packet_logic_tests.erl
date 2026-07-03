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

%%%===================================================================
%%% @doc SEC-03: red_packet_logic:detail/2 归属校验回归测试。
%%% 仅发送者或已领取者可读红包详情，防任意登录用户凭红包 id 越权读
%%% 发送者/祝福语/金额/领取名单。手法：meck red_packet_repo，不触真实 PG。
%%% @end
%%%===================================================================

-define(SENDER_UID, 1001).
-define(RECEIVER_UID, 2002).
-define(STRANGER_UID, 3003).
-define(PACKET_ID, 77001).

detail_setup() ->
    application:set_env(imboy, env, test),
    meck:new(red_packet_repo, [no_link, passthrough]),
    meck:expect(red_packet_repo, find_by_id, fun(?PACKET_ID) ->
        #{
            <<"id">> => ?PACKET_ID,
            <<"sender_uid">> => ?SENDER_UID,
            <<"greeting">> => <<"恭喜发财"/utf8>>,
            <<"amount">> => 100
        }
    end),
    meck:expect(red_packet_repo, get_receivers, fun(?PACKET_ID) -> [] end),
    ok.

detail_cleanup(_) ->
    meck:unload(red_packet_repo),
    ok.

detail_test_() ->
    {foreach, fun detail_setup/0, fun detail_cleanup/1, [
        fun detail_rejects_stranger/0,
        fun detail_allows_sender/0,
        fun detail_allows_receiver/0
    ]}.

detail_rejects_stranger() ->
    %% 陌生人（非发送者、未领取）→ 越权拒绝
    meck:expect(red_packet_repo, find_receive_by_user, fun(?PACKET_ID, ?STRANGER_UID) ->
        #{}
    end),
    ?assertEqual(
        {error, <<"无权查看该红包详情"/utf8>>},
        red_packet_logic:detail(?PACKET_ID, ?STRANGER_UID)
    ).

detail_allows_sender() ->
    meck:expect(red_packet_repo, find_receive_by_user, fun(?PACKET_ID, ?SENDER_UID) ->
        #{}
    end),
    ?assertMatch({ok, _}, red_packet_logic:detail(?PACKET_ID, ?SENDER_UID)).

detail_allows_receiver() ->
    %% 已领取者 → 放行
    meck:expect(red_packet_repo, find_receive_by_user, fun(?PACKET_ID, ?RECEIVER_UID) ->
        #{<<"id">> => 9001, <<"receiver_uid">> => ?RECEIVER_UID, <<"amount">> => 50}
    end),
    ?assertMatch({ok, _}, red_packet_logic:detail(?PACKET_ID, ?RECEIVER_UID)).
