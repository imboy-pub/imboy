-module(transfer_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc transfer_logic:send/4 转账安全契约测试。
%%%
%%% 对应路由 POST /v1/wallet/transfer/send（P0）。验证：① 正常转账（meck repo）；
%%%   ② 自转（SenderUid==ReceiverUid）拒绝；③ 金额低于下限拒绝。发起人 user_id
%%%   来自 JWT（handler 层），此处聚焦 logic 层不变量。手法：meck transfer_repo。
%%% @end
%%%===================================================================

-define(UID, 4401).
-define(OTHER, 4402).

setup() ->
    application:set_env(imboy, env, test),
    meck:new(transfer_repo, [no_link, passthrough]),
    meck:new(user_logic, [no_link, passthrough]),
    ok.

cleanup(_) ->
    meck:unload(transfer_repo),
    meck:unload(user_logic),
    ok.

transfer_send_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun send_success_returns_id/0,
        fun send_self_transfer_rejected/0,
        fun send_amount_too_small_rejected/0,
        fun send_receiver_not_found_rejected/0
    ]}.

%% 正常路径：合法金额 + 非自转 + 接收者存在 → 创建转账单
send_success_returns_id() ->
    meck:expect(user_logic, find_by_id, fun(?OTHER) -> #{<<"id">> => ?OTHER} end),
    meck:expect(transfer_repo, create, fun(?UID, ?OTHER, 1000, _Remark) -> {ok, 88001} end),
    {ok, TransferId} = transfer_logic:send(?UID, ?OTHER, 1000, <<"备注"/utf8>>),
    ?assertEqual(88001, TransferId).

%% 非法输入：自转（SenderUid==ReceiverUid）→ 拒绝（不触 repo）
send_self_transfer_rejected() ->
    ?assertEqual(
        {error, <<"转账参数不合法"/utf8>>},
        transfer_logic:send(?UID, ?UID, 1000, <<>>)
    ).

%% 非法输入：金额低于下限（0 < 1，下限=最小货币单位 1 分）→ 拒绝
send_amount_too_small_rejected() ->
    ?assertEqual(
        {error, <<"转账参数不合法"/utf8>>},
        transfer_logic:send(?UID, ?OTHER, 0, <<>>)
    ).

%% 边界：接收者不存在 → 拒绝（不触 repo；此前转给幻影 uid 资金永久挂起，
%% 只能 eval transfer_logic:refund/1 人工回收，无自动退款）
send_receiver_not_found_rejected() ->
    meck:expect(user_logic, find_by_id, fun(?OTHER) -> #{} end),
    ?assertEqual(
        {error, <<"接收者不存在"/utf8>>},
        transfer_logic:send(?UID, ?OTHER, 1000, <<>>)
    ).
