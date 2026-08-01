-module(channel_logic_order_pay_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc channel_logic_order:pay_order/2 统一支付信封契约测试。
%%%
%%% 验证 S4 缺口修复：pay_order 把网关返回的支付参数透传为统一信封
%%%   #{<<"payment_method">>, <<"payment_no">>, <<"pay_params">>, <<"status">>}（binary key）。
%%%   - wallet/mock：pay_params 为空 map（即时入账，无第三方拉起），status=1。
%%%   - alipay：pay_params 含 #{<<"order_str">>}，status=0。
%%%
%%% 并验证 B-01：第三方网关的 pay/3 只创建**支付意图**，此时用户尚未付款，
%%%   pay_order 不得标记订单已支付、不得订阅频道（否则等于零元购）。
%%%   发货由 payment_callback_logic 收到回调后完成。
%%%
%%% 手法：meck 所有 DS/Logic 边界（channel_order_ds / channel_ds /
%%%   channel_logic_notify）+ payment_gateway，绝不触真实 PG。
%%% @end
%%%===================================================================

-define(UID, 2002).
-define(CID, 11).

setup() ->
    %% 非生产环境（默认 local）允许 mock 支付方式；env 默认即可。
    meck:new(channel_order_ds, [no_link, passthrough]),
    meck:new(channel_ds, [no_link, passthrough]),
    meck:new(channel_logic_notify, [no_link, passthrough]),
    meck:new(payment_gateway, [no_link, passthrough]),
    %% 默认：订单存在、归属当前用户、待支付（status=0）
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 0
        }}
    end),
    meck:expect(channel_order_ds, pay, fun(_OrderNo, _PaymentData) -> ok end),
    meck:expect(channel_ds, subscribe, fun(_ChannelId, _Uid) -> ok end),
    meck:expect(channel_logic_notify, notify_order_paid, fun(_ChannelId, _Uid) -> ok end),
    ok.

cleanup(_) ->
    meck:unload(payment_gateway),
    meck:unload(channel_logic_notify),
    meck:unload(channel_ds),
    meck:unload(channel_order_ds),
    ok.

pay_order_envelope_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun mock_method_returns_envelope_with_empty_pay_params/0,
        fun thirdparty_method_returns_envelope_with_pay_params/0,
        fun gateway_error_propagated/0,
        fun thirdparty_does_not_ship_before_callback/0,
        fun wallet_ships_immediately/0
    ]}.

%% mock 网关（无第三元组）→ 信封 pay_params 为空 map
mock_method_returns_envelope_with_empty_pay_params() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 0,
            <<"payment_method">> => <<"mock">>
        }}
    end),
    meck:expect(payment_gateway, pay, fun(<<"mock">>, OrderNo, _Opts) ->
        {ok, <<"MOCK_", OrderNo/binary>>}
    end),
    Result = channel_logic_order:pay_order(?UID, <<"ORD_M1">>),
    ?assertEqual(
        {ok, #{
            <<"payment_method">> => <<"mock">>,
            <<"payment_no">> => <<"MOCK_ORD_M1">>,
            <<"pay_params">> => #{},
            <<"status">> => 1
        }},
        Result
    ).

%% 第三方网关（三元组 Extra）→ 信封 pay_params 透传网关支付参数
thirdparty_method_returns_envelope_with_pay_params() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 0,
            <<"payment_method">> => <<"alipay">>
        }}
    end),
    meck:expect(payment_gateway, pay, fun(<<"alipay">>, OrderNo, Opts) ->
        %% channel 第三方网关期望「分」：9.90 元 → 990 分
        ?assertEqual(990, maps:get(amount, Opts)),
        {ok, <<"ALIPAY_", OrderNo/binary>>, #{<<"order_str">> => <<"orderstr_xyz">>}}
    end),
    Result = channel_logic_order:pay_order(?UID, <<"ORD_T1">>),
    ?assertEqual(
        {ok, #{
            <<"payment_method">> => <<"alipay">>,
            <<"payment_no">> => <<"ALIPAY_ORD_T1">>,
            <<"pay_params">> => #{<<"order_str">> => <<"orderstr_xyz">>},
            <<"status">> => 0
        }},
        Result
    ).

%% 网关失败 → 原样透传 error，不产生信封
gateway_error_propagated() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 0,
            <<"payment_method">> => <<"alipay">>
        }}
    end),
    meck:expect(payment_gateway, pay, fun(<<"alipay">>, _OrderNo, _Opts) ->
        {error, <<"支付网关未配置真实凭据"/utf8>>}
    end),
    ?assertEqual(
        {error, <<"支付网关未配置真实凭据"/utf8>>},
        channel_logic_order:pay_order(?UID, <<"ORD_T2">>)
    ).

%% B-01 核心断言：第三方网关下单后订单仍待支付、频道不可见。
%% 支付意图 ≠ 已付款；发货必须由回调触发，否则调 /order/pay 即白拿频道。
thirdparty_does_not_ship_before_callback() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 0,
            <<"payment_method">> => <<"wechat">>
        }}
    end),
    meck:expect(payment_gateway, pay, fun(<<"wechat">>, OrderNo, _Opts) ->
        {ok, <<"WX_", OrderNo/binary>>, #{<<"prepay_id">> => <<"pp_1">>}}
    end),
    {ok, Envelope} = channel_logic_order:pay_order(?UID, <<"ORD_T3">>),
    ?assertEqual(0, maps:get(<<"status">>, Envelope)),
    %% 三个发货动作一个都不许发生
    ?assertNot(meck:called(channel_order_ds, pay, '_')),
    ?assertNot(meck:called(channel_ds, subscribe, '_')),
    ?assertNot(meck:called(channel_logic_notify, notify_order_paid, '_')).

%% 反向对照：钱包余额是同步扣走的，必须就地发货，不能等一个永远不会来的回调。
wallet_ships_immediately() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 0,
            <<"payment_method">> => <<"wallet">>
        }}
    end),
    meck:expect(payment_gateway, pay, fun(<<"wallet">>, OrderNo, _Opts) ->
        {ok, <<"WPY_", OrderNo/binary>>}
    end),
    {ok, Envelope} = channel_logic_order:pay_order(?UID, <<"ORD_W1">>),
    ?assertEqual(1, maps:get(<<"status">>, Envelope)),
    ?assert(meck:called(channel_order_ds, pay, '_')),
    ?assert(meck:called(channel_ds, subscribe, [?CID, ?UID])).

%%%===================================================================
%%% B-03：超时未支付订单在**查询时**显示为已过期(4)。
%%% 不写库 —— 迟到的回调仍要能按 B-08 补单。
%%%===================================================================

order_expiry_view_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun expired_pending_order_reads_as_expired/0,
        fun unexpired_pending_order_keeps_pending/0,
        fun paid_order_never_marked_expired/0,
        fun missing_expires_at_keeps_pending/0
    ]}.

%% 构造一个 status/expires_at 可控的订单
stub_order(Status, ExpiresAt) ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => Status,
            <<"expires_at">> => ExpiresAt
        }}
    end).

%% PG timestamptz 经 epgsql 回来是 {{Y,M,D},{H,Mi,S}}（UTC）
utc_datetime(OffsetMs) ->
    Secs = (elib_dt:millisecond() + OffsetMs) div 1000,
    calendar:gregorian_seconds_to_datetime(Secs + 62167219200).

expired_pending_order_reads_as_expired() ->
    stub_order(0, utc_datetime(-60 * 1000)),
    {ok, Order} = channel_logic_order:get_order(?UID, <<"ORD_E1">>),
    ?assertEqual(4, maps:get(<<"status">>, Order)).

unexpired_pending_order_keeps_pending() ->
    stub_order(0, utc_datetime(10 * 60 * 1000)),
    {ok, Order} = channel_logic_order:get_order(?UID, <<"ORD_E2">>),
    ?assertEqual(0, maps:get(<<"status">>, Order)).

%% 已支付订单即使 expires_at 早已过去也不能被改成过期
paid_order_never_marked_expired() ->
    stub_order(1, utc_datetime(-86400 * 1000)),
    {ok, Order} = channel_logic_order:get_order(?UID, <<"ORD_E3">>),
    ?assertEqual(1, maps:get(<<"status">>, Order)).

%% 取不到 expires_at 时按未过期处理，不误杀
missing_expires_at_keeps_pending() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 0
        }}
    end),
    {ok, Order} = channel_logic_order:get_order(?UID, <<"ORD_E4">>),
    ?assertEqual(0, maps:get(<<"status">>, Order)).
