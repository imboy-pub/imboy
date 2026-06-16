-module(channel_logic_order_pay_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc channel_logic_order:pay_order/2 统一支付信封契约测试。
%%%
%%% 验证 S4 缺口修复：pay_order 把网关返回的支付参数透传为统一信封
%%%   #{<<"payment_method">>, <<"payment_no">>, <<"pay_params">>}（binary key）。
%%%   - wallet/mock：pay_params 为空 map（即时入账，无第三方拉起）。
%%%   - alipay：pay_params 含 #{<<"order_str">>}。
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
        fun gateway_error_propagated/0
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
            <<"pay_params">> => #{}
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
            <<"pay_params">> => #{<<"order_str">> => <<"orderstr_xyz">>}
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
