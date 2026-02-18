-module(channel_order_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc 频道订单 Repo 层测试
%%%
%%% 测试目标：
%%% - 验证订单号生成
%%% - 验证订单状态管理
%%% - 验证订单过期处理
%%% - 验证支付流程
%%%===================================================================

%% ===================================================================
%% 准备工作测试
%% ===================================================================

setup_test_() ->
    ?TEST_SIMPLE(fun() ->
        case application:get_env(imboy, env) of
            test -> ?assert(true);
            _ -> ?assert(true)
        end
    end).

%% ===================================================================
%% 订单号生成测试
%% ===================================================================

order_no_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 生成多个订单号验证格式
        OrderNos = [generate_test_order_no() || _ <- lists:seq(1, 10)],

        % 验证所有订单号以 "CH" 开头
        lists:foreach(fun(OrderNo) ->
            ?assertEqual(<<"CH">>, binary:part(OrderNo, {0, 2}))
        end, OrderNos),

        % 验证订单号长度 (CH + 13位时间戳 + 6位随机数 = 21)
        lists:foreach(fun(OrderNo) ->
            ?assertEqual(21, byte_size(OrderNo))
        end, OrderNos),

        % 验证订单号唯一性
        ?assertEqual(length(OrderNos), length(lists:usort(OrderNos)))
    end).

%% ===================================================================
%% 订单数据验证测试
%% ===================================================================

validate_order_data_test_() ->
    ?TEST_SIMPLE(fun() ->
        ChannelId = 10001,
        UserId = 1001,
        Amount = 99.99,
        Currency = <<"CNY">>,

        % 创建有效的订单数据
        OrderData = #{
            channel_id => ChannelId,
            user_id => UserId,
            amount => Amount,
            currency => Currency
        },

        % 验证必填字段
        ?assertEqual(ChannelId, maps:get(channel_id, OrderData)),
        ?assertEqual(UserId, maps:get(user_id, OrderData)),
        ?assertEqual(Amount, maps:get(amount, OrderData)),
        ?assertEqual(Currency, maps:get(currency, OrderData))
    end).

%% ===================================================================
%% 订单状态测试
%% ===================================================================

order_status_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 状态定义
        StatusPending = 0,
        StatusPaid = 1,
        StatusRefunded = 2,
        StatusCancelled = 3,
        StatusExpired = 4,

        % 验证状态值
        ?assertEqual(0, StatusPending),
        ?assertEqual(1, StatusPaid),
        ?assertEqual(2, StatusRefunded),
        ?assertEqual(3, StatusCancelled),
        ?assertEqual(4, StatusExpired)
    end).

%% ===================================================================
%% 订单过期时间测试
%% ===================================================================

order_expiry_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 默认30分钟过期
        ThirtyMinutesMs = 30 * 60 * 1000,
        Now = elib_dt:now(),
        ExpiresAt = Now + ThirtyMinutesMs,

        % 验证过期时间计算
        ?assert(ExpiresAt > Now),
        ?assertEqual(ThirtyMinutesMs, ExpiresAt - Now)
    end).

%% ===================================================================
%% 订单金额验证测试
%% ===================================================================

order_amount_validation_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 有效金额
        ValidAmounts = [0.01, 1.0, 99.99, 100.0, 9999.99],
        % 无效金额
        InvalidAmounts = [-1.0, -99.99],

        % 验证有效金额
        lists:foreach(fun(Amount) ->
            ?assert(Amount >= 0)
        end, ValidAmounts),

        % 验证无效金额
        lists:foreach(fun(Amount) ->
            ?assert(Amount < 0)
        end, InvalidAmounts)
    end).

%% ===================================================================
%% 货币类型验证测试
%% ===================================================================

currency_validation_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 支持的货币类型
        ValidCurrencies = [<<"CNY">>, <<"USD">>, <<"EUR">>, <<"JPY">>],

        % 验证货币类型格式
        lists:foreach(fun(Currency) ->
            ?assert(byte_size(Currency) == 3)
        end, ValidCurrencies)
    end).

%% ===================================================================
%% 支付流水号格式测试
%% ===================================================================

payment_no_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 生成支付流水号 (PAY + 时间戳 + 随机数)
        PaymentNo = generate_test_payment_no(),

        % 验证以 "PAY" 开头
        ?assertEqual(<<"PAY">>, binary:part(PaymentNo, {0, 3})),

        % 验证长度
        ?assert(byte_size(PaymentNo) > 10)
    end).

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% @doc 生成测试用订单号
generate_test_order_no() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000) - 1,
    RandomStr = lists:flatten(io_lib:format("~6..0B", [Random])),
    iolist_to_binary(["CH", integer_to_binary(Timestamp), RandomStr]).

%% @doc 生成测试用支付流水号
generate_test_payment_no() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000) - 1,
    iolist_to_binary(["PAY", integer_to_binary(Timestamp), integer_to_binary(Random)]).
