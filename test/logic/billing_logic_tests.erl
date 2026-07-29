-module(billing_logic_tests).

%%%===================================================================
%%% @doc billing_logic 纯函数单测（套餐校验/周期换算/jsonb 编解码）
%%%
%%% 覆盖 SaaS 计费的配置校验与金额/周期相关纯逻辑，防回归。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%% 套餐校验：空编码/空名称/负价格/非法周期拒绝；合法通过
validate_plan_test() ->
    ?assertEqual(ok, billing_logic:validate_plan(<<"pro">>, <<"专业版"/utf8>>, 39000, <<"month">>)),
    ?assertEqual(ok, billing_logic:validate_plan(<<"ent">>, <<"企业版"/utf8>>, 0, <<"year">>)),
    ?assertMatch({error, _}, billing_logic:validate_plan(<<>>, <<"x">>, 100, <<"month">>)),
    ?assertMatch({error, _}, billing_logic:validate_plan(<<"c">>, <<>>, 100, <<"month">>)),
    ?assertMatch({error, _}, billing_logic:validate_plan(<<"c">>, <<"x">>, -1, <<"month">>)),
    ?assertMatch({error, _}, billing_logic:validate_plan(<<"c">>, <<"x">>, 100, <<"week">>)).

%% 周期 → 毫秒（month≈30 天 / year≈365 天）
period_ms_test() ->
    ?assertEqual(30 * 86400000, billing_logic:period_ms(<<"month">>)),
    ?assertEqual(365 * 86400000, billing_logic:period_ms(<<"year">>)),
    %% 未知周期回退 month
    ?assertEqual(30 * 86400000, billing_logic:period_ms(<<"unknown">>)).

%% 计费周期标识解析：显式值原样返回；缺省返回当前 YYYY-MM 格式
resolve_period_test() ->
    ?assertEqual(<<"2026-06">>, billing_logic:resolve_period(<<"2026-06">>)),
    P = billing_logic:resolve_period(undefined),
    ?assertEqual(7, byte_size(P)),
    ?assertMatch([_Y, _M], binary:split(P, <<"-">>)).

%% 月份补零
pad2_test() ->
    ?assertEqual(<<"05">>, billing_logic:pad2(5)),
    ?assertEqual(<<"12">>, billing_logic:pad2(12)),
    ?assertEqual(<<"01">>, billing_logic:pad2(1)).

%% jsonb 编码：map→JSON，binary 原样
encode_jsonb_test() ->
    ?assertEqual(<<"{}">>, billing_logic:encode_jsonb(#{})),
    ?assertEqual(<<"already_json">>, billing_logic:encode_jsonb(<<"already_json">>)),
    Enc = billing_logic:encode_jsonb(#{<<"message">> => 1000}),
    ?assert(is_binary(Enc)),
    ?assertEqual(#{<<"message">> => 1000}, jsone:decode(Enc)).

%% jsonb 解码：JSON binary→map，map 原样，非法→空 map
decode_jsonb_test() ->
    ?assertEqual(#{<<"a">> => 1}, billing_logic:decode_jsonb(<<"{\"a\":1}">>)),
    ?assertEqual(#{<<"a">> => 1}, billing_logic:decode_jsonb(#{<<"a">> => 1})),
    ?assertEqual(#{}, billing_logic:decode_jsonb(<<"not json">>)),
    ?assertEqual(#{}, billing_logic:decode_jsonb(undefined)),
    ?assertEqual(#{}, billing_logic:decode_jsonb(null)).

%% 毫秒时间戳 → timestamptz SQL 片段
ts_ms_to_sql_test() ->
    ?assertEqual(
        <<"to_timestamp(1771481621000::bigint/1000)">>,
        billing_logic:ts_ms_to_sql(1771481621000)
    ).

%%%===================================================================
%%% 归属校验（W0-SEC-01 / C0-BILL-01）—— 8 类跨用户拒绝
%%%===================================================================
-define(OWNER, 1001).
-define(ATTACKER, 2002).
-define(SUB_ID, 500001).

owner_guard_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun t1_owner_allowed/0,
        fun t2_other_user_denied/0,
        fun t3_ownerless_legacy_denied/0,
        fun t4_missing_subscription_denied/0,
        fun t5_invalid_sub_id_denied/0,
        fun t6_invalid_uid_denied/0,
        fun t7_invoice_owner_allowed/0,
        fun t8_invoice_other_user_denied/0,
        fun t9_invoice_missing_denied/0,
        fun t10_denial_message_does_not_leak_existence/0
    ]}.

setup() ->
    meck:new(billing_subscription_ds, [passthrough, non_strict]),
    meck:new(billing_invoice_ds, [passthrough, non_strict]),
    ok.

cleanup(_) ->
    catch meck:unload(billing_invoice_ds),
    catch meck:unload(billing_subscription_ds),
    ok.

sub_with_owner(OwnerUid) ->
    #{
        <<"id">> => ?SUB_ID,
        <<"tenant_id">> => 0,
        <<"owner_uid">> => OwnerUid,
        <<"plan_id">> => 7,
        <<"status">> => 1
    }.

expect_sub(Sub) ->
    meck:expect(billing_subscription_ds, find_by_id, fun(_) -> Sub end).

%% 1) 本人操作自己的订阅 → 放行
t1_owner_allowed() ->
    expect_sub(sub_with_owner(?OWNER)),
    ?assertEqual(ok, billing_logic:assert_owner(?SUB_ID, ?OWNER)).

%% 2) 他人订阅 → 拒绝（核心越权场景）
t2_other_user_denied() ->
    expect_sub(sub_with_owner(?OWNER)),
    ?assertMatch({error, _}, billing_logic:assert_owner(?SUB_ID, ?ATTACKER)).

%% 3) owner_uid=0 的历史无主订阅 → 用户端一律拒绝，不得被任意用户接管
t3_ownerless_legacy_denied() ->
    expect_sub(sub_with_owner(0)),
    ?assertMatch({error, _}, billing_logic:assert_owner(?SUB_ID, ?OWNER)),
    ?assertMatch({error, _}, billing_logic:assert_owner(?SUB_ID, ?ATTACKER)).

%% 4) 订阅不存在 → 拒绝
t4_missing_subscription_denied() ->
    expect_sub(#{}),
    ?assertMatch({error, _}, billing_logic:assert_owner(?SUB_ID, ?OWNER)).

%% 5) subscription_id 非法（0/负数/非整数）→ 拒绝且不触达 DS
t5_invalid_sub_id_denied() ->
    meck:expect(billing_subscription_ds, find_by_id, fun(_) ->
        erlang:error(should_not_be_called)
    end),
    lists:foreach(
        fun(Bad) -> ?assertMatch({error, _}, billing_logic:assert_owner(Bad, ?OWNER)) end,
        [0, -1, undefined, <<"500001">>]
    ).

%% 6) uid 非法（未登录场景）→ 拒绝且不触达 DS
t6_invalid_uid_denied() ->
    meck:expect(billing_subscription_ds, find_by_id, fun(_) ->
        erlang:error(should_not_be_called)
    end),
    lists:foreach(
        fun(Bad) -> ?assertMatch({error, _}, billing_logic:assert_owner(?SUB_ID, Bad)) end,
        [0, -1, undefined, <<"1001">>]
    ).

%% 7) 账单支付：invoice → subscription 反查归属，本人放行
t7_invoice_owner_allowed() ->
    expect_sub(sub_with_owner(?OWNER)),
    meck:expect(billing_invoice_ds, find_by_invoice_no, fun(<<"INV-1">>) ->
        #{<<"invoice_no">> => <<"INV-1">>, <<"subscription_id">> => ?SUB_ID}
    end),
    ?assertEqual(ok, billing_logic:assert_invoice_owner(<<"INV-1">>, ?OWNER)).

%% 8) 账单支付：他人账单 → 拒绝（付款人不得替他人订阅结算）
t8_invoice_other_user_denied() ->
    expect_sub(sub_with_owner(?OWNER)),
    meck:expect(billing_invoice_ds, find_by_invoice_no, fun(<<"INV-1">>) ->
        #{<<"invoice_no">> => <<"INV-1">>, <<"subscription_id">> => ?SUB_ID}
    end),
    ?assertMatch({error, _}, billing_logic:assert_invoice_owner(<<"INV-1">>, ?ATTACKER)).

%% 9) 账单不存在 / 账单号非法 → 拒绝
t9_invoice_missing_denied() ->
    meck:expect(billing_invoice_ds, find_by_invoice_no, fun(_) -> #{} end),
    ?assertMatch({error, _}, billing_logic:assert_invoice_owner(<<"NOPE">>, ?OWNER)),
    ?assertMatch({error, _}, billing_logic:assert_invoice_owner(<<>>, ?OWNER)),
    ?assertMatch({error, _}, billing_logic:assert_invoice_owner(undefined, ?OWNER)).

%% 10) 拒绝文案不得区分「不存在」与「非本人」，否则可枚举他人订阅 id
t10_denial_message_does_not_leak_existence() ->
    expect_sub(sub_with_owner(?OWNER)),
    {error, MsgOther} = billing_logic:assert_owner(?SUB_ID, ?ATTACKER),
    expect_sub(#{}),
    {error, MsgMissing} = billing_logic:assert_owner(?SUB_ID, ?ATTACKER),
    ?assertEqual(MsgOther, MsgMissing).
