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
