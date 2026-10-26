-module(eb_time_unit_contract_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc R08-BE 时间单位合同测试（纯函数，不依赖 PG）。
%%%
%%% 钉住三端消费的时间单位合同（state.json D04 / R08-AD / R08-FL）：
%%%   * EB 域（eb_pg_store_sql）wire = **Unix 秒**（零 wire 变更）；
%%%   * 通用 REST（elib_response → elib_cnv:convert_at_timestamps）
%%%     `*_at/*_ts` = **epoch 毫秒**；
%%%   * EB 归一化输出内不出现毫秒级混用（ts kind 全部走秒）。
%%% 若未来有人改动任一侧单位，此测试必须红。
%%%===================================================================

%% EB 域 to_unix：timestamptz/rfc3339 → 秒
eb_to_unix_outputs_seconds_test_() ->
    %% 2026-10-26T00:00:00Z = epoch 1792972800 秒（毫秒为 1792972800000）
    Sec = eb_pg_store_sql:to_unix(<<"2026-10-26T00:00:00Z">>),
    [
        ?_assert(is_integer(Sec)),
        ?_assertEqual(1792972800, Sec),
        %% 秒级（< 10^10），排除毫秒漂移
        ?_assert(Sec < 10000000000),
        ?_assertEqual(undefined, eb_pg_store_sql:to_unix(null)),
        ?_assertEqual(undefined, eb_pg_store_sql:to_unix(undefined))
    ].

%% EB normalize 的 ts kind 字段出站秒（抽样字段规格）
eb_normalize_ts_fields_seconds_test_() ->
    Row = #{
        <<"consent_at">> => <<"2026-10-26T00:00:00Z">>,
        <<"retain_until">> => <<"2026-11-01T12:00:00Z">>,
        <<"created_at">> => <<"2026-10-26T00:00:00Z">>
    },
    Fields = [
        {consent_at, <<"consent_at">>, ts},
        {retain_until, <<"retain_until">>, ts},
        {created_at, <<"created_at">>, ts}
    ],
    M = eb_pg_store_sql:normalize(Row, Fields),
    [
        ?_assertEqual(1792972800, maps:get(consent_at, M)),
        ?_assert(maps:get(retain_until, M) > maps:get(consent_at, M)),
        %% 全部字段均为秒级整数
        ?_assert(
            lists:all(
                fun(V) -> is_integer(V) andalso V < 10000000000 end,
                maps:values(M)
            )
        )
    ].

%% 通用 REST 合同（elib_cnv:convert_at_timestamps，elib_response 出站通用口径）：
%% 数值 *_ts/*_at 原样透传（毫秒整数）；RFC3339 字符串归一为毫秒 epoch 整数。
%% 与 state.json D04 wire（accepted_at 毫秒 int）一致。
rest_common_contract_millisecond_test_() ->
    Ms = 1792972800000,
    Out = elib_cnv:convert_at_timestamps(#{<<"sv_ts">> => Ms}),
    Out2 = elib_cnv:convert_at_timestamps(#{<<"updated_at">> => <<"2026-10-26T00:00:00Z">>}),
    [
        %% 数值毫秒原样透传（不做 rfc3339 字符串化、不换算）
        ?_assertEqual(Ms, maps:get(<<"sv_ts">>, Out)),
        %% rfc3339 字符串 → 毫秒 epoch 整数
        ?_assertEqual(Ms, maps:get(<<"updated_at">>, Out2))
    ].

%% EB 与通用 REST 单位口径显式不同：秒 vs 毫秒（同一瞬间的两条合同）
contracts_differ_by_design_test_() ->
    EbSec = eb_pg_store_sql:to_unix(<<"2026-10-26T00:00:00Z">>),
    [
        %% 同一瞬间：EB=秒级量级，通用 REST=毫秒级量级（3 个数量级差）
        ?_assert(EbSec * 1000 > 1000000000000),
        ?_assert(EbSec < 10000000000)
    ].
