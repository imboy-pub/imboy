%% enterprise_internal_rate_tests
%% EPGZ-02 — internal API 限流三桶（manifest rate_buckets / plan-gz §4.1 INV-9）。
%%
%% 冻结桶键：internal_read / internal_write / internal_sso。
%% 数值由配置下发（config/sys.config.example 的 imboy.enterprise_internal_rate_limits）；
%% **缺配置/部分配置/非正数值一律 fail-closed**（拒绝请求，不得 fail-open）。
%%
%% 纯内存测试（throttle 应用 + application:set_env），不需要 PG。
-module(enterprise_internal_rate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BUCKETS, [internal_read, internal_write, internal_sso]).
-define(TEST_RATES, #{
    internal_read => 3,
    internal_write => 3,
    internal_sso => 3
}).

setup_rate() ->
    {ok, _} = application:ensure_all_started(throttle),
    ok.

teardown_rate(_State) ->
    application:unset_env(imboy, enterprise_internal_rate_limits),
    ok.

rate_test_() ->
    {foreach, fun setup_rate/0, fun teardown_rate/1, [
        {"buckets_frozen", fun buckets_frozen/0},
        {"config_missing_fail_closed", fun config_missing_fail_closed/0},
        {"partial_config_fail_closed", fun partial_config_fail_closed/0},
        {"non_positive_limit_fail_closed", fun non_positive_limit_fail_closed/0},
        {"enforcement_allows_n_rejects_n_plus_1", fun enforcement_allows_n_rejects_n_plus_1/0},
        {"per_bucket_independent_counters", fun per_bucket_independent_counters/0},
        {"shipped_config_declares_all_buckets", fun shipped_config_declares_all_buckets/0}
    ]}.

%% ------------------------------------------------------------------
%% 桶键与 manifest 冻结一致
%% ------------------------------------------------------------------

buckets_frozen() ->
    ?assertEqual(lists:sort(?BUCKETS), lists:sort(enterprise_internal_rate:buckets())).

%% ------------------------------------------------------------------
%% INV-9：配置缺失 => fail-closed（拒绝），绝不放行
%% ------------------------------------------------------------------

config_missing_fail_closed() ->
    application:unset_env(imboy, enterprise_internal_rate_limits),
    lists:foreach(
        fun(B) ->
            ?assertEqual(
                {error, rate_not_configured},
                enterprise_internal_rate:check(B, unique_key()),
                "缺配置时必须 fail-closed 拒绝（INV-9），不得 fail-open"
            )
        end,
        ?BUCKETS
    ).

partial_config_fail_closed() ->
    %% 只配了两只桶：internal_sso 缺失也必须拒绝
    application:set_env(imboy, enterprise_internal_rate_limits, #{
        internal_read => 10, internal_write => 10
    }),
    ?assertEqual({ok, 10}, enterprise_internal_rate:limit(internal_read)),
    ?assertEqual({error, rate_not_configured}, enterprise_internal_rate:limit(internal_sso)),
    ?assertEqual(
        {error, rate_not_configured}, enterprise_internal_rate:check(internal_sso, unique_key())
    ).

non_positive_limit_fail_closed() ->
    application:set_env(imboy, enterprise_internal_rate_limits, #{
        internal_read => 0, internal_write => -1, internal_sso => <<"x">>
    }),
    lists:foreach(
        fun(B) ->
            ?assertEqual(
                {error, rate_not_configured},
                enterprise_internal_rate:check(B, unique_key()),
                "非正整数限额视同缺配置（fail-closed）"
            )
        end,
        ?BUCKETS
    ).

%% ------------------------------------------------------------------
%% 正向：配置的数值确实被执行——N 次放行、第 N+1 次拒绝
%% ------------------------------------------------------------------

enforcement_allows_n_rejects_n_plus_1() ->
    application:set_env(imboy, enterprise_internal_rate_limits, ?TEST_RATES),
    Key = unique_key(),
    InQuota = [enterprise_internal_rate:check(internal_read, Key) || _ <- lists:seq(1, 3)],
    ?assertEqual(
        [], [R || R <- InQuota, element(1, R) =/= ok], "配额内必须全部放行"
    ),
    ?assertMatch({limited, _}, enterprise_internal_rate:check(internal_read, Key)).

per_bucket_independent_counters() ->
    application:set_env(imboy, enterprise_internal_rate_limits, ?TEST_RATES),
    K = unique_key(),
    ?assertMatch({ok, _}, enterprise_internal_rate:check(internal_read, K)),
    ?assertMatch({ok, _}, enterprise_internal_rate:check(internal_write, K)),
    ?assertMatch({ok, _}, enterprise_internal_rate:check(internal_sso, K)).

%% ------------------------------------------------------------------
%% 配置守护：随发布的 sys.config.example 必须声明全部三桶
%% （配置仅用于覆写数值；漂移删除即回到 fail-closed 拒绝，这里钉住发布契约）
%% ------------------------------------------------------------------

shipped_config_declares_all_buckets() ->
    Path =
        case filelib:is_file("config/sys.config") of
            true -> "config/sys.config";
            false -> "config/sys.config.example"
        end,
    {ok, [Config]} = file:consult(Path),
    Imboy = proplists:get_value(imboy, Config, []),
    Rates = proplists:get_value(enterprise_internal_rate_limits, Imboy, undefined),
    ?assertMatch(#{}, Rates, "imboy.enterprise_internal_rate_limits 必须是 map 且存在于发布配置"),
    lists:foreach(
        fun(B) ->
            V = maps:get(B, Rates, undefined),
            ?assertMatch(
                N when is_integer(N) andalso N > 0,
                V,
                {bucket_missing_or_invalid_in_shipped_config, B}
            )
        end,
        ?BUCKETS
    ).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

unique_key() ->
    <<"epgz02_rate_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.
