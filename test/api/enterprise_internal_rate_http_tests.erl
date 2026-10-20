%% enterprise_internal_rate_http_tests
%% Q08 / AC19-4.3 — 429 rate_limited + Retry-After 的真 HTTP 正反例。
%%
%% 背景（Q08 调查 §1 AC19-4.3 缺口）：限流此前只有单元层覆盖
%% （enterprise_internal_rate_tests 的 fail-closed 语义 + enterprise_internal_pg_tests
%% 的 take_retry_after 一次性取值），没有任何**真 HTTP 触达 429** 的用例。
%% 本套件补齐：经 intbe02_http_support 的真 Cowboy 监听 + 一次性 marker PG，
%% 把 enterprise_internal_rate_limits 调成 1/min 后对同一 application
%% 连发真 HTTP 请求，断言：
%%   1) 第 1 次放行（200）；
%%   2) 第 2 次 429 + 信封 error.code = rate_limited；
%%   3) Retry-After 头存在、为整数秒且 >= 1（rate_gate 的 ceil+下限 1s 语义）；
%%   4) 限流窗口内第 3 次仍 429（持续拒绝，非一次性）；
%%   5) 另一 application 同刻不受牵连（配额按 application_id 计数，非全局/按 IP）。
%%
%% 配置恢复：用例结束恢复 setup_all 注入的原始限流值（10000/min），避免
%% 同 VM 后续套件被 1/min 桶值毒化（enterprise_internal_rate:ensure_setup
%% 仅在数值变化时重建 throttle scope，恢复即重建）。
-module(enterprise_internal_rate_http_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SAVED_RATES, #{internal_read => 10000, internal_write => 10000, internal_sso => 10000}).

rate_http_test_() ->
    {timeout, 90,
        {setup, fun intbe02_http_support:setup_all/0, fun teardown_marker/1, fun(
            S
        ) ->
            {timeout, 90, fun() -> run(S) end}
        end}}.

run(#{conn := C} = S) ->
    %% 独立 application + 全 scope grant + 凭证（995 段夹具外新造，计数零污染）。
    {ok, App} = enterprise_application_repo:create_tx(
        C, 995101, <<"q08-rate-http">>, <<"Q08 rate http probe">>, 995014
    ),
    AppId = maps:get(<<"id">>, App),
    {ok, #{credential := Credential}} = enterprise_internal_ops:issue_credential_tx(
        C, 995101, AppId, <<"q08_rate_http_secret_0123456789">>, undefined
    ),
    try
        application:set_env(imboy, enterprise_internal_rate_limits, #{
            internal_read => 1, internal_write => 1, internal_sso => 1
        }),
        first_request_passes(S, Credential),
        second_request_429_with_retry_after(S, Credential),
        third_request_still_limited(S, Credential),
        other_application_not_affected(S),
        io:format("Q08_RATE_HTTP_RESULT=ok~n")
    after
        application:set_env(imboy, enterprise_internal_rate_limits, ?SAVED_RATES)
    end.

first_request_passes(S, Credential) ->
    R = get_application(S, Credential),
    ?assertEqual(200, maps:get(status, R), first_request),
    Json = jsone:decode(maps:get(body, R)),
    ?assertEqual(<<"q08-rate-http">>, maps:get(<<"application_key">>, Json)).

second_request_429_with_retry_after(S, Credential) ->
    R = get_application(S, Credential),
    ?assertEqual(429, maps:get(status, R), second_request),
    Json = jsone:decode(maps:get(body, R)),
    ?assertEqual(
        <<"rate_limited">>, maps:get(<<"code">>, maps:get(<<"error">>, Json)), second_request
    ),
    RetryAfter = intbe02_http_support:json_header_val(R, <<"retry-after">>),
    ?assert(is_binary(RetryAfter), "429 必须携带 Retry-After 头"),
    Seconds = binary_to_integer(RetryAfter),
    ?assert(is_integer(Seconds) andalso Seconds >= 1, "Retry-After 必须是 >=1 的整秒").

third_request_still_limited(S, Credential) ->
    R = get_application(S, Credential),
    ?assertEqual(429, maps:get(status, R), third_request),
    ?assert(is_binary(intbe02_http_support:json_header_val(R, <<"retry-after">>))).

%% 配额 key = application_id（rate_gate）——App A 的 429 不得波及无消费记录的
%% 其他 application；同一 VM 内 App A 本轮只发过 GET（internal_read）。
other_application_not_affected(S) ->
    R = get_application(S, maps:get(cred_ro, S)),
    ?assertEqual(200, maps:get(status, R), other_application),
    Json = jsone:decode(maps:get(body, R)),
    ?assertEqual(<<"intbe02-oa-ro">>, maps:get(<<"application_key">>, Json)).

get_application(S, Credential) ->
    intbe02_http_support:http(
        maps:get(port, S),
        <<"GET">>,
        <<"/api/internal/v1/application">>,
        #{},
        intbe02_http_support:auth(Credential)
    ).

%% This fixture owns its disposable marker; release it even if HTTP cleanup fails.
teardown_marker(State) ->
    try
        intbe02_http_support:teardown_all(State)
    after
        inttest_marker_db:release(State)
    end.
