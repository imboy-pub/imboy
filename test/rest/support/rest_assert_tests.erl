-module(rest_assert_tests).

%% RTF-02-A1/A2: positive and negative coverage for the assertion set, and
%% proof that failure reasons carry sanitized summaries only (no map values
%% from token-bearing payloads).

-include_lib("eunit/include/eunit.hrl").

response() ->
    #{
        status => 200,
        headers => #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        body => #{
            <<"code">> => 0,
            <<"msg">> => <<"success">>,
            <<"sv_ts">> => 1789900000,
            <<"payload">> => #{
                <<"token">> => <<"super-secret-token-value">>,
                <<"uid">> => 123
            }
        }
    }.

positive_paths_test() ->
    Resp = response(),
    ok = rest_assert:status(200, Resp),
    ok = rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Resp),
    ok = rest_assert:json_path([<<"code">>], 0, Resp),
    ok = rest_assert:json_contains(#{<<"code">> => 0, <<"msg">> => <<"success">>}, Resp),
    ok = rest_assert:json_contains(#{<<"payload">> => #{<<"uid">> => 123}}, Resp),
    ok = rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Resp),
    ok = rest_assert:predicate([<<"payload">>, <<"token">>], fun nonempty/1, Resp).

negative_paths_test() ->
    Resp = response(),
    ?assertError({rest_assertion_failed, status, _}, rest_assert:status(404, Resp)),
    ?assertError(
        {rest_assertion_failed, {header, <<"content-type">>}, _},
        rest_assert:header_contains(<<"content-type">>, <<"text/html">>, Resp)
    ),
    ?assertError(
        {rest_assertion_failed, {json_path, [<<"code">>]}, _},
        rest_assert:json_path([<<"code">>], 1, Resp)
    ),
    ?assertError(
        {rest_assertion_failed, json_contains, _},
        rest_assert:json_contains(#{<<"code">> => 1}, Resp)
    ),
    ?assertError(
        {rest_assertion_failed, {json_predicate, [<<"code">>]}, _},
        rest_assert:predicate([<<"code">>], fun is_binary/1, Resp)
    ).

missing_path_test() ->
    Resp = response(),
    ?assertError(
        {rest_assertion_failed, {json_path, [<<"nope">>]}, _},
        rest_assert:json_path([<<"nope">>], 1, Resp)
    ).

%% RTF-02 task 4: the failure reason must not carry values of nested map
%% entries (the token string never appears), only summaries.
failure_reason_carries_no_payload_values_test() ->
    Resp = response(),
    try
        rest_assert:json_contains(#{<<"payload">> => #{<<"token">> => <<"other">>}}, Resp),
        ?assert(fail_expected)
    catch
        error:Reason ->
            Formatted = unicode:characters_to_binary(io_lib:format("~p", [Reason])),
            ?assertEqual(nomatch, binary:match(Formatted, <<"super-secret-token-value">>)),
            %% the summary describes the shape, not the content
            SummaryMap = element(3, Reason),
            ?assertMatch({map_keys, _}, maps:get(actual, SummaryMap))
    end.

long_binary_is_truncated_test() ->
    Long = binary:copy(<<"a">>, 500),
    ?assertError(
        {rest_assertion_failed, status, #{expected := 404, actual := {binary_head_64, _, 500}}},
        rest_assert:status(404, #{status => Long})
    ).

nonempty(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.
