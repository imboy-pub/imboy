-module(rest_evidence_tests).

%% RTF-02-A2/A3/A4: recursive case-insensitive redaction, FAIL evidence
%% written before the error propagates, and no raw_body in evidence.

-include_lib("eunit/include/eunit.hrl").

-define(CANARY_PWD, <<"canary-password-7f3a9c">>).
-define(CANARY_TOKEN, <<"canary-token-2b81de">>).

with_evidence_dir(Test) ->
    Dir = filename:join([
        "/tmp", "rest_evidence_tests", integer_to_list(erlang:unique_integer([positive]))
    ]),
    os:putenv("REST_EVIDENCE_DIR", Dir),
    try
        Test(Dir)
    after
        os:unsetenv("REST_EVIDENCE_DIR"),
        ec_file_remove(Dir)
    end.

ec_file_remove(Dir) ->
    _ = file:del_dir_r(Dir),
    ok.

meta(CaseId) ->
    #{
        case_id => CaseId,
        api => <<"unit">>,
        method => <<"POST">>,
        path => <<"/unit">>,
        expected => #{<<"code">> => 0}
    }.

response() ->
    #{
        status => 200,
        headers => #{<<"authorization">> => <<"Bearer abc">>, <<"x-trace">> => <<"t1">>},
        raw_body => <<"raw-secret-body">>,
        duration_ms => 12,
        body => #{
            <<"code">> => 1,
            <<"Password">> => ?CANARY_PWD,
            <<"nested">> => [
                #{<<"Refresh_Token">> => ?CANARY_TOKEN}
            ]
        }
    }.

fail_evidence_written_before_raise_test() ->
    with_evidence_dir(fun(Dir) ->
        Request = #{<<"pwd">> => ?CANARY_PWD},
        Raised =
            try
                rest_evidence:verify(
                    meta(<<"UNIT-001">>),
                    Request,
                    response(),
                    fun(Resp) -> rest_assert:json_contains(#{<<"code">> => 0}, Resp) end
                ),
                not_raised
            catch
                error:{rest_assertion_failed, _, _} = Raiser -> {raised, Raiser}
            end,
        %% A no-raise run must fail the test, not pass it vacuously.
        ?assertMatch({raised, _}, Raised),
        File = filename:join(Dir, "unit-001.json"),
        {ok, Bin} = file:read_file(File),
        Doc = jsone:decode(Bin, [{object_format, map}]),
        ?assertEqual(<<"FAIL">>, maps:get(<<"result">>, Doc)),
        %% A3: FAIL evidence exists although the assertion raised.
        ?assertMatch(#{<<"result">> := <<"FAIL">>}, Doc)
    end).

redaction_recursive_and_case_insensitive_test() ->
    with_evidence_dir(fun(Dir) ->
        Outcome =
            try
                rest_evidence:verify(
                    meta(<<"UNIT-002">>),
                    #{<<"pwd">> => ?CANARY_PWD},
                    response(),
                    fun(_) -> error(boom) end
                ),
                not_raised
            catch
                _:_ -> raised
            end,
        ?assertEqual(raised, Outcome),
        {ok, Bin} = file:read_file(filename:join(Dir, "unit-002.json")),
        ?assertEqual(nomatch, binary:match(Bin, ?CANARY_PWD)),
        ?assertEqual(nomatch, binary:match(Bin, ?CANARY_TOKEN)),
        ?assertEqual(nomatch, binary:match(Bin, <<"raw-secret-body">>)),
        Doc = jsone:decode(Bin, [{object_format, map}]),
        Req = maps:get(<<"request">>, Doc),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"pwd">>, Req)),
        Resp = maps:get(<<"response">>, Doc),
        ?assertEqual(
            <<"[REDACTED]">>, maps:get(<<"authorization">>, maps:get(<<"headers">>, Resp))
        ),
        Body = maps:get(<<"body">>, Resp),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"Password">>, Body)),
        %% raw_body never enters evidence at all (RTF-02 task 2)
        ?assertNot(maps:is_key(<<"raw_body">>, Resp)),
        Nested = hd(maps:get(<<"nested">>, Body)),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"Refresh_Token">>, Nested))
    end).

%% RTF-02 task 5: substring semantics — prefixed/camelCase/snake_case
%% spellings of sensitive words are redacted too.
sensitive_key_substring_spellings_test() ->
    with_evidence_dir(fun(Dir) ->
        Outcome =
            try
                rest_evidence:verify(
                    meta(<<"UNIT-006">>),
                    #{},
                    #{
                        status => 200,
                        headers => #{
                            <<"x-auth-token">> => ?CANARY_TOKEN,
                            <<"SET-COOKIE">> => ?CANARY_PWD,
                            <<"x-api-key">> => ?CANARY_PWD,
                            <<"client_secret">> => ?CANARY_TOKEN
                        },
                        raw_body => <<"raw">>,
                        duration_ms => 1,
                        body => #{<<"accessToken">> => ?CANARY_TOKEN, <<"ok">> => true}
                    },
                    fun(_) -> error(boom) end
                ),
                not_raised
            catch
                _:_ -> raised
            end,
        ?assertEqual(raised, Outcome),
        {ok, Bin} = file:read_file(filename:join(Dir, "unit-006.json")),
        ?assertEqual(nomatch, binary:match(Bin, ?CANARY_TOKEN)),
        ?assertEqual(nomatch, binary:match(Bin, ?CANARY_PWD)),
        Doc = jsone:decode(Bin, [{object_format, map}]),
        Headers = maps:get(<<"headers">>, maps:get(<<"response">>, Doc)),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"x-auth-token">>, Headers)),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"SET-COOKIE">>, Headers)),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"x-api-key">>, Headers)),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"client_secret">>, Headers)),
        Body = maps:get(<<"body">>, maps:get(<<"response">>, Doc)),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"accessToken">>, Body)),
        ?assertEqual(true, maps:get(<<"ok">>, Body))
    end).

pass_evidence_shape_test() ->
    with_evidence_dir(fun(Dir) ->
        ok = rest_evidence:verify(
            meta(<<"UNIT-003">>),
            #{<<"account">> => <<"u">>},
            #{status => 200, headers => #{}, body => #{}, duration_ms => 3},
            fun(_) -> ok end
        ),
        {ok, Bin} = file:read_file(filename:join(Dir, "unit-003.json")),
        Doc = jsone:decode(Bin, [{object_format, map}]),
        ?assertEqual(<<"PASS">>, maps:get(<<"result">>, Doc)),
        ?assertEqual(<<"UNIT-003">>, maps:get(<<"case_id">>, Doc)),
        ?assert(erlang:is_map_key(<<"timestamp">>, Doc))
    end).

%% RTF-02 task 5: the expected field goes through the same redaction entry —
%% a suite that puts a token-shaped expectation under a sensitive key must
%% not plant it verbatim in evidence.
expected_field_redacted_test() ->
    with_evidence_dir(fun(Dir) ->
        ExpectedMeta = (meta(<<"UNIT-004">>))#{
            expected => #{
                <<"http_status">> => 200,
                <<"token">> => ?CANARY_TOKEN
            }
        },
        ok = rest_evidence:verify(
            ExpectedMeta,
            #{},
            #{status => 200, headers => #{}, body => #{}, duration_ms => 1},
            fun(_) -> ok end
        ),
        {ok, Bin} = file:read_file(filename:join(Dir, "unit-004.json")),
        ?assertEqual(nomatch, binary:match(Bin, ?CANARY_TOKEN)),
        Doc = jsone:decode(Bin, [{object_format, map}]),
        Expected = maps:get(<<"expected">>, Doc),
        ?assertEqual(<<"[REDACTED]">>, maps:get(<<"token">>, Expected)),
        ?assertEqual(200, maps:get(<<"http_status">>, Expected))
    end).

%% RTF-02 task 5: non-assertion errors (e.g. a badmatch carrying a whole
%% response) are capped so unbounded payloads cannot enter evidence.
failure_text_capped_test() ->
    with_evidence_dir(fun(Dir) ->
        Huge = binary:copy(<<"x">>, 100000),
        Outcome =
            try
                rest_evidence:verify(
                    meta(<<"UNIT-005">>),
                    #{},
                    #{status => 200, headers => #{}, body => #{}, duration_ms => 1},
                    fun(_) -> erlang:error({boom, Huge}) end
                ),
                not_raised
            catch
                _:_ -> raised
            end,
        ?assertEqual(raised, Outcome),
        {ok, Bin} = file:read_file(filename:join(Dir, "unit-005.json")),
        Doc = jsone:decode(Bin, [{object_format, map}]),
        Failure = maps:get(<<"failure">>, maps:get(<<"actual">>, Doc)),
        ?assert(byte_size(Failure) < 100000),
        ?assertMatch(
            <<" ...[truncated by rest_evidence]">>,
            binary:part(
                Failure,
                byte_size(Failure) - byte_size(<<" ...[truncated by rest_evidence]">>),
                byte_size(<<" ...[truncated by rest_evidence]">>)
            )
        )
    end).
