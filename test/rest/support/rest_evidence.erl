-module(rest_evidence).

-export([verify/4]).

-spec verify(map(), term(), map(), fun((map()) -> term())) -> term().
verify(Meta, Request, Response, AssertFun) ->
    try AssertFun(Response) of
        Result ->
            ok = write(Meta, Request, Response, <<"PASS">>, null),
            Result
    catch
        Class:Reason:Stacktrace ->
            Failure = unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])),
            ok = write(Meta, Request, Response, <<"FAIL">>, Failure),
            erlang:raise(Class, Reason, Stacktrace)
    end.

write(Meta, Request, Response, Result, Failure) ->
    Dir = evidence_dir(),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    CaseId = maps:get(case_id, Meta),
    Evidence = #{
        <<"case_id">> => CaseId,
        <<"api">> => maps:get(api, Meta),
        <<"method">> => maps:get(method, Meta),
        <<"path">> => maps:get(path, Meta),
        <<"request">> => redact(Request),
        <<"response">> => safe_response(Response),
        <<"expected">> => maps:get(expected, Meta),
        <<"actual">> => #{
            <<"http_status">> => maps:get(status, Response, null),
            <<"result">> => Result,
            <<"failure">> => Failure
        },
        <<"duration_ms">> => maps:get(duration_ms, Response, null),
        <<"commit_sha">> => env("REST_COMMIT_SHA", <<"unknown">>),
        <<"environment">> => #{
            <<"imboy_env">> => env("IMBOYENV", <<"test">>),
            <<"database">> => env("IMBOY_PG_DATABASE", <<"unknown">>),
            <<"otp_release">> => list_to_binary(erlang:system_info(otp_release))
        },
        <<"timestamp">> => list_to_binary(
            calendar:system_time_to_rfc3339(erlang:system_time(second), [{unit, second}])
        ),
        <<"result">> => Result
    },
    Filename = binary_to_list(string:lowercase(CaseId)) ++ ".json",
    file:write_file(filename:join(Dir, Filename), jsone:encode(Evidence, [native_utf8])).

safe_response(Response) ->
    redact(maps:with([status, headers, body, duration_ms], Response)).

redact(Map) when is_map(Map) ->
    maps:map(
        fun(Key, Value) ->
            case sensitive_key(Key) of
                true -> <<"[REDACTED]">>;
                false -> redact(Value)
            end
        end,
        Map
    );
redact(List) when is_list(List) ->
    [redact(Value) || Value <- List];
redact(Value) ->
    Value.

sensitive_key(Key) when is_atom(Key) ->
    sensitive_key(atom_to_binary(Key, utf8));
sensitive_key(Key) when is_binary(Key) ->
    lists:member(string:lowercase(Key), [
        <<"authorization">>,
        <<"cookie">>,
        <<"pwd">>,
        <<"password">>,
        <<"plain_password">>,
        <<"token">>,
        <<"refreshtoken">>,
        <<"refresh_token">>
    ]);
sensitive_key(_) ->
    false.

evidence_dir() ->
    binary_to_list(env("REST_EVIDENCE_DIR", <<".reports/rest/manual/evidence">>)).

env(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Value -> unicode:characters_to_binary(Value)
    end.
