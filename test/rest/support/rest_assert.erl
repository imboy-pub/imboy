-module(rest_assert).

%% Minimal black-box assertions for REST Common Test suites.
%% Failure exceptions carry only the assertion kind, the expectation and a
%% sanitized summary of the actual value — never a full response body that
%% could embed tokens (RTF-02 task 4).

-export([status/2, header_contains/3, json_path/3, json_contains/2, predicate/3]).

-spec status(non_neg_integer(), map()) -> ok.
status(Expected, #{status := Expected}) ->
    ok;
status(Expected, Response) ->
    fail(status, Expected, maps:get(status, Response, missing)).

-spec header_contains(binary(), binary(), map()) -> ok.
header_contains(Name, ExpectedPart, #{headers := Headers}) ->
    case maps:get(Name, Headers, missing) of
        Value when is_binary(Value) ->
            case binary:match(Value, ExpectedPart) of
                nomatch -> fail({header, Name}, ExpectedPart, Value);
                _ -> ok
            end;
        Value ->
            fail({header, Name}, ExpectedPart, Value)
    end.

-spec json_path([term()], term(), map()) -> ok.
json_path(Path, Expected, #{body := Body}) ->
    Actual = get_path(Path, Body),
    case Actual =:= Expected of
        true -> ok;
        false -> fail({json_path, Path}, Expected, Actual)
    end.

-spec json_contains(map(), map()) -> ok.
json_contains(Expected, #{body := Body}) ->
    case contains(Expected, Body) of
        true -> ok;
        false -> fail(json_contains, Expected, Body)
    end.

-spec predicate([term()], fun((term()) -> boolean()), map()) -> ok.
predicate(Path, Pred, #{body := Body}) ->
    Actual = get_path(Path, Body),
    case Pred(Actual) of
        true -> ok;
        false -> fail({json_predicate, Path}, predicate, Actual)
    end.

%% ===================================================================
%% Internal
%% ===================================================================

get_path([], Value) ->
    Value;
get_path([Key | Rest], Map) when is_map(Map) ->
    get_path(Rest, maps:get(Key, Map, missing));
get_path(_Path, _Value) ->
    missing.

contains(Expected, Actual) when is_map(Expected), is_map(Actual) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            Acc andalso maps:is_key(Key, Actual) andalso contains(Value, maps:get(Key, Actual))
        end,
        true,
        Expected
    );
contains(Expected, Actual) ->
    Expected =:= Actual.

fail(Kind, Expected, Actual) ->
    erlang:error(
        {rest_assertion_failed, Kind, #{
            expected => summarize(Expected),
            actual => summarize(Actual)
        }}
    ).

%% Sanitized value summaries: maps contribute their key names only, lists
%% their length. Binaries NEVER contribute content: header values and
%% expected fragments are exactly where bearer tokens/cookies live, and the
%% failure reason is later written verbatim into the evidence `failure`
%% field, where key-based redaction cannot help (RTF-02 task 5). Scalars
%% (atoms/integers/floats) pass through: they cannot carry credential
%% payloads. Debuggability is preserved through the evidence response field,
%% which keeps non-sensitive values after key redaction.
summarize(Value) when is_map(Value) ->
    {map_keys, lists:sort(maps:keys(Value))};
summarize(Value) when is_list(Value) ->
    {list_length, length(Value)};
summarize(Value) when is_binary(Value) ->
    {binary, byte_size(Value)};
summarize(Value) when is_atom(Value); is_integer(Value); is_float(Value) ->
    Value;
summarize(Value) when is_tuple(Value) ->
    {tuple_size_summary, tuple_size(Value)};
summarize(Value) ->
    {non_printable_summary, byte_size(term_to_binary(Value))}.
