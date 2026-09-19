-module(rest_assert).

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
    erlang:error({rest_assertion_failed, Kind, #{expected => Expected, actual => Actual}}).
