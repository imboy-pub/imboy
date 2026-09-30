%%% Seat configuration for trusted adapters holding a transaction connection.
%%% Authentication/Grant/idempotency belong to the adapter; no credentials issued.
-module(cs_seat_governance_app).
-export([govern/2]).

govern(OrgId, #{connection := Conn, operation := Operation} = Params) when is_pid(Conn) ->
    case id(OrgId) andalso valid(Operation, Params) of
        true ->
            cs_app_support:with_store(Params, fun(Store) ->
                Store:govern_seat(Conn, OrgId, Operation, Params)
            end);
        false ->
            {error, {invalid_argument, govern_seat}}
    end;
govern(_, _) ->
    {error, {invalid_argument, govern_seat}}.

%% Internal lookahead may fetch 101 rows for a public page capped at 100.
valid(list, P) ->
    After = maps:get(after_id, P, 0),
    Limit = maps:get(limit, P, 50),
    is_integer(After) andalso After >= 0 andalso After =< 9223372036854775807 andalso
        is_integer(Limit) andalso Limit >= 1 andalso Limit =< 101;
valid(detail, P) ->
    id(maps:get(business_identity_id, P, undefined));
valid(create, P) ->
    mutation(P) andalso is_boolean(maps:get(enabled, P, true)) andalso
        positive_int32(maps:get(max_concurrent, P, 1));
valid(update, P) ->
    mutation(P) andalso positive_int32(maps:get(expected_version, P, undefined)) andalso
        (maps:is_key(enabled, P) orelse maps:is_key(max_concurrent, P)) andalso
        optional(enabled, P, fun is_boolean/1) andalso
        optional(max_concurrent, P, fun positive_int32/1);
valid(_, _) ->
    false.

mutation(P) ->
    id(maps:get(business_identity_id, P, undefined)) andalso
        id(maps:get(workspace_id, P, undefined)) andalso
        id(maps:get(application_id, P, undefined)) andalso
        cs_app_support:pos_int(maps:get(at, P, undefined)) andalso
        cs_app_support:non_empty_binary(maps:get(correlation_id, P, undefined)).

optional(Key, P, Check) ->
    case maps:find(Key, P) of
        {ok, Value} -> Check(Value);
        error -> true
    end.
id(Value) -> is_integer(Value) andalso Value > 0 andalso Value =< 9223372036854775807.
positive_int32(Value) -> is_integer(Value) andalso Value > 0 andalso Value =< 2147483647.
