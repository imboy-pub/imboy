%%% Internal adapter; all writes share Grant, seat, audit and idempotency transaction.
-module(enterprise_cs_seat_logic).
-export([read_tx/4, write_tx/7]).
-include("generated/imboy_product_features.hrl").
-define(FAMILY, <<"customer_service_seats">>).
-define(EPOCH, <<"1970-01-01T00:00:00Z">>).

read_tx(Conn, Ctx, list, Opts) ->
    boundary(Conn, Ctx, <<"INT-33">>),
    Limit = maps:get(limit, Opts),
    Pivot =
        case
            enterprise_internal_read_page:resolve(
                maps:get(cursor, Opts, undefined), Ctx, ?FAMILY, #{}
            )
        of
            {ok, undefined} -> 0;
            {ok, {?EPOCH, Id}} when Id > 0, Id =< 9223372036854775807 -> Id;
            {error, Code} -> fail(Code);
            _ -> fail(invalid_request)
        end,
    Rows0 = invoke(Conn, Ctx, list, #{limit => Limit + 1, after_id => Pivot}),
    HasMore = length(Rows0) > Limit,
    Rows = lists:sublist(Rows0, Limit),
    Next =
        case HasMore of
            true ->
                case
                    enterprise_internal_read_page:encode(
                        Ctx,
                        ?FAMILY,
                        #{},
                        {?EPOCH, maps:get(business_identity_id, lists:last(Rows))}
                    )
                of
                    {ok, Cursor} -> Cursor;
                    {error, _} -> fail(security_gate_closed)
                end;
            false ->
                null
        end,
    usage(Conn, Ctx),
    {ok, #{
        <<"items">> => [view(R) || R <- Rows],
        <<"limit">> => Limit,
        <<"has_more">> => HasMore,
        <<"next_cursor">> => Next
    }};
read_tx(Conn, Ctx, detail, Opts) ->
    boundary(Conn, Ctx, <<"INT-34">>),
    Row = invoke(Conn, Ctx, detail, Opts),
    usage(Conn, Ctx),
    {ok, view(Row)}.

write_tx(Conn, Ctx, Operation, Key, Digest, IdentityId, Body) ->
    Route =
        case Operation of
            create -> <<"INT-35">>;
            update -> <<"INT-36">>
        end,
    boundary(Conn, Ctx, Route),
    Type =
        case Operation of
            create -> <<"customer_service_seat_create">>;
            update -> <<"customer_service_seat_update">>
        end,
    case enterprise_internal_idempotency:begin_tx(Conn, Ctx, Type, Key, Digest) of
        {ok, inserted} ->
            Params0 = params(Operation, Body),
            Params =
                case Operation of
                    create -> Params0;
                    update -> Params0#{business_identity_id => IdentityId}
                end,
            Row = invoke(Conn, Ctx, Operation, Params),
            Encoded = jsone:encode(view(Row)),
            case
                enterprise_internal_idempotency:complete_tx(
                    Conn,
                    Ctx,
                    Type,
                    Key,
                    maps:get(business_identity_id, Row),
                    200,
                    Encoded
                )
            of
                ok -> {ok, 200, Encoded, []};
                {error, _} -> fail(internal_error)
            end;
        {ok, replay, #{response_code := Status, response_body := Encoded}} when
            is_binary(Encoded)
        ->
            {ok, Status, Encoded, [enterprise_internal_idempotency:replay_header()]};
        {ok, pending} ->
            fail(idempotency_conflict);
        {error, digest_conflict} ->
            fail(idempotency_conflict);
        _ ->
            fail(internal_error)
    end.

params(Operation, Body) ->
    Keys =
        case Operation of
            create ->
                [
                    <<"workspace_id">>,
                    <<"business_identity_id">>,
                    <<"enabled">>,
                    <<"max_concurrent">>
                ];
            update ->
                [<<"workspace_id">>, <<"expected_version">>, <<"enabled">>, <<"max_concurrent">>]
        end,
    case maps:keys(Body) -- Keys of
        [] -> ok;
        _ -> fail(invalid_request)
    end,
    Names = #{
        <<"workspace_id">> => workspace_id,
        <<"business_identity_id">> => business_identity_id,
        <<"enabled">> => enabled,
        <<"max_concurrent">> => max_concurrent,
        <<"expected_version">> => expected_version
    },
    maps:from_list([{maps:get(K, Names), V} || {K, V} <- maps:to_list(Body)]).

boundary(Conn, Ctx, Route) ->
    case enterprise_internal_boundary:enforce(Conn, Ctx, Route, undefined) of
        ok -> ok;
        {error, Code} -> fail(enterprise_internal_boundary:error_code(Code))
    end.

-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).
invoke(Conn, Ctx, Operation, Params) ->
    Trusted = Params#{
        connection => Conn,
        operation => Operation,
        application_id => maps:get(application_id, Ctx),
        correlation_id => maps:get(correlation_id, Ctx),
        at => os:system_time(second)
    },
    Result =
        try
            customer_service_facade:govern_seat(maps:get(organization_id, Ctx), Trusted)
        catch
            throw:{rollback, {error, R}} -> {error, R}
        end,
    case Result of
        {ok, Row} -> Row;
        {error, R1} -> fail(code(R1))
    end.
-else.
invoke(_, _, _, _) -> fail(security_gate_closed).
-endif.

usage(Conn, Ctx) ->
    case
        enterprise_application_usage_repo:bump_tx(
            Conn,
            maps:get(organization_id, Ctx),
            maps:get(application_id, Ctx),
            <<"seat.read">>
        )
    of
        ok -> ok;
        _ -> fail(internal_error)
    end.
view(Row) -> maps:from_list([{atom_to_binary(K, utf8), V} || {K, V} <- maps:to_list(Row)]).
code({invalid_argument, _}) -> invalid_request;
code(not_found) -> resource_not_found;
code({not_found, _}) -> resource_not_found;
code(conflict) -> version_conflict;
code(seat_limit_exceeded) -> seat_limit_exceeded;
code({sql, <<"23505">>, _}) -> resource_conflict;
code(_) -> internal_error.
fail(Code) when is_atom(Code) -> fail(atom_to_binary(Code, utf8));
fail(Code) -> throw({rollback, {internal_error_code, Code}}).
