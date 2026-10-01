%%% Application writes: Grant, resource, audit and idempotency share one transaction.
-module(enterprise_workspace_write_logic).
-export([write_tx/7]).

write_tx(Conn, Ctx, Op, Key, Digest, WsId, Body) ->
    validate(Op, Body),
    {Route, Type} = route(Op),
    Row =
        case Op of
            create -> undefined;
            _ -> lock(Conn, Ctx, WsId)
        end,
    case
        enterprise_internal_boundary:enforce(
            Conn,
            Ctx,
            Route,
            case Op of
                create -> undefined;
                _ -> WsId
            end
        )
    of
        ok -> ok;
        {error, R} -> fail(enterprise_internal_boundary:error_code(R))
    end,
    case enterprise_internal_idempotency:begin_tx(Conn, Ctx, Type, Key, Digest) of
        {ok, inserted} ->
            Id = mutate(Conn, Ctx, Op, Row, Body, Key),
            Current = lock(Conn, Ctx, Id),
            audit(Conn, Ctx, Id, Op),
            Encoded = jsone:encode(view(Current)),
            case
                enterprise_internal_idempotency:complete_tx(Conn, Ctx, Type, Key, Id, 200, Encoded)
            of
                ok -> {ok, 200, Encoded, []};
                _ -> fail(internal_error)
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

route(create) -> {<<"INT-37">>, <<"workspace_create">>};
route(update) -> {<<"INT-38">>, <<"workspace_update">>};
route(archive) -> {<<"INT-39">>, <<"workspace_archive">>}.

mutate(Conn, Ctx, create, _Row, Body, Digest) ->
    create_tx(Conn, Ctx, Body, Digest);
mutate(Conn, Ctx, Op, Row, Body, _Digest) ->
    case {maps:get(<<"status">>, Row), maps:get(<<"version">>, Row)} of
        {<<"active">>, V} ->
            case V =:= maps:get(<<"expected_version">>, Body) of
                true -> ok;
                false -> fail(version_conflict)
            end;
        _ ->
            fail(resource_not_found)
    end,
    Id = maps:get(<<"id">>, Row),
    case Op of
        update -> update_tx(Conn, Id, Row, Body);
        archive -> archive_tx(Conn, Ctx, Id, Body)
    end,
    Id.

create_tx(Conn, Ctx, Body, Key) ->
    Owner = maps:get(<<"owner_user_id">>, Body),
    App = integer_to_binary(maps:get(application_id, Ctx)),
    Token = binary:encode_hex(crypto:hash(sha256, Key), lowercase),
    Request = <<"internal-", App/binary, "-", Token/binary>>,
    Result =
        try
            workspace_ds:create_template_tx(
                Conn,
                Owner,
                maps:get(organization_id, Ctx),
                maps:get(<<"name">>, Body),
                Request
            )
        catch
            throw:{abort_tx, {idempotent_hit, Existing}} -> Existing;
            throw:{abort_tx, R} -> fail(code(R))
        end,
    maps:get(workspace_id, Result).

update_tx(Conn, Id, Row, Body) ->
    Branding = maps:merge(
        branding(maps:get(<<"branding">>, Row)), maps:get(<<"branding">>, Body, #{})
    ),
    case
        workspace_repo:update_profile_tx(
            Conn,
            Id,
            maps:get(<<"name">>, Body, maps:get(<<"name">>, Row)),
            maps:get(<<"logo">>, Body, maps:get(<<"logo">>, Row)),
            Branding
        )
    of
        ok -> ok;
        {error, R} -> fail(code(R))
    end.

archive_tx(Conn, Ctx, Id, Body) ->
    Replacement = maps:get(<<"replacement_workspace_id">>, Body, undefined),
    case Replacement of
        undefined ->
            ok;
        _ ->
            lock(Conn, Ctx, Replacement),
            case enterprise_internal_boundary:enforce(Conn, Ctx, <<"INT-38">>, Replacement) of
                ok -> ok;
                {error, E} -> fail(enterprise_internal_boundary:error_code(E))
            end
    end,
    try
        workspace_ds:archive_tx(Conn, Id, null, Replacement)
    catch
        throw:{abort_tx, R} -> fail(code(R))
    end.

lock(Conn, Ctx, Id) ->
    case workspace_repo:internal_lock_tx(Conn, maps:get(organization_id, Ctx), Id) of
        {ok, Row} -> Row;
        {error, not_found} -> fail(resource_not_found);
        {error, _} -> fail(internal_error)
    end.

audit(Conn, Ctx, Id, Op) ->
    case
        enterprise_audit_event_repo:append_tx(Conn, maps:get(organization_id, Ctx), #{
            resource_type => <<"workspace">>,
            resource_id => Id,
            action => <<"workspace.", (atom_to_binary(Op, utf8))/binary>>,
            actor_role => <<"application">>,
            detail => #{
                application_id => maps:get(application_id, Ctx),
                correlation_id => maps:get(correlation_id, Ctx)
            }
        })
    of
        {ok, _} -> ok;
        {error, _} -> fail(internal_error)
    end.

view(Row) ->
    maps:merge(
        maps:with(
            [
                <<"name">>,
                <<"logo">>,
                <<"owner_id">>,
                <<"status">>,
                <<"version">>,
                <<"created_at">>,
                <<"updated_at">>
            ],
            Row
        ),
        #{
            <<"workspace_id">> => maps:get(<<"id">>, Row),
            <<"branding">> => workspace_ds:branding_public_view(maps:get(<<"branding">>, Row))
        }
    ).
branding(B) when is_map(B) -> B;
branding(B) when is_binary(B) -> jsone:decode(B).

validate(create, Body) ->
    keys(Body, [<<"owner_user_id">>, <<"name">>]),
    id(maps:get(<<"owner_user_id">>, Body, undefined)),
    name(maps:get(<<"name">>, Body, undefined));
validate(Op, Body) ->
    Allowed =
        case Op of
            update -> [<<"expected_version">>, <<"name">>, <<"logo">>, <<"branding">>];
            archive -> [<<"expected_version">>, <<"replacement_workspace_id">>]
        end,
    keys(Body, Allowed),
    id(maps:get(<<"expected_version">>, Body, undefined)),
    case Op of
        update ->
            case maps:size(Body) > 1 of
                true -> ok;
                false -> fail(invalid_request)
            end,
            validate_fields(Body);
        archive ->
            case maps:find(<<"replacement_workspace_id">>, Body) of
                {ok, V} -> id(V);
                error -> ok
            end
    end.
keys(Body, Allowed) when is_map(Body) ->
    case maps:keys(Body) -- Allowed of
        [] -> ok;
        _ -> fail(invalid_request)
    end;
keys(_, _) ->
    fail(invalid_request).
id(I) when is_integer(I), I > 0, I =< 9223372036854775807 -> ok;
id(_) -> fail(invalid_request).
name(N) when is_binary(N), byte_size(N) > 0, byte_size(N) =< 800 ->
    case unicode:characters_to_list(N) of
        L when is_list(L), length(L) =< 200 ->
            case string:trim(L) =/= [] andalso not lists:member(0, L) of
                true -> ok;
                false -> fail(invalid_request)
            end;
        _ ->
            fail(invalid_request)
    end;
name(_) ->
    fail(invalid_request).
validate_fields(Body) ->
    maps:foreach(
        fun
            (<<"name">>, V) ->
                name(V);
            (<<"logo">>, V) when is_binary(V), byte_size(V) =< 2048 -> ok;
            (<<"branding">>, V) when is_map(V), map_size(V) > 0 ->
                keys(V, [<<"name">>, <<"logo">>, <<"primaryColor">>]),
                maps:foreach(
                    fun
                        (<<"name">>, N) ->
                            name(N);
                        (<<"logo">>, L) when is_binary(L), byte_size(L) =< 2048 -> ok;
                        (<<"primaryColor">>, C) when is_binary(C), byte_size(C) =:= 7 ->
                            case re:run(C, <<"^#[0-9a-fA-F]{6}$">>, [{capture, none}]) of
                                match -> ok;
                                _ -> fail(invalid_request)
                            end;
                        (_, _) ->
                            fail(invalid_request)
                    end,
                    V
                );
            (<<"expected_version">>, _) ->
                ok;
            (_, _) ->
                fail(invalid_request)
        end,
        Body
    ).
code(organization_create_forbidden) -> organization_boundary_violation;
code(owner_workspace_limit) -> resource_conflict;
code({default_workspace_handover_required, _}) -> resource_conflict;
code({default_workspace_handover_invalid, _}) -> resource_conflict;
code(already_archived) -> resource_not_found;
code(_) -> internal_error.
fail(C) when is_atom(C) -> fail(atom_to_binary(C, utf8));
fail(C) -> throw({rollback, {internal_error_code, C}}).
