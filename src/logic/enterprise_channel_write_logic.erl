%%% Application channel governance: authorization, mutation, audit and replay are atomic.
-module(enterprise_channel_write_logic).
-export([write_tx/7]).

write_tx(Conn, Ctx, Op, Key, Digest, Id, Body) ->
    validate(Op, Body),
    Initial =
        case Op of
            create -> undefined;
            _ -> locate(Conn, Ctx, Id, false)
        end,
    Ws =
        case Op of
            create -> maps:get(<<"workspace_id">>, Body);
            _ -> maps:get(<<"workspace_id">>, Initial)
        end,
    {Route, Type} = route(Op),
    case enterprise_internal_boundary:enforce(Conn, Ctx, Route, Ws) of
        ok -> ok;
        {error, R} -> fail(enterprise_internal_boundary:error_code(R))
    end,
    case enterprise_internal_idempotency:begin_tx(Conn, Ctx, Type, Key, Digest) of
        {ok, inserted} ->
            finish_tx(Conn, Ctx, Op, Id, Ws, Body, Type, Key);
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

finish_tx(Conn, Ctx, Op, Id, Ws, Body, Type, Key) ->
    case Op of
        create ->
            case
                channel_repo:internal_creator_org_tx(
                    Conn,
                    maps:get(organization_id, Ctx),
                    maps:get(<<"creator_user_id">>, Body)
                )
            of
                ok -> ok;
                {error, forbidden} -> fail(organization_boundary_violation);
                _ -> fail(internal_error)
            end;
        _ ->
            ok
    end,
    Writable = workspace_repo:internal_lock_tx(Conn, maps:get(organization_id, Ctx), Ws),
    case Writable of
        {ok, #{<<"status">> := <<"active">>}} -> ok;
        {ok, _} -> fail(resource_conflict);
        {error, not_found} -> fail(resource_not_found);
        _ -> fail(internal_error)
    end,
    ChannelId = mutate(Conn, Ctx, Op, Id, Ws, Body),
    audit(Conn, Ctx, ChannelId, Op),
    Encoded = jsone:encode(view(locate(Conn, Ctx, ChannelId, true))),
    case
        enterprise_internal_idempotency:complete_tx(
            Conn, Ctx, Type, Key, ChannelId, 200, Encoded
        )
    of
        ok -> {ok, 200, Encoded, []};
        _ -> fail(internal_error)
    end.

route(create) -> {<<"INT-40">>, <<"channel_create">>};
route(update) -> {<<"INT-41">>, <<"channel_update">>};
route(archive) -> {<<"INT-42">>, <<"channel_archive">>}.

mutate(Conn, Ctx, create, _Id, Ws, Body) -> create_tx(Conn, Ctx, Ws, Body);
mutate(Conn, Ctx, Op, Id, Ws, Body) -> change_tx(Conn, Ctx, Op, Id, Ws, Body).

create_tx(Conn, Ctx, Ws, Body) ->
    Uid = maps:get(<<"creator_user_id">>, Body),
    case channel_repo:internal_creator_tx(Conn, maps:get(organization_id, Ctx), Ws, Uid) of
        ok -> ok;
        {error, forbidden} -> fail(organization_boundary_violation);
        _ -> fail(internal_error)
    end,
    Options = #{
        scope => <<"workspace">>,
        workspace_id => Ws,
        visibility => 1,
        access_type => 0,
        join_policy => 1,
        description => maps:get(<<"description">>, Body, <<>>),
        avatar => maps:get(<<"avatar">>, Body, <<>>)
    },
    try channel_ds:create_channel_tx(Conn, Uid, maps:get(<<"name">>, Body), Options) of
        {ok, Created} -> Created
    catch
        throw:{abort_tx, channel_creation_limit} -> fail(resource_conflict);
        throw:{abort_tx, {980, _}} -> fail(resource_conflict);
        throw:{abort_tx, _} -> fail(internal_error)
    end.

change_tx(Conn, Ctx, Op, Id, Ws, Body) ->
    Row = locate(Conn, Ctx, Id, true),
    case {maps:get(<<"status">>, Row), maps:get(<<"workspace_id">>, Row)} of
        {1, Ws} -> ok;
        _ -> fail(resource_not_found)
    end,
    case maps:get(<<"version">>, Row) =:= maps:get(<<"expected_version">>, Body) of
        true -> ok;
        false -> fail(version_conflict)
    end,
    Result =
        case Op of
            update ->
                channel_repo:update_tx(
                    Conn,
                    Id,
                    (maps:with([<<"name">>, <<"description">>, <<"avatar">>], Body))#{
                        updated_at => elib_dt:now()
                    }
                );
            archive ->
                channel_repo:archive_tx(Conn, Id, elib_dt:now())
        end,
    case Result of
        {ok, 1} -> Id;
        _ -> fail(internal_error)
    end.

locate(Conn, Ctx, Id, Lock) ->
    case channel_repo:internal_write_find_tx(Conn, maps:get(organization_id, Ctx), Id, Lock) of
        {ok, Row} -> Row;
        {error, not_found} -> fail(resource_not_found);
        _ -> fail(internal_error)
    end.

audit(Conn, Ctx, Id, Op) ->
    case
        enterprise_audit_event_repo:append_tx(Conn, maps:get(organization_id, Ctx), #{
            resource_type => <<"channel">>,
            resource_id => Id,
            action => <<"channel.", (atom_to_binary(Op, utf8))/binary>>,
            actor_role => <<"application">>,
            detail => #{
                application_id => maps:get(application_id, Ctx),
                correlation_id => maps:get(correlation_id, Ctx)
            }
        })
    of
        {ok, _} -> ok;
        _ -> fail(internal_error)
    end.

view(Row) ->
    (maps:with(
        [
            <<"workspace_id">>,
            <<"name">>,
            <<"description">>,
            <<"avatar">>,
            <<"creator_uid">>,
            <<"visibility">>,
            <<"access_type">>,
            <<"join_policy">>,
            <<"status">>,
            <<"subscriber_count">>,
            <<"version">>,
            <<"created_at">>,
            <<"updated_at">>
        ],
        Row
    ))#{
        <<"channel_id">> => maps:get(<<"id">>, Row)
    }.

validate(Op, Body) when is_map(Body) ->
    Allowed =
        case Op of
            create ->
                [
                    <<"workspace_id">>,
                    <<"creator_user_id">>,
                    <<"name">>,
                    <<"description">>,
                    <<"avatar">>
                ];
            update ->
                [<<"expected_version">>, <<"name">>, <<"description">>, <<"avatar">>];
            archive ->
                [<<"expected_version">>]
        end,
    case maps:keys(Body) -- Allowed of
        [] -> ok;
        _ -> fail(invalid_request)
    end,
    case Op of
        create ->
            id(maps:get(<<"workspace_id">>, Body, undefined)),
            id(maps:get(<<"creator_user_id">>, Body, undefined)),
            text(maps:get(<<"name">>, Body, undefined), 200, true);
        _ ->
            id(maps:get(<<"expected_version">>, Body, undefined))
    end,
    case Op =:= update andalso map_size(Body) < 2 of
        true -> fail(invalid_request);
        false -> ok
    end,
    maps:foreach(
        fun
            (<<"name">>, V) -> text(V, 200, true);
            (<<"description">>, V) -> text(V, 2000, false);
            (<<"avatar">>, V) -> text(V, 320, false);
            (_, _) -> ok
        end,
        Body
    );
validate(_, _) ->
    fail(invalid_request).

id(I) when is_integer(I), I > 0, I =< 9223372036854775807 -> ok;
id(_) -> fail(invalid_request).
text(V, Limit, Required) when is_binary(V), byte_size(V) =< Limit * 4 ->
    case unicode:characters_to_list(V) of
        L when is_list(L), length(L) =< Limit ->
            case not lists:member(0, L) andalso (not Required orelse string:trim(L) =/= []) of
                true -> ok;
                false -> fail(invalid_request)
            end;
        _ ->
            fail(invalid_request)
    end;
text(_, _, _) ->
    fail(invalid_request).
fail(C) when is_atom(C) -> fail(atom_to_binary(C, utf8));
fail(C) -> throw({rollback, {internal_error_code, C}}).
