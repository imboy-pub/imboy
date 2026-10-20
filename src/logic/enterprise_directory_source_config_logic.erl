-module(enterprise_directory_source_config_logic).
-moduledoc "Trusted instance Admin configuration. Handler must verify the Admin session and enterprise_business:write ACL before passing its server-derived adm_user_id. Machine credential requests must never invoke this surface. Public wrappers use elib_pg:with_tx; configure_source_tx callers must rollback abort_tx. Connection input uses the existing Admin application_key, name and redirect_uris atom keys and validation limits.".
-export([configure_source/4, configure_source_tx/5, create_connection/2]).

configure_source(Actor, AppId, ExternalId, Spec) ->
    elib_pg:with_tx(fun(Conn) -> configure_source_tx(Conn, Actor, AppId, ExternalId, Spec) end).

create_connection(Input, #{adm_user_id := AdminId}) when is_integer(AdminId), AdminId > 0 ->
    case connection_input(Input) of
        {ok, Key, Name, Uris} ->
            elib_pg:with_tx(fun(Conn) ->
                _ = one(
                    elib_pg:query(
                        Conn,
                        <<"SELECT id FROM adm_user WHERE id=$1 AND status=1 FOR SHARE">>,
                        [AdminId]
                    ),
                    invalid_admin
                ),
                Row = must(
                    enterprise_application_repo:create_tx(
                        Conn,
                        null,
                        Key,
                        Name,
                        null,
                        Uris
                    )
                ),
                _ = must(
                    enterprise_audit_event_repo:append_tx(
                        Conn,
                        null,
                        #{
                            resource_type => <<"enterprise_application">>,
                            resource_id => maps:get(<<"id">>, Row),
                            action => <<"directory.connection_created">>,
                            actor_role => <<"platform_admin">>,
                            detail => #{<<"adm_user_id">> => AdminId, <<"application_key">> => Key}
                        }
                    )
                ),
                {ok, Row}
            end);
        {error, _} = Error ->
            Error
    end;
create_connection(_, _) ->
    {error, invalid_admin}.

connection_input(Input) when is_map(Input) ->
    Key = maps:get(application_key, Input, <<>>),
    Name = maps:get(name, Input, <<>>),
    Uris = maps:get(redirect_uris, Input, []),
    case maps:keys(Input) -- [application_key, name, redirect_uris] of
        [] -> validate_connection(Key, Name, Uris);
        _ -> {error, invalid_directory_connection}
    end;
connection_input(_) ->
    {error, invalid_directory_connection}.

validate_connection(Key, Name, Uris) ->
    ValidKey =
        is_binary(Key) andalso byte_size(Key) >= 8 andalso byte_size(Key) =< 128 andalso
            lists:all(fun(Char) -> Char >= 32 andalso Char =< 126 end, binary_to_list(Key)),
    ValidName =
        is_binary(Name) andalso byte_size(Name) >= 1 andalso byte_size(Name) =< 128 andalso
            string:trim(Name) =/= <<>>,
    ValidUris =
        is_list(Uris) andalso length(Uris) =< 20 andalso
            length(Uris) =:= length(lists:usort(Uris)) andalso lists:all(fun valid_uri/1, Uris),
    case {ValidKey, ValidName, ValidUris} of
        {false, _, _} -> {error, invalid_application_key};
        {_, false, _} -> {error, invalid_name};
        {_, _, false} -> {error, invalid_redirect_uris};
        _ -> {ok, Key, Name, Uris}
    end.

valid_uri(Uri) when is_binary(Uri), byte_size(Uri) > 8, byte_size(Uri) =< 2048 ->
    binary:part(Uri, 0, 8) =:= <<"https://">> andalso binary:match(Uri, <<"#">>) =:= nomatch andalso
        binary:at(Uri, 8) =/= $/ andalso binary:at(Uri, 8) =/= $?;
valid_uri(_) ->
    false.

configure_source_tx(Conn, #{adm_user_id := AdminId}, AppId, ExternalId, Spec) when
    is_integer(AdminId),
    AdminId > 0,
    is_integer(AppId),
    AppId > 0,
    is_binary(ExternalId),
    byte_size(ExternalId) > 0,
    byte_size(ExternalId) =< 256
->
    validate(Spec),
    _ = one(
        elib_pg:query(Conn, <<"SELECT id FROM adm_user WHERE id=$1 AND status=1 FOR SHARE">>, [
            AdminId
        ]),
        invalid_admin
    ),
    _App = one(
        elib_pg:query(
            Conn,
            <<
                "SELECT id FROM enterprise_application"
                " WHERE id=$1 AND status='active' FOR UPDATE"
            >>,
            [AppId]
        ),
        application_not_active
    ),
    Owner = maps:get(confirmed_owner_user_id, Spec),
    _ =
        case maps:get(enabled, Spec) of
            true ->
                must(enterprise_directory_source_repo:confirmed_owner_tx(Conn, Owner));
            false ->
                ok
        end,
    Until = maps:get(authorization_expires_at, Spec),
    ExpirySql =
        case maps:get(enabled, Spec) of
            true ->
                <<"SELECT $1::timestamptz AS expires_at WHERE $1::timestamptz>clock_timestamp()">>;
            false ->
                <<"SELECT $1::timestamptz AS expires_at">>
        end,
    _ = one(
        elib_pg:query(
            Conn,
            ExpirySql,
            [Until]
        ),
        invalid_authorization_expiry
    ),
    Rows = must(
        elib_pg:query(
            Conn,
            <<
                "SELECT * FROM enterprise_directory_organization_source"
                " WHERE application_id=$1 AND external_organization_id=$2 FOR UPDATE"
            >>,
            [AppId, ExternalId]
        )
    ),
    configure(Conn, AdminId, AppId, ExternalId, Spec, Rows);
configure_source_tx(_, _, _, _, _) ->
    throw({abort_tx, invalid_source_configuration}).

validate(
    #{
        confirmed_owner_user_id := Owner,
        enabled := Enabled,
        authorization_expires_at := Until,
        expected_version := Version
    } = Spec
) when
    is_integer(Owner),
    Owner > 0,
    is_boolean(Enabled),
    is_binary(Until),
    byte_size(Until) > 0,
    is_integer(Version),
    Version >= 0
->
    case
        lists:sort(maps:keys(Spec)) =:=
            lists:sort([
                confirmed_owner_user_id,
                enabled,
                authorization_expires_at,
                expected_version
            ])
    of
        true -> ok;
        false -> throw({abort_tx, invalid_source_configuration})
    end;
validate(_) ->
    throw({abort_tx, invalid_source_configuration}).

configure(Conn, AdminId, AppId, ExternalId, #{expected_version := 0} = Spec, []) ->
    Row = one(
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO enterprise_directory_organization_source"
                " (application_id,external_organization_id,confirmed_owner_user_id,enabled,authorization_expires_at,configured_by_adm_user_id)"
                " VALUES ($1,$2,$3,$4,$5,$6) RETURNING *"
            >>,
            params(AdminId, AppId, ExternalId, Spec)
        ),
        configuration_conflict
    ),
    _ = audit(Conn, AdminId, AppId, Row),
    {ok, Row};
configure(Conn, AdminId, AppId, ExternalId, Spec, [Previous]) ->
    Expected = maps:get(expected_version, Spec),
    case
        {maps:get(<<"configuration_version">>, Previous), maps:get(<<"organization_id">>, Previous)}
    of
        {Expected, null} ->
            ok;
        {Expected, _} ->
            case
                maps:get(<<"confirmed_owner_user_id">>, Previous) =:=
                    maps:get(confirmed_owner_user_id, Spec)
            of
                true -> ok;
                false -> throw({abort_tx, initial_owner_immutable})
            end;
        _ ->
            throw({abort_tx, configuration_conflict})
    end,
    Row = one(
        elib_pg:query(
            Conn,
            <<
                "UPDATE enterprise_directory_organization_source"
                " SET confirmed_owner_user_id=$3,enabled=$4,authorization_expires_at=$5,configured_by_adm_user_id=$6,"
                " configuration_version=configuration_version+1"
                " WHERE application_id=$1 AND external_organization_id=$2 RETURNING *"
            >>,
            params(AdminId, AppId, ExternalId, Spec)
        ),
        configuration_conflict
    ),
    _ = audit(Conn, AdminId, AppId, Row),
    {ok, Row};
configure(_, _, _, _, _, _) ->
    throw({abort_tx, configuration_conflict}).

params(AdminId, AppId, ExternalId, Spec) ->
    [
        AppId,
        ExternalId,
        maps:get(confirmed_owner_user_id, Spec),
        maps:get(enabled, Spec),
        maps:get(authorization_expires_at, Spec),
        AdminId
    ].

audit(Conn, AdminId, AppId, Row) ->
    must(
        enterprise_audit_event_repo:append_tx(
            Conn,
            maps:get(<<"organization_id">>, Row),
            #{
                resource_type => <<"enterprise_application">>,
                resource_id => AppId,
                action => <<"directory.source_configured">>,
                actor_role => <<"platform_admin">>,
                detail => #{
                    <<"adm_user_id">> => AdminId,
                    <<"enabled">> => maps:get(<<"enabled">>, Row),
                    <<"confirmed_owner_user_id">> => maps:get(<<"confirmed_owner_user_id">>, Row),
                    <<"authorization_expires_at">> => maps:get(<<"authorization_expires_at">>, Row),
                    <<"configuration_version">> => maps:get(<<"configuration_version">>, Row)
                }
            }
        )
    ).

one(Result, Reason) ->
    case must(Result) of
        [Row] -> Row;
        _ -> throw({abort_tx, Reason})
    end.
must(ok) -> ok;
must({ok, Value}) -> Value;
must({error, Reason}) -> throw({abort_tx, Reason}).
