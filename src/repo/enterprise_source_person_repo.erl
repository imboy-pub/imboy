-module(enterprise_source_person_repo).
-moduledoc "Source-scoped personnel and verified account association; callers own authentication/transaction audit.".
-export([find_tx/3, sync_tx/6, claim_tx/4]).
-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS,
    "application_id, external_user_id, user_id, binding_retired, display_name, status, source_version"
).

find_tx(Conn, AppId, ExternalId) ->
    row(
        elib_pg:query(
            Conn,
            <<"SELECT ", ?COLUMNS,
                " FROM enterprise_source_person WHERE application_id=$1 AND external_user_id=$2">>,
            [AppId, ExternalId]
        ),
        not_found
    ).

%% Sync never assigns an IMBoy user, even when the caller supplies an existing external ID.
sync_tx(Conn, AppId, ExternalId, Name, Status, Version) when
    is_integer(AppId),
    AppId > 0,
    is_binary(ExternalId),
    byte_size(ExternalId) > 0,
    is_binary(Name),
    (Status =:= <<"active">> orelse Status =:= <<"removed">>),
    is_integer(Version),
    Version > 0,
    Version =< 9223372036854775807
->
    Result = elib_pg:query(
        Conn,
        <<
            "INSERT INTO enterprise_source_person AS p "
            "(application_id,external_user_id,display_name,status,source_version) VALUES ($1,$2,$3,$4,$5) "
            "ON CONFLICT(application_id,external_user_id) DO UPDATE SET display_name=EXCLUDED.display_name, "
            "status=EXCLUDED.status,source_version=EXCLUDED.source_version,updated_at=CURRENT_TIMESTAMP "
            "WHERE p.source_version<EXCLUDED.source_version "
            "AND NOT p.binding_retired RETURNING ",
            ?COLUMNS
        >>,
        [AppId, ExternalId, Name, Status, Version]
    ),
    case Result of
        {ok, []} -> sync_replay(Conn, AppId, ExternalId, Name, Status, Version);
        _ -> row(Result, not_found)
    end;
sync_tx(_, _, _, _, _, _) ->
    {error, invalid_request}.

sync_replay(Conn, AppId, ExternalId, Name, Status, Version) ->
    case find_tx(Conn, AppId, ExternalId) of
        {ok, #{<<"binding_retired">> := true}} ->
            {error, identity_conflict};
        {ok,
            #{
                <<"source_version">> := Version,
                <<"display_name">> := Name,
                <<"status">> := Status
            } = Person} ->
            {ok, Person};
        {ok, #{<<"source_version">> := Current}} when Current > Version ->
            {error, stale_source_version};
        {ok, _} ->
            {error, source_version_conflict};
        Error ->
            Error
    end.

%% Only call after proving both the external identity and the IMBoy account.
claim_tx(Conn, AppId, ExternalId, UserId) when
    is_integer(AppId),
    AppId > 0,
    is_binary(ExternalId),
    byte_size(ExternalId) > 0,
    is_integer(UserId),
    UserId > 0
->
    row(
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO enterprise_source_person AS p "
                "(application_id,external_user_id,user_id) VALUES ($1,$2,$3) "
                "ON CONFLICT(application_id,external_user_id) DO UPDATE SET user_id=EXCLUDED.user_id "
                "WHERE (p.user_id IS NULL OR p.user_id=EXCLUDED.user_id) "
                "AND NOT p.binding_retired AND p.status='active' RETURNING ",
                ?COLUMNS
            >>,
            [AppId, ExternalId, UserId]
        ),
        identity_conflict
    );
claim_tx(_, _, _, _) ->
    {error, invalid_request}.

row({ok, [Person]}, _) -> {ok, Person};
row({ok, []}, Empty) -> {error, Empty};
row({error, #error{code = <<"23505">>}}, _) -> {error, user_already_mapped};
row({error, #error{code = <<"23514">>}}, _) -> {error, invalid_person};
row({error, Reason}, _) -> {error, Reason}.
