-module(enterprise_directory_admin_http_pg_tests).
-export([exercise/1]).
-include_lib("eunit/include/eunit.hrl").

directory_http_test_() ->
    {setup, fun intbe02_http_support:setup_all/0, fun teardown/1, fun(State) ->
        [{timeout, 600, fun() -> exercise(State) end}]
    end}.

teardown(State) ->
    try
        intbe02_http_support:teardown_all(State)
    after
        inttest_marker_db:release(State)
    end.

exercise(State) ->
    Conn = maps:get(conn, State),
    Headers = admin_headers(Conn),
    Input = #{
        <<"application_key">> => <<"directory-http-oa">>,
        <<"name">> => <<"OA Directory">>,
        <<"allowed_scopes">> => enterprise_internal_scope:all()
    },
    CreatePath = <<"/api/adm/enterprise/directory/connections">>,
    Denied = request(State, CreatePath, Input, #{}),
    ?assertNotEqual(0, maps:get(<<"code">>, Denied, -1)),
    Created = payload(request(State, CreatePath, Input, Headers)),
    ?assertEqual(
        [<<"identities:write">>, <<"identities:read">>], maps:get(<<"allowed_scopes">>, Created)
    ),
    AppId = maps:get(<<"application_id">>, Created),
    Path = <<"/api/adm/enterprise/directory/applications/", AppId/binary>>,
    Owner = intbe02_http_support:fixture(h_a1, State),
    Configuration = #{
        <<"external_organization_id">> => <<"oa-directory-org">>,
        <<"confirmed_owner_user_id">> => Owner,
        <<"enabled">> => true,
        <<"authorization_expires_at">> => <<"2030-01-01T00:00:00Z">>,
        <<"expected_version">> => 0
    },
    Configured = payload(request(State, <<Path/binary, "/sources">>, Configuration, Headers)),
    ?assertEqual(1, maps:get(<<"configuration_version">>, Configured)),
    pending_owner_blocked(Conn, Owner, binary_to_integer(AppId)),
    Conflict = request(State, <<Path/binary, "/sources">>, Configuration, Headers),
    ?assertEqual(409, maps:get(<<"code">>, Conflict)),
    Credential = payload(request(State, <<Path/binary, "/credentials">>, #{}, Headers)),
    ok = intbe02_http_support:sql_exec(
        Conn,
        <<"UPDATE enterprise_application SET allowed_scopes='[]'::jsonb WHERE id=$1">>,
        [binary_to_integer(AppId)]
    ),
    Machine = #{
        signed_credential => maps:get(<<"secret">>, Credential),
        <<"idempotency-key">> => <<"directory-http-sync-1">>
    },
    sync_and_verify(State, Conn, AppId, Machine, Owner),
    Renewed = payload(
        request(
            State,
            <<Path/binary, "/sources">>,
            Configuration#{
                <<"expected_version">> => 1,
                <<"authorization_expires_at">> => <<"2031-01-01T00:00:00Z">>
            },
            Headers
        )
    ),
    ?assertEqual(2, maps:get(<<"configuration_version">>, Renewed)),
    SourceDenied = request(State, <<Path/binary, "/sources">>, Configuration, Machine),
    ?assertNotEqual(0, maps:get(<<"code">>, SourceDenied, -1)),
    revoke_and_verify(
        State, Conn, Path, Configuration#{<<"expected_version">> => 2}, Headers, Machine, AppId
    ).

pending_owner_blocked(Conn, Owner, AppId) ->
    lists:foreach(
        fun({Enabled, Until}) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "UPDATE enterprise_directory_organization_source SET enabled=$2,authorization_expires_at=$3::timestamptz"
                    " WHERE application_id=$1"
                >>,
                [AppId, Enabled, Until]
            ),
            {ok, #{blockers := Blockers}} = eb_preflight_facts_pg:facts_enterprise_business(Owner),
            ?assert(
                lists:any(
                    fun(#{code := Code}) -> Code =:= <<"DIRECTORY_INITIAL_OWNER_PENDING">> end,
                    Blockers
                )
            ),
            {ok, _} = elib_pg:query(Conn, <<"BEGIN">>, []),
            try user_deletion_executor:execute_main_tx(Conn, Owner) of
                Result -> error({pending_owner_deletion_allowed, Result})
            catch
                throw:{abort_tx, {deletion_preflight_blockers, ActualBlockers}} ->
                    ?assert(
                        lists:any(
                            fun(Blocker) ->
                                maps:get(code, Blocker) =:= <<"DIRECTORY_INITIAL_OWNER_PENDING">> andalso
                                    maps:get(resource_id, Blocker) =:= integer_to_binary(AppId) andalso
                                    maps:get(organization_id, Blocker) =:= null
                            end,
                            ActualBlockers
                        )
                    )
            after
                {ok, _} = elib_pg:query(Conn, <<"ROLLBACK">>, [])
            end,
            {ok, [_]} = elib_pg:query(Conn, <<"SELECT id FROM \"user\" WHERE id=$1">>, [Owner])
        end,
        [
            {true, <<"2030-01-01T00:00:00Z">>},
            {false, <<"2030-01-01T00:00:00Z">>},
            {true, <<"2000-01-01T00:00:00Z">>}
        ]
    ),
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "UPDATE enterprise_directory_organization_source SET enabled=true,authorization_expires_at='2030-01-01T00:00:00Z'"
            " WHERE application_id=$1"
        >>,
        [AppId]
    ).

revoke_and_verify(State, Conn, Path, Configuration, Headers, Machine, AppId) ->
    Disabled = payload(
        request(
            State,
            <<Path/binary, "/sources">>,
            Configuration#{
                <<"enabled">> => false,
                <<"expected_version">> => maps:get(<<"expected_version">>, Configuration),
                <<"authorization_expires_at">> => <<"2000-01-01T00:00:00Z">>
            },
            Headers
        )
    ),
    ?assertEqual(false, maps:get(<<"enabled">>, Disabled)),
    ?assertEqual(946684800000, maps:get(<<"authorization_expires_at">>, Disabled)),
    Rejected = request(
        State,
        <<"/api/internal/v1/directory/sync">>,
        #{
            <<"organization">> => #{
                <<"external_id">> => <<"oa-directory-org">>,
                <<"name">> => <<"OA Created Org">>,
                <<"version">> => 1
            },
            <<"departments">> => [],
            <<"persons">> => []
        },
        Machine
    ),
    ?assertMatch(
        #{<<"error">> := #{<<"code">> := <<"organization_boundary_violation">>}}, Rejected
    ),
    Enabled = payload(
        request(
            State,
            <<Path/binary, "/sources">>,
            Configuration#{<<"expected_version">> => 3},
            Headers
        )
    ),
    ?assertEqual(true, maps:get(<<"enabled">>, Enabled)),
    ?assertEqual(4, maps:get(<<"configuration_version">>, Enabled)),
    ?assertMatch(
        {ok, [#{<<"n">> := 0}]},
        elib_pg:query(
            Conn,
            <<"SELECT count(*) AS n FROM enterprise_application_grant WHERE application_id=$1">>,
            [binary_to_integer(AppId)]
        )
    ).

sync_and_verify(State, Conn, AppId, Headers, Owner) ->
    Body = #{
        <<"organization">> => #{
            <<"external_id">> => <<"oa-directory-org">>,
            <<"name">> => <<"OA Created Org">>,
            <<"version">> => 1
        },
        <<"departments">> => [],
        <<"persons">> => []
    },
    Synced = request(State, <<"/api/internal/v1/directory/sync">>, Body, Headers),
    ?assertEqual(true, maps:get(<<"synced">>, Synced)),
    ?assertEqual(Synced, request(State, <<"/api/internal/v1/directory/sync">>, Body, Headers)),
    {ok, [#{<<"owner_id">> := Owner, <<"organization_id">> := OrgId}]} = elib_pg:query(
        Conn,
        <<
            "SELECT o.owner_id,s.organization_id FROM enterprise_directory_organization_source s"
            " JOIN organization o ON o.id=s.organization_id WHERE s.application_id=$1"
        >>,
        [binary_to_integer(AppId)]
    ),
    ?assert(is_integer(OrgId)),
    ?assertMatch(
        {ok, [#{<<"n">> := 0}]},
        elib_pg:query(
            Conn,
            <<"SELECT count(*) AS n FROM enterprise_application_grant WHERE application_id=$1">>,
            [binary_to_integer(AppId)]
        )
    ).

admin_headers(Conn) ->
    AdminId = 976811,
    ok = intbe02_http_support:sql_exec(
        Conn,
        <<
            "INSERT INTO adm_user(id,account,password,nickname,role_id)"
            " VALUES($1,'directory-http-admin','synthetic-password','directory',ARRAY[1]::bigint[])"
        >>,
        [AdminId]
    ),
    {ok, Signature} = adm_session_ds:issue(integer_to_binary(AdminId)),
    #{
        <<"cookie">> =>
            <<"adm_user_id=", (integer_to_binary(AdminId))/binary, "; adm_user_sig=",
                Signature/binary>>
    }.

request(State, Path, Body, Headers) ->
    Raw = jsone:encode(Body),
    WireHeaders =
        case maps:take(signed_credential, Headers) of
            {Credential, PublicHeaders} ->
                intbe02_http_support:signed_headers(
                    Credential,
                    <<"POST">>,
                    Path,
                    Raw,
                    PublicHeaders#{<<"content-type">> => <<"application/json">>},
                    sha256
                );
            error ->
                Headers
        end,
    Response = intbe02_http_support:http(maps:get(port, State), <<"POST">>, Path, Raw, WireHeaders),
    jsone:decode(maps:get(body, Response)).

payload(#{<<"code">> := 0, <<"payload">> := Value}) -> Value.
