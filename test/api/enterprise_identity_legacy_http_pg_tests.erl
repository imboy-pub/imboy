-module(enterprise_identity_legacy_http_pg_tests).
-include_lib("eunit/include/eunit.hrl").

legacy_identity_http_test_() ->
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
    AppId = maps:get(app_a, State),
    OrgId = intbe02_http_support:fixture(org_a, State),
    ExternalId = intbe02_http_support:fixture(ext_h1, State),
    UserId = intbe02_http_support:fixture(h_a1, State),
    OtherUserId = intbe02_http_support:fixture(h_a2, State),
    Path = iolist_to_binary([
        <<"/api/adm/enterprise/organizations/">>,
        integer_to_binary(OrgId),
        <<"/applications/">>,
        integer_to_binary(AppId),
        <<"/identities">>
    ]),
    lists:foreach(
        fun(Status) ->
            verify_status(
                State, Conn, Headers, AppId, ExternalId, UserId, OtherUserId, Path, Status
            )
        end,
        [<<"active">>, <<"removed">>]
    ).

admin_headers(Conn) ->
    AdminId = 976801,
    ok = intbe02_http_support:sql_exec(
        Conn,
        <<
            "INSERT INTO adm_user(id,account,password,nickname,role_id) "
            "VALUES($1,'r20-admin','synthetic-password','r20',ARRAY[1]::bigint[])"
        >>,
        [AdminId]
    ),
    {ok, Signature} = adm_session_ds:issue(integer_to_binary(AdminId)),
    #{
        <<"cookie">> => iolist_to_binary([
            <<"adm_user_id=">>, integer_to_binary(AdminId), <<"; adm_user_sig=">>, Signature
        ])
    }.

verify_status(State, Conn, Headers, AppId, ExternalId, UserId, OtherUserId, Path, Status) ->
    ok = intbe02_http_support:sql_exec(
        Conn,
        <<
            "UPDATE enterprise_external_identity SET status=$3 "
            "WHERE application_id=$1 AND external_user_id=$2"
        >>,
        [AppId, ExternalId, Status]
    ),
    Before = snapshot(Conn, AppId, ExternalId),
    Rejected = request(State, Path, Headers, ExternalId, OtherUserId),
    ?assertEqual(200, maps:get(status, Rejected)),
    ?assertEqual(409, maps:get(<<"code">>, jsone:decode(maps:get(body, Rejected)))),
    ?assertEqual(Before, snapshot(Conn, AppId, ExternalId)),
    Restored = request(State, Path, Headers, ExternalId, UserId),
    ?assertEqual(200, maps:get(status, Restored)),
    ?assertMatch(#{<<"code">> := 0}, jsone:decode(maps:get(body, Restored))),
    {ok, [#{<<"user_id">> := UserId, <<"status">> := <<"active">>}]} =
        elib_pg:query(
            Conn,
            <<
                "SELECT user_id,status FROM enterprise_external_identity "
                "WHERE application_id=$1 AND external_user_id=$2"
            >>,
            [AppId, ExternalId]
        ).

request(State, Path, Headers, ExternalId, UserId) ->
    intbe02_http_support:http(
        maps:get(port, State),
        <<"POST">>,
        Path,
        #{<<"external_user_id">> => ExternalId, <<"user_id">> => UserId},
        Headers
    ).

snapshot(Conn, AppId, ExternalId) ->
    {ok, Identities} = elib_pg:query(
        Conn,
        <<
            "SELECT to_jsonb(i) AS row FROM enterprise_external_identity i "
            "WHERE application_id=$1 AND external_user_id=$2 ORDER BY organization_id"
        >>,
        [AppId, ExternalId]
    ),
    {ok, Persons} = elib_pg:query(
        Conn,
        <<
            "SELECT to_jsonb(p) AS row FROM enterprise_source_person p "
            "WHERE application_id=$1 AND external_user_id=$2"
        >>,
        [AppId, ExternalId]
    ),
    {ok, Audit} = elib_pg:query(
        Conn,
        <<"SELECT count(*) AS count FROM enterprise_audit_event">>,
        []
    ),
    {Identities, Persons, Audit}.
