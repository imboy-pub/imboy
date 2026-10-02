%% Real HTTP/PG authorization negatives across the complete route registry.
-module(enterprise_internal_authorization_http_checks).
-export([run/0, run/1]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    S = intbe02_http_support:setup_all(),
    try
        run(S)
    after
        intbe02_http_support:teardown_all(S),
        inttest_marker_db:release(S)
    end.

run(#{conn := C} = S) ->
    Routes = enterprise_internal_routes:routes(),
    ?assertEqual(42, length(Routes)),
    {AppId, Credential} = create_application(C),
    check_all(S, Routes, zero_grant, fun(_) -> Credential end),
    {ok, Grant} = enterprise_internal_ops:issue_grant_tx(C, 995101, AppId, #{
        scopes => enterprise_internal_scope:all(),
        workspace_scope_kind => none,
        workspace_ids => [],
        idempotency_key => <<"auth-matrix-all-scopes">>,
        expires_at => <<"2099-01-01T00:00:00Z">>
    }),
    assert_application_access(S, Credential),
    check_all(S, Routes, missing_scope, fun(R) ->
        case maps:get(scope, R) of
            <<"application:read">> -> maps:get(cred_w, S);
            _ -> maps:get(cred_ro, S)
        end
    end),
    ok = enterprise_internal_ops:revoke_grant_tx(
        C, 995101, AppId, maps:get(<<"id">>, Grant), maps:get(<<"version">>, Grant), 995001
    ),
    check_all(S, Routes, revoked_grant, fun(_) -> Credential end),
    {ok, _} = enterprise_application_grant_repo:create_tx(C, 995101, AppId, #{
        scopes => enterprise_internal_scope:all(),
        workspace_scope_kind => none,
        workspace_ids => [],
        idempotency_key => <<"auth-matrix-expired">>,
        valid_from => <<"2020-01-01T00:00:00Z">>,
        expires_at => <<"2020-12-31T00:00:00Z">>
    }),
    check_all(S, Routes, expired_grant, fun(_) -> Credential end),
    io:format("INTERNAL_ALL_ROUTES_AUTHORIZATION_RESULT=ok~n").

create_application(C) ->
    {ok, App} = enterprise_application_repo:create_tx(
        C,
        995101,
        <<"auth-matrix-all">>,
        <<"Synthetic authorization matrix">>,
        {995014, enterprise_internal_scope:all()}
    ),
    Id = maps:get(<<"id">>, App),
    {ok, #{credential := Credential}} = enterprise_internal_ops:issue_credential_tx(
        C, 995101, Id, <<"synthetic-authorization-matrix-only">>, undefined
    ),
    {Id, Credential}.

assert_application_access(S, Credential) ->
    R = intbe02_http_support:http(
        maps:get(port, S),
        <<"GET">>,
        <<"/api/internal/v1/application">>,
        #{},
        intbe02_http_support:auth(Credential)
    ),
    ?assertEqual(200, maps:get(status, R)).

check_all(S, Routes, Kind, CredentialFor) ->
    Before = business_snapshot(maps:get(conn, S)),
    lists:foreach(fun(R) -> check(S, R, Kind, CredentialFor(R)) end, Routes),
    ?assertEqual(Before, business_snapshot(maps:get(conn, S))),
    io:format("INTERNAL_AUTH_~s=PASS routes=42~n", [string:uppercase(atom_to_list(Kind))]).

check(S, Route, Kind, Credential) ->
    check(S, Route, Kind, Credential, body(maps:get(id, Route)), application),
    case maps:get(id, Route) of
        Id when Id =:= <<"INT-09">>; Id =:= <<"INT-10">> ->
            Human = (body(Id))#{
                <<"sender_mode">> => <<"human">>,
                <<"sender_user_id">> => <<"intbe02-ext-h1">>
            },
            check(S, Route, Kind, Credential, Human, human);
        _ ->
            ok
    end.

check(S, Route, Kind, Credential, Body, Mode) ->
    Id = maps:get(id, Route),
    Path =
        case Id of
            <<"INT-10">> ->
                <<"/api/internal/v1/groups/995301/messages">>;
            _ ->
                re:replace(maps:get(path, Route), <<"\\{[^}]+\\}">>, <<"1">>, [
                    global, {return, binary}
                ])
        end,
    Key =
        <<"auth-matrix-", (atom_to_binary(Kind))/binary, "-", Id/binary, "-",
            (atom_to_binary(Mode))/binary>>,
    Headers = maps:merge(intbe02_http_support:auth(Credential), intbe02_http_support:idem(Key)),
    R = intbe02_http_support:http(
        maps:get(port, S), maps:get(method, Route), Path, Body, Headers
    ),
    ?assertEqual(403, maps:get(status, R), {Kind, Id}),
    Json = jsone:decode(maps:get(body, R)),
    ?assertEqual(
        <<"insufficient_scope">>, maps:get(<<"code">>, maps:get(<<"error">>, Json)), {Kind, Id}
    ),
    record(Id, Kind, Mode).

body(<<"INT-09">>) ->
    #{
        <<"sender_mode">> => <<"application">>,
        <<"recipient_user_id">> => <<"intbe02-ext-h1">>,
        <<"msg_type">> => <<"text">>,
        <<"content">> => <<"synthetic authorization probe">>
    };
body(<<"INT-10">>) ->
    #{
        <<"sender_mode">> => <<"application">>,
        <<"msg_type">> => <<"text">>,
        <<"content">> => <<"synthetic authorization probe">>
    };
body(_) ->
    #{}.

business_snapshot(C) ->
    Tables = [
        <<"workspace">>,
        <<"\"group\"">>,
        <<"group_member">>,
        <<"channel">>,
        <<"enterprise_external_identity">>,
        <<"attachment">>,
        <<"enterprise_message">>,
        <<"msg_c2c">>,
        <<"msg_c2g">>,
        <<"enterprise_message_origin">>,
        <<"user_friend">>,
        <<"enterprise_oa_sso_code">>,
        <<"customer_service_seat">>,
        <<"customer_service_event">>,
        <<"attach_pending">>,
        <<"enterprise_attachment_retention">>,
        <<"bot_delivery">>,
        <<"enterprise_audit_event">>
    ],
    [
        begin
            Sql =
                <<"SELECT md5(COALESCE(jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text)::text,'[]')) AS digest FROM ",
                    Table/binary, " t">>,
            {ok, [Row]} = elib_pg:query(C, Sql, []),
            {Table, maps:get(<<"digest">>, Row)}
        end
     || Table <- Tables
    ].

record(Id, Kind, Mode) ->
    case os:getenv("IMBOY_GATE_RUN_DIR") of
        false ->
            ok;
        Dir ->
            ok = file:write_file(
                filename:join(Dir, "internal-auth-negatives.jsonl"),
                [
                    jsone:encode(#{
                        id => Id,
                        scenario => Kind,
                        sender_mode => Mode,
                        status => 403,
                        code => <<"insufficient_scope">>
                    }),
                    "\n"
                ],
                [append]
            )
    end.
