-module(enterprise_app_credential_lifecycle_http_pg_tests).
-export([run/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SUP, intbe02_http_support).

-define(ORG, 995101).
-define(KEY_L, <<"intbe05-oa-lifecycle">>).
-define(KEY_M, <<"intbe05-oa-revoked">>).
-define(KEY_Z, <<"intbe05-oa-active">>).
-define(SCOPES, [<<"application:read">>, <<"workspaces:read">>, <<"groups:read">>]).
-define(ACTOR, #{adm_user_id => 995001, account => <<"intbe05-admin">>}).
%% intbe02 种子矩阵的 principal（995 段；套件动态 app 复用同 principal）。
-define(PRIN, 995014).

%% Reuse an existing disposable HTTP/PG fixture without provisioning another DB.
run(State) ->
    eunit:test(cases(State), [verbose]).

app_credential_lifecycle_http_pg_test_() ->
    {setup, fun ?SUP:setup_all/0, fun teardown/1, fun cases/1}.

teardown(State) ->
    try
        ?SUP:teardown_all(State)
    after
        inttest_marker_db:release(State)
    end.

cases(State) ->
    [
        {timeout, 600, fun() -> full_lifecycle_rotate_revoke(State) end},
        {timeout, 300, fun() -> secret_shown_once_encrypted_only(State) end},
        {timeout, 300, fun() -> active_disabled_expired_credentials(State) end},
        {timeout, 300, fun() -> audit_failure_preserves_http_access(State) end},
        {timeout, 300, fun() -> audit_trail_complete(State) end}
    ].

full_lifecycle_rotate_revoke(State) ->
    {ok, App} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, ?KEY_L, <<"intbe05 lifecycle oa"/utf8>>, ?PRIN
        )
    end),
    AppId = json_int(maps:get(<<"id">>, App)),
    {ok, #{<<"secret">> := Full1, <<"credential">> := Meta1}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppId, undefined, ?ACTOR),
    CredId = json_int(maps:get(<<"id">>, Meta1)),
    %% internal 调用成功（INT-01 真 HTTP）。
    assert_application_info(State, Full1, AppId, CredId),
    %% rotate：旧 secret 立即 401、新 secret 200（rotate 撤旧签新——
    %% 旧 credential 行状态机转 revoked）。
    {ok, #{<<"secret">> := Full2, <<"credential">> := Meta2}} =
        enterprise_admin_governance_logic:rotate_credential(?ORG, AppId, CredId, ?ACTOR),
    NewCredId = json_int(maps:get(<<"id">>, Meta2)),
    ?assertNotEqual(Full1, Full2),
    ?assertEqual(401, internal_status(State, Full1), "rotate 后旧 secret 必须立即失效"),
    assert_application_info(State, Full2, AppId, NewCredId),
    ?assertEqual(
        <<"revoked">>,
        one(
            <<
                "SELECT status FROM enterprise_application_credential"
                " WHERE organization_id = $1 AND id = $2"
            >>,
            [?ORG, CredId]
        )
    ),
    %% revoke rotate 后的新 credential：新 secret 立即 401（行转 revoked）。
    ok = enterprise_admin_governance_logic:revoke_credential(?ORG, AppId, NewCredId, ?ACTOR),
    ?assertEqual(401, internal_status(State, Full2), "revoke 后新 secret 必须立即失效"),
    ?assertEqual(
        <<"revoked">>,
        one(
            <<
                "SELECT status FROM enterprise_application_credential"
                " WHERE organization_id = $1 AND id = $2"
            >>,
            [?ORG, NewCredId]
        )
    ).

secret_shown_once_encrypted_only(_State) ->
    {ok, App} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, <<"intbe05-oa-secret">>, <<"intbe05 secret oa"/utf8>>, ?PRIN
        )
    end),
    AppId = json_int(maps:get(<<"id">>, App)),
    {ok, #{<<"secret">> := Full, <<"credential">> := MetaS}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppId, undefined, ?ACTOR),
    CredId = json_int(maps:get(<<"id">>, MetaS)),
    ?assertMatch(<<"ib_int_", _/binary>>, Full),
    [Prefix, SecretPart] = binary:split(Full, <<".">>),
    ?assertEqual(Prefix, maps:get(<<"credential_prefix">>, MetaS)),
    ?assert(byte_size(SecretPart) >= 32),
    Columns = one(
        <<
            "SELECT string_agg(column_name, ',') FROM information_schema.columns"
            " WHERE table_name = 'enterprise_application_credential'"
        >>,
        []
    ),
    ?assert(is_binary(Columns)),
    SecretColumns = [
        C
     || C <- binary:split(Columns, <<",">>, [global]),
        binary:match(C, <<"secret">>) =/= nomatch
    ],
    ?assertEqual(
        lists:sort([<<"secret_digest">>, <<"signing_secret_enc">>]), lists:sort(SecretColumns)
    ),
    Ciphertext = one(
        <<
            "SELECT signing_secret_enc FROM enterprise_application_credential"
            " WHERE organization_id = $1 AND id = $2"
        >>,
        [?ORG, CredId]
    ),
    ?assert(is_binary(Ciphertext) andalso byte_size(Ciphertext) > 0),
    ?assertNotEqual(SecretPart, Ciphertext),
    ?assertEqual(
        {ok, SecretPart},
        elib_pg:with_conn(fun(C) ->
            {ok, Row} = enterprise_application_credential_repo:find_by_prefix_tx(C, Prefix),
            enterprise_application_credential_repo:signing_secret(Row)
        end)
    ),
    Creds = enterprise_admin_governance_logic:list_credentials(?ORG, AppId),
    ?assertMatch({ok, _}, Creds),
    {ok, CredRows} = Creds,
    lists:foreach(
        fun(Row) ->
            ?assertNot(is_map_key(<<"secret">>, Row)),
            ?assertNot(is_map_key(<<"secret_digest">>, Row))
        end,
        CredRows
    ).

active_disabled_expired_credentials(State) ->
    %% A live signed credential admits its application without a permission registry.
    {ok, AppZ} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, ?KEY_Z, <<"intbe05 active oa"/utf8>>, ?PRIN
        )
    end),
    AppZId = json_int(maps:get(<<"id">>, AppZ)),
    {ok, #{<<"secret">> := FullZ}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppZId, undefined, ?ACTOR),
    ?assertEqual(200, internal_status(State, FullZ)),
    %% disabled Application：credential 有效也 403。
    {ok, #{version := VerZ}} = app_version(AppZId),
    ok = enterprise_admin_governance_logic:set_status(?ORG, AppZId, VerZ, <<"disabled">>, ?ACTOR),
    {ok, #{<<"secret">> := FullD}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppZId, undefined, ?ACTOR),
    ?assertEqual(403, internal_status(State, FullD)),
    {ok, AppE} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, <<"intbe05-oa-expired">>, <<"intbe05 expired oa"/utf8>>, ?PRIN
        )
    end),
    AppEId = json_int(maps:get(<<"id">>, AppE)),
    {ok, #{<<"secret">> := FullE}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppEId, undefined, ?ACTOR),
    ok = elib_pg:with_conn(fun(C) ->
        {ok, _} = elib_pg:execute(
            C,
            <<"UPDATE enterprise_application_credential SET expires_at=clock_timestamp()-interval '1 second' WHERE application_id=$1">>,
            [AppEId]
        ),
        ok
    end),
    ?assertEqual(401, internal_status(State, FullE), "expired credential must fail closed").

%% Failed governance must not silently revoke a still usable credential.
audit_failure_preserves_http_access(State) ->
    {ok, App} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, <<"intbe05-audit-rollback">>, <<"synthetic audit rollback">>, ?PRIN
        )
    end),
    AppId = json_int(maps:get(<<"id">>, App)),
    {ok, #{<<"secret">> := Full, <<"credential">> := Meta}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppId, undefined, ?ACTOR),
    CredId = json_int(maps:get(<<"id">>, Meta)),
    assert_application_info(State, Full, AppId, CredId),
    ok = cs_pg_test_fixture:exec(
        <<"CREATE FUNCTION gate_drop_lifecycle_audit() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.action='credential_revoked' THEN RETURN NULL; END IF; RETURN NEW; END $$">>,
        []
    ),
    ok = cs_pg_test_fixture:exec(
        <<"CREATE TRIGGER gate_drop_lifecycle_audit BEFORE INSERT ON enterprise_audit_event FOR EACH ROW EXECUTE FUNCTION gate_drop_lifecycle_audit()">>,
        []
    ),
    Result =
        try
            enterprise_admin_governance_logic:revoke_credential(?ORG, AppId, CredId, ?ACTOR)
        after
            ok = cs_pg_test_fixture:exec(
                <<"DROP TRIGGER gate_drop_lifecycle_audit ON enterprise_audit_event">>, []
            ),
            ok = cs_pg_test_fixture:exec(<<"DROP FUNCTION gate_drop_lifecycle_audit()">>, [])
        end,
    ?assert(Result =:= {error, {audit_failed, audit_not_inserted}}),
    assert_application_info(State, Full, AppId, CredId),
    ok = enterprise_admin_governance_logic:revoke_credential(?ORG, AppId, CredId, ?ACTOR),
    ?assertEqual(401, internal_status(State, Full)).

%% ===================================================================
%% 全链审计完整（五类动作各有落行）
%% ===================================================================

audit_trail_complete(_State) ->
    Counts = one(
        <<
            "SELECT string_agg(action || '=' || n::text, ',') FROM ("
            "  SELECT action, count(*) AS n FROM enterprise_audit_event"
            "  WHERE organization_id = $1 AND resource_type = 'enterprise_application'"
            "    AND action IN ('credential_issued','credential_rotated',"
            "                   'credential_revoked')"
            "  GROUP BY action) t"
        >>,
        [?ORG]
    ),
    ?assert(is_binary(Counts), {audit_counts, Counts}),
    ?assert(binary:match(Counts, <<"credential_issued=">>) =/= nomatch),
    ?assert(binary:match(Counts, <<"credential_rotated=">>) =/= nomatch),
    ?assert(binary:match(Counts, <<"credential_revoked=">>) =/= nomatch).

%% ===================================================================
%% helpers
%% ===================================================================

app_version(AppId) ->
    Row =
        one_map(
            <<
                "SELECT id, version FROM enterprise_application"
                " WHERE organization_id = $1 AND id = $2"
            >>,
            [?ORG, AppId]
        ),
    {ok, #{version => maps:get(<<"version">>, Row)}}.

%% JSON 投影整数（binary | integer 兼容）。
json_int(V) when is_integer(V) -> V;
json_int(V) when is_binary(V) -> binary_to_integer(V).

%% INT-01 success binds to the actual application/credential, not just HTTP 200.
assert_application_info(State, FullCredential, AppId, CredId) ->
    R = internal_response(State, FullCredential),
    ?assertEqual(200, maps:get(status, R)),
    Json = jsone:decode(maps:get(body, R)),
    ?assertEqual(?ORG, maps:get(<<"organization_id">>, Json)),
    ?assertEqual(AppId, maps:get(<<"application_id">>, Json)),
    ?assertEqual(CredId, maps:get(<<"credential_id">>, Json)),
    ?assertEqual(<<"hmac">>, maps:get(<<"authentication">>, Json)),
    ?assertNot(maps:is_key(<<"granted_scopes">>, Json)).

internal_status(State, FullCredential) ->
    maps:get(status, internal_response(State, FullCredential)).

internal_response(State, FullCredential) ->
    R = ?SUP:http(
        maps:get(port, State),
        <<"GET">>,
        <<"/api/internal/v1/application">>,
        <<>>,
        ?SUP:auth(FullCredential)
    ),
    Body = maps:get(body, R),
    ?assert(binary:match(Body, FullCredential) =:= nomatch),
    Json = jsone:decode(Body),
    assert_safe_payload(Json),
    case os:getenv("IMBOY_GATE_RUN_DIR") of
        false ->
            ok;
        Dir ->
            Safe = #{
                status => maps:get(status, R),
                application_id => maps:get(<<"application_id">>, Json, null),
                credential_id => maps:get(<<"credential_id">>, Json, null),
                error => maps:get(<<"error">>, Json, null),
                safe_payload => true
            },
            ok = file:write_file(
                filename:join(Dir, "credential-lifecycle-http.jsonl"),
                [jsone:encode(Safe), <<"\n">>],
                [append]
            )
    end,
    R.

assert_safe_payload(Value) when is_map(Value) ->
    maps:foreach(
        fun(Key, Child) ->
            ?assertNot(
                lists:member(Key, [
                    <<"secret">>,
                    <<"secret_digest">>,
                    <<"credential_secret">>,
                    <<"token_digest">>,
                    <<"private_key">>
                ])
            ),
            assert_safe_payload(Child)
        end,
        Value
    );
assert_safe_payload(Value) when is_list(Value) ->
    lists:foreach(fun assert_safe_payload/1, Value);
assert_safe_payload(_) ->
    ok.

one(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} ->
            case maps:values(Row) of
                [Value | _] -> Value;
                [] -> undefined
            end;
        {ok, []} ->
            undefined;
        {error, Reason} ->
            erlang:error({intbe05_sql_failed, Sql, Reason})
    end.

one_map(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} -> Row;
        {error, Reason} -> erlang:error({intbe05_sql_failed, Sql, Reason})
    end.
