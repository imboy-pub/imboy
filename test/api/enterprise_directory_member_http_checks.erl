-module(enterprise_directory_member_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

%% Real authenticated HTTP, same disposable marker DB as the audit matrix.
run(#{conn := C, app_a := App} = S) ->
    Org = intbe02_http_support:fixture(org_a, S),
    User = 995090,
    ok = exec(
        C,
        <<"INSERT INTO \"user\"(id,password,account,account_type,status,reg_ip,reg_cosv) VALUES($1,'synthetic','audit-member-lifecycle',0,1,'127.0.0.1','synthetic')">>,
        [User]
    ),
    ok = exec(
        C,
        <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
        [Org, User]
    ),
    case application:get_env(imboy, internal_test_hmac_client, false) of
        true ->
            ok;
        false ->
            ok = exec(
                C,
                <<"UPDATE enterprise_application SET allowed_scopes=allowed_scopes || '[\"members:write\"]'::jsonb WHERE id=$1">>,
                [App]
            ),
            ok = exec(
                C,
                <<"INSERT INTO enterprise_application_grant_scope(grant_id,scope) SELECT id,'members:write' FROM enterprise_application_grant WHERE application_id=$1 AND organization_id=$2 AND workspace_scope_kind='none' AND status='active' ON CONFLICT DO NOTHING">>,
                [App, Org]
            )
    end,
    {ok, _} = enterprise_source_person_repo:sync_tx(
        C, App, <<"audit-member">>, <<"Synthetic OA member">>, <<"active">>, 1
    ),
    ok = exec(
        C,
        <<"UPDATE enterprise_source_person SET user_id=$2 WHERE application_id=$1 AND external_user_id='audit-member'">>,
        [App, User]
    ),
    ok = exec(
        C,
        <<"INSERT INTO enterprise_directory_membership_source(application_id,organization_id,external_user_id,department_external_ids,source_version,payload_digest,status,managed,projected_user_id) VALUES($1,$2,'audit-member','[]',1,repeat('b',64),'active',true,$3)">>,
        [App, Org, User]
    ),
    First = get(S, query()),
    ?assertEqual(true, maps:get(<<"can_suspend">>, First)),
    intbe02_http_support:cover(<<"INT-48">>, ok),
    lists:foreach(
        fun(Q) ->
            error_reply(read(S, Q), <<"invalid_request">>)
        end,
        [
            <<"?external_organization_id=%">>,
            <<"?external_organization_id=a&external_organization_id=b&external_user_id=x">>,
            <<"?unknown=x">>
        ]
    ),
    error_reply(
        read(S, <<"?external_organization_id=foreign&external_user_id=audit-member">>),
        <<"resource_not_found">>
    ),
    Access = enterprise_directory_member_access_http_checks:setup(S, Org, User),
    enterprise_directory_member_access_http_checks:check(Access, true),
    V = maps:get(<<"member_version">>, First),
    Pause = write(S, <<"suspended">>, V, <<"audit-member-pause">>),
    ?assertEqual(200, maps:get(status, Pause)),
    intbe02_http_support:cover(<<"INT-49">>, ok),
    enterprise_directory_member_access_http_checks:check(Access, false),
    Paused = get(S, query()),
    ?assertEqual(true, maps:get(<<"can_restore">>, Paused)),
    error_reply(write(S, <<"active">>, V, <<"audit-member-stale">>), <<"version_conflict">>),
    Restore = write(
        S, <<"active">>, maps:get(<<"member_version">>, Paused), <<"audit-member-restore">>
    ),
    ?assertEqual(200, maps:get(status, Restore)),
    enterprise_directory_member_access_http_checks:check(Access, true).

query() -> <<"?external_organization_id=audit-directory-org&external_user_id=audit-member">>.
read(#{port := Port, cred_a := Cred}, Q) ->
    intbe02_http_support:http(
        Port,
        <<"GET">>,
        <<"/api/internal/v1/directory/memberships", Q/binary>>,
        <<>>,
        intbe02_http_support:auth(Cred)
    ).
get(S, Q) ->
    R = read(S, Q),
    ?assertEqual(200, maps:get(status, R)),
    ?assertEqual(<<"no-store">>, intbe02_http_support:json_header_val(R, <<"cache-control">>)),
    jsone:decode(maps:get(body, R)).
write(S, Status, V, Key) ->
    enterprise_internal_audit_http_checks:request(
        S,
        <<"PATCH">>,
        <<"/api/internal/v1/directory/memberships">>,
        #{
            <<"external_organization_id">> => <<"audit-directory-org">>,
            <<"external_user_id">> => <<"audit-member">>,
            <<"status">> => Status,
            <<"expected_member_version">> => V
        },
        Key
    ).
error_reply(R, Code) ->
    ?assertEqual(enterprise_internal_error:http_status(Code), maps:get(status, R)),
    ?assertMatch(#{<<"error">> := #{<<"code">> := Code}}, jsone:decode(maps:get(body, R))).
exec(C, Q, P) -> intbe02_http_support:sql_exec(C, Q, P).
