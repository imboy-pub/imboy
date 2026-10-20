-module(enterprise_oa_login_three_gap_pg_checks).
-export([setup/0, teardown/1, snapshot/1, link_wrong_key/2, issue_deadline/2, session_deadline/2]).
-include_lib("eunit/include/eunit.hrl").

%% Test-only: caller selects focused generators; production modules are unchanged.
setup() ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    S = intbe02_http_support:setup_all(),
    Owner =
        case whereis(imboy_cache_sync) of
            undefined ->
                {ok, Pid} = imboy_cache_sync:start_link(),
                Pid;
            _ ->
                undefined
        end,
    try
        S#{oa_gap_context => authorize(S), oa_gap_cache_owner => Owner}
    catch
        Class:Reason:Stack ->
            teardown(S#{oa_gap_cache_owner => Owner}),
            erlang:raise(Class, Reason, Stack)
    end.

teardown(S) ->
    try
        case maps:get(oa_gap_cache_owner, S, undefined) of
            undefined -> ok;
            Pid -> gen_server:stop(Pid)
        end
    after
        try
            intbe02_http_support:teardown_all(S)
        after
            inttest_marker_db:release(S)
        end
    end.

authorize(S) ->
    C = maps:get(conn, S),
    App = maps:get(app_sso, S),
    [#{<<"id">> := Cred}] = rows(
        C,
        <<"SELECT id FROM enterprise_application_credential WHERE application_id=$1 AND status='active'">>,
        [App]
    ),
    [#{<<"id">> := Cred}] = rows(
        C,
        <<"UPDATE enterprise_application_credential SET organization_id=NULL WHERE id=$1 RETURNING id">>,
        [Cred]
    ),
    #{application_id => App, credential_id => Cred}.

%% Exact committed business rows, including device login_count/epoch and audit.
snapshot(S) ->
    C = maps:get(conn, S),
    Tables = [
        <<"\"user\"">>,
        <<"user_device">>,
        <<"user_auth_epoch">>,
        <<"enterprise_source_person">>,
        <<"enterprise_external_identity">>,
        <<"enterprise_directory_membership_source">>,
        <<"organization_member">>,
        <<"enterprise_oa_login_ticket">>,
        <<"enterprise_audit_event">>
    ],
    [
        {Table,
            rows(
                C,
                <<"SELECT to_jsonb(t) AS row FROM ", Table/binary,
                    " t ORDER BY to_jsonb(t)::text">>,
                []
            )}
     || Table <- Tables
    ].

link_wrong_key(S, Ticket) ->
    Uid = intbe02_http_support:fixture(h_a1, S),
    Did = <<"oa-three-gap-link">>,
    {ok, _} = elib_pg:with_tx(fun(C) -> enterprise_oa_login_repo:device_tx(C, Uid, Did, #{}) end),
    {ok, #{<<"epoch">> := Epoch}} = elib_pg:with_tx(fun(C) ->
        enterprise_oa_login_repo:epoch_tx(C, Uid)
    end),
    P = Ticket(<<"three-gap-link-person">>, <<"link">>, Did),
    {ok, Pending} = enterprise_oa_login_logic:exchange(P),
    Complete = maps:merge(P, Pending),
    [#{<<"application_key">> := Wrong}] = rows(
        maps:get(conn, S),
        <<"SELECT application_key FROM enterprise_application WHERE id=$1 AND status='active'">>,
        [maps:get(app_a, S)]
    ),
    ?assertNotEqual(maps:get(<<"application_key">>, P), Wrong),
    Before = snapshot(S),
    Result = enterprise_oa_login_logic:link(
        Uid,
        Did,
        {Epoch, erlang:system_time(second) + 120},
        Complete#{<<"application_key">> => Wrong}
    ),
    ?assertEqual({error, invalid_request}, safe_result(Result)),
    ?assertEqual(Before, snapshot(S)),
    {ok, Session} = enterprise_oa_login_logic:link(
        Uid, Did, {Epoch, erlang:system_time(second) + 120}, Complete
    ),
    ?assertEqual(Uid, maps:get(<<"user_id">>, Session)),
    ?assertMatch(
        {ok, Uid, _, <<"tk">>, Did, _}, token_ds:decrypt_token(maps:get(<<"token">>, Session))
    ).

issue_deadline(S, Issue) ->
    ok = with_deadline(S, issued, Issue),
    {ok, Payload} = Issue(),
    Ticket = maps:get(<<"ticket">>, Payload),
    ?assertEqual(43, byte_size(Ticket)),
    ?assertEqual(60, maps:get(<<"expires_in">>, Payload)),
    Ctx = maps:get(oa_gap_context, S),
    ?assertMatch(
        [_],
        rows(
            maps:get(conn, S),
            <<
                "SELECT digest FROM enterprise_oa_login_ticket WHERE digest=$1 AND stage='issued' "
                "AND application_id=$2 AND credential_id=$3"
            >>,
            [
                crypto:hash(sha256, Ticket),
                maps:get(application_id, Ctx),
                maps:get(credential_id, Ctx)
            ]
        )
    ).

session_deadline(S, Ticket) ->
    Ext = intbe02_http_support:fixture(ext_alice, S),
    Uid = intbe02_http_support:fixture(alice, S),
    %% Original mapping seed reaches source_person through migration 176 trigger.
    ?assertEqual(
        [#{<<"user_id">> => Uid, <<"status">> => <<"active">>, <<"binding_retired">> => false}],
        rows(
            maps:get(conn, S),
            <<
                "SELECT user_id,status,binding_retired FROM enterprise_source_person "
                "WHERE application_id=$1 AND external_user_id=$2"
            >>,
            [maps:get(app_sso, S), Ext]
        )
    ),
    P = Ticket(Ext, <<"login">>, <<"oa-three-gap-session">>),
    ok = with_deadline(S, completed, fun() -> enterprise_oa_login_logic:exchange(P) end),
    {ok, Session} = enterprise_oa_login_logic:exchange(P),
    ?assertEqual(Uid, maps:get(<<"user_id">>, Session)),
    ?assertMatch(
        {ok, Uid, _, <<"tk">>, <<"oa-three-gap-session">>, _},
        token_ds:decrypt_token(maps:get(<<"token">>, Session))
    ),
    ?assertEqual(
        [#{<<"stage">> => <<"consumed">>}],
        rows(
            maps:get(conn, S),
            <<"SELECT stage FROM enterprise_oa_login_ticket WHERE digest=$1">>,
            [crypto:hash(sha256, maps:get(<<"ticket">>, P))]
        )
    ).

with_deadline(S, Phase, Call) ->
    C = maps:get(conn, S),
    Cred = maps:get(credential_id, maps:get(oa_gap_context, S)),
    [#{<<"expires_at">> := Original}] = rows(
        C,
        <<"SELECT expires_at FROM enterprise_application_credential WHERE id=$1">>,
        [Cred]
    ),
    try
        install_delay(C, Phase),
        exec(
            C,
            <<"UPDATE enterprise_application_credential SET expires_at=clock_timestamp()+interval '5 seconds' WHERE id=$1">>,
            [Cred]
        ),
        Before = snapshot(S),
        [#{<<"valid">> := true}] = rows(
            C,
            <<"SELECT expires_at>clock_timestamp() AS valid FROM enterprise_application_credential WHERE id=$1">>,
            [Cred]
        ),
        Result = Call(),
        %% Sequence advancement is nontransactional: proves the delayed audit was reached.
        ?assertEqual(
            [#{<<"last_value">> => 1, <<"is_called">> => true}],
            rows(C, <<"SELECT last_value,is_called FROM oa_three_gap_probe">>, [])
        ),
        [#{<<"expired">> := true}] = rows(
            C,
            <<"SELECT expires_at<=clock_timestamp() AS expired FROM enterprise_application_credential WHERE id=$1">>,
            [Cred]
        ),
        ?assertEqual({error, invalid_ticket}, safe_result(Result)),
        ?assertEqual(Before, snapshot(S))
    after
        remove_delay(C),
        exec(C, <<"UPDATE enterprise_application_credential SET expires_at=$2 WHERE id=$1">>, [
            Cred, Original
        ])
    end.

install_delay(C, Phase) ->
    Condition =
        case Phase of
            issued -> <<"NEW.action='oa.login_ticket_issued'">>;
            completed -> <<"NEW.action='oa.login_completed'">>
        end,
    exec(C, <<"CREATE SEQUENCE oa_three_gap_probe">>, []),
    exec(
        C,
        <<"CREATE FUNCTION oa_three_gap_delay() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF ",
            Condition/binary,
            " THEN PERFORM nextval('oa_three_gap_probe'); PERFORM pg_sleep(6); END IF; RETURN NEW; END $$">>,
        []
    ),
    exec(
        C,
        <<
            "CREATE TRIGGER oa_three_gap_delay BEFORE INSERT ON enterprise_audit_event "
            "FOR EACH ROW EXECUTE FUNCTION oa_three_gap_delay()"
        >>,
        []
    ).

remove_delay(C) ->
    exec(C, <<"DROP TRIGGER IF EXISTS oa_three_gap_delay ON enterprise_audit_event">>, []),
    exec(C, <<"DROP FUNCTION IF EXISTS oa_three_gap_delay()">>, []),
    exec(C, <<"DROP SEQUENCE IF EXISTS oa_three_gap_probe">>, []).

safe_result({error, Reason}) -> {error, Reason};
safe_result({ok, _}) -> {ok, secret_payload_withheld};
safe_result(_) -> unexpected_result_shape.

rows(C, Sql, Params) ->
    {ok, Result} = elib_pg:query(C, Sql, Params),
    Result.

exec(C, Sql, Params) ->
    case elib_pg:execute(C, Sql, Params) of
        {ok, _} -> ok;
        Other -> erlang:error({fixture_sql_failed, Other})
    end.
