-module(organization_preserve_b_personal_http_checks).
-export([setup/0, cleanup/1, run/3]).
-include_lib("eunit/include/eunit.hrl").
-define(H, intbe02_http_support).
-define(UID, 995011).
-define(PEER, 995012).
-define(DID, <<"c08-preserve-b-r1">>).
-define(MSG, <<"c08-preserve-personal-r1">>).
-define(TEXT, <<"Synthetic private C2C retained after organization exit">>).

setup() ->
    quiet(),
    ?assertEqual(undefined, whereis(imboy_domain_event)),
    ?assertEqual(undefined, whereis(imboy_cache)),
    try
        setup_marker()
    catch
        Class:Reason:Stack ->
            %% No H may exist if the original fixture fails before returning.
            %% Preserve unknown marker ownership for parent reconciliation, never scan/drop.
            cleanup_step(fun() -> stop_owned(imboy_cache, whereis(imboy_cache)) end),
            erlang:raise(Class, Reason, Stack)
    end.

setup_marker() ->
    H = ?H:setup_all(),
    try
        {ok, Bus} = imboy_domain_event:start_link(),
        ensure_archive_id(),
        Cache = whereis(imboy_cache),
        ?assert(is_pid(Cache)),
        ?assertEqual([], imboy_domain_event:which_subscribers()),
        H#{preserve_event_owner => Bus, preserve_cache_owner => Cache}
    catch
        Class:Reason:Stack ->
            cleanup_step(fun() ->
                cleanup(H#{
                    preserve_event_owner => whereis(imboy_domain_event),
                    preserve_cache_owner => whereis(imboy_cache)
                })
            end),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(H) ->
    Results = [
        cleanup_step(fun() -> ?H:teardown_all(H) end),
        cleanup_step(fun() -> inttest_marker_db:release(H) end),
        cleanup_step(fun() -> stop_owned(imboy_domain_event, maps:get(preserve_event_owner, H)) end),
        cleanup_step(fun() -> stop_owned(imboy_cache, maps:get(preserve_cache_owner, H)) end)
    ],
    io:format("C08_PRESERVE_CLEANUP ~p~n", [Results]),
    ?assertEqual([true, true, true, true], Results),
    quiet().

%% Same source registration recipe as the original cloud archive checks.
ensure_archive_id() ->
    case lists:member(msg_store, elib_tsid:registered()) of
        true -> ok;
        false -> ok = elib_tsid:register(msg_store)
    end.

cleanup_step(F) ->
    try
        ok = F(),
        true
    catch
        Class:_ ->
            io:format("C08_PRESERVE_CLEANUP_FAILED_CLASS ~p~n", [Class]),
            false
    end.

stop_owned(_, undefined) ->
    ok;
stop_owned(Name, Pid) when is_pid(Pid) ->
    ?assertEqual(Pid, whereis(Name)),
    Ref = erlang:monitor(process, Pid),
    case Name of
        imboy_domain_event -> gen_event:stop(Pid);
        imboy_cache -> gen_server:stop(Pid)
    end,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 -> erlang:error(owned_process_not_stopped)
    end,
    ?assertEqual(undefined, whereis(Name)).

run(H, Seed, Accept) ->
    quiet_with_bus(),
    C = maps:get(conn, H),
    Resources = create_a(C, Seed, Accept),
    W = maps:get(workspace_id, Resources),
    {Token, Headers, Epoch} = session(),
    Seq = archive(),
    BeforeProbes = snapshot(C),
    assert_session(Token, Epoch),
    assert_workspace(H, W, Headers),
    B = assert_workspace(H, 995201, Headers),
    Personal = assert_personal(H, Headers, Seq),
    anonymous_controls(H, Headers),
    ?assertEqual(BeforeProbes, snapshot(C)),
    Unrelated = preserved_rows(C),
    Exit = request(
        H,
        <<"POST">>,
        <<"/api/v1/organizations/995902/members/995011/offboard">>,
        #{},
        Headers
    ),
    #{<<"status">> := <<"removed">>} = success(Exit),
    ?assertEqual(Unrelated, preserved_rows(C)),
    assert_departure(C, Resources),
    AfterTransition = snapshot(C),
    assert_a_denied(H, W, Headers),
    ?assertEqual(B, assert_workspace(H, 995201, Headers)),
    ?assertEqual(Personal, assert_personal(H, Headers, Seq)),
    assert_session(Token, Epoch),
    ?assertEqual(AfterTransition, snapshot(C)),
    quiet_with_bus(),
    io:format("C08_PRESERVE_SCOPE committed_exit_same_DID_B_HTTP_private_C2C_read_only~n"),
    ok.

create_a(C, Seed, Accept) ->
    ok = ?H:sql_exec(C, <<
        "INSERT INTO organization(id,name,owner_id,status) "
        "VALUES(995902,'Synthetic preserve A',995021,'active')"
    >>),
    {ok, Resources, created} = workspace_ds:create_template(
        995021, 995902, <<"Synthetic preserve A resources">>, undefined
    ),
    ok = Seed(C, 995906),
    ?assertMatch({ok, #{already_accepted := false}}, Accept()),
    Resources.

session() ->
    {ok, Epoch} = elib_pg:with_tx(fun(C) ->
        {ok, _} = enterprise_oa_login_repo:device_tx(C, ?UID, ?DID, #{}),
        {ok, #{<<"epoch">> := E}} = enterprise_oa_login_repo:epoch_tx(C, ?UID),
        {ok, E}
    end),
    Session = token_ds:issue_device_session(?UID, ?DID, Epoch),
    Token = maps:get(<<"token">>, Session),
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>,
        <<"c08-r1">>,
        <<"synthetic.c08.preserve">>,
        <<"synthetic-c08-preserve-device-key">>
    ),
    Signed = #{
        <<"cos">> => <<"synthetic">>,
        <<"vsn">> => <<"c08-r1">>,
        <<"pkg">> => <<"synthetic.c08.preserve">>,
        <<"did">> => ?DID,
        <<"method">> => <<"sha256">>,
        <<"sign">> => elib_hasher:hmac_sha256(
            <<?DID/binary, "|c08-r1|synthetic|synthetic.c08.preserve">>,
            <<"synthetic-c08-preserve-device-key">>
        )
    },
    {Token, maps:merge(Signed, ?H:auth(Token)), Epoch}.

assert_session(Token, Epoch) ->
    ?assertMatch({ok, ?UID, _, <<"tk">>, ?DID, Epoch}, token_ds:decrypt_token(Token)),
    ?assertEqual({ok, ?UID, ?DID}, auth_ds:verify_token(<<"Bearer ", Token/binary>>)).

archive() ->
    Payload = jsone:encode(#{<<"body">> => ?TEXT}),
    {ok, StagingId, Seq} = msg_store_repo:stage_with_sequence(
        <<"c2c">>,
        ?MSG,
        <<"text">>,
        <<>>,
        null,
        Payload,
        ?UID,
        ?PEER,
        <<"2026-10-16T00:00:00Z">>,
        <<"2026-10-16T00:00:00Z">>,
        ?DID
    ),
    ?assert(is_integer(StagingId) andalso StagingId > 0),
    ?assert(is_integer(Seq) andalso Seq > 0),
    ?assertEqual({ok, Seq}, personal_message_order_repo:accepted_sequence(?UID, ?PEER, ?MSG)),
    ok = msg_archive_repo:archive(#{
        <<"type">> => <<"c2c">>,
        <<"msg_id">> => ?MSG,
        <<"msg_type">> => <<"text">>,
        <<"from_id">> => ?UID,
        <<"to_id">> => ?PEER,
        <<"conv_seq">> => Seq,
        <<"e2ee">> => null,
        <<"sender_did">> => ?DID,
        <<"payload">> => Payload,
        <<"created_at">> => <<"2026-10-16T00:00:00Z">>,
        <<"server_ts">> => <<"2026-10-16T00:00:00Z">>
    }),
    Seq.

assert_workspace(H, W, Headers) ->
    Payload = success(request(H, <<"GET">>, workspace_path(W), <<>>, Headers)),
    ?assertEqual(W, maps:get(<<"id">>, Payload)),
    ExpectedOrg =
        case W of
            995201 -> 995101;
            _ -> 995902
        end,
    ?assertEqual(ExpectedOrg, maps:get(<<"organization_id">>, Payload)),
    Payload.

assert_personal(H, Headers, Seq) ->
    P = success(request(H, <<"GET">>, history_path(), <<>>, Headers)),
    [M] = maps:get(<<"messages">>, P),
    ?assertEqual(?MSG, maps:get(<<"msg_id">>, M)),
    ?assertEqual(<<"995011">>, maps:get(<<"from">>, M)),
    ?assertEqual(<<"995012">>, maps:get(<<"to">>, M)),
    ?assertEqual(#{<<"body">> => ?TEXT}, maps:get(<<"payload">>, M)),
    ?assertEqual(null, maps:get(<<"e2ee">>, M)),
    ?assertEqual(Seq, maps:get(<<"conv_seq">>, M)),
    ?assertEqual(Seq, maps:get(<<"next_seq">>, P)),
    ?assertEqual(false, maps:get(<<"has_more">>, P)),
    ?assertEqual(msg_archive_repo:conv_key(<<"c2c">>, ?UID, ?PEER), maps:get(<<"conv_key">>, P)),
    P.

anonymous_controls(H, Headers) ->
    NoJwt = maps:remove(<<"authorization">>, Headers),
    lists:foreach(
        fun(Path) ->
            R = request(H, <<"GET">>, Path, <<>>, NoJwt),
            ?assertEqual(401, maps:get(status, R)),
            ?assertEqual(#{}, maps:get(<<"payload">>, maps:get(json, R)))
        end,
        [workspace_path(995201), history_path()]
    ).

assert_a_denied(H, W, Headers) ->
    R = request(H, <<"GET">>, workspace_path(W), <<>>, Headers),
    %% Ordinary business refusal uses HTTP200 and the exact business code403.
    ?assertEqual(200, maps:get(status, R)),
    ?assertEqual(403, maps:get(<<"code">>, maps:get(json, R))),
    ?assertEqual(#{}, maps:get(<<"payload">>, maps:get(json, R))).

assert_departure(C, #{workspace_id := W, group_id := G, channel_id := Ch}) ->
    Sql = <<
        "SELECT 'org' AS kind,count(*) AS n FROM organization_member "
        "WHERE organization_id=995902 AND user_id=995011 AND status='active' "
        "UNION ALL SELECT 'workspace',count(*) FROM workspace_member "
        "WHERE workspace_id=$1 AND user_id=995011 AND status='active' "
        "UNION ALL SELECT 'group',count(*) FROM group_member "
        "WHERE group_id=$2 AND user_id=995011 AND status=1 "
        "UNION ALL SELECT 'channel',count(*) FROM channel_subscription "
        "WHERE channel_id=$3 AND user_id=995011 AND status=1"
    >>,
    {ok, Rows} = elib_pg:query(C, Sql, [W, G, Ch]),
    ?assertEqual(4, length(Rows)),
    [?assertEqual(0, maps:get(<<"n">>, Row)) || Row <- Rows],
    ?assertEqual(
        [#{<<"status">> => 1}],
        rows(
            C,
            <<"SELECT status FROM \"user\" WHERE id=995011">>
        )
    ).

snapshot(C) ->
    Tables = [
        <<"\"user\"">>,
        <<"user_device">>,
        <<"user_auth_epoch">>,
        <<"enterprise_source_person">>,
        <<"enterprise_external_identity">>,
        <<"enterprise_oa_login_ticket">>,
        <<"enterprise_directory_membership_source">>,
        <<"organization_member">>,
        <<"workspace_member">>,
        <<"group_member">>,
        <<"group_member_generation">>,
        <<"channel_subscription">>,
        <<"organization_invitation">>,
        <<"enterprise_audit_event">>,
        <<"msg_store_staging">>,
        <<"msg_store">>,
        <<"msg_store_seq">>
    ],
    [
        {T,
            rows(
                C, <<"SELECT to_jsonb(t) AS row FROM ", T/binary, " t ORDER BY to_jsonb(t)::text">>
            )}
     || T <- Tables
    ].

preserved_rows(C) ->
    Queries = [
        <<"SELECT to_jsonb(t) AS row FROM \"user\" t WHERE id=995011">>,
        <<"SELECT to_jsonb(t) AS row FROM user_device t WHERE user_id=995011 ORDER BY id">>,
        <<"SELECT to_jsonb(t) AS row FROM user_auth_epoch t WHERE user_id=995011">>,
        <<"SELECT to_jsonb(t) AS row FROM organization_member t WHERE organization_id=995101 AND user_id=995011">>,
        <<"SELECT to_jsonb(t) AS row FROM workspace_member t WHERE workspace_id=995201 AND user_id=995011">>,
        <<"SELECT to_jsonb(t) AS row FROM enterprise_source_person t ORDER BY application_id,external_user_id">>,
        <<"SELECT to_jsonb(t) AS row FROM enterprise_external_identity t ORDER BY id">>,
        <<"SELECT to_jsonb(t) AS row FROM msg_store_staging t ORDER BY id">>,
        <<"SELECT to_jsonb(t) AS row FROM msg_store t ORDER BY id">>,
        <<"SELECT to_jsonb(t) AS row FROM msg_store_seq t ORDER BY conv_key">>
    ],
    [rows(C, Q) || Q <- Queries].

request(H, Method, Path, Body, Headers) ->
    quiet_with_bus(),
    ?assertEqual(maps:get(preserve_event_owner, H), whereis(imboy_domain_event)),
    ?assertEqual(maps:get(preserve_cache_owner, H), whereis(imboy_cache)),
    R = ?H:http(maps:get(port, H), Method, Path, Body, Headers),
    Json = jsone:decode(maps:get(body, R)),
    io:format(
        "C08_PRESERVE_HTTP ~s ~s status=~p code=~p~n",
        [Method, Path, maps:get(status, R), maps:get(<<"code">>, Json, undefined)]
    ),
    R#{json => Json}.

success(R) ->
    ?assertEqual(200, maps:get(status, R)),
    #{<<"code">> := 0, <<"payload">> := P} = maps:get(json, R),
    P.

workspace_path(W) -> <<"/api/v1/workspaces/", (integer_to_binary(W))/binary>>.
history_path() ->
    <<"/api/v1/msg/history?chat_type=c2c&peer_id=995012&after_seq=0&limit=100&did=", ?DID/binary>>.

rows(C, Q) ->
    {ok, Rows} = elib_pg:query(C, Q, []),
    Rows.

quiet() ->
    ?assertEqual(undefined, whereis(imboy_cache_sync)),
    ?assertEqual(undefined, whereis(imboy_sup)),
    ?assertNot(lists:keymember(imboy, 1, application:which_applications())).

quiet_with_bus() ->
    quiet(),
    ?assertEqual([], imboy_domain_event:which_subscribers()).
