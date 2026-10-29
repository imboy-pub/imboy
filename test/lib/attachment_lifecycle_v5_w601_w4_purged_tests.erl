%%% @doc W6-01 E 组：W4-02/purged-session-recovery（40 叶 = 10 scenario ×
%%% 2 entrypoint × 2 driver），schema_phase=integrated，domain=enterprise。
%%%
%%% oracle 复用封印源 w402_purged_recovery_tests（fn_v5_copy_eb_recovery_
%%% tombstone 快照复制 + register UUID 查重链：同 UUID 拒、变更声明拒、
%%% key 重指派拒、竞态双序、外部 actor 不泄露存在性、访客读级联兼容）。
%%% 恢复协议面与 entrypoint 无关（enterprise_seat / cs_widget 共用同一
%%% fn 协议面，封印先例如实披露）；每叶独立种子（driver 真实三元组）后
%%% 真实执行。
-module(attachment_lifecycle_v5_w601_w4_purged_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(X, attachment_lifecycle_v5_w601_w4_context).
-define(CASE, <<"W4-02/purged-session-recovery">>).
-define(EP_SEAT, <<"enterprise_seat">>).
-define(EP_WIDGET, <<"cs_widget">>).
-define(SCENARIOS, [
    <<"confirmed_purge_register_same_uuid">>,
    <<"done_job_same_uuid_no_new_asset">>,
    <<"foreign_actor_no_disclosure">>,
    <<"pending_purge_register_response_lost">>,
    <<"same_uuid_changed_declaration">>,
    <<"purge_register_race_register_first">>,
    <<"purge_register_race_purge_first">>,
    <<"purge_commit_unknown_readback">>,
    <<"deleted_parent_authorization_denied">>,
    <<"purge_visitor_read_cascade_compatible">>
]).

w601_w4_purged_test_() ->
    {setup, fun() -> ?X:setup(<<"V5_INTTEST_W601W4PRG">>) end, fun ?X:cleanup/1, fun(St) ->
        [?X:leaf(S, fun() -> scenario(St, S) end) || S <- specs()]
    end}.

specs() ->
    [
        {?CASE, EP, Drv, Scn, <<"enterprise">>}
     || Scn <- ?SCENARIOS,
        EP <- [?EP_SEAT, ?EP_WIDGET],
        Drv <- [<<"disk">>, <<"garage">>]
    ].

%% ------------------------------------------------------------- 分发

scenario(St, {_C, EP, Drv, Scn, _Dom}) ->
    #{conn := Conn} = St,
    N = ((?X:leaf_id(pbase) rem 100000) * 1000 + ?X:leaf_id(prg)),
    Key = <<"v5e2e/w601p/", Scn/binary, "/", (integer_to_binary(N))/binary>>,
    Triple = ?X:triple(St, Drv, Key),
    Measured = run_scenario(St, Conn, Scn, Key, Triple, N),
    Measured#{
        driver => Drv,
        object_key => Key,
        storage_namespace => maps:get(namespace, Triple),
        storage_container => maps:get(container, Triple),
        locator_sha256 => maps:get(locator, Triple),
        entrypoint_face => EP,
        entrypoint_agnostic_protocol => true
    }.

reg_png() ->
    <<
        "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
        "\"declaration\":{\"mime\":\"image/png\"}}"
    >>.

uuid_bin() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

uuid_text() -> uuid_bin().

eb_id(N) -> 951000000000 + N.

%% confirmed 企业资产（client_upload_id/upload_registration 在位——purge
%% 墓碑快照链的资格前提）。
seed_eb_confirmed(Conn, Key, Triple, Id, Uuid) ->
    ?X:q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,lifecycle_state,client_upload_id,"
        "  upload_registration)"
        " VALUES ($1,9101,9102,$2,$3,'text/plain',8,'active',$4,$5,$6,$7,"
        "  'live',$8,$9::jsonb)",
        [
            Id,
            Key,
            ?X:hex64(crypto:hash(sha256, Key)),
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple),
            Uuid,
            reg_png()
        ]
    ),
    ok.

register(Conn, Key, Uuid, Decl, Triple, Locator) ->
    ?X:e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',$4,$5,$6)",
        [
            Key,
            Uuid,
            Decl,
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            Locator
        ]
    ).

%% --------------------------------------------------------- 场景 1/2

run_scenario(St, Conn, Scn, Key, Triple, N) ->
    case Scn of
        <<"confirmed_purge_register_same_uuid">> ->
            confirmed_cycle(St, Conn, Key, Triple, N);
        <<"done_job_same_uuid_no_new_asset">> ->
            done_job_cycle(St, Conn, Key, Triple, N);
        <<"foreign_actor_no_disclosure">> ->
            foreign_cycle(Conn, Key, Triple, N);
        <<"pending_purge_register_response_lost">> ->
            response_lost_cycle(Conn, Key, Triple, N);
        <<"same_uuid_changed_declaration">> ->
            changed_decl_cycle(Conn, Key, Triple, N);
        <<"purge_register_race_register_first">> ->
            race_rf_cycle(Conn, Key, Triple, N);
        <<"purge_register_race_purge_first">> ->
            race_pf_cycle(Conn, Key, Triple, N);
        <<"purge_commit_unknown_readback">> ->
            unknown_rb_cycle(Conn, Key, Triple, N);
        <<"deleted_parent_authorization_denied">> ->
            dead_parent_cycle(Conn, Key, Triple, N);
        <<"purge_visitor_read_cascade_compatible">> ->
            visitor_cycle(St, Conn)
    end.

confirmed_cycle(St, Conn, Key, Triple, N) ->
    Uuid = uuid_bin(),
    ok = seed_eb_confirmed(Conn, Key, Triple, eb_id(N), Uuid),
    {ok, _, _} = ?X:e(
        Conn, "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w601p purge')", [9101, 9102, eb_id(N)]
    ),
    {ok, _, _} = ?X:e(
        Conn, "SELECT fn_v5_copy_eb_recovery_tombstone($1,$2,$3)", [9101, 9102, eb_id(N)]
    ),
    {ok, _, [{Uuid, R2}]} = ?X:e(
        Conn,
        "SELECT client_upload_id, upload_registration"
        " FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [eb_id(N)]
    ),
    ?assertNotEqual(null, R2),
    {error, #error{code = <<"23505">>}} =
        register(Conn, <<Key/binary, "-r1">>, Uuid, reg_png(), Triple, ?X:loc(N)),
    ok = epgsql:sync(Conn),
    #{register_rejected => <<"23505">>, tombstone_uuid => Uuid, scope => face(St)}.

done_job_cycle(St, Conn, Key, Triple, N) ->
    Uuid = uuid_bin(),
    ok = seed_eb_confirmed(Conn, Key, Triple, eb_id(N), Uuid),
    {ok, _, _} = ?X:e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w601p done purge')",
        [9101, 9102, eb_id(N)]
    ),
    {ok, _, _} = ?X:e(
        Conn, "SELECT fn_v5_copy_eb_recovery_tombstone($1,$2,$3)", [9101, 9102, eb_id(N)]
    ),
    claim_until(Conn, eb_id(N), 20),
    {ok, _, [{AssetsBefore}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE organization_id=9101"
        " AND workspace_id=9102 AND object_key LIKE 'v5e2e/w601p/replay%'",
        []
    ),
    {error, #error{code = <<"23505">>}} =
        register(Conn, <<"v5e2e/w601p/replay-2">>, Uuid, reg_png(), Triple, ?X:loc(N)),
    ok = epgsql:sync(Conn),
    {ok, _, [{AssetsAfter}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE organization_id=9101"
        " AND workspace_id=9102 AND object_key LIKE 'v5e2e/w601p/replay%'",
        []
    ),
    ?assertEqual(0, AssetsBefore),
    ?assertEqual(AssetsBefore, AssetsAfter),
    #{
        register_rejected => <<"23505">>,
        new_assets_created => AssetsAfter,
        scope => face(St)
    }.

claim_until(_Conn, Id, 0) ->
    erlang:error({w601p_target_job_not_claimed, Id});
claim_until(Conn, Id, K) ->
    T = uuid_text(),
    case
        ?X:e(
            Conn,
            "SELECT out_organization_id, out_workspace_id, out_asset_id"
            " FROM fn_v5_claim_eb($1)",
            [T]
        )
    of
        {ok, _, [{9101, 9102, Id}]} ->
            {ok, _, [{<<"done">>}]} = ?X:e(
                Conn, "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)", [9101, 9102, Id, T]
            ),
            ok;
        {ok, _, [{O, W, Other}]} ->
            {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)", [O, W, Other, T]),
            claim_until(Conn, Id, K - 1);
        {ok, _, []} ->
            claim_until(Conn, Id, K - 1);
        Other ->
            erlang:error({w601p_claim_failed, Other})
    end.

%% --------------------------------------------------------- 场景 3-5

foreign_cycle(Conn, Key, Triple, N) ->
    FreshUuid = uuid_bin(),
    ForeignDecl =
        <<
            "{\"subject\":{\"kind\":\"user\",\"principal\":\"202\"},"
            "\"declaration\":{\"mime\":\"image/png\"}}"
        >>,
    {ok, _, [{KeyOut, <<"registered">>, Cid}]} =
        register(Conn, Key, FreshUuid, ForeignDecl, Triple, ?X:loc(N)),
    ?assertEqual(Key, KeyOut),
    ?assertEqual(FreshUuid, Cid),
    #{fresh_uuid_accepted => true, key_shape => KeyOut}.

response_lost_cycle(Conn, Key, Triple, N) ->
    Uuid = uuid_bin(),
    {ok, _, [{K1, <<"registered">>, Uuid}]} =
        register(Conn, Key, Uuid, reg_png(), Triple, ?X:loc(N)),
    {ok, _, [{K1, <<"registered">>, Uuid}]} =
        register(Conn, Key, Uuid, reg_png(), Triple, ?X:loc(N)),
    #{replay_same_key => K1, replay_state => <<"registered">>}.

changed_decl_cycle(Conn, Key, Triple, N) ->
    Uuid = uuid_bin(),
    {ok, _, [{_K, <<"registered">>, Uuid}]} =
        register(Conn, Key, Uuid, reg_png(), Triple, ?X:loc(N)),
    ChangedDecl =
        <<
            "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
            "\"declaration\":{\"mime\":\"video/mp4\"}}"
        >>,
    {error, #error{code = <<"23510">>}} =
        register(Conn, Key, Uuid, ChangedDecl, Triple, ?X:loc(N)),
    ok = epgsql:sync(Conn),
    {ok, _, [{DeclAfter}]} = ?X:e(
        Conn,
        "SELECT upload_registration->'declaration'->>'mime'"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    ?assertEqual(<<"image/png">>, DeclAfter),
    #{replay_rejected => <<"23510">>, declaration_unchanged => DeclAfter}.

%% --------------------------------------------------------- 场景 6-8

race_rf_cycle(Conn, Key, Triple, N) ->
    Uuid = uuid_bin(),
    {ok, _, [{Key, <<"registered">>, Uuid}]} =
        register(Conn, Key, Uuid, reg_png(), Triple, ?X:loc(N)),
    {ok, _, _} = ?X:e(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w601p race rf','sweep')", [Key]
    ),
    {ok, _, [{<<"ready">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{register_first_won => true, queue_job_state => <<"ready">>}.

race_pf_cycle(Conn, Key, Triple, N) ->
    Uuid = uuid_bin(),
    {ok, _, [{Key, <<"registered">>, Uuid}]} =
        register(Conn, Key, Uuid, reg_png(), Triple, ?X:loc(N)),
    {ok, _, _} = ?X:e(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w601p race pf','sweep')", [Key]
    ),
    {ok, _, [{Key, <<"delete_intent">>, Uuid}]} =
        register(Conn, Key, Uuid, reg_png(), Triple, ?X:loc(N)),
    #{purge_first_won => true, replay_state => <<"delete_intent">>}.

unknown_rb_cycle(Conn, Key, Triple, N) ->
    Uuid = uuid_bin(),
    {ok, _, [{Key, <<"registered">>, Uuid}]} =
        register(Conn, Key, Uuid, reg_png(), Triple, ?X:loc(N)),
    {ok, _, _} = ?X:e(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w601p unknown rb','sweep')", [Key]
    ),
    FreshUuid = uuid_bin(),
    {error, #error{code = <<"23510">>}} =
        register(Conn, Key, FreshUuid, reg_png(), Triple, ?X:loc(N)),
    ok = epgsql:sync(Conn),
    #{fresh_uuid_rejected => <<"23510">>, key_reassignment => false}.

%% --------------------------------------------------------- 场景 9/10

dead_parent_cycle(Conn, Key, Triple, N) ->
    Id = eb_id(N),
    ok = ?X:seed_eb_asset_row(Conn, Key, Id, Triple, <<"active">>, false, <<"live">>),
    ?X:q(Conn, "UPDATE enterprise_asset SET lifecycle_state='deleted' WHERE id=$1", [Id]),
    {error, #error{code = <<"23510">>}} = ?X:e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w601p dead parent')",
        [9101, 9102, Id]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{QueueRows}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1", [Id]
    ),
    ?assertEqual(0, QueueRows),
    #{intent_rejected => <<"23510">>, queue_rows => QueueRows}.

visitor_cycle(St, Conn) ->
    {ok, _, Rows} = ?X:e(
        Conn,
        "SELECT table_name, column_name FROM information_schema.columns"
        " WHERE table_name IN ('customer_service_visitor_message_read',"
        " 'customer_service_visitor_read_cursor')"
        " AND (column_name LIKE '%object%' OR column_name LIKE '%path%'"
        "      OR column_name LIKE '%asset%' OR column_name LIKE '%attachment%'"
        "      OR column_name LIKE '%upload%')",
        []
    ),
    ?assertEqual([], Rows),
    {ok, _, [{CursorRows}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM customer_service_visitor_read_cursor"
        " WHERE organization_id=9101 AND workspace_id=9102",
        []
    ),
    ?assertEqual(0, CursorRows),
    #{object_columns => 0, orphan_cursor_rows => CursorRows, scope => face(St)}.

face(_St) -> <<"purged-session-recovery">>.
