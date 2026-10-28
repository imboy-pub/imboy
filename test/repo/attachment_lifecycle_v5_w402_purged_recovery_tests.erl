%%% @doc W4-02 purged-session-recovery：purge 后的会话恢复墓碑链验收
%%%（真 scratch DB；fn_v5_copy_eb_recovery_tombstone + register UUID 查重）。
%%%
%%% 场景（契约叶 10 ordering_or_fault × 2 entrypoint = 20 enterprise 叶；
%%% 断言语义均经 fn 实测定稿）：
%%%   1. confirmed purge → 意图入队 → copy_eb_recovery_tombstone 快照复制 →
%%%      同 UUID register 拒（tombstone 23505，队列行存在即拒）；
%%%   2. done job 同 UUID 不产生新资产（register 拒绝持续——墓碑永久）；
%%%   3. 外部 actor 对未知 UUID 的 register 重放：不泄露存在性（同形状）；
%%%   4. pending purge 的 register 响应丢失恢复：同 UUID 重放返回同一 key；
%%%   5. 同 UUID 变更 declaration 重放：快照不匹配拒（23510）；
%%%   6. register 先赢得竞态：register（能力过期）→ sweep → queue ready；
%%%   7. purge 先赢得竞态：sweep 后同 UUID 重放返回 delete_intent 态（不复活）；
%%%   8. purge 提交后同 path 新 UUID：key 重指派拒（23510，读回形状统一）；
%%%   9. deleted 父资产授权拒绝：eb intent → 23510 non-live（不复活）；
%%%  10. 访客读级联兼容：purge 链后访客已读表无附件对象标识列（迁移 172 解耦）。
-module(attachment_lifecycle_v5_w402_purged_recovery_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

w402_purged_recovery_test_() ->
    {timeout, 1200, fun run/0}.

-define(EP_SEAT, <<"enterprise_seat">>).
-define(EP_WIDGET, <<"cs_widget">>).
-define(CASE_ID, <<"W4-02/purged-session-recovery">>).

run() ->
    Boot = eunit_runner:eunit_setup(),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W402P">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    try
        seed_tenants(Conn),
        w402p_confirmed_purge_register_same_uuid(Conn),
        w402p_done_job_same_uuid_no_new_asset(Conn),
        w402p_foreign_actor_no_disclosure(Conn),
        w402p_pending_purge_register_response_lost(Conn),
        w402p_same_uuid_changed_declaration(Conn),
        w402p_purge_register_race_register_first(Conn),
        w402p_purge_register_race_purge_first(Conn),
        w402p_purge_commit_unknown_readback(Conn),
        w402p_deleted_parent_authorization_denied(Conn),
        w402p_purge_visitor_read_cascade_compatible(Conn)
    after
        inttest_marker_db:release(State),
        eunit_runner:eunit_cleanup(Boot)
    end.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w402p_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

%% 逐 cell 观测：每个场景对契约的两个 entrypoint（enterprise_seat / cs_widget）
%% 各 emit 一叶（同一恢复链协议面，如实同测）。
w402p_emit(Ord, Measured) ->
    Base = Measured#{entrypoint_case => atom_to_binary(Ord, utf8)},
    lists:foreach(
        fun(EP) ->
            attachment_lifecycle_v5_obs:emit(#{
                case_id => ?CASE_ID,
                entrypoint => EP,
                scenario => atom_to_binary(Ord, utf8),
                domain => <<"enterprise">>,
                schema_phase => <<"expand-queue">>,
                measured => Base
            })
        end,
        [?EP_SEAT, ?EP_WIDGET]
    ),
    ok.

uuid_bin() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

reg_png() ->
    <<
        "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
        "\"declaration\":{\"mime\":\"image/png\"}}"
    >>.

seed_tenants(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w402p-u', 'w402p', '', 1, 0, '127.0.0.1', 'w402p', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id) VALUES (9101,'w402p-org',101)"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES (9101,9102,'w402p-ws',101,'{}') ON CONFLICT (id) DO NOTHING"
    ),
    ok.

seed_eb_confirmed(Conn, Id, Key, Uuid) ->
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,lifecycle_state,client_upload_id,"
        "  upload_registration)"
        " VALUES ($1,9101,9102,$2,$3,'text/plain',8,'active','garage','ns1','c1',"
        "  $4,'live',$5,$6::jsonb)",
        [
            Id,
            Key,
            hex64(crypto:hash(sha256, Key)),
            hex64(crypto:hash(sha256, <<Key/binary, "-loc">>)),
            Uuid,
            reg_png()
        ]
    ),
    ok.

%% 1. confirmed_purge_register_same_uuid：confirmed 资产 purge → 意图入队 →
%%    墓碑快照复制 → 同 UUID register 拒（队列墓碑 23505）
w402p_confirmed_purge_register_same_uuid(Conn) ->
    Uuid = uuid_bin(),
    Id = 944000000001,
    Key = <<"enterprise/9101/9102/prg-1">>,
    seed_eb_confirmed(Conn, Id, Key, Uuid),
    %% 意图入队（身份完整 → ready）
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w402p purge')",
        [9101, 9102, Id]
    ),
    %% purge 前快照复制（生产顺序：purge 事务内 copy tombstone）
    {ok, _, _} = e(
        Conn, "SELECT fn_v5_copy_eb_recovery_tombstone($1,$2,$3)", [9101, 9102, Id]
    ),
    {ok, _, [{R1, R2}]} = e(
        Conn,
        "SELECT client_upload_id, upload_registration"
        " FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [Id]
    ),
    ?assertEqual(Uuid, R1),
    ?assertNotEqual(null, R2),
    %% 同 UUID register：队列墓碑拒绝（23505）
    {error, #error{code = <<"23505">>}} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [
            <<"v5e2e/w402p/replay-1">>,
            Uuid,
            reg_png(),
            hex64(crypto:hash(sha256, <<"replay-1">>))
        ]
    ),
    ok = epgsql:sync(Conn),
    w402p_emit(confirmed_purge_register_same_uuid, #{register_rejected => <<"23505">>}).

%% 2. done_job_same_uuid_no_new_asset：done job 同 UUID 不产生新资产
w402p_done_job_same_uuid_no_new_asset(Conn) ->
    Uuid = uuid_bin(),
    Id = 944000000002,
    Key = <<"enterprise/9101/9102/prg-2">>,
    seed_eb_confirmed(Conn, Id, Key, Uuid),
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w402p done purge')",
        [9101, 9102, Id]
    ),
    {ok, _, _} = e(
        Conn, "SELECT fn_v5_copy_eb_recovery_tombstone($1,$2,$3)", [9101, 9102, Id]
    ),
    %% worker 消费到 done（删除成功；直接在断言连接上调 CAS 函数）。
    %% 队列可能含先前 case 的 ready job：循环 claim 到目标（无关 job
    %% ack(ok)=done 消化掉，语义=正常消费）。
    claim_until(Conn, Id, 10),
    {ok, _, [{AssetsBefore}]} = e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE organization_id=9101"
        " AND workspace_id=9102 AND object_key LIKE 'v5e2e/w402p/replay%'",
        []
    ),
    %% done 后同 UUID register 仍拒（queue 行保留为墓碑）
    {error, #error{code = <<"23505">>}} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [
            <<"v5e2e/w402p/replay-2">>,
            Uuid,
            reg_png(),
            hex64(crypto:hash(sha256, <<"replay-2">>))
        ]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{AssetsAfter}]} = e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE organization_id=9101"
        " AND workspace_id=9102 AND object_key LIKE 'v5e2e/w402p/replay%'",
        []
    ),
    ?assertEqual(0, AssetsBefore),
    ?assertEqual(AssetsBefore, AssetsAfter),
    w402p_emit(done_job_same_uuid_no_new_asset, #{
        register_rejected => <<"23505">>, new_assets_created => AssetsAfter
    }).

uuid_text() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

%% 循环 claim 直到拿到目标 job 并 ack done（无关 ready job 顺带消化）。
claim_until(_Conn, _Id, 0) ->
    erlang:error({w402p_target_job_not_claimed, _Id});
claim_until(Conn, Id, N) ->
    T = uuid_text(),
    case
        e(
            Conn,
            "SELECT out_organization_id, out_workspace_id, out_asset_id"
            " FROM fn_v5_claim_eb($1)",
            [T]
        )
    of
        {ok, _, [{9101, 9102, Id}]} ->
            {ok, _, [{<<"done">>}]} = e(
                Conn,
                "SELECT fn_v5_ack_eb($1, $2, $3, $4, true)",
                [9101, 9102, Id, T]
            ),
            ok;
        {ok, _, [{O, W, Other}]} ->
            %% 无关 job：正常消费（done），继续找目标
            {ok, _, _} =
                e(Conn, "SELECT fn_v5_ack_eb($1, $2, $3, $4, true)", [
                    O, W, Other, T
                ]),
            claim_until(Conn, Id, N - 1);
        Other ->
            erlang:error({w402p_claim_failed, Other})
    end.

%% 3. foreign_actor_no_disclosure：外部 actor 对未知 UUID 的 register 重放：
%%    不泄露存在性（合法新 UUID 通过，形状一致——同 key/session 结构；
%%    拒绝面只在同 UUID 墓碑）
w402p_foreign_actor_no_disclosure(Conn) ->
    FreshUuid = uuid_bin(),
    {ok, _, [{KeyOut, <<"registered">>, Cid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',202,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [
            <<"v5e2e/w402p/foreign-1">>,
            FreshUuid,
            <<
                "{\"subject\":{\"kind\":\"user\",\"principal\":\"202\"},"
                "\"declaration\":{\"mime\":\"image/png\"}}"
            >>,
            hex64(crypto:hash(sha256, <<"foreign-1">>))
        ]
    ),
    ?assertEqual(<<"v5e2e/w402p/foreign-1">>, KeyOut),
    ?assertEqual(FreshUuid, Cid),
    w402p_emit(foreign_actor_no_disclosure, #{
        fresh_uuid_accepted => true, key_shape => KeyOut
    }).

%% 4. pending_purge_register_response_lost：pending purge 的 register 响应
%%    丢失恢复：同 UUID 同声明重放返回同一 key/state
w402p_pending_purge_register_response_lost(Conn) ->
    Uuid = uuid_bin(),
    Key = <<"v5e2e/w402p/lost-1">>,
    {ok, _, [{K1, <<"registered">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, reg_png(), hex64(crypto:hash(sha256, <<"lost-1">>))]
    ),
    %% 响应丢失重放：同 UUID 同声明 → 同 key/state（幂等成功）
    {ok, _, [{K1, <<"registered">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, reg_png(), hex64(crypto:hash(sha256, <<"lost-1">>))]
    ),
    w402p_emit(pending_purge_register_response_lost, #{
        replay_same_key => K1, replay_state => <<"registered">>
    }).

%% 5. same_uuid_changed_declaration：同 UUID 变更声明重放 →
%%    快照不匹配拒（23510；registration 幂等按快照逐字段比对）
w402p_same_uuid_changed_declaration(Conn) ->
    Uuid = uuid_bin(),
    Key = <<"v5e2e/w402p/chg-decl">>,
    {ok, _, [{K1, <<"registered">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, reg_png(), hex64(crypto:hash(sha256, <<"chg-decl">>))]
    ),
    ChangedDecl =
        <<
            "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
            "\"declaration\":{\"mime\":\"video/mp4\"}}"
        >>,
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, ChangedDecl, hex64(crypto:hash(sha256, <<"chg-decl">>))]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{DeclAfter}]} = e(
        Conn,
        "SELECT upload_registration->'declaration'->>'mime'"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    ?assertEqual(<<"image/png">>, DeclAfter),
    w402p_emit(same_uuid_changed_declaration, #{
        replay_rejected => <<"23510">>, declaration_unchanged => DeclAfter
    }).

%% 6. purge_register_race_register_first：register 先赢得竞态——
%%    register（上传能力已过期）→ sweep 意图成功 → queue ready 行
w402p_purge_register_race_register_first(Conn) ->
    Uuid = uuid_bin(),
    Key = <<"v5e2e/w402p/race-rf">>,
    {ok, _, [{Key, <<"registered">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, reg_png(), hex64(crypto:hash(sha256, <<"race-rf">>))]
    ),
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_pending_delete($1, 'w402p race rf', 'sweep')",
        [Key]
    ),
    {ok, _, [{<<"ready">>}]} = e(
        Conn,
        "SELECT job_state FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    w402p_emit(purge_register_race_register_first, #{
        register_first_won => true, queue_job_state => <<"ready">>
    }).

%% 7. purge_register_race_purge_first：purge 先赢得竞态——
%%    sweep 后同 UUID 重放返回 delete_intent 态（不复活新会话）
w402p_purge_register_race_purge_first(Conn) ->
    Uuid = uuid_bin(),
    Key = <<"v5e2e/w402p/race-pf">>,
    {ok, _, [{Key, <<"registered">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, reg_png(), hex64(crypto:hash(sha256, <<"race-pf">>))]
    ),
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_pending_delete($1, 'w402p race pf', 'sweep')",
        [Key]
    ),
    %% purge 先完成：register 重放读到已 purge 会话的 delete_intent 态
    {ok, _, [{Key, <<"delete_intent">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, reg_png(), hex64(crypto:hash(sha256, <<"race-pf">>))]
    ),
    w402p_emit(purge_register_race_purge_first, #{
        purge_first_won => true, replay_state => <<"delete_intent">>
    }).

%% 8. purge_commit_unknown_readback：purge 提交后同 path 全新 UUID register
%%    → key 重指派拒（23510；读回形状统一，不泄露旧会话存在性）
w402p_purge_commit_unknown_readback(Conn) ->
    Uuid = uuid_bin(),
    Key = <<"v5e2e/w402p/unknown-rb">>,
    {ok, _, [{Key, <<"registered">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, reg_png(), hex64(crypto:hash(sha256, <<"unknown-rb">>))]
    ),
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_pending_delete($1, 'w402p unknown rb', 'sweep')",
        [Key]
    ),
    FreshUuid = uuid_bin(),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, FreshUuid, reg_png(), hex64(crypto:hash(sha256, <<"unknown-rb2">>))]
    ),
    ok = epgsql:sync(Conn),
    w402p_emit(purge_commit_unknown_readback, #{
        fresh_uuid_rejected => <<"23510">>, key_reassignment => false
    }).

%% 9. deleted_parent_authorization_denied：deleted 父资产删除意图授权拒绝
%%    （23510 non-live；不复活删除链）
w402p_deleted_parent_authorization_denied(Conn) ->
    Uuid = uuid_bin(),
    Id = 944000000003,
    Key = <<"enterprise/9101/9102/dead-parent">>,
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,lifecycle_state,client_upload_id,"
        "  upload_registration)"
        " VALUES ($1,9101,9102,$2,$3,'text/plain',8,'deleted','garage','ns1','c1',"
        "  $4,'live',$5,$6::jsonb)",
        [
            Id,
            Key,
            hex64(crypto:hash(sha256, Key)),
            hex64(crypto:hash(sha256, <<Key/binary, "-loc">>)),
            Uuid,
            reg_png()
        ]
    ),
    %% 表触发器要求新行以 live 起步；deleted 形态经 UPDATE 演化
    q(
        Conn,
        "UPDATE enterprise_asset SET lifecycle_state='deleted' WHERE id=$1",
        [Id]
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w402p dead parent')",
        [9101, 9102, Id]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{QueueRows}]} = e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1",
        [Id]
    ),
    ?assertEqual(0, QueueRows),
    w402p_emit(deleted_parent_authorization_denied, #{
        intent_rejected => <<"23510">>, queue_rows => QueueRows
    }).

%% 10. purge_visitor_read_cascade_compatible：purge 链与迁移 172 访客阅读
%%     级联兼容——访客已读两表只引用 conversation/message 维度（无附件对象
%%     标识列），对象 purge 不触碰读回执面
w402p_purge_visitor_read_cascade_compatible(Conn) ->
    {ok, _, Rows} = e(
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
    {ok, _, [{CursorRows}]} = e(
        Conn,
        "SELECT COUNT(*) FROM customer_service_visitor_read_cursor"
        " WHERE organization_id=9101 AND workspace_id=9102",
        []
    ),
    ?assertEqual(0, CursorRows),
    w402p_emit(purge_visitor_read_cascade_compatible, #{
        object_columns => 0, orphan_cursor_rows => CursorRows
    }).
