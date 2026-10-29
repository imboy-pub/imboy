%%% @doc W6-01 E 组：W4-01 三组竞态（reference-gc-race 16 + hold-gc-race 16 +
%%% confirm-gc-race 8 叶），schema_phase=integrated，2 域 × 2 driver。
%%%
%%% oracle 复用封印源：reference/confirm 镜像 pg_tests（W3-01 bind 先提交 =>
%%% 意图拒绝；intent 先提交 => 绑定 0 行/confirm 重放 23510；confirm 先提交 =>
%%% 意图落败），hold 镜像 hold_protocol_tests（hold 先提交 => 拒绝；intent 先
%%% 提交 => 治理不追认/already_deleting）。每叶独立种子（driver 真实三元组 +
%%% 探针字节）后真实执行；竞态用第二连接在行锁上真实阻塞。
-module(attachment_lifecycle_v5_w601_w4_race_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(C_REF, <<"W4-01/reference-gc-race">>).
-define(C_HOLD, <<"W4-01/hold-gc-race">>).
-define(C_CONF, <<"W4-01/confirm-gc-race">>).
-define(X, attachment_lifecycle_v5_w601_w4_context).
-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).
-define(ENT_ORG, 9101).
-define(ENT_WS, 9102).
-define(REF_SCENARIOS, [
    <<"bind_commits_first">>,
    <<"direct_dml_bind_commits_first">>,
    <<"direct_dml_intent_commits_first">>,
    <<"intent_commits_first">>
]).
-define(HOLD_SCENARIOS, [
    <<"hold_commits_first">>,
    <<"intent_before_asset_hold">>,
    <<"intent_before_retention_extension">>,
    <<"workspace_hold_exclusion_receipt">>
]).

w601_w4_race_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(St) ->
        [?X:leaf(S, fun() -> scenario(St, S) end) || S <- specs()]
    end}.

specs() ->
    %% 元组字段序与其余套件一致：{Case, Entry, Driver, Scenario, Domain}。
    Refs = [
        {?C_REF, <<"reference-gc-race">>, Drv, Scn, Dom}
     || Scn <- ?REF_SCENARIOS, Dom <- [?ORD, ?ENT], Drv <- [<<"disk">>, <<"garage">>]
    ],
    Holds = [
        {?C_HOLD, <<"hold-gc-race">>, Drv, Scn, Dom}
     || Scn <- ?HOLD_SCENARIOS, Dom <- [?ORD, ?ENT], Drv <- [<<"disk">>, <<"garage">>]
    ],
    Confs = [
        {?C_CONF, <<"confirm-gc-race">>, Drv, Scn, Dom}
     || Scn <- [<<"confirm_commits_first">>, <<"intent_commits_first">>],
        Dom <- [?ORD, ?ENT],
        Drv <- [<<"disk">>, <<"garage">>]
    ],
    Refs ++ Holds ++ Confs.

setup() ->
    St = ?X:setup(<<"V5_INTTEST_W601W4RACE">>),
    #{conn := Conn} = St,
    try
        seed_eb_fixtures(Conn),
        St
    catch
        Class:Reason:Stack ->
            ?X:cleanup(St),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(St) -> ?X:cleanup(St).

seed_eb_fixtures(Conn) ->
    ?X:q(
        Conn,
        "INSERT INTO public.\"user\"(id,password,account,reg_ip,reg_cosv)"
        " VALUES(980001,'x','w601race','127.0.0.1','x')"
        " ON CONFLICT (id) DO NOTHING"
    ),
    ?X:q(
        Conn,
        "INSERT INTO organization(id,name,owner_id,status)"
        " VALUES(980002,'w601race',980001,'active') ON CONFLICT (id) DO NOTHING"
    ),
    ?X:q(
        Conn,
        "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
        " VALUES(980003,'w601race',980001,'active','project',980002)"
        " ON CONFLICT (id) DO NOTHING"
    ),
    ?X:q(
        Conn,
        "INSERT INTO enterprise_contact(id,organization_id,status,display_name)"
        " VALUES(980004,980002,'active','probe') ON CONFLICT (id) DO NOTHING"
    ),
    ?X:q(
        Conn,
        "INSERT INTO enterprise_conversation(id,organization_id,workspace_id,"
        "contact_id,status) VALUES(980005,980002,980003,980004,'active')"
        " ON CONFLICT (id) DO NOTHING"
    ),
    ?X:q(
        Conn,
        "INSERT INTO enterprise_message(id,organization_id,workspace_id,"
        "conversation_id,sender_type,sender_contact_id,client_msg_id,"
        "retention_days,retain_until) VALUES(980006,980002,980003,980005,"
        "'contact',980004,'w601race',1,now()+interval '1 day')"
        " ON CONFLICT (id) DO NOTHING"
    ),
    ok.

%% ------------------------------------------------------------- 分发

scenario(St, {C, _E, Drv, Scn, Dom}) ->
    case C of
        ?C_REF -> reference_race(St, Scn, Dom, Drv);
        ?C_HOLD -> hold_race(St, Scn, Dom, Drv);
        ?C_CONF -> confirm_race(St, Scn, Dom, Drv)
    end.

leaf_base() -> (?X:leaf_id(racebase) rem 100000) * 1000.

%% --------------------------------------------------- reference-gc-race

reference_race(St, Scn, ?ORD, Drv) -> ref_ordinary(St, Scn, Drv);
reference_race(St, Scn, ?ENT, Drv) -> ref_enterprise(St, Scn, Drv).

ref_ordinary(St, Scn, Drv) ->
    #{conn := Conn} = St,
    N = leaf_base() + ?X:leaf_id(ref),
    Key = <<"v5e2e/w601r/ref-", (integer_to_binary(N))/binary>>,
    GfId = 942000000 + N,
    Id = 943000000 + N,
    Bytes = seed_group_for_race(St, Conn, Key, Id, GfId, Drv),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    Measured = ref_step(St, Conn, Scn, Key, Id, GfId),
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    Measured#{object_key => Key, attachment_id => Id, domain_face => ?ORD}.

seed_group_for_race(St, Conn, Key, Id, GfId, Drv) ->
    ?X:seed_group_asset(St, Conn, Key, Id, GfId, Drv, false),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key)),
    probe_bytes(Key).

ref_step(_St, Conn, <<"bind_commits_first">>, Key, _Id, GfId) ->
    ?X:q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,"
        "file_type,file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [GfId, integer_to_binary(GfId)]
    ),
    {error, #error{code = <<"23510">>}} =
        ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r bind first')", [Key]),
    ok = epgsql:sync(Conn),
    assert_lifecycle(Conn, Key, <<"live">>),
    #{bind_committed => true, intent_rejected => <<"23510">>};
ref_step(_St, Conn, <<"intent_commits_first">>, Key, _Id, GfId) ->
    {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r int 1st')", [Key]),
    ?X:q(Conn, "BEGIN"),
    {error, #error{code = <<"23510">>}} = ?X:e(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,"
        "file_type,file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [GfId, integer_to_binary(GfId)]
    ),
    ok = epgsql:sync(Conn),
    ?X:q(Conn, "ROLLBACK"),
    ?assertEqual({error, tombstone}, with_tx_bind(Conn, Key)),
    {ok, _, [{0}]} = ?X:e(Conn, "SELECT COUNT(*) FROM group_file WHERE id=$1", [GfId]),
    assert_lifecycle(Conn, Key, <<"delete_intent">>),
    #{
        intent_committed => true,
        dml_insert_rejected => <<"23510">>,
        production_bind_rejected => <<"tombstone">>,
        group_file_rows => 0
    };
ref_step(_St, Conn, <<"direct_dml_bind_commits_first">>, Key, _Id, GfId) ->
    ?X:q(Conn, "UPDATE attachment SET group_file_id=$2 WHERE path=$1", [Key, GfId]),
    ?X:q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,"
        "file_type,file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [GfId, integer_to_binary(GfId)]
    ),
    {error, #error{code = <<"23510">>}} =
        ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r dml bind')", [Key]),
    ok = epgsql:sync(Conn),
    #{dml_bind_committed => true, intent_rejected => <<"23510">>};
ref_step(_St, Conn, <<"direct_dml_intent_commits_first">>, Key, _Id, GfId) ->
    {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r dml int')", [Key]),
    {ok, _, [{0}]} = ?X:e(
        Conn,
        "WITH u AS (UPDATE attachment SET group_file_id=$2 WHERE path=$1"
        " AND lifecycle_state='live' RETURNING 1) SELECT COUNT(*) FROM u",
        [Key, GfId]
    ),
    #{intent_committed => true, dml_bind_rows => 0}.

with_tx_bind(Conn, Key) ->
    ?X:q(Conn, "BEGIN"),
    R = attachment_repo:bind_reference_lock_tx(Conn, Key),
    ?X:q(Conn, "ROLLBACK"),
    R.

%% 企业面：message 绑定 vs fn_v5_intent_eb_delete（镜像 pg_tests 企业 race）。
ref_enterprise(St, Scn, Drv) ->
    #{conn := Conn} = St,
    N = leaf_base() + ?X:leaf_id(refeb),
    Key = <<"v5e2e/w601r/eb-ref-", (integer_to_binary(N))/binary>>,
    Id = 944000000000 + N,
    Triple = ?X:triple(St, Drv, Key),
    ok = seed_eb_live(Conn, Key, Id, Triple),
    Bytes = probe_bytes(Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    Measured = ref_eb_step(St, Conn, Scn, Id),
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    Measured#{object_key => Key, asset_id => Id, domain_face => ?ENT}.

seed_eb_live(Conn, Key, Id, Triple) ->
    ?X:q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,conversation_id,object_key,object_hash,"
        "  status,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,lifecycle_state)"
        " VALUES ($1,980002,980003,980005,$2,$3,'active',$4,$5,$6,$7,'live')"
        " ON CONFLICT (id) DO NOTHING",
        [
            Id,
            Key,
            ?X:hex64(crypto:hash(sha256, Key)),
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    ok.

eb_bind_sql(Id) ->
    io_lib:format(
        "UPDATE enterprise_asset SET message_id=980006,"
        " retain_until=now()+interval '2 days' WHERE id=~B",
        [Id]
    ).

eb_intent_sql(Id) ->
    io_lib:format(
        "SELECT fn_v5_intent_eb_delete(980002,980003,~B,'w601r eb race')", [Id]
    ).

ref_eb_step(St, Conn, <<"bind_commits_first">>, Id) ->
    Result = lock_race(St, Conn, eb_bind_sql(Id), eb_intent_sql(Id)),
    {error, #error{code = <<"23510">>}} = Result,
    ok = epgsql:sync(Conn),
    #{bind_committed => true, intent_rejected => <<"23510">>};
ref_eb_step(St, Conn, <<"intent_commits_first">>, Id) ->
    Result = lock_race(St, Conn, eb_intent_sql(Id), eb_bind_sql(Id)),
    {error, #error{code = <<"23510">>}} = Result,
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"delete_intent">>, null}]} = ?X:e(
        Conn, "SELECT lifecycle_state,message_id FROM enterprise_asset WHERE id=$1", [Id]
    ),
    #{intent_committed => true, bind_rejected => <<"23510">>, message_id_null => true};
ref_eb_step(_St, Conn, <<"direct_dml_bind_commits_first">>, Id) ->
    ?X:q(Conn, eb_bind_sql(Id)),
    {error, #error{code = <<"23510">>}} = ?X:e(Conn, eb_intent_sql(Id)),
    ok = epgsql:sync(Conn),
    #{dml_bind_committed => true, intent_rejected => <<"23510">>};
ref_eb_step(_St, Conn, <<"direct_dml_intent_commits_first">>, Id) ->
    {ok, _, _} = ?X:e(Conn, eb_intent_sql(Id)),
    {ok, _, [{0}]} = ?X:e(
        Conn,
        "WITH u AS (UPDATE enterprise_asset SET message_id=980006,"
        " retain_until=now()+interval '2 days' WHERE id=$1"
        " AND lifecycle_state='live' RETURNING 1) SELECT COUNT(*) FROM u",
        [Id]
    ),
    #{intent_committed => true, dml_bind_rows => 0}.

%% 主连接事务持锁 + 第二连接竞争者真实阻塞于行锁，主连接提交后竞争者落地。
lock_race(St, Conn, HoldSql, ContenderSql) ->
    Other = ?X:barrier_connect(St),
    try
        {ok, _, [{Pid}]} = ?X:e(Other, "SELECT pg_backend_pid()", []),
        ?X:q(Other, "SET statement_timeout = '10s'"),
        ?X:q(Conn, "BEGIN"),
        ?X:q(Conn, HoldSql),
        Parent = self(),
        spawn(fun() -> Parent ! {contender, epgsql:equery(Other, ContenderSql, [])} end),
        ?X:wait_lock(Conn, Pid, 100),
        ?X:q(Conn, "COMMIT"),
        receive
            {contender, R} -> R
        after 15000 -> erlang:error(w601_race_contender_timeout)
        end
    after
        ok = epgsql:close(Other)
    end.

%% ------------------------------------------------------- hold-gc-race

hold_race(St, Scn, ?ORD, Drv) -> hold_ordinary(St, Scn, Drv);
hold_race(St, Scn, ?ENT, Drv) -> hold_enterprise(St, Scn, Drv).

hold_ordinary(St, Scn, Drv) ->
    #{conn := Conn} = St,
    N = leaf_base() + ?X:leaf_id(holdrace),
    Key = <<"v5e2e/w601r/hold-", (integer_to_binary(N))/binary>>,
    {Scope, Ref} =
        case Scn of
            <<"intent_before_asset_hold">> -> {<<"private">>, null};
            <<"intent_before_retention_extension">> -> {<<"private">>, null};
            _ -> {<<"group">>, <<"66">>}
        end,
    Id = 945000000 + N,
    #{id := Id} = ?X:seed_scoped_asset(St, Conn, Key, Id, Drv, Scope, Ref),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key)),
    Bytes = probe_bytes(Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    Measured = hold_ord_step(Conn, Scn, Key, Id),
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    Measured#{object_key => Key, attachment_id => Id, domain_face => ?ORD}.

hold_ord_step(Conn, <<"hold_commits_first">>, Key, Id) ->
    Cleanup = ?X:install_governance(Conn, Id, hold_message),
    {error, #error{code = <<"23510">>}} =
        ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r hold 1st')", [Key]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?X:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    Cleanup(),
    #{
        hold_fixture => <<"hold_message">>,
        intent_rejected => <<"23510">>,
        attachment_rows_after => 1
    };
hold_ord_step(Conn, <<"intent_before_asset_hold">>, Key, Id) ->
    {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r late hold')", [Key]),
    {ok, _} = enterprise_attachment_retention_repo:upsert_tx(Conn, Id, 9101, 9103),
    ok = enterprise_attachment_retention_repo:hold_tx(Conn, Id, <<"w601r late hold">>),
    assert_lifecycle(Conn, Key, <<"delete_intent">>),
    {ok, _, [{1}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        intent_committed => true,
        late_hold_installed => true,
        lifecycle_state_after => <<"delete_intent">>,
        deletion_queue_rows_after_late_hold => 1
    };
hold_ord_step(Conn, <<"intent_before_retention_extension">>, Key, Id) ->
    {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r late ext')", [Key]),
    {ok, _} = enterprise_attachment_retention_repo:upsert_tx(Conn, Id, 9101, 9103),
    NewUntil = elib_dt:add(elib_dt:now(), {3650 * 86400, second}),
    {ok, _} = enterprise_attachment_retention_repo:extend_tx(Conn, Id, NewUntil),
    assert_lifecycle(Conn, Key, <<"delete_intent">>),
    #{
        intent_committed => true,
        retention_extended => true,
        lifecycle_state_after => <<"delete_intent">>
    };
hold_ord_step(Conn, <<"workspace_hold_exclusion_receipt">>, Key, Id) ->
    Cleanup = ?X:install_governance(Conn, Id, hold_workspace),
    {error, #error{code = <<"23510">>}} =
        ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r ws r1')", [Key]),
    ok = epgsql:sync(Conn),
    {error, #error{code = <<"23510">>}} =
        ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r ws r2')", [Key]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?X:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM workspace_attachment_retention WHERE attachment_id=$1",
        [Id]
    ),
    {ok, _, [{0}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    Cleanup(),
    #{
        hold_excluded_intent => <<"23510">>,
        exclusion_persistent => true,
        attachment_rows_after => 1,
        war_rows => 1,
        deletion_queue_rows => 0
    }.

hold_enterprise(St, Scn, Drv) ->
    #{conn := Conn} = St,
    N = leaf_base() + ?X:leaf_id(holdraceeb),
    Key = <<"v5e2e/w601r/eb-hold-", (integer_to_binary(N))/binary>>,
    Id = 946000000 + N,
    #{id := Id} =
        ?X:seed_scoped_asset(
            St, Conn, Key, Id, Drv, <<"enterprise">>, <<"org:9101/app:9103">>
        ),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key)),
    Bytes = probe_bytes(Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    Ctx = #{organization_id => 9101, application_id => 9103},
    Measured = hold_ent_step(Conn, Scn, Key, Id, Ctx),
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    Measured#{object_key => Key, attachment_id => Id, domain_face => ?ENT}.

hold_ent_step(Conn, <<"hold_commits_first">>, Key, Id, Ctx) ->
    {ok, _} = enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
        object_key => Key, op => <<"hold">>, reason => <<"w601r hold 1st ent">>
    }),
    {ok, _, [{<<"held">>}]} = ?X:e(
        Conn,
        "SELECT hold_state FROM enterprise_attachment_retention WHERE attachment_id=$1",
        [Id]
    ),
    {error, #error{code = <<"23510">>}} =
        ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r hold ent')", [Key]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?X:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    #{
        governance_entry => <<"governance_tx(hold)">>,
        hold_state => <<"held">>,
        intent_rejected => <<"23510">>,
        attachment_rows_after => 1
    };
hold_ent_step(Conn, <<"intent_before_asset_hold">>, Key, _Id, Ctx) ->
    {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r int ent')", [Key]),
    {error, {<<"invalid_request">>, already_deleting}} =
        enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
            object_key => Key, op => <<"hold">>, reason => <<"w601r late hold ent">>
        }),
    #{intent_committed => true, governance_rejection => <<"already_deleting">>};
hold_ent_step(Conn, <<"intent_before_retention_extension">>, Key, _Id, Ctx) ->
    {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r ext ent')", [Key]),
    {error, {<<"invalid_request">>, already_deleting}} =
        enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
            object_key => Key, op => <<"set_retention">>, retention_days => 90
        }),
    assert_lifecycle(Conn, Key, <<"delete_intent">>),
    #{
        intent_committed => true,
        governance_rejection => <<"already_deleting">>,
        lifecycle_state_after => <<"delete_intent">>
    };
hold_ent_step(Conn, <<"workspace_hold_exclusion_receipt">>, Key, Id, Ctx) ->
    {ok, _} = enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
        object_key => Key, op => <<"hold">>, reason => <<"w601r ws receipt ent">>
    }),
    {error, #error{code = <<"23510">>}} =
        ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r ws ent 1')", [Key]),
    ok = epgsql:sync(Conn),
    {ok, _} = enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
        object_key => Key, op => <<"release_hold">>
    }),
    {ok, _, [{<<"none">>}]} = ?X:e(
        Conn,
        "SELECT hold_state FROM enterprise_attachment_retention WHERE attachment_id=$1",
        [Id]
    ),
    {error, #error{code = <<"23510">>}} =
        ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r ws ent 2')", [Key]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?X:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    #{
        hold_excluded_intent => <<"23510">>,
        released_hold_state => <<"none">>,
        retention_window_still_excludes => <<"23510">>,
        attachment_rows_after => 1
    }.

%% ----------------------------------------------------- confirm-gc-race

%% 域面：ordinary 真实执行；enterprise 走企业原生 pending-cleanup CAS ×
%% 真实企业确认（eb_pg_asset_meta），不得借用 ordinary fn_v5_confirm_pending
%% 贴 shared_protocol_face 标签。
confirm_race(St, Scn, ?ORD, Drv) ->
    Measured = confirm_face(St, Scn, Drv),
    Measured#{domain_face => ?ORD};
confirm_race(St, Scn, ?ENT, Drv) ->
    Measured = confirm_face_ent(St, Scn, Drv),
    Measured#{domain_face => ?ENT}.

confirm_face(St, <<"intent_commits_first">>, Drv) ->
    #{conn := Conn} = St,
    N = leaf_base() + ?X:leaf_id(conf),
    Key = <<"v5e2e/w601r/conf-int-", (integer_to_binary(N))/binary>>,
    Id = 947000000 + N,
    {ok, Bytes} = seed_confirm_asset(St, Conn, Key, Id, Drv, expired_window),
    {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r cint')", [Key]),
    assert_lifecycle(Conn, Key, <<"delete_intent">>),
    {error, #error{code = <<"23510">>}} = ?X:e(Conn, "SELECT fn_v5_confirm_pending($1,$2)", [
        Key, Id
    ]),
    ok = epgsql:sync(Conn),
    assert_lifecycle(Conn, Key, <<"delete_intent">>),
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    #{
        intent_committed => true,
        confirm_replay_rejected => <<"23510">>,
        lifecycle_state => <<"delete_intent">>,
        object_key => Key
    };
confirm_face(St, <<"confirm_commits_first">>, Drv) ->
    #{conn := Conn} = St,
    N = leaf_base() + ?X:leaf_id(conf),
    Key = <<"v5e2e/w601r/conf-cfm-", (integer_to_binary(N))/binary>>,
    Id = 947000000 + N,
    {ok, Bytes} = seed_confirm_asset(St, Conn, Key, Id, Drv, open_window),
    {ok, _, [{Id}]} = ?X:e(Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, Id]),
    confirm_barrier(St, Conn, Key, Id),
    {ok, _, [{0}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    Result = ?X:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601r ccf')", [Key]),
    ok = epgsql:sync(Conn),
    ?assertMatch({error, #error{}}, Result),
    {ok, _, [{0}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    {ok, _, [{Id}]} = ?X:e(Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, Id]),
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    #{
        confirm_committed => true,
        confirm_idempotent_replay => true,
        deletion_queue_rows => 0,
        intent_rejected => true,
        object_key => Key
    }.

%% expired_window：注册即能力窗已耗尽（confirm 不可再提交）；
%% open_window：窗口未过（confirm 先提交的唯一合法起点）。M3 守卫禁 UPDATE
%% 扩窗，窗口只能在注册时一次定死。
seed_confirm_asset(St, Conn, Key, Id, Drv, Window) ->
    Triple = ?X:triple(St, Drv, Key),
    register_confirm_session(Conn, Key, Id, Triple, Window),
    Bytes = probe_bytes(Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    case Window of
        expired_window -> ?X:await_upload_expiry(Conn, Key);
        open_window -> ok
    end,
    {ok, Bytes}.

%% expired_window：注册即能力窗已耗尽；open_window：窗口未过（confirm 先提
%% 交的唯一合法起点）。M3 守卫禁 UPDATE 扩窗，窗口只能注册时一次定死。
%% fn_v5_register_pending 为 12 参（00000191 定稿），location 四元组逐位对齐。
register_confirm_session(Conn, Key, Id, Triple, Window) ->
    {DeadlineSql, ExpirySql} =
        case Window of
            expired_window ->
                {"now() + interval '2 hours'", "now() + interval '3 seconds'"};
            open_window ->
                {"now() + interval '2 hours'", "now() + interval '1 hour'"}
        end,
    ?X:q(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb," ++ DeadlineSql ++ "," ++ ExpirySql ++ ",$3,$4,$5,$6)",
        [
            Key,
            jsone:encode(#{
                <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => <<"101">>},
                <<"declaration">> => #{<<"mime">> => <<"image/png">>}
            }),
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    ?X:q(Conn, "SELECT fn_v5_pin_pending_content($1,$2::jsonb)", [Key, ?X:receipt_fixture(Id)]),
    ?X:q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,'private',101,$3,$4,$5,$6,true,NULL,NULL,$7)",
        [
            Id,
            Key,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple),
            ?X:hex64(crypto:hash(sha256, Key))
        ]
    ),
    ok.

%% confirm 先提交的屏障：Holder 持 attachment 行锁，主连接 confirm 重放真实
%% 阻塞，释放后幂等成功（镜像 w203 形状）。
confirm_barrier(St, Conn, Key, Id) ->
    Parent = self(),
    Holder =
        spawn_link(fun() ->
            R =
                try
                    {ok, HConn} = ?X:barrier_connect(St),
                    try
                        _ = epgsql:equery(HConn, "BEGIN", []),
                        {ok, _, _} = epgsql:equery(
                            HConn,
                            "SELECT path FROM attachment WHERE path=$1 FOR UPDATE",
                            [Key]
                        ),
                        Parent ! {w601_held, self()},
                        receive
                            release -> ok
                        after 8000 -> ok
                        end,
                        _ = epgsql:equery(HConn, "ROLLBACK", []),
                        released
                    after
                        epgsql:close(HConn)
                    end
                catch
                    C:E -> {C, E}
                end,
            Parent ! {w601_holder_done, self(), R}
        end),
    receive
        {w601_held, Holder} -> ok
    after 8000 -> erlang:error(w601_barrier_timeout)
    end,
    {ok, _, [{Id}]} = ?X:e(Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, Id]),
    receive
        {w601_holder_done, Holder, released} -> ok
    after 8000 -> erlang:error(w601_holder_join_timeout)
    end,
    ok.

%% ------------------------------------------- confirm-gc-race（企业原生面）

%% 企业叶：真实 pending-cleanup CAS 竞争——eb_pg_asset_meta:claim_pending_
%% cleanup/5（src/features/enterprise_business/infrastructure/
%% eb_pg_asset_meta.erl:645-682，原子 CAS：仅 pending_confirm、未绑消息、
%% 年龄/保留/hold 条件，成功写 status='deleted'+pending_object_delete）×
%% 真实企业确认 eb_pg_asset_meta:confirm_asset/3（pending_confirm→active）。
%% confirm-first：企业确认（生产 CAS 语句逐字）先提交 active → cleanup CAS
%% 不符资格 {error, not_eligible}、零队列行、pending_object_delete=false、
%% 字节保留；cleanup-first：清理（生产 claim CAS 语句逐字）先提交 deleted →
%% 确认 CAS 0 行失败 {error, conflict}。两序均为双连接真实屏障：先提交方
%% 在 A 连接未提交事务中持 enterprise_asset 行锁，落败方真实生产函数经
%% elib_pg 转发在 B 连接异步执行，空闲连接证明 pg_blocking_pids(B) 含 A 且
%% pg_locks 存在未授予锁后 A COMMIT，B 的 CAS 复核落败。
confirm_face_ent(St, <<"confirm_commits_first">>, Drv) ->
    #{conn := Conn} = St,
    N = leaf_base() + ?X:leaf_id(conf),
    Key = <<"v5e2e/w601r/ent-cfm-", (integer_to_binary(N))/binary>>,
    Id = 947100000 + N,
    ok = seed_ent_confirm_asset(St, Conn, Key, Id, Drv),
    Bytes = probe_bytes(Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    LockType =
        ent_cas_race(
            St,
            Key,
            ent_confirm_cas_sql(Id),
            fun() ->
                eb_pg_asset_meta:claim_pending_cleanup(
                    ?ENT_ORG, ?ENT_WS, Id, now_unix(), 3600
                )
            end,
            {error, not_eligible}
        ),
    {ok, _, [{<<"active">>, false}]} = ?X:e(
        Conn,
        "SELECT status, pending_object_delete FROM enterprise_asset WHERE id=$1",
        [Id]
    ),
    {ok, _, [{0}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1", [Id]
    ),
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    #{
        order => confirm_first,
        confirm_committed => true,
        cleanup_cas_rejected => not_eligible,
        intent_generated => false,
        pending_object_delete => false,
        lock_wait_proven => true,
        lock_type => LockType,
        object_key => Key
    };
confirm_face_ent(St, <<"intent_commits_first">>, Drv) ->
    #{conn := Conn} = St,
    N = leaf_base() + ?X:leaf_id(conf),
    Key = <<"v5e2e/w601r/ent-cln-", (integer_to_binary(N))/binary>>,
    Id = 947100000 + N,
    ok = seed_ent_confirm_asset(St, Conn, Key, Id, Drv),
    Bytes = probe_bytes(Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    LockType =
        ent_cas_race(
            St,
            Key,
            ent_cleanup_cas_sql(Id, 3600),
            fun() -> eb_pg_asset_meta:confirm_asset(?ENT_ORG, ?ENT_WS, Id) end,
            {error, conflict}
        ),
    {ok, _, [{<<"deleted">>, true}]} = ?X:e(
        Conn,
        "SELECT status, pending_object_delete FROM enterprise_asset WHERE id=$1",
        [Id]
    ),
    {ok, _, [{0}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1", [Id]
    ),
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    #{
        order => cleanup_first,
        cleanup_committed => true,
        pending_object_delete => true,
        confirm_cas_rejected => conflict,
        lock_wait_proven => true,
        lock_type => LockType,
        object_key => Key
    }.

%% 企业原生 pending_confirm 种子（镜像 hold_tests eb-pending-cleanup 先例）：
%% status='pending_confirm'、未绑消息、created_at 早于 TTL、retain_until
%% NULL、live；身份三元组指向该叶 driver 的真实 registry 根。
seed_ent_confirm_asset(St, Conn, Key, Id, Drv) ->
    Triple = ?X:triple(St, Drv, Key),
    ok = ?X:seed_eb_asset_row(Conn, Key, Id, Triple, <<"pending_confirm">>, false, <<"live">>),
    ?X:q(
        Conn,
        "UPDATE enterprise_asset SET created_at = now() - interval '2 hours',"
        " message_id = NULL, retain_until = NULL WHERE id=$1",
        [Id]
    ),
    ok.

%% 生产确认 CAS 语句（eb_pg_asset_meta ?SQL_CONFIRM_ASSET 逐字，租户键内联）。
ent_confirm_cas_sql(Id) ->
    io_lib:format(
        "UPDATE enterprise_asset SET status = 'active', version = version + 1,"
        " updated_at = now()"
        " WHERE organization_id = 9101 AND workspace_id = 9102 AND id = ~B"
        " AND status = 'pending_confirm' AND lifecycle_state = 'live' RETURNING id",
        [Id]
    ).

%% 生产 pending-cleanup claim CAS 语句（claim_pending_in 逐字；本套件无
%% conversation/message 作用域 hold，workspace 级 NOT EXISTS 为其生效投影）。
ent_cleanup_cas_sql(Id, Ttl) ->
    io_lib:format(
        "UPDATE enterprise_asset SET status = 'deleted',"
        " pending_object_delete = true, deleted_at = now(), updated_at = now(),"
        " version = version + 1"
        " WHERE organization_id = 9101 AND workspace_id = 9102 AND id = ~B"
        " AND status = 'pending_confirm' AND message_id IS NULL"
        " AND created_at <= to_timestamp(~B - ~B)"
        " AND (retain_until IS NULL OR retain_until <= to_timestamp(~B))"
        " AND NOT EXISTS (SELECT 1 FROM enterprise_retention_hold h"
        " WHERE h.organization_id = 9101 AND h.workspace_id = 9102"
        " AND h.released_at IS NULL)"
        " RETURNING object_key",
        [Id, now_unix(), Ttl, now_unix()]
    ).

%% 双连接真实屏障：A BEGIN + 先提交方生产 CAS 语句（持行锁未提交）；B 上
%% 落败方真实生产函数异步执行并真实阻塞；空闲连接证明 pg_blocking_pids(B)
%% 包含 A 且 pg_locks 中 B 有未授予锁（不许只看 wait_event_type）→
%% A COMMIT → B 的 CAS 复核落败（期望损失形态由调用方断言）。异步结果以
%% 唯一引用收集，超时即失败；清理路径 unload meck、关闭三连接。
ent_cas_race(St, Key, WinnerSql, LoserFun, ExpectedLoss) ->
    #{conn := Conn} = St,
    A = ?X:barrier_connect(St),
    B = ?X:barrier_connect(St),
    Chk = ?X:barrier_connect(St),
    try
        ok = ent_pool_install(Conn),
        {ok, _, [{APid}]} = ?X:e(A, "SELECT pg_backend_pid()", []),
        {ok, _, [{BPid}]} = ?X:e(B, "SELECT pg_backend_pid()", []),
        ok = ?X:q(A, "BEGIN"),
        _WinnerRow =
            case ?X:e(A, WinnerSql) of
                {ok, _, [Row]} -> Row;
                {ok, _, []} -> erlang:error({w601_ent_winner_no_row, Key});
                {error, Reason} -> erlang:error({w601_ent_winner_failed, Key, Reason})
            end,
        Ref = make_ref(),
        Parent = self(),
        spawn(fun() ->
            put(w601_ent_pool_conn, B),
            Parent !
                {Ref,
                    try
                        LoserFun()
                    catch
                        C:R -> {crash, C, R}
                    end}
        end),
        LockType = ent_wait_blocked(Chk, APid, BPid, 200),
        ok = ?X:q(A, "COMMIT"),
        LossResult =
            receive
                {Ref, R} -> R
            after 20000 -> erlang:error(w601_ent_cas_timeout)
            end,
        ExpectedLoss = LossResult,
        LockType
    after
        ent_pool_unload(),
        ok = epgsql:close(A),
        ok = epgsql:close(B),
        ok = epgsql:close(Chk)
    end.

ent_wait_blocked(_Chk, _APid, _BPid, 0) ->
    erlang:error(w601_ent_cas_block_unproven);
ent_wait_blocked(Chk, APid, BPid, Left) ->
    {ok, _, [{Blocked, LockType}]} = ?X:e(
        Chk,
        "SELECT $1 = ANY(pg_blocking_pids($2)),"
        " (SELECT locktype::text FROM pg_locks WHERE pid = $2 AND NOT granted LIMIT 1)",
        [APid, BPid]
    ),
    case Blocked of
        true ->
            LockType;
        false ->
            timer:sleep(50),
            ent_wait_blocked(Chk, APid, BPid, Left - 1)
    end.

%% elib_pg 池转发（按封印 W4-03 predecessor 先例，hold_tests 同型）：真实
%% 生产函数的 with_tx/query/execute 落到本套件连接；调用进程可用 pdict 键
%% w601_ent_pool_conn 覆盖（竞态落败方绑定独立屏障连接 B）。with_tx 以真实
%% BEGIN/COMMIT/ROLLBACK 包装，{rollback,_}/{abort_tx,_} throw 语义与
%% elib_pg:with_tx 一致。
ent_pool_install(DefaultConn) ->
    ok = meck:new(elib_pg, [non_strict, no_link, passthrough]),
    ok = meck:expect(elib_pg, with_tx, fun(F) ->
        C = ent_pool_conn(DefaultConn),
        {ok, _, _} = epgsql:equery(C, "BEGIN", []),
        try
            R = F(C),
            {ok, _, _} = epgsql:equery(C, "COMMIT", []),
            R
        catch
            throw:{rollback, Reason} ->
                _ = epgsql:equery(C, "ROLLBACK", []),
                {rollback, Reason};
            throw:{abort_tx, Reason} ->
                _ = epgsql:equery(C, "ROLLBACK", []),
                {error, Reason}
        end
    end),
    ok = meck:expect(elib_pg, query, 2, fun(Sql, Params) ->
        meck:passthrough([ent_pool_conn(DefaultConn), Sql, Params])
    end),
    ok = meck:expect(elib_pg, query, 3, fun(C, Sql, Params) ->
        meck:passthrough([C, Sql, Params])
    end),
    ok = meck:expect(elib_pg, execute, 2, fun(Sql, Params) ->
        meck:passthrough([ent_pool_conn(DefaultConn), Sql, Params])
    end),
    ok = meck:expect(elib_pg, execute, 3, fun(C, Sql, Params) ->
        meck:passthrough([C, Sql, Params])
    end),
    ok.

ent_pool_conn(Default) ->
    case get(w601_ent_pool_conn) of
        undefined -> Default;
        C -> C
    end.

ent_pool_unload() ->
    try
        meck:unload(elib_pg),
        ok
    catch
        _:_ -> ok
    end.

now_unix() -> os:system_time(second).

%% ------------------------------------------------------------- helpers

probe_bytes(Key) -> <<"w601-race-probe:", Key/binary>>.

assert_lifecycle(Conn, Key, State) ->
    {ok, _, [{State}]} = ?X:e(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
    ),
    ok.
