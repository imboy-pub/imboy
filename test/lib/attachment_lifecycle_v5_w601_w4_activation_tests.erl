%%% @doc W6-01 E 组：W4-04 五组（reconcile 16 + no-infinite-loop 16 +
%%% guards-activation 16 + activation-commit-unknown 16 + all-writer-compat
%%% 8 = 72 叶），schema_phase=integrated。
%%%
%%% 复用：guards-activation / activation-commit-unknown 直接调封印执行器
%%% attachment_lifecycle_v5_w404_activation_guards:guards/3 与 activation/3
%%% （catalog/触发器协议面）；reconcile / no-infinite-loop / all-writer-compat
%%% 镜像 w404_activation_tests 的场景 oracle，driver 维度 = 队列行/资产行
%%% 冻结身份三元组指向该叶 driver 的真实 registry 根。catalog 面与 driver
%%% 无关，guards/activation 叶如实披露 shared 协议面。
-module(attachment_lifecycle_v5_w601_w4_activation_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(X, attachment_lifecycle_v5_w601_w4_context).
-define(QP, attachment_lifecycle_v5_w601_w4_queueproto).
-define(GUARDS, attachment_lifecycle_v5_w404_activation_guards).
-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).
-define(ORG, 9101).

w601_w4_activation_test_() ->
    %% 受控让位（仓库先例：attachment_lifecycle_v5_pg_tests r20_test_/
    %% v5_gate_pinned：eunit 无 {skip, ...} 顶层描述符，生成器返回空清单
    %% 才是等价的静默让位机制）。setup 经 ?X:setup → inttest_marker_db:
    %% provision 钉 V5_INTTEST_W601W4ACT（gate 成套导出 _DB env）；nightly
    %% 全量裸跑不经 gate，缺钉名 env 时在生成器入口让位，不进 setup
    %% （provision 的 fail-closed 契约留给非法半配置）。gate 路径（env 在
    %% 场）语义与断言零删减。
    case os:getenv("V5_INTTEST_W601W4ACT_DB") of
        V when is_list(V), V =/= "" ->
            activation_units();
        _ ->
            io:format(
                user, "W601W4_ACTIVATION_EUNIT_YIELD missing_env=\"V5_INTTEST_W601W4ACT_DB\"~n", []
            ),
            []
    end.

activation_units() ->
    {setup, fun() -> ?X:setup(<<"V5_INTTEST_W601W4ACT">>) end, fun ?X:cleanup/1, fun(St) ->
        [?X:leaf(S, fun() -> scenario(St, S) end) || S <- specs()]
    end}.

specs() ->
    lists:append([
        full(<<"W4-04/reconcile">>, <<"reconcile">>, [
            <<"startup_ready">>,
            <<"periodic_retry">>,
            <<"restart_expired_lease">>,
            <<"nonowner_discrepancy_only">>
        ]),
        full(<<"W4-04/no-infinite-loop">>, <<"no-infinite-loop">>, [
            <<"task_attempt5_restart">>,
            <<"claim_attempt8_restart">>,
            <<"blocked_no_poll">>,
            <<"db_outage_bounded_wakeup">>
        ]),
        full(<<"W4-04/guards-activation">>, <<"guards-activation">>, [
            <<"all_prerequisites_present">>,
            <<"producer_missing">>,
            <<"old_worker_running">>,
            <<"guard_missing">>
        ]),
        full(<<"W4-04/activation-commit-unknown">>, <<"activation-commit-unknown">>, [
            <<"ddl_committed_checkpoint_lost">>,
            <<"proven_rollback">>,
            <<"partial_catalog_conflict">>,
            <<"guard_failure_worker_off">>
        ]),
        full(<<"W4-04/all-writer-compat">>, <<"all-writer-compat">>, [
            <<"supported_schema_writer">>,
            <<"old_writer_incomplete_identity">>
        ])
    ]).

full(Case, Entry, Scenarios) ->
    [
        {Case, Entry, Drv, Scn, Dom}
     || Scn <- Scenarios, Dom <- [?ORD, ?ENT], Drv <- [<<"disk">>, <<"garage">>]
    ].

%% ------------------------------------------------------------- 分发

scenario(St, {Case, Entry, Drv, Scn, Dom}) ->
    #{conn := Conn} = St,
    N = ((?X:leaf_id(abase) rem 100000) * 1000 + ?X:leaf_id(act)),
    Key = <<"v5e2e/w601a/", Entry/binary, "/", (integer_to_binary(N))/binary>>,
    Triple = ?X:triple(St, Drv, Key),
    Base = #{
        object_key => Key,
        driver => Drv,
        storage_namespace => maps:get(namespace, Triple),
        storage_container => maps:get(container, Triple),
        locator_sha256 => maps:get(locator, Triple),
        domain_face => Dom
    },
    Measured =
        case Case of
            <<"W4-04/reconcile">> ->
                reconcile_scenario(St, Conn, Scn, Dom, Key, Triple, N);
            <<"W4-04/no-infinite-loop">> ->
                loop_scenario(St, Conn, Scn, Dom, Key, Triple, N);
            <<"W4-04/guards-activation">> ->
                (?GUARDS:guards(Scn, Dom, #{conn => Conn}))#{
                    catalog_protocol_shared_across_drivers => true
                };
            <<"W4-04/activation-commit-unknown">> ->
                (?GUARDS:activation(Scn, Dom, #{conn => Conn}))#{
                    catalog_protocol_shared_across_drivers => true
                };
            <<"W4-04/all-writer-compat">> ->
                writer_scenario(St, Conn, Scn, Dom, Key, Triple, N)
        end,
    maps:merge(Base, Measured).

%% --------------------------------------------------------- reconcile

reconcile_scenario(St, Conn, <<"startup_ready">>, Dom, Key, Triple, N) ->
    reap_cycle(St, Conn, Dom, Key, Triple, N, <<"startup">>);
reconcile_scenario(St, Conn, <<"periodic_retry">>, Dom, Key, Triple, N) ->
    retry_cycle(St, Conn, Dom, Key, Triple, N);
reconcile_scenario(St, Conn, <<"restart_expired_lease">>, Dom, Key, Triple, N) ->
    restart_cycle(St, Conn, Dom, Key, Triple, N);
reconcile_scenario(St, Conn, <<"nonowner_discrepancy_only">>, Dom, Key, Triple, N) ->
    discrepancy(St, Conn, Dom, Key, Triple, N).

%% startup_ready：过期租约 → reaper 回收 → retry_wait（字节零删除）。
reap_cycle(_St, Conn, ?ORD, Key, Triple, _N, _Label) ->
    seed_job(Conn, Key, Triple, 0, ready),
    %% Keep the real global claim protocol, but select this fixture's target.
    %% 保留真实全局 claim 协议，仅将本夹具目标置为最先到期。
    ?X:q(
        Conn,
        "UPDATE attachment_object_deletion SET next_retry_at='-infinity'::timestamptz WHERE path=$1",
        [Key]
    ),
    {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, ?X:token()),
    ?QP:expire_ordinary(Conn, Key),
    {ok, Reaped} = attachment_delete_worker:reconcile(Conn),
    {ok, _, [{<<"retry_wait">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        reaped => Reaped,
        state_after => <<"retry_wait">>,
        lease_cleared => true,
        queue => <<"ordinary">>
    };
reap_cycle(_St, Conn, ?ENT, Key, Triple, N, _Label) ->
    %% 企业队列无 path 条件，claim 取最旧活跃行；组内共享同库时前序场景遗留
    %% 的 retry_wait（本场景收尾态）会被 target_claim 优先取走并报
    %% not_claimed，掩盖本叶首错（run2: A3 遗留 → A4 起连环 11 失败）。
    %% claim 前按 job guard 合法 UPDATE 把遗留活跃行收敛到 done
    %% （queueproto settle_active 先例），require_empty 保留隔离断言。
    ?QP:drain_eb(Conn),
    AssetId = 952000000000 + N,
    ok = ?X:seed_eb_asset_row(Conn, Key, AssetId, Triple, <<"deleted">>, true, <<"live">>),
    ok = ?QP:enqueue_eb(Conn, ?ORG, 9102, AssetId, <<"w601a startup">>),
    ok = target_first(Conn, AssetId),
    {ok, _} = ?QP:claim_eb_for(Conn, ?ORG, 9102, AssetId),
    ?QP:expire_eb(Conn, ?ORG, AssetId),
    {ok, _, [{Reaped}]} = ?X:e(Conn, "SELECT fn_v5_reaper_eb(100)", []),
    {ok, _, [{<<"retry_wait">>}]} = ?QP:eb_state(Conn, ?ORG, AssetId),
    #{
        reaped => Reaped,
        state_after => <<"retry_wait">>,
        lease_cleared => true,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

%% periodic_retry：retry_wait 到期 → tick 批消费到 done。
retry_cycle(_St, Conn, ?ORD, Key, Triple, _N) ->
    seed_job(Conn, Key, Triple, 1, retry_wait),
    ?X:q(
        Conn,
        "UPDATE attachment_object_deletion SET next_retry_at = now() - interval '1 s'"
        " WHERE path=$1",
        [Key]
    ),
    {ok, Consumed} =
        attachment_delete_worker:run_batch(Conn, fun(_, _, _, _) -> ok end),
    {ok, _, [{<<"done">>, Attempts}]} = ?X:e(
        Conn,
        "SELECT job_state, attempts FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    #{
        consumed => Consumed,
        state_after => <<"done">>,
        attempts => Attempts,
        queue => <<"ordinary">>
    };
retry_cycle(_St, Conn, ?ENT, Key, Triple, N) ->
    %% claim 前收敛前序场景遗留活跃行（见 reap_cycle ?ENT 注释）。
    ?QP:drain_eb(Conn),
    AssetId = 952000000000 + N,
    ok = ?X:seed_eb_asset_row(Conn, Key, AssetId, Triple, <<"deleted">>, true, <<"live">>),
    ok = ?QP:enqueue_eb(Conn, ?ORG, 9102, AssetId, <<"w601a periodic">>),
    ok = target_first(Conn, AssetId),
    {ok, #{token := T1}} = ?QP:claim_eb_for(Conn, ?ORG, 9102, AssetId),
    {ok, _} = ?QP:ack_eb_ok(Conn, ?ORG, 9102, AssetId, T1, false),
    {ok, #{token := T2, attempts := Attempts}} = claim_eb_retry(Conn, AssetId),
    {ok, <<"done">>} = ?QP:ack_eb_ok(Conn, ?ORG, 9102, AssetId, T2, true),
    {ok, _, [{<<"done">>}]} = ?QP:eb_state(Conn, ?ORG, AssetId),
    #{
        consumed => 1,
        state_after => <<"done">>,
        attempts => Attempts,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

%% restart_expired_lease：租约过期 → reap → 再 claim（attempts 递增）。
restart_cycle(_St, Conn, ?ORD, Key, Triple, _N) ->
    seed_job(Conn, Key, Triple, 0, ready),
    {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, ?X:token()),
    ?QP:expire_ordinary(Conn, Key),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    {ok, #{<<"out_path">> := Key, <<"out_attempts">> := 2}} =
        ?QP:claim_ordinary(Conn, ?X:token()),
    #{reaped => 1, attempts_after => 2, queue => <<"ordinary">>};
restart_cycle(_St, Conn, ?ENT, Key, Triple, N) ->
    %% claim 前收敛前序场景遗留活跃行（见 reap_cycle ?ENT 注释）。
    ?QP:drain_eb(Conn),
    AssetId = 952000000000 + N,
    ok = ?X:seed_eb_asset_row(Conn, Key, AssetId, Triple, <<"deleted">>, true, <<"live">>),
    ok = ?QP:enqueue_eb(Conn, ?ORG, 9102, AssetId, <<"w601a restart">>),
    ok = target_first(Conn, AssetId),
    {ok, _} = ?QP:claim_eb_for(Conn, ?ORG, 9102, AssetId),
    ?QP:expire_eb(Conn, ?ORG, AssetId),
    {ok, _, [{1}]} = ?X:e(Conn, "SELECT fn_v5_reaper_eb(100)", []),
    {ok, #{attempts := 2}} = claim_eb_retry(Conn, AssetId),
    #{reaped => 1, attempts_after => 2, queue => <<"enterprise">>, asset_id => AssetId}.

%% nonowner_discrepancy_only：只读清点，不越权改行。
discrepancy(_St, Conn, ?ORD, Key, Triple, _N) ->
    seed_job(Conn, Key, Triple, 0, ready),
    {ok, _, [{Total}]} = ?X:e(Conn, "SELECT COUNT(*) FROM attachment_object_deletion", []),
    {ok, _, [{Matching}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ?assert(Total >= 1),
    ?assertEqual(1, Matching),
    {ok, _, [{<<"ready">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        discrepancy_rows => Total,
        matching_rows => Matching,
        state_untouched => <<"ready">>,
        queue => <<"ordinary">>
    };
discrepancy(_St, Conn, ?ENT, Key, Triple, N) ->
    AssetId = 952000000000 + N,
    ok = ?X:seed_eb_asset_row(Conn, Key, AssetId, Triple, <<"deleted">>, true, <<"live">>),
    ok = ?QP:enqueue_eb(Conn, ?ORG, 9102, AssetId, <<"w601a disc">>),
    {ok, _, [{Total}]} = ?X:e(Conn, "SELECT COUNT(*) FROM enterprise_asset_delete_queue", []),
    {ok, _, [{Matching}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [AssetId]
    ),
    ?assertEqual(1, Matching),
    {ok, _, [{<<"ready">>}]} = ?QP:eb_state(Conn, ?ORG, AssetId),
    #{
        discrepancy_rows => Total,
        matching_rows => Matching,
        state_untouched => <<"ready">>,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

%% ---------------------------------------------------- no-infinite-loop

loop_scenario(St, Conn, <<"task_attempt5_restart">>, Dom, Key, Triple, N) ->
    attempts_kept(St, Conn, Dom, Key, Triple, N, 5);
loop_scenario(St, Conn, <<"claim_attempt8_restart">>, Dom, Key, Triple, N) ->
    attempt8_rejected(St, Conn, Dom, Key, Triple, N);
loop_scenario(St, Conn, <<"blocked_no_poll">>, Dom, Key, Triple, N) ->
    blocked_skipped(St, Conn, Dom, Key, Triple, N);
loop_scenario(St, Conn, <<"db_outage_bounded_wakeup">>, Dom, Key, Triple, N) ->
    outage_bounded(St, Conn, Dom, Key, Triple, N).

attempts_kept(_St, Conn, ?ORD, Key, Triple, _N, Attempts) ->
    seed_job(Conn, Key, Triple, Attempts, retry_wait),
    {ok, _, [{Kept}]} = ?X:e(
        Conn, "SELECT attempts FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ?assertEqual(Attempts, Kept),
    #{
        attempts_before => Attempts,
        attempts_after => Kept,
        reset => false,
        queue => <<"ordinary">>
    };
attempts_kept(_St, Conn, ?ENT, Key, Triple, N, Attempts) ->
    %% claim 前收敛前序场景遗留活跃行（见 reap_cycle ?ENT 注释）。
    ?QP:drain_eb(Conn),
    AssetId = 952000000000 + N,
    ok = ?X:seed_eb_asset_row(Conn, Key, AssetId, Triple, <<"deleted">>, true, <<"live">>),
    ok = ?QP:enqueue_eb(Conn, ?ORG, 9102, AssetId, <<"w601a nl5">>),
    ok = target_first(Conn, AssetId),
    {ok, #{token := T1}} = ?QP:claim_eb_for(Conn, ?ORG, 9102, AssetId),
    {ok, _} = ?QP:ack_eb_ok(Conn, ?ORG, 9102, AssetId, T1, false),
    set_eb_attempts(Conn, AssetId, Attempts),
    {ok, _, [{Kept}]} = ?X:e(
        Conn,
        "SELECT attempts FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [AssetId]
    ),
    ?assertEqual(Attempts, Kept),
    #{
        attempts_before => Attempts,
        attempts_after => Kept,
        reset => false,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

attempt8_rejected(_St, Conn, ?ORD, Key, Triple, _N) ->
    ?QP:drain_ordinary(Conn),
    seed_job(Conn, Key, Triple, 8, blocked),
    {error, empty} = ?QP:claim_ordinary(Conn, ?X:token()),
    {ok, _, [{<<"blocked">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        attempts => 8,
        claim_result => <<"empty">>,
        state_after => <<"blocked">>,
        queue => <<"ordinary">>
    };
attempt8_rejected(_St, Conn, ?ENT, Key, Triple, N) ->
    %% claim 前收敛前序场景遗留活跃行（见 reap_cycle ?ENT 注释）；场景中段的
    %% drain_eb 保留——负向 claim 前的隔离断言。
    ?QP:drain_eb(Conn),
    AssetId = 952000000000 + N,
    ok = ?X:seed_eb_asset_row(Conn, Key, AssetId, Triple, <<"deleted">>, true, <<"live">>),
    ok = ?QP:enqueue_eb(Conn, ?ORG, 9102, AssetId, <<"w601a nl8">>),
    ok = target_first(Conn, AssetId),
    {ok, #{token := T1}} = ?QP:claim_eb_for(Conn, ?ORG, 9102, AssetId),
    {ok, _} = ?QP:ack_eb_ok(Conn, ?ORG, 9102, AssetId, T1, false),
    set_eb_attempts(Conn, AssetId, 8),
    ?X:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET job_state='blocked',"
        " last_error_code='max_attempts', lease_token=NULL, lease_until=NULL"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [AssetId]
    ),
    ?QP:drain_eb(Conn),
    empty = ?QP:claim_eb(Conn),
    {ok, _, [{<<"blocked">>}]} = ?QP:eb_state(Conn, ?ORG, AssetId),
    #{
        attempts => 8,
        claim_result => <<"empty">>,
        state_after => <<"blocked">>,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

blocked_skipped(_St, Conn, ?ORD, Key, Triple, _N) ->
    BlockedKey = <<Key/binary, "-blocked">>,
    ReadyKey = <<Key/binary, "-ready">>,
    seed_job(Conn, BlockedKey, Triple, 8, blocked),
    seed_job(Conn, ReadyKey, Triple, 0, ready),
    {ok, Consumed} =
        attachment_delete_worker:run_batch(Conn, fun(_, _, _, _) -> ok end),
    {ok, _, [{<<"blocked">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [BlockedKey]
    ),
    {ok, _, [{<<"done">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [ReadyKey]
    ),
    ?assert(Consumed >= 1),
    #{
        blocked_state => <<"blocked">>,
        ready_state => <<"done">>,
        consumed => Consumed,
        queue => <<"ordinary">>
    };
blocked_skipped(_St, Conn, ?ENT, Key, Triple, N) ->
    %% claim 前收敛前序场景遗留活跃行（见 reap_cycle ?ENT 注释）；场景中段的
    %% drain_eb 保留——负向 claim 前的隔离断言。
    ?QP:drain_eb(Conn),
    AssetId = 952000000000 + N,
    ok = ?X:seed_eb_asset_row(Conn, Key, AssetId, Triple, <<"deleted">>, true, <<"live">>),
    ok = ?QP:enqueue_eb(Conn, ?ORG, 9102, AssetId, <<"w601a blocked">>),
    ok = target_first(Conn, AssetId),
    {ok, #{token := T1}} = ?QP:claim_eb_for(Conn, ?ORG, 9102, AssetId),
    {ok, _} = ?QP:ack_eb_ok(Conn, ?ORG, 9102, AssetId, T1, false),
    set_eb_attempts(Conn, AssetId, 8),
    ?X:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET job_state='blocked',"
        " last_error_code='max_attempts', lease_token=NULL, lease_until=NULL"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [AssetId]
    ),
    ?QP:drain_eb(Conn),
    empty = ?QP:claim_eb(Conn),
    {ok, _, [{<<"blocked">>}]} = ?QP:eb_state(Conn, ?ORG, AssetId),
    #{
        blocked_state => <<"blocked">>,
        consumed => 0,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

%% DB 断连：claim 失败即止（有界失败，不无限快轮询）。
outage_bounded(St, _Conn, ?ORD, _Key, _Triple, _N) ->
    Closed = closed_connection(St),
    Result = guard_outage(fun() ->
        attachment_delete_worker:run_batch(Closed, fun(_, _, _, _) -> ok end)
    end),
    ok = assert_bounded(Result),
    #{outage_result => outcome_tag(Result), bounded => true, queue => <<"ordinary">>};
outage_bounded(St, _Conn, ?ENT, _Key, _Triple, _N) ->
    Closed = closed_connection(St),
    Result = guard_outage(fun() -> ?QP:claim_eb(Closed) end),
    ok = assert_bounded(Result),
    #{outage_result => outcome_tag(Result), bounded => true, queue => <<"enterprise">>};
outage_bounded(_St, _Conn, Dom, _Key, _Triple, _N) ->
    erlang:error({w601_unknown_domain, Dom}).

guard_outage(Fun) ->
    try
        Fun()
    catch
        Class:Reason -> {'EXIT', {Class, Reason}}
    end.

assert_bounded({'EXIT', _}) -> ok;
assert_bounded({error, _}) -> ok;
assert_bounded(empty) -> ok;
assert_bounded(Other) -> erlang:error({w601_outage_not_bounded, Other}).

outcome_tag({'EXIT', _}) -> <<"exit">>;
outcome_tag({error, _}) -> <<"error">>;
outcome_tag(_) -> <<"other">>.

closed_connection(St) ->
    DbMap = maps:get(db, St),
    Server = maps:get(server, DbMap),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{
            database => maps:get(db, DbMap), timeout => 10000
        }
    ),
    {ok, C2} = epgsql:connect(Opts),
    ok = epgsql:close(C2),
    C2.

%% ---------------------------------------------------- all-writer-compat

writer_scenario(_St, Conn, <<"supported_schema_writer">>, ?ORD, Key, Triple, _N) ->
    {ok, _, [{Key, <<"registered">>, _}]} = ?X:e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now()+interval '2 hours',"
        "now()+interval '1 hour',$3,$4,$5,$6)",
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
    #{
        writer => <<"fn_v5_register_pending">>,
        state => <<"registered">>,
        queue => <<"ordinary">>
    };
writer_scenario(_St, Conn, <<"supported_schema_writer">>, ?ENT, Key, Triple, N) ->
    AssetId = 952000000000 + N,
    ok = ?X:seed_eb_asset_row(Conn, Key, AssetId, Triple, <<"deleted">>, true, <<"live">>),
    ok = ?QP:enqueue_eb(Conn, ?ORG, 9102, AssetId, <<"w601a compat">>),
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [AssetId]
    ),
    #{
        writer => <<"fn_v5_intent_eb_delete">>,
        state => <<"enqueued">>,
        queue_rows => 1,
        asset_id => AssetId,
        queue => <<"enterprise">>
    };
writer_scenario(_St, Conn, <<"old_writer_incomplete_identity">>, ?ORD, Key, _T, _N) ->
    {error, #error{code = <<"23514">>}} = ?X:e(
        Conn,
        "INSERT INTO attachment_object_deletion (path,bucket,job_state)"
        " VALUES ($1,'b','ready')",
        [Key]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        writer => <<"direct_insert">>,
        rejected => <<"23514">>,
        rows => 0,
        queue => <<"ordinary">>
    };
writer_scenario(_St, Conn, <<"old_writer_incomplete_identity">>, ?ENT, Key, _T, N) ->
    AssetId = 952000000000 + N,
    {error, #error{code = Code}} = ?X:e(
        Conn,
        "INSERT INTO enterprise_asset_delete_queue"
        " (organization_id,workspace_id,asset_id,object_key,job_state)"
        " VALUES (9101,9102,$1,$2,'ready')",
        [AssetId, Key]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [AssetId]
    ),
    #{
        writer => <<"direct_insert">>,
        rejected => Code,
        rows => 0,
        asset_id => AssetId,
        queue => <<"enterprise">>
    }.

%% ------------------------------------------------------------- helpers

seed_job(Conn, Key, Triple, Attempts, State) ->
    ok = ?X:seed_ordinary_job_row(
        Conn,
        Key,
        Triple,
        Attempts,
        State,
        case State of
            blocked -> <<"max_attempts">>;
            _ -> null
        end
    ).

set_eb_attempts(Conn, AssetId, Attempts) ->
    ?X:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET attempts=$2"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [AssetId, Attempts]
    ),
    ok.

%% 与 ORD 分支既有防护同构（reap_cycle ORD 的 next_retry_at='-infinity'）：
%% 保留真实全局 claim 协议，仅将本夹具目标置为最先到期。fn_v5_claim_eb 按
%% ORDER BY next_retry_at 全局取最早行，而 fn_v5_reaper_eb 把过期租约收敛为
%% retry_wait(next_retry_at=now())——前序叶（如 startup/reap_cycle）留下的
%% retry_wait 行会先于本叶 enqueue 的 ready 行被选中，且失败叶 target_claim
%% 回滚后残留行原样保留，会持续劫持本组后续所有 ENT claim 叶。目标行置
%% '-infinity' 后 claim 必然先选它；迁移 190 守卫只锁 identity/done/attempts，
%% next_retry_at 的 UPDATE 合法。
target_first(Conn, AssetId) ->
    ?X:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET next_retry_at='-infinity'::timestamptz"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [AssetId]
    ),
    ok.

claim_eb_retry(Conn, AssetId) ->
    %% 置 '-infinity' 而非 now()-1s：库内残留 retry_wait 行的 next_retry_at
    %% （前序叶 reaper 收敛时刻）可能晚于也可能早于 now()-1s，只有 -infinity
    %% 才保证目标严格先于任何残留行被 fn_v5_claim_eb 选中。
    ?X:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET next_retry_at='-infinity'::timestamptz"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1"
        " AND job_state='retry_wait'",
        [AssetId]
    ),
    ?QP:claim_eb_for(Conn, 9101, 9102, AssetId).
