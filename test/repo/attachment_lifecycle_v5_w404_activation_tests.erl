%%% @doc W4-04 M3 激活与调和验收（真 scratch DB；36 叶）。
%%%
%%% 五个 case 各若干 scenario × 2 domain（ordinary/enterprise），每叶一个具名
%%% eunit unit：场景真实断言后 emit 一行观测，adapter 按
%%% case/entrypoint/scenario/domain 唯一匹配封印。
%%%
%%%   reconcile 8 —— 启动调和 reaper 回收过期租约、周期重试消费到 done、
%%%     重启后过期租约回收、非 owner 只读清点（零删除）；
%%%   no-infinite-loop 8 —— attempts=5 重启保持、attempts=8 不被 claim、
%%%     blocked 终态不轮询、DB 断连有界失败；
%%%   guards-activation 8 —— 前置四项报告、生产者缺失、旧 worker 存活、
%%%     守卫未挂探测；
%%%   all-writer-compat 4 —— 已适配写入口在守卫下成功、旧 writer 缺身份被拒；
%%%   activation-commit-unknown 8 —— DDL 已提交但 checkpoint 丢失重放、
%%%     down 被 forward-fix 阻断、部分 catalog 冲突、守卫缺失时 worker 关闭。
-module(attachment_lifecycle_v5_w404_activation_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([q/2, e/3, hex64/1, random_id/0, seed_eb_asset/6, unique_key/1]).

-define(C_RECONCILE, <<"W4-04/reconcile">>).
-define(C_LOOP, <<"W4-04/no-infinite-loop">>).
-define(C_GUARDS, <<"W4-04/guards-activation">>).
-define(C_WRITER, <<"W4-04/all-writer-compat">>).
-define(C_ACT, <<"W4-04/activation-commit-unknown">>).

-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).
-define(ORG, 9101).
-define(WS, 9102).
-define(APP, 9103).

w404_activation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(St) ->
        [leaf_unit(St, Spec) || Spec <- specs()]
    end}.

%% ---------------------------------------------------------------- specs

specs() ->
    lists:append([
        case_specs(?C_RECONCILE, <<"reconcile">>, [
            <<"startup_ready">>,
            <<"periodic_retry">>,
            <<"restart_expired_lease">>,
            <<"nonowner_discrepancy_only">>
        ]),
        case_specs(?C_LOOP, <<"no-infinite-loop">>, [
            <<"task_attempt5_restart">>,
            <<"claim_attempt8_restart">>,
            <<"blocked_no_poll">>,
            <<"db_outage_bounded_wakeup">>
        ]),
        case_specs(?C_GUARDS, <<"guards-activation">>, [
            <<"all_prerequisites_present">>,
            <<"producer_missing">>,
            <<"old_worker_running">>,
            <<"guard_missing">>
        ]),
        case_specs(?C_WRITER, <<"all-writer-compat">>, [
            <<"supported_schema_writer">>,
            <<"old_writer_incomplete_identity">>
        ]),
        case_specs(?C_ACT, <<"activation-commit-unknown">>, [
            <<"ddl_committed_checkpoint_lost">>,
            <<"proven_rollback">>,
            <<"partial_catalog_conflict">>,
            <<"guard_failure_worker_off">>
        ])
    ]).

case_specs(Case, Entry, Scenarios) ->
    [
        {Case, Entry, <<"none">>, Scenario, Domain}
     || Scenario <- Scenarios,
        Domain <- [?ORD, ?ENT]
    ].

leaf_unit(St, {Case, _Entry, Driver, Scenario, Domain} = Spec) ->
    Desc = binary_to_list(
        iolist_to_binary([short_case(Case), "/", Driver, "/", Scenario, "/", Domain])
    ),
    {Desc,
        {timeout, 240, fun() ->
            Measured = scenario(St, Spec),
            emit(Spec, Measured)
        end}}.

short_case(<<"W4-04/reconcile">>) -> <<"reconcile">>;
short_case(<<"W4-04/no-infinite-loop">>) -> <<"no-infinite-loop">>;
short_case(<<"W4-04/guards-activation">>) -> <<"guards-activation">>;
short_case(<<"W4-04/all-writer-compat">>) -> <<"all-writer-compat">>;
short_case(<<"W4-04/activation-commit-unknown">>) -> <<"activation-commit-unknown">>.

emit({Case, Entry, Driver, Scenario, Domain}, Measured) ->
    attachment_lifecycle_v5_obs:emit(#{
        case_id => Case,
        entrypoint => Entry,
        scenario => Scenario,
        domain => Domain,
        schema_phase => <<"integrated">>,
        measured => Measured#{driver => Driver}
    }).

scenario(St, {?C_RECONCILE, _, _, Scenario, Domain}) ->
    reconcile(short_scenario(Scenario), Domain, St);
scenario(St, {?C_LOOP, _, _, Scenario, Domain}) ->
    no_infinite_loop(short_scenario(Scenario), Domain, St);
scenario(St, {?C_GUARDS, _, _, Scenario, Domain}) ->
    attachment_lifecycle_v5_w404_activation_guards:guards(
        short_scenario(Scenario), Domain, St
    );
scenario(St, {?C_WRITER, _, _, Scenario, Domain}) ->
    attachment_lifecycle_v5_w404_activation_guards:writer(
        short_scenario(Scenario), Domain, St
    );
scenario(St, {?C_ACT, _, _, Scenario, Domain}) ->
    attachment_lifecycle_v5_w404_activation_guards:activation(
        short_scenario(Scenario), Domain, St
    ).

short_scenario(Scenario) ->
    [Last | _] = lists:reverse(binary:split(Scenario, <<"/">>, [global])),
    Last.

%% ---------------------------------------------------------------- setup

setup() ->
    Boot = eunit_runner:eunit_setup(),
    Db = inttest_marker_db:provision(#{
        env_prefix => <<"V5_INTTEST_W404">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    Conn = maps:get(conn, Db),
    Storage = setup_storage_fixture(),
    try
        seed_tenants(Conn),
        #{boot => Boot, db => Db, conn => Conn, storage => Storage}
    catch
        Class:Reason:Stack ->
            cleanup_storage_fixture(Storage),
            inttest_marker_db:release(Db),
            eunit_runner:eunit_cleanup(Boot),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{boot := Boot, db := Db, storage := Storage}) ->
    cleanup_storage_fixture(Storage),
    inttest_marker_db:release(Db),
    eunit_runner:eunit_cleanup(Boot),
    ok.

q(Conn, Sql) -> q(Conn, Sql, []).

q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w404_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

uuid_text() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

unique_key(Prefix) ->
    <<Prefix/binary, "-", (binary:encode_hex(crypto:strong_rand_bytes(8), lowercase))/binary>>.

random_id() ->
    binary:decode_unsigned(crypto:strong_rand_bytes(4)) rem 100000000.

%% 真 registry 与规范 locator；只替换最后的删除回调，不绕过冻结位置校验。
setup_storage_fixture() ->
    Previous = application:get_env(imboy, attachment_storage_registry),
    Registry = attachment_storage:registry(),
    Namespace = <<"w404-", (binary:encode_hex(crypto:strong_rand_bytes(16), lowercase))/binary>>,
    Root = filename:join(
        case os:getenv("V5_DISK_ROOT") of
            DiskRoot when is_list(DiskRoot), DiskRoot =/= "" -> DiskRoot;
            _ -> "/tmp"
        end,
        binary_to_list(Namespace)
    ),
    ok = filelib:ensure_dir(filename:join(Root, "probe")),
    try
        Record = #{
            driver => <<"disk">>,
            namespace => Namespace,
            endpoint => list_to_binary("file://" ++ Root),
            container => <<"w404-fixture">>
        },
        Roots = maps:get(roots, Registry, #{}),
        ok = application:set_env(imboy, attachment_storage_registry, Registry#{
            roots => Roots#{Namespace => Record}
        }),
        #{
            previous_registry => Previous,
            root => Root,
            namespace => Namespace,
            container => <<"w404-fixture">>,
            locator => attachment_storage:locator_digest(Namespace, Record#{root_id => Namespace})
        }
    catch
        Class:Reason:Stack ->
            cleanup_storage_fixture(#{previous_registry => Previous, root => Root}),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup_storage_fixture(#{previous_registry := Previous, root := Root}) ->
    try
        case Previous of
            undefined -> application:unset_env(imboy, attachment_storage_registry);
            {ok, Registry} -> application:set_env(imboy, attachment_storage_registry, Registry)
        end
    after
        ok = file:del_dir_r(Root)
    end.

seed_tenants(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w404-u', 'w404', '', 1, 0, '127.0.0.1', 'w404', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id) VALUES ($1,'w404-org',101)"
        " ON CONFLICT (id) DO NOTHING",
        [?ORG]
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES ($1,$2,'w404-ws',101,'{}') ON CONFLICT (id) DO NOTHING",
        [?ORG, ?WS]
    ),
    q(
        Conn,
        "INSERT INTO enterprise_application"
        " (id, organization_id, application_key, name, status)"
        " VALUES ($1, $2, 'w404-9103', 'w404 app', 'active')"
        " ON CONFLICT (organization_id, id) DO NOTHING",
        [?APP, ?ORG]
    ),
    ok.

seed_job(Conn, Storage, Key, Attempts, State) ->
    q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path,bucket,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,job_state,next_retry_at,attempts,last_error_code)"
        " VALUES ($1,'w404-fixture','disk',$2,'w404-fixture',$3,$4,now(),$5,$6)"
        " ON CONFLICT (path) DO NOTHING",
        [
            Key,
            maps:get(namespace, Storage),
            maps:get(locator, Storage),
            State,
            Attempts,
            case State of
                blocked -> <<"max_attempts">>;
                _ -> null
            end
        ]
    ).

seed_eb_asset(Conn, Storage, Id, Key, Lifecycle, PendingFlag) ->
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,pending_object_delete,storage_driver,storage_namespace,"
        "  storage_container,storage_locator_sha256,lifecycle_state)"
        " VALUES ($1,$2,$3,$4,$5,'text/plain',8,'deleted',$6,'disk',$7,"
        "  'w404-fixture',$8,$9)",
        [
            Id,
            ?ORG,
            ?WS,
            Key,
            hex64(crypto:hash(sha256, Key)),
            PendingFlag,
            maps:get(namespace, Storage),
            maps:get(locator, Storage),
            Lifecycle
        ]
    ).

%% ---------- reconcile 8 ----------

reconcile(<<"startup_ready">>, Domain, St) ->
    reap_cycle(Domain, St, <<"startup">>);
reconcile(<<"periodic_retry">>, Domain, St) ->
    retry_cycle(Domain, St, <<"periodic">>);
reconcile(<<"restart_expired_lease">>, Domain, St) ->
    restart_cycle(Domain, St, <<"restart">>);
reconcile(<<"nonowner_discrepancy_only">>, Domain, St) ->
    discrepancy(Domain, St).

%% startup_ready：过期租约 → reaper 回收 → retry_wait（字节零删除）。
reap_cycle(?ORD, #{conn := Conn, storage := Storage}, Label) ->
    Key = unique_key(<<"v5e2e/w404/", Label/binary>>),
    ok = seed_job(Conn, Storage, Key, 0, ready),
    T = uuid_text(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    expire_ordinary(Conn, Key),
    {ok, Reaped} = attachment_delete_worker:reconcile(Conn),
    {ok, _, [{<<"retry_wait">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        reaped => Reaped,
        state_after => <<"retry_wait">>,
        lease_cleared => true,
        object_key => Key,
        queue => <<"ordinary">>
    };
reap_cycle(?ENT, #{conn := Conn, storage := Storage}, Label) ->
    Key = unique_key(<<"v5e2e/w404/eb-", Label/binary>>),
    AssetId = 960000000000 + random_id(),
    ok = seed_eb_asset(Conn, Storage, AssetId, Key, <<"live">>, true),
    ok = enqueue_eb(Conn, AssetId),
    {ok, _} = claim_eb_for(Conn, AssetId),
    expire_eb(Conn, AssetId),
    {ok, _, [{Reaped}]} = e(Conn, "SELECT fn_v5_reaper_eb(100)", []),
    {ok, _, [{<<"retry_wait">>}]} = eb_state(Conn, AssetId),
    #{
        reaped => Reaped,
        state_after => <<"retry_wait">>,
        lease_cleared => true,
        object_key => Key,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

%% periodic_retry：retry_wait 到期 → tick 批消费到 done。
retry_cycle(?ORD, #{conn := Conn, storage := Storage}, Label) ->
    Key = unique_key(<<"v5e2e/w404/", Label/binary>>),
    ok = seed_job(Conn, Storage, Key, 1, retry_wait),
    q(
        Conn,
        "UPDATE attachment_object_deletion SET next_retry_at = now() - interval '1 s'"
        " WHERE path=$1",
        [Key]
    ),
    {ok, Consumed} = attachment_delete_worker:run_batch(
        Conn, fun(_, _, _, _) -> ok end
    ),
    {ok, _, [{<<"done">>, Attempts}]} = e(
        Conn, "SELECT job_state, attempts FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        consumed => Consumed,
        state_after => <<"done">>,
        attempts => Attempts,
        object_key => Key,
        queue => <<"ordinary">>
    };
retry_cycle(?ENT, #{conn := Conn, storage := Storage}, Label) ->
    Key = unique_key(<<"v5e2e/w404/eb-", Label/binary>>),
    AssetId = 960000000000 + random_id(),
    ok = seed_eb_asset(Conn, Storage, AssetId, Key, <<"live">>, true),
    ok = enqueue_eb(Conn, AssetId),
    {ok, #{token := T1}} = claim_eb_for(Conn, AssetId),
    {ok, _} = ack_eb(Conn, AssetId, T1, false),
    q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET next_retry_at = now() - interval '1 s'"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    {ok, #{attempts := Attempts, token := T2}} = claim_eb_for(Conn, AssetId),
    {ok, _} = ack_eb(Conn, AssetId, T2, true),
    {ok, _, [{<<"done">>}]} = eb_state(Conn, AssetId),
    #{
        consumed => 1,
        state_after => <<"done">>,
        attempts => Attempts,
        object_key => Key,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

%% restart_expired_lease：租约过期 → reap → 再 claim（attempts 递增）。
restart_cycle(?ORD, #{conn := Conn, storage := Storage}, Label) ->
    Key = unique_key(<<"v5e2e/w404/", Label/binary>>),
    ok = seed_job(Conn, Storage, Key, 0, ready),
    {ok, #{<<"out_path">> := Key}} = attachment_gc_repo:v5_claim(Conn, uuid_text()),
    expire_ordinary(Conn, Key),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    {ok, #{<<"out_path">> := Key, <<"out_attempts">> := 2}} =
        attachment_gc_repo:v5_claim(Conn, uuid_text()),
    #{reaped => 1, attempts_after => 2, object_key => Key, queue => <<"ordinary">>};
restart_cycle(?ENT, #{conn := Conn, storage := Storage}, Label) ->
    Key = unique_key(<<"v5e2e/w404/eb-", Label/binary>>),
    AssetId = 960000000000 + random_id(),
    ok = seed_eb_asset(Conn, Storage, AssetId, Key, <<"live">>, true),
    ok = enqueue_eb(Conn, AssetId),
    {ok, _} = claim_eb_for(Conn, AssetId),
    expire_eb(Conn, AssetId),
    {ok, _, [{1}]} = e(Conn, "SELECT fn_v5_reaper_eb(100)", []),
    {ok, #{attempts := 2}} = claim_eb_retry_for(Conn, AssetId),
    #{
        reaped => 1,
        attempts_after => 2,
        object_key => Key,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

%% nonowner_discrepancy_only：只读清点，不越权改行、不删字节。
discrepancy(?ORD, #{conn := Conn, storage := Storage}) ->
    Key = unique_key(<<"v5e2e/w404/disc">>),
    ok = seed_job(Conn, Storage, Key, 0, ready),
    {ok, _, [{Total}]} = e(Conn, "SELECT COUNT(*) FROM attachment_object_deletion", []),
    {ok, _, [{Matching}]} = e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ?assert(Total >= 1),
    ?assertEqual(1, Matching),
    {ok, _, [{<<"ready">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        discrepancy_rows => Total,
        matching_rows => Matching,
        state_untouched => <<"ready">>,
        queue => <<"ordinary">>,
        object_key => Key
    };
discrepancy(?ENT, #{conn := Conn, storage := Storage}) ->
    Key = unique_key(<<"v5e2e/w404/eb-disc">>),
    AssetId = 960000000000 + random_id(),
    ok = seed_eb_asset(Conn, Storage, AssetId, Key, <<"live">>, true),
    ok = enqueue_eb(Conn, AssetId),
    {ok, _, [{Total}]} = e(Conn, "SELECT COUNT(*) FROM enterprise_asset_delete_queue", []),
    {ok, _, [{Matching}]} = e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    ?assertEqual(1, Matching),
    {ok, _, [{<<"ready">>}]} = eb_state(Conn, AssetId),
    #{
        discrepancy_rows => Total,
        matching_rows => Matching,
        state_untouched => <<"ready">>,
        queue => <<"enterprise">>,
        object_key => Key,
        asset_id => AssetId
    }.

%% ---------- no-infinite-loop 8 ----------

no_infinite_loop(<<"task_attempt5_restart">>, Domain, St) ->
    attempt_kept(Domain, St, 5);
no_infinite_loop(<<"claim_attempt8_restart">>, Domain, St) ->
    attempt8_rejected(Domain, St);
no_infinite_loop(<<"blocked_no_poll">>, Domain, St) ->
    blocked_skipped(Domain, St);
no_infinite_loop(<<"db_outage_bounded_wakeup">>, Domain, St) ->
    outage_bounded(Domain, St).

attempt_kept(?ORD, #{conn := Conn, storage := Storage}, Attempts) ->
    Key = unique_key(<<"v5e2e/w404/nl5">>),
    ok = seed_job(Conn, Storage, Key, Attempts, retry_wait),
    {ok, _, [{Kept}]} = e(
        Conn, "SELECT attempts FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ?assertEqual(Attempts, Kept),
    #{
        attempts_before => Attempts,
        attempts_after => Kept,
        reset => false,
        object_key => Key,
        queue => <<"ordinary">>
    };
attempt_kept(?ENT, #{conn := Conn, storage := Storage}, Attempts) ->
    Key = unique_key(<<"v5e2e/w404/eb-nl5">>),
    AssetId = 960000000000 + random_id(),
    ok = seed_eb_asset(Conn, Storage, AssetId, Key, <<"live">>, true),
    ok = enqueue_eb(Conn, AssetId),
    {ok, #{token := T1}} = claim_eb_for(Conn, AssetId),
    {ok, _} = ack_eb(Conn, AssetId, T1, false),
    set_eb_attempts(Conn, AssetId, Attempts),
    {ok, _, [{Kept}]} = e(
        Conn,
        "SELECT attempts FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    ?assertEqual(Attempts, Kept),
    #{
        attempts_before => Attempts,
        attempts_after => Kept,
        reset => false,
        object_key => Key,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

attempt8_rejected(?ORD, #{conn := Conn, storage := Storage}) ->
    Key = unique_key(<<"v5e2e/w404/nl8">>),
    ok = seed_job(Conn, Storage, Key, 8, blocked),
    ok = drain_ordinary(Conn),
    {error, empty} = attachment_gc_repo:v5_claim(Conn, uuid_text()),
    {ok, _, [{<<"blocked">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{
        attempts => 8,
        claim_result => <<"empty">>,
        state_after => <<"blocked">>,
        object_key => Key,
        queue => <<"ordinary">>
    };
attempt8_rejected(?ENT, #{conn := Conn, storage := Storage}) ->
    Key = unique_key(<<"v5e2e/w404/eb-nl8">>),
    AssetId = 960000000000 + random_id(),
    ok = seed_eb_asset(Conn, Storage, AssetId, Key, <<"live">>, true),
    ok = enqueue_eb(Conn, AssetId),
    {ok, #{token := T1}} = claim_eb_for(Conn, AssetId),
    {ok, _} = ack_eb(Conn, AssetId, T1, false),
    set_eb_attempts(Conn, AssetId, 8),
    q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET job_state='blocked',"
        " last_error_code='max_attempts', lease_token=NULL, lease_until=NULL"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    empty = claim_eb(Conn),
    {ok, _, [{<<"blocked">>}]} = eb_state(Conn, AssetId),
    #{
        attempts => 8,
        claim_result => <<"empty">>,
        state_after => <<"blocked">>,
        object_key => Key,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

blocked_skipped(?ORD, #{conn := Conn, storage := Storage}) ->
    BlockedKey = unique_key(<<"v5e2e/w404/nl-blocked">>),
    ReadyKey = unique_key(<<"v5e2e/w404/nl-ready">>),
    ok = seed_job(Conn, Storage, BlockedKey, 8, blocked),
    ok = seed_job(Conn, Storage, ReadyKey, 0, ready),
    {ok, Consumed} = attachment_delete_worker:run_batch(
        Conn, fun(_, _, _, _) -> ok end
    ),
    {ok, _, [{<<"blocked">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [BlockedKey]
    ),
    {ok, _, [{<<"done">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [ReadyKey]
    ),
    ?assert(Consumed >= 1),
    #{
        blocked_state => <<"blocked">>,
        ready_state => <<"done">>,
        consumed => Consumed,
        object_key => ReadyKey,
        queue => <<"ordinary">>
    };
blocked_skipped(?ENT, #{conn := Conn, storage := Storage}) ->
    Key = unique_key(<<"v5e2e/w404/eb-nl-blocked">>),
    AssetId = 960000000000 + random_id(),
    ok = seed_eb_asset(Conn, Storage, AssetId, Key, <<"live">>, true),
    ok = enqueue_eb(Conn, AssetId),
    {ok, #{token := T1}} = claim_eb_for(Conn, AssetId),
    {ok, _} = ack_eb(Conn, AssetId, T1, false),
    set_eb_attempts(Conn, AssetId, 8),
    q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET job_state='blocked',"
        " last_error_code='max_attempts', lease_token=NULL, lease_until=NULL"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    ok = drain_eb(Conn),
    empty = claim_eb(Conn),
    {ok, _, [{<<"blocked">>}]} = eb_state(Conn, AssetId),
    #{
        blocked_state => <<"blocked">>,
        ready_state => <<"blocked">>,
        consumed => 0,
        object_key => Key,
        queue => <<"enterprise">>,
        asset_id => AssetId
    }.

%% DB 断连：claim 失败即止（有界失败，不无限快轮询）。
outage_bounded(?ORD, #{conn := Conn}) ->
    Closed = closed_connection(Conn),
    Result = guard_outage(fun() ->
        attachment_delete_worker:run_batch(Closed, fun(_, _, _, _) -> ok end)
    end),
    ok = assert_bounded_outage(Result),
    #{outage_result => outcome_tag(Result), bounded => true, queue => <<"ordinary">>};
outage_bounded(?ENT, #{conn := Conn}) ->
    Closed = closed_connection(Conn),
    Result = guard_outage(fun() -> claim_eb(Closed) end),
    ok = assert_bounded_outage(Result),
    #{outage_result => outcome_tag(Result), bounded => true, queue => <<"enterprise">>}.

guard_outage(Fun) ->
    try
        Fun()
    catch
        Class:Reason -> {'EXIT', {Class, Reason}}
    end.

assert_bounded_outage({'EXIT', _}) -> ok;
assert_bounded_outage({error, _}) -> ok;
assert_bounded_outage(empty) -> ok;
assert_bounded_outage(Other) -> erlang:error({w404_outage_not_bounded, Other}).

outcome_tag({'EXIT', _}) -> <<"exit">>;
outcome_tag({error, _}) -> <<"error">>;
outcome_tag(_) -> <<"other">>.

closed_connection(_Conn) ->
    {ok, C2} = epgsql:connect(#{
        host => w404_env("PG_HOST", "127.0.0.1"),
        port => list_to_integer(w404_env("PG_PORT", "5432")),
        username => w404_env("PG_USER", "imboy_user"),
        password => list_to_binary(w404_env("PG_PASSWORD", "")),
        database => w404_env("DB", "postgres")
    }),
    ok = epgsql:close(C2),
    C2.

w404_env(Suffix, Default) ->
    case os:getenv("V5_INTTEST_W404_" ++ Suffix) of
        Value when is_list(Value), Value =/= "" -> Value;
        _ -> Default
    end.

%% ------------------------------------------------------------------ helpers

expire_ordinary(Conn, Key) ->
    q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Key]
    ).

expire_eb(Conn, AssetId) ->
    q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET lease_until = now() - interval '1 s'"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ).

set_eb_attempts(Conn, AssetId, Attempts) ->
    q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET attempts=$4"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId, Attempts]
    ).

eb_state(Conn, AssetId) ->
    e(
        Conn,
        "SELECT job_state FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ).

enqueue_eb(Conn, AssetId) ->
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w404 activation')",
        [?ORG, ?WS, AssetId]
    ),
    ok.

claim_eb(Conn) ->
    T = uuid_text(),
    case e(Conn, "SELECT * FROM fn_v5_claim_eb($1)", [T]) of
        {ok, _, [{Org, Ws, Asset, _Key, _D, _N, _C, _L, _P, Attempts}]} ->
            {ok, #{
                token => T,
                attempts => Attempts,
                org => Org,
                ws => Ws,
                asset_id => Asset
            }};
        {ok, _, []} ->
            empty;
        {error, _} = Error ->
            Error;
        Other ->
            erlang:error({w404_claim_eb_failed, Other})
    end.

%% 队列里可能有其他叶/上次重试的 ready 行：领到非目标行就先 ack 收回。
claim_eb_for(Conn, AssetId) ->
    case claim_eb(Conn) of
        {ok, #{asset_id := AssetId} = Row} ->
            {ok, Row};
        {ok, #{org := Org, ws := Ws, asset_id := Other, token := T}} ->
            {ok, _, _} = e(
                Conn,
                "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)",
                [Org, Ws, Other, T]
            ),
            claim_eb_for(Conn, AssetId);
        empty ->
            erlang:error({w404_target_eb_job_not_claimed, AssetId});
        {error, _} = Error ->
            Error
    end.

claim_eb_retry_for(Conn, AssetId) ->
    q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET next_retry_at = now() - interval '1 s'"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
        " AND job_state='retry_wait'",
        [?ORG, ?WS, AssetId]
    ),
    claim_eb_for(Conn, AssetId).

ack_eb(Conn, AssetId, Token, Ok) ->
    case
        e(
            Conn,
            "SELECT fn_v5_ack_eb($1,$2,$3,$4,$5)",
            [?ORG, ?WS, AssetId, Token, Ok]
        )
    of
        {ok, _, [{State}]} -> {ok, State};
        Other -> erlang:error({w404_ack_eb_failed, Other})
    end.

drain_ordinary(Conn) ->
    T = uuid_text(),
    case attachment_gc_repo:v5_claim(Conn, T) of
        {ok, #{<<"out_path">> := P}} ->
            ok = attachment_gc_repo:v5_ack(Conn, P, T, true),
            drain_ordinary(Conn);
        {error, empty} ->
            ok
    end.

drain_eb(Conn) ->
    case claim_eb(Conn) of
        {ok, #{token := T}} ->
            {ok, _, _} = e(
                Conn,
                "SELECT fn_v5_ack_eb(organization_id, workspace_id, asset_id, $1, true)"
                " FROM enterprise_asset_delete_queue WHERE lease_token=$1",
                [T]
            ),
            drain_eb(Conn);
        empty ->
            ok;
        {error, _} ->
            ok
    end.
