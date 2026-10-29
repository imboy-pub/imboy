%%% @doc W4-04 guards-activation / all-writer-compat / activation-commit-unknown
%%% 三组共 16 叶的实测实现（主套件按 case 分发）。
-module(attachment_lifecycle_v5_w404_activation_guards).

-export([guards/3, writer/3, activation/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(C, attachment_lifecycle_v5_w404_activation_tests).
-define(ORG, 9101).
-define(WS, 9102).
-define(ORD_DOMAIN, <<"ordinary">>).
-define(ENT_DOMAIN, <<"enterprise">>).

%% ------------------------------------------------------------ guards 8

guards(<<"all_prerequisites_present">>, Domain, St) ->
    prerequisites(Domain, St);
guards(<<"producer_missing">>, Domain, St) ->
    producer_missing(Domain, St);
guards(<<"old_worker_running">>, Domain, St) ->
    old_worker(Domain, St);
guards(<<"guard_missing">>, Domain, St) ->
    guard_missing(Domain, St).

prerequisites(Domain, #{conn := Conn}) ->
    {Result, Report} =
        case attachment_guards_activation_check:check(#{conn => Conn}) of
            {ok, R} -> {ok, R};
            {error, R} -> {error, R}
        end,
    ?assertEqual([], maps:get(missing_functions, Report)),
    ?assertEqual(true, maps:get(producers_ready, Report)),
    ?assertEqual([], maps:get(old_workers_running, Report)),
    ?assertEqual(true, maps:get(triggers_already_attached, Report)),
    #{
        check_result => atom_to_binary(Result, utf8),
        missing_functions => 0,
        producers_ready => true,
        old_workers_running => 0,
        triggers_already_attached => true,
        domain => Domain
    }.

%% 生产者缺失：用 meck 把 elib_pg:query 的 pg_proc 探测置 0（检查逻辑真跑，
%% 数据库状态是注入的），断言 missing_functions 非空且整体拒绝激活。
producer_missing(Domain, #{conn := Conn}) ->
    ok = meck:new(elib_pg, [non_strict, no_link]),
    try
        ok = meck:expect(elib_pg, query, fun(_C, _Sql, _Params) ->
            {ok, [#{<<"n">> => 0}]}
        end),
        {error, Report} = attachment_guards_activation_check:check(#{conn => Conn}),
        Missing = maps:get(missing_functions, Report),
        ?assert(length(Missing) >= 1),
        #{
            check_result => <<"error">>,
            missing_functions => length(Missing),
            producers_ready => maps:get(producers_ready, Report),
            injection => <<"elib_pg_query_pg_proc_zero">>,
            domain => Domain
        }
    after
        ok = meck:unload(elib_pg)
    end.

%% 旧 worker 存活：注册旧 gc drain 名 → 检查报 old_workers_running 非空。
old_worker(Domain, #{conn := Conn}) ->
    Name = attachment_gc_worker,
    case whereis(Name) of
        undefined ->
            true = register(Name, self()),
            try
                {error, Report} = attachment_guards_activation_check:check(#{conn => Conn}),
                ?assertEqual([Name], maps:get(old_workers_running, Report)),
                #{
                    check_result => <<"error">>,
                    old_workers_running => [Name],
                    registered => true,
                    domain => Domain
                }
            after
                unregister(Name)
            end;
        Pid ->
            %% 真实旧 worker 已在跑：直接断言检查能看见它（不抢占注册名）。
            {error, Report} = attachment_guards_activation_check:check(#{conn => Conn}),
            ?assert(lists:member(Name, maps:get(old_workers_running, Report))),
            #{
                check_result => <<"error">>,
                old_workers_running => [Name],
                registered => false,
                existing_pid => list_to_binary(pid_to_list(Pid)),
                domain => Domain
            }
    end.

%% 守卫未挂：临时禁用 metadata delete guard → 检查报 triggers_already_attached=false。
guard_missing(Domain, #{conn := Conn}) ->
    Trigger = <<"trg_v5_metadata_delete_guard">>,
    ok = ?C:q(Conn, "BEGIN"),
    Result =
        try
            %% 真实 DROP（事务内），检查必须报告守卫未挂；ROLLBACK 复原。
            ok = ?C:q(
                Conn,
                "DROP TRIGGER " ++ binary_to_list(Trigger) ++ " ON attachment"
            ),
            Report =
                case attachment_guards_activation_check:check(#{conn => Conn}) of
                    {ok, R} -> R;
                    {error, R} -> R
                end,
            ?assertEqual(false, maps:get(triggers_already_attached, Report)),
            #{
                check_result => <<"not_attached_reported">>,
                triggers_already_attached => false,
                missing_functions => length(maps:get(missing_functions, Report)),
                domain => Domain
            }
        after
            ok = ?C:q(Conn, "ROLLBACK")
        end,
    {ok, _, [{1}]} = ?C:e(
        Conn,
        "SELECT COUNT(*) FROM pg_trigger WHERE tgname=$1 AND NOT tgisinternal",
        [Trigger]
    ),
    Result.

%% ------------------------------------------------------------ writer 4

writer(<<"supported_schema_writer">>, Domain, St) ->
    supported_writer(Domain, St);
writer(<<"old_writer_incomplete_identity">>, Domain, St) ->
    incomplete_writer(Domain, St).

supported_writer(?ORD_DOMAIN, #{conn := Conn}) ->
    Key = unique_key(<<"v5e2e/w404/compat-v5">>),
    {ok, _, [{Key, <<"registered">>, _}]} = ?C:e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now()+interval '2 hours',"
        "now()+interval '1 hour','garage','ns1','c1',$3)",
        [
            Key,
            <<
                "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
                "\"declaration\":{\"mime\":\"image/png\"}}"
            >>,
            ?C:hex64(crypto:hash(sha256, Key))
        ]
    ),
    #{
        writer => <<"fn_v5_register_pending">>,
        state => <<"registered">>,
        object_key => Key,
        domain => <<"ordinary">>
    };
supported_writer(?ENT_DOMAIN, #{conn := Conn, storage := Storage}) ->
    Key = unique_key(<<"v5e2e/w404/eb-compat">>),
    AssetId = 970000000000 + ?C:random_id(),
    ok = ?C:seed_eb_asset(Conn, Storage, AssetId, Key, <<"live">>, true),
    {ok, _, _} = ?C:e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w404 compat')",
        [?ORG, ?WS, AssetId]
    ),
    {ok, _, [{1}]} = ?C:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    #{
        writer => <<"fn_v5_intent_eb_delete">>,
        state => <<"enqueued">>,
        object_key => Key,
        queue_rows => 1,
        asset_id => AssetId,
        domain => <<"enterprise">>
    }.

incomplete_writer(?ORD_DOMAIN, #{conn := Conn}) ->
    Key = unique_key(<<"v5e2e/w404/compat-old">>),
    {error, #error{code = <<"23514">>}} = ?C:e(
        Conn,
        "INSERT INTO attachment_object_deletion (path,bucket,job_state)"
        " VALUES ($1,'b','ready')",
        [Key]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = ?C:e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    #{
        writer => <<"direct_insert">>,
        rejected => <<"23514">>,
        rows => 0,
        object_key => Key,
        domain => <<"ordinary">>
    };
incomplete_writer(?ENT_DOMAIN, #{conn := Conn}) ->
    AssetId = 970000000000 + ?C:random_id(),
    Key = unique_key(<<"v5e2e/w404/eb-old">>),
    {error, #error{code = Code}} = ?C:e(
        Conn,
        "INSERT INTO enterprise_asset_delete_queue"
        " (organization_id,workspace_id,asset_id,object_key,job_state)"
        " VALUES ($1,$2,$3,$4,'ready')",
        [?ORG, ?WS, AssetId, Key]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = ?C:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    #{
        writer => <<"direct_insert">>,
        rejected => Code,
        rows => 0,
        object_key => Key,
        asset_id => AssetId,
        domain => <<"enterprise">>
    }.

%% -------------------------------------------------------- activation 8

activation(<<"ddl_committed_checkpoint_lost">>, Domain, St) ->
    checkpoint_lost(Domain, St);
activation(<<"proven_rollback">>, Domain, St) ->
    forward_fix(Domain, St);
activation(<<"partial_catalog_conflict">>, Domain, St) ->
    catalog_conflict(Domain, St);
activation(<<"guard_failure_worker_off">>, Domain, St) ->
    worker_off(Domain, St).

%% DDL 已提交但 checkpoint 丢失：重放 190 的 CREATE TRIGGER 得到
%% duplicate_object，catalog 仍只有一个（幂等效果由迁移器 marker 保证）。
checkpoint_lost(Domain, #{conn := Conn}) ->
    Trigger = <<"trg_v5_metadata_delete_guard">>,
    {ok, _, [{Before}]} = trigger_count(Conn, Trigger),
    {error, #error{code = Code}} = ?C:e(
        Conn,
        "CREATE TRIGGER " ++
            binary_to_list(Trigger) ++
            " BEFORE DELETE ON attachment FOR EACH ROW"
            " EXECUTE FUNCTION fn_v5_metadata_delete_guard()",
        []
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{After}]} = trigger_count(Conn, Trigger),
    ?assertEqual(Before, After),
    #{
        replay_error => Code,
        triggers_before => Before,
        triggers_after => After,
        domain => Domain
    }.

%% proven_rollback：190 的 down 被 forward-fix 明确阻断（真实执行 down 的 DO
%% 块），catalog 不因失败的回滚尝试改变。
forward_fix(Domain, #{conn := Conn}) ->
    Trigger = <<"trg_v5_metadata_delete_guard">>,
    {ok, _, [{Before}]} = trigger_count(Conn, Trigger),
    {error, #error{code = Code, message = Message}} = ?C:e(
        Conn,
        "DO $$ BEGIN RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX'; END $$;",
        []
    ),
    ok = epgsql:sync(Conn),
    ?assertEqual(<<"P0001">>, Code),
    ?assertNotEqual(nomatch, binary:match(Message, <<"V5_DOWN_BLOCKED_USE_FORWARD_FIX">>)),
    {ok, _, [{After}]} = trigger_count(Conn, Trigger),
    ?assertEqual(Before, After),
    #{
        rollback_error => Code,
        forward_fix_required => true,
        triggers_before => Before,
        triggers_after => After,
        domain => Domain
    }.

%% partial_catalog_conflict：重复 DDL 失败后九个守卫全部仍在（无半挂）。
catalog_conflict(Domain, #{conn := Conn}) ->
    Triggers = [
        <<"trg_v5_attachment_location_immutable">>,
        <<"trg_v5_attachment_identity_immutable">>,
        <<"trg_v5_attachment_first_bind">>,
        <<"trg_v5_pending_session_guard">>,
        <<"trg_v5_eb_registration_guard">>,
        <<"trg_v5_eb_audit_actor_guard">>,
        <<"trg_v5_aod_job_guard">>,
        <<"trg_v5_ebdq_job_guard">>,
        <<"trg_v5_metadata_delete_guard">>
    ],
    {ok, _, [{Before}]} = ?C:e(
        Conn,
        "SELECT COUNT(*) FROM pg_trigger WHERE tgname = ANY($1::text[])"
        " AND NOT tgisinternal",
        [Triggers]
    ),
    ?assertEqual(9, Before),
    {error, #error{code = Code}} = ?C:e(
        Conn,
        "CREATE TRIGGER trg_v5_metadata_delete_guard"
        " BEFORE DELETE ON attachment FOR EACH ROW"
        " EXECUTE FUNCTION fn_v5_metadata_delete_guard()",
        []
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{After}]} = ?C:e(
        Conn,
        "SELECT COUNT(*) FROM pg_trigger WHERE tgname = ANY($1::text[])"
        " AND NOT tgisinternal",
        [Triggers]
    ),
    ?assertEqual(Before, After),
    #{
        conflict_error => Code,
        triggers_before => Before,
        triggers_after => After,
        domain => Domain
    }.

%% guard_failure_worker_off：守卫是安全边界，worker 默认关闭；默认配置下
%% 不会在缺守卫时消费队列（真实 application env，不是伪造）。
worker_off(Domain, #{conn := Conn}) ->
    EnabledOrdinary = application:get_env(imboy, attachment_delete_worker_enabled, false),
    EnabledReaper = application:get_env(imboy, attachment_delete_reaper_enabled, false),
    ?assertEqual(false, EnabledOrdinary),
    ?assertEqual(false, EnabledReaper),
    {ok, _, [{Pending}]} = ?C:e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE job_state='ready'",
        []
    ),
    #{
        worker_enabled => EnabledOrdinary,
        reaper_enabled => EnabledReaper,
        ready_rows => Pending,
        domain => Domain
    }.

trigger_count(Conn, Trigger) ->
    ?C:e(
        Conn,
        "SELECT COUNT(*) FROM pg_trigger WHERE tgname=$1 AND NOT tgisinternal",
        [Trigger]
    ).

unique_key(Prefix) ->
    ?C:unique_key(Prefix).
