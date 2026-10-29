%%% @doc W4-03 disk-predecessor-protocols（66 叶）。
%%%
%%% W4-01 intent-rollback（12 入口 × 4 故障 = 48）与 W4-02 四组协议
%%% （worker-crash 6、worker-db-fault 4、queue-adoption 4、
%%% ordinary-queue-recovery 4）在真实 disk driver 下的重放。每叶一个具名
%%% eunit unit：场景断言全过后 emit 一行观测，adapter 再按
%%% case/entrypoint/scenario/domain/driver 唯一匹配封印。
%%%
%%% driver 恒为 disk：registry 指向 run-owned V5_DISK_ROOT；队列行、attachment
%%% 行与 enterprise_asset 行的位置三元组都写 disk/namespace/container，删除经
%%% `attachment_storage:delete_snapshot/1` 真实落盘。
-module(attachment_lifecycle_v5_w403_predecessor_tests).

-include_lib("eunit/include/eunit.hrl").

-define(C, <<"W4-03/disk-predecessor-protocols">>).
-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).
-define(SUP, attachment_lifecycle_v5_w403_predecessor_support).

w403_predecessor_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(St) ->
        [leaf_unit(St, Spec) || Spec <- specs()]
    end}.

%% ---------------------------------------------------------------- specs

%% 66 = 48 + 6 + 4 + 4 + 4；scenario 是完整 ordering_or_fault 字面量，
%% 与 execution-contract expected_leaf_matrix 逐字一致。
specs() ->
    Intents = [
        {?C, Entry, <<"disk">>, Scenario, Domain}
     || {Entry, Domain} <- intent_entrypoints(),
        Scenario <- [
            <<"W4-01/intent-rollback/intent_insert_error">>,
            <<"W4-01/intent-rollback/audit_insert_error">>,
            <<"W4-01/intent-rollback/sql_rollback">>,
            <<"W4-01/intent-rollback/commit_response_lost">>
        ]
    ],
    Crash = [
        {?C, <<"worker-crash">>, <<"disk">>, Scenario, Domain}
     || Domain <- [?ORD, ?ENT],
        Scenario <- [
            <<"W4-02/worker-crash/kill_before_claim_commit">>,
            <<"W4-02/worker-crash/kill_after_claim_before_delete">>,
            <<"W4-02/worker-crash/kill_after_delete_before_completion">>
        ]
    ],
    DbFault = [
        {?C, <<"worker-db-fault">>, <<"disk">>, Scenario, Domain}
     || Domain <- [?ORD, ?ENT],
        Scenario <- [
            <<"W4-02/worker-db-fault/db_lost_before_claim_verified">>,
            <<"W4-02/worker-db-fault/db_lost_after_delete">>
        ]
    ],
    Adoption = [
        {?C, <<"queue-adoption">>, <<"disk">>, Scenario, ?ENT}
     || Scenario <- [
            <<"W4-02/queue-adoption/enterprise_pending_adoption">>,
            <<"W4-02/queue-adoption/enterprise_intent_replay">>,
            <<"W4-02/queue-adoption/ordinary_pk_preserved">>,
            <<"W4-02/queue-adoption/cross_queue_identity_conflict">>
        ]
    ],
    Recovery = [
        {?C, <<"ordinary-queue-recovery">>, <<"disk">>, Scenario, ?ORD}
     || Scenario <- [
            <<"W4-02/ordinary-queue-recovery/pending_recovery">>,
            <<"W4-02/ordinary-queue-recovery/orphan_recovery">>,
            <<"W4-02/ordinary-queue-recovery/confirmed_fk_preserved">>,
            <<"W4-02/ordinary-queue-recovery/done_path_reuse_rejected">>
        ]
    ],
    Intents ++ Crash ++ DbFault ++ Adoption ++ Recovery.

%% ordinary 9 入口 × 4 = 36；enterprise 3 入口 × 4 = 12。
intent_entrypoints() ->
    [
        {<<"reject-size">>, ?ORD},
        {<<"reject-mime">>, ?ORD},
        {<<"reject-teaching">>, ?ORD},
        {<<"orphan-cleanup">>, ?ORD},
        {<<"pending-cleanup">>, ?ORD},
        {<<"account-deletion">>, ?ORD},
        {<<"teaching-unbound">>, ?ORD},
        {<<"app-confirm-reject">>, ?ORD},
        {<<"app-retention-purge">>, ?ORD},
        {<<"eb-private-delete">>, ?ENT},
        {<<"eb-pending-cleanup">>, ?ENT},
        {<<"eb-purge-drain">>, ?ENT}
    ].

leaf_unit(St, {Case, _Entry, Driver, Scenario, Domain} = Spec) ->
    Desc = binary_to_list(
        iolist_to_binary([
            "disk-predecessor-protocols/", Driver, "/", Scenario, "/", Domain
        ])
    ),
    {Desc,
        {timeout, 240, fun() ->
            Measured = scenario(St, Spec),
            _ = Case,
            emit(Spec, Measured)
        end}}.

emit({Case, Entry, Driver, Scenario, Domain}, Measured) ->
    attachment_lifecycle_v5_obs:emit(#{
        case_id => Case,
        entrypoint => Entry,
        scenario => Scenario,
        domain => Domain,
        schema_phase => <<"expand-queue">>,
        measured => Measured#{driver => Driver}
    }).

%% ------------------------------------------------------------ dispatch

scenario(St, {?C, Entry, Driver, Scenario, Domain}) ->
    Measured =
        case Scenario of
            <<"W4-01/intent-rollback/", _/binary>> ->
                attachment_lifecycle_v5_w403_predecessor_intent:run(
                    Entry, fault(Scenario), St
                );
            <<"W4-02/", _/binary>> ->
                attachment_lifecycle_v5_w403_predecessor_worker:run(
                    Entry, short_scenario(Scenario), Domain, St
                )
        end,
    Measured#{driver => Driver}.

fault(<<"W4-01/intent-rollback/sql_rollback">>) -> sql_rollback;
fault(<<"W4-01/intent-rollback/intent_insert_error">>) -> intent_insert_error;
fault(<<"W4-01/intent-rollback/audit_insert_error">>) -> audit_insert_error;
fault(<<"W4-01/intent-rollback/commit_response_lost">>) -> commit_response_lost.

short_scenario(<<"W4-02/worker-crash/kill_before_claim_commit">>) ->
    <<"kill_before_claim_commit">>;
short_scenario(<<"W4-02/worker-crash/kill_after_claim_before_delete">>) ->
    <<"kill_after_claim_before_delete">>;
short_scenario(<<"W4-02/worker-crash/kill_after_delete_before_completion">>) ->
    <<"kill_after_delete_before_completion">>;
short_scenario(<<"W4-02/worker-db-fault/db_lost_before_claim_verified">>) ->
    <<"db_lost_before_claim_verified">>;
short_scenario(<<"W4-02/worker-db-fault/db_lost_after_delete">>) ->
    <<"db_lost_after_delete">>;
short_scenario(<<"W4-02/queue-adoption/enterprise_pending_adoption">>) ->
    <<"enterprise_pending_adoption">>;
short_scenario(<<"W4-02/queue-adoption/enterprise_intent_replay">>) ->
    <<"enterprise_intent_replay">>;
short_scenario(<<"W4-02/queue-adoption/ordinary_pk_preserved">>) ->
    <<"ordinary_pk_preserved">>;
short_scenario(<<"W4-02/queue-adoption/cross_queue_identity_conflict">>) ->
    <<"cross_queue_identity_conflict">>;
short_scenario(<<"W4-02/ordinary-queue-recovery/pending_recovery">>) ->
    <<"pending_recovery">>;
short_scenario(<<"W4-02/ordinary-queue-recovery/orphan_recovery">>) ->
    <<"orphan_recovery">>;
short_scenario(<<"W4-02/ordinary-queue-recovery/confirmed_fk_preserved">>) ->
    <<"confirmed_fk_preserved">>;
short_scenario(<<"W4-02/ordinary-queue-recovery/done_path_reuse_rejected">>) ->
    <<"done_path_reuse_rejected">>.

%% ---------------------------------------------------------------- setup

setup() ->
    Boot = eunit_runner:eunit_setup(),
    PriorRegistry = application:get_env(imboy, attachment_storage_registry),
    Db = inttest_marker_db:provision(#{
        env_prefix => <<"V5_INTTEST_W403P">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    Root = disk_root(),
    try
        ok = file:change_mode(Root, 8#700),
        ok = application:set_env(imboy, attachment_storage_registry, registry(Root)),
        case attachment_storage:validate_registry() of
            ok -> ok;
            Other -> erlang:error({w403p_registry_not_executable, Other})
        end,
        Conn = maps:get(conn, Db),
        ok = ?SUP:seed_tenants(Conn),
        #{
            boot => Boot,
            db => Db,
            conn => Conn,
            root => Root,
            prior_registry => PriorRegistry
        }
    catch
        Class:Reason:Stack ->
            restore_registry(PriorRegistry),
            _ = file:del_dir_r(Root),
            inttest_marker_db:release(Db),
            eunit_runner:eunit_cleanup(Boot),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{root := Root, prior_registry := Prior, db := Db, boot := Boot}) ->
    restore_registry(Prior),
    _ = file:del_dir_r(Root),
    inttest_marker_db:release(Db),
    eunit_runner:eunit_cleanup(Boot),
    ok.

restore_registry({ok, Value}) -> application:set_env(imboy, attachment_storage_registry, Value);
restore_registry(undefined) -> application:unset_env(imboy, attachment_storage_registry).

registry(Root) ->
    #{
        version => 1,
        roots => #{
            <<"root-disk-1">> => #{
                driver => <<"disk">>,
                namespace => ?SUP:ns(),
                aliases => [],
                endpoint => list_to_binary("file://" ++ Root),
                container => ?SUP:container()
            }
        },
        upload_namespaces => #{}
    }.

%% V5_DISK_ROOT 由 gate 固定为 admission allowlist 内路径；缺省退回临时目录
%% （仅普通 eunit 干跑；gate 执行时一定存在）。
disk_root() ->
    Parent =
        case os:getenv("V5_DISK_ROOT") of
            false -> temp_parent();
            "" -> temp_parent();
            Dir -> Dir
        end,
    ok = filelib:ensure_dir(filename:join(Parent, "probe")),
    Root = filename:join(
        Parent,
        "w403p-" ++ binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = file:make_dir(Root),
    Root.

temp_parent() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.
