%%% @doc W4-03 disk-predecessor-protocols（66 叶）。
%%%
%%% W4-01 intent-rollback（12 入口 × 4 故障 = 48）与 W4-02 四组协议
%%% （worker-crash 6、worker-db-fault 4、queue-adoption 4、
%%% ordinary-queue-recovery 4）的协议重放。每叶一个具名 eunit unit：场景断言
%%% 全过后 emit 一行观测，adapter 再按 case/entrypoint/scenario/domain/driver
%%% 唯一匹配封印。
%%%
%%% V3-20 形态：registry 只装 garage 根（探针 put/head 走真实 Garage）；
%%% 全部 SQL 种子（queue/attachment/enterprise_asset/pending）改为与
%%% w403p-garage 根严格对齐的 garage 四元组——worker 的 frozen_location
%%% 校验通过、DELETE 经 `attachment_storage:delete_snapshot/1` 真落到
%%% Garage（对象消失/队列 done 断言自然成立）。specs 的 <<"disk">> 仅为
%%% 封印 case/entrypoint/driver 匹配标签（历史 contract 键）。
-module(attachment_lifecycle_v5_w403_predecessor_tests).

-include_lib("eunit/include/eunit.hrl").

-define(C, <<"W4-03/disk-predecessor-protocols">>).
-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).
-define(SUP, attachment_lifecycle_v5_w403_predecessor_support).

w403_predecessor_test_() ->
    %% 受控让位（仓库先例：attachment_lifecycle_v5_pg_tests r20_test_/
    %% v5_gate_pinned：eunit 无 {skip, ...} 顶层描述符，生成器返回空清单
    %% 才是等价的静默让位机制）。setup 经 inttest_marker_db:provision
    %% 钉 V5_INTTEST_W403P（gate 由 scripts/test/attachment_lifecycle_v5_
    %% gate.sh 成套导出 V5_INTTEST_W403P_DB）；nightly 全量裸跑不经 gate，
    %% 缺钉名 env 时在生成器入口让位，不进 setup（provision 的 fail-closed
    %% 契约留给非法半配置）。gate 路径（env 在场）语义与断言零删减。
    case os:getenv("V5_INTTEST_W403P_DB") of
        V when is_list(V), V =/= "" ->
            predecessor_units();
        _ ->
            io:format(user, "W403_PRED_EUNIT_YIELD missing_env=\"V5_INTTEST_W403P_DB\"~n", []),
            []
    end.

predecessor_units() ->
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
    %% V3-20：探针走 garage 路径（本叶进程的 support driver 上下文显式切
    %% garage；specs 的 <<"disk">> 仅封印匹配标签）。
    ok = ?SUP:set_driver(garage),
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
    PriorGarage = application:get_env(imboy, garage),
    Db = inttest_marker_db:provision(#{
        env_prefix => <<"V5_INTTEST_W403P">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    try
        %% V3-20：disk 驱动退役，registry 只装 garage 根（run-owned 目录不再
        %% 建；root 字段保留接口形状，执行器 garage 分支不消费）。探针写读、
        %% identity 解析与种子身份都需要 garage env（w403p-garage 根）。
        Garage = garage_env(),
        ok = application:set_env(imboy, garage, Garage),
        ok = application:set_env(imboy, attachment_storage_registry, registry(Garage)),
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
            root => <<"run-owned-registry-root">>,
            prior_registry => PriorRegistry
        }
    catch
        Class:Reason:Stack ->
            restore_registry(PriorRegistry, PriorGarage),
            inttest_marker_db:release(Db),
            eunit_runner:eunit_cleanup(Boot),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(S) ->
    %% setup 早期失败时 prior_garage/db/boot 可能未入 State（第十三轮
    %% function_clause 实证）——cleanup 必须对部分 setup 成功的 State 稳健。
    Prior = maps:get(prior_registry, S, undefined),
    restore_registry(Prior, maps:get(prior_garage, S, undefined)),
    case maps:get(db, S, undefined) of
        Db when is_map(Db) -> inttest_marker_db:release(Db);
        _ -> ok
    end,
    case maps:get(boot, S, undefined) of
        Boot when is_map(Boot) -> eunit_runner:eunit_cleanup(Boot);
        _ -> ok
    end,
    ok.

restore_registry({ok, Value}, PG) ->
    ok = application:set_env(imboy, attachment_storage_registry, Value),
    restore_garage(PG);
restore_registry(undefined, PG) ->
    ok = application:unset_env(imboy, attachment_storage_registry),
    restore_garage(PG).

restore_garage({ok, V}) -> ok = application:set_env(imboy, garage, V);
restore_garage(undefined) -> ok = application:unset_env(imboy, garage).

%% namespace/container 与 ?SUP:set_driver(garage) 后的 ns()/container()（场景
%% 叶进程内探针、identity 解析与种子参数的实际取值）同值：ns=w403p-garage、
%% container=bucket（with_snapshot 要求 resolve 出的 container 与传入值相等；
%% 种子行的 driver/ns/container/digest 与根一致，frozen_location 通过）。
registry(Garage) ->
    #{
        version => 1,
        roots => #{
            <<"w403p-garage-root">> => #{
                driver => <<"garage">>,
                namespace => <<"w403p-garage">>,
                aliases => [],
                endpoint => maps:get(endpoint, Garage),
                container => maps:get(bucket, Garage)
            }
        },
        upload_namespaces => #{}
    }.

garage_env() ->
    Keys = [
        "W202_GARAGE_ENDPOINT",
        "W202_GARAGE_BUCKET",
        "W202_GARAGE_ACCESS_KEY",
        "W202_GARAGE_SECRET_KEY"
    ],
    Values = [os:getenv(K) || K <- Keys],
    case lists:all(fun(V) -> is_list(V) andalso V =/= "" end, Values) of
        false ->
            erlang:error({no_go, garage_env_missing, lists:zip(Keys, Values)});
        true ->
            [EP, Bucket, AK, SK] = [list_to_binary(V) || V <- Values],
            #{endpoint => EP, bucket => Bucket, access_key => AK, secret_key => SK}
    end.
