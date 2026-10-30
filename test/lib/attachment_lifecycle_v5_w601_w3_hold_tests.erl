%%% W6-01 D 组 hold 面（W3-02：hold-all-entrypoints 144 + hold-gc-race 16
%%% 合同叶；本 driver 遍 72+8=80 叶）。
%%%
%%% hold-all-entrypoints：12 生产入口 × 6 治理 fixture（hold_workspace /
%%% hold_conversation / hold_message / hold_attachment / future_retention /
%%% permanent_workspace_retention）——每叶独立 confirmed 资产（storage 四元组
%%% 从 run-owned registry 按 V5_PROBE_DRIVER 解析），统一明文真值路径
%%% （gc_verified 引用零证明为真，排除 gc=false 混淆变量），治理行安装后
%%% fn_v5_intent_ordinary_delete 恒回 23510 且资产行保留；同叶以治理行存在性
%%% 为唯一变量的对照资产证明 23510 确由 hold 触发（对照资产无治理行，同一
%%% 协议入口成功推进 delete_intent）。逐入口另以生产入口真实调用分派
%%%（entry_point_call / entry_domain）区分入口：pending 域走
%%% attachment_ds:pending_cleanup、metadata 域走 orphan/account-deletion
%%% 收敛链、enterprise 域走 governance_tx / fn_v5_intent_eb_delete。
%%% hold-gc-race：hold 先提交 => intent 拒绝；intent 先提交 => 治理不复活
%%% 已删对象。全部 8 单元真实双连接屏障（ordinary 走 attachment 行锁 +
%%% 对象 advisory 锁面，enterprise 走 enterprise_asset 行锁），waiter 被
%%% holder 阻塞经 pg_blocking_pids/pg_locks 对账；enterprise 叶全部走真实
%%% 企业资产删除域（enterprise_asset + fn_v5_intent_eb_delete /
%%% workspace hold / governance_tx），不用 ordinary intent 贴标签。
%%%
%%% 每叶 {timeout,240}，断言全过后 emit 一行观测（schema_phase=合同叶值
%%% integrated；measured 含 driver/ac_id/identity_resolved/
%%% cross_driver_rejected/probe_sha256/probe_readback_ok）；after 兜底清理
%%% 本叶可销毁行与探针对象（WAR 等治理不可变行留在步级一次性库）。
-module(attachment_lifecycle_v5_w601_w3_hold_tests).

-export([specs/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(SUP, attachment_lifecycle_v5_w601_w3_support).
-define(C_HE, <<"W3-02/hold-all-entrypoints">>).
-define(C_HR, <<"W3-02/hold-gc-race">>).
-define(ASSET_BASE, 950000000).
-define(MARKER, <<"V5_INTTEST_W601H">>).

w601_w3_hold_test_() ->
    Driver = ?SUP:driver(),
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        [leaf_unit(S, Spec) || Spec <- specs(Driver)]
    end}.

setup() -> ?SUP:setup(?MARKER).

cleanup(S) -> ?SUP:cleanup(S).

%% ------------------------------------------------------------ leaf matrix

%% Spec = {Case, Entry, Driver, Scenario, Domain, AcId, Idx, Scope, Ep}
specs(Driver) -> he_specs(Driver) ++ hr_specs(Driver).

he_specs(Driver) ->
    [
        {?C_HE, ep_binary(EP), Driver, fixture_binary(Fix), hold_domain(EP), <<"AC05">>,
            (EpIdx - 1) * 6 + FixIdx, ScopeVal,
            EP}
     || {EpIdx, {EP, ScopeVal}} <- indexed(entrypoints()),
        {FixIdx, Fix} <- indexed(fixtures())
    ].

hr_specs(Driver) ->
    Units = hr_units(),
    [
        {?C_HR, <<"hold-gc-race">>, Driver, ScBin, DomBin, <<"AC05">>, 72 + Idx, Scope, null}
     || {Idx, {ScBin, DomBin, Scope}} <- indexed(Units)
    ].

%% 双跑幂等 id：driver 分段（garage 低半段 / disk 高半段）+ 叶序 * 20。
leaf_id(Driver, Idx) -> ?ASSET_BASE + (Idx - 1) * 20 + ?SUP:driver_index(Driver).

leaf_unit(S, {Case, Entry, Driver, Sc, Dom, Ac, _Idx, _Scope, _EP} = Spec) ->
    Desc = binary_to_list(
        iolist_to_binary([short_case(Case), "/", Driver, "/", Sc, "/", Dom])
    ),
    {Desc,
        {timeout, 240, fun() ->
            try
                Measured = scenario(S, Spec),
                ?SUP:emit({Case, Entry, Driver, Sc, Dom, Ac}, Measured)
            after
                sweep(S, Spec)
            end
        end}}.

short_case(Case) ->
    [_, Short] = binary:split(Case, <<"/">>),
    Short.

%% -------------------------------------------------------- scenario dispatch

scenario(S, {?C_HE, _, _, Sc, _, _, Idx, ScopeVal, EP}) ->
    hold_all_entrypoint(S, Sc, Idx, ScopeVal, EP);
scenario(S, {?C_HR, _, Driver, Sc, Dom, _, Idx, Scope, _}) ->
    hold_gc_race(S, Driver, Sc, Dom, Idx, Scope).

%% ------------------------------------------------ hold-all-entrypoints（72）

hold_all_entrypoint(S, ScBin, Idx, ScopeVal, EP) ->
    #{conn := Conn} = S,
    Driver = ?SUP:driver(),
    Key = he_key(Driver, Idx),
    Id = leaf_id(Driver, Idx),
    CtrlKey = he_ctrl_key(Key),
    CtrlId = Id + 1,
    Quad = ?SUP:seed_identity(S, Driver, Key),
    ok = ?SUP:seed_confirmed_live(
        Conn, Key, Id, null, Quad, ScopeVal, scope_ref_fixture(ScopeVal), null
    ),
    CtrlQuad = ?SUP:seed_identity(S, Driver, CtrlKey),
    ok = ?SUP:seed_confirmed_live(
        Conn, CtrlKey, CtrlId, null, CtrlQuad, ScopeVal, scope_ref_fixture(ScopeVal), null
    ),
    %% Independent primary/control windows overlap; governance still runs serially
    %% only after BOTH original three-second windows have naturally elapsed.
    %% 主/对照资产独立能力窗重叠；两者原三秒均自然到期后才顺序执行治理断言。
    ok = ?SUP:await_upload_expiry(Conn, Key),
    ok = ?SUP:await_upload_expiry(Conn, CtrlKey),
    Probe = ?SUP:probe_io(Key, Quad),
    CtrlProbe = ?SUP:probe_io(CtrlKey, CtrlQuad),
    %% 明文真值前提：gc_verified 引用零证明为真（排除 gc=false 混淆变量）。
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key)),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, CtrlKey)),
    %% 控制组（唯一变量 = 治理行缺席）：同一协议入口成功推进 delete_intent。
    {ok, _, _} = ?SUP:e(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 hold control')", [CtrlKey]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"delete_intent">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [CtrlKey]
    ),
    %% 主资产：治理行在场 => 协议入口 23510 且资产行保留。
    Cleanup = ?SUP:install_governance(Conn, Id, fixture_atom(ScBin)),
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        ?SUP:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 hold')", [Key])
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    EntryMeasured = entry_call(EP, S, Key, Id, Quad),
    ok = ?SUP:run_cleanup(Cleanup),
    CtrlProof = ?SUP:identity_proof(CtrlKey, CtrlQuad),
    maps:merge(
        maps:merge(
            maps:merge(
                #{
                    asset_path => Key,
                    attachment_id => Id,
                    attachment_scope => ScopeVal,
                    governance_fixture => ScBin,
                    gc_verified_refresh_ok => true,
                    intent_rejected => <<"23510">>,
                    control_asset_path => CtrlKey,
                    control_intent_advanced => <<"delete_intent">>
                },
                Probe
            ),
            CtrlProbe
        ),
        maps:merge(EntryMeasured, maps:merge(?SUP:identity_proof(Key, Quad), CtrlProof))
    ).

he_key(Driver, Idx) ->
    <<"v5e2e/w601/", Driver/binary, "/he/", (integer_to_binary(Idx))/binary>>.

he_ctrl_key(Key) -> <<Key/binary, "/ctrl">>.

he_eb_id(Id) -> Id + 100000.

he_hold_id(Id) -> Id + 200000.

%% 逐入口生产调用分派（w4 hold-entrypoints 生产入口先例的真实 DB 形态）。
%% pending 域入口（reject-*/teaching-unbound/pending-cleanup）：confirmed
%% 会话无治理锚点（N/A 语义明示），入口真实执行后会话/资产保留、零 ready。
entry_call(EP, S, Key, _Id, _Quad) when
    EP =:= reject_size;
    EP =:= reject_mime;
    EP =:= reject_teaching;
    EP =:= teaching_unbound;
    EP =:= pending_cleanup
->
    #{conn := Conn} = S,
    {ok, #{cleaned := _}} = attachment_ds:pending_cleanup(1),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, _, [{1}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM attach_pending WHERE object_key=$1"
        " AND session_state='confirmed'",
        [Key]
    ),
    {ok, _, [{0}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1 AND job_state='ready'",
        [Key]
    ),
    #{
        entry_point_call => <<"attachment_ds:pending_cleanup/1">>,
        entry_domain => <<"pending">>,
        confirmed_session_untouched => true,
        ready_jobs => 0
    };
%% orphan-cleanup：metadata 域收敛链真实执行，治理资产保守保留。
entry_call(orphan_cleanup, S, Key, _Id, _Quad) ->
    #{conn := Conn} = S,
    {ok, #{cleaned := _}} = attachment_ds:orphan_cleanup(#{age_days => 7}),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, _, [{0}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1 AND job_state='ready'",
        [Key]
    ),
    #{
        entry_point_call => <<"attachment_ds:orphan_cleanup/1">>,
        entry_domain => <<"metadata">>,
        asset_preserved => true,
        ready_jobs => 0
    };
%% account-deletion：注销收集面真实列出候选键，治理资产 intent 冲突保留。
entry_call(account_deletion, S, Key, _Id, _Quad) ->
    #{conn := Conn} = S,
    {ok, Keys} = elib_pg:with_tx(fun(C) ->
        user_deletion_executor:collect_attachment_keys_tx(C, 101)
    end),
    true = lists:member(Key, Keys),
    ?assertMatch(
        {error, intent_conflict},
        in_fixture_tx(Conn, fun() -> attachment_repo:intent_ordinary_delete_tx(
            Conn, Key, <<"w601w3 hold account deletion">>
        ) end)
    ),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    #{
        entry_point_call =>
            <<
                "user_deletion_executor:collect_attachment_keys_tx/2"
                " + attachment_repo:intent_ordinary_delete_tx/3"
            >>,
        entry_domain => <<"metadata">>,
        collect_listed => true,
        intent_conflict => true
    };
%% app-confirm-reject：pending reject 入口对 confirmed 会话拒绝（状态机面）。
entry_call(app_confirm_reject, S, Key, _Id, _Quad) ->
    #{conn := Conn} = S,
    ?assertMatch(
        {error, intent_conflict},
        in_fixture_tx(Conn, fun() -> attach_pending_repo:intent_pending_delete_tx(
            Conn, Key, <<"reject">>, <<"w601w3 confirm reject under hold">>
        ) end)
    ),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    #{
        entry_point_call => <<"attach_pending_repo:intent_pending_delete_tx/4">>,
        entry_domain => <<"pending_reject">>,
        confirmed_session_reject_denied => true
    };
%% app-retention-purge：EAR 域 purge 真实治理入口拒绝（invalid_request）。
entry_call(app_retention_purge, S, Key, _Id, _Quad) ->
    #{conn := Conn} = S,
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(
            Conn,
            ?SUP:governance_ctx(),
            #{op => <<"purge">>, object_key => Key}
        )
    ),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    #{
        entry_point_call =>
            <<"enterprise_asset_retention_logic:governance_tx/3 (purge, EAR domain)">>,
        entry_domain => <<"enterprise_governance">>,
        purge_denied => true
    };
%% eb-private-delete：企业资产删除域真实入口（eb_asset_store:delete_private）
%% 在 workspace hold 下 intent_conflict，资产 live、零 ready 任务。
entry_call(eb_private_delete, S, Key, Id, Quad) ->
    #{conn := Conn} = S,
    EbId = he_eb_id(Id),
    ok = ?SUP:seed_eb_row(Conn, Key, EbId, Quad),
    Hold = ?SUP:install_workspace_hold(Conn, he_hold_id(Id)),
    ?assertEqual({error, intent_conflict}, eb_asset_store:delete_private(9101, 9102, EbId)),
    {ok, _, [{1}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE id=$1 AND lifecycle_state='live'",
        [EbId]
    ),
    {ok, _, [{0}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1"
        " AND job_state='ready'",
        [EbId]
    ),
    ok = ?SUP:run_cleanup(maps:get(cleanup, Hold)),
    #{
        entry_point_call => <<"eb_asset_store:delete_private/3">>,
        entry_domain => <<"enterprise">>,
        intent_conflict => true,
        ready_jobs => 0
    };
%% eb-pending-cleanup：hold gate 先拦（app 层 active_hold）→ skipped 零删除。
entry_call(eb_pending_cleanup, S, Key, Id, Quad) ->
    #{conn := Conn} = S,
    EbId = he_eb_id(Id),
    ok = ?SUP:seed_eb_row(Conn, Key, EbId, Quad),
    ?SUP:q(
        Conn,
        "UPDATE enterprise_asset SET status='pending_confirm', created_at="
        "now() - interval '2 hours' WHERE id=$1",
        [EbId]
    ),
    Hold = ?SUP:install_workspace_hold(Conn, he_hold_id(Id)),
    {ok, #{skipped := Skipped}} = eb_asset_app:cleanup_pending(9101, #{
        workspace_id => 9102, asset_ids => [EbId], ttl_seconds => 3600
    }),
    case [true || {Id0, active_hold} <- Skipped, Id0 =:= EbId] of
        [_] -> ok;
        _ -> erlang:error({w601_w3_hold_gate_expected, EbId, Skipped})
    end,
    {ok, _, [{1}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE id=$1 AND lifecycle_state='live'",
        [EbId]
    ),
    {ok, _, [{0}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1"
        " AND job_state='ready'",
        [EbId]
    ),
    ok = ?SUP:run_cleanup(maps:get(cleanup, Hold)),
    #{
        entry_point_call => <<"eb_asset_app:cleanup_pending/2">>,
        entry_domain => <<"enterprise">>,
        hold_gate_skipped => true,
        ready_jobs => 0
    };
%% eb-purge-drain：企业治理路径——workspace hold 下 drain 删除域真实入口
%%（fn_v5_intent_eb_delete）23510，治理保护零任务（drain worker W4-02）。
entry_call(eb_purge_drain, S, Key, Id, Quad) ->
    #{conn := Conn} = S,
    EbId = he_eb_id(Id),
    ok = ?SUP:seed_eb_row(Conn, Key, EbId, Quad),
    Hold = ?SUP:install_workspace_hold(Conn, he_hold_id(Id)),
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        ?SUP:e(
            Conn, "SELECT fn_v5_intent_eb_delete(9101,9102,$1,'w601w3 purge drain')", [EbId]
        )
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1"
        " AND job_state='ready'",
        [EbId]
    ),
    {ok, _, [{1}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE id=$1 AND lifecycle_state='live'",
        [EbId]
    ),
    ok = ?SUP:run_cleanup(maps:get(cleanup, Hold)),
    #{
        entry_point_call => <<"fn_v5_intent_eb_delete/4 (drain delete domain)">>,
        entry_domain => <<"enterprise_governance">>,
        intent_rejected => <<"23510">>,
        drain_ready_jobs => 0
    }.

%% ------------------------------------------------- hold-gc-race（8 单元屏障）

hr_units() ->
    [
        {<<"hold_commits_first">>, <<"ordinary">>, <<"group">>},
        {<<"hold_commits_first">>, <<"enterprise">>, <<"enterprise">>},
        {<<"intent_before_asset_hold">>, <<"ordinary">>, <<"private">>},
        {<<"intent_before_asset_hold">>, <<"enterprise">>, <<"enterprise">>},
        {<<"intent_before_retention_extension">>, <<"ordinary">>, <<"private">>},
        {<<"intent_before_retention_extension">>, <<"enterprise">>, <<"enterprise">>},
        {<<"workspace_hold_exclusion_receipt">>, <<"ordinary">>, <<"group">>},
        {<<"workspace_hold_exclusion_receipt">>, <<"enterprise">>, <<"enterprise">>}
    ].

hold_gc_race(S, Driver, ScBin, DomBin, Idx, Scope) ->
    #{conn := Conn} = S,
    Key = hr_key(Driver, ScBin, DomBin),
    Id = leaf_id(Driver, Idx),
    Quad = ?SUP:seed_identity(S, Driver, Key),
    ok = ?SUP:seed_confirmed(
        Conn, Key, Id, Id, Quad, Scope, scope_ref_fixture(Scope), null
    ),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key)),
    case DomBin of
        <<"enterprise">> ->
            ok = ?SUP:seed_eb_row(Conn, Key, hr_eb_id(Id), Quad);
        <<"ordinary">> ->
            ok
    end,
    Probe = ?SUP:probe_io(Key, Quad),
    {ok, #{measured := Measured, proof := Proof, cleanup := Cleanup}} = hr_step(
        S, ScBin, DomBin, Key, Id
    ),
    ok = ?SUP:run_cleanup(Cleanup),
    maps:merge(
        maps:merge(Measured#{asset_path => Key, attachment_id => Id}, Probe),
        maps:merge(?SUP:identity_proof(Key, Quad), Proof)
    ).

hr_key(Driver, ScBin, DomBin) ->
    <<"v5e2e/w601/", Driver/binary, "/hr/", ScBin/binary, "/", DomBin/binary>>.

hr_eb_id(Id) -> Id + 100000.

hr_hold_id(Id) -> Id + 200000.

%% (1) hold 先提交 ordinary：Holder 持 attachment 行锁 => intent 真实阻塞 =>
%% EAR hold 先提交 => intent 23510，行保留。
hr_step(S, <<"hold_commits_first">>, <<"ordinary">>, Key, Id) ->
    #{conn := Conn} = S,
    Other = ?SUP:barrier_connect(S),
    {Blocked, Cleanup} =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:attachment_row_lock(Key), fun(Pids) ->
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other, "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 hold race')", [
                            Key
                        ]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                Cleanup0 = ?SUP:install_governance(Conn, Id, hold_message),
                ?SUP:q(Conn, "COMMIT"),
                ?assertMatch(
                    {error, #error{code = <<"23510">>}}, ?SUP:collect_step(Ref, 12000)
                ),
                ok = epgsql:sync(Other),
                {B, Cleanup0}
            end)
        after
            epgsql:close(Other)
        end,
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, #{
        measured => #{
            hold_fixture => <<"hold_message">>,
            intent_rejected => <<"23510">>,
            attachment_rows_after => 1
        },
        proof => Blocked,
        cleanup => Cleanup
    }};
%% (2) hold 先提交 enterprise：workspace hold（真实企业资产 hold 面）先提交 =>
%% fn_v5_intent_eb_delete 23510（active workspace hold），资产 live、零队列。
%% P2 注（oracle prose 差异）：oracle prose 写「EAR row installed through the
%% production governance_tx hold entrypoint」（hr(4) 适用——其阻塞面确为 EAR
%% 行，governance_tx hold 真实写入）；本单元阻塞面是 fn_v5_intent_eb_delete
%% 的 enterprise_retention_hold active workspace hold 检查，governance_tx
%% hold 只写 EAR 行、不触及该表，入口语义对本单元不成立——故保留 workspace
%% hold 直插 fixture，measured 如实记 governance_entry=workspace_hold。
hr_step(S, <<"hold_commits_first">>, <<"enterprise">>, _Key, Id) ->
    #{conn := Conn} = S,
    EbId = hr_eb_id(Id),
    Other = ?SUP:barrier_connect(S),
    {Blocked, Hold} =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:eb_workspace_lock(9101, 9102), fun(Pids) ->
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "SELECT fn_v5_intent_eb_delete(9101,9102,$1,'w601w3 hold race eb')",
                        [EbId]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                Hold0 = ?SUP:install_workspace_hold(Conn, hr_hold_id(Id)),
                ?SUP:q(Conn, "COMMIT"),
                ?assertMatch(
                    {error, #error{code = <<"23510">>}}, ?SUP:collect_step(Ref, 12000)
                ),
                ok = epgsql:sync(Other),
                {B, Hold0}
            end)
        after
            epgsql:close(Other)
        end,
    ok = ?SUP:run_cleanup(maps:get(cleanup, Hold)),
    {ok, _, [{<<"live">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM enterprise_asset WHERE id=$1", [EbId]
    ),
    {ok, _, [{0}]} = ?SUP:e(
        Conn, "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1", [EbId]
    ),
    {ok, #{
        measured => #{
            governance_entry => <<"workspace_hold">>,
            %% hold_state 为屏障事务内的时点采样（emit 时该行已被清理释放）
            hold_state => <<"active">>,
            intent_rejected => <<"23510">>,
            lifecycle_state => <<"live">>,
            deletion_queue_rows => 0
        },
        proof => Blocked,
        cleanup => ok
    }};
%% (3) intent 先提交 ordinary：Holder 事务内 intent 先落定 => 治理 hold 安装
%%（EAR upsert，FK 锁面真实阻塞于 attachment 行）在 intent 提交后完成 =>
%% lifecycle 保持 delete_intent、删除队列行仍在（hold 不复活）。
hr_step(S, <<"intent_before_asset_hold">>, <<"ordinary">>, Key, Id) ->
    #{conn := Conn} = S,
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:attachment_row_lock(Key), fun(Pids) ->
                ?SUP:q(
                    Conn,
                    "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 hold race intent')",
                    [Key]
                ),
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:q(Other, "BEGIN"),
                    Up = enterprise_attachment_retention_repo:upsert_tx(Other, Id, 9101, 9103),
                    ok = enterprise_attachment_retention_repo:hold_tx(
                        Other, Id, <<"w601w3 late hold">>
                    ),
                    {ok, _, [{LS}]} = ?SUP:e(
                        Other, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
                    ),
                    ?SUP:q(Other, "COMMIT"),
                    {Up, LS}
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(Conn, "COMMIT"),
                {{ok, _}, <<"delete_intent">>} = ?SUP:collect_step(Ref, 12000),
                B
            end)
        after
            epgsql:close(Other)
        end,
    {ok, _, [{<<"held">>}]} = ?SUP:e(
        Conn, "SELECT hold_state FROM enterprise_attachment_retention WHERE attachment_id=$1", [
            Id
        ]
    ),
    {ok, _, [{<<"delete_intent">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
    ),
    {ok, _, [{1}]} = ?SUP:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    {ok, #{
        measured => #{
            intent_committed => true,
            late_hold_installed => true,
            hold_state => <<"held">>,
            lifecycle_state_after => <<"delete_intent">>,
            deletion_queue_rows_after_late_hold => 1
        },
        proof => Blocked,
        cleanup => fun() -> ?SUP:ear_release_and_purge(Conn, Id) end
    }};
%% (4) intent 先提交 enterprise：intent(eb) 先提交（真实阻塞的重放屏障）=>
%% 治理 hold（governance_tx）后装 => enterprise_asset 不复活，EAR held，
%% 重放恒 23510。
hr_step(S, <<"intent_before_asset_hold">>, <<"enterprise">>, Key, Id) ->
    #{conn := Conn} = S,
    EbId = hr_eb_id(Id),
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:eb_row_lock(EbId), fun(Pids) ->
                ?SUP:q(
                    Conn,
                    "SELECT fn_v5_intent_eb_delete(9101,9102,$1,'w601w3 hold race intent eb')",
                    [EbId]
                ),
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "SELECT fn_v5_intent_eb_delete("
                        "9101,9102,$1,'w601w3 hold race intent eb replay')",
                        [EbId]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(Conn, "COMMIT"),
                ?assertMatch(
                    {error, #error{code = <<"23510">>}}, ?SUP:collect_step(Ref, 12000)
                ),
                ok = epgsql:sync(Other),
                B
            end)
        after
            epgsql:close(Other)
        end,
    {ok, _} = enterprise_asset_retention_logic:governance_tx(Conn, ?SUP:governance_ctx(), #{
        object_key => Key,
        op => <<"hold">>,
        reason => <<"w601w3 late hold (enterprise)">>
    }),
    {ok, _, [{<<"held">>}]} = ?SUP:e(
        Conn, "SELECT hold_state FROM enterprise_attachment_retention WHERE attachment_id=$1", [
            Id
        ]
    ),
    {ok, _, [{<<"delete_intent">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM enterprise_asset WHERE id=$1", [EbId]
    ),
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        ?SUP:e(
            Conn,
            "SELECT fn_v5_intent_eb_delete("
            "9101,9102,$1,'w601w3 hold race intent eb after hold')",
            [EbId]
        )
    ),
    ok = epgsql:sync(Conn),
    {ok, #{
        measured => #{
            intent_committed => true,
            late_hold_installed => true,
            governance_hold_state => <<"held">>,
            lifecycle_state_after => <<"delete_intent">>,
            replay_rejected => <<"23510">>
        },
        proof => Blocked,
        cleanup => fun() -> ?SUP:ear_release_and_purge(Conn, Id) end
    }};
%% (5) intent 先提交 ordinary：留存延长（生产 repo extend_tx，FK 锁面真实阻塞）
%% 不复活已删对象。
hr_step(S, <<"intent_before_retention_extension">>, <<"ordinary">>, Key, Id) ->
    #{conn := Conn} = S,
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:attachment_row_lock(Key), fun(Pids) ->
                ?SUP:q(
                    Conn,
                    "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 hold race retain ord')",
                    [Key]
                ),
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:q(Other, "BEGIN"),
                    Up = enterprise_attachment_retention_repo:upsert_tx(Other, Id, 9101, 9103),
                    NewUntil = elib_dt:add(elib_dt:now(), {3650 * 86400, second}),
                    Ext = enterprise_attachment_retention_repo:extend_tx(Other, Id, NewUntil),
                    {ok, _, [{LS}]} = ?SUP:e(
                        Other, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
                    ),
                    ?SUP:q(Other, "COMMIT"),
                    {Up, Ext, LS}
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(Conn, "COMMIT"),
                {{ok, _}, {ok, _}, <<"delete_intent">>} = ?SUP:collect_step(Ref, 12000),
                B
            end)
        after
            epgsql:close(Other)
        end,
    {ok, _, [{<<"delete_intent">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
    ),
    {ok, #{
        measured => #{
            intent_committed => true,
            retention_extended => true,
            lifecycle_state_after => <<"delete_intent">>
        },
        proof => Blocked,
        cleanup => fun() -> ?SUP:ear_release_and_purge(Conn, Id) end
    }};
%% (6) intent 先提交 enterprise：intent(eb) 先提交 => 生产治理留存延长
%%（governance_tx set_retention）不复活已删对象，重放恒 23510。
hr_step(S, <<"intent_before_retention_extension">>, <<"enterprise">>, Key, Id) ->
    #{conn := Conn} = S,
    EbId = hr_eb_id(Id),
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:eb_row_lock(EbId), fun(Pids) ->
                ?SUP:q(
                    Conn,
                    "SELECT fn_v5_intent_eb_delete("
                    "9101,9102,$1,'w601w3 hold race retain eb')",
                    [EbId]
                ),
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "SELECT fn_v5_intent_eb_delete("
                        "9101,9102,$1,'w601w3 hold race retain eb replay')",
                        [EbId]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(Conn, "COMMIT"),
                ?assertMatch(
                    {error, #error{code = <<"23510">>}}, ?SUP:collect_step(Ref, 12000)
                ),
                ok = epgsql:sync(Other),
                B
            end)
        after
            epgsql:close(Other)
        end,
    {ok, _} = enterprise_asset_retention_logic:governance_tx(Conn, ?SUP:governance_ctx(), #{
        object_key => Key,
        op => <<"set_retention">>,
        retention_days => 3650
    }),
    {ok, _, [{<<"delete_intent">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM enterprise_asset WHERE id=$1", [EbId]
    ),
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        ?SUP:e(
            Conn,
            "SELECT fn_v5_intent_eb_delete("
            "9101,9102,$1,'w601w3 hold race retain eb after set')",
            [EbId]
        )
    ),
    ok = epgsql:sync(Conn),
    {ok, #{
        measured => #{
            intent_committed => true,
            retention_extended => true,
            governance_rejection => <<"replay_23510">>,
            lifecycle_state_after => <<"delete_intent">>
        },
        proof => Blocked,
        cleanup => fun() -> ?SUP:ear_release_and_purge(Conn, Id) end
    }};
%% (7) workspace hold 排除回执 ordinary：WAR 行在场——intent 拒且重放恒拒
%%（WAR 不可变），行不复活、无删除队列。
hr_step(S, <<"workspace_hold_exclusion_receipt">>, <<"ordinary">>, Key, Id) ->
    #{conn := Conn} = S,
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:attachment_row_lock(Key), fun(Pids) ->
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other, "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 ws receipt')", [
                            Key
                        ]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                _Cleanup = ?SUP:install_governance(Conn, Id, hold_workspace),
                ?SUP:q(Conn, "COMMIT"),
                ?assertMatch(
                    {error, #error{code = <<"23510">>}}, ?SUP:collect_step(Ref, 12000)
                ),
                ok = epgsql:sync(Other),
                B
            end)
        after
            epgsql:close(Other)
        end,
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        ?SUP:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 ws receipt 2')", [Key])
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, _, [{1}]} = ?SUP:e(
        Conn, "SELECT COUNT(*) FROM workspace_attachment_retention WHERE attachment_id=$1", [Id]
    ),
    {ok, _, [{0}]} = ?SUP:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    {ok, #{
        measured => #{
            hold_excluded_intent => <<"23510">>,
            exclusion_persistent => true,
            attachment_rows_after => 1,
            war_rows => 1,
            deletion_queue_rows => 0
        },
        proof => Blocked,
        %% WAR 行不可变（00000168 23514），留在步级一次性库不清理。
        cleanup => ok
    }};
%% (8) workspace hold 排除回执 enterprise：workspace hold 在场——intent(eb)
%% 恒拒（重放两次均 23510），资产 live、零队列；release 后 hold 行回执闭环。
%% P2 注：同 hr(2)——阻塞面是 enterprise_retention_hold active workspace
%% hold，governance_tx hold 入口语义不适用，保留直插 fixture，
%% measured 如实记 governance_entry=workspace_hold。
hr_step(S, <<"workspace_hold_exclusion_receipt">>, <<"enterprise">>, _Key, Id) ->
    #{conn := Conn} = S,
    EbId = hr_eb_id(Id),
    Other = ?SUP:barrier_connect(S),
    {Blocked, Hold} =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:eb_workspace_lock(9101, 9102), fun(Pids) ->
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "SELECT fn_v5_intent_eb_delete("
                        "9101,9102,$1,'w601w3 ws receipt eb')",
                        [EbId]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                Hold0 = ?SUP:install_workspace_hold(Conn, hr_hold_id(Id)),
                ?SUP:q(Conn, "COMMIT"),
                ?assertMatch(
                    {error, #error{code = <<"23510">>}}, ?SUP:collect_step(Ref, 12000)
                ),
                ok = epgsql:sync(Other),
                {B, Hold0}
            end)
        after
            epgsql:close(Other)
        end,
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        ?SUP:e(
            Conn,
            "SELECT fn_v5_intent_eb_delete("
            "9101,9102,$1,'w601w3 ws receipt eb 2')",
            [EbId]
        )
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?SUP:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE id=$1 AND lifecycle_state='live'",
        [EbId]
    ),
    {ok, _, [{0}]} = ?SUP:e(
        Conn, "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1", [EbId]
    ),
    ok = ?SUP:run_cleanup(maps:get(cleanup, Hold)),
    {ok, _, [{true}]} = ?SUP:e(
        Conn,
        "SELECT released_at IS NOT NULL FROM enterprise_retention_hold WHERE id=$1",
        [maps:get(id, Hold)]
    ),
    {ok, #{
        measured => #{
            governance_entry => <<"workspace_hold">>,
            hold_excluded_intent => <<"23510">>,
            exclusion_persistent => true,
            released_hold_receipt => true,
            attachment_rows_after => 1
        },
        proof => Blocked,
        cleanup => ok
    }}.

%% ---------------------------------------------------------------- fixtures

entrypoints() ->
    [
        {reject_size, <<"group">>},
        {reject_mime, <<"group">>},
        {reject_teaching, <<"group">>},
        {orphan_cleanup, <<"moment">>},
        {pending_cleanup, <<"private">>},
        {account_deletion, <<"private">>},
        {teaching_unbound, <<"group">>},
        {app_confirm_reject, <<"enterprise">>},
        {app_retention_purge, <<"enterprise">>},
        {eb_private_delete, <<"c2c">>},
        {eb_pending_cleanup, <<"moment">>},
        {eb_purge_drain, <<"private">>}
    ].

fixtures() ->
    [
        hold_workspace,
        hold_conversation,
        hold_message,
        hold_attachment,
        future_retention,
        permanent_workspace_retention
    ].

%% 叶矩阵域归属：eb-* 三入口为 enterprise 域，其余入口协议单元为 ordinary。
hold_domain(eb_private_delete) -> <<"enterprise">>;
hold_domain(eb_pending_cleanup) -> <<"enterprise">>;
hold_domain(eb_purge_drain) -> <<"enterprise">>;
hold_domain(_) -> <<"ordinary">>.

ep_binary(Ep) ->
    iolist_to_binary(string:replace(atom_to_binary(Ep, utf8), <<"_">>, <<"-">>, all)).

fixture_binary(F) -> atom_to_binary(F, utf8).

fixture_atom(Bin) -> binary_to_atom(Bin, utf8).

scope_ref_fixture(<<"group">>) -> <<"66">>;
scope_ref_fixture(<<"c2c">>) -> <<"c2c:101:202">>;
scope_ref_fixture(<<"enterprise">>) -> <<"org:9101/app:9103">>;
scope_ref_fixture(_) -> null.

%% ------------------------------------------------- 失败路径兜底清理（P2）
%%
%% EAR 行禁删（DELETE 一律 23514）→ ear_release_and_purge（release+窗口内
%% 置 purged）；enterprise_retention_hold append-only（DELETE 一律 23514）
%% → release_workspace_holds（遗留 active hold 一次性 release，已释放行不动）。
sweep(S, {?C_HE, _Entry, Driver, _Sc, _Dom, _Ac, Idx, _Scope, _EP}) ->
    #{conn := Conn} = S,
    Key = he_key(Driver, Idx),
    CtrlKey = he_ctrl_key(Key),
    Id = leaf_id(Driver, Idx),
    lists:foreach(
        fun(K) -> sweep_asset(Conn, K, Id, he_eb_id(Id)) end,
        [Key, CtrlKey]
    ),
    ?SUP:probe_cleanup_after(),
    ok;
sweep(S, {?C_HR, _Entry, Driver, ScBin, DomBin, _Ac, Idx, _Scope, _EP}) ->
    #{conn := Conn} = S,
    Key = hr_key(Driver, ScBin, DomBin),
    Id = leaf_id(Driver, Idx),
    sweep_asset(Conn, Key, Id, hr_eb_id(Id)),
    ?SUP:probe_cleanup_after(),
    ok.

sweep_asset(Conn, Key, Id, EbId) ->
    ?SUP:lenient_q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    ?SUP:cleanup_attachment(Conn, Key),
    ?SUP:cleanup_tombstone(Conn, Key),
    ok = ?SUP:ear_release_and_purge(Conn, Id),
    ?SUP:lenient_q(Conn, "DELETE FROM group_file WHERE id=$1", [Id]),
    ?SUP:cleanup_enterprise_asset(Conn, EbId),
    ?SUP:cleanup_enterprise_queue(Conn, EbId),
    ?SUP:release_workspace_holds(Conn).

%% _tx APIs require a real outer transaction for their savepoints.
%% _tx 接口要求真实外层事务以承载 savepoint；断言后回滚自有夹具事务。
in_fixture_tx(Conn, Fun) ->
    ok = ?SUP:q(Conn, "BEGIN"),
    try Fun() after ok = ?SUP:q(Conn, "ROLLBACK") end.

indexed(List) ->
    lists:zip(lists:seq(1, length(List)), List).
