%%% W6-01 D 组 race/shared 面（W3-01：reference-gc-race 16 + shared-owner 16
%%% 合同叶；本 driver 遍 8+8=16 叶）。
%%%
%%% reference-gc-race：4 场景 × 双域，每叶真实双连接屏障——屏障锁取生产入口
%%% 锁序中与对侧真实竞争的那把锁：ordinary intent 锁序为 fn_v5_lock_ordinary_
%%% upload（workspace[条件]→identity advisory→对象 advisory 168→attach_pending
%%% 行→attachment 行）；绑定入口 bind_reference_lock_tx 请求同一把对象 advisory
%%% 168，故 bind_commits_first 屏障持 168 对象锁（若持 attachment 行锁，
%%% holder 等 168 / waiter 等行锁构成环，40P01）；enterprise intent 锁序为
%%% fn_v5_lock_eb_upload（workspace 行锁→advisory 169→identity advisory 1685
%%% →资产行锁），bind_commits_first 屏障持 workspace 行锁——holder 的
%%% eb_pg_store:lock_assets_in 只取 identity advisory + 资产行，waiter 在取得
%%% 任何锁之前即被阻塞，锁序不构成环。worker 真实执行生产入口后以
%%% blocking_proof 经 pg_blocking_pids + pg_locks 对账阻塞对象（waiter 被
%%% holder 阻塞）再裁决提交顺序，禁 sleep 猜并发：
%%%   bind_commits_first：绑定先提交 => intent 23510；
%%%   intent_commits_first：intent 先提交 => 绑定零业务行（墓碑拒绝）；
%%%   direct_dml_*：同一裁决的直接受限 DML 双面。
%%% shared-owner：注销/登出/原帖删除不掉共享引用资产（群绑定/收藏 hash/
%%% 企业 scope 排除面）；原帖删除叶以真实读鉴权入口（attach_logic:view_url）
%%% 证明删除前可读、删除后不可读；无关读者对真实资产经同一入口拒绝。
%%%
%%% direct_dml_* 两叶 source_schema_phase=guards-contract（历史标签差异由
%%% adapter 用 integrated_phase_battery_db 断言处理；本套件 emit 合同叶值
%%% integrated）。每叶 {timeout,240}，断言全过后 emit；after 兜底清理本叶
%%% 可销毁行与探针对象（治理不可变行留在步级一次性库）。
-module(attachment_lifecycle_v5_w601_w3_race_tests).

-export([specs/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(SUP, attachment_lifecycle_v5_w601_w3_support).
-define(C_RR, <<"W3-01/reference-gc-race">>).
-define(C_SO, <<"W3-01/shared-owner">>).
-define(ASSET_BASE, 951000000).
-define(MARKER, <<"V5_INTTEST_W601R">>).

w601_w3_race_test_() ->
    Driver = ?SUP:driver(),
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        [leaf_unit(S, Spec) || Spec <- specs(Driver)]
    end}.

setup() -> ?SUP:setup(?MARKER).

cleanup(S) -> ?SUP:cleanup(S).

%% ------------------------------------------------------------ leaf matrix

%% Spec = {Case, Scenario, Driver, Domain, AcId, Idx}
specs(Driver) -> rr_specs(Driver) ++ so_specs(Driver).

rr_specs(Driver) ->
    [
        {?C_RR, ScBin, Driver, DomBin, <<"AC04">>, Idx}
     || {Idx, {ScBin, DomBin}} <- indexed(rr_units())
    ].

so_specs(Driver) ->
    [
        {?C_SO, ScBin, Driver, DomBin, <<"AC04">>, 8 + Idx}
     || {Idx, {ScBin, DomBin}} <- indexed(so_units())
    ].

leaf_id(Driver, Idx) -> ?ASSET_BASE + (Idx - 1) * 20 + ?SUP:driver_index(Driver).

leaf_unit(S, {Case, Sc, Driver, Dom, Ac, Idx} = Spec) ->
    Desc = binary_to_list(
        iolist_to_binary([short_case(Case), "/", Driver, "/", Sc, "/", Dom])
    ),
    {Desc,
        {timeout, 240, fun() ->
            try
                Measured = scenario(S, Spec),
                ?SUP:emit({Case, sc_entry(Case), Driver, Sc, Dom, Ac}, Measured)
            after
                sweep(S, Case, Driver, Sc, Dom, Idx)
            end
        end}}.

short_case(Case) ->
    [_, Short] = binary:split(Case, <<"/">>),
    Short.

sc_entry(?C_RR) -> <<"reference-gc-race">>;
sc_entry(?C_SO) -> <<"shared-owner">>.

%% 失败路径兜底：清本叶可销毁行（前置 DELETE 撞治理 FK 的残留跳过）+ 探针。
sweep(S, Case, Driver, Sc, Dom, Idx) ->
    #{conn := Conn} = S,
    Key =
        case Case of
            ?C_RR -> rr_key(Driver, Sc, Dom);
            ?C_SO -> so_key(Driver, Sc, Dom)
        end,
    Id = leaf_id(Driver, Idx),
    EbId = eb_asset_id(Id),
    ?SUP:lenient_q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    ?SUP:lenient_q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    ?SUP:lenient_q(Conn, "DELETE FROM attachment_object_deletion WHERE path=$1", [Key]),
    ?SUP:lenient_q(Conn, "DELETE FROM group_file WHERE id=$1", [Id]),
    ?SUP:lenient_q(Conn, "DELETE FROM user_collect WHERE id=$1", [collect_id(Id)]),
    ?SUP:lenient_q(Conn, "DELETE FROM enterprise_asset WHERE id=$1", [EbId]),
    ?SUP:lenient_q(Conn, "DELETE FROM enterprise_asset_delete_queue WHERE asset_id=$1", [EbId]),
    ok = ?SUP:ear_release_and_purge(Conn, Id),
    ?SUP:probe_cleanup_after(),
    ok.

%% -------------------------------------------------------- scenario dispatch

scenario(S, {?C_RR, Sc, Driver, Dom, _, Idx}) ->
    reference_gc_race(S, Driver, Sc, Dom, Idx);
scenario(S, {?C_SO, Sc, Driver, Dom, _, Idx}) ->
    shared_owner(S, Driver, Sc, Dom, Idx).

%% ================================================== reference-gc-race（8 叶）

rr_units() ->
    [
        {<<"bind_commits_first">>, <<"ordinary">>},
        {<<"bind_commits_first">>, <<"enterprise">>},
        {<<"intent_commits_first">>, <<"ordinary">>},
        {<<"intent_commits_first">>, <<"enterprise">>},
        {<<"direct_dml_bind_commits_first">>, <<"ordinary">>},
        {<<"direct_dml_bind_commits_first">>, <<"enterprise">>},
        {<<"direct_dml_intent_commits_first">>, <<"ordinary">>},
        {<<"direct_dml_intent_commits_first">>, <<"enterprise">>}
    ].

reference_gc_race(S, Driver, ScBin, DomBin, Idx) ->
    #{conn := Conn} = S,
    Key = rr_key(Driver, ScBin, DomBin),
    Id = leaf_id(Driver, Idx),
    Quad = ?SUP:seed_identity(S, Driver, Key),
    case DomBin of
        <<"ordinary">> ->
            ok = ?SUP:seed_confirmed(Conn, Key, Id, Id, Quad, <<"group">>, <<"66">>, null);
        <<"enterprise">> ->
            ok = ?SUP:seed_eb_row(Conn, Key, Id, Quad)
    end,
    Probe = ?SUP:probe_io(Key, Quad),
    {ok, #{measured := Measured, proof := Proof, cleanup := Cleanup}} = rr_step(
        S, ScBin, DomBin, Key, Id
    ),
    ok = ?SUP:run_cleanup(Cleanup),
    maps:merge(
        maps:merge(Measured#{asset_path => Key, asset_id => Id}, Probe),
        maps:merge(?SUP:identity_proof(Key, Quad), Proof)
    ).

rr_key(Driver, ScBin, DomBin) ->
    <<"v5e2e/w601/", Driver/binary, "/rr/", ScBin/binary, "/", DomBin/binary>>.

%% (1) bind_commits_first ordinary：Holder 持对象 advisory 168 锁（生产 intent
%% 在行锁之前取得的对象锁；绑定入口 bind_reference_lock_tx 请求同一把 168，
%% 屏障若持 attachment 行锁则 holder 等 168/waiter 等行锁构成环）=> intent
%% 真实阻塞（pg_blocking_pids 对账）=> 绑定（group_file 重挂）先提交 =>
%% intent 23510，资产仍 live。
rr_step(S, <<"bind_commits_first">>, <<"ordinary">>, Key, Id) ->
    #{conn := Conn} = S,
    ?assertEqual(true, ?SUP:refresh_gc_verified(Conn, Key)),
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, {advisory, Key}, fun(Pids) ->
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 concurrent bind')",
                        [Key]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?assertEqual(ok, attachment_repo:bind_reference_lock_tx(Conn, Key)),
                ?SUP:bind_group_file(Conn, Id),
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
    {ok, _, [{<<"live">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
    ),
    {ok, #{
        measured => #{
            gc_verified_refresh => true,
            bind_committed => true,
            intent_rejected => <<"23510">>,
            lifecycle_state => <<"live">>
        },
        proof => Blocked,
        cleanup => fun() -> ?SUP:unbind_group_file(Conn, Id) end
    }};
%% (2) bind_commits_first enterprise：Holder 持 workspace 行锁（生产 intent
%% 第一把锁，fn_v5_lock_eb_upload 首语句）=> intent 在取得任何锁之前即被
%% 阻塞（若屏障持资产行锁，holder 的 lock_assets_in 会请求 waiter 已持的
%% identity advisory 1685，构成环）=> intent 真实阻塞 => 生产 store 绑定
%%（message_id+retain）提交 => intent 23510。
rr_step(S, <<"bind_commits_first">>, <<"enterprise">>, _Key, Id) ->
    #{conn := Conn} = S,
    Other = ?SUP:barrier_connect(S),
    MsgId = message_id_of(Id),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(
                Conn, Other, ?SUP:eb_workspace_lock(9101, 9102), fun(Pids) ->
                    Ref = ?SUP:start_step(fun() ->
                        ?SUP:e(
                            Other,
                            "SELECT fn_v5_intent_eb_delete(9101,9102,$1,'w601w3 bind first')",
                            [Id]
                        )
                    end),
                    B = ?SUP:blocking_proof(Conn, Pids, 100),
                    {ok, [#{lifecycle_state := <<"live">>}]} = eb_pg_store:lock_assets_in(
                        Conn, 9101, 9102, [Id]
                    ),
                    Retain = erlang:system_time(second) + 172800,
                    ok = eb_pg_store:bind_asset_message_in(Conn, 9101, 9102, Id, MsgId, Retain),
                    ?SUP:q(Conn, "COMMIT"),
                    ?assertMatch(
                        {error, #error{code = <<"23510">>}}, ?SUP:collect_step(Ref, 12000)
                    ),
                    ok = epgsql:sync(Other),
                    B
                end
            )
        after
            epgsql:close(Other)
        end,
    {ok, _, [{MsgId}]} = ?SUP:e(
        Conn, "SELECT message_id FROM enterprise_asset WHERE id=$1", [Id]
    ),
    {ok, #{
        measured => #{
            bind_committed => true,
            bind_surface => <<"eb_pg_store">>,
            intent_rejected => <<"23510">>,
            message_id => MsgId
        },
        proof => Blocked,
        cleanup => ok
    }};
%% (3) intent_commits_first ordinary：intent 事务内先执行 => 生产绑定入口在
%% 对象锁上真实阻塞 => intent 提交 => 墓碑拒绝，零业务行。
rr_step(S, <<"intent_commits_first">>, <<"ordinary">>, Key, Id) ->
    #{conn := Conn} = S,
    ?assertEqual(true, ?SUP:refresh_gc_verified(Conn, Key)),
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, {advisory, Key}, fun(Pids) ->
                ?SUP:q(
                    Conn,
                    "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 concurrent intent')",
                    [Key]
                ),
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:q(Other, "BEGIN"),
                    R = attachment_repo:bind_reference_lock_tx(Other, Key),
                    ?SUP:q(Other, "ROLLBACK"),
                    R
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(Conn, "COMMIT"),
                ?assertEqual({error, tombstone}, ?SUP:collect_step(Ref, 12000)),
                B
            end)
        after
            epgsql:close(Other)
        end,
    {ok, _, [{0}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM group_file WHERE id=$1", [Id]),
    {ok, _, [{<<"delete_intent">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
    ),
    {ok, #{
        measured => #{
            intent_committed => true,
            production_bind_rejected => <<"tombstone">>,
            group_file_rows => 0,
            lifecycle_state => <<"delete_intent">>
        },
        proof => Blocked,
        cleanup => ok
    }};
%% (4) intent_commits_first enterprise：intent 先提交 => 生产 store 绑定锁内
%% 读到墓碑、绑定被拒（not_bound），message_id 不落。
rr_step(S, <<"intent_commits_first">>, <<"enterprise">>, _Key, Id) ->
    #{conn := Conn} = S,
    Other = ?SUP:barrier_connect(S),
    MsgId = message_id_of(Id),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:eb_row_lock(Id), fun(Pids) ->
                ?SUP:q(
                    Conn, "SELECT fn_v5_intent_eb_delete(9101,9102,$1,'w601w3 intent first')", [
                        Id
                    ]
                ),
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:q(Other, "BEGIN"),
                    Locked = eb_pg_store:lock_assets_in(Other, 9101, 9102, [Id]),
                    Bind = eb_pg_store:bind_asset_message_in(
                        Other, 9101, 9102, Id, MsgId, erlang:system_time(second) + 172800
                    ),
                    ?SUP:q(Other, "ROLLBACK"),
                    {Locked, Bind}
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(Conn, "COMMIT"),
                {{ok, [#{lifecycle_state := <<"delete_intent">>}]}, {error, not_bound}} =
                    ?SUP:collect_step(Ref, 12000),
                B
            end)
        after
            epgsql:close(Other)
        end,
    {ok, _, [{<<"delete_intent">>, null}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state,message_id FROM enterprise_asset WHERE id=$1", [Id]
    ),
    {ok, #{
        measured => #{
            intent_committed => true,
            store_lock_saw => <<"delete_intent">>,
            bind_rejected => <<"not_bound">>,
            lifecycle_state => <<"delete_intent">>,
            message_id_null => true
        },
        proof => Blocked,
        cleanup => ok
    }};
%% (5) direct_dml_bind_commits_first ordinary：直接受限 DML 绑定（UPDATE+INSERT）
%% 先提交 => intent 23510。屏障取 advisory(path)（生产锁序第一把，同 rr(1)）：
%% waiter 不得在行锁阻塞前持任何锁——holder 的 group_file 触发器
%% （trg_v5_group_file_reference_guard）会请求同一把 advisory(path)。
rr_step(S, <<"direct_dml_bind_commits_first">>, <<"ordinary">>, Key, Id) ->
    #{conn := Conn} = S,
    ?assertEqual(true, ?SUP:refresh_gc_verified(Conn, Key)),
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, {advisory, Key}, fun(Pids) ->
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 dml bind')",
                        [Key]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(Conn, "UPDATE attachment SET group_file_id=$2 WHERE path=$1", [Key, Id]),
                ?SUP:bind_group_file(Conn, Id),
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
    {ok, #{
        measured => #{
            dml_bind_committed => true,
            intent_rejected => <<"23510">>
        },
        proof => Blocked,
        cleanup => fun() -> ?SUP:unbind_group_file(Conn, Id) end
    }};
%% (6) direct_dml_bind_commits_first enterprise：裸 UPDATE 绑定（message_id+
%% retain）先提交 => intent 23510（enterprise retention active）。
rr_step(S, <<"direct_dml_bind_commits_first">>, <<"enterprise">>, _Key, Id) ->
    #{conn := Conn} = S,
    Other = ?SUP:barrier_connect(S),
    MsgId = message_id_of(Id),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:eb_row_lock(Id), fun(Pids) ->
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "SELECT fn_v5_intent_eb_delete(9101,9102,$1,'w601w3 dml bind')",
                        [Id]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(
                    Conn,
                    "UPDATE enterprise_asset SET message_id=$2,"
                    "retain_until=now()+interval '2 days' WHERE id=$1",
                    [Id, MsgId]
                ),
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
    {ok, #{
        measured => #{
            dml_bind_committed => true,
            intent_rejected => <<"23510">>,
            message_id => MsgId
        },
        proof => Blocked,
        cleanup => ok
    }};
%% (7) direct_dml_intent_commits_first ordinary：intent 先提交 => 直接 DML
%% 绑定条件 UPDATE 命中 0 行（lifecycle 已非 live）。
rr_step(S, <<"direct_dml_intent_commits_first">>, <<"ordinary">>, Key, Id) ->
    #{conn := Conn} = S,
    ?assertEqual(true, ?SUP:refresh_gc_verified(Conn, Key)),
    Other = ?SUP:barrier_connect(S),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, {advisory, Key}, fun(Pids) ->
                ?SUP:q(
                    Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 dml intent')", [Key]
                ),
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "WITH u AS (UPDATE attachment SET group_file_id=$2 WHERE path=$1"
                        " AND lifecycle_state='live' RETURNING 1) SELECT COUNT(*) FROM u",
                        [Key, Id]
                    )
                end),
                B = ?SUP:blocking_proof(Conn, Pids, 100),
                ?SUP:q(Conn, "COMMIT"),
                {ok, _, [{0}]} = ?SUP:collect_step(Ref, 12000),
                B
            end)
        after
            epgsql:close(Other)
        end,
    {ok, _, [{0}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM group_file WHERE id=$1", [Id]),
    {ok, #{
        measured => #{
            intent_committed => true,
            dml_bind_rows => 0
        },
        proof => Blocked,
        cleanup => ok
    }};
%% (8) direct_dml_intent_commits_first enterprise：intent 先提交 => 裸 UPDATE
%% 绑定被墓碑触发器拒（23510），message_id 保持 NULL。
rr_step(S, <<"direct_dml_intent_commits_first">>, <<"enterprise">>, _Key, Id) ->
    #{conn := Conn} = S,
    Other = ?SUP:barrier_connect(S),
    MsgId = message_id_of(Id),
    Blocked =
        try
            ?SUP:with_row_lock_barrier(Conn, Other, ?SUP:eb_row_lock(Id), fun(Pids) ->
                ?SUP:q(
                    Conn, "SELECT fn_v5_intent_eb_delete(9101,9102,$1,'w601w3 dml intent')", [
                        Id
                    ]
                ),
                Ref = ?SUP:start_step(fun() ->
                    ?SUP:e(
                        Other,
                        "UPDATE enterprise_asset SET message_id=$2,"
                        "retain_until=now()+interval '2 days' WHERE id=$1",
                        [Id, MsgId]
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
    {ok, _, [{<<"delete_intent">>, null}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state,message_id FROM enterprise_asset WHERE id=$1", [Id]
    ),
    {ok, #{
        measured => #{
            intent_committed => true,
            dml_bind_rejected => <<"23510">>,
            lifecycle_state => <<"delete_intent">>,
            message_id_null => true
        },
        proof => Blocked,
        cleanup => ok
    }}.

message_id_of(Id) -> ?ASSET_BASE + 100000 + Id rem 100000.

%% ====================================================== shared-owner（8 叶）

so_units() ->
    [
        {<<"owner_logout">>, <<"ordinary">>},
        {<<"owner_logout">>, <<"enterprise">>},
        {<<"owner_account_delete">>, <<"ordinary">>},
        {<<"owner_account_delete">>, <<"enterprise">>},
        {<<"original_post_remove">>, <<"ordinary">>},
        {<<"original_post_remove">>, <<"enterprise">>},
        {<<"unrelated_reader_denied">>, <<"ordinary">>},
        {<<"unrelated_reader_denied">>, <<"enterprise">>}
    ].

%% 步返回统一 {ok, #{measured, proof, cleanup}}；Proof（identity/blocking）与
%% 探针证据由 shared_owner 合并层统一并入。
shared_owner(S, Driver, ScBin, DomBin, Idx) ->
    Key = so_key(Driver, ScBin, DomBin),
    Id = leaf_id(Driver, Idx),
    Quad = ?SUP:seed_identity(S, Driver, Key),
    seed_so_asset(S, ScBin, DomBin, Key, Id, Quad),
    Probe = ?SUP:probe_io(Key, Quad),
    {ok, #{measured := Measured, proof := Proof, cleanup := Cleanup}} = so_step(
        S, ScBin, DomBin, Key, Id, Quad
    ),
    ok = ?SUP:run_cleanup(Cleanup),
    maps:merge(
        maps:merge(Measured#{asset_path => Key, attachment_id => Id}, Probe),
        maps:merge(?SUP:identity_proof(Key, Quad), Proof)
    ).

seed_so_asset(S, <<"original_post_remove">>, <<"enterprise">>, Key, Id, Quad) ->
    ok = seed_enterprise_ref(S, Key, Id, Quad),
    #{conn := Conn} = S,
    ok = ?SUP:seed_eb_row(Conn, Key, eb_asset_id(Id), Quad);
seed_so_asset(S, <<"unrelated_reader_denied">>, <<"ordinary">>, Key, Id, Quad) ->
    #{conn := Conn} = S,
    ok = ?SUP:seed_confirmed(Conn, Key, Id, Id, Quad, <<"group">>, <<"66">>, null),
    ok = ?SUP:bind_group_file(Conn, Id);
seed_so_asset(S, <<"unrelated_reader_denied">>, <<"enterprise">>, Key, Id, Quad) ->
    ok = seed_enterprise_ref(S, Key, Id, Quad);
seed_so_asset(S, <<"owner_logout">>, <<"ordinary">>, Key, Id, Quad) ->
    #{conn := Conn} = S,
    ok = ?SUP:seed_confirmed(Conn, Key, Id, Id, Quad, <<"group">>, <<"66">>, null),
    ok = ?SUP:bind_group_file(Conn, Id);
seed_so_asset(S, <<"owner_logout">>, <<"enterprise">>, Key, Id, Quad) ->
    ok = seed_enterprise_ref(S, Key, Id, Quad);
seed_so_asset(S, <<"owner_account_delete">>, <<"ordinary">>, Key, Id, Quad) ->
    #{conn := Conn} = S,
    Hash = ?SUP:hex64(crypto:hash(sha256, integer_to_binary(Id))),
    ok = ?SUP:seed_plain_row(Conn, Key, Id, Quad, <<"private">>, null, false, Hash);
seed_so_asset(S, <<"owner_account_delete">>, <<"enterprise">>, Key, Id, Quad) ->
    ok = seed_enterprise_ref(S, Key, Id, Quad);
seed_so_asset(S, <<"original_post_remove">>, <<"ordinary">>, Key, Id, Quad) ->
    #{conn := Conn} = S,
    ok = ?SUP:seed_plain_row(
        Conn,
        Key,
        Id,
        Quad,
        <<"private">>,
        null,
        true,
        ?SUP:hex64(crypto:hash(sha256, integer_to_binary(Id)))
    ).

so_key(Driver, ScBin, DomBin) ->
    <<"v5e2e/w601/", Driver/binary, "/so/", ScBin/binary, "/", DomBin/binary>>.

%% (1) owner_logout ordinary：群绑定资产（绑定活跃），creator 注销不得删除。
so_step(S, <<"owner_logout">>, <<"ordinary">>, Key, Id, _Quad) ->
    #{conn := Conn} = S,
    {ok, _, [{Cnt}]} = ?SUP:e(Conn, owner_delete_sql(), [Key]),
    ?assertEqual(0, Cnt),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, #{
        measured => #{
            confirmed_group_bound => true,
            deletion_deleted_rows => 0,
            attachment_rows_after => 1
        },
        proof => #{},
        cleanup => fun() -> ?SUP:unbind_group_file(Conn, Id) end
    }};
%% (2) owner_logout enterprise：scope='enterprise' 引用行不随个人注销删除
%% （注销删除 SQL 的 scope NOT IN 排除面）。
so_step(S, <<"owner_logout">>, <<"enterprise">>, Key, _Id, _Quad) ->
    #{conn := Conn} = S,
    {ok, _, [{Cnt}]} = ?SUP:e(Conn, owner_delete_sql(), [Key]),
    ?assertEqual(0, Cnt),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, #{
        measured => #{
            enterprise_scope_excluded => true,
            deletion_deleted_rows => 0,
            attachment_rows_after => 1
        },
        proof => #{},
        cleanup => ok
    }};
%% (3) owner_account_delete ordinary：被收藏的个人资产 hash 关联保护。
so_step(S, <<"owner_account_delete">>, <<"ordinary">>, Key, Id, _Quad) ->
    #{conn := Conn} = S,
    CollectId = collect_id(Id),
    ok = seed_collect(Conn, CollectId, ?SUP:hex64(crypto:hash(sha256, integer_to_binary(Id)))),
    {ok, _, [{Cnt}]} = ?SUP:e(Conn, owner_delete_sql(), [Key]),
    ?assertEqual(0, Cnt),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, #{
        measured => #{
            collected_hash_protected => true,
            deletion_deleted_rows => 0,
            attachment_rows_after => 1
        },
        proof => #{},
        cleanup => fun() -> ?SUP:q(Conn, "DELETE FROM user_collect WHERE id=$1", [CollectId]) end
    }};
%% (4) owner_account_delete enterprise：带收藏 hash 关联的企业引用同样被排除。
so_step(S, <<"owner_account_delete">>, <<"enterprise">>, Key, Id, _Quad) ->
    #{conn := Conn} = S,
    CollectId = collect_id(Id),
    ok = seed_collect(Conn, CollectId, ?SUP:hex64(crypto:hash(sha256, integer_to_binary(Id)))),
    {ok, _, [{Cnt}]} = ?SUP:e(Conn, owner_delete_sql(), [Key]),
    ?assertEqual(0, Cnt),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, #{
        measured => #{
            enterprise_scope_excluded => true,
            collected_hash_protected => true,
            deletion_deleted_rows => 0,
            attachment_rows_after => 1
        },
        proof => #{},
        cleanup => fun() -> ?SUP:q(Conn, "DELETE FROM user_collect WHERE id=$1", [CollectId]) end
    }};
%% (5) original_post_remove ordinary：无共享证据的个人明文附件——真实注销
%% 路径（intent 提交 + 队列 tombstone）后元数据删除 1 行；删除前经真实读
%% 鉴权入口（attach_logic:view_url）可读，删除后同一入口拒绝（不可读）。
so_step(S, <<"original_post_remove">>, <<"ordinary">>, Key, _Id, _Quad) ->
    #{conn := Conn} = S,
    {ok, ReadUrl} = attach_logic:view_url(101, Key),
    true = is_binary(ReadUrl) andalso ReadUrl =/= <<>>,
    {ok, _, _} = ?SUP:e(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601w3 account deletion')", [Key]
    ),
    {ok, _, [{Cnt}]} = ?SUP:e(Conn, personal_delete_sql(), [Key]),
    ?assertEqual(1, Cnt),
    {ok, _, [{0}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    ?assertEqual({error, forbidden}, attach_logic:view_url(101, Key)),
    {ok, #{
        measured => #{
            intent_committed => true,
            readable_before_delete => true,
            read_entry_point => <<"attach_logic:view_url/2">>,
            unreadable_after_delete => true,
            deletion_deleted_rows => 1,
            attachment_rows_after => 0
        },
        proof => #{},
        cleanup => ok
    }};
%% (6) original_post_remove enterprise：企业引用释放（fn_v5_intent_eb_delete）
%% 后 attachment scope='enterprise' 行仍在且注销删除仍排除。
so_step(S, <<"original_post_remove">>, <<"enterprise">>, Key, Id, _Quad) ->
    #{conn := Conn} = S,
    EbId = eb_asset_id(Id),
    {ok, _, _} = ?SUP:e(
        Conn,
        "SELECT fn_v5_intent_eb_delete(9101,9102,$1,'w601w3 shared owner eb')",
        [EbId]
    ),
    {ok, _, [{Cnt}]} = ?SUP:e(Conn, personal_delete_sql(), [Key]),
    ?assertEqual(0, Cnt),
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, _, [{<<"delete_intent">>}]} = ?SUP:e(
        Conn, "SELECT lifecycle_state FROM enterprise_asset WHERE id=$1", [EbId]
    ),
    {ok, #{
        measured => #{
            eb_intent_committed => true,
            lifecycle_state => <<"delete_intent">>,
            deletion_deleted_rows => 0,
            attachment_rows_after => 1
        },
        proof => #{},
        cleanup => ok
    }};
%% (7) unrelated_reader_denied ordinary：真实群绑定资产在场，非群成员经真实
%% 读鉴权入口（attach_logic:view_url）拒绝，删除任务细节不可见。
so_step(S, <<"unrelated_reader_denied">>, <<"ordinary">>, Key, _Id, _Quad) ->
    #{conn := Conn} = S,
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    ?assertEqual({error, forbidden}, attach_logic:view_url(999999, Key)),
    {ok, _, [{0}]} = ?SUP:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    {ok, #{
        measured => #{
            unrelated_reader_authorized => false,
            read_entry_point => <<"attach_logic:view_url/2">>,
            asset_present => true,
            deletion_queue_rows => 0
        },
        proof => #{},
        cleanup => ok
    }};
%% (8) unrelated_reader_denied enterprise：真实企业引用资产在场，非组织成员
%% 经真实读鉴权入口拒绝。
so_step(S, <<"unrelated_reader_denied">>, <<"enterprise">>, Key, _Id, _Quad) ->
    #{conn := Conn} = S,
    {ok, _, [{1}]} = ?SUP:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    ?assertEqual({error, forbidden}, attach_logic:view_url(999999, Key)),
    {ok, #{
        measured => #{
            unrelated_reader_authorized => false,
            read_entry_point => <<"attach_logic:view_url/2">>,
            asset_present => true
        },
        proof => #{},
        cleanup => ok
    }}.

eb_asset_id(Id) -> ?ASSET_BASE + 100000 + Id rem 100000.

collect_id(Id) -> ?ASSET_BASE + 200000 + Id rem 100000.

%% 企业引用行（scope='enterprise'）：注销删除的排除面对偶形态。
seed_enterprise_ref(S, Key, Id, Quad) ->
    #{conn := Conn} = S,
    ?SUP:seed_plain_row(
        Conn,
        Key,
        Id,
        Quad,
        <<"enterprise">>,
        <<"org:9101">>,
        true,
        ?SUP:hex64(crypto:hash(sha256, integer_to_binary(Id)))
    ).

seed_collect(Conn, CollectId, Hash) ->
    ?SUP:q(
        Conn,
        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
        " VALUES ($1,202,0,'x',$2,1)"
        " ON CONFLICT (id) DO UPDATE SET attach_file_hash256=EXCLUDED.attach_file_hash256",
        [CollectId, Hash]
    ),
    ok.

%% 注销删除谓词（pg_tests:w301_shared_owner 逐字）：共享证据任一成立即排除。
owner_delete_sql() ->
    <<
        "WITH del AS (DELETE FROM attachment a WHERE a.creator_user_id=101 AND a.path=$1"
        " AND a.cipher IS NULL AND a.group_file_id IS NULL"
        " AND a.scope NOT IN ('group','channel','enterprise')"
        " AND NOT EXISTS (SELECT 1 FROM group_file gf WHERE gf.id=a.group_file_id)"
        " AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "   AND uc.attach_file_hash256 = a.file_hash256 AND uc.status = 1)"
        " AND NOT EXISTS (SELECT 1 FROM moment_post mp WHERE position(a.path in mp.media::text) > 0)"
        " RETURNING 1) SELECT COUNT(*) FROM del"
    >>.

%% 原帖删除路径的元数据删除谓词（pg_tests:original_post_remove 逐字）。
personal_delete_sql() ->
    <<
        "WITH del AS (DELETE FROM attachment a WHERE a.creator_user_id=101 AND a.path=$1"
        " AND a.cipher IS NULL AND a.group_file_id IS NULL"
        " AND a.scope NOT IN ('group','channel','enterprise')"
        " AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "   AND uc.attach_file_hash256 = a.file_hash256 AND uc.status = 1)"
        " RETURNING 1) SELECT COUNT(*) FROM del"
    >>.

indexed(List) ->
    lists:zip(lists:seq(1, length(List)), List).
