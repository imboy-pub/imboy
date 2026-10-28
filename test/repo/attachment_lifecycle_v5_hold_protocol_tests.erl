%% attachment_lifecycle_v5_hold_protocol_tests — W3-02 Retention/Hold
%% 统一资格与 Hold 竞争协议（02 §5，fixture 映射协议级；12 生产入口的
%% 真实改造与整矩阵复验属 W4-01，不作为 G3 条件）。
%%
%% hold-all-entrypoints：12 入口 fixture × 6 治理 fixture
%%（hold_workspace / hold_conversation / hold_message / hold_attachment /
%%  future_retention / permanent_workspace_retention）=> intent 全部 23510。
%% hold-gc-race：hold 先提交 => intent 拒绝；intent 先提交 => 治理操作
%% already_deleting。
-module(attachment_lifecycle_v5_hold_protocol_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(ASSET_BASE, 920000000).

hold_protocol_test_() ->
    {timeout, 900, fun run/0}.

run() ->
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W302">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    try
        q(
            Conn,
            "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
            " account_type, reg_ip, reg_cosv, created_at)"
            " VALUES (101, 'w302-u', 'w302', '', 1, 0, '127.0.0.1', 'w302', now())"
        ),
        q(
            Conn,
            "INSERT INTO organization(id,name,owner_id)"
            " VALUES (9101,'w302-org',101)"
        ),
        q(
            Conn,
            "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
            " VALUES (9101,9102,'w302-ws',101,'{}')"
        ),
        %% EAR 行（hold_message/hold_attachment/future_retention）引用
        %% (organization_id, application_id)=(9101,9103)，须先种 Application
        q(
            Conn,
            "INSERT INTO enterprise_application"
            " (id, organization_id, application_key, name, status)"
            " VALUES (9103, 9101, 'w302-9103', 'w302 app', 'active')"
            " ON CONFLICT (organization_id, id) DO NOTHING"
        ),
        lists:foreach(
            fun({Ep, ScopeVal, Cipher, N}) ->
                w302_entrypoint_matrix(Conn, Ep, ScopeVal, Cipher, N)
            end,
            entrypoints()
        ),
        w302_hold_gc_race(Conn)
    after
        inttest_marker_db:release(State)
    end.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        {error, Reason} ->
            erlang:error({w302_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _, _} = Ok -> Ok;
        {error, _} = Err -> Err
    end.

%% 12 未来删除入口的 fixture 映射（§6 入口表；协议同 fn_v5_intent_ordinary_delete；
%% 密文/历史歧义入口按 02 §5 语义以 cipher 形态表达——资格协议同一 fn）
entrypoints() ->
    [
        {reject_size, <<"group">>, null, 10},
        {reject_mime, <<"group">>, null, 11},
        {reject_teaching, <<"group">>, null, 22},
        {orphan_cleanup, <<"moment">>, null, 13},
        {pending_cleanup, <<"private">>, null, 14},
        {account_deletion, <<"private">>, null, 15},
        {teaching_unbound, <<"group">>, null, 23},
        {app_confirm_reject, <<"enterprise">>, null, 17},
        {app_retention_purge, <<"enterprise">>, null, 18},
        {eb_private_delete, <<"c2c">>, null, 19},
        {eb_pending_cleanup, <<"moment">>, <<"AES-256-GCM">>, 20},
        {eb_purge_drain, <<"private">>, null, 21}
    ].

%% 叶矩阵域归属：eb-* 三入口为 enterprise 域，其余入口协议单元为 ordinary。
hold_domain(eb_private_delete) -> <<"enterprise">>;
hold_domain(eb_pending_cleanup) -> <<"enterprise">>;
hold_domain(eb_purge_drain) -> <<"enterprise">>;
hold_domain(_) -> <<"ordinary">>.

ep_binary(Ep) ->
    iolist_to_binary(string:replace(atom_to_binary(Ep, utf8), <<"_">>, <<"-">>, all)).

scope_binary(Scope) ->
    atom_to_binary(Scope, utf8).

hex64(Base) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Base)]).

%% 单入口 fixture：confirmed 资产（绑定释放形态 => 引用零可证明）+ 能力窗已过
seed(Conn, Key, ScopeVal, Cipher, N) ->
    ScopeRef = scope_ref_fixture(ScopeVal),
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b',$2,101,"
        "gen_random_uuid(),$3::jsonb,now() + interval '2 hours',"
        "now() + interval '3 seconds','garage','ns1','c1',$4)",
        [Key, ScopeVal, registration_fixture(ScopeVal, ScopeRef), locator_fixture(N)]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_pin_pending_content($1,$2::jsonb)",
        [Key, receipt_fixture(N)]
    ),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,$3,$7,101,'garage','ns1','c1',$4,false,$5,NULL,$6)",
        [
            ?ASSET_BASE + N,
            Key,
            ScopeVal,
            locator_fixture(N),
            Cipher,
            hex64(crypto:hash(sha256, integer_to_binary(N))),
            ScopeRef
        ]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_BASE + N]
    ),
    await_upload_expiry(Conn, Key),
    ok.

registration_fixture(Scope, ScopeRef) ->
    jsone:encode(#{
        <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => <<"101">>},
        <<"declaration">> => #{
            <<"mime">> => <<"image/png">>,
            <<"scope">> => Scope,
            <<"scope_ref">> => ScopeRef
        }
    }).

scope_ref_fixture(<<"group">>) -> <<"66">>;
scope_ref_fixture(<<"c2c">>) -> <<"c2c:101:202">>;
scope_ref_fixture(<<"enterprise">>) -> <<"org:9101/app:9103">>;
scope_ref_fixture(_) -> null.

receipt_fixture(N) ->
    Hex = hex64(crypto:hash(sha256, integer_to_binary(N))),
    [
        "{\"version\":1,\"sha256\":\"",
        Hex,
        "\",\"size_bytes\":\"16\","
        "\"mime\":\"image/png\"}"
    ].

locator_fixture(N) ->
    hex64(crypto:hash(sha256, integer_to_binary(N))).

%% 治理 fixture：按 scope 安装阻尼行 => 返回 {ok, 清理函数}
install_governance(Conn, AttId, hold_workspace) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'group','66')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope = 'group',"
        " scope_ref = '66'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, hold_conversation) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'channel','66')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope = 'channel',"
        " scope_ref = '66'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, hold_message) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now()+interval '30 days','held',now(),'w302 message hold')",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM enterprise_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, hold_attachment) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now()+interval '30 days','held',now(),'w302 attachment hold')",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM enterprise_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, future_retention) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        %% ck_ear_hold_reason：hold_state='none' 时 hold_reason 须为 NULL
        " VALUES ($1,9101,9103,now() + interval '50 years','none',NULL,NULL)",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM enterprise_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, permanent_workspace_retention) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'group','66-permanent')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope = 'group',"
        " scope_ref = '66-permanent'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end.

w302_entrypoint_matrix(Conn, Ep, ScopeVal, Cipher, N) ->
    %% 6 治理 fixture：WAR/EAR 行不可变（迁移触发器）——每个 fixture 用全新
    %% 资产（无复用、无清理），安装后 intent 必须 23510（资格协议拒绝）。
    lists:foreach(
        fun({Idx, Scope}) ->
            Key = [
                "v5e2e/w302/",
                atom_to_binary(Ep, utf8),
                "/",
                integer_to_binary(Idx)
            ],
            N2 = N * 10 + Idx,
            seed(Conn, Key, ScopeVal, Cipher, N2),
            AttId = ?ASSET_BASE + N2,
            %% 明文先证明可清理，避免 gc_verified=false 掩盖真正的 hold 拒绝。
            %% Make plaintext otherwise eligible before installing its hold.
            ?assertEqual(
                {ok, Cipher =:= null},
                attachment_repo:refresh_gc_verified_tx(Conn, iolist_to_binary(Key))
            ),
            install_governance(Conn, AttId, Scope),
            R = e(
                Conn,
                "SELECT fn_v5_intent_ordinary_delete($1,'w302')",
                [Key]
            ),
            ?assertMatch({error, #error{code = <<"23510">>}}, R),
            ok = epgsql:sync(Conn),
            attachment_lifecycle_v5_obs:emit(#{
                case_id => <<"W3-02/hold-all-entrypoints">>,
                entrypoint => ep_binary(Ep),
                scenario => scope_binary(Scope),
                domain => hold_domain(Ep),
                schema_phase => <<"expand-queue">>,
                measured => #{
                    asset_path => iolist_to_binary(Key),
                    attachment_id => ?ASSET_BASE + N2,
                    attachment_scope => ScopeVal,
                    governance_fixture => scope_binary(Scope),
                    gc_verified_refresh_ok => true,
                    intent_rejected => <<"23510">>
                }
            })
        end,
        lists:zip(lists:seq(1, 6), [
            hold_workspace,
            hold_conversation,
            hold_message,
            hold_attachment,
            future_retention,
            permanent_workspace_retention
        ])
    ),
    ok.

%% hold-gc-race：hold 先提交 => intent 拒绝；intent 先提交 => 治理
%% already_deleting（生产入口 governance_tx）+ 治理不复活已删对象。
%% 双域：ordinary 走 WAR/EAR fixture 协议面 + 治理不追认；enterprise 走
%% 生产 governance_tx 的显式 already_deleting 拒绝。
w302_hold_gc_race(Conn) ->
    %% (1) hold_commits_first ordinary：WAR hold 行先提交 => intent 23510
    Key1 = <<"v5e2e/w302/race/hold-first">>,
    seed(Conn, Key1, <<"group">>, null, 40),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key1)),
    AttId1 = ?ASSET_BASE + 40,
    Cleanup1 = install_governance(Conn, AttId1, hold_message),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 race')", [Key1]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key1]
    ),
    Cleanup1,
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W3-02/hold-gc-race">>,
        entrypoint => <<"hold-gc-race">>,
        scenario => <<"hold_commits_first">>,
        domain => <<"ordinary">>,
        schema_phase => <<"expand-queue">>,
        measured => #{
            asset_path => Key1,
            hold_fixture => <<"hold_message">>,
            intent_rejected => <<"23510">>,
            attachment_rows_after => 1
        }
    }),
    %% (2) hold_commits_first enterprise：生产入口 governance_tx(hold) 先提交
    %% => EAR held => intent 23510，行保留
    Key2 = <<"v5e2e/w302/race/hold-first-eb">>,
    seed(Conn, Key2, <<"enterprise">>, null, 42),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key2)),
    Ctx = #{organization_id => 9101, application_id => 9103},
    {ok, _} = enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
        object_key => Key2,
        op => <<"hold">>,
        reason => <<"w302 hold commits first (enterprise)">>
    }),
    {ok, _, [{<<"held">>}]} = epgsql:equery(
        Conn,
        "SELECT hold_state FROM enterprise_attachment_retention WHERE attachment_id=$1",
        [?ASSET_BASE + 42]
    ),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 race eb')", [Key2]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key2]
    ),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W3-02/hold-gc-race">>,
        entrypoint => <<"hold-gc-race">>,
        scenario => <<"hold_commits_first">>,
        domain => <<"enterprise">>,
        schema_phase => <<"expand-queue">>,
        measured => #{
            asset_path => Key2,
            governance_entry => <<"governance_tx(hold)">>,
            hold_state => <<"held">>,
            intent_rejected => <<"23510">>,
            attachment_rows_after => 1
        }
    }),
    %% (3) intent_before_asset_hold ordinary：intent 先提交 => 治理不追认
    %% （生产 repo hold_tx 可安装 EAR 行，但资格层不复活：lifecycle 保持
    %% delete_intent，元数据不重建）
    Key3 = <<"v5e2e/w302/race/intent-first-ord">>,
    seed(Conn, Key3, <<"private">>, null, 43),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key3)),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 race intent ord')", [Key3]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment WHERE path=$1 AND lifecycle_state='delete_intent'",
        [Key3]
    ),
    {ok, _} = enterprise_attachment_retention_repo:upsert_tx(
        Conn, ?ASSET_BASE + 43, 9101, 9103
    ),
    ok = enterprise_attachment_retention_repo:hold_tx(
        Conn, ?ASSET_BASE + 43, <<"w302 late hold (ordinary)">>
    ),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state FROM attachment WHERE path=$1",
        [Key3]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key3]
    ),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W3-02/hold-gc-race">>,
        entrypoint => <<"hold-gc-race">>,
        scenario => <<"intent_before_asset_hold">>,
        domain => <<"ordinary">>,
        schema_phase => <<"expand-queue">>,
        measured => #{
            asset_path => Key3,
            intent_committed => true,
            late_hold_installed => true,
            lifecycle_state_after => <<"delete_intent">>,
            deletion_queue_rows_after_late_hold => 1
        }
    }),
    %% (4) intent_before_asset_hold enterprise：intent 先提交 =>
    %% 生产 governance_tx(hold) 显式 already_deleting
    Key4 = <<"v5e2e/w302/race/intent-first">>,
    seed(Conn, Key4, <<"enterprise">>, null, 41),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key4)),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 race intent')", [Key4]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment WHERE path=$1 AND lifecycle_state='delete_intent'",
        [Key4]
    ),
    {error, {<<"invalid_request">>, already_deleting}} =
        enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
            object_key => Key4,
            op => <<"hold">>,
            reason => <<"w302 late hold">>
        }),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W3-02/hold-gc-race">>,
        entrypoint => <<"hold-gc-race">>,
        scenario => <<"intent_before_asset_hold">>,
        domain => <<"enterprise">>,
        schema_phase => <<"expand-queue">>,
        measured => #{
            asset_path => Key4,
            intent_committed => true,
            governance_rejection => <<"already_deleting">>
        }
    }),
    %% (5) intent_before_retention_extension ordinary：intent 先提交 =>
    %% 留存延长（生产 repo extend_tx）不复活已删对象
    Key5 = <<"v5e2e/w302/race/retain-ext-ord">>,
    seed(Conn, Key5, <<"private">>, null, 44),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key5)),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 race retain ord')", [Key5]
    ),
    {ok, _} = enterprise_attachment_retention_repo:upsert_tx(
        Conn, ?ASSET_BASE + 44, 9101, 9103
    ),
    NewUntil5 = elib_dt:add(elib_dt:now(), {3650 * 86400, second}),
    {ok, _} = enterprise_attachment_retention_repo:extend_tx(
        Conn, ?ASSET_BASE + 44, NewUntil5
    ),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state FROM attachment WHERE path=$1",
        [Key5]
    ),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W3-02/hold-gc-race">>,
        entrypoint => <<"hold-gc-race">>,
        scenario => <<"intent_before_retention_extension">>,
        domain => <<"ordinary">>,
        schema_phase => <<"expand-queue">>,
        measured => #{
            asset_path => Key5,
            intent_committed => true,
            retention_extended => true,
            lifecycle_state_after => <<"delete_intent">>
        }
    }),
    %% (6) intent_before_retention_extension enterprise：intent 先提交 =>
    %% 生产 governance_tx(set_retention) 显式 already_deleting
    Key6 = <<"v5e2e/w302/race/retain-ext-eb">>,
    seed(Conn, Key6, <<"enterprise">>, null, 45),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key6)),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 race retain eb')", [Key6]
    ),
    {error, {<<"invalid_request">>, already_deleting}} =
        enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
            object_key => Key6,
            op => <<"set_retention">>,
            retention_days => 90
        }),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state FROM attachment WHERE path=$1",
        [Key6]
    ),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W3-02/hold-gc-race">>,
        entrypoint => <<"hold-gc-race">>,
        scenario => <<"intent_before_retention_extension">>,
        domain => <<"enterprise">>,
        schema_phase => <<"expand-queue">>,
        measured => #{
            asset_path => Key6,
            intent_committed => true,
            governance_rejection => <<"already_deleting">>,
            lifecycle_state_after => <<"delete_intent">>
        }
    }),
    %% (7) workspace_hold_exclusion_receipt ordinary：workspace hold（WAR 行
    %% 不可变）明确排除对象——intent 拒且重放恒拒，行不复活、无删除队列。
    Key7 = <<"v5e2e/w302/race/ws-hold-receipt-ord">>,
    seed(Conn, Key7, <<"group">>, null, 46),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key7)),
    AttId7 = ?ASSET_BASE + 46,
    _Cleanup7 = install_governance(Conn, AttId7, hold_workspace),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 ws receipt')", [Key7]),
    ok = epgsql:sync(Conn),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 ws receipt 2')", [Key7]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key7]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM workspace_attachment_retention WHERE attachment_id=$1",
        [AttId7]
    ),
    {ok, _, [{0}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key7]
    ),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W3-02/hold-gc-race">>,
        entrypoint => <<"hold-gc-race">>,
        scenario => <<"workspace_hold_exclusion_receipt">>,
        domain => <<"ordinary">>,
        schema_phase => <<"expand-queue">>,
        measured => #{
            asset_path => Key7,
            hold_excluded_intent => <<"23510">>,
            exclusion_persistent => true,
            attachment_rows_after => 1,
            war_rows => 1,
            deletion_queue_rows => 0
        }
    }),
    %% (8) workspace_hold_exclusion_receipt enterprise：生产 governance_tx
    %% hold/re-intent/释放闭环：deleted 对象被显式排除（already_deleting）
    Key8 = <<"v5e2e/w302/race/ws-hold-receipt-eb">>,
    seed(Conn, Key8, <<"enterprise">>, null, 47),
    ?assertEqual({ok, true}, attachment_repo:refresh_gc_verified_tx(Conn, Key8)),
    {ok, _} = enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
        object_key => Key8,
        op => <<"hold">>,
        reason => <<"w302 ws receipt (enterprise)">>
    }),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 ws receipt eb')", [Key8]),
    ok = epgsql:sync(Conn),
    {ok, _} = enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
        object_key => Key8,
        op => <<"release_hold">>
    }),
    %% release（生产入口）后 hold_state 回 none，但 EAR 实例留存窗仍在
    %% ——intent 仍被显式排除（future retention 语义），行不复活。
    {ok, _, [{<<"none">>}]} = epgsql:equery(
        Conn,
        "SELECT hold_state FROM enterprise_attachment_retention WHERE attachment_id=$1",
        [?ASSET_BASE + 47]
    ),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 ws receipt eb 2')", [Key8]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key8]
    ),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W3-02/hold-gc-race">>,
        entrypoint => <<"hold-gc-race">>,
        scenario => <<"workspace_hold_exclusion_receipt">>,
        domain => <<"enterprise">>,
        schema_phase => <<"expand-queue">>,
        measured => #{
            asset_path => Key8,
            hold_excluded_intent => <<"23510">>,
            released_hold_state => <<"none">>,
            retention_window_still_excludes => <<"23510">>,
            attachment_rows_after => 1
        }
    }),
    ok.

%% Pin/confirm must finish inside the real capability window. Never shrink it.
%% Wait on the database clock, capped at five seconds; a missed window fails.
await_upload_expiry(Conn, Key) ->
    {ok, _, [{true}]} = epgsql:equery(
        Conn,
        "SELECT session_state='confirmed' AND upload_content IS NOT NULL"
        " AND upload_expires_at > clock_timestamp()"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    {ok, _, [_]} = epgsql:equery(
        Conn,
        "SELECT pg_sleep(LEAST(5.0, GREATEST(0.0,"
        " EXTRACT(EPOCH FROM (upload_expires_at - clock_timestamp())))))"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    {ok, _, [{true}]} = epgsql:equery(
        Conn,
        "SELECT upload_expires_at <= clock_timestamp()"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    ok.
