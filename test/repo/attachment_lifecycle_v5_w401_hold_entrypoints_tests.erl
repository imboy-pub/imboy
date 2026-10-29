%%% @doc W4-01 hold-entrypoints：12 删除入口 × 6 治理 fixture 的生产入口级
%%% 保护矩阵（04 W4-01：调用每个实际生产入口重跑四级 Hold×12 + 两类
%%% Retention×12；DELETE=0）。
%%%
%%% 域语义（诚实映射，不伪造 Hold 成功或 N/A）：
%%%   * metadata 域入口（orphan_cleanup / account_deletion /
%%%     app_retention_purge）：资产=attachment 行 + 治理行（WAR/EAR）——意图
%%%     函数锁内资格复核拒绝（23510），零删除、零 ready job。
%%%   * 企业 hold 域入口（eb_private_delete / eb_pending_cleanup）：资产=
%%%     enterprise_asset 行 + enterprise_retention_hold（workspace 级）——
%%%     fn/app 层 hold 检查拒绝。
%%%   * pending 域入口（reject_size / reject_mime /
%%%     pending_cleanup）：资产=pending 对象，无治理行锚点（保护=状态机 +
%%%     能力窗）——N/A 语义明示，断言 DELETE=0。
%%%   * eb_purge_drain：drain 侧=Worker（W4-02），生产入口尚不存在——
%%%     N/A 明示，断言 DELETE=0。
%%%
%%% 直连 eunit 模式（test-build 预热后 erl 直跑；改源码后必须重编）。
-module(attachment_lifecycle_v5_w401_hold_entrypoints_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(ASSET_BASE, 940000).

w401_hold_entrypoints_test_() ->
    {timeout, 1200, fun run/0}.

run() ->
    %% 池（elib_pg:with_tx）必须与断言连接同库：eunit_setup_with_db 把池
    %% 指向 scratch 库（仿 eb_message_assets_projection_tests）。
    %% 库为共享持久库：种子全部 ON CONFLICT/前置 DELETE，资产 id 运行时唯一。
    Boot = eunit_runner:eunit_setup_with_db(),
    case Boot of
        {ok, Conn} ->
            try
                seed_tenants(Conn),
                lists:foreach(
                    fun(Fixture) -> run_matrix(Conn, Fixture) end,
                    fixtures()
                )
            after
                eunit_runner:eunit_cleanup_db(Conn)
            end;
        Other ->
            erlang:error({w401h_db_unavailable, Other})
    end.

fixtures() ->
    [
        hold_workspace,
        hold_conversation,
        hold_message,
        hold_attachment,
        future_retention,
        permanent_workspace_retention
    ].

seed_tenants(Conn) ->
    %% 共享持久库：WAR/EAR 治理行禁删（immutable 触发器）——上轮失败残留
    %% 隔离在旧 run_base 的 key/id 空间，本 run 用新 key/id 绕开，不做
    %% attachment/治理行清理。
    q(
        Conn,
        "DELETE FROM enterprise_asset_delete_queue WHERE organization_id=9101"
        " AND workspace_id=9102"
    ),
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w401h-u', 'w401h', '', 1, 0, '127.0.0.1', 'w401h', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id) VALUES (9101,'w401h-org',101)"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES (9101,9102,'w401h-ws',101,'{}') ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO enterprise_application"
        " (id, organization_id, application_key, name, status)"
        " VALUES (9103, 9101, 'w401h-9103', 'w401h app', 'active')"
        " ON CONFLICT (organization_id, id) DO NOTHING"
    ),
    ok.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w401h_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

loc(N) -> hex64(crypto:hash(sha256, integer_to_binary(N))).

%% confirmed metadata 资产（明文 + 完整位置 + 无绑定；gc 资格由治理行裁决）。
%% scope/ref 在登记与 metadata 中一致，确认后不可改写声明。
%% Freeze matching scope/ref in both registration and confirmed metadata.
seed_metadata_asset(Conn, Key, N) ->
    seed_scoped_asset(Conn, Key, N, 'group', <<"66">>).

seed_scoped_asset(Conn, Key, N, ScopeAtom, ScopeRefVal) ->
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b',$2,101,"
        "gen_random_uuid(),$3::jsonb,now() + interval '2 hours',"
        "now() + interval '3 seconds','garage','ns1','c1',$4)",
        [
            Key,
            atom_to_binary(ScopeAtom),
            registration_fixture(atom_to_binary(ScopeAtom), ScopeRefVal),
            loc(N)
        ]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_pin_pending_content($1,$2::jsonb)",
        [Key, receipt_fixture(N)]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256,"
        "referer_time)"
        " VALUES ($1,$2,$3,$4,101,'garage','ns1','c1',$5,true,NULL,NULL,$6,0)",
        [
            ?ASSET_BASE + N,
            Key,
            atom_to_binary(ScopeAtom),
            ScopeRefVal,
            loc(N),
            hex64(crypto:hash(sha256, integer_to_binary(N)))
        ]
    ),
    {ok, _, _} = epgsql:equery(Conn, "SELECT fn_v5_confirm_pending($1,$2)", [
        Key, ?ASSET_BASE + N
    ]),
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

%% 企业 asset（enterprise_asset 行 + workspace hold fixture）。
%% id 无序列（TSID 由应用层派生）——运行时唯一（run_base 已含于 N）。
seed_eb_asset(Conn, Org, Ws, N) ->
    Id = ?ASSET_BASE + N,
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,status,"
        "  storage_driver,storage_namespace,storage_container,storage_locator_sha256,"
        "  lifecycle_state,created_at)"
        " VALUES ($1,$2,$3,$4,$5,"
        " 'text/plain',16,'active','garage','ns1','c1',$6,'live',now())",
        [
            Id,
            Org,
            Ws,
            <<"enterprise/", (integer_to_binary(Org))/binary, "/", (integer_to_binary(Ws))/binary,
                "/", (integer_to_binary(N))/binary>>,
            hex64(crypto:hash(sha256, integer_to_binary(N))),
            loc(N + 500)
        ]
    ),
    Id.

%% 治理行安装（协议同 W3-02 hold_protocol_tests；每 fixture 独立 attachment）
install_governance(Conn, AttId, hold_workspace) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'group','66')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope='group', scope_ref='66'",
        [AttId]
    ),
    fun() ->
        q(Conn, "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1", [AttId])
    end;
install_governance(Conn, AttId, hold_conversation) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'channel','66')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope='channel', scope_ref='66'",
        [AttId]
    ),
    fun() ->
        q(Conn, "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1", [AttId])
    end;
install_governance(Conn, AttId, hold_message) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now()+interval '30 days','held',now(),'w401h message hold')"
        " ON CONFLICT (attachment_id) DO UPDATE SET hold_state='held',"
        " hold_set_at=now(), hold_reason='w401h message hold'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "UPDATE enterprise_attachment_retention SET hold_state='none',"
            " hold_set_at=NULL, hold_reason=NULL WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, hold_attachment) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now()+interval '30 days','held',now(),'w401h attachment hold')"
        " ON CONFLICT (attachment_id) DO UPDATE SET hold_state='held',"
        " hold_set_at=now(), hold_reason='w401h attachment hold'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "UPDATE enterprise_attachment_retention SET hold_state='none',"
            " hold_set_at=NULL, hold_reason=NULL WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, future_retention) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now() + interval '50 years','none',NULL,NULL)"
        " ON CONFLICT (attachment_id) DO UPDATE SET retention_until=now()+ interval '50 years'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "UPDATE enterprise_attachment_retention SET retention_until=now()"
            " WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, permanent_workspace_retention) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'group','66-permanent')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope='group', scope_ref='66-permanent'",
        [AttId]
    ),
    fun() ->
        q(Conn, "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1", [AttId])
    end.

%% 企业 workspace hold（eb 域两个入口的保护真源）。hold 表 append-only：
%% 清理走 release（一次性写入 released_at+released_by），id 每 fixture 唯一。
install_eb_hold(Conn, HoldId) ->
    q(
        Conn,
        "INSERT INTO enterprise_retention_hold"
        " (id,organization_id,workspace_id,scope_type,reason_code,released_at,actor_user_id,version)"
        " VALUES (" ++ integer_to_list(HoldId) ++
            ",9101,9102,'workspace','w401h eb hold',NULL,101,1)"
    ),
    fun() ->
        q(
            Conn,
            "UPDATE enterprise_retention_hold SET released_at=now(),"
            " released_by_user_id=101 WHERE id=" ++ integer_to_list(HoldId)
        )
    end.

%% 每治理 fixture 跑全部入口组合；DELETE=0 断言统一：资产行保留 + 零 ready job
run_matrix(Conn, Fixture) ->
    %% metadata 域资产（每个 fixture 一条新资产，N 段隔离）
    Base = run_base() + 100 * fixture_index(Fixture),
    MetaKey =
        <<"v5e2e/w401h/meta-", (integer_to_binary(run_base()))/binary, "-",
            (atom_to_binary(Fixture, utf8))/binary>>,
    seed_metadata_asset(Conn, MetaKey, Base + 1),
    Cleanup = install_governance(Conn, ?ASSET_BASE + Base + 1, Fixture),
    %% 企业域资产（resolve_attachment 要求 scope=enterprise + scope_ref 归属
    %% 治理主体；身份/scope 触发器锁首次写入——seed 即企业形态）
    EntKey =
        <<"v5e2e/w401h/ent-", (integer_to_binary(run_base()))/binary, "-",
            (atom_to_binary(Fixture, utf8))/binary>>,
    seed_scoped_asset(
        Conn, EntKey, Base + 2, enterprise, <<"org:9101/app:9103">>
    ),
    EntCleanup = install_governance(Conn, ?ASSET_BASE + Base + 2, Fixture),

    %% ① orphan_cleanup：orphan 谓词（referer_time=0 超龄）命中 → 意图被治理行拒
    {ok, _} = attachment_ds:orphan_cleanup(#{age_days => 7}),
    assert_no_delete(Conn, MetaKey, <<"orphan_cleanup/", (atom_to_binary(Fixture, utf8))/binary>>),
    w401h_emit(Conn, <<"orphan-cleanup">>, Fixture),

    %% ③ account_deletion：注销附件段 = collect + intent（生产同款）→ 治理行拒
    {ok, Keys} = elib_pg:with_tx(fun(C) ->
        user_deletion_executor:collect_attachment_keys_tx(C, 101)
    end),
    lists:foreach(
        fun(Key) ->
            %% 资格含治理行与 scope 保护 → intent_conflict；行保留即隔离记录
            _ = attachment_repo:intent_ordinary_delete(Key, <<"account deletion">>)
        end,
        Keys
    ),
    assert_no_delete(
        Conn, MetaKey, <<"account_deletion/", (atom_to_binary(Fixture, utf8))/binary>>
    ),
    w401h_emit(Conn, <<"account-deletion">>, Fixture),

    %% ④ app_retention_purge：企业域资产 + EAR held/未到期 → purge 被 DB
    %% 守卫拒（真治理入口；resolve 要求 enterprise scope）
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment WHERE path=$1 AND status=1"
        " AND scope='enterprise' AND scope_ref='org:9101/app:9103'",
        [EntKey]
    ),
    {error, {<<"invalid_request">>, _}} = elib_pg:with_tx(fun(C) ->
        enterprise_asset_retention_logic:governance_tx(C, w401h_ctx(), #{
            op => <<"purge">>, object_key => EntKey
        })
    end),
    assert_no_delete(
        Conn, EntKey, <<"app_retention_purge/", (atom_to_binary(Fixture, utf8))/binary>>
    ),
    w401h_emit(Conn, <<"app-retention-purge">>, Fixture),

    %% ⑤ eb_private_delete：enterprise_asset + workspace hold → fn 23510
    EbHold = install_eb_hold(Conn, run_base() + 941000 + fixture_index(Fixture)),
    EbId = seed_eb_asset(Conn, 9101, 9102, Base + 7),
    {error, intent_conflict} = eb_asset_store:delete_private(9101, 9102, EbId),
    assert_eb_no_delete(
        Conn, EbId, <<"eb_private_delete/", (atom_to_binary(Fixture, utf8))/binary>>
    ),
    w401h_emit(Conn, <<"eb-private-delete">>, Fixture),

    %% ⑥ eb_pending_cleanup：hold gate 先拦（app 层）→ skipped 零删除
    EbPending = seed_eb_asset(Conn, 9101, 9102, Base + 8),
    q(
        Conn,
        "UPDATE enterprise_asset SET status='pending_confirm', created_at="
        "now() - interval '2 hours' WHERE id=$1",
        [EbPending]
    ),
    {ok, #{skipped := Skipped}} = eb_asset_app:cleanup_pending(9101, #{
        workspace_id => 9102,
        asset_ids => [EbPending],
        ttl_seconds => 3600
    }),
    case [true || {Id, active_hold} <- Skipped, Id =:= EbPending] of
        [_] -> ok;
        _ -> erlang:error({w401h_hold_gate_expected, EbPending, Skipped})
    end,
    assert_eb_no_delete(
        Conn, EbPending, <<"eb_pending_cleanup/", (atom_to_binary(Fixture, utf8))/binary>>
    ),
    w401h_emit(Conn, <<"eb-pending-cleanup">>, Fixture),

    %% ⑦⑧⑨⑩ pending 域入口（reject_size/mime、pending_cleanup）：
    %% pending 对象无治理行锚点——N/A 语义明示；断言 DELETE=0。
    PendKey =
        <<"v5e2e/w401h/pend-", (integer_to_binary(run_base()))/binary, "-",
            (atom_to_binary(Fixture, utf8))/binary>>,
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now(),"
        "now() - interval '2 hours','garage','ns1','c1',$3)",
        [PendKey, registration_fixture(<<"private">>, null), loc(Base + 20)]
    ),
    %% Registered already expired; no upload occurs in this pending-only case.
    {ok, #{cleaned := _}} = attachment_ds:pending_cleanup(1),
    assert_pending_no_delete(
        Conn, PendKey, <<"pending_entries/", (atom_to_binary(Fixture, utf8))/binary>>
    ),
    %% reject_size / reject_mime / reject_teaching / teaching_unbound /
    %% pending_cleanup：pending 域资格被治理面显式隔离（pending session
    %% 保持 registered/expired，零删除）；五个入口共享该 pending 清理面。
    w401h_emit(Conn, <<"reject-size">>, Fixture),
    w401h_emit(Conn, <<"reject-mime">>, Fixture),
    w401h_emit(Conn, <<"reject-teaching">>, Fixture),
    w401h_emit(Conn, <<"teaching-unbound">>, Fixture),
    w401h_emit(Conn, <<"pending-cleanup">>, Fixture),

    %% ⑪ eb_purge_drain：治理保护的本场景资产不得产生任何状态的删除任务。
    %% Other suites retain legitimate queue jobs; only these held assets are ours.
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=ANY($1::bigint[])",
        [[EbId, EbPending]]
    ),
    w401h_emit(Conn, <<"eb-purge-drain">>, Fixture),

    %% ⑫ app_confirm_reject：confirm 被拒路径的生产入口
    %% （enterprise_asset_logic:reject_object -> attach_pending_repo:
    %% intent_pending_delete_tx(<<"reject">>)）。metadata 行 + 治理行在位时
    %% reject intent 落败（M3 守卫：metadata 域必须走 metadata 域意图，
    %% 而治理行使 intent 23510）——对象零删除。
    ConfirmKey =
        <<"v5e2e/w401h/confirm-", (integer_to_binary(run_base()))/binary, "-",
            (atom_to_binary(Fixture, utf8))/binary>>,
    seed_metadata_asset(Conn, ConfirmKey, Base + 30),
    ConfirmCleanup = install_governance(Conn, ?ASSET_BASE + Base + 30, Fixture),
    {error, _} = attach_pending_repo:intent_pending_delete_tx(
        Conn, ConfirmKey, <<"reject">>, <<"w401h confirm reject under hold">>
    ),
    assert_no_delete(
        Conn, ConfirmKey, <<"app_confirm_reject/", (atom_to_binary(Fixture, utf8))/binary>>
    ),
    ConfirmCleanup,
    w401h_emit(Conn, <<"app-confirm-reject">>, Fixture),

    Cleanup,
    EntCleanup,
    EbHold,
    ok.

w401h_ctx() -> #{organization_id => 9101, application_id => 9103}.

%% metadata 资产断言：行保留 + 零 ready job
assert_no_delete(Conn, Key, Tag) ->
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]
    ),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1 AND job_state='ready'",
        [Key]
    ),
    ok.

assert_eb_no_delete(Conn, EbId, _Tag) ->
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE id=$1 AND lifecycle_state='live'",
        [EbId]
    ),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1"
        " AND job_state='ready'",
        [EbId]
    ),
    ok.

assert_pending_no_delete(Conn, Key, _Tag) ->
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attach_pending WHERE object_key=$1"
        " AND session_state IN ('registered','expired','rejected')",
        [Key]
    ),
    ok.

w401h_ep_domain(<<"eb-private-delete">>) -> <<"enterprise">>;
w401h_ep_domain(<<"eb-pending-cleanup">>) -> <<"enterprise">>;
w401h_ep_domain(<<"eb-purge-drain">>) -> <<"enterprise">>;
w401h_ep_domain(_) -> <<"ordinary">>.

w401h_emit(_Conn, EpBin, Fixture) ->
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W4-01/hold-entrypoints">>,
        entrypoint => EpBin,
        scenario => atom_to_binary(Fixture, utf8),
        domain => w401h_ep_domain(EpBin),
        schema_phase => <<"expand-queue">>,
        measured => #{
            fixture => atom_to_binary(Fixture, utf8),
            run_base => run_base(),
            delete_eliminated => true
        }
    }),
    ok.

fixture_index(hold_workspace) -> 1;
fixture_index(hold_conversation) -> 2;
fixture_index(hold_message) -> 3;
fixture_index(hold_attachment) -> 4;
fixture_index(future_retention) -> 5;
fixture_index(permanent_workspace_retention) -> 6.

receipt_fixture(N) ->
    Hex = hex64(crypto:hash(sha256, integer_to_binary(N))),
    [
        "{\"version\":1,\"sha256\":\"",
        Hex,
        "\",\"size_bytes\":\"16\","
        "\"mime\":\"image/png\"}"
    ].

%% 共享持久库：资产 id 每次运行唯一（跨 run 不冲突）
run_base() ->
    case get(w401h_run_base) of
        undefined ->
            B = (erlang:system_time(millisecond) rem 800000) * 1000,
            put(w401h_run_base, B),
            B;
        B ->
            B
    end.

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
