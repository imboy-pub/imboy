%%% @doc EB-03 唯一 bounded retention purge worker 套件。
%%%
%%% 覆盖 EB-03-A06：
%%%   * 注入时钟（不是 DB 时钟）决定候选；
%%%   * 显式 Org/Workspace（每条语句都带两者，不跨租户删除）；
%%%   * batch limit 与 `SKIP LOCKED`（并发会话持锁时不阻塞、不误删）；
%%%   * 失败**多保留**（事务回滚，消息与附件都不动）；
%%%   * 附件不得早于其消息被删；active hold 优先于 retain_until。
%%%
%%% D1 的处置（EB-03R M1）：原「released hold 仍永久阻断 purge」的现象由迁移
%%% `00000121` 修掉——引用列改为 **active-only 派生列**（released 后为 NULL，MATCH
%%% SIMPLE 下不再参与引用校验），因此 released 行既不删也不挡；active hold 的阻断
%%% 仍由 DB 行锁 + RI 触发器保障。本套件的对应断言已按新语义重写。
-module(eb_retention_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

retention_purge_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    Previous = eb_pg_test_fixture:select_asset_stub(),
    {asset_stub, Previous, setup_db()}.

setup_db() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            case eb_pg_test_fixture:ensure_purge_role() of
                ok -> {ok, Conn};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

cleanup_db({asset_stub, Previous, Result}) ->
    try
        cleanup_db(Result)
    after
        eb_pg_test_fixture:restore_asset_store(Previous)
    end;
cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({asset_stub, _Previous, Result}) ->
    cases(Result);
cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a06_injected_clock_drives_candidate_selection/0},
        {timeout, 60, fun a06_missing_or_invalid_clock_fails_closed/0},
        {timeout, 60, fun a06_purge_deletes_children_then_message_and_audits/0},
        {timeout, 60, fun a06_batch_limit_bounds_each_run/0},
        {timeout, 60, fun a06_active_hold_blocks_purge_even_when_due/0},
        {timeout, 60, fun a06_db_guard_rejection_keeps_everything/0},
        {timeout, 60, fun a06_asset_retained_longer_than_message_blocks_purge/0},
        {timeout, 60, fun a06_purge_never_crosses_tenants/0},
        {timeout, 90, fun a06_skip_locked_does_not_block_on_other_sessions/0},
        {timeout, 60, fun a06_released_hold_no_longer_blocks_due_purge_after_m1/0},
        {timeout, 60, fun a06_purge_statements_are_tenant_scoped_and_use_skip_locked/0}
    ];
cases(_Skipped) ->
    {skip, "retention purge suite requires the scratch database connection"}.

%% ===================================================================
%% EB-03-A06：注入时钟
%% ===================================================================

a06_injected_clock_drives_candidate_selection() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        %% retain_until 已过（对 DB 时钟也到期）
        MsgId = insert_message(Scope, <<"eb03-clock-1">>, Now - 60),
        %% 注入「更早」的时钟：候选筛选必须为空（证明是注入时钟在决定，而不是 DB 时钟）
        {ok, Early} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now - 3600, batch_limit => 10}),
        ?assertEqual(0, maps:get(deleted, Early)),
        ?assertEqual(1, alive_message_count(Org, Ws, MsgId)),
        %% 注入当前时钟：到期 → 只清理目标
        {ok, OnTime} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual(1, maps:get(deleted, OnTime)),
        ?assertEqual([MsgId], maps:get(purged, OnTime)),
        ?assertEqual(0, alive_message_count(Org, Ws, MsgId)),
        %% 未到期（retain_until 在未来）：注入当前时钟仍为零
        FutureMsg = insert_message(Scope, <<"eb03-clock-2">>, Now + 86400),
        {ok, NotDue} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual(0, maps:get(deleted, NotDue)),
        ?assertEqual(1, alive_message_count(Org, Ws, FutureMsg))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a06_missing_or_invalid_clock_fails_closed() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        MsgId = insert_message(Scope, <<"eb03-clock-3">>, Now - 60),
        ?assertMatch({error, {missing_clock, _}}, eb_pg_purge:purge_batch(Org, Ws, #{})),
        ?assertMatch(
            {error, {invalid_clock, _}},
            eb_pg_purge:purge_batch(Org, Ws, #{now => <<"not-a-clock">>})
        ),
        ?assertMatch(
            {error, {invalid_tenant, _}},
            eb_pg_purge:purge_batch(undefined, Ws, #{now => Now})
        ),
        ?assertMatch(
            {error, {invalid_tenant, _}},
            eb_pg_purge:purge_batch(Org, undefined, #{now => Now})
        ),
        ?assertMatch(
            {error, {invalid_batch_limit, _}},
            eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 0})
        ),
        ?assertEqual(1, alive_message_count(Org, Ws, MsgId))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% EB-03-A06：正向清理（子表 + 审计）
%% ===================================================================

a06_purge_deletes_children_then_message_and_audits() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        RetainUntil = Now - 120,
        %% FND-4 正向证据：对象字节真实存在（synthetic 前缀），purge 后必须消失
        ObjectKey = scoped_object_key(Org, Ws, eb_pg_test_fixture:id()),
        ok = eb_asset_object_stub:put(ObjectKey, <<"fnd4-object-bytes">>, #{}),
        MsgId = insert_message(Scope, <<"eb03-purge-1">>, RetainUntil),
        AssetId = attach_asset_with_key(Scope, MsgId, RetainUntil, ObjectKey),
        DeliveryId = attach_delivery(Scope, MsgId),
        ?assertEqual(1, eb_pg_test_fixture:count(Org, Ws, assets)),
        ?assertEqual(1, eb_pg_test_fixture:count(Org, Ws, deliveries)),
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual(1, maps:get(deleted, Summary)),
        ?assertEqual([MsgId], maps:get(purged, Summary)),
        ?assert(is_integer(maps:get(audit_id, Summary))),
        %% FND-4：对象字节随 purge 消失（先对象后元数据顺序的正面证据）
        ?assertMatch({error, not_found}, object_get(Org, Ws, ObjectKey)),
        %% FND-4：无作用域 key 的对象不得被 purge 触碰（跨租户 fail-closed）
        ?assertMatch([], maps:get(object_delete_failures, Summary)),
        ?assertEqual(0, alive_message_count(Org, Ws, MsgId)),
        ?assertEqual(0, eb_pg_test_fixture:count(Org, Ws, assets)),
        ?assertEqual(0, eb_pg_test_fixture:count(Org, Ws, deliveries)),
        ?assertEqual(0, row_count(<<"enterprise_asset">>, AssetId)),
        ?assertEqual(0, row_count(<<"enterprise_message_delivery">>, DeliveryId)),
        %% 逐批 append-only 审计
        ?assertEqual(
            <<"message.purge">>,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT action FROM enterprise_audit_event"
                    " WHERE organization_id=$1 ORDER BY id DESC LIMIT 1"
                >>,
                [Org]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a06_batch_limit_bounds_each_run() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        MsgIds = [
            insert_message(Scope, <<"eb03-batch-", (integer_to_binary(N))/binary>>, Now - 60 - N)
         || N <- [1, 2, 3]
        ],
        {ok, First} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 2}),
        ?assertEqual(2, maps:get(deleted, First)),
        ?assertEqual(
            1,
            length([Id || Id <- MsgIds, alive_message_count(Org, Ws, Id) =:= 1])
        ),
        {ok, Second} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 2}),
        ?assertEqual(1, maps:get(deleted, Second)),
        ?assertEqual(0, length([Id || Id <- MsgIds, alive_message_count(Org, Ws, Id) =:= 1]))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% EB-03-A06：hold 优先 / 失败多保留
%% ===================================================================

a06_active_hold_blocks_purge_even_when_due() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        MsgId = insert_message(Scope, <<"eb03-hold-1">>, Now - 60),
        HoldId = insert_hold(Scope, #{scope_type => <<"message">>, scope_message_id => MsgId}),
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual(0, maps:get(deleted, Summary)),
        ?assertEqual([{MsgId, {ineligible, active_hold}}], maps:get(skipped, Summary)),
        ?assertEqual(undefined, maps:get(audit_id, Summary)),
        ?assertEqual(1, alive_message_count(Org, Ws, MsgId)),
        %% release 后（同一 hold 行已释放）active hold 不再阻断 domain 判定
        ok = eb_pg_store:release_hold(Org, Ws, HoldId, maps:get(actor_user_id, Scope)),
        ?assertEqual(
            0,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM enterprise_retention_hold"
                    " WHERE organization_id=$1 AND released_at IS NULL"
                >>,
                [Org]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% DB 守卫（retain_until 相对 DB 时钟未到期）在 purge worker 内被命中时，
%% 整个批次回滚：消息与附件都不删、不写审计（失败宁可多保留）。
a06_db_guard_rejection_keeps_everything() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        %% 对 DB 时钟未到期，但注入时钟已越过 → 候选被选中，DB 守卫拒绝
        RetainUntil = Now + 3600,
        MsgId = insert_message(Scope, <<"eb03-guard-1">>, RetainUntil),
        AssetId = attach_asset(Scope, MsgId, RetainUntil),
        AuditsBefore = eb_pg_test_fixture:count(Org, Ws, audits),
        Result = eb_pg_purge:purge_batch(Org, Ws, #{now => Now + 7200, batch_limit => 10}),
        ?assertMatch({error, {sql, _, _}}, Result),
        {error, {sql, Code, _Constraint}} = Result,
        ?assertEqual(<<"23514">>, Code),
        ?assertEqual(1, alive_message_count(Org, Ws, MsgId)),
        ?assertEqual(1, eb_pg_test_fixture:count(Org, Ws, assets)),
        ?assertEqual(1, alive_asset_count(Org, Ws, AssetId)),
        ?assertEqual(AuditsBefore, eb_pg_test_fixture:count(Org, Ws, audits))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% 附件保留期长于消息时，purge 必须连附件一起保留（附件不得早于其消息被删）。
a06_asset_retained_longer_than_message_blocks_purge() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        MsgId = insert_message(Scope, <<"eb03-asset-1">>, Now - 600),
        AssetId = attach_asset(Scope, MsgId, Now + 86400),
        Result = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertMatch({error, {sql, _, _}}, Result),
        {error, {sql, Code, Constraint}} = Result,
        ?assertEqual(<<"23514">>, Code),
        ?assertEqual(<<"trg_enterprise_asset_purge_guard">>, Constraint),
        ?assertEqual(1, alive_message_count(Org, Ws, MsgId)),
        ?assertEqual(1, alive_asset_count(Org, Ws, AssetId))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% EB-03-A06：不跨租户
%% ===================================================================

a06_purge_never_crosses_tenants() ->
    ScopeA = eb_pg_test_fixture:new_scope(),
    ScopeB = eb_pg_test_fixture:new_scope(),
    try
        {OrgA, WsA} = scope_tenant(ScopeA),
        {OrgB, WsB} = scope_tenant(ScopeB),
        Now = eb_system_clock:now(),
        MsgA = insert_message(ScopeA, <<"eb03-tenant-a">>, Now - 60),
        MsgB = insert_message(ScopeB, <<"eb03-tenant-b">>, Now - 60),
        %% 用 B 的 Workspace 配 A 的 Org（错配）→ 不得删任何行，也不得报错放行
        {ok, Mismatched} = eb_pg_purge:purge_batch(OrgA, WsB, #{now => Now, batch_limit => 10}),
        ?assertEqual(0, maps:get(deleted, Mismatched)),
        ?assertEqual(1, alive_message_count(OrgA, WsA, MsgA)),
        ?assertEqual(1, alive_message_count(OrgB, WsB, MsgB)),
        %% 正确的 A 租户只清理 A 的目标
        {ok, Scoped} = eb_pg_purge:purge_batch(OrgA, WsA, #{now => Now, batch_limit => 10}),
        ?assertEqual(1, maps:get(deleted, Scoped)),
        ?assertEqual([MsgA], maps:get(purged, Scoped)),
        ?assertEqual(0, alive_message_count(OrgA, WsA, MsgA)),
        ?assertEqual(1, alive_message_count(OrgB, WsB, MsgB))
    after
        eb_pg_test_fixture:cleanup(ScopeA),
        eb_pg_test_fixture:cleanup(ScopeB)
    end.

%% ===================================================================
%% EB-03-A06：SKIP LOCKED
%% ===================================================================

%% 另一个会话持有候选行的行锁时，purge 必须**立即**跳过（不阻塞、不误删）。
%% 若实现用的是裸 `FOR UPDATE`，这里会阻塞到对方提交（>1.2s）而被断言抓住。
a06_skip_locked_does_not_block_on_other_sessions() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        MsgIds = [
            insert_message(Scope, <<"eb03-lock-", (integer_to_binary(N))/binary>>, Now - 60)
         || N <- [1, 2]
        ],
        Parent = self(),
        _Holder = spawn_link(fun() ->
            _ = elib_pg:with_tx(
                fun(Conn) ->
                    {ok, _} = elib_pg:query(
                        Conn,
                        <<
                            "SELECT id FROM enterprise_message"
                            " WHERE organization_id=$1 AND workspace_id=$2 FOR UPDATE"
                        >>,
                        [Org, Ws]
                    ),
                    Parent ! {eb03_locked, self()},
                    timer:sleep(1500),
                    ok
                end,
                [{reraise, false}]
            ),
            Parent ! eb03_holder_done
        end),
        receive
            {eb03_locked, _Pid} -> ok
        after 10000 ->
            erlang:error(lock_holder_never_locked)
        end,
        Started = erlang:monotonic_time(millisecond),
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        Elapsed = erlang:monotonic_time(millisecond) - Started,
        ?assertEqual(0, maps:get(deleted, Summary)),
        ?assert(Elapsed < 1000),
        ?assertEqual(2, length([Id || Id <- MsgIds, alive_message_count(Org, Ws, Id) =:= 1])),
        receive
            eb03_holder_done -> ok
        after 10000 ->
            ok
        end,
        %% 持锁方结束后，同一批可正常清理
        {ok, After} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual(2, maps:get(deleted, After))
    after
        eb_pg_test_fixture:cleanup(Scope)
        %% ===================================================================
    end.
%% D1（EB-03R M1 修复后）：release 且到期 ⇒ purge 成功；历史行与原 id 保留
%% ===================================================================

a06_released_hold_no_longer_blocks_due_purge_after_m1() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = scope_tenant(Scope),
        Now = eb_system_clock:now(),
        MsgId = insert_message(Scope, <<"eb03-d1-1">>, Now - 60),
        HoldId = insert_hold(Scope, #{scope_type => <<"message">>, scope_message_id => MsgId}),
        %% active 时：domain 判 ineligible，DB 层也不许删（两处都不放行）
        {ok, Before} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual(0, maps:get(deleted, Before)),
        ?assertEqual(1, alive_message_count(Org, Ws, MsgId)),
        %% release 后：domain 判 eligible，且 **DB 不再阻断**（M1 的正面证据）
        ok = eb_pg_store:release_hold(Org, Ws, HoldId, maps:get(actor_user_id, Scope)),
        {ok, Msg} = eb_pg_store:fetch_message(Org, Ws, MsgId),
        {ok, Holds} = eb_pg_store:list_active_holds(Org, Ws),
        ?assertEqual(eligible, eb_retention:purge_eligible(Msg, Holds, Now)),
        {ok, After} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual(1, maps:get(deleted, After)),
        ?assertEqual([MsgId], maps:get(purged, After)),
        ?assertEqual(0, alive_message_count(Org, Ws, MsgId)),
        %% 历史事实可追：hold 行仍在、released_at 非空、原 resource id 逐字未变
        ?assertEqual(
            1,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM enterprise_retention_hold"
                    " WHERE organization_id=$1 AND id=$2 AND scope_message_id=$3"
                    "   AND released_at IS NOT NULL"
                >>,
                [Org, HoldId, MsgId]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 静态：SQL 形态
%% ===================================================================

a06_purge_statements_are_tenant_scoped_and_use_skip_locked() ->
    Statements = eb_pg_purge:sql_statements(),
    ?assert(length(Statements) >= 4),
    lists:foreach(
        fun(Sql) ->
            ?assert(binary:match(Sql, <<"$1">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"$2">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"organization_id">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"workspace_id">>) =/= nomatch)
        end,
        Statements
    ),
    ?assert(
        lists:any(
            fun(Sql) -> binary:match(Sql, <<"SKIP LOCKED">>) =/= nomatch end,
            Statements
        )
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

scope_tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

%% 直接走 store 的 append_message/3 造消息：可精确控制 retain_until，
%% 且与 canonical 事务共用同一套 sender 合同与租户贯穿。
insert_message(Scope, ClientMsgId, RetainUntil) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    Contact = maps:get(contact_id, Scope),
    MsgId = eb_pg_test_fixture:id(),
    KeyRef = eb_pg_test_fixture:key_ref(1),
    Aad = #{
        organization_id => Org,
        workspace_id => Ws,
        conversation_id => Conv,
        message_id => MsgId
    },
    {ok, Sealed} = eb_managed_crypto:seal(Aad, <<"eb03-retention-body">>, KeyRef),
    {ok, _Row} = eb_pg_store:append_message(Org, Ws, #{
        id => MsgId,
        conversation_id => Conv,
        client_msg_id => ClientMsgId,
        sender_type => <<"contact">>,
        sender_contact_id => Contact,
        sender_business_identity_id => null,
        actor_user_id => null,
        body_cipher => maps:get(cipher, Sealed),
        key_version => maps:get(key_version, Sealed),
        aad_hash => maps:get(aad_hash, Sealed),
        content_hash => sha256_hex(maps:get(cipher, Sealed)),
        policy_id => maps:get(policy_id, Scope),
        policy_version => 1,
        retention_days => 1095,
        retain_until => RetainUntil
    }),
    MsgId.

%% 附件与所属消息同 Org/Workspace；object_key 不含任何 URL（EB-01 约束）。
attach_asset(Scope, MsgId, RetainUntil) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    AssetId = eb_pg_test_fixture:id(),
    ok = eb_pg_test_fixture:exec(
        <<
            "INSERT INTO enterprise_asset"
            " (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,"
            "  uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,"
            "  retain_until,version)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,'application/octet-stream',64,'active',1,"
            "         to_timestamp($10::bigint/1000),1)"
        >>,
        [
            AssetId,
            Org,
            Ws,
            Conv,
            MsgId,
            maps:get(sales_identity_id, Scope),
            maps:get(actor_user_id, Scope),
            %% FND-4：object_key 必须带租户作用域前缀（与生产链路
            %% eb_asset_object_stub:key_prefix/2 同构）—— purge 的对象回收
            %% 对无作用域/跨租户 key fail-closed 拒删。
            <<"enterprise/", (integer_to_binary(Org))/binary, "/", (integer_to_binary(Ws))/binary,
                "/eb03-", (integer_to_binary(AssetId))/binary, ".bin">>,
            sha256_hex(<<"eb03-asset-hash">>),
            RetainUntil * 1000
        ]
    ),
    AssetId.

attach_delivery(Scope, MsgId) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    DeliveryId = eb_pg_test_fixture:id(),
    RecipientRef = <<"identity:", (integer_to_binary(maps:get(sales_identity_id, Scope)))/binary>>,
    ok = eb_pg_test_fixture:exec(
        <<
            "INSERT INTO enterprise_message_delivery"
            " (id,organization_id,workspace_id,message_id,recipient_ref,device_id,status,version)"
            " VALUES ($1,$2,$3,$4,$5,'eb03-device','pending',1)"
        >>,
        [DeliveryId, Org, Ws, MsgId, RecipientRef]
    ),
    DeliveryId.

insert_hold(Scope, Extra) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    HoldId = eb_pg_test_fixture:id(),
    Hold = maps:merge(
        #{
            id => HoldId,
            scope_type => <<"workspace">>,
            reason_code => <<"eb03-synthetic-hold">>,
            actor_user_id => maps:get(actor_user_id, Scope)
        },
        Extra
    ),
    {ok, _Row} = eb_pg_store:insert_hold(Org, Ws, Hold),
    HoldId.

alive_message_count(Org, Ws, MsgId) ->
    eb_pg_test_fixture:scalar(
        <<
            "SELECT count(*) AS n FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MsgId]
    ).

alive_asset_count(Org, Ws, AssetId) ->
    eb_pg_test_fixture:scalar(
        <<
            "SELECT count(*) AS n FROM enterprise_asset"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, AssetId]
    ).

%% 按主键回读任意企业表的行数（子表已删时需要精确到 id 的证据）。
row_count(TableName, Id) ->
    eb_pg_test_fixture:scalar(
        <<"SELECT count(*) AS n FROM ", TableName/binary, " WHERE id=$1">>,
        [Id]
    ).

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

%% FND-4 helpers：作用域内真实对象 + 直读断言
scoped_object_key(Org, Ws, Id) ->
    iolist_to_binary([
        "enterprise/",
        integer_to_binary(Org),
        "/",
        integer_to_binary(Ws),
        "/fnd4-",
        integer_to_binary(Id),
        ".bin"
    ]).

attach_asset_with_key(Scope, MsgId, RetainUntil, ObjectKey) ->
    {Org, Ws, Conv, Identity, Uploader} = asset_tuple(Scope),
    AssetId = eb_pg_test_fixture:id(),
    ok = eb_pg_test_fixture:exec(
        <<
            "INSERT INTO enterprise_asset"
            " (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,"
            "  uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,"
            "  retain_until,version)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,'application/octet-stream',64,'active',1,"
            "         to_timestamp($10::bigint/1000),1)"
        >>,
        [
            AssetId,
            Org,
            Ws,
            Conv,
            MsgId,
            Identity,
            Uploader,
            ObjectKey,
            sha256_hex(<<"fnd4-object-bytes">>),
            RetainUntil * 1000
        ]
    ),
    AssetId.

asset_tuple(Scope) ->
    {
        maps:get(org_id, Scope),
        maps:get(workspace_id, Scope),
        maps:get(conversation_id, Scope),
        maps:get(sales_identity_id, Scope),
        maps:get(actor_user_id, Scope)
    }.

object_get(Org, Ws, Key) ->
    eb_asset_object_stub:get(Key, eb_asset_object_stub:key_prefix(Org, Ws)).
