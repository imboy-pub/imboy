%%% @doc EB-03R T3 + M1 套件：bounded purge 用例级 Port 与 D1 语义修复。
%%%
%%% 覆盖：
%%%   * T3 信封：`(OrgId, WorkspaceId, NowMs, Limit)` → `{ok, #{deleted := N}}`，
%%%     注入时钟是唯一准入闸门，Limit 用绑定参数；
%%%   * **A06(a)** active hold 下 purge 被拒（DB 守卫 + 引用完整性）；
%%%   * **A06(b)** 并发竞争：purge 与 `insert_hold` 同时进行时被 hold 的 resource
%%%     不得被删（防「检查后插入」窗口——DB 行锁 + RI 最新快照复核）；
%%%   * **A06(c)** release 且到期后 purge **成功**（M1：released hold 不再永久阻断）；
%%%   * **A06(d)** 历史 hold 行与原 `scope_message_id` 仍在（审计可追）。
%%%
%%% 边界声明：本套件只证明**本地数据模型语义**，不构成真实 Legal Hold / 法规 /
%%% 生产合规结论。
-module(eb_purge_port_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

purge_port_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            case eb_pg_test_fixture:ensure_purge_role() of
                ok -> {ok, Conn};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun t3_envelope_purges_only_due_rows/0},
        {timeout, 60, fun t3_envelope_is_fail_closed/0},
        {timeout, 60, fun a06a_active_hold_blocks_purge_at_db_level/0},
        {timeout, 120, fun a06b_concurrent_hold_insert_wins_over_purge/0},
        {timeout, 60, fun a06c_released_hold_no_longer_blocks_due_purge/0},
        {timeout, 60, fun a06d_released_hold_keeps_history_and_original_resource_id/0},
        {timeout, 60, fun t3_does_not_cross_tenants/0},
        {timeout, 60, fun a18_transitional_opts_envelope_is_gone/0}
    ];
cases(_Skipped) ->
    {skip, "purge port suite requires the scratch database connection"}.

t3_envelope_purges_only_due_rows() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        Due = insert_message(Scope, <<"eb03r-p1">>, Now - 60),
        Future = insert_message(Scope, <<"eb03r-p2">>, Now + 86400),
        {ok, Summary} = eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 10),
        ?assertEqual(1, maps:get(deleted, Summary)),
        ?assertEqual([Due], maps:get(purged, Summary)),
        ?assertEqual(0, alive(Org, Ws, Due)),
        ?assertEqual(1, alive(Org, Ws, Future))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

t3_envelope_is_fail_closed() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        Msg = insert_message(Scope, <<"eb03r-p3">>, Now - 60),
        %% 注入时钟仍是唯一闸门：更早的时钟 ⇒ 一行不删
        {ok, Early} = eb_pg_purge_port:purge_batch(Org, Ws, (Now - 3600) * 1000, 10),
        ?assertEqual(0, maps:get(deleted, Early)),
        ?assertEqual(1, alive(Org, Ws, Msg)),
        %% 非法 Limit / 非法租户 / 非法时钟
        ?assertMatch(
            {error, {invalid_batch_limit, _}}, eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 0)
        ),
        ?assertMatch(
            {error, {invalid_batch_limit, _}},
            eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 100000)
        ),
        ?assertMatch(
            {error, {invalid_tenant, _}},
            eb_pg_purge_port:purge_batch(undefined, Ws, Now * 1000, 10)
        ),
        ?assertMatch(
            {error, {invalid_clock, _}}, eb_pg_purge_port:purge_batch(Org, Ws, <<"now">>, 10)
        ),
        ?assertEqual(1, alive(Org, Ws, Msg))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% A06(a)：active hold 覆盖时，**DB 层**拒绝物理删除（不是靠应用层先查后删）。
a06a_active_hold_blocks_purge_at_db_level() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        Msg = insert_message(Scope, <<"eb03r-p4">>, Now - 60),
        _HoldId = insert_hold(Scope, Msg),
        %% DB 直删（带 purge 上下文）必须被守卫/引用完整性拒绝
        Result = purge_delete_sql(Org, Ws, Msg),
        ?assertMatch({error, _}, Result),
        %% 端口层：候选被 domain 裁决为 ineligible 或整批回滚，一行不删
        {ok, Summary} = eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 10),
        ?assertEqual(0, maps:get(deleted, Summary)),
        ?assertEqual(1, alive(Org, Ws, Msg))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% A06(b)：并发竞争。持锁方在**未提交**的事务里插入 active hold，purge 同时在跑；
%% 被 hold 的 resource 不得被删。若实现只靠应用层「先查后删」，这里会删掉它。
a06b_concurrent_hold_insert_wins_over_purge() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        Msg = insert_message(Scope, <<"eb03r-p5">>, Now - 60),
        HoldId = eb_pg_test_fixture:id(),
        Parent = self(),
        Holder = spawn_link(fun() ->
            _ = elib_pg:with_tx(
                fun(Conn) ->
                    {ok, _} = elib_pg:execute(
                        Conn,
                        <<
                            "INSERT INTO enterprise_retention_hold"
                            " (id,organization_id,workspace_id,scope_type,scope_message_id,"
                            "  reason_code,actor_user_id,version)"
                            " VALUES ($1,$2,$3,'message',$4,'eb03r-race',$5,1)"
                        >>,
                        [HoldId, Org, Ws, Msg, maps:get(actor_user_id, Scope)]
                    ),
                    Parent ! {hold_inserted, self()},
                    timer:sleep(1200),
                    ok
                end,
                [{reraise, false}]
            ),
            Parent ! holder_done
        end),
        receive
            {hold_inserted, _Pid} -> ok
        after 10000 ->
            erlang:error(hold_holder_never_inserted)
        end,
        Started = erlang:monotonic_time(millisecond),
        Summary = eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 10),
        Elapsed = erlang:monotonic_time(millisecond) - Started,
        ?assertMatch({ok, _}, Summary),
        {ok, PurgeSummary} = Summary,
        %% 被 hold 的行一行都不许删（SKIP LOCKED 跳过 / DB 守卫拒绝，两条路都多保留）
        ?assertEqual(0, maps:get(deleted, PurgeSummary)),
        ?assertEqual(1, alive(Org, Ws, Msg)),
        ?assert(Elapsed < 2000),
        receive
            holder_done -> ok
        after 10000 -> ok
        end,
        %% 持锁方提交后：hold 生效中，第二次 purge 仍不得删
        ?assertEqual(
            1,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM enterprise_retention_hold"
                    " WHERE organization_id=$1 AND id=$2 AND released_at IS NULL"
                >>,
                [Org, HoldId]
            )
        ),
        {ok, Second} = eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 10),
        ?assertEqual(0, maps:get(deleted, Second)),
        ?assertEqual(1, alive(Org, Ws, Msg)),
        _ = Holder,
        ok
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% A06(c)：release 且到期后 purge **成功**——M1 的正面证据（D1 已修复）。
a06c_released_hold_no_longer_blocks_due_purge() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        Msg = insert_message(Scope, <<"eb03r-p6">>, Now - 60),
        HoldId = insert_hold(Scope, Msg),
        {ok, Before} = eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 10),
        ?assertEqual(0, maps:get(deleted, Before)),
        %% release（append-only 事实的唯一可变动作）
        ok = eb_pg_store:release_hold(Org, Ws, HoldId, maps:get(actor_user_id, Scope)),
        {ok, After} = eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 10),
        ?assertEqual(1, maps:get(deleted, After)),
        ?assertEqual([Msg], maps:get(purged, After)),
        ?assertEqual(0, alive(Org, Ws, Msg))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% A06(d)：历史 hold 行与原 resource id 仍在（审计可追；M1-b/M1-c）。
a06d_released_hold_keeps_history_and_original_resource_id() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        Msg = insert_message(Scope, <<"eb03r-p7">>, Now - 60),
        HoldId = insert_hold(Scope, Msg),
        ok = eb_pg_store:release_hold(Org, Ws, HoldId, maps:get(actor_user_id, Scope)),
        {ok, _} = eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 10),
        ?assertEqual(0, alive(Org, Ws, Msg)),
        %% 行还在、原 resource id 逐字未变、release 事实可追
        ?assertEqual(
            1,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM enterprise_retention_hold"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
                    "   AND scope_message_id=$4 AND released_at IS NOT NULL"
                >>,
                [Org, Ws, HoldId, Msg]
            )
        ),
        {ok, Row} = eb_pg_store:fetch_hold(Org, Ws, HoldId),
        ?assertEqual(Msg, maps:get(scope_message_id, Row)),
        ?assertNotEqual(undefined, maps:get(released_at, Row))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

t3_does_not_cross_tenants() ->
    ScopeA = eb_pg_test_fixture:new_scope(),
    ScopeB = eb_pg_test_fixture:new_scope(),
    try
        {OrgA, WsA} = tenant(ScopeA),
        {OrgB, WsB} = tenant(ScopeB),
        Now = eb_system_clock:now(),
        MsgA = insert_message(ScopeA, <<"eb03r-p8">>, Now - 60),
        MsgB = insert_message(ScopeB, <<"eb03r-p9">>, Now - 60),
        {ok, Mismatched} = eb_pg_purge_port:purge_batch(OrgA, WsB, Now * 1000, 10),
        ?assertEqual(0, maps:get(deleted, Mismatched)),
        ?assertEqual(1, alive(OrgA, WsA, MsgA)),
        ?assertEqual(1, alive(OrgB, WsB, MsgB)),
        {ok, Scoped} = eb_pg_purge_port:purge_batch(OrgA, WsA, Now * 1000, 10),
        ?assertEqual(1, maps:get(deleted, Scoped)),
        ?assertEqual(1, alive(OrgB, WsB, MsgB))
    after
        eb_pg_test_fixture:cleanup(ScopeA),
        eb_pg_test_fixture:cleanup(ScopeB)
    end.

%% EB-06-A18：过渡信封 `/3`（`Opts :: #{now, batch_limit}`）**已删除**，只保留 `/4`
%% 窄形状。判定三处同时成立：契约声明 / registry / 实现导出。
%%
%% 负例（load-bearing 证明）：`/3` 一旦被恢复（契约、registry、实现任一），
%% 下面的 `?assertNot(...)` 立刻失败 —— 该用例不是恒真断言。
a18_transitional_opts_envelope_is_gone() ->
    %% `function_exported/3` 对未加载模块恒为 false ⇒ 先确保加载，否则断言恒真。
    ?assertMatch({module, eb_pg_purge_port}, code:ensure_loaded(eb_pg_purge_port)),
    ?assertMatch({module, eb_purge_port}, code:ensure_loaded(eb_purge_port)),
    ?assertNot(erlang:function_exported(eb_pg_purge_port, purge_batch, 3)),
    ?assert(erlang:function_exported(eb_pg_purge_port, purge_batch, 4)),
    ?assertEqual(
        [{purge_batch, 4}],
        lists:sort([{N, A} || {N, A} <- eb_pg_purge_port:module_info(exports), N =/= module_info])
    ),
    ?assertEqual([{purge_batch, 4}], lists:sort(eb_purge_port:behaviour_info(callbacks))),
    ?assertEqual([{purge_batch, 4}], lists:sort(maps:get(eb_purge_port, eb_ports:contracts()))),
    ?assertNot(lists:member({purge_batch, 3}, maps:get(eb_purge_port, eb_ports:contracts()))),
    ?assertNot(lists:member({purge_batch, 3}, eb_purge_port:behaviour_info(callbacks))),
    %% `/4` 仍是窄信封：越界 Limit / 非整数 NowMs / 错租户一律 fail-closed。
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        ?assertMatch(
            {error, {invalid_batch_limit, _}},
            eb_pg_purge_port:purge_batch(Org, Ws, Now * 1000, 0)
        ),
        ?assertMatch(
            {error, {invalid_tenant, _}}, eb_pg_purge_port:purge_batch(undefined, Ws, 0, 1)
        ),
        ?assertMatch(
            {error, {invalid_clock, _}}, eb_pg_purge_port:purge_batch(Org, Ws, <<"now">>, 10)
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

%% 直接走 store 的 append_message/3 造消息（与 canonical 事务共用同一套合同）。
insert_message(Scope, ClientMsgId, RetainUntil) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    MsgId = eb_pg_test_fixture:id(),
    Aad = #{
        organization_id => Org,
        workspace_id => Ws,
        conversation_id => Conv,
        message_id => MsgId
    },
    {ok, Sealed} = eb_managed_crypto:seal(
        Aad, <<"eb03r-purge-body">>, eb_pg_test_fixture:key_ref(1)
    ),
    {ok, _Row} = eb_pg_store:append_message(Org, Ws, #{
        id => MsgId,
        conversation_id => Conv,
        client_msg_id => ClientMsgId,
        sender_type => <<"contact">>,
        sender_contact_id => maps:get(contact_id, Scope),
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

insert_hold(Scope, MsgId) ->
    HoldId = eb_pg_test_fixture:id(),
    {ok, _Row} = eb_pg_store:insert_hold(
        maps:get(org_id, Scope),
        maps:get(workspace_id, Scope),
        #{
            id => HoldId,
            scope_type => <<"message">>,
            scope_message_id => MsgId,
            reason_code => <<"eb03r-synthetic-hold">>,
            actor_user_id => maps:get(actor_user_id, Scope)
        }
    ),
    HoldId.

%% DB 层直删（带 bounded purge 上下文）：必须被守卫或引用完整性拒绝。
purge_delete_sql(Org, Ws, MsgId) ->
    elib_pg:with_tx(
        fun(Conn) ->
            _ = epgsql:squery(Conn, <<"SET LOCAL imboy.enterprise_purge = 'on'">>),
            case
                elib_pg:execute(
                    Conn,
                    <<"DELETE FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
                    [Org, Ws, MsgId]
                )
            of
                {ok, _} -> {ok, deleted};
                {error, Reason} -> {error, Reason}
            end
        end,
        [{reraise, false}]
    ).

alive(Org, Ws, MsgId) ->
    eb_pg_test_fixture:scalar(
        <<
            "SELECT count(*) AS n FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MsgId]
    ).

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).
