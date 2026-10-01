%%% @doc REVIEW-3 F-1 套件：bounded purge 的孤儿 pending 资产清理路径（真 PG）。
%%%
%%% 覆盖：
%%%   * 超龄孤儿 pending 资产（status='pending_confirm' + message_id IS NULL +
%%%     created_at 早于 age 线）随批清理：元数据行与对象字节都消失；
%%%   * 新鲜孤儿（未到 age 线）与已 confirm 绑定消息的资产**不动**；
%%%   * 绑定消息但未 confirm 的 pending 资产（message_id 非空）不进孤儿候选
%%%     （由其消息的保留期治理，age 不扩大到它们）；
%%%   * retain_until 已固化未到期的超龄孤儿不清理（与 DB 守卫同判据，防整批回滚）；
%%%   * active hold 覆盖的超龄孤儿被跳过（workspace scope）；
%%%   * `imboy.enterprise_purge` 开关门：无 GUC / GUC='off' 的直删被
%%%     trg_enterprise_asset_purge_guard 拒绝（23514），行不动；worker 路径
%%%     只能经 GUC=on 的同一事务触达删除；
%%%   * age 配置化：Opts 显式值生效、非法值 fail-closed、
%%%     `imboy` app env `eb_purge_orphan_asset_age_seconds` 生效；
%%%   * 跨租户错配对孤儿资产同样删 0 行；
%%%   * 混合批（到期消息 + 超龄孤儿）审计仍为 message.purge 且带孤儿计数。
%%%
%%% 数据全部为合成 fixture（随机 TSID / eb03- 前缀），零 PII。
-module(eb_purge_orphan_asset_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

orphan_purge_test_() ->
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
        {timeout, 60, fun f1_aged_orphan_purged_fresh_and_confirmed_kept/0},
        {timeout, 60, fun f1_purge_switch_off_refuses_orphan_delete/0},
        {timeout, 60, fun f1_orphan_retain_until_in_future_is_kept/0},
        {timeout, 60, fun f1_active_hold_keeps_aged_orphan/0},
        {timeout, 60, fun f1_bound_pending_asset_not_orphan_purged/0},
        {timeout, 60, fun f1_invalid_age_fails_closed/0},
        {timeout, 60, fun f1_env_age_config_is_honored/0},
        {timeout, 60, fun f1_orphans_never_cross_tenants/0},
        {timeout, 60, fun f1_mixed_batch_audit_keeps_message_purge_action/0},
        {timeout, 60, fun f1_orphan_statements_are_tenant_scoped/0}
    ];
cases(_Skipped) ->
    {skip, "orphan purge suite requires the scratch database connection"}.

%% ===================================================================
%% F-1 核心三态：超龄孤儿清、新鲜孤儿留、已 confirm 绑定消息的资产留
%% ===================================================================

f1_aged_orphan_purged_fresh_and_confirmed_kept() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        %% 1) 超龄孤儿 pending（2 天前，无 message_id，无 retain_until）+ 真实对象字节
        {AgedId, AgedKey} = insert_orphan_asset(Scope, #{age_seconds => 2 * 86400}),
        %% 2) 新鲜孤儿 pending（未到 age 线）
        {FreshId, FreshKey} = insert_orphan_asset(Scope, #{age_seconds => 0}),
        %% 3) 已 confirm 且绑定消息的资产（消息未到期 ⇒ 消息路径也不得触碰）
        MsgId = insert_message(Scope, <<"eb03-f1-confirmed-msg">>, Now + 86400),
        {ConfirmedId, ConfirmedKey} = insert_orphan_asset(
            Scope, bound_asset_spec(MsgId, Now)
        ),
        PurgeOpts = #{now => Now, batch_limit => 10, orphan_asset_age_seconds => 86400},
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, PurgeOpts),
        %% 只有超龄孤儿进批
        ?assertEqual(0, maps:get(deleted, Summary)),
        ?assertEqual([AgedId], maps:get(orphan_assets_purged, Summary)),
        ?assertEqual(1, maps:get(orphan_assets_deleted, Summary)),
        ?assertEqual([], maps:get(skipped, Summary)),
        ?assertEqual([], maps:get(object_delete_failures, Summary)),
        ?assert(is_integer(maps:get(audit_id, Summary))),
        %% 超龄孤儿：元数据行与对象字节都消失（对象先删、元数据后删的正面证据）
        ?assertEqual(0, alive_asset_count(Org, Ws, AgedId)),
        ?assertMatch({error, not_found}, object_get(Org, Ws, AgedKey)),
        %% 新鲜孤儿：行与对象原样保留
        ?assertEqual(1, alive_asset_count(Org, Ws, FreshId)),
        ?assertMatch({ok, _}, object_get(Org, Ws, FreshKey)),
        %% 已 confirm 资产：行、对象、所属消息全部保留
        ?assertEqual(1, alive_asset_count(Org, Ws, ConfirmedId)),
        ?assertMatch({ok, _}, object_get(Org, Ws, ConfirmedKey)),
        ?assertEqual(1, alive_message_count(Org, Ws, MsgId)),
        %% 孤儿-only 批的审计动作：asset.purge
        ?assertEqual(
            <<"asset.purge">>,
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

%% ===================================================================
%% F-1 开关门：GUC 缺失 / 'off' 时删除被 DB 守卫拒绝（勿绕过的正确设计）
%% ===================================================================

f1_purge_switch_off_refuses_orphan_delete() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        {AgedId, _Key} = insert_orphan_asset(Scope, #{age_seconds => 2 * 86400}),
        %% 无 GUC：守卫第一步拒绝（23514）
        ?assertMatch(
            {error, {<<"23514">>, <<"trg_enterprise_asset_purge_guard">>}},
            direct_delete_asset(Org, Ws, AgedId, unset)
        ),
        %% GUC 显式 'off'：同样拒绝
        ?assertMatch(
            {error, {<<"23514">>, <<"trg_enterprise_asset_purge_guard">>}},
            direct_delete_asset(Org, Ws, AgedId, off)
        ),
        ?assertEqual(1, alive_asset_count(Org, Ws, AgedId)),
        %% worker 路径（内部设 GUC=on）才能清理：正向对照
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, #{
            now => Now, batch_limit => 10, orphan_asset_age_seconds => 86400
        }),
        ?assertEqual([AgedId], maps:get(orphan_assets_purged, Summary)),
        ?assertEqual(0, alive_asset_count(Org, Ws, AgedId))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% F-1 retain_until 门：已固化未到期的超龄孤儿不清理
%% ===================================================================

f1_orphan_retain_until_in_future_is_kept() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        {AssetId, Key} = insert_orphan_asset(Scope, #{
            age_seconds => 2 * 86400, retain_until => Now + 3600
        }),
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, #{
            now => Now, batch_limit => 10, orphan_asset_age_seconds => 86400
        }),
        ?assertEqual(0, maps:get(orphan_assets_deleted, Summary)),
        ?assertEqual([], maps:get(orphan_assets_purged, Summary)),
        ?assertEqual(undefined, maps:get(audit_id, Summary)),
        ?assertEqual(1, alive_asset_count(Org, Ws, AssetId)),
        ?assertMatch({ok, _}, object_get(Org, Ws, Key))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% F-1 active hold：workspace 级 hold 覆盖的超龄孤儿被跳过
%% ===================================================================

f1_active_hold_keeps_aged_orphan() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        {AssetId, Key} = insert_orphan_asset(Scope, #{age_seconds => 2 * 86400}),
        _HoldId = insert_workspace_hold(Scope),
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, #{
            now => Now, batch_limit => 10, orphan_asset_age_seconds => 86400
        }),
        ?assertEqual(0, maps:get(orphan_assets_deleted, Summary)),
        ?assertEqual(
            [{AssetId, {orphan_asset_ineligible, active_hold}}], maps:get(skipped, Summary)
        ),
        ?assertEqual(undefined, maps:get(audit_id, Summary)),
        ?assertEqual(1, alive_asset_count(Org, Ws, AssetId)),
        ?assertMatch({ok, _}, object_get(Org, Ws, Key))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% F-1 作用域边界：绑定消息的 pending 资产不进孤儿候选（message_id IS NULL）
%% ===================================================================

f1_bound_pending_asset_not_orphan_purged() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        %% pending_confirm 且 message_id 非空：即便 ancient，也归消息保留期治理
        MsgId = insert_message(Scope, <<"eb03-f1-bound-msg">>, Now + 86400),
        {AssetId, Key} = insert_orphan_asset(Scope, bound_asset_spec(MsgId, Now)),
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, #{
            now => Now, batch_limit => 10, orphan_asset_age_seconds => 86400
        }),
        ?assertEqual(0, maps:get(deleted, Summary)),
        ?assertEqual(0, maps:get(orphan_assets_deleted, Summary)),
        ?assertEqual([], maps:get(skipped, Summary)),
        ?assertEqual(undefined, maps:get(audit_id, Summary)),
        ?assertEqual(1, alive_asset_count(Org, Ws, AssetId)),
        ?assertMatch({ok, _}, object_get(Org, Ws, Key)),
        ?assertEqual(1, alive_message_count(Org, Ws, MsgId))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% F-1 age 配置化：非法值 fail-closed
%% ===================================================================

f1_invalid_age_fails_closed() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        {AssetId, _Key} = insert_orphan_asset(Scope, #{age_seconds => 2 * 86400}),
        ?assertMatch(
            {error, {invalid_orphan_asset_age, <<"x">>}},
            eb_pg_purge:purge_batch(Org, Ws, #{
                now => Now, batch_limit => 10, orphan_asset_age_seconds => <<"x">>
            })
        ),
        ?assertMatch(
            {error, {invalid_orphan_asset_age, -1}},
            eb_pg_purge:purge_batch(Org, Ws, #{
                now => Now, batch_limit => 10, orphan_asset_age_seconds => -1
            })
        ),
        ?assertEqual(1, alive_asset_count(Org, Ws, AssetId))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% F-1 age 配置化：`imboy` app env 生效（用后还原）
%% ===================================================================

f1_env_age_config_is_honored() ->
    Scope = eb_pg_test_fixture:new_scope(),
    Old = application:get_env(imboy, eb_purge_orphan_asset_age_seconds),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        {AgedId, _Key} = insert_orphan_asset(Scope, #{age_seconds => 60}),
        {FreshId, _FreshKey} = insert_orphan_asset(Scope, #{age_seconds => 0}),
        %% env age=0：1 分钟前的孤儿即超龄
        ok = application:set_env(imboy, eb_purge_orphan_asset_age_seconds, 0),
        {ok, Short} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual([AgedId], maps:get(orphan_assets_purged, Short)),
        ?assertEqual(0, alive_asset_count(Org, Ws, AgedId)),
        %% env age 一年：1 分钟前的孤儿留
        ok = application:set_env(
            imboy, eb_purge_orphan_asset_age_seconds, 86400 * 365
        ),
        {ok, Long} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
        ?assertEqual(0, maps:get(orphan_assets_deleted, Long)),
        ?assertEqual(1, alive_asset_count(Org, Ws, FreshId))
    after
        restore_env(Old),
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% F-1 跨租户：孤儿候选同样受租户两键约束
%% ===================================================================

f1_orphans_never_cross_tenants() ->
    ScopeA = eb_pg_test_fixture:new_scope(),
    ScopeB = eb_pg_test_fixture:new_scope(),
    try
        {OrgA, WsA} = tenant(ScopeA),
        {OrgB, WsB} = tenant(ScopeB),
        Now = eb_system_clock:now(),
        {IdA, _KeyA} = insert_orphan_asset(ScopeA, #{age_seconds => 2 * 86400}),
        {IdB, _KeyB} = insert_orphan_asset(ScopeB, #{age_seconds => 2 * 86400}),
        %% A 的 Org 配 B 的 Workspace（错配）→ 不报错、不删行
        {ok, Mismatched} = eb_pg_purge:purge_batch(OrgA, WsB, #{
            now => Now, batch_limit => 10, orphan_asset_age_seconds => 86400
        }),
        ?assertEqual(0, maps:get(deleted, Mismatched)),
        ?assertEqual(0, maps:get(orphan_assets_deleted, Mismatched)),
        ?assertEqual(1, alive_asset_count(OrgA, WsA, IdA)),
        ?assertEqual(1, alive_asset_count(OrgB, WsB, IdB)),
        %% 正确作用域只清自己的孤儿
        {ok, Scoped} = eb_pg_purge:purge_batch(OrgA, WsA, #{
            now => Now, batch_limit => 10, orphan_asset_age_seconds => 86400
        }),
        ?assertEqual([IdA], maps:get(orphan_assets_purged, Scoped)),
        ?assertEqual(0, alive_asset_count(OrgA, WsA, IdA)),
        ?assertEqual(1, alive_asset_count(OrgB, WsB, IdB))
    after
        eb_pg_test_fixture:cleanup(ScopeA),
        eb_pg_test_fixture:cleanup(ScopeB)
    end.

%% ===================================================================
%% F-1 混合批：到期消息 + 超龄孤儿同批，审计仍为 message.purge 且带孤儿计数
%% ===================================================================

f1_mixed_batch_audit_keeps_message_purge_action() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        MsgId = insert_message(Scope, <<"eb03-f1-mixed-msg">>, Now - 60),
        {AgedId, _Key} = insert_orphan_asset(Scope, #{age_seconds => 2 * 86400}),
        {ok, Summary} = eb_pg_purge:purge_batch(Org, Ws, #{
            now => Now, batch_limit => 10, orphan_asset_age_seconds => 86400
        }),
        ?assertEqual([MsgId], maps:get(purged, Summary)),
        ?assertEqual(1, maps:get(deleted, Summary)),
        ?assertEqual([AgedId], maps:get(orphan_assets_purged, Summary)),
        ?assert(is_integer(maps:get(audit_id, Summary))),
        ?assertEqual(
            <<"message.purge">>,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT action FROM enterprise_audit_event"
                    " WHERE organization_id=$1 ORDER BY id DESC LIMIT 1"
                >>,
                [Org]
            )
        ),
        ?assertEqual(
            <<"1">>,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT detail->>'orphan_assets_deleted' FROM enterprise_audit_event"
                    " WHERE organization_id=$1 ORDER BY id DESC LIMIT 1"
                >>,
                [Org]
            )
        ),
        ?assertEqual(0, alive_message_count(Org, Ws, MsgId)),
        ?assertEqual(0, alive_asset_count(Org, Ws, AgedId))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 静态：F-1 语句保持租户两键 + 参数化 + SKIP LOCKED 形态
%% ===================================================================

f1_orphan_statements_are_tenant_scoped() ->
    Statements = eb_pg_purge:sql_statements(),
    OrphanCandidate =
        [S || S <- Statements, binary:match(S, <<"pending_confirm'">>) =/= nomatch],
    ?assertEqual(2, length(OrphanCandidate)),
    lists:foreach(
        fun(Sql) ->
            ?assert(binary:match(Sql, <<"$1">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"$2">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"organization_id">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"workspace_id">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"message_id IS NULL">>) =/= nomatch)
        end,
        OrphanCandidate
    ),
    ?assert(
        lists:any(
            fun(Sql) -> binary:match(Sql, <<"SKIP LOCKED">>) =/= nomatch end,
            OrphanCandidate
        )
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

%% 已绑定消息的资产造数（retain_until 与消息对齐，满足 retention guard）。
bound_asset_spec(MsgId, Now) ->
    #{
        age_seconds => 2 * 86400,
        message_id => MsgId,
        status => <<"active">>,
        retain_until => Now + 86400
    }.

%% 直接 SQL 造孤儿资产行 + 真实对象字节（对象 key 带租户作用域前缀）。
insert_orphan_asset(Scope, Extra) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    AssetId = eb_pg_test_fixture:id(),
    AgeSec = maps:get(age_seconds, Extra, 2 * 86400),
    MsgId = maps:get(message_id, Extra, undefined),
    Status = maps:get(status, Extra, <<"pending_confirm">>),
    RetainUntil =
        case maps:get(retain_until, Extra, undefined) of
            undefined -> null;
            RU when is_integer(RU) -> RU * 1000
        end,
    Payload = <<"f1-orphan-bytes-", (integer_to_binary(AssetId))/binary>>,
    ObjectKey = scoped_object_key(Org, Ws, AssetId),
    ok = eb_asset_object_stub:put(ObjectKey, Payload, #{}),
    ok = eb_pg_test_fixture:exec(
        <<
            "INSERT INTO enterprise_asset"
            " (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,"
            "  uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,"
            "  retain_until,version,created_at)"
            " VALUES ($1,$2,$3,$4,$5,NULL,NULL,$6,$7,'application/octet-stream',64,$8,1,"
            "         to_timestamp($9::bigint/1000),1,"
            "         CURRENT_TIMESTAMP - ($10::bigint) * interval '1 second')"
        >>,
        [
            AssetId,
            Org,
            Ws,
            Conv,
            MsgId,
            ObjectKey,
            sha256_hex(Payload),
            Status,
            RetainUntil,
            AgeSec
        ]
    ),
    {AssetId, ObjectKey}.

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
        Aad, <<"eb03-f1-orphan-body">>, eb_pg_test_fixture:key_ref(1)
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

insert_workspace_hold(Scope) ->
    HoldId = eb_pg_test_fixture:id(),
    {ok, _Row} = eb_pg_store:insert_hold(
        maps:get(org_id, Scope),
        maps:get(workspace_id, Scope),
        #{
            id => HoldId,
            scope_type => <<"workspace">>,
            reason_code => <<"eb03-f1-synthetic-hold">>,
            actor_user_id => maps:get(actor_user_id, Scope)
        }
    ),
    HoldId.

%% DB 直删（可控制 GUC 上下文）：unset = 不设 GUC；off = 显式 'off'。
%% 返回 {ok, deleted} 或 {error, {SQLState, ConstraintName}}。
direct_delete_asset(Org, Ws, AssetId, GucMode) ->
    elib_pg:with_tx(
        fun(Conn) ->
            case GucMode of
                unset -> ok;
                off -> _ = epgsql:squery(Conn, <<"SET LOCAL imboy.enterprise_purge = 'off'">>)
            end,
            case
                elib_pg:execute(
                    Conn,
                    <<
                        "DELETE FROM enterprise_asset"
                        " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
                    >>,
                    [Org, Ws, AssetId]
                )
            of
                {ok, _} ->
                    {ok, deleted};
                {error, #error{code = Code, extra = Extra}} ->
                    {error, {Code, constraint_of(Extra)}};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        [{reraise, false}]
    ).

constraint_of(Extra) when is_list(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        false -> undefined
    end;
constraint_of(_Other) ->
    undefined.

restore_env(undefined) ->
    _ = application:unset_env(imboy, eb_purge_orphan_asset_age_seconds),
    ok;
restore_env({ok, Value}) ->
    _ = application:set_env(imboy, eb_purge_orphan_asset_age_seconds, Value),
    ok.

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

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

scoped_object_key(Org, Ws, Id) ->
    iolist_to_binary([
        "enterprise/",
        integer_to_binary(Org),
        "/",
        integer_to_binary(Ws),
        "/f1-",
        integer_to_binary(Id),
        ".bin"
    ]).

object_get(Org, Ws, Key) ->
    eb_asset_object_stub:get(Key, eb_asset_object_stub:key_prefix(Org, Ws)).
