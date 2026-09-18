%% @doc AG31-03：Agent Grant 命令面套件（架构合同 §7 Grant / §8 Delegation）。
%%
%% 两个生成器：
%%   * `grant_domain_test_`——纯域断言（零 mock，铁律 4）：validate_issue /
%%     effective_status / constraint 只收窄 / revoke 裁决 / 幂等指纹 /
%%     catalog D7 空集格式；
%%   * `grant_command_test_`——meck {foreach, setup, cleanup, [groups]} 范式
%%     （镜像 agent_run_command_deny_tests）：mock agent_grant_pg /
%%     agent_org_membership_adapter / agent_capability_catalog（必要时
%%     passthrough meck logger 断言审计行），逐条覆盖派工包 §M.1-§M.4
%%     至少一正一反，并验证 env seam（agent_membership_module /
%%     agent_capability_catalog_module）。
%%
%% 时钟注入：Now 由用例显式给出（{{2026,9,17},{12,0,0}} 锚点），domain 与
%% command 均不取系统时间。
-module(agent_grant_tests).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, log/2]).

-define(PG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).

-define(ORG, 22).
-define(AGENT, 11).
-define(DELEGATOR, 77).
-define(GRANT_ID, 4242).
-define(EVENT_ID, 9100).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).
-define(VF, {{2026, 9, 1}, {0, 0, 0}}).
-define(EXP, {{2027, 9, 1}, {0, 0, 0}}).

%% ===================================================================
%% 生成器 1：纯域（零 mock）
%% ===================================================================

grant_domain_test_() ->
    [
        {"d1: validate_issue positive normalizes and keeps window", fun t_validate_positive/0},
        {"d2: validate_issue negatives (ids/key/window/scope/capabilities)",
            fun t_validate_negatives/0},
        {"d3: effective_status four computed states; expired not stored", fun t_effective_status/0},
        {"d4: constraint narrowing (legal keys + scalar shapes)", fun t_constraint/0},
        {"d5: revoke verdict active-only; pending stored-active revocable", fun t_revoke_verdict/0},
        {"d6: idempotency fingerprint same vs conflict", fun t_idempotency_verdict/0},
        {"d7: catalog frozen empty set + D7 entry format + lookup miss", fun t_catalog_empty/0}
    ].

t_validate_positive() ->
    Caps = [cap(<<"message.send">>, <<"invoke">>, <<"message">>, #{<<"workspace_id">> => 5})],
    {ok, Norm} = agent_grant_domain:validate_issue(
        ?ORG, ?AGENT, ?DELEGATOR, <<"explicit">>, [33, 34], Caps, ?VF, ?EXP, <<"k1">>
    ),
    ?assertEqual(explicit, maps:get(workspace_scope_kind, Norm)),
    ?assertEqual([33, 34], maps:get(workspace_ids, Norm)),
    ?assertEqual(1, length(maps:get(capabilities, Norm))),
    ?assertEqual(<<"k1">>, maps:get(idempotency_key, Norm)),
    %% scope=none 规范为零行
    {ok, NormNone} = agent_grant_domain:validate_issue(
        ?ORG, ?AGENT, ?DELEGATOR, none, [], [cap0()], ?VF, ?EXP, <<"k1">>
    ),
    ?assertEqual([], maps:get(workspace_ids, NormNone)),
    ok.

t_validate_negatives() ->
    %% id 非法
    ?assertEqual(
        {error, validation_failed},
        agent_grant_domain:validate_issue(
            0, ?AGENT, ?DELEGATOR, none, [], [cap0()], ?VF, ?EXP, <<"k1">>
        )
    ),
    %% 幂等键空
    ?assertEqual(
        {error, validation_failed},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, none, [], [cap0()], ?VF, ?EXP, <<>>
        )
    ),
    %% 时间序：expires == valid_from 同刻拒绝；逆序拒绝（§M.1 expired 时间序）
    ?assertEqual(
        {error, invalid_validity},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, none, [], [cap0()], ?VF, ?VF, <<"k1">>
        )
    ),
    ?assertEqual(
        {error, invalid_validity},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, none, [], [cap0()], ?EXP, ?VF, <<"k1">>
        )
    ),
    %% scope：explicit 空列表必须拒绝（≥1 行）；none 带行必须拒绝；通配 all 非法
    ?assertEqual(
        {error, invalid_workspace_scope},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, explicit, [], [cap0()], ?VF, ?EXP, <<"k1">>
        )
    ),
    ?assertEqual(
        {error, invalid_workspace_scope},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, none, [33], [cap0()], ?VF, ?EXP, <<"k1">>
        )
    ),
    ?assertEqual(
        {error, invalid_workspace_scope},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, all, [], [cap0()], ?VF, ?EXP, <<"k1">>
        )
    ),
    %% 重复 workspace id
    ?assertEqual(
        {error, invalid_workspace_scope},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, explicit, [33, 33], [cap0()], ?VF, ?EXP, <<"k1">>
        )
    ),
    %% capability 列表空 / 空串字段 / 三元组重复
    ?assertEqual(
        {error, validation_failed},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, none, [], [], ?VF, ?EXP, <<"k1">>
        )
    ),
    ?assertEqual(
        {error, validation_failed},
        agent_grant_domain:validate_issue(
            ?ORG,
            ?AGENT,
            ?DELEGATOR,
            none,
            [],
            [cap(<<>>, <<"a">>, <<"b">>, #{})],
            ?VF,
            ?EXP,
            <<"k1">>
        )
    ),
    Dup = [cap(<<"x">>, <<"a">>, <<"b">>, #{}), cap(<<"x">>, <<"a">>, <<"b">>, #{})],
    ?assertEqual(
        {error, validation_failed},
        agent_grant_domain:validate_issue(
            ?ORG, ?AGENT, ?DELEGATOR, none, [], Dup, ?VF, ?EXP, <<"k1">>
        )
    ),
    ok.

t_effective_status() ->
    Grant = #{status => active, valid_from => ?VF, expires_at => ?EXP},
    %% pending：未生效（读时实时算，不入库）
    ?assertEqual(
        pending, agent_grant_domain:effective_status(Grant, {{2026, 8, 31}, {23, 59, 59}})
    ),
    %% active：窗内
    ?assertEqual(active, agent_grant_domain:effective_status(Grant, ?NOW)),
    %% expired：到期即拒（不依赖后台任务；存储态仍是 active）
    ?assertEqual(expired, agent_grant_domain:effective_status(Grant, ?EXP)),
    ?assertEqual(expired, agent_grant_domain:effective_status(Grant, {{2028, 1, 1}, {0, 0, 0}})),
    %% revoked 是终态，优先于时间窗
    ?assertEqual(
        revoked,
        agent_grant_domain:effective_status(Grant#{status => revoked}, {{2020, 1, 1}, {0, 0, 0}})
    ),
    ok.

t_constraint() ->
    Legal = [<<"workspace_id">>, <<"max_calls">>],
    ?assertEqual(ok, agent_grant_domain:validate_constraint(undefined, Legal)),
    ?assertEqual(ok, agent_grant_domain:validate_constraint(#{}, Legal)),
    ?assertEqual(
        ok,
        agent_grant_domain:validate_constraint(
            #{<<"workspace_id">> => 5, <<"max_calls">> => [1, 2]}, Legal
        )
    ),
    %% 未知 key 在发行阶段拒绝、不忽略（D7 §3）
    ?assertEqual(
        {error, {invalid_constraint, <<"role">>}},
        agent_grant_domain:validate_constraint(#{<<"role">> => <<"admin">>}, Legal)
    ),
    %% 嵌套 map = 潜在「否定后再扩大」表达面，拒绝
    ?assertEqual(
        {error, {invalid_constraint, <<"filter">>}},
        agent_grant_domain:validate_constraint(#{<<"filter">> => #{<<"not">> => 1}}, Legal)
    ),
    ok.

t_revoke_verdict() ->
    ?assertEqual(ok, agent_grant_domain:assert_revoke_allowed(#{status => active})),
    %% 存储态裁决：pending（存储 active）可撤销
    ?assertEqual(ok, agent_grant_domain:assert_revoke_allowed(#{status => active})),
    ?assertEqual(
        {error, already_revoked}, agent_grant_domain:assert_revoke_allowed(#{status => revoked})
    ),
    ?assertEqual({error, not_found}, agent_grant_domain:assert_revoke_allowed(#{})),
    ok.

t_idempotency_verdict() ->
    Req = #{
        agent_id => ?AGENT,
        workspace_scope_kind => none,
        workspace_ids => [],
        capabilities => [cap0()],
        valid_from => ?VF,
        expires_at => ?EXP
    },
    StoredSame = #{
        agent_id => ?AGENT,
        workspace_scope_kind => none,
        workspace_ids => [],
        capabilities => [cap0()],
        valid_from => ?VF,
        expires_at => ?EXP
    },
    ?assertEqual(same, agent_grant_domain:idempotency_verdict(Req, StoredSame)),
    %% 子行顺序不敏感（workspace 排序后比对）
    ?assertEqual(
        same,
        agent_grant_domain:idempotency_verdict(
            Req#{workspace_scope_kind => explicit, workspace_ids => [33, 34]},
            StoredSame#{workspace_scope_kind => explicit, workspace_ids => [34, 33]}
        )
    ),
    %% 任一差异 → conflict
    ?assertEqual(
        conflict, agent_grant_domain:idempotency_verdict(Req, StoredSame#{agent_id => 12})
    ),
    ?assertEqual(
        conflict,
        agent_grant_domain:idempotency_verdict(Req, StoredSame#{
            expires_at => {{2027, 9, 2}, {0, 0, 0}}
        })
    ),
    ?assertEqual(
        conflict,
        agent_grant_domain:idempotency_verdict(
            Req,
            StoredSame#{capabilities => [cap(<<"other">>, <<"a">>, <<"b">>, #{})]}
        )
    ),
    ok.

t_catalog_empty() ->
    %% D7 冻结：V3.1 首切片枚举 = 空集
    ?assertEqual([], agent_capability_catalog:entries()),
    %% 查询未命中 → not_found（消费方映射 unknown_capability 拒绝）
    ?assertEqual(
        {error, not_found},
        agent_capability_catalog:lookup(<<"message.send">>, <<"invoke">>, <<"message">>)
    ),
    ok.

%% ===================================================================
%% 生成器 2：命令编排（meck）
%% ===================================================================

grant_command_test_() ->
    {foreach,
        fun() ->
            meck:new(?PG, [no_link]),
            meck:new(?MEMBERSHIP, [no_link]),
            meck:new(?CATALOG, [no_link]),
            ok
        end,
        fun(_) ->
            %% logger 由 audit 组按用例内 meck；失败中断时在此兜底卸载
            lists:foreach(
                fun(M) ->
                    try
                        meck:unload(M)
                    catch
                        _:_ -> ok
                    end
                end,
                [?PG, ?MEMBERSHIP, ?CATALOG, logger]
            ),
            application:unset_env(imboy, agent_membership_module),
            application:unset_env(imboy, agent_capability_catalog_module),
            ok
        end,
        [
            fun issue_positive_tests/1,
            fun issue_identity_tests/1,
            fun issue_membership_tests/1,
            fun issue_scope_tests/1,
            fun issue_capability_tests/1,
            fun issue_idempotency_tests/1,
            fun get_list_tests/1,
            fun revoke_tests/1,
            fun audit_tests/1,
            fun env_seam_tests/1
        ]}.

%% ------------------------------------------------------------------
%% §M.1 issue 正路径
%% ------------------------------------------------------------------

issue_positive_tests(_) ->
    [
        {"issue happy path: single tx writes grant+event(issued,human), replay=false", fun() ->
            given_preissue_ok(),
            expect_insert_ok(),
            {ok, Result} = agent_grant_command:issue(conn(), ctx()),
            ?assertEqual(?GRANT_ID, maps:get(grant_id, Result)),
            ?assertEqual(1, maps:get(version, Result)),
            ?assertEqual(active, maps:get(effective_status, Result)),
            ?assertEqual(false, maps:get(replay, Result)),
            %% 四写单事务：grant + workspace 行 + capability 行 + event
            %% （meck:capture 按参数位捕获：1=Conn 2=Grant 3=WsIds 4=Caps 5=Event）
            Grant = meck:capture(first, ?PG, insert_grant_tx, ['_', '_', '_', '_', '_'], 2),
            WsIds = meck:capture(first, ?PG, insert_grant_tx, ['_', '_', '_', '_', '_'], 3),
            Caps = meck:capture(first, ?PG, insert_grant_tx, ['_', '_', '_', '_', '_'], 4),
            Event = meck:capture(first, ?PG, insert_grant_tx, ['_', '_', '_', '_', '_'], 5),
            ?assertEqual(?ORG, maps:get(organization_id, Grant)),
            ?assertEqual(?DELEGATOR, maps:get(delegator_user_id, Grant)),
            ?assertEqual(none, maps:get(workspace_scope_kind, Grant)),
            ?assertEqual([], WsIds),
            ?assertEqual(1, length(Caps)),
            ?assertEqual(issued, maps:get(event_type, Event)),
            ?assertEqual(human, maps:get(actor_kind, Event)),
            ?assertEqual(?DELEGATOR, maps:get(actor_user_id, Event)),
            %% event 幂等键带 (org, delegator) 前缀（全局唯一域消除跨 Org 碰撞）
            ?assertMatch(<<"agent_grant:22:77:", _/binary>>, maps:get(idempotency_key, Event)),
            %% 幂等预检先行
            ?assert(
                meck:called(?PG, find_grant_by_idempotency, [conn(), ?ORG, ?DELEGATOR, <<"k1">>])
            )
        end},
        {"issue explicit scope passes workspace rows to tx", fun() ->
            %% 组内共享 meck 实例：清历史再捕获本用例的调用
            meck:reset(?PG),
            given_preissue_ok(),
            expect_insert_ok(),
            Ctx = ctx(#{workspace_scope_kind => explicit, workspace_ids => [33, 34]}),
            {ok, _} = agent_grant_command:issue(conn(), Ctx),
            WsIds = meck:capture(first, ?PG, insert_grant_tx, ['_', '_', '_', '_', '_'], 3),
            ?assertEqual([33, 34], WsIds)
        end}
    ].

%% ------------------------------------------------------------------
%% §M.1 身份：delegator Human（user 表权威事实）；Agent=1
%% ------------------------------------------------------------------

issue_identity_tests(_) ->
    [
        {"delegator missing -> delegator_not_found", fun() ->
            meck:expect(?PG, get_user_account_type, fun(_C, _U) -> {error, not_found} end),
            ?assertEqual({error, delegator_not_found}, agent_grant_command:issue(conn(), ctx()))
        end},
        {"delegator account_type=1 (Agent) -> delegator_not_human (V3.1 禁转授权)", fun() ->
            meck:expect(?PG, get_user_account_type, fun(_C, _U) -> {ok, 1} end),
            ?assertEqual({error, delegator_not_human}, agent_grant_command:issue(conn(), ctx())),
            ?assertEqual(0, meck:num_calls(?PG, insert_grant_tx, '_'))
        end},
        {"delegator account_type=2 (system_bot) -> delegator_not_human (Human 权威判据=0)", fun() ->
            meck:expect(?PG, get_user_account_type, fun(_C, _U) -> {ok, 2} end),
            ?assertEqual({error, delegator_not_human}, agent_grant_command:issue(conn(), ctx())),
            ?assertEqual(0, meck:num_calls(?PG, insert_grant_tx, '_'))
        end},
        {"delegator account_type=3 (bot) -> delegator_not_human (Human 权威判据=0)", fun() ->
            meck:expect(?PG, get_user_account_type, fun(_C, _U) -> {ok, 3} end),
            ?assertEqual({error, delegator_not_human}, agent_grant_command:issue(conn(), ctx())),
            ?assertEqual(0, meck:num_calls(?PG, insert_grant_tx, '_'))
        end},
        {"delegator account_type=0 (Human) passes identity gate", fun() ->
            meck:expect(?PG, get_user_account_type, fun
                (_C, U) when U =:= ?DELEGATOR -> {ok, 0};
                (_C, U) when U =:= ?AGENT -> {ok, 1}
            end),
            meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
                {error, inactive}
            end),
            %% 通过 Human 关后停在 membership 关（证明 identity 门已过）
            ?assertEqual({error, agent_membership_denied}, agent_grant_command:issue(conn(), ctx()))
        end},
        {"agent account_type=0 -> agent_not_agent", fun() ->
            meck:expect(?PG, get_user_account_type, fun
                (_C, U) when U =:= ?DELEGATOR -> {ok, 0};
                (_C, _U) -> {ok, 0}
            end),
            ?assertEqual({error, agent_not_agent}, agent_grant_command:issue(conn(), ctx()))
        end},
        {"agent missing -> agent_not_found", fun() ->
            meck:expect(?PG, get_user_account_type, fun
                (_C, U) when U =:= ?DELEGATOR -> {ok, 0};
                (_C, _U) -> {error, not_found}
            end),
            ?assertEqual({error, agent_not_found}, agent_grant_command:issue(conn(), ctx()))
        end}
    ].

%% ------------------------------------------------------------------
%% §M.1 membership：port 必须 {ok, active/member}
%% ------------------------------------------------------------------

issue_membership_tests(_) ->
    [
        {"membership inactive -> agent_membership_denied", fun() ->
            given_identity_ok(),
            meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
                {error, inactive}
            end),
            ?assertEqual({error, agent_membership_denied}, agent_grant_command:issue(conn(), ctx()))
        end},
        {"membership not_found -> agent_membership_denied", fun() ->
            given_identity_ok(),
            meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
                {error, not_found}
            end),
            ?assertEqual({error, agent_membership_denied}, agent_grant_command:issue(conn(), ctx()))
        end},
        {"membership port unavailable -> membership_unavailable (fail closed)", fun() ->
            given_identity_ok(),
            meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
                {error, unavailable}
            end),
            ?assertEqual({error, membership_unavailable}, agent_grant_command:issue(conn(), ctx()))
        end},
        {"membership crash -> membership_unavailable (fail closed)", fun() ->
            given_identity_ok(),
            meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
                erlang:error(simulated_crash)
            end),
            ?assertEqual({error, membership_unavailable}, agent_grant_command:issue(conn(), ctx()))
        end}
    ].

%% ------------------------------------------------------------------
%% §M.1 workspace scope：none→零行；explicit→≥1；跨 Org→拒绝
%% ------------------------------------------------------------------

issue_scope_tests(_) ->
    [
        {"explicit scope: rows forwarded (positive path covered in issue_positive)", fun() ->
            given_preissue_ok(),
            expect_insert_ok(),
            {ok, _} = agent_grant_command:issue(
                conn(), ctx(#{workspace_scope_kind => explicit, workspace_ids => [33]})
            ),
            ?assert(meck:called(?PG, insert_grant_tx, '_'))
        end},
        {"cross-org workspace (DB FK 23503) -> cross_org_workspace rejection", fun() ->
            given_preissue_ok(),
            meck:expect(?PG, insert_grant_tx, fun(_C, _G, _W, _Cap, _E) ->
                {error, cross_org_workspace}
            end),
            Ctx = ctx(#{workspace_scope_kind => explicit, workspace_ids => [99]}),
            ?assertEqual({error, cross_org_workspace}, agent_grant_command:issue(conn(), Ctx))
        end},
        {"explicit with empty workspace list rejected before any DB access", fun() ->
            meck:reset(?PG),
            meck:expect(?PG, get_user_account_type, fun(_C, _U) -> {ok, 0} end),
            Ctx = ctx(#{workspace_scope_kind => explicit, workspace_ids => []}),
            ?assertEqual({error, invalid_workspace_scope}, agent_grant_command:issue(conn(), Ctx)),
            ?assertEqual(0, meck:num_calls(?PG, get_user_account_type, '_'))
        end}
    ].

%% ------------------------------------------------------------------
%% §M.1 capability：目录命中 + constraint 只收窄（D7 空集 → 全拒）
%% ------------------------------------------------------------------

issue_capability_tests(_) ->
    [
        {"default D7 empty catalog: every capability is unknown", fun() ->
            given_identity_ok(),
            given_membership_ok(),
            %% 默认目录模块被 mock 但无 lookup 期望 → 显式给空集行为
            meck:expect(?CATALOG, lookup, fun(_C, _A, _R) -> {error, not_found} end),
            ?assertEqual(
                {error, {unknown_capability, {<<"echo">>, <<"invoke">>, <<"message">>}}},
                agent_grant_command:issue(conn(), ctx())
            )
        end},
        {"catalog entry hit + legal constraint keys -> passes gate", fun() ->
            given_preissue_ok(),
            expect_insert_ok(),
            {ok, _} = agent_grant_command:issue(conn(), ctx()),
            ?assert(meck:called(?PG, insert_grant_tx, '_'))
        end},
        {"constraint key outside legal_constraint_keys -> invalid_constraint", fun() ->
            given_identity_ok(),
            given_membership_ok(),
            given_catalog_entry([<<"workspace_id">>]),
            BadCtx = ctx(#{
                capabilities => [
                    cap(<<"echo">>, <<"invoke">>, <<"message">>, #{<<"role">> => <<"admin">>})
                ]
            }),
            ?assertEqual(
                {error, {invalid_constraint, <<"role">>}}, agent_grant_command:issue(conn(), BadCtx)
            )
        end},
        {"catalog crash -> catalog_unavailable (fail closed)", fun() ->
            given_identity_ok(),
            given_membership_ok(),
            meck:expect(?CATALOG, lookup, fun(_C, _A, _R) -> erlang:error(catalog_down) end),
            ?assertEqual({error, catalog_unavailable}, agent_grant_command:issue(conn(), ctx()))
        end}
    ].

%% ------------------------------------------------------------------
%% §M.1 幂等：同 key 同 payload → 既有 Grant；异 payload → 冲突
%% ------------------------------------------------------------------

issue_idempotency_tests(_) ->
    [
        {"same key same payload -> existing grant, replay=true", fun() ->
            given_preissue_ok(),
            meck:expect(?PG, find_grant_by_idempotency, fun(_C, _O, _D, _K) ->
                {ok, stored_grant(#{})}
            end),
            meck:expect(?PG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            meck:expect(?PG, list_capabilities, fun(_C, _O, _G) -> caps_row() end),
            {ok, Result} = agent_grant_command:issue(conn(), ctx()),
            ?assertEqual(true, maps:get(replay, Result)),
            ?assertEqual(?GRANT_ID, maps:get(grant_id, Result)),
            ?assertEqual(active, maps:get(effective_status, Result)),
            ?assertEqual(0, meck:num_calls(?PG, insert_grant_tx, '_'))
        end},
        {"same key different payload -> idempotency_conflict", fun() ->
            given_preissue_ok(),
            meck:expect(?PG, find_grant_by_idempotency, fun(_C, _O, _D, _K) ->
                {ok, stored_grant(#{agent_id => 12})}
            end),
            meck:expect(?PG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            meck:expect(?PG, list_capabilities, fun(_C, _O, _G) -> caps_row() end),
            ?assertEqual({error, idempotency_conflict}, agent_grant_command:issue(conn(), ctx()))
        end},
        {"replay computes effective_status=expired from injected clock", fun() ->
            given_preissue_ok(),
            %% 请求窗与存储窗一致（指纹全等），但整窗已过 → replay 且 expired
            PastCtx = ctx(#{
                valid_from => {{2025, 9, 1}, {0, 0, 0}},
                expires_at => {{2026, 9, 16}, {0, 0, 0}}
            }),
            meck:expect(?PG, find_grant_by_idempotency, fun(_C, _O, _D, _K) ->
                {ok,
                    stored_grant(#{
                        valid_from => {{2025, 9, 1}, {0, 0, 0}},
                        expires_at => {{2026, 9, 16}, {0, 0, 0}}
                    })}
            end),
            meck:expect(?PG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            meck:expect(?PG, list_capabilities, fun(_C, _O, _G) -> caps_row() end),
            {ok, Result} = agent_grant_command:issue(conn(), PastCtx),
            ?assertEqual(true, maps:get(replay, Result)),
            ?assertEqual(expired, maps:get(effective_status, Result))
        end},
        {"insert race (23505) re-reads and converges to same grant", fun() ->
            given_preissue_ok(),
            meck:expect(?PG, find_grant_by_idempotency, fun(_C, _O, _D, _K) ->
                {error, not_found}
            end),
            meck:expect(?PG, insert_grant_tx, fun(_C, _G, _W, _Cap, _E) ->
                {error, idempotency_duplicate}
            end),
            %% re-read 路径（第二次 find）返回既有行 → 收敛为 replay
            meck:expect(?PG, find_grant_by_idempotency, fun(_C, _O, _D, _K) ->
                {ok, stored_grant(#{})}
            end),
            meck:expect(?PG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            meck:expect(?PG, list_capabilities, fun(_C, _O, _G) -> caps_row() end),
            {ok, Result} = agent_grant_command:issue(conn(), ctx()),
            ?assertEqual(true, maps:get(replay, Result))
        end}
    ].

%% ------------------------------------------------------------------
%% §M.2 get/list：org 内 bounded + 实时有效态
%% ------------------------------------------------------------------

get_list_tests(_) ->
    [
        {"get returns view with computed effective_status", fun() ->
            meck:expect(?PG, get_grant, fun(_C, ?ORG, ?GRANT_ID) -> {ok, stored_grant(#{})} end),
            meck:expect(?PG, list_workspace_ids, fun(_C, _O, _G) -> [33] end),
            meck:expect(?PG, list_capabilities, fun(_C, _O, _G) -> caps_row() end),
            {ok, View} = agent_grant_command:get(conn(), ?ORG, ?GRANT_ID, ?NOW),
            ?assertEqual(active, maps:get(effective_status, View)),
            ?assertEqual([33], maps:get(workspace_ids, View)),
            ?assertEqual(1, length(maps:get(capabilities, View)))
        end},
        {"get org mismatch / missing -> not_found", fun() ->
            meck:expect(?PG, get_grant, fun(_C, _O, _G) -> {error, not_found} end),
            ?assertEqual({error, not_found}, agent_grant_command:get(conn(), ?ORG, ?GRANT_ID, ?NOW))
        end},
        {"list returns bounded org-scoped views", fun() ->
            meck:expect(?PG, list_grants, fun(_C, #{organization_id := ?ORG, limit := 10}) ->
                [stored_grant(#{id => 1}), stored_grant(#{id => 2})]
            end),
            meck:expect(?PG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            meck:expect(?PG, list_capabilities, fun(_C, _O, _G) -> [] end),
            {ok, Views} = agent_grant_command:list(conn(), #{
                organization_id => ?ORG, limit => 10, now => ?NOW
            }),
            ?assertEqual(2, length(Views)),
            ?assertEqual(active, maps:get(effective_status, hd(Views)))
        end},
        {"list after expiry computes expired without rewriting storage", fun() ->
            meck:expect(?PG, list_grants, fun(_C, _F) -> [stored_grant(#{})] end),
            meck:expect(?PG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            meck:expect(?PG, list_capabilities, fun(_C, _O, _G) -> [] end),
            {ok, [View]} = agent_grant_command:list(conn(), #{
                organization_id => ?ORG, now => {{2028, 1, 1}, {0, 0, 0}}
            }),
            ?assertEqual(expired, maps:get(effective_status, View)),
            ?assertEqual(active, maps:get(status, View))
        end}
    ].

%% ------------------------------------------------------------------
%% §M.3 revoke：CAS；仅 active→revoked；一胜一拒
%% ------------------------------------------------------------------

revoke_tests(_) ->
    [
        {"revoke happy path: CAS wins, event(revoked) written, version bumped", fun() ->
            meck:expect(?PG, next_id, fun(agent_grant_event) -> ?EVENT_ID end),
            meck:expect(?PG, get_grant, fun(_C, ?ORG, ?GRANT_ID) -> {ok, stored_grant(#{})} end),
            meck:expect(?PG, revoke_cas_tx, fun(_C, O, G, 1, Now, Event) ->
                ?assertEqual(?ORG, O),
                ?assertEqual(?GRANT_ID, G),
                ?assertEqual(?NOW, Now),
                ?assertEqual(revoked, maps:get(event_type, Event)),
                ?assertEqual(human, maps:get(actor_kind, Event)),
                ?assertEqual(7, maps:get(actor_user_id, Event)),
                ?assertEqual(2, maps:get(to_version, Event)),
                {ok, 2}
            end),
            Ctx = revoke_ctx(#{revoker_user_id => 7, expected_version => 1}),
            {ok, Result} = agent_grant_command:revoke(conn(), Ctx),
            ?assertEqual(2, maps:get(version, Result)),
            ?assertEqual(revoked, maps:get(effective_status, Result))
        end},
        {"revoke on already-revoked grant -> already_revoked (no CAS attempt)", fun() ->
            meck:reset(?PG),
            meck:expect(?PG, get_grant, fun(_C, _O, _G) ->
                {ok, stored_grant(#{status => revoked})}
            end),
            ?assertEqual(
                {error, already_revoked}, agent_grant_command:revoke(conn(), revoke_ctx(#{}))
            ),
            ?assertEqual(0, meck:num_calls(?PG, revoke_cas_tx, '_'))
        end},
        {"concurrent revoke loser: CAS 0 rows -> version_conflict (一胜一拒)", fun() ->
            meck:expect(?PG, next_id, fun(agent_grant_event) -> ?EVENT_ID end),
            meck:expect(?PG, get_grant, fun(_C, _O, _G) -> {ok, stored_grant(#{})} end),
            meck:expect(?PG, revoke_cas_tx, fun(_C, _O, _G, _V, _N, _E) ->
                {error, version_conflict}
            end),
            ?assertEqual(
                {error, version_conflict}, agent_grant_command:revoke(conn(), revoke_ctx(#{}))
            )
        end},
        {"revoke missing grant -> not_found", fun() ->
            meck:expect(?PG, get_grant, fun(_C, _O, _G) -> {error, not_found} end),
            ?assertEqual({error, not_found}, agent_grant_command:revoke(conn(), revoke_ctx(#{})))
        end},
        {"revoke ctx validation: bad expected_version -> validation_failed", fun() ->
            ?assertEqual(
                {error, validation_failed},
                agent_grant_command:revoke(conn(), revoke_ctx(#{expected_version => 0}))
            )
        end},
        {"revoke winner payload passes expected_version through (CAS atom)", fun() ->
            meck:expect(?PG, next_id, fun(agent_grant_event) -> ?EVENT_ID end),
            meck:expect(?PG, get_grant, fun(_C, _O, _G) -> {ok, stored_grant(#{version => 3})} end),
            meck:expect(?PG, revoke_cas_tx, fun(_C, _O, _G, Expected, _N, _E) ->
                ?assertEqual(3, Expected),
                {ok, 4}
            end),
            {ok, Result} = agent_grant_command:revoke(conn(), revoke_ctx(#{expected_version => 3})),
            ?assertEqual(4, maps:get(version, Result))
        end}
    ].

%% ------------------------------------------------------------------
%% §M.4 拒绝也要审计（真 logger handler 捕获；logger 是 sticky 内核模块
%% 不 meck——测试模块自带 handler 回调，经 add_handler/remove_handler 挂载）
%% ------------------------------------------------------------------

audit_tests(_) ->
    [
        {"pre-issue denial emits structured audit line (zero PII)", fun() ->
            with_audit_capture(fun() ->
                meck:expect(?PG, get_user_account_type, fun(_C, _U) -> {ok, 1} end),
                ?assertEqual(
                    {error, delegator_not_human}, agent_grant_command:issue(conn(), ctx())
                ),
                #{report := Report} = recv_audit(),
                ?assertEqual(agent_grant_denied, maps:get(what, Report)),
                ?assertEqual(<<"issue">>, maps:get(action, Report))
            end)
        end},
        {"audit line carries decision summary (ids/enums/counts, zero PII)", fun() ->
            with_audit_capture(fun() ->
                meck:expect(?PG, get_user_account_type, fun(_C, _U) -> {ok, 1} end),
                _ = agent_grant_command:issue(conn(), ctx()),
                #{meta := Meta} = recv_audit(),
                ?assertEqual(true, maps:get(audit, Meta)),
                Summary = maps:get(summary, Meta),
                ?assertEqual(?ORG, maps:get(organization_id, Summary)),
                ?assertEqual(?AGENT, maps:get(agent_id, Summary)),
                ?assertEqual(?DELEGATOR, maps:get(delegator_user_id, Summary)),
                ?assertEqual(<<"k1">>, maps:get(idempotency_key, Summary))
            end)
        end},
        {"revoke version_conflict (on-existing-grant denial) audited", fun() ->
            with_audit_capture(fun() ->
                meck:expect(?PG, next_id, fun(agent_grant_event) -> ?EVENT_ID end),
                meck:expect(?PG, get_grant, fun(_C, _O, _G) -> {ok, stored_grant(#{})} end),
                meck:expect(?PG, revoke_cas_tx, fun(_C, _O, _G, _V, _N, _E) ->
                    {error, version_conflict}
                end),
                ?assertEqual(
                    {error, version_conflict}, agent_grant_command:revoke(conn(), revoke_ctx(#{}))
                ),
                #{report := Report} = recv_audit(),
                ?assertEqual(agent_grant_denied, maps:get(what, Report)),
                ?assertEqual(<<"revoke">>, maps:get(action, Report))
            end)
        end},
        {"success path emits no denial audit", fun() ->
            with_audit_capture(fun() ->
                given_preissue_ok(),
                expect_insert_ok(),
                {ok, _} = agent_grant_command:issue(conn(), ctx()),
                receive
                    {ag31_audit, _UnexpectedReport, _M} -> erlang:error(unexpected_denial_audit)
                after 500 -> ok
                end
            end)
        end}
    ].

%% ------------------------------------------------------------------
%% env seam：agent_membership_module / agent_capability_catalog_module
%% ------------------------------------------------------------------

env_seam_tests(_) ->
    [
        {"agent_membership_module env overrides default adapter", fun() ->
            Fake = ag31_test_membership_mod,
            meck:new(Fake, [no_link, non_strict]),
            meck:expect(Fake, resolve_organization_membership, fun(O, A) ->
                ?assertEqual(?ORG, O),
                ?assertEqual(?AGENT, A),
                {error, inactive}
            end),
            ok = application:set_env(imboy, agent_membership_module, Fake),
            given_identity_ok(),
            ?assertEqual(
                {error, agent_membership_denied}, agent_grant_command:issue(conn(), ctx())
            ),
            ?assertEqual(1, meck:num_calls(Fake, resolve_organization_membership, '_')),
            %% 默认 adapter 未被触碰
            ?assertEqual(0, meck:num_calls(?MEMBERSHIP, resolve_organization_membership, '_')),
            %% 组内自清理（{foreach} 的 cleanup 按组项运行，组内测试共享 setup）
            meck:unload(Fake),
            application:unset_env(imboy, agent_membership_module)
        end},
        {"agent_capability_catalog_module env overrides default catalog", fun() ->
            %% 双保险：确认前一用例的 membership env 已清（组内自清理）
            application:unset_env(imboy, agent_membership_module),
            Fake = ag31_test_catalog_mod,
            meck:new(Fake, [no_link, non_strict]),
            %% Fake 目录 = 空集语义：lookup 未命中 → unknown_capability
            meck:expect(Fake, lookup, fun(_C, _A, _R) -> {error, not_found} end),
            ok = application:set_env(imboy, agent_capability_catalog_module, Fake),
            given_identity_ok(),
            given_membership_ok(),
            ?assertEqual(
                {error, {unknown_capability, {<<"echo">>, <<"invoke">>, <<"message">>}}},
                agent_grant_command:issue(conn(), ctx())
            ),
            ?assertEqual(1, meck:num_calls(Fake, lookup, '_')),
            meck:unload(Fake),
            application:unset_env(imboy, agent_capability_catalog_module)
        end}
    ].

%% ===================================================================
%% 审计捕获（logger handler 回调；§M.4 的测试面）
%% ===================================================================

%% handler API（logger:add_handler(Id, ?MODULE, #{config => self()})）。
%% 注意 OTP29 语义：log/2 第二参是 handler config（init 的 {ok, State} 落在
%% 该 map 的 `config` 键），测试 pid 从那里取。
init(Config) ->
    {ok, maps:get(config, Config, self())}.

%% 只转发本套件的审计报告（what=agent_grant_denied），其余日志噪声忽略；
%% handler 在独立 olp 进程回调，消息投递到测试进程邮箱。
log(Event, HandlerConfig) ->
    TestPid = maps:get(config, HandlerConfig, undefined),
    %% OTP29 起 report msg 形如 {report, Report}；兼容旧三元素 {report, Report, Format}
    Case = {is_pid(TestPid), maps:get(msg, Event, undefined)},
    case Case of
        {true, {report, Report}} when is_map(Report) ->
            forward_audit(TestPid, Event, Report);
        {true, {report, Report, _Format}} when is_map(Report) ->
            forward_audit(TestPid, Event, Report);
        _NotOurs ->
            ok
    end;
log(_Event, _HandlerConfig) ->
    ok.

forward_audit(TestPid, Event, Report) ->
    case maps:get(what, Report, undefined) =:= agent_grant_denied of
        true ->
            TestPid ! {ag31_audit, Report, maps:get(meta, Event, #{})},
            ok;
        false ->
            ok
    end.

with_audit_capture(F) ->
    case logger:add_handler(ag31_grant_test_capture, ?MODULE, #{config => self()}) of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok = logger:remove_handler(ag31_grant_test_capture),
            ok = logger:add_handler(ag31_grant_test_capture, ?MODULE, #{config => self()})
    end,
    try
        F()
    after
        _ = logger:remove_handler(ag31_grant_test_capture)
    end,
    ok.

recv_audit() ->
    receive
        {ag31_audit, Report, Meta} -> #{report => Report, meta => Meta}
    after 2000 -> erlang:error(audit_line_not_emitted)
    end.

%% ===================================================================
%% fixtures
%% ===================================================================

ctx() -> ctx(#{}).

ctx(Over) ->
    Maps = #{
        organization_id => ?ORG,
        agent_id => ?AGENT,
        delegator_user_id => ?DELEGATOR,
        workspace_scope_kind => none,
        workspace_ids => [],
        capabilities => [cap(<<"echo">>, <<"invoke">>, <<"message">>, #{})],
        valid_from => ?VF,
        expires_at => ?EXP,
        idempotency_key => <<"k1">>,
        now => ?NOW
    },
    maps:merge(Maps, Over).

revoke_ctx(Over) ->
    maps:merge(
        #{
            organization_id => ?ORG,
            grant_id => ?GRANT_ID,
            revoker_user_id => ?DELEGATOR,
            expected_version => 1,
            now => ?NOW
        },
        Over
    ).

cap(C, A, R, Constraint) ->
    #{
        capability => C,
        action => A,
        resource_type => R,
        constraint => Constraint
    }.

cap0() ->
    cap(<<"echo">>, <<"invoke">>, <<"message">>, #{}).

stored_grant(Over) ->
    maps:merge(
        #{
            id => ?GRANT_ID,
            agent_id => ?AGENT,
            organization_id => ?ORG,
            delegator_user_id => ?DELEGATOR,
            workspace_scope_kind => none,
            status => active,
            valid_from => ?VF,
            expires_at => ?EXP,
            revoked_at => undefined,
            revoked_by_user_id => undefined,
            version => 1,
            idempotency_key => <<"k1">>,
            created_at => ?VF,
            updated_at => ?VF
        },
        Over
    ).

caps_row() ->
    [
        #{
            capability => <<"echo">>,
            action => <<"invoke">>,
            resource_type => <<"message">>,
            constraint => #{}
        }
    ].

%% delegator Human + agent=1（身份关全过）
expect_delegator_human() ->
    meck:expect(?PG, get_user_account_type, fun
        (_C, U) when U =:= ?DELEGATOR -> {ok, 0};
        (_C, U) when U =:= ?AGENT -> {ok, 1}
    end),
    ok.

given_identity_ok() ->
    expect_delegator_human().

given_membership_ok() ->
    meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
        {ok, #{status => active, role => member, version => 3}}
    end),
    ok.

given_catalog_entry(LegalKeys) ->
    meck:expect(?CATALOG, lookup, fun(C, A, R) ->
        {ok, #{
            capability => C, action => A, resource_type => R, legal_constraint_keys => LegalKeys
        }}
    end),
    ok.

%% 身份 + membership + 目录三关全过（未做幂等预检桩）
given_preissue_ok() ->
    expect_delegator_human(),
    given_membership_ok(),
    given_catalog_entry([<<"workspace_id">>]),
    ok.

expect_insert_ok() ->
    %% 幂等预检默认未命中（正路径）；幂等专用用例在其后覆盖此期望
    meck:expect(?PG, find_grant_by_idempotency, fun(_C, _O, _D, _K) -> {error, not_found} end),
    meck:expect(?PG, next_id, fun
        (agent_grant) -> ?GRANT_ID;
        (agent_grant_event) -> ?EVENT_ID
    end),
    meck:expect(?PG, insert_grant_tx, fun(_C, _G, _W, _Cap, _E) -> {ok, ?GRANT_ID} end),
    ok.

%% 永不被用作真连接（agent_grant_pg 全 mock）
conn() ->
    self().
