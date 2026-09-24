-module(agent_preflight_facts_pg_tests).

%% Agent 域 DeletionPreflightFacts provider 真库测试（ORG-06 Agent track 交付 /
%% C17 / 计划 §1.6；2026-09-18 用户拍板登记，见
%% control/ruling-agent-provider-defer.md 顶部注记）。
%%
%% 覆盖（用户八条正负例中的真库部分）：
%%   ① subject 名下活跃 bot / ai_agent → AGENT_OWNER_ACTIVE blocker
%%      （冻结四字段形状：code/resource_type/resource_id(opaque)/
%%      organization_id 恒 null；bot 不挂 org）；status 语义：
%%      bot 1=active / 0=disabled / -1=deleted，ai_agent 1=启用 / 0=停用，
%%      仅 status=1 构成 blocker；
%%   ② subject 无任何 bot/ai_agent → 合法空 blockers（fact_version 下限 1）；
%%   ③ DB 不可达（错误端口池）→ {error, unavailable}（编排器归口
%%      DEPENDENCY_FACTS_UNAVAILABLE 整体拒绝，不做本地兜底）。
%%
%% 库供给（inttest_marker_db 一次性 marker 库配方）：目标服务器解析优先级
%% 环境变量 AGENT_PF_INTTEST_PG_HOST/_PG_PORT/_PG_USER/_PG_PASSWORD
%% （/ _MAINT_DB）> eunit VM 配置（-config 装载的 imboy.pg_conf 连接段）；
%% 建库 → 12 扩展 → erlang_migrate:up 全链（strict）→ 测试 → DROP DATABASE。
%% pooler `pgsql` 池直指 marker 库（本套件不启动 imboy app，
%% trust_audit_repo_integration_tests 同款先例）；供给任一步失败显式
%% error，无静默 skip。
%%
%% 全量共存（连接注入模式）：provider 只经 elib_pg:query/2 读库，而 elib_pg
%% 的池名恒为 pgsql——共享 VM 里该池归常驻 imboy app 所有。本套件**不碰共享
%% 池**（不 rm_pool / 不 new_pool，也不换 app 池指向）：改为在套件级把
%% elib_pg:query/2 期望为「转发到本套件 marker 库连接」，转发体调用
%% elib_pg:query/3 的原实现（meck 备份模块，生产同款代码含 rows_to_maps），
%% 故被测 provider 的 SQL 仍由真 PG 执行、语义不变；夹具写入同样落 marker 库
%% （随库一起丢弃）。
%%
%% 为什么不像旧版那样换池：换池（rm_pool(pgsql) + 同名 new_pool）会在共享 VM
%% 里制造两类全量事故（ent-org-v21-closure gate5 实证）：①换池窗口内 app 常驻
%% worker 的读写落到 marker 库、且 marker 库随后被 DROP；②在途 pooler_starter
%% 按池名回报 accept_member 时池已不存在 ——「no such process or port in call
%% to gen_server:call(pgsql, ...)」连环崩，拖挂后续套件。连接注入无窗口、无
%% 全局副作用，且后台 worker 因 meck_helper 常驻调用者护栏自动走原池（真实
%% 主库），互不干扰。
%%
%% 运行：make eunit-local t=agent_preflight_facts_pg_tests（单跑同口径）

-include_lib("eunit/include/eunit.hrl").

%% 夹具独立 ID 段（97 段，不与既有套件夹具冲突）
-define(OWNER_A, 970001).
-define(BOT_USER_A, 970002).
-define(AGENT_USER_A, 970003).
-define(OWNER_B, 970011).

agent_preflight_facts_pg_test_() ->
    {timeout, 900, {setup, fun setup_conn/0, fun close_conn/1, fun cases/1}}.

setup_conn() ->
    State =
        inttest_marker_db:provision(#{
            env_prefix => <<"AGENT_PF_INTTEST">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }),
    ok = install_marker_query(State),
    State.

close_conn(State) ->
    meck_helper:cleanup_mock(elib_pg),
    inttest_marker_db:release(State),
    ok.

%% 套件级连接注入：elib_pg:query/2 → 本套件 marker 库连接。
%% 经 meck_helper 安装（享受常驻调用者护栏）：app 常驻 worker 走原池（主库），
%% 测试侧（含 provider 读与夹具写）走 marker 库，两侧互不污染。
%%
%% 转发体刻意调用 **meck 备份的原模块**（meck:new 无条件备份 <mod>_meck_original，
%% 见 deps/meck/src/meck_proc.erl backup_original/4），而不是 elib_pg:query/3：
%% 后者会再次进入 mock 模块，mock 未带 passthrough 选项（setup_mock 的兜底策略）
%% 时 /3 无期望即 undef。原模块引用与 mock 选项无关，确定性成立。
install_marker_query(#{conn := Conn}) ->
    %% meck 备份的原模块名（meck_util:original_name/1 约定）；安装时解析一次，
    %% 随闭包捕获。
    Original = list_to_atom(atom_to_list(elib_pg) ++ "_meck_original"),
    {ok, elib_pg} = meck_helper:setup_mock(elib_pg, [
        {'query', 2, fun(Sql, Params) -> Original:query(Conn, Sql, Params) end}
    ]),
    ok.

cases(State) ->
    [
        {"active bot/ai_agent → AGENT_OWNER_ACTIVE（冻结形状 + status 语义）",
            {timeout, 90, fun() -> active_resources_report_agent_owner_active(State) end}},
        {"无 agent 资源 → 合法空 blockers",
            {timeout, 60, fun() -> no_agent_resources_empty_blockers(State) end}},
        {"DB 不可达（错误端口池）→ {error, unavailable}",
            {timeout, 90, fun() -> unreachable_db_returns_unavailable(State) end}}
    ].

%% ===================================================================
%% ① 活跃 bot / ai_agent
%% ===================================================================

active_resources_report_agent_owner_active(_State) ->
    cleanup_users([?OWNER_A, ?BOT_USER_A, ?AGENT_USER_A]),
    try
        lists:foreach(fun create_user/1, [?OWNER_A, ?BOT_USER_A, ?AGENT_USER_A]),
        {ok, _} = elib_pg:query(
            <<
                "INSERT INTO public.bot (user_id, name, owner_uid)"
                " VALUES ($1, 'pf-bot', $2)"
            >>,
            [?BOT_USER_A, ?OWNER_A]
        ),
        {ok, _} = elib_pg:query(
            <<
                "INSERT INTO public.ai_agent (user_id, provider, owner_uid)"
                " VALUES ($1, 'openai', $2)"
            >>,
            [?AGENT_USER_A, ?OWNER_A]
        ),
        %% --- 活跃 bot + 活跃 ai_agent → 两枚同码 blocker，形状逐字段冻结 ---
        {ok, Fact} = agent_preflight_facts:facts_agent(?OWNER_A),
        ?assertEqual(?OWNER_A, maps:get(subject_user_id, Fact)),
        ?assertEqual(agent, maps:get(domain, Fact)),
        ?assert(is_integer(maps:get(observed_at, Fact))),
        ?assert(maps:get(fact_version, Fact) >= 1),
        Blockers = maps:get(blockers, Fact),
        ?assertEqual(2, length(Blockers)),
        Types = [maps:get(resource_type, B) || B <- Blockers],
        ?assertEqual([<<"ai_agent">>, <<"bot">>], lists:sort(Types)),
        lists:foreach(
            fun(B) ->
                %% 冻结四字段 + opaque resource_id + org 恒 null
                ?assertEqual(<<"AGENT_OWNER_ACTIVE">>, maps:get(code, B)),
                ?assert(is_binary(maps:get(resource_id, B))),
                ?assertEqual(null, maps:get(organization_id, B))
            end,
            Blockers
        ),
        Rids = [maps:get(resource_id, B) || B <- Blockers],
        ?assert(lists:member(integer_to_binary(?BOT_USER_A), Rids)),
        ?assert(lists:member(integer_to_binary(?AGENT_USER_A), Rids)),
        %% --- bot disabled（0）→ 仅剩 ai_agent blocker ---
        {ok, _} = elib_pg:query(
            <<"UPDATE public.bot SET status = 0 WHERE user_id = $1">>, [?BOT_USER_A]
        ),
        {ok, FactNoBot} = agent_preflight_facts:facts_agent(?OWNER_A),
        [OnlyAgent] = maps:get(blockers, FactNoBot),
        ?assertEqual(<<"ai_agent">>, maps:get(resource_type, OnlyAgent)),
        %% --- bot deleted（-1）、ai_agent 停用（0）→ 空 blocker ---
        {ok, _} = elib_pg:query(
            <<"UPDATE public.bot SET status = -1 WHERE user_id = $1">>, [?BOT_USER_A]
        ),
        {ok, _} = elib_pg:query(
            <<"UPDATE public.ai_agent SET status = 0 WHERE user_id = $1">>, [?AGENT_USER_A]
        ),
        {ok, FactCleared} = agent_preflight_facts:facts_agent(?OWNER_A),
        ?assertEqual([], maps:get(blockers, FactCleared))
    after
        cleanup_users([?OWNER_A, ?BOT_USER_A, ?AGENT_USER_A])
    end.

%% ===================================================================
%% ② 无 agent 资源
%% ===================================================================

no_agent_resources_empty_blockers(_State) ->
    cleanup_users([?OWNER_B]),
    try
        %% 从未创建 bot/ai_agent 的用户（用户行存在与否不影响结论）
        {ok, Fact} = agent_preflight_facts:facts_agent(?OWNER_B),
        ?assertEqual(?OWNER_B, maps:get(subject_user_id, Fact)),
        ?assertEqual(agent, maps:get(domain, Fact)),
        ?assertEqual([], maps:get(blockers, Fact)),
        %% 空集 fact_version 取单调投影下限 1（编排器校验 >= 1）
        ?assertEqual(1, maps:get(fact_version, Fact)),
        %% 非法 subject 参数 → 与 DB 不可得同形（{error, unavailable}）
        {error, unavailable} = agent_preflight_facts:facts_agent(<<"bad">>)
    after
        cleanup_users([?OWNER_B])
    end.

%% ===================================================================
%% ③ DB 不可达 → {error, unavailable}（不吞不兜底）
%% ===================================================================

unreachable_db_returns_unavailable(#{conn := Conn}) ->
    %% 故障注入点选在 elib_pg:query/2（provider 唯一读库入口）：把「连接层
    %% 错误」注入为 {error, econnrefused}，provider 必须原样归口
    %% {error, unavailable}（编排器 DEPENDENCY_FACTS_UNAVAILABLE 整体拒绝，
    %% 不做本地兜底/不返回空 blockers）。注入期间不触碰共享 pgsql 池。
    {ok, _} = meck_helper:setup_mock(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {error, econnrefused} end}
    ]),
    try
        {error, unavailable} = agent_preflight_facts:facts_agent(?OWNER_A)
    after
        %% 恢复 marker 库转发并复验可读（套件自身状态不受污染）
        ok = install_marker_query(#{conn => Conn})
    end,
    {ok, _} = agent_preflight_facts:facts_agent(?OWNER_A),
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

create_user(Uid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
            " VALUES ($1, $2, 'x', '127.0.0.1', '')"
        >>,
        [Uid, <<"apf", (integer_to_binary(Uid))/binary>>]
    ),
    ok.

cleanup_users(Uids) ->
    %% bot/ai_agent 随 user 行 ON DELETE CASCADE（00000070/00000027）
    lists:foreach(
        fun(Uid) ->
            _ = elib_pg:query(<<"DELETE FROM public.\"user\" WHERE id = $1">>, [Uid])
        end,
        Uids
    ),
    ok.
