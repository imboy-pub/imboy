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
%% 运行：make eunit-local t=agent_preflight_facts_pg_tests

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
    {ok, _} = application:ensure_all_started(pooler),
    Good = pool_conf(maps:get(server, State), maps:get(db, State)),
    case pooler:new_pool(Good) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _}} ->
            %% 共享 VM 里 pgsql 池已指向他库：本套件必须单跑（loudly fail，
            %% 不静默复用他库连接）
            erlang:error({agent_pf_pg_pool_conflict, 'pgsql'})
    end,
    State#{good_pool_conf => Good}.

close_conn(#{good_pool_conf := _Good} = State) ->
    _ = pooler:rm_pool(pgsql),
    inttest_marker_db:release(State),
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
%% ③ DB 不可达（错误端口池）
%% ===================================================================

unreachable_db_returns_unavailable(#{good_pool_conf := Good}) ->
    %% 换入错误端口池（127.0.0.1:1），provider 须 {error, unavailable}，
    %% 不吞不兜底；随后恢复好池并复验可读。
    Bad = Good#{start_mfa => bad_port_start_mfa(maps:get(start_mfa, Good))},
    ok = pooler:rm_pool(pgsql),
    {ok, _} = pooler:new_pool(Bad),
    try
        {error, unavailable} = agent_preflight_facts:facts_agent(?OWNER_A)
    after
        ok = pooler:rm_pool(pgsql),
        {ok, _} = pooler:new_pool(Good)
    end,
    %% 恢复后 provider 恢复可读（套件自身状态不受污染）
    {ok, _} = agent_preflight_facts:facts_agent(?OWNER_A),
    ok.

bad_port_start_mfa({epgsql, connect, [Opts]}) ->
    {epgsql, connect, [Opts#{host => "127.0.0.1", port => 1, timeout => 500}]}.

%% ===================================================================
%% Internal
%% ===================================================================

pool_conf(#{host := Host, port := Port, username := User, password := Pass}, Db) ->
    #{
        name => pgsql,
        max_count => 10,
        init_count => 1,
        start_mfa =>
            {epgsql, connect, [
                #{
                    host => Host,
                    username => User,
                    password => Pass,
                    database => Db,
                    port => Port,
                    ssl => false,
                    timeout => 10000,
                    codecs => [{epgsql_codec_rfc3339_bin, []}]
                }
            ]}
    }.

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
