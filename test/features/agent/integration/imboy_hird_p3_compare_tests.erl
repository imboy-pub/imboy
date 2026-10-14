%% @doc IMBOY_HIRD_AUTH_REPLAY_POC_V1 — P3：同一业务 Tool、同一授权器、
%% 同一组 fixture、同样输入与预期结果下，两条编排路径的最小对照：
%%
%%   A：现有 Erlang 原生编排路径（authorize/3 → 业务 handler 直调）。
%%   B：Hirð 编译程序编排路径（P1 注册表桥全链）。
%%
%% 对照不建通用框架、不为支持全部能力重写 Erlang 路径；差异记录见
%% docs/architecture/hird-poc/IMBOY_HIRD_AUTH_REPLAY_POC_V1.md §4 对照表。
-module(imboy_hird_p3_compare_tests).

-include_lib("eunit/include/eunit.hrl").

-define(P1, imboy_hird_p1_auth_tool_tests).
-define(OUT_ROOT,
    unicode:characters_to_binary(
        os:getenv("IMBOY_HIRD_OUT_ROOT", "_build/hird_poc_v1/prog")
    )
).

%%%===================================================================
%%% 套件
%%%===================================================================

p3_test_() ->
    {timeout, 900,
        {foreach, fun setup/0, fun cleanup/1, [
            {~B'路径 A（Erlang 原生）：authorize+业务直调，allow 结果与 B 等价', fun t_path_a_allow/0},
            {~B'路径 A 无授权：deny 语义与 B 等价，业务执行零次', fun t_path_a_deny/0},
            {~B'路径 A/B 结果与授权事实等价性（同输入同 fixture）', fun t_equivalence/0}
        ]}}.

setup() ->
    ?P1:ensure_env(),
    ?P1:ensure_tsid(),
    ?P1:ensure_built(),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"HIRDPOC">>,
        reuse => true
    }),
    ok = imboy_hird_poc_membership:install(maps:get(conn, State)),
    ok = ?P1:install_poc_env(),
    Counters = #{
        biz => atomics:new(1, [{signed, false}]),
        provider => atomics:new(1, [{signed, false}])
    },
    ok = imboy_hird_poc_registry:install(#{
        conn => maps:get(conn, State),
        biz_counter => maps:get(biz, Counters),
        provider_counter => maps:get(provider, Counters),
        trap => false
    }),
    persistent_term:put({imboy_hird_p3, state}, State#{counters => Counters}),
    ok.

state() ->
    persistent_term:get({imboy_hird_p3, state}).

cleanup(_) ->
    State = persistent_term:get({imboy_hird_p3, state}, undefined),
    imboy_hird_poc_membership:teardown(),
    imboy_hird_poc_registry:teardown(),
    ?P1:uninstall_poc_env(),
    ?P1:stop_supervisor(),
    ?P1:stop_audit(),
    persistent_term:erase({imboy_hird_p3, state}),
    case State of
        undefined -> ok;
        _ -> inttest_marker_db:release(State)
    end.

biz_calls() -> imboy_hird_poc_registry:biz_calls().

%%%===================================================================
%%% 路径 A：Erlang 原生编排（无 hird 参与）
%%%===================================================================

%% 原生路径 = 授权咽喉 → 注册表查 handler → 业务执行 → sanitized。
%% 与 agent_native_tool_adapter:invoke/3 同构（但显式传递 RunCtx 使
%% effect 凭证可签发——adapter 的 registry 合同单参 Handler 不含
%% 授权上下文，PoC 以同构内联保持口径一致）。
path_a_invoke(RunCtx, Resource) ->
    case agent_tool_authorizer:authorize(RunCtx, ?P1:tool_desc(), Resource) of
        {deny, Reason} ->
            {deny, Reason};
        {approval_required, _} = A ->
            A;
        {allow, Decision} ->
            EffectId = maps:get(effect_id, Decision, undefined),
            imboy_hird_poc_registry:set_effect({EffectId, maps:get(run_id, RunCtx)}),
            {ok, Handler} = imboy_hird_poc_registry:lookup(<<"tool.workspace.detail">>),
            Raw =
                case Handler(Resource) of
                    {ok, Raw0} -> {ok, Raw0};
                    {error, R} -> {error, R}
                end,
            imboy_hird_poc_registry:clear_effect(),
            case Raw of
                {ok, Bin} -> {ok, #{result => Bin, effect_id => EffectId}};
                {error, R2} -> {error, R2}
            end
    end.

%%%===================================================================
%%% 用例
%%%===================================================================

t_path_a_allow() ->
    State = state(),
    Conn = maps:get(conn, State),
    ok = ?P1:given_full_fixture(Conn),
    RunCtx = ?P1:run_ctx(Conn, 914001, 911001),
    Resource = (?P1:resource_of(Conn))(#{ws_id => 912001}),
    ?assertMatch(
        {ok, #{result := <<"{\"id\":912001", _/binary>>}},
        path_a_invoke(RunCtx, Resource)
    ),
    ?assertEqual(1, biz_calls()).

t_path_a_deny() ->
    State = state(),
    Conn = maps:get(conn, State),
    ok = ?P1:given_full_fixture(Conn),
    %% 撤销 Grant → 实时重读拒（与 B 路径同授权器同语义）。
    Event = #{
        id => 916003,
        grant_id => 913001,
        event_type => revoked,
        actor_kind => human,
        actor_user_id => 910002,
        revoked_by_user_id => 910002,
        from_version => 1,
        to_version => 2,
        detail => #{},
        idempotency_key => <<"poc-revoke-p3">>,
        now => {{2026, 10, 13}, {6, 0, 0}}
    },
    {ok, 2} = agent_grant_pg:revoke_cas_tx(
        Conn,
        911001,
        913001,
        1,
        {{2026, 10, 13}, {6, 0, 0}},
        Event
    ),
    RunCtx = ?P1:run_ctx(Conn, 914002, 911001),
    ok = insert_extra_run(Conn),
    Resource = (?P1:resource_of(Conn))(#{ws_id => 912001}),
    ?assertEqual({deny, grant_revoked}, path_a_invoke(RunCtx, Resource)),
    ?assertEqual(0, biz_calls()).

t_equivalence() ->
    State = state(),
    Conn = maps:get(conn, State),
    ok = ?P1:given_full_fixture(Conn),
    %% A 路径结果。
    RunCtxA = ?P1:run_ctx(Conn, 914001, 911001),
    Resource = (?P1:resource_of(Conn))(#{ws_id => 912001}),
    {ok, #{result := RawA}} = path_a_invoke(RunCtxA, Resource),
    CallsA = biz_calls(),
    %% B 路径结果（重置计数后经 P1 全链跑一次；同 Run 同 fixture）。
    Counters = maps:get(counters, State),
    atomics:put(maps:get(biz, Counters), 1, 0),
    {_Outcome, File} = ?P1:run_program(state(), RunCtxA, #{}),
    [_, WsRec] = ?P1:read_audit(File),
    #{<<"ok">> := RawB} = maps:get(<<"result">>, WsRec),
    %% 同输入同授权事实下，业务返回值逐字节一致；授权均恰发生一次。
    ?assertEqual(RawA, RawB),
    %% B 路径业务执行恰一次（B 段计数从 0 重置后）。
    ?assertEqual(1, biz_calls()),
    %% A 路径的授权决策同样落库（agent_effect authorized ≥1）。
    {ok, _, [{N}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM agent_effect WHERE run_id = $1 AND status = 'authorized'",
        [914001]
    ),
    ?assert(N >= 1).

insert_extra_run(Conn) ->
    case
        epgsql:equery(
            Conn,
            "INSERT INTO agent_run (id, agent_id, organization_id, workspace_id, grant_id, "
            "grant_version_at_start, delegating_principal_id, trigger_type, trigger_id, "
            "runtime_type, status, context_digest, idempotency_key, started_at) "
            "VALUES ($1, $2, $3, NULL, $4, 1, $5, 'message', $6, 'native', "
            "'running', 'sha256:poc-ctx', $7, $8) ON CONFLICT DO NOTHING",
            [
                914002,
                910001,
                911001,
                913001,
                910002,
                <<"poc-trg-p3">>,
                <<"poc-run-idem-p3">>,
                {{2026, 10, 13}, {6, 0, 0}}
            ]
        )
    of
        {ok, _} -> ok;
        {ok, _, _} -> ok
    end.
