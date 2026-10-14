%%% @doc P0-6（v4 03-E）：预算 attempt 账本 lifecycle 真库套件。
%%%
%%% 覆盖 04 §P0-6.8：E-1 单次预占 / E-2 并发穿透拒绝 / E-3 结算语义 /
%%% E-4 重复结算不重复计费 / E-5 超预占 23514 / E-6 三层补偿原子 /
%%% E-7 uncertain 不丢额度 + E.6 reconcile（known→settled / unknown→released、
%%% 幂等重跑）。
%%%
%%% 两张预算表（customer_service_ai_budget / _attempt）均无外键 ⇒ 本套件
%%% 不播种 organization/session，纯隔离 scope。
-module(cs_ai_budget_lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_ai_budget_lifecycle_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun e1_single_reserve/0},
        {timeout, 60, fun e2_concurrent_penetration_refused/0},
        {timeout, 60, fun e3_settle_semantics/0},
        {timeout, 60, fun e4_double_settle_idempotent/0},
        {timeout, 60, fun e5_over_reserve_rejected/0},
        {timeout, 60, fun e6_three_layer_compensation_atomic/0},
        {timeout, 60, fun e7_uncertain_keeps_quota/0},
        {timeout, 60, fun reconcile_known_and_unknown/0}
    ];
cases({error, Reason}) ->
    erlang:error({csai_e_group_suite_db_unavailable, Reason}).

%% ===================================================================
%% E-1：单次预占成功（复现 E1）
%% ===================================================================

e1_single_reserve() ->
    Scene = new_scope(),
    try
        #{org := Org, session := S, job := J} = Scene,
        ok = seed_budget(Scene, 1000),
        {ok, reserved} = cs_ai_app:admit(Org, #{
            session_id => S, job_id => J, attempt => 1, est_tokens => 500
        }),
        %% 三层各一条 attempt（冻结 attempt_id 规则 '<job>:<attempt>:<layer>'），
        %% 三层预算行各预占 500。
        {ok, reserved} = attempt_state(Org, attempt_id(J, 1, session)),
        {ok, reserved} = attempt_state(Org, attempt_id(J, 1, org)),
        {ok, reserved} = attempt_state(Org, attempt_id(J, 1, global)),
        500 = budget_tokens(Org, session, S, reserved),
        500 = budget_tokens(Org, org, Org, reserved),
        500 = budget_tokens(Org, global, 0, reserved)
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% E-2：并发穿透被拒（复现 E2，影响 0 行；I7 任意时刻成立）
%% ===================================================================

e2_concurrent_penetration_refused() ->
    Scene = new_scope(),
    try
        #{org := Org, session := S, job := J} = Scene,
        ok = seed_budget(Scene, 1000),
        Self = self(),
        Spawn =
            fun(N) ->
                spawn_link(fun() ->
                    R =
                        cs_ai_app:admit(Org, #{
                            session_id => S, job_id => J + N, attempt => 1, est_tokens => 800
                        }),
                    Self ! {done, N, R}
                end)
            end,
        Spawn(1),
        Spawn(2),
        Results = [
            receive
                {done, N1, R1} -> {N1, R1}
            after 20000 -> timeout
            end
         || _ <- [a, b]
        ],
        Oks = [R || {_N, {ok, _}} = R <- Results],
        Refused = [R || {_N, {error, {budget_exceeded, _}}} = R <- Results],
        %% limit=1000、est=800：恰好一个成功一个被拒，与先后顺序无关。
        1 = length(Oks),
        1 = length(Refused),
        800 = budget_tokens(Org, session, S, reserved)
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% E-3：结算语义（复现 E3：reserved=0, used=520）
%% ===================================================================

e3_settle_semantics() ->
    Scene = new_scope(),
    try
        #{org := Org, session := S, job := J} = Scene,
        ok = seed_budget(Scene, 1000),
        {ok, reserved} = cs_ai_app:admit(Org, #{
            session_id => S, job_id => J, attempt => 1, est_tokens => 520
        }),
        %% 每层各自结算（账本每层一条）：各层 reserved=0、used=520。
        {ok, settled} = cs_ai_app:settle(Org, attempt_id(J, 1, session), 520),
        {ok, settled} = cs_ai_app:settle(Org, attempt_id(J, 1, org), 520),
        {ok, settled} = cs_ai_app:settle(Org, attempt_id(J, 1, global), 520),
        0 = budget_tokens(Org, session, S, reserved),
        520 = budget_tokens(Org, session, S, used)
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% E-4：重复结算不重复计费（复现 E4：第二次影响 0 行，used 不变）
%% ===================================================================

e4_double_settle_idempotent() ->
    Scene = new_scope(),
    try
        #{org := Org, session := S, job := J} = Scene,
        ok = seed_budget(Scene, 1000),
        {ok, reserved} = cs_ai_app:admit(Org, #{
            session_id => S, job_id => J, attempt => 1, est_tokens => 520
        }),
        {ok, settled} = cs_ai_app:settle(Org, attempt_id(J, 1, session), 520),
        520 = budget_tokens(Org, session, S, used),
        {skipped, already_settled} = cs_ai_app:settle(Org, attempt_id(J, 1, session), 520),
        520 = budget_tokens(Org, session, S, used),
        0 = budget_tokens(Org, session, S, reserved),
        ok
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% E-5：超预占结算被拒（复现 E5：23514，budget 不变）
%% ===================================================================

e5_over_reserve_rejected() ->
    Scene = new_scope(),
    try
        #{org := Org, session := S, job := J} = Scene,
        ok = seed_budget(Scene, 1000),
        {ok, reserved} = cs_ai_app:admit(Org, #{
            session_id => S, job_id => J, attempt => 1, est_tokens => 100
        }),
        Result = cs_ai_app:settle(Org, attempt_id(J, 1, session), 200),
        %% ck_csaiba_settled（23514）拦截 actual > reserved，整事务回滚。
        ?assert(string:find(io_lib:format("~p", [Result]), "23514") =/= nomatch),
        100 = budget_tokens(Org, session, S, reserved),
        0 = budget_tokens(Org, session, S, used)
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% E-6：三层补偿原子（global 拒 ⇒ session/org reserved 回原值）
%% ===================================================================

e6_three_layer_compensation_atomic() ->
    Scene = new_scope(),
    try
        #{org := Org, session := S, job := J} = Scene,
        ok = seed_budget(Scene, 1000),
        %% global 层限额清零 ⇒ 第三层拒绝，前两层同事务补偿。
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_ai_budget SET limit_tokens=0"
                " WHERE organization_id=$1 AND scope_kind='global'"
            >>,
            [Org]
        ),
        {error, {budget_exceeded, global}} = cs_ai_app:admit(Org, #{
            session_id => S, job_id => J, attempt => 1, est_tokens => 300
        }),
        0 = budget_tokens(Org, session, S, reserved),
        0 = budget_tokens(Org, org, Org, reserved),
        0 = budget_tokens(Org, global, 0, reserved),
        %% 补偿层 attempt 落 released（终态），未补偿层不存在。
        {ok, released} = attempt_state(Org, attempt_id(J, 1, session)),
        {ok, released} = attempt_state(Org, attempt_id(J, 1, org)),
        %% 拒绝层自身也落 released（实现比合同伪代码多清理一步，见 state/P0-6）。
        {ok, released} = attempt_state(Org, attempt_id(J, 1, global))
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% E-7：uncertain 不丢额度（reserved + used ≤ limit 仍成立）
%% ===================================================================

e7_uncertain_keeps_quota() ->
    Scene = new_scope(),
    try
        #{org := Org, session := S, job := J} = Scene,
        ok = seed_budget(Scene, 1000),
        {ok, reserved} = cs_ai_app:admit(Org, #{
            session_id => S, job_id => J, attempt => 1, est_tokens => 400
        }),
        {ok, uncertain} = cs_ai_app:to_uncertain(Org, attempt_id(J, 1, session)),
        400 = budget_tokens(Org, session, S, reserved),
        0 = budget_tokens(Org, session, S, used),
        %% I7 不变量在 uncertain 期间继续成立。
        400 = budget_tokens(Org, session, S, reserved) + budget_tokens(Org, session, S, used)
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% E.6 reconcile：known → settled / unknown → released；重跑幂等
%% ===================================================================

reconcile_known_and_unknown() ->
    Scene = new_scope(),
    try
        #{org := Org, session := S, job := J} = Scene,
        ok = seed_budget(Scene, 1000),
        {ok, reserved} = cs_ai_app:admit(Org, #{
            session_id => S, job_id => J, attempt => 1, est_tokens => 400
        }),
        {ok, uncertain} = cs_ai_app:to_uncertain(Org, attempt_id(J, 1, session)),
        {ok, uncertain} = cs_ai_app:to_uncertain(Org, attempt_id(J, 1, org)),
        {ok, uncertain} = cs_ai_app:to_uncertain(Org, attempt_id(J, 1, global)),
        %% E.6 冻结只收 10 分钟前的到期行 ⇒ 测试把行回拨到期。
        ok = backdate_attempts(Org),
        Lookup = fun(Att) ->
            case maps:get(<<"layer_kind">>, Att) of
                <<"session">> -> {ok, 250};
                _ -> unknown
            end
        end,
        {ok, Results} = cs_pg_ai:budget_reconcile(Lookup),
        {ok, settled} = proplists:get_value(attempt_id(J, 1, session), Results),
        {ok, released} = proplists:get_value(attempt_id(J, 1, org), Results),
        {ok, released} = proplists:get_value(attempt_id(J, 1, global), Results),
        %% known：reserved 400→0、used=250；unknown：reserved 400→0、used=0。
        0 = budget_tokens(Org, session, S, reserved),
        250 = budget_tokens(Org, session, S, used),
        0 = budget_tokens(Org, org, Org, reserved),
        0 = budget_tokens(Org, org, Org, used),
        %% 重跑：无可收口行（CAS 幂等）。
        {ok, []} = cs_pg_ai:budget_reconcile(Lookup)
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

%% budget 表带 organization FK（fk_csaib_organization RESTRICT）⇒ 复用
%% fixture 的完整 scope 播种（org/identity 等），预算表自身数据用后即清。
new_scope() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Session = ?FIX:id(),
    #{scope => Scope, org => Org, session => Session, job => ?FIX:id()}.

attempt_id(Job, Attempt, Layer) ->
    iolist_to_binary(
        io_lib:format("~B:~B:~s", [Job, Attempt, atom_to_binary(Layer)])
    ).

seed_budget(#{org := Org, session := S}, Limit) ->
    Period = cs_pg_ai:budget_current_period(),
    lists:foreach(
        fun({Kind, ScopeId}) ->
            ok = ?FIX:exec(
                <<
                    "INSERT INTO customer_service_ai_budget"
                    " (organization_id, period, scope_kind, scope_id, limit_tokens)"
                    " VALUES ($1,$2,$3,$4,$5)"
                    " ON CONFLICT (organization_id, period, scope_kind, scope_id) DO UPDATE SET"
                    "  limit_tokens = $5, reserved_tokens = 0, used_tokens = 0, version = 1"
                >>,
                [Org, Period, atom_to_binary(Kind), ScopeId, Limit]
            )
        end,
        [{session, S}, {org, Org}, {global, 0}]
    ),
    ok.

budget_tokens(Org, Kind, ScopeId, Which) ->
    Column =
        case Which of
            reserved -> <<"reserved_tokens">>;
            used -> <<"used_tokens">>
        end,
    Period = cs_pg_ai:budget_current_period(),
    ?FIX:scalar(
        <<
            "SELECT ",
            Column/binary,
            " FROM customer_service_ai_budget"
            " WHERE organization_id=$1 AND period=$2 AND scope_kind=$3 AND scope_id=$4"
        >>,
        [Org, Period, atom_to_binary(Kind), ScopeId]
    ).

attempt_state(Org, AttemptId) ->
    case
        ?FIX:scalar(
            <<
                "SELECT state FROM customer_service_ai_budget_attempt"
                " WHERE attempt_id=$1 AND organization_id=$2"
            >>,
            [AttemptId, Org]
        )
    of
        undefined -> {error, not_found};
        <<"reserved">> -> {ok, reserved};
        <<"settled">> -> {ok, settled};
        <<"released">> -> {ok, released};
        <<"uncertain">> -> {ok, uncertain};
        Other -> {error, {unknown_state, Other}}
    end.

backdate_attempts(Org) ->
    ?FIX:exec(
        <<
            "UPDATE customer_service_ai_budget_attempt"
            "   SET created_at = now() - interval '11 minutes'"
            " WHERE organization_id=$1"
        >>,
        [Org]
    ).

cleanup_scene(#{scope := Scope, org := Org}) ->
    lists:foreach(
        fun(Table) ->
            _ = ?FIX:exec(
                <<"DELETE FROM ", Table/binary, " WHERE organization_id=$1">>, [Org]
            )
        end,
        [<<"customer_service_ai_budget_attempt">>, <<"customer_service_ai_budget">>]
    ),
    ?FIX:cleanup(Scope),
    ok.
