%%% @doc P0-4（v4 02-C.4）：权限矩阵冻结契约套件（纯函数，零 DB）。
%%%
%%% 覆盖 C-5（对矩阵中每个 — 组合做负例）+ fail-closed 规则：
%%%   * 每个 A 格子正例；
%%%   * C 条件格子：scope 满足正例 / scope 缺失或越界负例；
%%%   * 未列出的 (动作, 角色) 组合 ⇒ forbidden（fail-closed 规则 1）；
%%%   * ai_runtime 无人类身份 ⇒ 不能改配置/claim/transfer/purge（规则 2）；
%%%   * 畸形入参 ⇒ forbidden；
%%%   * C-6：cs_session 无 queued 目标子句（死代码删除的静态面）。
-module(cs_ai_permission_matrix_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MOD, cs_ai_app).

permission_matrix_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() -> ok.
cleanup(_State) -> ok.

cases(_State) ->
    [
        fun allowed_cells_test_/0,
        fun scoped_cells_test_/0,
        fun denied_cells_test_/0,
        fun fail_closed_exhaustive_test_/0,
        fun ai_runtime_has_no_human_identity_test_/0,
        {timeout, 30, fun malformed_input_test_/0},
        fun c6_dead_code_removed_test/0
    ].

%% 全动作清单（02-C.4 行）。
actions() ->
    [
        read_config,
        set_mode,
        set_limits,
        set_canary,
        faq_write,
        faq_publish,
        read_sessions,
        read_stats,
        claim,
        ai_deliver,
        purge_trace
    ].

roles() -> [org_admin, workspace_admin, seat, ai_runtime, anonymous, superuser].

%% 允许格（A）期望表：{Action, Role} → ok。
allowed() ->
    [
        {read_config, org_admin},
        {read_config, workspace_admin},
        {read_config, seat},
        {read_config, ai_runtime},
        {read_config, superuser},
        {set_mode, org_admin},
        {set_mode, superuser},
        {set_limits, org_admin},
        {set_limits, superuser},
        {set_canary, org_admin},
        {set_canary, superuser},
        {faq_write, org_admin},
        {faq_write, superuser},
        {faq_publish, org_admin},
        {faq_publish, superuser},
        {read_sessions, org_admin},
        {read_sessions, superuser},
        {read_stats, org_admin},
        {read_stats, superuser},
        {claim, seat},
        {claim, superuser},
        {ai_deliver, ai_runtime}
    ].

%% 条件格（C）：scope 满足 ⇒ ok；缺失 ⇒ {forbidden, _}。
scoped_ok() ->
    [
        {faq_write, workspace_admin, scoped_ws_ctx()},
        {faq_publish, workspace_admin, scoped_ws_ctx()},
        {read_sessions, workspace_admin, scoped_ws_ctx()},
        {read_sessions, seat, scoped_ws_ctx()},
        {read_stats, workspace_admin, scoped_ws_ctx()},
        {claim, org_admin, #{seat_identity_id => 123}},
        {claim, workspace_admin, #{seat_identity_id => 123}},
        {purge_trace, superuser, #{}}
    ].

scoped_ws_ctx() -> #{workspace_id => 10, target_workspace_id => 10}.

%% 显式禁止格（—）期望表：{Action, Role} → forbidden。
denied() ->
    [
        {set_mode, workspace_admin},
        {set_mode, seat},
        {set_mode, ai_runtime},
        {set_mode, anonymous},
        {set_limits, workspace_admin},
        {set_limits, seat},
        {set_limits, ai_runtime},
        {set_limits, anonymous},
        {set_canary, workspace_admin},
        {set_canary, seat},
        {set_canary, ai_runtime},
        {set_canary, anonymous},
        {faq_write, seat},
        {faq_write, ai_runtime},
        {faq_write, anonymous},
        {faq_publish, seat},
        {faq_publish, ai_runtime},
        {faq_publish, anonymous},
        {read_sessions, ai_runtime},
        {read_sessions, anonymous},
        {read_stats, seat},
        {read_stats, ai_runtime},
        {read_stats, anonymous},
        {claim, ai_runtime},
        {claim, anonymous},
        {ai_deliver, org_admin},
        {ai_deliver, workspace_admin},
        {ai_deliver, seat},
        {ai_deliver, anonymous},
        {ai_deliver, superuser},
        {purge_trace, org_admin},
        {purge_trace, workspace_admin},
        {purge_trace, seat},
        {purge_trace, ai_runtime},
        {purge_trace, anonymous}
    ].

allowed_cells_test_() ->
    [
        {lists:flatten(io_lib:format("~p/~p", [A, R])), fun() ->
            ?assertEqual(ok, ?MOD:authorize(A, R, #{}))
        end}
     || {A, R} <- allowed()
    ].

scoped_cells_test_() ->
    {generator, fun() ->
        [
            {lists:flatten(io_lib:format("~p/~p scoped-ok", [A, R])), fun() ->
                ?assertEqual(ok, ?MOD:authorize(A, R, Ctx))
            end}
         || {A, R, Ctx} <- scoped_ok()
        ] ++
            [
                {lists:flatten(io_lib:format("~p/~p scoped-missing", [A, R])), fun() ->
                    ?assertMatch({error, {forbidden, _}}, ?MOD:authorize(A, R, #{}))
                end}
             || {A, R, _} <- scoped_ok(), R =/= superuser
            ] ++
            [
                {lists:flatten(io_lib:format("~p/~p ws-cross", [A, R])), fun() ->
                    Bad = #{workspace_id => 10, target_workspace_id => 99},
                    ?assertMatch(
                        {error, {forbidden, workspace_scope}}, ?MOD:authorize(A, R, Bad)
                    )
                end}
             || {A, R, _} <- scoped_ok(), R =:= workspace_admin, A =/= claim
            ]
    end}.

denied_cells_test_() ->
    [
        {lists:flatten(io_lib:format("~p/~p denied", [A, R])), fun() ->
            ?assertMatch({error, forbidden}, ?MOD:authorize(A, R, #{}))
        end}
     || {A, R} <- denied()
    ].

%% fail-closed 规则 1：全笛卡尔积中，凡不在 allowed ∪ scoped 的组合必须拒绝。
fail_closed_exhaustive_test_() ->
    Known =
        [{A, R} || {A, R} <- allowed()] ++
            [{A, R} || {A, R, _} <- scoped_ok()],
    [
        {lists:flatten(io_lib:format("fail-closed ~p/~p", [A, R])), fun() ->
            case lists:member({A, R}, Known) of
                true ->
                    ok;
                false ->
                    ?assertMatch(
                        {error, _}, ?MOD:authorize(A, R, scoped_ws_ctx())
                    )
            end
        end}
     || A <- actions(), R <- roles()
    ].

%% fail-closed 规则 2：ai_runtime 的全部人类治理动作。
ai_runtime_has_no_human_identity_test_() ->
    HumanActions = [set_mode, set_limits, set_canary, claim, purge_trace],
    [
        {lists:flatten(io_lib:format("ai_runtime/~p", [A])), fun() ->
            ?assertMatch({error, _}, ?MOD:authorize(A, ai_runtime, #{}))
        end}
     || A <- HumanActions
    ].

%% 畸形入参 fail-closed。
malformed_input_test_() ->
    [
        {"unknown action", fun() ->
            ?assertMatch({error, forbidden}, ?MOD:authorize(nonexistent, org_admin, #{}))
        end},
        {"unknown role", fun() ->
            ?assertMatch({error, forbidden}, ?MOD:authorize(read_config, hacker, #{}))
        end},
        {"non-atom action", fun() ->
            ?assertMatch({error, forbidden}, ?MOD:authorize(<<"read_config">>, org_admin, #{}))
        end},
        {"non-map ctx on scoped", fun() ->
            ?assertMatch({error, _}, ?MOD:authorize(faq_write, workspace_admin, bad))
        end}
    ].

%% C-6：cs_session 无 queued 目标子句（死代码已删除——静态面）。
c6_dead_code_removed_test() ->
    {ok, Bin} = file:read_file(path("src/features/customer_service/domain/cs_session.erl")),
    %% 死代码子句（含尾箭头）必须不存在；注释提及不算（带 "->" 代码形状）。
    ?assert(
        binary:match(Bin, <<"apply_transition(Session, _From, queued, Ctx) ->">>) =:= nomatch
    ),
    %% 状态机冻结面不回归：合法边仍在、closed 终态守卫仍在。
    ?assert(binary:match(Bin, <<"valid_transition(queued, active)">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"valid_transition(closed, _To)">>) =/= nomatch).

path(Rel) ->
    filename:join(code:lib_dir(imboy), Rel).
