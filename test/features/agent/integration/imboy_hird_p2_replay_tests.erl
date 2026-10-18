%% @doc IMBOY_HIRD_AUTH_REPLAY_POC_V1 — P2：真正的有限顺序轨迹离线 Replay。
%%
%% 录制 = P1 授权链真实执行（合法链含真实业务读取；deny 链含域错误）+
%% hird_audit:sync/0 显式落盘校验；回放 = hird_replay cursor
%% （start_link/2 严格顺序匹配；active 期间一切 dispatch 路由到 cursor，
%% handler/注册表/授权/业务全不执行）+ 程序重跑 + finish/0 全消费校验。
%%
%% 「调用即失败」断言：model_chat 注册 trap handler（live 误走即 error），
%% ws_detail 注册表不装（live 误走 → unhandled_tool crash）——replay 正确时
%% 两者零触发；对业务侧再以 atomics 计数恒不增长兜底（覆盖任意派生 worker）。
%%
%% 对比：jsone 逐行结构化解析；归一化仅裁 timestamp（schema_version 保留）；
%% 调用事件保持顺序、不排序数组、不忽略参数、不忽略多余尾部。
%%
%% 本套件修正历史口径：G16/G17/A18 的「重复执行一致性」不是 replay
%% （见 imboy_hird_replay moduledoc 更新与本文件各负向用例）。
%%
%% C2 CLOSEOUT：回放编排装填宿主回放护栏（cursor 存活期 arm、出口 disarm
%% ——cursor 死亡后 dispatch 回退 live 时桥在任何资源解析/授权前拒绝）。
%% 调用差异（运行时工具名变体/次序漂移/schema 版本门/cursor 运行中死亡）
%% 的端到端用例见 imboy_hird_replay_offline_tests（离线构造类型化日志，
%% 不依赖 PG——语义与日志来源无关）。
-module(imboy_hird_p2_replay_tests).

-include_lib("eunit/include/eunit.hrl").

-define(P1, imboy_hird_p1_auth_tool_tests).
-define(MOD, (?P1:mod())).
-define(OUT_ROOT,
    unicode:characters_to_binary(
        os:getenv("IMBOY_HIRD_OUT_ROOT", "_build/hird_poc_v1/prog")
    )
).
-define(TRAP_HANDLER, fun imboy_hird_poc_registry:model_handler/2).
-define(STATE_KEY, {imboy_hird_p2, state}).

%%%===================================================================
%%% 套件
%%%===================================================================

p2_test_() ->
    {timeout, 900,
        {foreach, fun setup/0, fun cleanup/1, [
            {timeout, 120, {~B'正常轨迹回放成功：cursor 全消费、真实执行尝试为零、audit 结构一致', fun t_replay_ok/0}},
            {timeout, 120, {~B'程序调用参数偏移：args_mismatch 失败，游标不前移', fun t_args_mismatch/0}},
            {timeout, 120, {~B'工具名偏移致签名不兼容：加载期 replay_load_error', fun t_tool_mismatch/0}},
            {timeout, 120, {~B'日志不足（删末行）：log_exhausted 失败', fun t_log_exhausted/0}},
            {timeout, 120, {~B'多余调用记录：finish 报 replay_incomplete（不静默截尾）', fun t_extra_records/0}},
            {timeout, 120, {~B'日志格式损坏：start_link 显式 replay_load_error（带行号）', fun t_corrupt_line/0}},
            {timeout, 120,
                {~B'已记录领域错误按原语义回放：{err} 以 {hird_exn} throw，两轮一致', fun t_err_semantics/0}},
            {timeout, 120, {~B'cursor 无法启动：编排中止，不退回真实执行', fun t_start_failure/0}}
        ]}}.

%%%===================================================================
%%% setup / cleanup（与 P1 同构：marker 库 reuse 共享、PoC env、registry）
%%%===================================================================

setup() ->
    ?P1:setup_with_owned_env(fun setup_fixture/0, fun cleanup_fixture/0).

setup_fixture() ->
    ?P1:ensure_env(),
    ?P1:ensure_tsid(),
    ?P1:ensure_built(),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"HIRDPOC">>,
        reuse => true
    }),
    %% Own the provisioned resource before any later setup step can fail.
    persistent_term:put(?STATE_KEY, State),
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
    persistent_term:put(?STATE_KEY, State#{counters => Counters}),
    ok.

state() ->
    persistent_term:get(?STATE_KEY).

cleanup(SavedEnv) ->
    ?P1:cleanup_with_owned_env(SavedEnv, fun cleanup_fixture/0).

cleanup_fixture() ->
    State = persistent_term:get(?STATE_KEY, undefined),
    try
        %% 护栏兜底解除：用例崩溃后不得让 armed 护栏泄漏到同 VM 后续套件
        %% （六套件同 VM 串行——泄漏会使 P3/验收的桥全部拒绝）。
        imboy_hird_host_bridge:disarm_replay_guard(),
        imboy_hird_poc_membership:teardown(),
        imboy_hird_poc_registry:teardown(),
        ?P1:stop_supervisor(),
        ?P1:stop_audit(),
        stop_cursor()
    after
        persistent_term:erase(?STATE_KEY),
        case State of
            undefined -> ok;
            _ -> inttest_marker_db:release(State)
        end
    end.

%%%===================================================================
%%% 录制 / 回放编排
%%%===================================================================

%% 录制一轮合法轨迹（P1 全链：注册表桥 + 真实授权 + 真实业务读取）。
record_legal(Conn) ->
    given_legal_fixture(Conn),
    RunCtx = ?P1:run_ctx(Conn, 914001, 911001),
    {_Outcome, File} = ?P1:run_program(p2_state(Conn), RunCtx, #{}),
    %% 显式等待录制完成：sync 已在 run_program 内；此处校验文件非空且可解析。
    Records = ?P1:read_audit(File),
    ?assert(length(Records) >= 2),
    File.

%% 录制一轮 deny 轨迹（无 capability）。
record_deny(Conn) ->
    ok = ?P1:reset_tables(Conn),
    ok = ?P1:given_full_fixture(Conn),
    %% 撤掉 capability 行 → not_granted。
    {ok, _} = epgsql:squery(Conn, "DELETE FROM agent_grant_capability"),
    RunCtx = ?P1:run_ctx(Conn, 914001, 911001),
    {_Outcome, File} = ?P1:run_program(p2_state(Conn), RunCtx, #{}),
    File.

%% P1 run_program 需要的 State 最小形状（conn + counters 读取走 registry）。
p2_state(Conn) ->
    #{conn => Conn}.

given_legal_fixture(Conn) ->
    ok = ?P1:reset_tables(Conn),
    ok = ?P1:given_full_fixture(Conn),
    ok.

%% 回放编排：cursor + 回放护栏 + trap 注册表 + audit 新 sink + 程序重跑 +
%% finish。返回 {MainOutcome, ReplayAuditFile, Finish}。
replay_run(LogFile) ->
    Tables = [?MOD:hird_tools@()],
    {ok, _Cursor} = safe_start_link(LogFile, Tables),
    try
        true = hird_replay:active(),
        %% C2#10：cursor 存活期装填宿主护栏——若 cursor 中途死亡导致
        %% dispatch 回退 live，桥在资源解析/授权之前拒绝（真实执行零可能）。
        ok = imboy_hird_host_bridge:arm_replay_guard(),
        ReplayAudit = replay_audit_file(),
        ?P1:open_audit(ReplayAudit),
        ok = hird_audit:register_tools(?MOD:hird_tools@()),
        %% trap 面：model_chat 装调用即崩 handler；ws_detail 不装
        %% （live 误走 = unhandled_tool 崩，均为可观测失败）。
        MainOutcome =
            try
                ok = hird_handlers:with_handlers(
                    [{{tool, model_chat}, ?TRAP_HANDLER}], fun() ->
                        ?MOD:main(#{})
                    end
                ),
                ok
            catch
                Class:Reason -> {caught, Class, Reason}
            end,
        ok = hird_audit:sync(),
        Finish = hird_replay:finish(),
        {MainOutcome, ReplayAudit, Finish}
    after
        ?P1:stop_supervisor(),
        ?P1:stop_audit(),
        imboy_hird_host_bridge:disarm_replay_guard()
    end.

%% start_link 失败隔离：cursor init 失败以 exit 信号直达链接者（非 trap
%% 进程即死——inttest_marker_db:safe_connect 同款"迟到补刀"）。一次性
%% worker trap 并吸收信号，父进程只拿结果。
safe_start_link(Path, Tables) ->
    Parent = self(),
    Worker = spawn(fun() ->
        process_flag(trap_exit, true),
        R =
            try hird_replay:start_link(Path, Tables) of
                Res -> Res
            catch
                Class:Reason -> {safe_raise, Class, Reason}
            end,
        receive
            {'EXIT', _, _} -> ok
        after 500 -> ok
        end,
        Parent ! {self(), R}
    end),
    receive
        {Worker, {safe_raise, C, R}} -> erlang:raise(C, R, []);
        {Worker, R} -> R
    after 5000 ->
        erlang:error({safe_start_link_timeout, Path})
    end.

%% 用例体隔离：整个用例跑在 trap_exit 的一次性 worker 内——任意被链接
%% 进程（cursor/audit/supervisor/actor 重启风暴）的 EXIT 信号都被吸收，
%% eunit runner 不可被杀（cancelled 根治）。
run_isolated(Fun) ->
    Parent = self(),
    W = spawn(fun() ->
        process_flag(trap_exit, true),
        R =
            try
                {ok, Fun()}
            catch
                C:Rn -> {caught, C, Rn}
            end,
        absorb_exits(2),
        Parent ! {self(), R}
    end),
    receive
        {W, {ok, ok}} -> ok;
        {W, {caught, C, Rn}} -> erlang:raise(C, Rn, []);
        {W, Other} -> Other
    after 150000 ->
        erlang:error({isolated_case_timeout, W})
    end.

absorb_exits(0) ->
    ok;
absorb_exits(N) ->
    receive
        {'EXIT', _, _} -> absorb_exits(N - 1)
    after 200 -> ok
    end.

stop_cursor() ->
    try
        gen_server:stop(hird_replay)
    catch
        _:_ -> ok
    end,
    ok.

replay_audit_file() ->
    Tag = integer_to_binary(erlang:unique_integer([positive])),
    filename:join([?OUT_ROOT, "audit", <<"p2_", Tag/binary, ".jsonl">>]).

%% 结构化归一化：仅裁剪时间戳；顺序保留、参数保留、不忽略尾部。
normalize(Records) ->
    [maps:without([<<"timestamp">>], R) || R <- Records].

biz_calls() -> imboy_hird_poc_registry:biz_calls().
provider_calls() -> imboy_hird_poc_registry:provider_calls().

%% 结构化篡改：解码 → 变换 → 重编码写回（保持行序）。
%% 定向篡改：对目标行保格式二进制替换（hird_types:decode_invocation 对
%% 键序敏感——jsone 重编码会重排键序导致加载失败）。替换后仍以 jsone
%% 全量解析验证篡改命中目标行（schema_version 字段仍为整数即可——版本
%% 篡改用例本身会把它改成 2）；两轮对比断言始终走结构化解析不受影响。
mutate_log(File, LineIndex, FromBin, ToBin) ->
    Lines = ?P1:read_lines(File),
    Mutated = [
        mutate_line(I, L, LineIndex, FromBin, ToBin)
     || {I, L} <- lists:zip(lists:seq(0, length(Lines) - 1), Lines)
    ],
    Target = lists:nth(LineIndex + 1, Mutated),
    ?assertNotMatch(nomatch, binary:match(Target, ToBin)),
    ?assertMatch(#{<<"schema_version">> := _}, jsone:decode(Target)),
    Tmp = <<File/binary, ".mutated">>,
    ok = file:write_file(Tmp, [[L, $\n] || L <- Mutated]),
    Tmp.

mutate_line(I, Line, I, FromBin, ToBin) ->
    ?assertNotMatch(nomatch, binary:match(Line, FromBin)),
    binary:replace(Line, FromBin, ToBin);
mutate_line(_I, Line, _Target, _FromBin, _ToBin) ->
    Line.

%% 递归搜索异常原因中的 {hird_exn, Error} 域错误值。
find_exn_value({hird_exn, Error}) ->
    {ok, Error};
find_exn_value(T) when is_tuple(T) ->
    find_exn_value(tuple_to_list(T));
find_exn_value([H | T]) ->
    case find_exn_value(H) of
        {ok, V} -> {ok, V};
        error -> find_exn_value(T)
    end;
find_exn_value(_) ->
    error.

%% 递归搜索异常原因中的 {replay_divergence, #{kind := K}}。
find_divergence_kind({replay_divergence, #{kind := K}}) ->
    {ok, K};
find_divergence_kind(T) when is_tuple(T) ->
    find_divergence_kind(tuple_to_list(T));
find_divergence_kind([H | T]) ->
    case find_divergence_kind(H) of
        {ok, K} -> {ok, K};
        error -> find_divergence_kind(T)
    end;
find_divergence_kind(_) ->
    error.

%%%===================================================================
%%% 用例
%%%===================================================================

t_replay_ok() ->
    run_isolated(fun t_replay_ok_body/0).

t_replay_ok_body() ->
    State = state(),
    Conn = maps:get(conn, State),
    RecFile = record_legal(Conn),
    Recorded = ?P1:read_audit(RecFile),
    BizBefore = biz_calls(),
    ProvBefore = provider_calls(),
    {MainOutcome, ReplayFile, Finish} = replay_run(RecFile),
    %% 程序在回放中正常跑完（与录制同形：ok）。
    ?assertEqual(ok, MainOutcome),
    %% 日志被完整消费。
    ?assertEqual(ok, Finish),
    %% 真实执行尝试为零：Provider 与业务读取计数零增长（atomics 跨进程）。
    ?assertEqual(BizBefore, biz_calls()),
    ?assertEqual(ProvBefore, provider_calls()),
    %% 两轮 audit 结构化一致（除时间戳）；顺序与参数全保留。
    ?assertEqual(normalize(Recorded), normalize(?P1:read_audit(ReplayFile))),
    %% cursor 停止后 active 归位。
    stop_cursor(),
    ?assertNot(hird_replay:active()).

t_args_mismatch() ->
    run_isolated(fun t_args_mismatch_body/0).

t_args_mismatch_body() ->
    State = state(),
    Conn = maps:get(conn, State),
    RecFile = record_legal(Conn),
    %% 篡改 WsDetail 行（第 2 行，0 基 1）的 args.ws_id。
    Tmp = mutate_log(
        RecFile,
        1,
        <<"\"ws_id\":912001">>,
        <<"\"ws_id\":912099">>
    ),
    {MainOutcome, ReplayFile, Finish} = replay_run(Tmp),
    %% 第 2 个 dispatch 与游标不匹配 → 崩（divergence 不前移游标）。
    ?assertMatch({error, {replay_incomplete, #{consumed := 1}}}, Finish),
    ?assertEqual(
        [<<"ModelChat">>],
        [maps:get(<<"tool">>, R) || R <- ?P1:read_audit(ReplayFile)]
    ),
    case MainOutcome of
        ok -> ok;
        {caught, _, Reason} -> ?assertMatch({ok, args_mismatch}, find_divergence_kind(Reason))
    end.

t_tool_mismatch() ->
    run_isolated(fun t_tool_mismatch_body/0).

t_tool_mismatch_body() ->
    State = state(),
    Conn = maps:get(conn, State),
    RecFile = record_legal(Conn),
    ProvBefore = provider_calls(),
    BizBefore = biz_calls(),
    %% 工具名偏移：类型化 audit 格式下签名随名而变（ModelChat 的 args
    %% 形状无法解码 ws_id 记录）→ 回放**加载即失败**——比运行时
    %% divergence 更早拒绝，真实执行零次。（运行时 tool_mismatch
    %% divergence 是 cursor 的单元行为，hird runtime 自测覆盖。）
    Tmp = mutate_log(
        RecFile,
        1,
        <<"\"tool\":\"WsDetail\"">>,
        <<"\"tool\":\"ModelChat\"">>
    ),
    ?assertMatch(
        {error, {replay_load_error, #{line := 2}}},
        safe_start_link(Tmp, [?MOD:hird_tools@()])
    ),
    ?assertNot(hird_replay:active()),
    ?assertEqual(ProvBefore, provider_calls()),
    ?assertEqual(BizBefore, biz_calls()).

t_log_exhausted() ->
    run_isolated(fun t_log_exhausted_body/0).

t_log_exhausted_body() ->
    State = state(),
    Conn = maps:get(conn, State),
    RecFile = record_legal(Conn),
    %% 删除末行：程序第 2 个 dispatch 时游标耗尽。
    Lines = ?P1:read_lines(RecFile),
    Tmp = <<RecFile/binary, ".truncated">>,
    ok = file:write_file(Tmp, [[L, $\n] || L <- lists:droplast(Lines)]),
    {MainOutcome, ReplayFile, Finish} = replay_run(Tmp),
    %% 删末行：剩余日志恰被第一次 dispatch 消费完 → finish=ok；
    %% 检测点在程序第 2 个 dispatch：空游标 → log_exhausted divergence
    %% （该次调用不产生 audit 记录）。
    ?assertEqual(ok, Finish),
    ?assertEqual(
        [<<"ModelChat">>],
        [maps:get(<<"tool">>, R) || R <- ?P1:read_audit(ReplayFile)]
    ),
    case MainOutcome of
        ok -> ok;
        {caught, _, Reason} -> ?assertMatch({ok, log_exhausted}, find_divergence_kind(Reason))
    end.

t_extra_records() ->
    run_isolated(fun t_extra_records_body/0).

t_extra_records_body() ->
    State = state(),
    Conn = maps:get(conn, State),
    RecFile = record_legal(Conn),
    %% 追加一条多余记录：程序可跑完，但 finish 必须报未消费（不静默截尾）。
    Lines = ?P1:read_lines(RecFile),
    Tmp = <<RecFile/binary, ".extra">>,
    ok = file:write_file(Tmp, [[L, $\n] || L <- Lines ++ [lists:last(Lines)]]),
    {MainOutcome, _ReplayFile, Finish} = replay_run(Tmp),
    ?assertEqual(ok, MainOutcome),
    ?assertMatch({error, {replay_incomplete, #{consumed := 2, log_size := 3}}}, Finish).

t_corrupt_line() ->
    run_isolated(fun t_corrupt_line_body/0).

t_corrupt_line_body() ->
    State = state(),
    Conn = maps:get(conn, State),
    RecFile = record_legal(Conn),
    Lines = ?P1:read_lines(RecFile),
    Tmp = <<RecFile/binary, ".corrupt">>,
    ok = file:write_file(Tmp, [[L, $\n] || L <- Lines ++ [<<"not-json">>]]),
    %% 加载即失败：带行号的结构化错误；cursor 未激活。
    ?assertMatch(
        {error, {replay_load_error, #{line := 3, reason := {decode_error, _}}}},
        safe_start_link(Tmp, [?MOD:hird_tools@()])
    ),
    ?assertNot(hird_replay:active()),
    %% 加载失败后未跑程序：录制后的业务计数不再增长（相对断言）。
    ?assertEqual(1, biz_calls()).

t_err_semantics() ->
    run_isolated(fun t_err_semantics_body/0).

t_err_semantics_body() ->
    State = state(),
    Conn = maps:get(conn, State),
    RecFile = record_deny(Conn),
    Recorded = ?P1:read_audit(RecFile),
    %% 录制轨迹：model_chat ok → ws_detail {err, ToolDenied(not_granted)}。
    ?assertMatch(
        {<<"WsDetail">>, #{
            <<"err">> :=
                #{<<"args">> := [<<"not_granted">>], <<"ctor">> := <<"ToolDenied">>}
        }},
        lists:last([{maps:get(<<"tool">>, R), maps:get(<<"result">>, R)} || R <- Recorded])
    ),
    {MainOutcome, ReplayFile, _Finish} = replay_run(RecFile),
    %% 回放中 {err} 以原 {hird_exn} throw 复现：actor 崩溃语义与录制一致
    %% （重启竞态下 main 形态不定；err 值本身经两轮 audit 一致性锁定）。
    case MainOutcome of
        ok ->
            ok;
        {caught, _, Reason} ->
            ?assertMatch(
                {ok, {tool_denied, <<"not_granted">>}},
                find_exn_value(Reason)
            )
    end,
    %% 两轮 audit（含 err 行）结构一致。
    ?assertEqual(normalize(Recorded), normalize(?P1:read_audit(ReplayFile))),
    %% deny 回放同样零真实执行。
    ?assertEqual(0, biz_calls()).

t_start_failure() ->
    run_isolated(fun t_start_failure_body/0).

t_start_failure_body() ->
    %% cursor 无法启动（文件不存在）：显式失败，active 为 false。
    ?assertMatch(
        {error, {replay_load_error, #{reason := enoent}}},
        safe_start_link(
            <<"_build/hird_poc_v1/prog/audit/absent.jsonl">>,
            [?MOD:hird_tools@()]
        )
    ),
    ?assertNot(hird_replay:active()),
    %% 编排纪律：cursor 不在绝不跑程序（否则 trap 注册表面临 live 执行）。
    %% 此处以零计数证明本用例没有产生任何真实执行路径。
    ?assertEqual(0, provider_calls()),
    ?assertEqual(0, biz_calls()).
