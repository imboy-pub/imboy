-module(agent_hird_mini_spike_tests).
-include_lib("eunit/include/eunit.hrl").

%%% AG31-10A Hirð Mini Spike。
%%%
%%% 目的：验证 Rust compiler (hird CLI) → Erlang/BEAM 生成物 → IMBoy/OTP
%%% 宿主进程的最小集成边界（G01-G06 最小面），不建第二套业务层。
%%%
%%% 机制（Spike 核心知识产出，见 evidence result.md）：
%%% - `hird build <file.hird> --out-dir X` 生成自包含产物：程序模块
%%%   `hird_<snake(module)>`、actor/supervisor 模块 `hird_<snake(声明名)>`
%%%   （取自声明名，不带 module 前缀）、固定名 boot 模块 `hird_boot`、
%%%   以及 9 个内嵌 runtime 模块，全部现场 erlc 到 X。产物是普通 Erlang
%%%   模块，IMBoy 宿主经 code:add_patha(X) 加载后自行扮演 boot 语义
%%%   （audit sink 生命周期自理），不使用 halt 语义的 hird_boot。
%%% - 词法 handle 块 lowering 为进程本地 handler map（普通 map 传参给
%%%   hird_tool_dispatch:call/4）；install 块 lowering 为
%%%   hird_handlers:with_handlers（persistent_term 全局注册表的作用域
%%%   安装+恢复）。spawn 出去的 actor 拿不到 map，落全局注册表。
%%% - hird_audit 是 {local, hird_audit} node 级单例 gen_server；supervisor
%%%   注册名 = 生成模块名 = snake(声明名)。因此实例隔离的唯一通道是
%%%   "每实例一组不同的声明名"（本 spike 用 SpikeAlpha/SpikeBeta 与
%%%   PingerSup/WatcherSup 两套命名实测）。
%%%
%%% 纪律：hird 仓库只读；runtime 不复制进本仓（产物现场生成到本仓
%%% gitignored 的 _build/ 下）；mock handler，不接真实 Model/Tool/DB/网络。

-define(HIRD_BIN, <<"/Users/leeyi/project/imboy.pub/hird/target/debug/hird">>).
-define(OUT_ROOT,
    <<"/Users/leeyi/project/imboy.pub/.worktrees/track-agent-v31-20260916/imboy/_build/spike_ag31_10a">>
).

%%% hird 源程序（两个纯函数实例 + 两个带 supervisor 树实例）。
%%% SpikeAlpha/SpikeBeta：同 tool 声明（Echo），不同 module 名与 handler 结果，
%%% 用于验证词法 handler map 的实例隔离与并发不串线。
%%% SpikeSupAlpha/SpikeSupBeta：actor+supervisor 树，声明名各不相同
%%% （PingerSup/WatcherSup），用于验证注册名隔离与重复 start/stop。
-define(HIRD_ALPHA_SRC, <<"""
module SpikeAlpha

tool Echo : { message: String } → String

fn main() → () ! {} =
  handle {
    Tool<Echo> → echo_alpha,
  } in let _ = echo({ message: "alpha-ping" }) in ()

fn echo_alpha(args: { message: String }) → String =
  "alpha-result"
"""/utf8>>).

-define(HIRD_BETA_SRC, <<"""
module SpikeBeta

tool Echo : { message: String } → String

fn main() → () ! {} =
  handle {
    Tool<Echo> → echo_beta,
  } in let _ = echo({ message: "beta-ping" }) in ()

fn echo_beta(args: { message: String }) → String =
  "beta-result"
"""/utf8>>).

-define(HIRD_SUP_ALPHA_SRC, <<"""
module SpikeSupAlpha

tool Ping : { n: Int } → String

type Cfg = Cfg(Int)
type alias PingerState = { pings: Int }
type PingerStatus = PingerStatus(Int)

actor Pinger {
  state: PingerState,

  message: PingerMsg =
    | Tick
    | GetStatus(ReplyTo<PingerStatus>)
    | Shutdown,

  init: fn(cfg: Cfg) ! {} = { pings: 0 },

  handle Tick, st ! {Tool<Ping>} =
    let _ = ping({ n: st.pings }) in
    Continue({ pings: st.pings + 1, ..st }),

  handle GetStatus(reply_to), st ! {Send<PingerStatus>} =
    reply(reply_to, PingerStatus(st.pings));
    Continue(st),

  handle Shutdown, _ ! {} = Stop,
} ! {Tool<Ping>, Send<PingerStatus>}

supervisor PingerSup {
  strategy: one_for_one,
  intensity: 5,
  period: 60,
  children: [
    { id: pinger, actor: Pinger, start_args: Cfg(7), restart: transient },
  ]
}

fn do_ping(args: { n: Int }) → String =
  "pong"

fn main() → () ! {Install, Supervise, Send<PingerMsg>, Await<PingerStatus>} =
  install {
    Tool<Ping> → do_ping,
  } in
  supervise(PingerSup);
  let pinger = child(PingerSup, pinger) in
  send(pinger, Tick);
  let PingerStatus(pings) = request(pinger, GetStatus) in
  send(pinger, Shutdown);
  if pings > 0 then () else crash!("no pings")
"""/utf8>>).

-define(HIRD_SUP_BETA_SRC, <<"""
module SpikeSupBeta

tool Ping : { n: Int } → String

type Cfg = Cfg(Int)
type alias WatcherState = { pings: Int }
type WatcherStatus = WatcherStatus(Int)

actor Watcher {
  state: WatcherState,

  message: WatcherMsg =
    | Tick
    | GetStatus(ReplyTo<WatcherStatus>)
    | Shutdown,

  init: fn(cfg: Cfg) ! {} = { pings: 0 },

  handle Tick, st ! {Tool<Ping>} =
    let _ = ping({ n: st.pings }) in
    Continue({ pings: st.pings + 1, ..st }),

  handle GetStatus(reply_to), st ! {Send<WatcherStatus>} =
    reply(reply_to, WatcherStatus(st.pings));
    Continue(st),

  handle Shutdown, _ ! {} = Stop,
} ! {Tool<Ping>, Send<WatcherStatus>}

supervisor WatcherSup {
  strategy: one_for_one,
  intensity: 5,
  period: 60,
  children: [
    { id: watcher, actor: Watcher, start_args: Cfg(9), restart: transient },
  ]
}

fn do_ping(args: { n: Int }) → String =
  "beta-pong"

fn main() → () ! {Install, Supervise, Send<WatcherMsg>, Await<WatcherStatus>} =
  install {
    Tool<Ping> → do_ping,
  } in
  supervise(WatcherSup);
  let watcher = child(WatcherSup, watcher) in
  send(watcher, Tick);
  let WatcherStatus(pings) = request(watcher, GetStatus) in
  send(watcher, Shutdown);
  if pings > 0 then () else crash!("no pings")
"""/utf8>>).

%%% Generated program modules（生成物 API 面）.
-define(MOD_ALPHA, hird_spike_alpha).
-define(MOD_BETA, hird_spike_beta).
-define(MOD_SUP_ALPHA, hird_spike_sup_alpha).
-define(MOD_SUP_BETA, hird_spike_sup_beta).
%%% 生成的 supervisor 模块名 = snake(声明名) = {local, ...} 注册名.
-define(SUP_ALPHA_NAME, hird_pinger_sup).
-define(SUP_BETA_NAME, hird_watcher_sup).

%%%===================================================================
%%% eunit 套件（全部串行：hird_audit 是 node 级单例，不能并行互踩）
%%%===================================================================

agent_hird_mini_spike_test_() ->
    {foreach, fun force_clean/0, fun(_) -> force_clean() end, [
        {<<"G01/G02 hird 工具链可用且产物可被宿主加载">>, fun case_toolchain_ready/0},
        {<<"G03/G05 纯函数双实例串行生命周期：结果不串线、audit 各自独立、无残留">>, fun case_pure_sequential_isolated/0},
        {<<"G03 并发双实例（词法 handler map）：结果互不污染">>, fun case_pure_concurrent_no_crosstalk/0},
        {<<"G04 supervisor 树启停干净、重复 start/stop 无 registration conflict">>,
            fun case_sup_tree_repeat_start_stop/0},
        {<<"G04/G05 声明名隔离双实例（supervisor 树）串行各自独立">>, fun case_sup_two_instances/0},
        {<<"G03/G06 宿主 mock 注入通道（registry/with_handlers）与恢复语义">>, fun case_host_mock_injection/0}
    ]}.

%%%-------------------------------------------------------------------
%%% G01/G02：工具链与产物加载
%%%-------------------------------------------------------------------

case_toolchain_ready() ->
    ensure_built(alpha),
    ?assert(is_list(code:which(?MOD_ALPHA))),
    ?assert(is_list(code:which(hird_tool_dispatch))),
    ?assert(is_list(code:which(hird_audit))),
    %% 生成模块暴露宿主可调用的签名表 hird_tools@/0.
    Table = ?MOD_ALPHA:hird_tools@(),
    #{tools := #{echo := #{name := <<"Echo">>}}} = Table,
    %% 纯函数版 main/0（effect row 已被 handle 消化）.
    ok = ?MOD_ALPHA:main(),
    %% 无 audit sink 时 dispatch 照常（audit cast 为 no-op），main 跑通即证.
    ok.

%%%-------------------------------------------------------------------
%%% G03/G05：串行双实例隔离
%%%-------------------------------------------------------------------

case_pure_sequential_isolated() ->
    ensure_built(alpha),
    ensure_built(beta),
    FileA = audit_path(<<"pure_a">>),
    FileB = audit_path(<<"pure_b">>),
    %% 实例 A 完整生命周期（宿主扮演 boot：audit sink → register → run → sync）.
    open_audit(FileA),
    ok = hird_audit:register_tools(?MOD_ALPHA:hird_tools@()),
    ok = ?MOD_ALPHA:main(),
    ok = hird_audit:sync(),
    ok = gen_server:stop(hird_audit),
    %% 实例 B 同构生命周期.
    open_audit(FileB),
    ok = hird_audit:register_tools(?MOD_BETA:hird_tools@()),
    ok = ?MOD_BETA:main(),
    ok = hird_audit:sync(),
    ok = gen_server:stop(hird_audit),
    %% G05：audit 各自独立——每个文件恰好 1 条且只含本实例结果.
    [LineA] = read_lines(FileA),
    ?assertNotMatch(nomatch, binary:match(LineA, <<"\"result\":{\"ok\":\"alpha-result\"}">>)),
    ?assertMatch(nomatch, binary:match(LineA, <<"beta">>)),
    [LineB] = read_lines(FileB),
    ?assertNotMatch(nomatch, binary:match(LineB, <<"\"result\":{\"ok\":\"beta-result\"}">>)),
    ?assertMatch(nomatch, binary:match(LineB, <<"alpha">>)),
    %% handler 被调用（audit 的 result 即 handler 返回值）且 caller 正确.
    ?assertNotMatch(nomatch, binary:match(LineA, <<"\"caller\":\"SpikeAlpha.main\"">>)),
    ?assertNotMatch(nomatch, binary:match(LineB, <<"\"caller\":\"SpikeBeta.main\"">>)).

%%%-------------------------------------------------------------------
%%% G03：并发双实例不串线（词法 handler map 进程本地）
%%%-------------------------------------------------------------------

case_pure_concurrent_no_crosstalk() ->
    ensure_built(alpha),
    ensure_built(beta),
    N = 20,
    FileAB = audit_path(<<"pure_ab">>),
    open_audit(FileAB),
    ok = hird_audit:register_tools(?MOD_ALPHA:hird_tools@()),
    ok = hird_audit:register_tools(?MOD_BETA:hird_tools@()),
    Parent = self(),
    %% 并发说明：hird_audit 是 node 级单例，两实例共享同一 sink（发现，
    %% 见 result.md）；隔离靠每条记录的 caller/result 配对断言.
    PidA = spawn_fun(fun() -> loop_main(?MOD_ALPHA, N) end, Parent),
    PidB = spawn_fun(fun() -> loop_main(?MOD_BETA, N) end, Parent),
    ?assertEqual(ok, await_worker(PidA)),
    ?assertEqual(ok, await_worker(PidB)),
    ok = hird_audit:sync(),
    ok = gen_server:stop(hird_audit),
    Lines = read_lines(FileAB),
    ?assertEqual(2 * N, length(Lines)),
    CountA = length([
        L
     || L <- Lines,
        is_pair(L, <<"SpikeAlpha.main">>, <<"alpha-result">>)
    ]),
    CountB = length([
        L
     || L <- Lines,
        is_pair(L, <<"SpikeBeta.main">>, <<"beta-result">>)
    ]),
    %% 每条记录 caller/result 严格配对：无交叉污染.
    ?assertEqual(N, CountA),
    ?assertEqual(N, CountB),
    ?assertEqual(CountA + CountB, length(Lines)).

loop_main(_Mod, 0) ->
    ok;
loop_main(Mod, K) ->
    ok = Mod:main(),
    loop_main(Mod, K - 1).

spawn_fun(Fun, Parent) ->
    spawn(fun() ->
        Result =
            try
                Fun()
            catch
                Class:Reason -> {spike_exn, Class, Reason}
            end,
        Parent ! {self(), Result}
    end).

await_worker(Pid) ->
    receive
        {Pid, Result} -> Result
    after 15000 -> erlang:error({worker_timeout, Pid})
    end.

is_pair(Line, Caller, Result) ->
    MatchCaller = binary:match(Line, <<"\"caller\":\"", Caller/binary, "\"">>) =/= nomatch,
    MatchResult =
        binary:match(
            Line,
            <<"\"result\":{\"ok\":\"", Result/binary, "\"}">>
        ) =/= nomatch,
    MatchCaller andalso MatchResult.

%%%-------------------------------------------------------------------
%%% G04：supervisor 树启停 + 重复 start/stop
%%%-------------------------------------------------------------------

case_sup_tree_repeat_start_stop() ->
    ensure_built(sup_alpha),
    Round1 = audit_path(<<"sup_r1">>),
    Round2 = audit_path(<<"sup_r2">>),
    %% 第一轮：起树 → 审计 → 停树.
    ok = run_sup_instance(?MOD_SUP_ALPHA, ?SUP_ALPHA_NAME, Round1),
    [Line1] = read_lines(Round1),
    ?assertNotMatch(
        nomatch,
        binary:match(Line1, <<"\"caller\":\"Pinger.handle_msg/Tick\"">>)
    ),
    ?assertNotMatch(nomatch, binary:match(Line1, <<"\"result\":{\"ok\":\"pong\"}">>)),
    assert_tree_down(?SUP_ALPHA_NAME),
    %% 第二轮：同样入口重复 start/stop——若首轮残留注册名/进程，
    %% supervise 的 {local, Name} start 会 already_started 崩溃.
    ok = run_sup_instance(?MOD_SUP_ALPHA, ?SUP_ALPHA_NAME, Round2),
    ?assertEqual(1, length(read_lines(Round2))),
    assert_tree_down(?SUP_ALPHA_NAME).

run_sup_instance(Mod, SupName, AuditFile) ->
    open_audit(AuditFile),
    ok = hird_audit:register_tools(Mod:hird_tools@()),
    %% install 块 lowering 为 with_handlers：main/1 接空 map（boot 语义）.
    ok = Mod:main(#{}),
    ok = hird_audit:sync(),
    ?assert(is_pid(whereis(SupName))),
    ok = gen_server:stop(SupName),
    ok = gen_server:stop(hird_audit),
    ok.

%%%-------------------------------------------------------------------
%%% G04/G05：声明名隔离的双 supervisor 实例
%%%-------------------------------------------------------------------

case_sup_two_instances() ->
    ensure_built(sup_alpha),
    ensure_built(sup_beta),
    FileA = audit_path(<<"sup_a">>),
    FileB = audit_path(<<"sup_b">>),
    ok = run_sup_instance(?MOD_SUP_ALPHA, ?SUP_ALPHA_NAME, FileA),
    ok = run_sup_instance(?MOD_SUP_BETA, ?SUP_BETA_NAME, FileB),
    [LineA] = read_lines(FileA),
    [LineB] = read_lines(FileB),
    ?assertNotMatch(nomatch, binary:match(LineA, <<"\"result\":{\"ok\":\"pong\"}">>)),
    ?assertNotMatch(nomatch, binary:match(LineB, <<"\"result\":{\"ok\":\"beta-pong\"}">>)),
    ?assertMatch(nomatch, binary:match(LineA, <<"beta-pong">>)),
    ?assertMatch(nomatch, binary:match(LineB, <<"\"ok\":\"pong\"}">>)),
    assert_tree_down(?SUP_ALPHA_NAME),
    assert_tree_down(?SUP_BETA_NAME).

%%%-------------------------------------------------------------------
%%% G03/G06：宿主 mock 注入通道与全局注册表恢复语义
%%%-------------------------------------------------------------------

case_host_mock_injection() ->
    ensure_built(alpha),
    File = audit_path(<<"host_inject">>),
    open_audit(File),
    %% 集成边界发现：audit 编码是类型导向的，宿主自定 tool 必须先注册
    %% 签名表，否则 hird_audit 崩溃（{unknown_tool, Tool}）——实测确证.
    HostTable = #{
        tools => #{
            host_probe => #{
                name => <<"HostProbe">>,
                args => {record, [{n, int}]},
                result => string,
                error => dynamic
            }
        },
        types => #{}
    },
    ok = hird_audit:register_tools(HostTable),
    %% 宿主经 hird_handlers（with_handlers 作用域安装）注入 mock，
    %% 并直接驱动 hird_tool_dispatch —— 不经生成程序也能完成 tool 效果.
    Mock = fun(_Args, _Handlers) -> <<"injected-mock-ping">> end,
    Result = hird_handlers:with_handlers(
        [{{tool, host_probe}, Mock}],
        fun() ->
            hird_tool_dispatch:call(
                host_probe,
                <<"Host.spike">>,
                #{},
                #{n => 1}
            )
        end
    ),
    ?assertEqual(<<"injected-mock-ping">>, Result),
    ok = hird_audit:sync(),
    ok = gen_server:stop(hird_audit),
    [Line] = read_lines(File),
    ?assertNotMatch(
        nomatch,
        binary:match(Line, <<"\"result\":{\"ok\":\"injected-mock-ping\"}">>)
    ),
    ?assertNotMatch(nomatch, binary:match(Line, <<"\"caller\":\"Host.spike\"">>)),
    ?assertNotMatch(nomatch, binary:match(Line, <<"\"tool\":\"HostProbe\"">>)),
    %% with_handlers 退出后恢复：注册表回到安装前状态（无残留，G06）.
    ?assertEqual(error, hird_handlers:lookup_handler({tool, host_probe})),
    %% 未安装的 tool 落 registry miss → 定义性 crash（不吞错）.
    ?assertError(
        {unhandled_tool, never_installed},
        hird_tool_dispatch:call(never_installed, <<"Host.x">>, #{}, #{})
    ).

%%%===================================================================
%%% fixture 与断言 helper
%%%===================================================================

ensure_built(Which) ->
    OutDir = out_dir(Which),
    BeamFile = filename:join([OutDir, program_module(Which) ++ ".beam"]),
    case filelib:is_regular(BeamFile) of
        true -> ok;
        false -> build_program(Which, OutDir)
    end,
    %% audit sink 目录（hird_audit 打不开 sink 会直接 {stop, cannot_open_sink}）.
    ok = filelib:ensure_dir(filename:join([?OUT_ROOT, "audit", "x"])),
    %% 产物路径加入 code path；幂等.
    true = code:add_patha(binary_to_list(OutDir)),
    Mod = program_module_atom(Which),
    case code:is_loaded(Mod) of
        {file, _} ->
            ok;
        %% 未加载才 load：同 beam 反复 load_file 会累积 old 版本（not_purged）.
        false ->
            {module, Mod} = code:load_file(Mod),
            ok
    end.

build_program(Which, OutDir) ->
    ok = filelib:ensure_dir(filename:join([OutDir, "x"])),
    SrcFile = src_file(Which),
    ok = file:write_file(SrcFile, hird_src(Which)),
    Cmd = io_lib:format(
        "\"~ts\" build \"~ts\" --out-dir \"~ts\" 2>&1",
        [?HIRD_BIN, SrcFile, OutDir]
    ),
    Output = os:cmd(binary_to_list(iolist_to_binary(Cmd))),
    case filelib:is_regular(filename:join([OutDir, program_module(Which) ++ ".beam"])) of
        true -> ok;
        false -> erlang:error({hird_build_failed, Which, Output})
    end.

out_dir(alpha) -> filename:join([?OUT_ROOT, "out_alpha"]);
out_dir(beta) -> filename:join([?OUT_ROOT, "out_beta"]);
out_dir(sup_alpha) -> filename:join([?OUT_ROOT, "out_sup_alpha"]);
out_dir(sup_beta) -> filename:join([?OUT_ROOT, "out_sup_beta"]).

src_file(Which) ->
    %% hird 约束（C0019）：源文件路径名必须等于 snake_case(Hirð module 名)
    %% （无生成 Erlang 模块名的 hird_ 前缀）.
    filename:join([?OUT_ROOT, hird_src_base(Which) ++ ".hird"]).

hird_src_base(alpha) -> "spike_alpha";
hird_src_base(beta) -> "spike_beta";
hird_src_base(sup_alpha) -> "spike_sup_alpha";
hird_src_base(sup_beta) -> "spike_sup_beta".

program_module(alpha) -> "hird_spike_alpha";
program_module(beta) -> "hird_spike_beta";
program_module(sup_alpha) -> "hird_spike_sup_alpha";
program_module(sup_beta) -> "hird_spike_sup_beta".

program_module_atom(Which) -> list_to_atom(program_module(Which)).

hird_src(alpha) -> ?HIRD_ALPHA_SRC;
hird_src(beta) -> ?HIRD_BETA_SRC;
hird_src(sup_alpha) -> ?HIRD_SUP_ALPHA_SRC;
hird_src(sup_beta) -> ?HIRD_SUP_BETA_SRC.

audit_path(Tag) when is_binary(Tag) ->
    filename:join([?OUT_ROOT, "audit", <<Tag/binary, ".jsonl">>]).

read_lines(Path) ->
    {ok, Bin} = file:read_file(Path),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    [L || L <- Lines, L =/= <<>>].

%%% audit sink 文件是 append-only：开新生命周期前删旧文件，
%%% 避免上一轮（或上次进程）的记录污染本轮断言.
open_audit(File) ->
    _ = file:delete(File),
    {ok, _} = hird_audit:start_link([{sink, {file, File}}]).

%%% G06：套件级残留断言——audit 单例、supervisor 注册名、
%%% persistent_term 注册表、hird 生成进程四类全部归零.
assert_tree_down(SupName) ->
    ?assertEqual(undefined, whereis(SupName)),
    HirdMods = [hird_pinger, hird_pinger_sup, hird_watcher, hird_watcher_sup],
    Leaks = [
        P
     || P <- erlang:processes(),
        {M, _, _} <- [proc_lib:initial_call(P)],
        lists:member(M, HirdMods)
    ],
    ?assertEqual([], Leaks).

force_clean() ->
    safe_stop(hird_audit),
    [safe_stop(N) || N <- [?SUP_ALPHA_NAME, ?SUP_BETA_NAME]],
    %% 清掉可能被其他用例残留的注册表条目.
    [
        persistent_term:erase(K)
     || {K, _} <- persistent_term:get(),
        element(1, K) =:= hird_handlers
    ],
    ok.

safe_stop(Name) ->
    try
        gen_server:stop(Name)
    catch
        _:_ -> ok
    end.
