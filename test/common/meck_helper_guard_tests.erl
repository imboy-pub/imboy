-module(meck_helper_guard_tests).

%% meck_helper 常驻调用者护栏的回归测试（跨套件隔离纪律，见 meck_helper.erl
%% §常驻调用者护栏）。
%%
%% 覆盖三件事：
%%   ① 调用者判据归一化：gen_server 进程 / supervisor 进程 → 模块名；
%%     测试进程 / 裸 spawn 进程 → skip（期望对这些调用者保持生效）。
%%   ② 名单完整性：名单内每个模块都能加载（防改名/拼写漂移导致的死条目）；
%%     登记在案的 SUT 排除项（login_attempt_ds）不得在名单内。
%%   ③ 端到端接线：经 meck_helper 安装的期望 fun 对常驻调用者让路到原实现，
%%     对测试进程仍然生效（用 msg_store_ds → msg_store_repo 的真实委托链，
%%     期望 fun 抛标记错误：测试进程调用必抛、常驻进程调用必不抛）。

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 探针：gen_server（判据归一化用）
%% ===================================================================

-define(PROBE_MARKER, meck_helper_guard_probe_hit).

-export([gs_start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

gs_start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, #{}}.
handle_call(get, _From, S) -> {reply, ok, S};
handle_call(_Msg, _From, S) -> {reply, ok, S}.
handle_cast(_Msg, S) -> {noreply, S}.
handle_info(_Msg, S) -> {noreply, S}.
terminate(_Reason, _S) -> ok.

%% ===================================================================
%% ① 调用者判据归一化
%% ===================================================================

caller_module_semantics_test() ->
    %% 测试进程自身非 proc_lib → skip（期望语义不受护栏影响）
    ?assertEqual(skip, meck_helper:caller_module(self())),

    %% 裸 spawn（无 $initial_call）→ skip
    Bare = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ?assertEqual(skip, meck_helper:caller_module(Bare)),
    Bare ! stop,

    %% gen_server 进程 → 模块名（初始 call 形态 {Mod, init, [...]}）
    {ok, Gs} = gs_start_link(),
    ?assertEqual(?MODULE, meck_helper:caller_module(Gs)),
    gen_server:stop(Gs),

    %% supervisor 进程 → 模块名。形态与 gen_server **不同**：首位是行为模块
    %% （{supervisor, CallbackMod, [...]}），归一化必须取第 2 位——用 VM 内
    %% 恒在的 kernel_sup 钉死（其回调模块名为 kernel，与注册名不同名，
    %% 若误取首位会得到 supervisor 而断言失败）。
    KernelSup = whereis(kernel_sup),
    ?assert(is_pid(KernelSup)),
    ?assertMatch({supervisor, kernel, _}, proc_lib:initial_call(KernelSup)),
    ?assertEqual(kernel, meck_helper:caller_module(KernelSup)),

    %% 非法/非进程入参 → skip（不抛错）
    ?assertEqual(skip, meck_helper:caller_module(undefined)),
    ?assertEqual(skip, meck_helper:caller_module(spawn(fun() -> ok end))).

%% ===================================================================
%% ② 名单完整性
%% ===================================================================

resident_list_integrity_test() ->
    Mods = meck_helper:resident_caller_modules(),
    ?assert(length(Mods) > 0),
    %% 无重复条目
    ?assertEqual(lists:usort(Mods), lists:sort(Mods)),
    lists:foreach(
        fun(M) ->
            ?assert(is_atom(M)),
            %% 可加载：改名/拼写漂移会让该条目永远匹配不到（死条目）
            ?assertNotEqual(
                non_existing,
                code:which(M),
                io_lib:format("resident caller module not loadable: ~p", [M])
            )
        end,
        Mods
    ),
    %% 登记在案的 SUT 排除项：login_attempt_ds 既是常驻 gen_server 又是单测
    %% SUT（handle_call 在 server 进程内执行业务），入名单会让其套件期望被
    %% 让路 → 必须不在名单内（v3 全量实证 is_locked 红）。
    ?assertNot(lists:member(login_attempt_ds, Mods)),
    %% gen_event 宿主（handler 模块在 gen_event 进程内执行、进程初始 call 只
    %% 暴露 gen_event）不是可识别调用者 → 不入名单
    ?assertNot(lists:member(imboy_domain_event, Mods)).

%% ===================================================================
%% ③ 端到端接线：期望 fun 对常驻调用者让路、对测试进程生效
%% ===================================================================

guard_wiring_test_() ->
    {timeout, 60, fun guard_wiring/0}.

guard_wiring() ->
    %% msg_store_ds:len/0 是纯委托：len() -> msg_store_repo:get_staging_stats()。
    %% 期望 fun 抛标记错误 → 命中即抛，未命中即穿透到真实实现。
    {ok, _} = meck_helper:setup_mock(msg_store_repo, [
        {'get_staging_stats', 0, fun() -> erlang:error(?PROBE_MARKER) end}
    ]),
    try
        %% (a) 阳性对照：测试进程调用 → 期望必须生效（护栏不得改变测试语义）
        ?assertError(?PROBE_MARKER, msg_store_ds:len()),

        %% (b) 常驻调用者：子进程 initial_call = {msg_store_ds, len, 0}（名单内）
        %%     → 期望让路到原实现，子进程不得因标记错误而死。
        %%     真实现无论成功/DB 不可用都不是标记错误，故断言稳定。
        Child = proc_lib:spawn(msg_store_ds, len, []),
        Ref = erlang:monitor(process, Child),
        Reason =
            receive
                {'DOWN', Ref, process, Child, R} -> R
            after 30000 -> timeout
            end,
        ?assertNot(is_probe_marker(Reason))
    after
        meck_helper:cleanup_mock(msg_store_repo)
    end,
    ok.

is_probe_marker(?PROBE_MARKER) -> true;
is_probe_marker({?PROBE_MARKER, _Stack}) -> true;
is_probe_marker({?PROBE_MARKER, _Stack, _Info}) -> true;
is_probe_marker(_Other) -> false.
