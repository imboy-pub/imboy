%%% elib_tsid_lock — TSID CombinedNode lifetime lock（TSID-06）
%%%
%%% 目标语义（计划 EXT-05）：两个独立 BEAM/容器间互斥；owner 崩溃后
%%% 由 OS 自动释放，绝不留下需要人工删除的 stale lock。
%%%
%%% Provider：
%%%   flock（生产，Linux）—— open_port 执行
%%%       flock -x -n <path> sh -c 'echo +locked; exec sleep <long>'
%%%     锁由 flock(2) 内核语义持有：持有进程死亡 → 内核释放 fd 锁；
%%%     port 随属主（guard）进程死亡而关闭 → flock 进程组死亡 → 释放。
%%%     确认协议：命令在拿到锁后才运行，stdout 输出 "+locked" 行即
%%%     锁定确认；-n 非阻塞（util-linux 与 busybox 通用），未获得时
%%%     立即以非零退出，超时语义由本模块指数退避重试实现。
%%%     自动释放机制：持锁命令阻塞读 stdin（cat）——port 属主进程或
%%%     整个 BEAM 死亡（含 kill -9）→ 管道写端关闭 → EOF → 命令退出
%%%     → 内核释放 flock。不依赖 BEAM 对子进程的信号传播（SIGKILL
%%%     下 BEAM 无法清理任何子进程）。
%%%   registry（测试/无 flock 环境）—— 本地 register/2 以进程名占位：
%%%     重名即互斥，进程死亡自动除名。语义与 flock 同构（同 VM 内），
%%%     用于 macOS 本地 guard 逻辑测试；真实跨进程互斥验证在 Linux
%%%     容器以 flock provider 执行。
%%%
%%% 禁止：file:open([exclusive]) 的 O_EXCL 文件锁（owner 崩溃后留
%%% stale 文件，且 NFS 语义不可靠）——见计划 §2.4 EXT-05。
-module(elib_tsid_lock).

-moduledoc "TSID CombinedNode lifetime lock（TSID-06）。".
-export([acquire/3, release/1, provider_available/1]).

-type provider() :: flock | registry.

-export_type([provider/0]).

%% 单次 flock 尝试的确认等待窗口
-define(LOCK_CONFIRM_MS, 2000).
%% 退避重试起始间隔与上限
-define(BACKOFF_START_MS, 100).
-define(BACKOFF_MAX_MS, 1000).

%% @doc 获取 lifetime lock。成功返回持有的 lock 句柄（flock：port；
%% registry：{registry, Name}）；锁的生命周期与调用进程绑定，进程
%% 死亡即自动释放。
-spec acquire(file:filename_all(), pos_integer(), provider()) ->
    {ok, Lock :: term()}
    | {error, lock_timeout | no_flock | lock_taken | term()}.
acquire(Path, _TimeoutMs, registry) ->
    Name = registry_name(Path),
    try register(Name, self()) of
        true -> {ok, {registry, Name}}
    catch
        error:badarg -> {error, lock_taken}
    end;
acquire(Path, TimeoutMs, flock) ->
    case os:find_executable("flock") of
        false ->
            {error, no_flock};
        Exe ->
            Args = [
                "-x",
                "-n",
                path_list(Path),
                "sh",
                "-c",
                "echo +locked; exec cat >/dev/null"
            ],
            flock_retry(Exe, Args, TimeoutMs, ?BACKOFF_START_MS)
    end.

%% @doc 显式释放（正常停机路径；崩溃路径由 OS/进程死亡自动释放）
-spec release(term()) -> ok.
release({registry, Name}) when is_atom(Name) ->
    try
        unregister(Name)
    catch
        _:_ -> ok
    end,
    ok;
release(Port) when is_port(Port) ->
    try
        erlang:port_close(Port)
    catch
        _:_ -> ok
    end,
    ok.

%% @doc provider 在当前环境是否可用
-spec provider_available(provider()) -> boolean().
provider_available(flock) ->
    os:find_executable("flock") =/= false;
provider_available(registry) ->
    true.

%% -------------------------------------------------------------------
%% 内部
%% -------------------------------------------------------------------

%% -n 非阻塞 + 指数退避重试实现超时（busybox 无 -w，util-linux 兼容）
flock_retry(_Exe, _Args, Budget, _Backoff) when Budget =< 0 ->
    {error, lock_timeout};
flock_retry(Exe, Args, Budget, Backoff) ->
    Port =
        erlang:open_port(
            {spawn_executable, Exe},
            [use_stdio, {line, 256}, binary, exit_status, hide, {args, Args}]
        ),
    case wait_lock(Port, ?LOCK_CONFIRM_MS) of
        {ok, Port} = Ok ->
            Ok;
        {error, not_locked} ->
            %% -n 竞争失败（目标被持）：退避后重试
            _ =
                try
                    erlang:port_close(Port)
                catch
                    _:_ -> ok
                end,
            ok = timer:sleep(Backoff),
            flock_retry(Exe, Args, Budget - Backoff, min(Backoff * 2, ?BACKOFF_MAX_MS));
        {error, _} = E ->
            _ =
                try
                    erlang:port_close(Port)
                catch
                    _:_ -> ok
                end,
            E
    end.

%% flock port 确认协议："+locked" 行 = 已持锁；非零退出 = 未获得
wait_lock(Port, TimeoutMs) ->
    receive
        {Port, {data, {eol, <<"+locked">>}}} ->
            {ok, Port};
        {Port, {data, _OtherLine}} ->
            wait_lock(Port, TimeoutMs);
        {Port, {exit_status, _N}} ->
            {error, not_locked};
        {'EXIT', Port, Reason} ->
            {error, {port_exit, Reason}}
    after TimeoutMs ->
        {error, lock_timeout}
    end.

path_list(Path) ->
    binary_to_list(iolist_to_binary(Path)).

registry_name(Path) ->
    list_to_atom("elib_tsid_lock_" ++ integer_to_list(erlang:phash2(path_list(Path)))).
