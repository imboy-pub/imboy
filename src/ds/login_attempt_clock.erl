-module(login_attempt_clock).

-moduledoc "Limiter V1 权威状态单调时钟 seam（计划 2026-10-13 附录 C.4）。".
%%%
%%% Limiter V1（L2）：login_attempt_ds 的唯一时间来源。
%%%
%%% 合同（附录 C.4）：
%%% - 统一 monotonic 毫秒；逻辑过期判定 `now >= expires_at_ms`。
%%% - 本地 monotonic 不可跨节点/跨重启原样复制（重启后为新空态，AC-13）。
%%%
%%% 独立成模块的唯一原因是可测试性：测试通过 meck 本模块注入/推进时钟
%%% （项目既有时间注入先例是对 elib_dt 的 meck；D/S/实际存储不可 mock，
%%% 时钟注入是 AC 执行合同明确允许的唯一 seam）。生产路径零间接成本——
%%% 本函数体就是 erlang:monotonic_time/1 的直接调用。
%%%

-export([now_ms/0]).

%% @doc 当前 monotonic 毫秒。owner（login_attempt_ds）内所有窗口/到期/
%% deadline 判定的唯一时间来源；严禁在 owner 内散落 erlang:monotonic_time
%% 直调（会绕开测试注入）。
-spec now_ms() -> integer().
now_ms() ->
    erlang:monotonic_time(millisecond).
