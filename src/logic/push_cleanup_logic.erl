-module(push_cleanup_logic).
-moduledoc "推送过期 token 定时清理业务逻辑（供 ecron 定时任务调用，G3/Q09）。".
%%%
% 推送过期 token 定时清理业务逻辑（供 ecron 定时任务调用）
%
% G3 缺口修复（Q09，2026-10-07）：push_notification_ds:cleanup_inactive_tokens/1
% 此前已实现但**零生产调用方**——过期 token 只有 provider 回报 404/410/1003 的
% 被动下线路径，无主动定期清理。本模块补齐挂载点：ecron 每日调用，把超过
% N 天未更新的不活跃 token 行翻转为 status=0（不物理删除，可追溯）。
%
% 形态对齐 attach_cleanup_logic（ecron 薄入口 + 配置可覆盖 + 下限钳制 +
% 错误吞掉）。幂等：deactivate_inactive 是 UPDATE 翻转，多节点/重复执行安全。
%%%

-export([run_inactive_token_cleanup/0]).
-export([run_inactive_token_cleanup/1]).

-include("log.hrl").

%% 安全下限：无论配置多小，至少保留 7 天，避免误删近期活跃的 token
-define(MIN_INACTIVE_DAYS, 7).
-define(DEFAULT_INACTIVE_DAYS, 30).

%% @doc ecron 入口：从配置读取不活跃天数后执行清理
%% 配置键 {push_token_cleanup_inactive_days, N}（IMBOY_* env 可覆盖，默认 30）
-spec run_inactive_token_cleanup() -> ok.
run_inactive_token_cleanup() ->
    Days = config_ds:env(push_token_cleanup_inactive_days, ?DEFAULT_INACTIVE_DAYS),
    run_inactive_token_cleanup(Days).

%% @doc 指定天数执行清理（钳制下限），记录结果日志；失败吞掉返回 ok
%% （ecron 作业永不 crash，下一轮继续）
-spec run_inactive_token_cleanup(pos_integer()) -> ok.
run_inactive_token_cleanup(InactiveDays0) ->
    InactiveDays = max(?MIN_INACTIVE_DAYS, InactiveDays0),
    case push_notification_ds:cleanup_inactive_tokens(InactiveDays) of
        {ok, Count} when Count > 0 ->
            ?INFO_LOG([
                "push_cleanup_logic inactive token cleanup done",
                {inactive_days, InactiveDays},
                {deactivated, Count}
            ]),
            ok;
        {ok, 0} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG([
                "push_cleanup_logic inactive token cleanup failed",
                {inactive_days, InactiveDays},
                {reason, Reason}
            ]),
            ok
    end.
