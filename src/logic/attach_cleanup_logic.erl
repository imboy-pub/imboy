-module(attach_cleanup_logic).
%%%
% 附件孤儿清理业务逻辑（供 ecron 定时任务调用）
%
% 定时触发 attachment_ds:orphan_cleanup：扫描 attachment 表中
% referer_time=0 且超龄的孤儿记录，先物理删除 Garage 对象再删库行。
% age_days 经配置管理（默认 30，IMBOY_* 可覆盖），下限 7 天兜底防误删。
%%%

-export([run_orphan_cleanup/0, run_orphan_cleanup/1]).

-include("log.hrl").

%% 安全下限：无论配置多小，至少保留 7 天，避免误删新近上传未引用的附件
-define(MIN_AGE_DAYS, 7).
-define(DEFAULT_AGE_DAYS, 30).

%% @doc ecron 入口：从配置读取 age_days 后执行清理
-spec run_orphan_cleanup() -> ok.
run_orphan_cleanup() ->
    AgeDays = config_ds:env(attachment_orphan_cleanup_age_days, ?DEFAULT_AGE_DAYS),
    run_orphan_cleanup(AgeDays).

%% @doc 指定 age_days 执行清理，记录结果日志
-spec run_orphan_cleanup(integer()) -> ok.
run_orphan_cleanup(AgeDays0) ->
    AgeDays = max(?MIN_AGE_DAYS, AgeDays0),
    case attachment_ds:orphan_cleanup(#{age_days => AgeDays}) of
        {ok, #{cleaned := Cleaned, errors := Errors}} ->
            ?INFO_LOG([
                "attach_cleanup_logic orphan_cleanup done",
                {age_days, AgeDays},
                {cleaned, Cleaned},
                {errors, Errors}
            ]),
            ok;
        {error, Reason} ->
            ?ERROR_LOG([
                "attach_cleanup_logic orphan_cleanup failed",
                {age_days, AgeDays},
                {reason, Reason}
            ]),
            ok
    end.
