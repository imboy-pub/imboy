-module(attach_cleanup_logic).
%%%
% 附件孤儿清理业务逻辑（供 ecron 定时任务调用）
%
% 定时触发 attachment_ds:orphan_cleanup：扫描 attachment 表中
% referer_time=0 且超龄的孤儿记录，先物理删除 Garage 对象再删库行。
% age_days 经配置管理（默认 30，IMBOY_* 可覆盖），下限 7 天兜底防误删。
%%%

-export([run_orphan_cleanup/0, run_orphan_cleanup/1]).
-export([run_pending_cleanup/0, run_pending_cleanup/1]).

-include("log.hrl").

%% 安全下限：无论配置多小，至少保留 7 天，避免误删新近上传未引用的附件
-define(MIN_AGE_DAYS, 7).
-define(DEFAULT_AGE_DAYS, 30).

%% 待确认 presign 的回收阈值（小时）。presign 有效期是分钟级，
%% 留 24 小时足够覆盖"上传成功但 confirm 请求丢了、客户端稍后重试"的场景；
%% 下限 2 小时，防止配置成 0 把正在上传的大文件删掉。
-define(MIN_PENDING_AGE_HOURS, 2).
-define(DEFAULT_PENDING_AGE_HOURS, 24).

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

%% ===================================================================
%% 待确认 presign 回收（#20）
%% ===================================================================

%% @doc ecron 入口：回收签发后从未 confirm 的对象。
%%
%% 与上面的孤儿清理是两码事，参数不可混用：
%%   orphan  = 已 confirm 落库、但长期无人引用 → 30 天，下限 7 天
%%   pending = 从未 confirm → 小时级。presign 有效期只有分钟级
%%             （attach_logic 的 ?PUT_EXPIRES），过了若干小时还没 confirm
%%             就是垃圾，没有理由留 7 天。
-spec run_pending_cleanup() -> ok.
run_pending_cleanup() ->
    AgeHours = config_ds:env(
        attachment_pending_cleanup_age_hours, ?DEFAULT_PENDING_AGE_HOURS
    ),
    run_pending_cleanup(AgeHours).

-spec run_pending_cleanup(integer()) -> ok.
run_pending_cleanup(AgeHours0) ->
    AgeHours = max(?MIN_PENDING_AGE_HOURS, AgeHours0),
    case attachment_ds:pending_cleanup(AgeHours) of
        {ok, #{cleaned := Cleaned, errors := Errors}} ->
            ?INFO_LOG([
                "attach_cleanup_logic pending_cleanup done",
                {age_hours, AgeHours},
                {cleaned, Cleaned},
                {errors, Errors}
            ]),
            ok;
        {error, Reason} ->
            ?ERROR_LOG([
                "attach_cleanup_logic pending_cleanup failed",
                {age_hours, AgeHours},
                {reason, Reason}
            ]),
            ok
    end.
