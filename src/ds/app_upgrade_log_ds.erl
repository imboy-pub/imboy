-module(app_upgrade_log_ds).
%%%
%% app_upgrade_log_ds — 客户端升级日志数据服务层
%% Client upgrade-log data service layer (thin pass-through to repo).
%%
%% 创建动机 / Motivation:
%%   G2-a 治理：`app_upgrade_log_handler` 原本直调 repo，违反 Handler → DS → Repo
%%   的 4 层架构。本模块暂时仅薄封装，后续若加入异步批写 / 版本统计缓存再扩展。
%%   (G2-a remediation; future batching/stats-cache hooks land here.)
%%%

-export([insert/1]).
-export([version_distribution/0]).
-export([event_stats/2]).

-spec insert(map()) -> {ok, term()} | {error, term()}.
insert(Data) ->
    app_upgrade_log_repo:insert(Data).

-spec version_distribution() -> [map()].
version_distribution() -> app_upgrade_log_repo:version_distribution().

-spec event_stats(binary(), binary()) -> [map()].
event_stats(StartTime, EndTime) -> app_upgrade_log_repo:event_stats(StartTime, EndTime).
