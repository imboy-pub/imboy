-module(e2ee_local_backup_ds).
%%%
%% e2ee_local_backup_ds — E2EE 本地备份数据服务层
%% E2EE local backup data service layer (thin pass-through to repo).
%%
%% 创建动机 / Motivation:
%%   G2-a 治理：`e2ee_handler` 原本直调 `e2ee_local_backup_repo`。
%%   本模块目前仅薄封装 handler 需要的两个查询/删除接口；其他接口
%%   （create / find_latest / count_by_uid 等）按 YAGNI 暂不包装，
%%   有 Logic/Handler 需要时再扩展。
%%   (G2-a remediation; scope-limited to what handler actually calls.)
%%%

-export([list_by_uid/1]).
-export([delete_by_id_and_uid/2]).
-export([find_latest/1]).

-spec find_latest(integer()) -> {ok, map()} | {error, term()}.
find_latest(Uid) ->
    e2ee_local_backup_repo:find_latest(Uid).

-spec list_by_uid(integer()) -> {ok, [map()]} | {error, term()}.
list_by_uid(Uid) ->
    e2ee_local_backup_repo:list_by_uid(Uid).

-spec delete_by_id_and_uid(integer() | binary(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_id_and_uid(BackupId, Uid) ->
    e2ee_local_backup_repo:delete_by_id_and_uid(BackupId, Uid).
