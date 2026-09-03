-module(project_event_repo).
%%%
% project_event_repo 是 project_event repository 缩写
% 项目事件数据仓库层（迁移 00000078，双体验 v2.5.2 WP4/T6a）
%
% 表结构：project_event(id TSID, project_id FK, actor_id, event_type, target_id,
%   payload jsonb, created_at)
% event_type CHECK 由 W2 迁移 00000081 扩展；本仓库提供通用事务写入，
% 当前 project_event_ds 只在 task 状态流转时写 task_status 事件。
% T6a 独占 writer 事务接口（经 project_event_ds:record_task_status_tx/6），
% T6b 只调用，不直接拼 SQL。
%%%

-export([tablename/0]).
-export([insert_tx/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"project_event">>).

%% @doc 事务内插入事件（与业务写同 Conn = 同事务；actor_id 不设 user FK，
%% 镜像 app_upgrade_log 审计惯例——审计历史不随用户删除抹除）
-spec insert_tx(any(), map()) -> {ok, integer()} | {error, term()}.
insert_tx(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(project_event),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.
