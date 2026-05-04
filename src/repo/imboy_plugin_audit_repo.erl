-module(imboy_plugin_audit_repo).

%%%
% 插件审计日志数据访问层
% Plugin audit log data access layer
% lifecycle.md §11
%%%

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-export([tablename/0]).
-export([insert/1]).
-export([list/3]).

%% @doc 获取审计日志表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"plugin_audit_log">>).

%% @doc 插入审计日志记录
%% @param Row 字段 map（plugin_name, event, started_at 等）
%% @returns {ok, Id} 或 {error, Reason}
-spec insert(map()) -> {ok, pos_integer()} | {error, term()}.
insert(Row) when is_map(Row) ->
    Id = elib_tsid:generate(plugin_audit_log),
    Fields = maps:merge(#{
        id => Id
    }, Row),
    {Sql, Params} = elib_pg_sql:insert(tablename(), Fields),
    case elib_pg:query(Sql, Params) of
        {ok, _} -> {ok, Id};
        {error, Reason} ->
            ?ERROR_LOG(["plugin_audit_log insert failed", Reason]),
            {error, Reason}
    end.

%% @doc 查询审计日志
%% @param PluginName 插件名称
%% @param Limit 每页条数
%% @param Offset 偏移量
%% @returns {ok, List} 或 {error, Reason}
-spec list(binary(), pos_integer(), pos_integer()) -> {ok, list()} | {error, term()}.
list(PluginName, Limit, Offset) ->
    Sql = <<"SELECT * FROM ", (tablename())/binary,
            " WHERE plugin_name = $1 ORDER BY started_at DESC",
            " LIMIT $2 OFFSET $3">>,
    elib_pg:query(Sql, [PluginName, Limit, Offset]).

%% EUnit tests.
