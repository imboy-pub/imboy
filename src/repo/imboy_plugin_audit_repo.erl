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

-export([insert/1]).
-export([list/3, count/1]).

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
    Fields = maps:merge(
        #{
            id => Id
        },
        Row
    ),
    {Sql, Params} = elib_pg_sql:insert(tablename(), Fields),
    case elib_pg:query(Sql, Params) of
        {ok, _} ->
            {ok, Id};
        {error, Reason} ->
            ?ERROR_LOG(["plugin_audit_log insert failed", Reason]),
            {error, Reason}
    end.

%% @doc 查询审计日志。PluginName 为空则不按插件过滤。
%% @param PluginName 插件名称（<<>> 表示全部）
%% @param Limit 每页条数
%% @param Offset 偏移量
%% @returns {ok, List} 或 {error, Reason}
-spec list(binary(), non_neg_integer(), non_neg_integer()) -> {ok, list()} | {error, term()}.
list(PluginName, Limit, Offset) ->
    {Where, Params} = plugin_filter(PluginName, 1),
    LimitIdx = length(Params) + 1,
    OffsetIdx = length(Params) + 2,
    Sql =
        <<"SELECT * FROM ", (tablename())/binary, Where/binary, " ORDER BY started_at DESC",
            " LIMIT $", (integer_to_binary(LimitIdx))/binary, " OFFSET $",
            (integer_to_binary(OffsetIdx))/binary>>,
    elib_pg:query(Sql, Params ++ [Limit, Offset]).

%% @doc 统计审计日志总数。PluginName 为空则统计全部。
-spec count(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count(PluginName) ->
    {Where, Params} = plugin_filter(PluginName, 1),
    Sql = <<"SELECT COUNT(*) AS count FROM ", (tablename())/binary, Where/binary>>,
    case elib_pg:query(Sql, Params) of
        {ok, [#{<<"count">> := Count}]} -> {ok, Count};
        {ok, _} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

%% 空插件名 → 无 WHERE；否则按 plugin_name 过滤。
plugin_filter(<<>>, _Idx) ->
    {<<"">>, []};
plugin_filter(PluginName, Idx) ->
    {<<" WHERE plugin_name = $", (integer_to_binary(Idx))/binary>>, [PluginName]}.

%% EUnit tests.
