-module(app_upgrade_log_repo).
%%%
% app_upgrade_log 存储库模块
% 升级事件日志表的数据访问层
%%%

-export([insert/1]).
-export([version_distribution/0]).
-export([event_stats/2]).

-include("common.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"app_upgrade_log">>).

%% @doc 插入一条升级事件日志
%% @param Data 包含事件信息的 map
%% @return {ok, integer()} | {error, any()}
-spec insert(map()) -> {ok, integer()} | {error, any()}.
insert(Data) ->
    Id = elib_tsid:generate(app_upgrade_log),
    Tb = tablename(),
    Data2 = Data#{<<"id">> => Id, <<"created_at">> => elib_dt:now()},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 查询客户端版本分布
%% 统计最近30天内每个版本的最新上报设备数
%% @return [map()] 版本分布列表
-spec version_distribution() -> [map()].
version_distribution() ->
    Tb = tablename(),
    %% 取每个 did 最后一次 check 事件的版本
    Sql = <<
        "WITH latest AS ( "
        "  SELECT DISTINCT ON (did) did, client_vsn, cos "
        "  FROM ",
        Tb/binary,
        "  WHERE event = 'check' "
        "    AND created_at > now() - interval '30 days' "
        "  ORDER BY did, created_at DESC "
        ") "
        "SELECT client_vsn, cos, COUNT(*) AS device_count "
        "FROM latest "
        "GROUP BY client_vsn, cos "
        "ORDER BY device_count DESC"
    >>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 查询指定时间范围内的升级事件统计
%% @param StartTime 开始时间 (timestamptz)
%% @param EndTime 结束时间 (timestamptz)
%% @return [map()] 按事件类型分组的统计
-spec event_stats(binary(), binary()) -> [map()].
event_stats(StartTime, EndTime) ->
    Tb = tablename(),
    Sql = <<
        "SELECT event, target_vsn, upgrade_type, COUNT(*) AS count "
        "FROM ",
        Tb/binary,
        " WHERE created_at >= $1 AND created_at <= $2 "
        "GROUP BY event, target_vsn, upgrade_type "
        "ORDER BY count DESC"
    >>,
    case elib_pg:query(Sql, [StartTime, EndTime]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
