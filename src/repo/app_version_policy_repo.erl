-module(app_version_policy_repo).
%%%
% app_version_policy 存储库模块
% 版本策略配置表的数据访问层
%%%

-export([find_by_type/1]).

-include("common.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"app_version_policy">>).

%% @doc 根据平台类型查询版本策略
%% @param Type 平台类型 <<"android">> | <<"ios">> | <<"web">>
%% @return map() 策略记录，未找到返回空 map
-spec find_by_type(binary()) -> map().
find_by_type(Type) ->
    Sql = <<"SELECT id, type, min_vsn, grayscale_enabled, check_interval_hours, status "
            "FROM ", (tablename())/binary,
            " WHERE status = 1 AND type = $1 LIMIT 1">>,
    case elib_pg:one(Sql, [Type]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 保存或更新版本策略
%% @param Data 包含策略信息的 map
%% @return {ok, any()} | {error, any()}
-spec save(map()) -> {ok, any()} | {error, any()}.
save(Data) ->
    Id = ec_cnv:to_integer(maps:get(<<"id">>, Data, 0)),
    Tb = tablename(),
    if Id > 0 ->
            Data2 = maps:remove(<<"id">>, Data),
            elib_pg:update(Tb, Data2#{<<"updated_at">> => elib_dt:now()},
                          <<"id = $1">>, [Id]);
       true ->
            NewId = elib_tsid:generate(app_version_policy),
            Data2 = maps:remove(<<"id">>, Data),
            {Sql, Params} = elib_pg_sql:insert(Tb, Data2#{<<"id">> => NewId, <<"created_at">> => elib_dt:now()}),
            case elib_pg:query(Sql, Params) of
                {ok, _Count} -> {ok, NewId};
                {error, _} = Err -> Err
            end
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
