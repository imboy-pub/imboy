-module(geo_people_nearby_ds).
%%%
% geo_people_nearby_ds 是地理位置数据服务层
% 封装附近的人的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([save/3]).
-export([delete/1]).
-export([people_nearby/5]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 保存用户地理位置信息
%% 使用 PostgreSQL PostGIS 插件存储地理位置数据
%% @param Uid 用户ID
%% @param Lat 纬度
%% @param Lng 经度
%% @return {ok, Result} | {error, Reason}
-spec save(integer(), binary(), binary()) -> {ok, any()} | {error, any()}.
save(Uid, Lat, Lng) ->
    geo_people_nearby_repo:save(Uid, Lat, Lng).

%% @doc 删除用户地理位置信息
%% @param Uid 用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(Uid) ->
    geo_people_nearby_repo:delete(Uid).

%% @doc 查询附近的人
%% 使用 PostgreSQL PostGIS 插件的地理空间查询功能
%% @param Lng 中心点经度
%% @param Lat 中心点纬度
%% @param Radius 半径（米）
%% @param Unit 单位（预留参数，当前固定使用米）
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} | {error, Reason}
-spec people_nearby(binary(), binary(), integer(), binary(), integer()) ->
          {ok, [map()]} | {error, any()}.
people_nearby(Lng, Lat, Radius, Unit, Limit) ->
    geo_people_nearby_repo:people_nearby(Lng, Lat, Radius, Unit, Limit).

%% ===================================================================
%% Internal Functions
%% ===================================================================
