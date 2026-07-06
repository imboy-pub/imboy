-module(geo_people_nearby_repo).
%%%
% geo_people_nearby 相关操作都放到该模块，存储库模块
% geo_people_nearby related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([save/3]).
-export([delete/1]).
-export([people_nearby/5]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取地理位置表的表名
%% @return 返回地理位置表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"geo_people_nearby">>).

%% @doc 保存用户地理位置信息
%% 使用 PostgreSQL PostGIS 插件存储地理位置数据
%% @param Uid 用户ID
%% @param Lat 纬度
%% @param Lng 经度
%% @return {ok, LastInsertId} 保存成功 | {error, Reason} 保存失败
%% @details 使用 UPSERT 语法，用户已存在则更新位置
-spec save(integer(), binary(), binary()) -> {ok, LastInsertId :: integer()} | {error, any()}.
save(Uid, Lat, Lng) ->
    Tb = tablename(),
    % EPSG:4326 就是 WGS84 的代码。GPS 是基于 WGS84 的，所以通常我们得到的坐标数据都是 WGS84 的
    % 使用安全的参数化查询，避免SQL注入
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (user_id, location) "
            "VALUES ($1, ST_GeomFromText('POINT(' || $2 || ' ' || $3 || ')', 4326)) "
            "ON CONFLICT (user_id) DO UPDATE SET "
            "location = EXCLUDED.location">>,
    % ?DEBUG_LOG(Sql),
    elib_pg:execute(Sql, [Uid, Lng, Lat]).

%% @doc 删除用户地理位置信息
%% @param Uid 用户ID
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
-spec delete(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete(Uid) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    elib_pg:execute(Sql, [Uid]).

%% @doc 查询附近的人
%% 使用 PostgreSQL PostGIS 插件的地理空间查询功能
%% @param Lng 中心点经度
%% @param Lat 中心点纬度
%% @param Radius 半径（米）
%% @param Unit 单位（预留参数，当前固定使用米）
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回附近用户列表（按距离排序） | {error, Reason} 查询失败
-spec people_nearby(binary(), binary(), integer(), binary(), integer()) ->
    {ok, [map()]} | {error, term()}.
people_nearby(Lng, Lat, Radius, _Unit, Limit) ->
    % 使用安全的参数化查询，避免SQL注入
    %% 点用字符串拼接构造(经度/纬度为 binary，$1/$2 在 || 中被推断为 text 正常绑定)。
    %% 注意: 勿改成 $1::float8 —— 会让 epgsql 把参数类型推断为 float8，二进制经纬度
    %% 编码失配致连接崩溃、查询返回空(2026-07 prod 事故)。
    %% 提速仅依赖 location::geography 命中 functional 索引 i_geo_people_nearby_geog，
    %% 与点如何构造无关。
    Sql = <<
        "SELECT u.id, u.account, u.nickname, u.avatar, u.sign, u.gender, u.region,\n"
        "                    ST_Distance(ST_GeographyFromText('SRID=4326;POINT(' || $1 || ' ' || $2 || ')'), location) as distance\n"
        "             FROM public.geo_people_nearby gpn\n"
        "             LEFT JOIN public.user u ON u.id = gpn.user_id\n"
        "             WHERE ST_DWithin(location::geography, ST_GeographyFromText('POINT(' || $1 || ' ' || $2 || ')'), $3)\n"
        "             ORDER BY distance ASC\n"
        "             LIMIT $4"
    >>,
    % ?DEBUG_LOG(Sql),
    elib_pg:query(Sql, [Lng, Lat, Radius, Limit]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================
