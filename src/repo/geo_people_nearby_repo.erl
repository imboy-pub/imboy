-module(geo_people_nearby_repo).
%%%
% geo_people_nearby 相关操作都放到该模块，存储库模块
% geo_people_nearby related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([save/3]).
-export([delete/1]).
-export([people_nearby/5]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_pg_sql:public_tablename(<<"geo_people_nearby">>).

-spec save(integer(), binary(), binary()) -> {ok, LastInsertId :: integer()} | {error, any()}.
save(Uid, Lat, Lng) ->
    Tb = tablename(),
    % EPSG:4326 就是 WGS84 的代码。GPS 是基于 WGS84 的，所以通常我们得到的坐标数据都是 WGS84 的
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"INSERT INTO ", Tb/binary, " (user_id, location) "
            "VALUES ($1, ST_GeomFromText('POINT(' || $2 || ' ' || $3 || ')', 4326)) "
            "ON CONFLICT (user_id) DO UPDATE SET "
            "location = EXCLUDED.location">>,
    % ?DEBUG_LOG(Sql),
    imboy_pg:execute(Sql, [Uid, Lng, Lat]).


delete(Uid) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    imboy_pg:execute(Sql, [Uid]).


-spec people_nearby(binary(), binary(), binary(), binary(), binary()) ->
          list().
people_nearby(Lng, Lat, Radius, _Unit, Limit) ->
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"SELECT u.id, u.account, u.nickname, u.avatar, u.sign, u.gender, u.region,
                    ST_AsText(location) as location,
                    ST_Distance(ST_GeographyFromText('SRID=4326;POINT(' || $1 || ' ' || $2 || ')'), location) as distance
             FROM public.geo_people_nearby gpn
             LEFT JOIN public.user u ON u.id = gpn.user_id
             WHERE ST_DWithin(location::geography, ST_GeographyFromText('POINT(' || $1 || ' ' || $2 || ')'), $3)
             ORDER BY distance ASC
             LIMIT $4">>,
    % ?DEBUG_LOG(Sql),
    imboy_pg:query(Sql, [Lng, Lat, Radius, Limit]).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
