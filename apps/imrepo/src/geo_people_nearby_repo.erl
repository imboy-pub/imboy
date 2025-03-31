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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"geo_people_nearby">>).

-spec save(integer(), binary(), binary()) -> {ok, LastInsertId :: integer()} | {error, any()}.
save(Uid, Lat, Lng) ->
    Tb = tablename(),
    % EPSG:4326 就是 WGS84 的代码。GPS 是基于 WGS84 的，所以通常我们得到的坐标数据都是 WGS84 的
    Location = <<"ST_GeomFromText('POINT(", (ec_cnv:to_binary(Lng))/binary, " ", (ec_cnv:to_binary(Lat))/binary, ")', 4326)">>,
    UpSql = <<" UPDATE SET location = ", Location/binary>>,
    % Sql = <<"INSERT INTO ", Tb/binary, "
    %     (user_id, location) VALUES ($1, $2)
    %     ON CONFLICT (user_id) DO "
    %     , UpSql/binary>>,
    Sql = <<"INSERT INTO ", Tb/binary, "(user_id, location) VALUES($1, ", Location/binary,
            ") ON CONFLICT (user_id) DO ", UpSql/binary>>,
    % ?LOG(Sql),
    imboy_db:execute(Sql, [Uid]).


delete(Uid) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    imboy_db:execute(Sql, [Uid]).


-spec people_nearby(binary(), binary(), binary(), binary(), binary()) ->
          list().
people_nearby(Lng, Lat, Radius, _Unit, Limit) ->
    Sql = <<"select
    u.id
    , u.account
    , u.nickname
    , u.avatar
    , u.sign
    , u.gender
    , u.region
    , ST_AsText(location) as location
    , ST_Distance(ST_GeographyFromText('SRID=4326;POINT(", Lng/binary, " ", Lat/binary, ")'), location) as distance
    from public.geo_people_nearby gpn left join public.user u on u.id = gpn.user_id where ST_DWithin(location::geography, ST_GeographyFromText('POINT(",
            Lng/binary, " ", Lat/binary, ")'), ", (ec_cnv:to_binary(Radius))/binary, ") order by distance asc limit ", (ec_cnv:to_binary(Limit))/binary, ";">>,
    % ?LOG(Sql),
    imboy_db:query(Sql).


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
