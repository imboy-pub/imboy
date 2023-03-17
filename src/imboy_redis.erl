-module(imboy_redis).

-include_lib("imboy/include/kv.hrl").
-include_lib("imboy/include/log.hrl").

-export([geoadd/4, zrem/2, georadius/6]).


-spec geoadd(Key::atom(), Lng::binary(), Lat::binary(), Member::binary()) -> ok.
geoadd(Key, Lng, Lat, Member) ->
    C = connect(),
    % GEOADD key longitude latitude member [longitude latitude member ...]
    _Res = eredis:q(C, ["GEOADD", Key, Lng, Lat, Member]),
    eredis:stop(C),
    ok.

zrem(Key, Member) ->
    C = connect(),
    % ZREM key member [member ...]
    Res = eredis:q(C, ["ZREM", Key, Member]),
    ?LOG([zrem, Res]),
    eredis:stop(C),
    ok.

-spec georadius(Key::atom(), Lng::binary(), Lat::binary(), Radius::binary(), Unit::binary(), Limit::binary()) ->
          {ok, nonempty_list()} | {error, Reason::binary() | no_connection}.
georadius(Key, Lng, Lat, Radius, Unit, Limit) ->
    C = connect(),
    % https://www.runoob.com/redis/redis-geo.html
    % GEORADIUS key longitude latitude radius m|km|ft|mi [WITHCOORD] [WITHDIST] [WITHHASH] [COUNT count] [ASC|DESC] [STORE key] [STOREDIST key]
    % WITHDIST: 在返回位置元素的同时， 将位置元素与中心之间的距离也一并返回。
    % ASC: 查找结果根据距离从近到远排序
    Res = eredis:q(C, ["GEORADIUS", Key, Lng, Lat, Radius, Unit, "WITHDIST", "ASC", "COUNT", Limit]),
    % ?LOG([georadius, Res]),
    eredis:stop(C),
    Res.


-spec connect() -> pid().
connect() ->
    % Options = [{password, ""}, {database, 0}, {host, "127.0.0.1"}, {port, 6379}],
    Options = imboy_func:env(redis_options),
    {ok, C} = eredis:start_link(Options),
    C.
