-module(location_logic).
%%%
% location 业务逻辑模块
% location business logic module
%%%

-export ([make_myself_visible/3, make_myself_unvisible/1, people_nearby/5]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").
-include_lib("imboy/include/log.hrl").
-include_lib("imboy/include/cache.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% 让自己可见
-spec make_myself_visible(integer(), binary(), binary()) ->
    ok | {error, Msg::binary()}.
make_myself_visible(_Uid, <<"">>, _Lng) ->
    {error, <<"latitude is empty">>};
make_myself_visible(_Uid, _Lat, <<"">>) ->
    {error, <<"longitude is empty">>};
make_myself_visible(Uid, Lat, Lng) ->
    user_setting_ds:save(Uid, <<"people_nearby_visible">>, true),
    ?LOG([Uid, Lat, Lng]),
    geo_people_nearby_repo:save(Uid, Lat, Lng),
    ok.

% 让自己不可见
-spec make_myself_unvisible(Uid::binary()) ->
    ok | {error, Msg::binary()}.
make_myself_unvisible(Uid) ->
    user_setting_ds:save(Uid, <<"people_nearby_visible">>, false),
    geo_people_nearby_repo:delete(Uid),
    ok.

-spec people_nearby(
    Lng::binary(), Lat::binary(),
    Radius::binary(), Unit::binary(),
    Limit::binary()
) -> list().
people_nearby(Lng, Lat, Radius, <<"km">>, Limit) ->
    RadiusM = binary_to_integer(Radius) * 1000,
    people_nearby(Lng, Lat, integer_to_binary(RadiusM), <<"m">>, Limit);
people_nearby(Lng, Lat, Radius, Unit, Limit) ->
    % ?LOG([people_nearby, logic, Lng, Lat, Radius, Unit, Limit]),
    % geo_people_nearby_repo:people_nearby(<<"113.88308">>, <<"22.55328">>, <<"10000000">>, <<"m">>,  <<"10">>).
    {ok, _, Li}  = geo_people_nearby_repo:people_nearby(Lng, Lat, Radius, Unit,  Limit),
    [imboy_hashids:replace_id([
        {<<"id">>, Id}
        , {<<"account">>, Account}
        , {<<"nickname">>, Nickname}
        , {<<"avatar">>, Avatar}
        , {<<"sign">>, Sign}
        , {<<"gender">>, Gender}
        , {<<"region">>, Region}
        , {<<"distance">>, Distance}
        , {<<"unit">>, Unit}
        ]) || {Id, Account, Nickname, Avatar, Sign, Gender, Region, _, Distance} <- Li].

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
