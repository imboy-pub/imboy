-module(location_logic).
%%%
% location 业务逻辑模块
% location business logic module
%%%

-export([make_myself_visible/3,
         make_myself_unvisible/1,
         people_nearby/6]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("log.hrl").
-include("cache.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% 让自己可见
-spec make_myself_visible(integer(), binary(), binary()) -> ok | {error, Msg :: binary()}.
make_myself_visible(_Uid, <<>>, _Lng) ->
    {error, <<"latitude is empty">>};
make_myself_visible(_Uid, _Lat, <<>>) ->
    {error, <<"longitude is empty">>};
make_myself_visible(Uid, Lat, Lng) ->
    user_setting_ds:save(Uid, <<"people_nearby_visible">>, true),
    % ?DEBUG_LOG([Uid, Lat, Lng]),
    _ = geo_people_nearby_repo:save(Uid, Lat, Lng),
    ok.


% 让自己不可见
-spec make_myself_unvisible(Uid :: integer()) -> ok | {error, Msg :: binary()}.
make_myself_unvisible(Uid) ->
    user_setting_ds:save(Uid, <<"people_nearby_visible">>, false),
    _ = geo_people_nearby_repo:delete(Uid),
    ok.


-spec people_nearby(integer(), binary(), binary(), binary() | integer(), binary(), integer()) ->
          [[tuple()] | map()].
people_nearby(CurrentUid, Lng, Lat, Radius, <<"km">>, Limit) when is_binary(Radius) ->
    RadiusM = binary_to_integer(Radius) * 1000,
    people_nearby(CurrentUid, Lng, Lat, RadiusM, <<"m">>, Limit);
people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit) ->
    % ?DEBUG_LOG([people_nearby, logic, Lng, Lat, Radius, Unit, Limit]),
    % geo_people_nearby_repo:people_nearby(<<"113.88308">>, <<"22.55328">>, 10000000, <<"m">>,  10).
    {ok, Li} = geo_people_nearby_repo:people_nearby(Lng, Lat, Radius, Unit, Limit),
    % {Id, Account, Nickname, Avatar, Sign, Gender, Region, Location, Distance} <- Li .
    % 为每个用户检查好友关系，并添加 remark 和 created_at 信息
    lists:map(fun(Row) ->
        case maps:get(<<"id">>, Row, undefined) of
            UserId when is_integer(UserId) ->
                {IsFriend, FieldsMap} = friend_ds:is_friend_fields(CurrentUid, UserId, [<<"remark">>, <<"created_at">>]),
                {Remark, CreatedAt} = if
                    IsFriend ->
                        % 如果是好友，从 FieldsMap 中获取 remark 和 created_at
                        {maps:get(<<"remark">>, FieldsMap, <<>>), maps:get(<<"created_at">>, FieldsMap, 0)};
                    true ->
                        {<<>>, 0}
                end,
                Data = #{
                    <<"id">> => imboy_hashids:encode(UserId),
                    <<"is_friend">> => IsFriend,
                    <<"remark">> => Remark,
                    <<"friend_created_at">> => CreatedAt
                },
                maps:merge(Row, Data);
            _ ->
                Row
        end
    end, Li).


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
