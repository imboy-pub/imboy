-module(location_logic).
%%%
% location 业务逻辑模块
% location business logic module
%%%

-export([
    make_myself_visible/3,
    make_myself_unvisible/1,
    people_nearby/6
]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("log.hrl").
-include("cache.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 设置用户位置可见
%% 将用户位置信息保存到数据库，允许被附近的人搜索到
%% @param Uid 用户ID
%% @param Lat 纬度
%% @param Lng 经度
%% @return ok | {error, Reason}
-spec make_myself_visible(integer(), binary(), binary()) -> ok | {error, binary()}.
make_myself_visible(_Uid, <<>>, _Lng) ->
    {error, <<"纬度不能为空"/utf8>>};
make_myself_visible(_Uid, _Lat, <<>>) ->
    {error, <<"经度不能为空"/utf8>>};
make_myself_visible(Uid, Lat, Lng) ->
    user_setting_ds:save(Uid, <<"people_nearby_visible">>, true),
    _ = geo_people_nearby_ds:save(Uid, Lat, Lng),
    ok.

%% @doc 设置用户位置不可见
%% 删除用户位置信息，不允许被附近的人搜索到
%% @param Uid 用户ID
%% @return ok | {error, Reason}
-spec make_myself_unvisible(integer()) -> ok | {error, binary()}.
make_myself_unvisible(Uid) ->
    user_setting_ds:save(Uid, <<"people_nearby_visible">>, false),
    _ = geo_people_nearby_ds:delete(Uid),
    ok.

%% @doc 查询附近的人
%% 基于地理位置查询附近用户，并检查好友关系
%% @param CurrentUid 当前用户ID
%% @param Lng 经度
%% @param Lat 纬度
%% @param Radius 半径（支持数字或带单位的binary）
%% @param Unit 单位（km 或 m）
%% @param Limit 返回数量限制
%% @return list() 附近用户列表，包含好友关系信息
-spec people_nearby(integer(), binary(), binary(), binary() | integer(), binary(), integer()) ->
    list().
people_nearby(CurrentUid, Lng, Lat, Radius, <<"km">>, Limit) when is_binary(Radius) ->
    try binary_to_integer(Radius) of
        RadiusKm ->
            people_nearby(CurrentUid, Lng, Lat, RadiusKm * 1000, <<"m">>, Limit)
    catch
        _:_ ->
            ok = ?WARN_LOG([people_nearby, invalid_radius, Radius]),
            []
    end;
people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit) ->
    %% Lng/Lat 为 handler 透传的未校验 query（可能非数字或 undefined），PostGIS 解析
    %% 失败时 ds 返回 {error,_}，此处 case 兜底避免 badmatch 崩溃 → 500
    case geo_people_nearby_ds:people_nearby(Lng, Lat, Radius, Unit, Limit) of
        {ok, Li} ->
            % {Id, Account, Nickname, Avatar, Sign, Gender, Region, Location, Distance} <- Li .
            % 一次批量查好友关系(替代逐行 N+1)，再为每个用户填充 remark / created_at
            UserIds = lists:filtermap(
                fun(Row) ->
                    case maps:get(<<"id">>, Row, undefined) of
                        Uid when is_integer(Uid) -> {true, Uid};
                        _ -> false
                    end
                end,
                Li
            ),
            FriendMap = friend_ds:friends_fields_map(
                CurrentUid, UserIds, [<<"remark">>, <<"created_at">>]
            ),
            lists:map(
                fun(Row) ->
                    case maps:get(<<"id">>, Row, undefined) of
                        UserId when is_integer(UserId) ->
                            {IsFriend, Remark, CreatedAt} =
                                case maps:get(UserId, FriendMap, undefined) of
                                    undefined ->
                                        {false, <<>>, 0};
                                    FieldsMap ->
                                        {
                                            true,
                                            maps:get(<<"remark">>, FieldsMap, <<>>),
                                            maps:get(<<"created_at">>, FieldsMap, 0)
                                        }
                                end,
                            Data = #{
                                <<"id">> => UserId,
                                <<"is_friend">> => IsFriend,
                                <<"remark">> => Remark,
                                <<"friend_created_at">> => CreatedAt
                            },
                            maps:merge(Row, Data);
                        _ ->
                            Row
                    end
                end,
                Li
            );
        Err ->
            ok = ?WARN_LOG([people_nearby, query_failed, Err]),
            []
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================
