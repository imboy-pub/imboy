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
-include_lib("imboy/include/kv.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------
%%% 查找非好友

%% 让自己可见
-spec make_myself_visible(Uid::binary(), Lat::binary(), Lng::binary()) ->
    ok | {error, Msg::binary()}.
make_myself_visible(_Uid, <<"">>, _Lng) ->
    {error, <<"latitude is empty">>};
make_myself_visible(_Uid, _Lat, <<"">>) ->
    {error, <<"longitude is empty">>};
make_myself_visible(Uid, Lat, Lng) ->
    user_setting_ds:people_nearby_visible(Uid, true),
    imboy_redis:geoadd(?GEO_PEOPLE_NEARBY, Lng, Lat, Uid),
    ok.

% 让自己不可见
-spec make_myself_unvisible(Uid::binary()) ->
    ok | {error, Msg::binary()}.
make_myself_unvisible(Uid) ->
    user_setting_ds:people_nearby_visible(Uid, false),
    imboy_redis:zrem(?GEO_PEOPLE_NEARBY, Uid),
    ok.

-spec people_nearby(
    Lng::binary(), Lat::binary(),
    Radius::binary(), Unit::binary(),
    Limit::binary()
) -> ok | {error, Msg::binary()}.
people_nearby(Lng, Lat, Radius, Unit, Limit) ->
    % ?LOG([people_nearby, logic, Lng, Lat, Radius, Unit, Limit]),
    {ok, Li} = imboy_redis:georadius(?GEO_PEOPLE_NEARBY, Lng, Lat, Radius, Unit, Limit),
    % Li.
    Uids = [imboy_hashids:uid_decode(Uid) || [Uid, _Distince] <- Li],
    Users = user_logic:find_by_ids(Uids, <<"`id`,`account`,`nickname`,`avatar`,`sign`,`gender`,`region`">>),
    lists:zipwith(fun(User, [_, Distince]) -> [{<<"distince">>, Distince} | imboy_hashids:replace_id(User)] end, Users, Li).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------------

%

%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
