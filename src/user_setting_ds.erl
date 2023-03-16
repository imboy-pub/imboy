-module(user_setting_ds).
%%%
% user_setting_ds 是 user_setting domain service 缩写
%%%
-export([find_by_uid/1]).
-export([chat_state_hide/1, save_state/2, people_nearby_visible/2]).
-export([search/1]).

-include_lib("imboy/include/log.hrl").


search(_Account) ->
    ok.

-spec find_by_uid(any()) -> map().
find_by_uid(Uid) when is_binary(Uid) ->
    find_by_uid(imboy_hashids:uid_decode(Uid));
find_by_uid(Uid) ->
    Column = <<"`setting`">>,
    case user_setting_repo:find_by_uid(Uid, Column) of
        {ok, _, []} ->
            #{};
        {ok, _, [[Setting]]} ->
            try
                jsone:decode(Setting, [{object_format, map}])
            of
                Res ->
                    Res
            catch
                _:_ ->
                    #{}
            end
    end.

% user_setting_ds:people_nearby_visible(1, false);
-spec people_nearby_visible(Uid::binary(), Visible::atom()) -> true | false.
people_nearby_visible(Uid, Visible) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    Setting2 = Setting#{<<"people_nearby_visible">> => Visible},
   % ?LOG([Setting, Setting2]),
    user_setting_repo:update(Uid, Setting2),
    true.

%  检查用户是否隐藏在线状态
% user_setting_ds:chat_state_hide(1).
-spec chat_state_hide(integer()) -> true | false.
chat_state_hide(Uid) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    case maps:get(<<"chat_state">>, Setting, false) of
        <<"hide">> ->
            true;
        _ ->
            false
    end.

% State: hide online offline
-spec save_state(Uid :: any(), State :: any()) -> true.
save_state(Uid, State) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    Setting2 = Setting#{<<"chat_state">> => State},
   % ?LOG(Setting2),
    user_setting_repo:update(Uid, Setting2),
    true.
