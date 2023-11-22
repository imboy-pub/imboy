-module(user_setting_ds).
%%%
% user_setting_ds 是 user_setting domain service 缩写
%%%
-export([find_by_uid/1]).
-export([chat_state_hide/1]).
-export([save/3]).
-export([search/1]).

-include_lib("imlib/include/log.hrl").


search(_Account) ->
    ok.


%%
% user_setting_ds:find_by_uid(1).
-spec find_by_uid(any()) -> map().
find_by_uid(Uid) when is_binary(Uid) ->
    find_by_uid(imboy_hashids:uid_decode(Uid));
find_by_uid(Uid) ->
    Column = <<"setting">>,
    case user_setting_repo:find_by_uid(Uid, Column) of
        {ok, _, []} ->
            #{};
        {ok, _, [{Setting}]} ->
            try jsone:decode(Setting, [{object_format, map}]) of
                Res ->
                    Res
            catch
                _:_ ->
                    #{}
            end
    end.


%% 检查用户是否隐藏在线状态
%% user_setting_ds:chat_state_hide(1).
-spec chat_state_hide(integer()) -> true | false.
chat_state_hide(Uid) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    case maps:get(<<"chat_state">>, Setting, false) of
        <<"hide">> ->
            true;
        _ ->
            false
    end.


%% 加我方式：
%           mobile 手机号;
%           account 账号;
%           qrcode 二维码;
%           group 群聊;
%           visit_card 名片;
%           people_nearby 附近的人
%           recently_user 新注册的朋友
%
% user_setting_ds:save(1, <<"add_friend_type">>, [<<"qrcode">>, <<"visit_card">>, <<"people_nearby">>]).
save(Uid, <<"add_friend_type">>, TypeLi) ->
    priv_save(Uid, <<"add_friend_type">>, TypeLi);

%% 设置附近的人是否可见
%% user_setting_ds:save(1, <<"people_nearby_visible">>, false).
%% user_setting_ds:save(1, <<"people_nearby_visible">>, true).
save(Uid, <<"people_nearby_visible">>, Visible) ->
    priv_save(Uid, <<"people_nearby_visible">>, Visible);

%% 设置聊天状态
%% State: hide online offline
%% user_setting_ds:save(CurrentUid, <<"chat_state">>, ChatState),
save(Uid, <<"chat_state">>, State) ->
    priv_save(Uid, <<"chat_state">>, State).


-spec priv_save(Uid :: any(), Key :: binary(), Val :: any()) -> ok.
priv_save(Uid, Key, State) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    Setting2 = Setting#{Key => State},
    % ?LOG(Setting2),
    user_setting_repo:update(Uid, Setting2),
    ok.
