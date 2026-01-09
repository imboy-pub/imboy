-module(user_setting_ds).

%%%
% user_setting_ds 是 user_setting domain service 缩写
%%%
-export([find_by_uid/1]).
-export([chat_state_hide/1]).
-export([save/3]).
-export([search/1]).

-include("log.hrl").

%% @doc 搜索用户设置
%%
%% 根据账号信息搜索用户设置，目前为占位实现
%%
%% @param Account 用户账号
%% @returns ok 占位返回值
-spec search(binary() | integer()) -> ok.
search(_Account) ->
    ok.

%% @doc 根据用户ID查找用户设置
%%
%% 从数据库中查找指定用户的设置信息，包括允许搜索状态等
%%
%% @param Uid 用户ID，可以是整数或base64编码的二进制
%% @returns map() 用户设置信息映射
% user_setting_ds:find_by_uid(1).
-spec find_by_uid(binary() | integer()) -> map().
find_by_uid(Uid) when is_binary(Uid) ->
    find_by_uid(imboy_hashids:decode(Uid));
find_by_uid(Uid) ->
    Column = <<"setting">>,
    S = case user_setting_repo:find_by_uid(Uid, Column) of
            #{<<"setting">> := Setting} when Setting =/= <<>> ->
                try jsone:decode(Setting, [{object_format, map}]) of
                    Res ->
                        Res
                catch
                    _:_ ->
                        #{}
                end;
            _ ->
                #{}
        end,
    S#{<<"allow_search">> => fts_user_repo:allow_search(Uid)}.

%% @doc 检查用户是否隐藏在线状态
%%
%% 检查用户设置中的聊天状态是否为隐藏
%%
%% @param Uid 用户ID
%% @returns true | false true表示隐藏状态，false表示显示状态
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

%% @doc 保存用户设置
%%
%% 支持多种用户设置项的保存，包括加好友方式、附近的人可见性、字体大小、聊天状态等
%%
%% 加我方式：
%%           mobile 手机号;
%%           account 账号;
%%           qrcode 二维码;
%%           group 群聊;
%%           visit_card 名片;
%%           people_nearby 附近的人
%%           recently_user 新注册的朋友
%%
%% @param Uid 用户ID
%% @param Key 设置项名称
%% @param Val 设置值
%% @returns ok 表示操作成功
% user_setting_ds:save(1, <<"add_friend_type">>, [<<"qrcode">>, <<"visit_card">>, <<"people_nearby">>]).
-spec save(binary() | integer(), binary(), term()) -> ok.
save(Uid, <<"add_friend_type">>, TypeLi) ->
    priv_save(Uid, <<"add_friend_type">>, TypeLi);
%% 设置附近的人是否可见
%% user_setting_ds:save(1, <<"people_nearby_visible">>, false).
%% user_setting_ds:save(1, <<"people_nearby_visible">>, true).
save(Uid, <<"people_nearby_visible">>, Visible) ->
    priv_save(Uid, <<"people_nearby_visible">>, Visible);
save(Uid, <<"font_size">>, State) ->
    priv_save(Uid, <<"font_size">>, State);
%% 设置聊天状态
%% State: hide online offline
%% user_setting_ds:save(CurrentUid, <<"chat_state">>, ChatState),
save(Uid, <<"chat_state">>, State) ->
    priv_save(Uid, <<"chat_state">>, State).

%% @doc 内部函数：保存用户设置
%%
%% 实际执行用户设置的更新操作
%%
%% @param Uid 用户ID
%% @param Key 设置项键名
%% @param State 设置值
%% @returns ok 表示操作成功
-spec priv_save(binary() | integer(), binary(), term()) -> ok.
priv_save(Uid, Key, State) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    Setting2 = Setting#{Key => State},
    % ?DEBUG_LOG(Setting2),
    user_setting_repo:update(Uid, Setting2),
    ok.
