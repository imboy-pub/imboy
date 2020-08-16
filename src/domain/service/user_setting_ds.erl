-module (user_setting_ds).
%%%
% user_setting_ds 是 user_setting domain service 缩写
%%%
-export ([find_by_uid/1]).
-export ([chat_state_hide/1]).
-export ([save_state/2]).

-include("common.hrl").

-spec find_by_uid(any()) -> list().
find_by_uid(Uid) ->
    Column = <<"`setting`">>,
    case user_setting_repo:find_by_uid(Uid, Column) of
        {ok, _ ,[]} ->
            [];
        {ok, _ ,[[Setting]]} ->
            try
                jsx:decode(Setting)
            of
                Res ->
                    Res
            catch
                _ : _ ->
                    []
            end
    end.

-spec chat_state_hide(integer()) -> true | false.
chat_state_hide(Uid) ->
    UserSetting = user_setting_ds:find_by_uid(Uid),
    case lists:keyfind(<<"chat_state">>, 1, UserSetting) of
        {<<"chat_state">>, <<"hide">>} ->
            true;
        _ ->
            false
    end.

-spec save_state(Uid::any(), State::any()) -> true .
save_state(Uid, State) ->
    UserSetting = user_setting_ds:find_by_uid(Uid),
    Setting = case lists:keyfind(<<"chat_state">>, 1, UserSetting) of
        {<<"chat_state">>, _} ->
            lists:keyreplace(<<"chat_state">>, 1, UserSetting, {<<"chat_state">>, State});
        _ ->
            [{<<"chat_state">>, State} | UserSetting]
    end,
    % ?LOG(Setting),
    user_setting_repo:update(Uid, Setting),
    true.
