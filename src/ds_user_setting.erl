-module(ds_user_setting).
%%%
% ds_user_setting 是 user_setting domain service 缩写
%%%
-export([find_by_uid/1]).
-export([chat_state_hide/1]).
-export([save_state/2]).
-export([search/1]).

-include("common.hrl").


search(_Account) ->
    ok.


-spec find_by_uid(any()) -> list().
find_by_uid(Uid) ->
    Column = <<"`more`">>,
    case repo_user_setting:find_by_uid(Uid, Column) of
        {ok, _, []} ->
            [];
        {ok, _, [[Setting]]} ->
            try
                jsone:decode(Setting, [{object_format, proplist}])
            of
                Res ->
                    Res
            catch
                _:_ ->
                    []
            end
    end.


-spec chat_state_hide(integer()) -> true | false.
chat_state_hide(Uid) ->
    UserSetting = ds_user_setting:find_by_uid(Uid),
    case lists:keyfind(<<"chat_state">>, 1, UserSetting) of
        {<<"chat_state">>, <<"hide">>} ->
            true;
        _ ->
            false
    end.


-spec save_state(Uid :: any(), State :: any()) -> true.
save_state(Uid, State) ->
    UserSetting = ds_user_setting:find_by_uid(Uid),
    Setting =
        case lists:keyfind(<<"chat_state">>, 1, UserSetting) of
            {<<"chat_state">>, _} ->
                lists:keyreplace(<<"chat_state">>,
                                 1,
                                 UserSetting,
                                 {<<"chat_state">>, State});
            _ ->
                [{<<"chat_state">>, State} | UserSetting]
        end,
    % ?LOG(Setting),
    repo_user_setting:update(Uid, Setting),
    true.
