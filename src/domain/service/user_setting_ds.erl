-module (user_setting_ds).
%%%
% user_setting_ds 是 user_setting domain service 缩写
%%%
-export ([chat_state_hide/1]).

-include("imboy.hrl").

-spec chat_state_hide(integer()) -> true | false.
chat_state_hide(Uid) ->
    Column = <<"`setting`">>,
    case user_setting_repo:find_by_uid(Uid, Column) of
        {ok, _ ,[]} ->
            false;
        {ok, _ ,[[Setting]]} ->
            try
                Setting2 = jsx:decode(Setting),
                {<<"chat_state">>, State} = lists:keyfind(<<"chat_state">>, 1, Setting2),
                State
            of
                Res ->
                    case Res of
                        <<"hide">> ->
                            true;
                        _ ->
                            false
                    end
            catch
                _ : _ ->
                    false
            end
    end.
