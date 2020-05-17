-module (user_as).
%%%
% user_as 是 user application service 缩写
%%%
-export ([do_login/2]).
-export ([refreshtoken/2]).
-export ([online/3]).
-export ([offline/1]).

-include("imboy.hrl").

do_login(Account, Pwd) ->
    Res = case imboy_func:is_mobile(Account) of
        true ->
            user_repo:find_by_mobile(Account);
        false ->
            user_repo:find_by_username(Account)
    end,
    {Check, User} = case Res of
        {ok, _FieldList, [[Id, Username, Password, Avator]]} ->
            Check0 = imboy_cipher:password_verify(Pwd, Password),
            {Check0, [Id, Username, Password, Avator]};
        _ ->
            % io:format("res is ~p~n",[Res]),
            {false, []}
    end,
    if
        Check == true ->
            {ok, login_success_aas:data(User)};
        true ->
            {error, "账号或密码错误"}
    end.

refreshtoken(Token, Refreshtoken) ->
    [Token, Refreshtoken].

-spec online(integer(), pid(), any()) -> ok.
online(Uid, Pid, Type) ->
    user_ds:online(Uid, Pid, Type),
    send_msg_to_friend(Uid, online).

-spec offline(integer()) -> ok.
offline(Uid) ->
    user_ds:offline(Uid),
    send_msg_to_friend(Uid, offline).

%% Internal.

-spec send_msg_to_friend(integer(), user_chat_state()) -> ok.
send_msg_to_friend(Uid, State) ->
    case user_setting_ds:chat_state_hide(Uid) of
        false ->
            Column = <<"`to_user_id`">>,
            case friend_ds:find_by_uid(Uid, Column) of
                [] ->
                    ok;
                Friends ->
                    send_state_msg(Uid, State, Friends),
                    ok
            end;
        true ->
            ok
    end.

-spec send_state_msg(any(), user_chat_state(), list()) -> ok.
send_state_msg(_FromId, _State, []) ->
    ok;
send_state_msg(FromId, State, [[{<<"to_user_id">>, ToUid}]| Tail]) ->
    case user_ds:is_offline(ToUid) of
        {ToUid, Pid, _Type} ->
            Msg = [
                {<<"type">>, <<"user_state">>},
                {<<"from_id">>, FromId},
                {<<"to_id">>, ToUid},
                {<<"status">>, State},
                {<<"timestamp">>, imboy_func:milliseconds()}
            ],
            erlang:start_timer(10, Pid, jsx:encode(Msg));
        _ ->
            ok
    end,
    send_state_msg(FromId, State, Tail).
