-module (user_as).
%%%
% user_as 是 user application service 缩写
%%%
-export ([do_login/2, refreshtoken/2]).

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
