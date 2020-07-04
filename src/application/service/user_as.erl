-module (user_as).
%%%
% user_as 是 user application service 缩写
%%%
-export ([do_login/2]).
-export ([refreshtoken/2]).
-export ([online/3]).
-export ([offline/2]).
-export ([idle_timeout/1]).

-include("imboy.hrl").

do_login(Account, Pwd) ->
    Res = case imboy_func:is_mobile(Account) of
        true ->
            user_repo:find_by_mobile(Account);
        false ->
            user_repo:find_by_account(Account)
    end,
    {Check, User} = case Res of
        {ok, _FieldList, [[Id, Username, Password, Nickname, Avator]]} ->
            Check0 = imboy_cipher:password_verify(Pwd, Password),
            {Check0, [Id, Username, Password, Nickname, Avator]};
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

-spec online(any(), pid(), any()) -> ok.
online(Uid, Pid, Type) ->
    case user_ds:is_offline(Uid) of
        {ToPid, _Uid, _Type} ->
            Msg = [
                {<<"type">>, <<"error">>},
                {<<"from_id">>, Uid},
                {<<"to_id">>, Uid},
                {<<"code">>, 786},
                {<<"msg">>, unicode:characters_to_binary("在其他地方上线")},
                {<<"timestamp">>, imboy_func:milliseconds()}
            ],
            erlang:start_timer(10, ToPid, jsx:encode(Msg));
        true ->
            ok
    end,
    user_ds:online(Uid, Pid, Type),

    % 检查离线消息 用异步队列实现
    gen_server:cast(offline_server, {online, Uid, Pid}),
    ok.

-spec offline(any(), pid()) -> ok.
offline(Uid, Pid) ->
    user_ds:offline(Pid),
    % 检查离线消息 用异步队列实现
    gen_server:cast(offline_server, {offline, Uid, Pid}).

% 设置用户websocket超时时间，默认60秒
idle_timeout(_UId) ->
    60000.
