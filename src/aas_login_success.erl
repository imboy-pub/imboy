-module(aas_login_success).
%%%
% aas_login_success 是 login_success application assembler 缩写
%%%

-export([data/2]).

-spec data(boolean(), list()) ->
    {ok, any()} | {error, any()}.
data(Check, [Id, Account, _, Nickname, Avatar, Gender]) when Check =:= true ->
    {ok, [
        {<<"token">>, ds_token:encrypt_token(Id)},
        {<<"refreshtoken">>, ds_token:encrypt_refreshtoken(Id)},
        {<<"uid">>, hashids_translator:uid_encode(Id)},
        {<<"nickname">>, Nickname},
        {<<"avatar">>, Avatar},
        {<<"account">>, Account},
        {<<"gender">>, Gender},
        {<<"area">>, <<"">>},
        {<<"role">>, 1}]
    };
data(_, _) ->
    {error, "账号或密码错误"}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
