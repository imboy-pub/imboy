-module(aas_login_success).
%%%
% aas_login_success 是 login_success application assembler 缩写
%%%

-export([data/1]).


data([Id, Account, _, Nickname, Avatar, Gender]) ->
    [{<<"token">>, ds_token:encrypt_token(Id)},
     {<<"refreshtoken">>, ds_token:encrypt_refreshtoken(Id)},
     {<<"uid">>, hashids_translator:uid_encode(Id)},
     {<<"nickname">>, Nickname},
     {<<"avatar">>, Avatar},
     {<<"account">>, Account},
     {<<"gender">>, Gender},
     {<<"area">>, <<"">>},
     {<<"role">>, 1}].

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
