-module (login_success_aas).
%%%
% login_success_aas 是 login_success application assembler 缩写
%%%

-export ([data/1]).

data([Id, Account, Nickname, Avatar, Gender]) ->
    [
        {<<"token">>, token_ds:encrypt_token(Id)}
        , {<<"refreshtoken">>, token_ds:encrypt_refreshtoken(Id)}
        , {<<"uid">>, hashids_translator:uid_encode(Id)}
        , {<<"nickname">>, Nickname}
        , {<<"avatar">>, Avatar}
        , {<<"account">>, Account}
        , {<<"gender">>, Gender}
        , {<<"area">>, <<"">>}
        , {<<"role">>, 1}
    ].
