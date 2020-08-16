-module (login_success_aas).
%%%
% login_success_aas 是 login_success application assembler 缩写
%%%

-export ([data/1]).

data([Id, Account, Nickname, Avator, Gender]) ->
    [
        {<<"token">>, token_ds:encrypt_token(Id)}
        , {<<"refreshtoken">>, token_ds:encrypt_refreshtoken(Id)}
        , {<<"uid">>, hashids_tl:uid_encode(Id)}
        , {<<"account">>, Account}
        , {<<"nickname">>, Nickname}
        , {<<"avator">>, Avator}
        , {<<"gender">>, Gender}
    ].
