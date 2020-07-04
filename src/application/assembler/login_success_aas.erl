-module (login_success_aas).
%%%
% login_success_aas 是 login_success application assembler 缩写
%%%

-export ([data/1]).

data([Id, Username, _, Nickname, Avator]) ->
    [
        {<<"token">>, token_ds:encrypt_token(Id)}
        , {<<"refreshtoken">>, token_ds:encrypt_refreshtoken(Id)}
        , {<<"account">>, Username}
        , {<<"nickname">>, Nickname}
        , {<<"avator">>, Avator}
    ].
