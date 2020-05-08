-module (chat_myfriend_aas).
%%%
% chat_myfriend_aas 是 chat_myfriend application assembler 缩写
%%%

-export ([data/3]).

data(User, Friend, Group) ->
    [
        {<<"mine">>, [{<<"status">>, <<"1">>} | User]}
        , {<<"friend">>, Friend}
        , {<<"group">>, Group}
    ].
