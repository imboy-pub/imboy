-module (myfriend_aas).
%%%
% myfriend_aas 是 myfriend application assembler 缩写
%%%

-export ([data/2]).

data(User, Friends) ->
    [
        {<<"mine">>, [{<<"status">>, <<"1">>} | User]}
        , {<<"friend">>, Friends}
    ].
