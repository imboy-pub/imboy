-module (friend_myfriend_aas).
%%%
% friend_myfriend_aas 是 friend_myfriend application assembler 缩写
%%%

-export ([data/3]).

data(User, Friend, Group) ->
    [
        {<<"mine">>, User}
        , {<<"friend">>, Friend}
        , {<<"group">>, Group}
    ].
