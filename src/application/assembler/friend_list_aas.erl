-module (friend_list_aas).
%%%
% friend_list_aas 是 friend_list application assembler 缩写
%%%

-export ([data/2]).

data(User, Friends) ->
    [
        % {<<"mine">>, proplists:delete(<<"id">>, User)}
        % , {<<"friend">>, [proplists:delete(<<"id">>, F) || F <-Friends]}
        {<<"mine">>, User}
        , {<<"friend">>, Friends}
    ].
