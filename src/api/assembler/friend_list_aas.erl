-module (friend_list_aas).
%%%
% friend_list_aas 是 friend_list application assembler 缩写
%%%

-export ([data/2]).

data(User, Friends) ->
    [
        {<<"mine">>, hashids_translator:replace_id(User)}
        , {<<"friend">>, [hashids_translator:replace_id(F) || F <-Friends]}
        % {<<"mine">>, User}
        % , {<<"friend">>, Friends}
    ].

%% Internal.
