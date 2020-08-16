-module (friend_myfriend_aas).
%%%
% friend_myfriend_aas 是 friend_myfriend application assembler 缩写
%%%

-export ([data/3]).

-include("common.hrl").

data(User, Friend, Group) ->
    [
        {<<"mine">>, hashids_tl:encode_id(User)}
        , {<<"group">>, [hashids_tl:encode_id(F) || F <-Group]}
        , {<<"friend">>, [
        [
            {<<"id">>, hashids_tl:uid_encode(proplists:get_value(<<"id">>, GF))},
            {<<"groupname">>, proplists:get_value(<<"groupname">>, GF)}
            , {
                <<"list">>, [hashids_tl:encode_id(U) || U <-  proplists:get_value(<<"list">>, GF)]
            }
        ]
         || GF <- Friend]}
    ].
