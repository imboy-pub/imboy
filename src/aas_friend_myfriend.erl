-module(aas_friend_myfriend).
%%%
% aas_friend_myfriend 是 friend_myfriend application assembler 缩写
%%%

-export([data/3]).

-include("common.hrl").


data(User, Friend, Group) ->
    [{<<"mine">>, hashids_translator:replace_id(User)},
     {<<"group">>, [hashids_translator:replace_id(F) || F <- Group]},
     {<<"friend">>,
      [[{<<"id">>,
         hashids_translator:uid_encode(proplists:get_value(<<"id">>,
                                                           GF))},
        {<<"groupname">>, proplists:get_value(<<"groupname">>, GF)},
        {<<"list">>,
         [hashids_translator:replace_id(U) ||
             U <- proplists:get_value(<<"list">>, GF)]}] ||
          GF <- Friend]}].
