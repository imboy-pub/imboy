-module(group_member_transfer).

-export([member_list/1]).


member_list(Li) ->
    [imboy_hashids:replace_id(imboy_hashids:replace_id(M, <<"group_id">>), <<"user_id">>) || M <- Li].
