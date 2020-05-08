-module (group_member_ds).
%%%
% group_member_ds 是 group_member domain service 缩写
%%%
-export ([user_groupid/1]).

-include("imboy.hrl").

-spec user_groupid(integer()) -> list().

user_groupid(Uid) ->
    group_member_repo:find_by_uid(Uid, <<"distinct `group_id`">>).
