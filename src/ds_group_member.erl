-module(ds_group_member).
%%%
% ds_group_member 是 group_member domain service 缩写
%%%
-export([user_groupid/1]).

-include("common.hrl").


-spec user_groupid(integer()) -> list().

user_groupid(Uid) ->
    repo_group_member:find_by_uid(Uid, <<"distinct `group_id`">>).
