-module(group_member_ds).
%%%
% group_member_ds 是 group_member domain service 缩写
%%%
-export([member_uids/1]).
-export([flush_cache/1]).

-include_lib("imlib/include/log.hrl").


% group_member_ds:member_uids(1).
-spec member_uids(integer()) -> list().
member_uids(Gid) ->
    Key = {group_member_uids, Gid},
    Fun = fun() ->
                  Column = <<"user_id">>,
                  {ok, _, Members} = group_member_repo:list_by_gid(Gid, Column),
                  [ Uid || {Uid} <- Members ]
          end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).

flush_cache(Gid) ->
  imboy_cache:flush({group_member_uids, Gid}).
