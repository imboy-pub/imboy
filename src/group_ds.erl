-module(group_ds).
%%%
% group_ds 是 group domain service 缩写
%%%
% -export ([find_by_id/2]).
-export([check_avatar/1]).
-export([user_join_ids/1]).

-include_lib("imboy/include/log.hrl").


% 获取用户加入的群组ID
% Uid = 1, group_ds:user_join_ids(Uid).
-spec user_join_ids(integer()) -> list().
user_join_ids(Uid) ->
    Key = {user_join_ids, Uid},
    Val = imboy_cache:get(Key),
    user_join_ids(Val, Key, Uid).

user_join_ids({ok, List}, _K, _Uid) ->
    List;
user_join_ids(undefined, Key, Uid) ->
    Column = <<"`group_id`">>,
    case group_member_repo:find_by_uid(Uid, Column) of
        {ok, _ColumnList, []} ->
            [];
        {ok, _ColumnList, Rows} ->
            List = lists:flatten(Rows),
            imboy_cache:set(Key, List, 864000),
            List
    end.


-spec check_avatar(list()) -> list().
%% 检查 group avatar 是否为空，如果为空设置默认
check_avatar([]) ->
    [];
check_avatar(Group) ->
    Default = <<"/static/image/group_default_avatar.jpeg">>,
    case lists:keyfind(<<"avatar">>, 1, Group) of
        {<<"avatar">>, <<>>} ->
            lists:keyreplace(<<"avatar">>,
                             1,
                             Group,
                             {<<"avatar">>, Default});
        {<<"avatar">>, _Aaatar} ->
            Group
    end.
