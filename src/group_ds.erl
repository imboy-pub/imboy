-module(group_ds).
%%%
% group_ds 是 group domain service 缩写
%%%
% -export ([find_by_id/2]).
-export([check_avatar/1]).
-export([user_join_ids/1]).

-include_lib("imboy/include/log.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

% 获取用户加入的群组ID
% Uid = 1, group_ds:user_join_ids(Uid).
-spec user_join_ids(integer()) -> list().
user_join_ids(Uid) ->
    Key = {user_join_ids, Uid},
    Fun = fun() ->
        Column = <<"`group_id`">>,
        case group_member_repo:find_by_uid(Uid, Column) of
            {ok, _ColumnList, []} ->
                [];
            {ok, _ColumnList, Rows} ->
                lists:flatten(Rows)
        end
    end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).


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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------------
