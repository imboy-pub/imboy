-module(ds_group).
%%%
% ds_group 是 group domain service 缩写
%%%
% -export ([find_by_id/2]).
-export([check_avatar/1]).

-include("common.hrl").


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
