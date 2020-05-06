-module (friend_as).
%%%
% friend_as 是 friend application service 缩写
%%%
-export ([group_friend/1]).

-include("imboy.hrl").

group_friend(Uid) ->
    Column = <<"`to_user_id`,`remark`,`category_id`">>,
    {ok, _ColumnList, Friends} = friend_repo:find_by_uid(Uid, Column),
    if
        length(Friends) == 0 ->
            [];
        true ->
            FriendRemark = [{Id, {Remark, Cid}} || [Id, Remark, Cid] <- Friends],
            % ?LOG(Friends),
            Uids = [Id || [Id, _Remark, _Cid] <- Friends],
            Users = user_ds:find_by_ids(Uids),
            % 替换朋友备注信息
            Users2 = [replace_remark({Id, Row}, FriendRemark) || [{<<"id">>, Id}|_] = Row <- Users],
            % 检查用户是否在线
            Users3 = [{Id, check_online(User)} || {Id, User} <- Users2],
            % 把用户归并到相应的分组
            Groups  = friend_category_ds:find_by_uid(Uid),
            [append_group_list(G, Users3) || G <- Groups]
    end.

%%%%

%% 把用户归并到相应的分组
append_group_list(Group, Users) ->
    {<<"id">>, Cid} = lists:keyfind(<<"id">>, 1, Group),
    List = [User || {Id, User} <- Users, Cid == Id],
    [{<<"list">>, List}|Group].

%% 替换朋友备注信息
replace_remark({Uid, Row}, FriendRemark) ->
    case lists:keyfind(Uid, 1, FriendRemark) of
        {Uid, {<<>>, Cid}} ->
            {Cid, Row};
        {Uid, {Remark, Cid}} ->
            Row2 = lists:keyreplace(<<"username">>, 1, Row, {<<"username">>, Remark}),
            {Cid, Row2}
    end.

%% 检查用户是否在线
check_online(User) ->
    {<<"id">>, Uid} = lists:keyfind(<<"id">>, 1, User),
    case user_ds:is_online(Uid) of
        {_Uid, _Pid, _Type} ->
            [{<<"status">>, <<"online">>}|User];
        false ->
            [{<<"status">>, <<"offline">>}|User]
    end.
