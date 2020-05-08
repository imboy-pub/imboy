-module (friend_as).
%%%
% friend_as 是 friend application service 缩写
%%%
-export ([category_friend/1]).

-include("imboy.hrl").

category_friend(Uid) ->
    Column = <<"`to_user_id`,`remark`,`category_id`">>,
    case friend_ds:find_by_uid(Uid, Column) of
        [] ->
            [];
        Friends ->
            FriendRemark = [{Id, {Remark, Cid}} || [
                {<<"to_user_id">>, Id}, {<<"remark">>, Remark},
                {<<"category_id">>, Cid}] <- Friends],
            % ?LOG(Friends),
            Uids = [Id || {Id, _} <- FriendRemark],
            Users = user_ds:find_by_ids(Uids),
            % 替换朋友备注信息
            Users2 = [replace_remark({Id, Row}, FriendRemark) || [
                {<<"id">>, Id}|_] = Row <- Users],
            % 获取用户在线状态
            Users3 = [{Id, user_ds:online_state(User)} || {Id, User} <- Users2],
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
