-module (friend_as).
%%%
% friend_as 是 friend application service 缩写
%%%
-export ([group_friend/1]).

-include("imboy.hrl").

group_friend(Uid) ->
    Groups  = friend_category_ds:find_by_uid(Uid),
    % ?LOG(Groups),
    Field = <<"`to_user_id`,`remark`,`category_id`">>,
    {ok, _FieldList, Friends} = friend_repo:find_by_uid(Uid, Field),
    FriendRemark = [{Id, {Remark, Cid}} || [Id, Remark, Cid] <- Friends],
    Uids = [Id || [Id, _Remark, _Cid] <- Friends],
    Users = user_ds:find_by_ids(Uids),
    % ?LOG(FriendRemark),
    Users2 = [replace_remark({Id, Row}, FriendRemark) || [{<<"id">>, Id}|_] = Row <- Users],
    [append_group_list(G, Users2) || G <- Groups].

append_group_list(Group, Users) ->
    {<<"id">>, Cid} = lists:keyfind(<<"id">>, 1, Group),
    List = [User || {Id, User} <- Users, Cid == Id],
    [{<<"list">>, List}|Group].

replace_remark({Uid, Row}, FriendRemark) ->
    case lists:keyfind(Uid, 1, FriendRemark) of
        {Uid, {<<>>, Cid}} ->
            {Cid, Row};
        {Uid, {Remark, Cid}} ->
            Row2 = lists:keyreplace(<<"username">>, 1, Row, {<<"username">>, Remark}),
            {Cid, Row2}
    end.
