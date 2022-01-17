-module (friend_logic).
%%%
%  friend 业务逻辑模块
%%%
-export ([search/1]).
-export ([move_to_category/3]).
-export ([information/2]).
-export ([category_friend/1]).
-export ([friend_list/1]).

-include("common.hrl").

%%% 查找非好友
search(Uid) ->
    % 只能够搜索“用户被允许搜索”的用户
    %
    FriendIDs = friend_ids(Uid),
    Info = FriendIDs,
    Info.

move_to_category(CurrentUid, Uid, CategoryId) ->
    friend_repo:move_to_category(CurrentUid, Uid, CategoryId),
    ok.

information(CurrentUid, Uid) ->
    ?LOG([CurrentUid, Uid]),
    Info = [],
    Info.

friend_list(Uid) ->
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
            Users2 = [replace_remark({Id, Row}, FriendRemark, false) || [
                {<<"id">>, Id}|_] = Row <- Users],
            % 获取用户在线状态
            [user_ds:online_state(User) || {_Id, User} <- Users2]
    end.

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
            Users2 = [replace_remark({Id, Row}, FriendRemark, true) || [
                {<<"id">>, Id}|_] = Row <- Users],
            % 获取用户在线状态
            Users3 = [{Id, user_ds:online_state(User)} || {Id, User} <- Users2],
            % 把用户归并到相应的分组
            Groups  = friend_category_ds:find_by_uid(Uid),
            [append_group_list(G, Users3) || G <- Groups]
    end.

%% Internal.

%% 把用户归并到相应的分组
append_group_list(Group, Users) ->
    {<<"id">>, Cid} = lists:keyfind(<<"id">>, 1, Group),
    List = [User || {Id, User} <- Users, Cid == Id],
    [{<<"list">>, List}|Group].

%% 替换朋友备注信息
replace_remark({Uid, Row}, FriendRemark, Replace) ->
    case lists:keyfind(Uid, 1, FriendRemark) of
        {Uid, {<<>>, Cid}} ->
            {Cid, Row};
        {Uid, {Remark, Cid}} when Replace =:= true ->
            Row2 = lists:keyreplace(<<"account">>, 1, Row, {<<"account">>, Remark}),
            {Cid, Row2};
        {Uid, {Remark, Cid}} ->
            {Cid, [{<<"remark">>, Remark} | Row]}
    end.

friend_ids(Uid) ->
    Column = <<"`to_user_id`">>,
    case friend_ds:find_by_uid(Uid, Column) of
        [] ->
            [];
        Friends ->
            [ID || [{<<"to_user_id">>, ID}] <- Friends]
    end.
