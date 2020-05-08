-module (group_as).
%%%
% group_as 是 group application service 缩写
%%%
-export ([user_group/1]).
-export ([member/1]).

-include("imboy.hrl").

user_group(Uid) ->
    Column = <<"`id`,`groupname`,`avatar`">>,
    case group_member_ds:user_groupid(Uid) of
        {ok, _ColumnList, []} ->
            [];
        {ok, _ColumnList, Rows} ->
            {ok, ColumnList, Rows2} = group_repo:find_by_ids([Id || [Id] <- Rows], Column),
            [group_ds:check_avatar(lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row)) || Row <- Rows2]
    end.

member(Gid) ->
    Column = <<"`user_id`,`alias`,`description`,`role`">>,
    case group_member_repo:find_by_group_id(Gid, Column) of
            {ok, _, []} ->
                [];
            {ok, ColumnLi, Members} ->
                Uids = [Uid || [Uid, _ ,_ ,_] <- Members],
                [_|ColumnLi2] = ColumnLi,
                Members2 = [lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnLi2, Row) || [_|Row] <- Members],
                Users = user_ds:find_by_ids(Uids),
                % 获取用户在线状态
                Users2 = [user_ds:online_state(User) || User <- Users],
                % 合并 user 信息 和 member信息
                lists:zipwith(fun(X, Y) -> X ++ Y end, Users2, Members2);
            _ ->
                []
    end.
