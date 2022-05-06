-module(logic_group).
%%%
% group 业务逻辑模块
%%%
-export([user_group/1]).
-export([member/1]).

-include("common.hrl").


user_group(Uid) ->
    Column = <<"`id`,`groupname`,`avatar`">>,
    case ds_group_member:user_groupid(Uid) of
        {ok, _ColumnList, []} ->
            [];
        {ok, _ColumnList, Rows} ->
            {ok, ColumnList, Rows2} = repo_group:find_by_ids([Id ||
                                                                 [Id] <- Rows],
                                                             Column),
            [ds_group:check_avatar(lists:zipwith(fun(X, Y) ->
                                                        {X, Y}
                                                 end,
                                                 ColumnList,
                                                 Row)) || Row <- Rows2]
    end.


member(Gid) ->
    Column = <<"`user_id`,`alias`,`description`,`role`">>,
    case repo_group_member:find_by_group_id(Gid, Column) of
        {ok, _, []} ->
            [];
        {ok, ColumnLi, Members} ->
            Uids = [Uid || [Uid, _, _, _] <- Members],
            [_ | ColumnLi2] = ColumnLi,
            Members2 = [lists:zipwith(fun(X, Y) -> {X, Y} end,
                                      ColumnLi2,
                                      Row) || [_ | Row] <- Members],
            Users = ds_user:find_by_ids(Uids),
            % 获取用户在线状态
            Users2 = [ds_user:online_state(User) || User <- Users],
            % 合并 user 信息 和 member信息
            lists:zipwith(fun(X, Y) -> X ++ Y end, Users2, Members2);
        _ ->
            []
    end.
