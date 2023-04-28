-module(group_logic).
%%%
% group 业务逻辑模块
%%%
-export([member/1]).

-include_lib("imboy/include/log.hrl").


member(Gid) ->
    Column = <<"`user_id`,`alias`,`description`,`role`">>,
    case group_member_repo:find_by_gid(Gid, Column) of
        {ok, _, []} ->
            [];
        {ok, ColumnLi, Members} ->
            Uids = [Uid || [Uid, _, _, _] <- Members],
            [_ | ColumnLi2] = ColumnLi,
            Members2 = [lists:zipwith(fun(X, Y) -> {X, Y} end,
                                      ColumnLi2,
                                      Row) || [_ | Row] <- Members],
            Users = user_logic:find_by_ids(Uids),
            % 获取用户在线状态
            Users2 = [user_logic:online_state(User) || User <- Users],
            % 合并 user 信息 和 member信息
            lists:zipwith(fun(X, Y) -> X ++ Y end, Users2, Members2);
        _ ->
            []
    end.
