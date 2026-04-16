-module(group_member_ds).
%%%
% group_member_ds 是群组成员数据服务层
% 封装群组成员的数据操作和事务管理
%%%

-include("log.hrl").
-include("common.hrl").

-export([list_member/2]).
-export([list_members/1]).
-export([list_members_with_info/1]).
-export([add_member/2]).
-export([join_group/5]).
-export([leave/4]).
-export([alias/4]).
-export([update_role/4]).
-export([update_role/5]).
-export([update_mute/4]).
-export([get_member_info/3]).
-export([update_statistics/2]).
-export([find_by_gid_and_uid/3]).
-export([is_member/2]).
-export([check_admin/2]).
-export([list_same_group/2]).
-export([count_by_uid/1]).
-export([page_by_gid/4]).
-export([page_with_user_info/3]).
-export([update_remark/3]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 兼容旧测试接口：获取群成员列表
-spec list_members(integer()) -> {ok, list(map())} | {error, any()}.
list_members(Gid) ->
    list_member(Gid, []).

%% @doc 兼容旧测试接口：获取群成员列表（包含用户信息）
-spec list_members_with_info(integer()) -> {ok, list(map())} | {error, any()}.
list_members_with_info(Gid) ->
    list_member(Gid, []).

%% @doc 兼容旧逻辑接口：检查是否为群成员
-spec is_member(integer(), integer()) -> boolean().
is_member(Gid, Uid) ->
    case group_member_repo:find(Gid, Uid, <<"id">>) of
        #{<<"id">> := _Id} -> true;
        _ -> false
    end.

%% @doc 兼容旧测试接口：添加群成员（事务内复用 join_group）
-spec add_member(integer(), integer()) -> ok | {error, any()}.
add_member(Gid, Uid) ->
    case elib_pg:with_tx(fun(Conn) ->
        join_group(Conn, <<"invite">>, Uid, Gid, #{role => 1})
    end) of
        {ok, _UidSum} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取群成员列表（包含用户信息）
%% @param Gid 群组ID
%% @param MemberUids 用户ID列表（可选，空列表表示获取所有成员）
%% @return {ok, Rows} | {error, Reason}
-spec list_member(integer(), list()) -> {ok, list(map())} | {error, any()}.
list_member(Gid, []) ->
    GMTb = group_member_repo:tablename(),
    UserTb = user_repo:tablename(),
    Column = <<"u.nickname,u.account,u.avatar,u.sign, gm.*">>,
    Sql = <<"SELECT ", Column/binary,
            " FROM ", GMTb/binary," gm LEFT JOIN ", UserTb/binary,
            " u ON u.id = gm.user_id WHERE gm.group_id = $1 ORDER BY id DESC LIMIT $2">>,
    elib_pg:query(Sql, [Gid, 50000]);
list_member(Gid, MemberUids) when length(MemberUids) > 0 ->
    GMTb = group_member_repo:tablename(),
    UserTb = user_repo:tablename(),
    Column = <<"u.nickname,u.account,u.avatar,u.sign, gm.*">>,
    Placeholders = elib_pg_sql:placeholders(length(MemberUids)),
    Sql = <<"SELECT ", Column/binary,
            " FROM ", GMTb/binary," gm LEFT JOIN ", UserTb/binary,
            " u ON u.id = gm.user_id WHERE gm.group_id = $1 AND gm.user_id IN (",
            Placeholders/binary, ")">>,
    elib_pg:query(Sql, [Gid | MemberUids]);
list_member(_Gid, _) ->
    {ok, []}.

%% @doc 用户加入群组（事务版本）
%% @param Conn 数据库连接
%% @param JoinMode 加入模式: <<"invite">> 邀请, <<"apply">> 申请, <<"qrcode">> 二维码
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @param OptData 选项数据
%% @return {ok, UidSum} | {error, Reason}
-spec join_group(pid(), binary(), integer(), integer(), map()) -> {ok, integer()} | {error, binary()}.
join_group(Conn, JoinMode, Uid, Gid, OptData) ->
    Role = maps:get(role, OptData, 1),
    _GMTb = group_member_repo:tablename(),

    % 检查是否已是成员
    case group_member_repo:find(Gid, Uid, <<"id">>) of
        #{<<"id">> := _Id} ->
            {ok, 0}; % 已是成员，幂等返回
        _ ->
            Now = elib_dt:now(),
            % 插入群成员
            case group_member_repo:add(Conn, #{
                group_id => Gid,
                user_id => Uid,
                role => Role,
                is_join => 1,
                join_mode => elib_str:trunc(JoinMode, 100),
                created_at => Now
            }) of
                {ok, _} ->
                    % 更新群组统计
                    case update_statistics(Conn, Gid) of
                        {ok, UidSum} ->
                            % 更新内存缓存
                            group_ds:join(Uid, Gid),
                            {ok, UidSum};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 用户离开群组（事务版本）
%% @param Conn 数据库连接
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @param CurrentUid 当前操作用户ID
%% @return {ok, UidSum, MemberInfo} | {error, Reason}
-spec leave(pid(), integer(), integer(), integer()) -> {ok, integer(), map()} | {error, any()}.
leave(Conn, Uid, Gid, CurrentUid) ->
    GMTb = group_member_repo:tablename(),
    GM = group_member_repo:find(Gid, Uid, <<"*">>),

    case maps:size(GM) of
        0 ->
            {ok, 0, #{}};
        _ ->
            Now = elib_dt:now(),
            Id = maps:get(<<"id">>, GM, 0),

            % 删除成员
            Sql = <<"DELETE FROM ", GMTb/binary, " WHERE id = $1">>,
            {ok, _} = elib_pg:execute(Conn, Sql, [Id]),

            % 写日志
            {ok, Body} = jsone_encode:encode(GM, [native_utf8]),
            Type = if CurrentUid == Uid -> 200; true -> 202 end,
            _ = group_log_repo:add(Conn, #{
                type => Type,
                option_uid => CurrentUid,
                group_id => Gid,
                body => Body,
                created_at => Now
            }),

            % 更新群组统计
            case update_statistics(Conn, Gid) of
                {ok, UidSum} ->
                    {ok, UidSum, GM};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 设置群成员昵称/别名
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @param Alias 别名
%% @param Description 描述
%% @return ok
-spec alias(integer(), integer(), binary(), binary()) -> ok | {error, any()}.
alias(Uid, Gid, Alias, Description) ->
    Now = elib_dt:now(),
    Data = #{alias => Alias, description => Description, updated_at => Now},
    GMTb = group_member_repo:tablename(),
    case elib_pg:update(GMTb, Data, <<"group_id = $1 AND user_id = $2">>, [Gid, Uid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新群组统计信息
%% @param Conn 数据库连接（可选）
%% @param Gid 群组ID
%% @return {ok, UidSum} | {error, Reason}
-spec update_statistics(pid() | undefined, integer()) -> {ok, integer()} | {error, any()}.
update_statistics(undefined, Gid) ->
    elib_pg:with_tx(fun(Conn) -> update_statistics(Conn, Gid) end);
update_statistics(Conn, Gid) ->
    GMTb = group_member_repo:tablename(),
    SqlSum = <<"SELECT COALESCE(SUM(user_id), 0) as user_id_sum, COUNT(*) as member_count ",
               "FROM ", GMTb/binary,
               " WHERE group_id = $1 AND status > -1">>,
    case elib_pg:query(Conn, SqlSum, [Gid]) of
        {ok, [#{<<"user_id_sum">> := UidSum0, <<"member_count">> := MemberCount}]} ->
            UidSum = ec_cnv:to_integer(UidSum0),
            GroupTb = group_repo:tablename(),
            {ok, _} = elib_pg:update(Conn, GroupTb,
                #{
                    member_count => ec_cnv:to_integer(MemberCount),
                    user_id_sum  => UidSum,
                    updated_at   => elib_dt:now()
                }, <<"id = $1">>, [Gid]),
            {ok, UidSum};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查找群成员
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @param Column 要查询的列名
%% @return map() 成员信息，未找到返回空map
-spec find_by_gid_and_uid(integer(), integer(), binary()) -> map().
find_by_gid_and_uid(Gid, Uid, Column) ->
    group_member_repo:find(Gid, Uid, Column).

%% @doc 检查用户是否为群管理员
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @return boolean() true=是管理员，false=不是管理员
-spec check_admin(integer(), integer()) -> boolean().
check_admin(Uid, Gid) ->
    case group_member_repo:find(Gid, Uid, <<"role">>) of
        #{<<"role">> := Role} when Role >= 3 -> true;
        _ -> false
    end.

%% @doc 更新群成员禁言状态
%% @param Conn 数据库连接
%% @param Gid 群组ID
%% @param UserId 用户ID
%% @param MuteUntil 禁言到期时间戳（null 表示取消禁言）
%% @return ok | {error, Reason}
-spec update_mute(pid() | undefined, integer(), integer(), integer() | null) -> ok | {error, any()}.
update_mute(undefined, Gid, UserId, MuteUntil) ->
    elib_pg:with_tx(fun(Conn) -> update_mute(Conn, Gid, UserId, MuteUntil) end);
update_mute(Conn, Gid, UserId, MuteUntil) ->
    GMTb = group_member_repo:tablename(),
    Now = elib_dt:now(),
    FixedMuteUntil = case MuteUntil of
        null -> null;
        Value when is_integer(Value), Value >= 0 -> elib_dt:to_rfc3339(Value, millisecond);
        Value -> Value
    end,
    Data = #{mute_until => FixedMuteUntil, updated_at => Now},

    case elib_pg:update(Conn, GMTb, Data, <<"group_id = $1 AND user_id = $2">>, [Gid, UserId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取群成员信息
%% @param Gid 群组ID
%% @param UserId 用户ID
%% @param Column 要查询的列名，可以是单个列名或列名列表
%% @return {ok, MemberInfo} | {error, Reason}
-spec get_member_info(integer(), integer(), binary() | list()) -> {ok, map()} | {error, any()}.
get_member_info(Gid, UserId, Column) ->
    MemberInfo = group_member_repo:find(Gid, UserId, Column),
    case maps:size(MemberInfo) of
        0 -> {error, not_found};
        _ -> {ok, MemberInfo}
    end.

%% @doc 更新群成员角色
%% @param Conn 数据库连接（可选）
%% @param Gid 群组ID
%% @param UserId 用户ID
%% @param Role 角色值（1-普通成员，2-管理员，3-群主）
%% @param UpdatedAt 更新时间
%% @return ok | {error, Reason}
-spec update_role(pid() | undefined, integer(), integer(), integer()) -> ok | {error, any()}.
update_role(undefined, Gid, UserId, Role) ->
    elib_pg:with_tx(fun(Conn) -> update_role(Conn, Gid, UserId, Role, elib_dt:now()) end);
update_role(Conn, Gid, UserId, Role) ->
    Now = elib_dt:now(),
    update_role(Conn, Gid, UserId, Role, Now).

%% @doc 更新群成员角色（带时间参数）
%% @param Conn 数据库连接
%% @param Gid 群组ID
%% @param UserId 用户ID
%% @param Role 角色值（1-普通成员，2-管理员，3-群主）
%% @param UpdatedAt 更新时间
%% @return ok | {error, Reason}
-spec update_role(pid(), integer(), integer(), integer(), integer()) -> ok | {error, any()}.
update_role(Conn, Gid, UserId, Role, UpdatedAt) ->
    GMTb = group_member_repo:tablename(),
    Data = #{role => Role, updated_at => UpdatedAt},
    case elib_pg:update(Conn, GMTb, Data, <<"group_id = $1 AND user_id = $2">>, [Gid, UserId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% G3: group_member_handler 不应直调 group_member_repo
-spec list_same_group(integer(), integer()) -> [integer()].
list_same_group(UidA, UidB) -> group_member_repo:list_same_group(UidA, UidB).

%% G3: adm_user_handler 不应直调 group_member_repo
-spec count_by_uid(integer()) -> non_neg_integer().
count_by_uid(Uid) -> group_member_repo:count_by_uid(Uid).

%% G3: adm_group_handler 不应直调 group_member_repo
-spec page_by_gid(integer(), pos_integer(), pos_integer(), binary()) -> {ok, map()} | {error, term()}.
page_by_gid(Gid, Page, Size, Column) -> group_member_repo:page_by_gid(Gid, Page, Size, Column).

%% @doc 更新群成员备注
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @param Remark 备注
%% @return {ok, Count} | {error, Reason}
-spec update_remark(integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
update_remark(Gid, Uid, Remark) ->
    Tb = group_member_repo:tablename(),
    elib_pg:update(Tb, #{<<"remark">> => Remark}, <<" WHERE group_id = $1 AND user_id = $2">>, [Gid, Uid]).

%% @doc 分页查询群成员（含用户基本信息 JOIN user 表）
%% @param Gid 群组ID
%% @param Page 页码
%% @param Size 每页大小
%% @return {ok, map()} | _
-spec page_with_user_info(integer(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page_with_user_info(Gid, Page, Size) ->
    UTb = user_repo:tablename(),
    MTb = group_member_repo:tablename(),
    Tb = <<UTb/binary, " u LEFT JOIN ", MTb/binary, " m ON u.id = m.user_id">>,
    Fields =
        <<"u.nickname, u.avatar, u.account, u.sign, m.user_id, m.group_id, "
          "m.alias, m.invite_code, m.description, m.role, m.is_join, m.join_mod"
          "e, m.status, m.updated_at, m.created_at">>,
    Where = #{<<"m.group_id">> => Gid},
    elib_pg:page_with_total(Tb, Fields, Where, <<"m.id desc">>, Page, Size).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
