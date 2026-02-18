%%--------------------------------------------------------------------
%% @doc 群组成员业务逻辑模块
%%--------------------------------------------------------------------
-module(group_member_logic).

%% API 导出
-export([
    join_group/4,          % 获取 Conn 并调用事务版本
    join_group/5,          % 事务内部核心逻辑
    leave/3,
    alias/4,
    update_role/4,         % 更新群成员角色
    mute/4,               % 群成员禁言
    check_mute/2,         % 检查禁言状态
    list_member/1,
    list_member/2,
    % 新增权限验证函数
    has_admin_permission/2,        % 检查是否有管理权限
    has_senior_admin_permission/2, % 检查是否有高级管理权限
    is_owner_or_vice_owner/2,      % 检查是否是群主或副群主
    role_name/1              % 获取角色名称
]).

-include("log.hrl").
-include("group_role.hrl").

%% ===================================================================
%% 公共方法
%% ===================================================================

%% 获取群成员列表（最多 50000）
-spec list_member(integer()) -> {ok, [map()]} | {error, term()}.
list_member(Gid) when is_integer(Gid) ->
    list_member(Gid, []).

%% 获取群成员列表，可指定用户ID列表
-spec list_member(integer(), list()) -> {ok, [map()]} | {error, term()}.
list_member(Gid, MemberUids) ->
    % 使用 DS 层接口
    group_member_ds:list_member(Gid, MemberUids).

%% ===================================================================
%% join_group/4
%% 自动获取事务连接并调用 join_group/5
%% ===================================================================
-spec join_group(binary(), integer(), integer(), map()) -> ok | {error, binary()}.
join_group(JoinMode, Uid, Gid, OptData) when is_map(OptData) ->
    % 验证群组限制
    Max = maps:get(max_members, OptData, undefined),
    Count = maps:get(current_count, OptData, undefined),
    case validate_group_limit(Max, Count) of
        {error, Reason} -> {error, Reason};
        ok ->
            % 使用事务执行加入操作
            case elib_pg:with_tx(
                fun(Conn) -> join_group(Conn, JoinMode, Uid, Gid, OptData) end
            ) of
                {ok, UidSum} ->
                    % 发送加入通知
                    group_member_join_notice(Gid, Uid, UidSum),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% ===================================================================
%% join_group/5
%% @doc 用户加入群组（事务内部版本）
%% 调用 DS 层执行数据操作
%% ===================================================================
-spec join_group(epgsql:connection(), binary(), integer(), integer(), map()) -> {ok, integer()} | {error, binary()}.
join_group(Conn, JoinMode, Uid, Gid, OptData) when is_map(OptData) ->
    % 使用 DS 层接口
    group_member_ds:join_group(Conn, JoinMode, Uid, Gid, OptData).

%% ===================================================================
%% leave 群组成员
%% ===================================================================
-spec leave(integer(), integer(), integer()) -> ok.
leave(Uid, Gid, CurrentUid) ->
    % 使用事务执行离开操作
    case elib_pg:with_tx(fun(Conn) -> leave_internal(Conn, Uid, Gid, CurrentUid) end) of
        {ok, UidSum, _GM} ->
            % 更新内存缓存
            group_ds:leave(Uid, Gid),
            % 通知其他成员
            ToUidLi = group_ds:member_uids(Gid),
            Action = <<"group_member_leave">>,
            Payload = #{
                <<"gid">>        => elib_hashids:encode(Gid),
                <<"user_id_sum">> => UidSum,
                <<"leave_uid">>   => elib_hashids:encode(Uid)
            },
            _ = msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, null, Payload, save),
            ok;
        _ ->
            ok
    end.

%% @doc 内部离开操作（事务内）
-spec leave_internal(pid(), integer(), integer(), integer()) -> {ok, integer(), map()} | {error, any()}.
leave_internal(Conn, Uid, Gid, CurrentUid) ->
    % 使用 DS 层接口
    group_member_ds:leave(Conn, Uid, Gid, CurrentUid).

%% ===================================================================
%% alias 设置群成员昵称/别名
%% ===================================================================
-spec alias(integer(), integer(), binary(), binary()) -> ok | {error, term()}.
alias(Uid, Gid, Alias, Description) ->
    % 使用 DS 层接口
    case group_member_ds:alias(Uid, Gid, Alias, Description) of
        ok ->
            ToUidLi = group_ds:member_uids(Gid),
            Now = elib_dt:now(),
            Data = #{alias => Alias, description => Description, updated_at => Now},
            % v2.0: 使用 send/7 API
            Action = <<"group_member_alias">>,
            Payload = maps:put(<<"gid">>, elib_hashids:encode(Gid), Data),
            _ = msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, null, Payload, save),
            ok;
        {error, Reason} ->
            ?ERROR_LOG([<<"group_member_logic alias error">>, Reason]),
            {error, Reason}
    end.

%% @doc 更新群成员角色
%% 更新指定群成员的角色
%%
%% @param CurrentUid 当前操作用户ID
%% @param Gid 群组ID
%% @param UserId 要更新的用户ID
%% @param Role 新的角色值（1-普通成员，2-管理员，3-群主）
%% @return ok | {error, binary()}
%% @end
-spec update_role(integer(), integer(), integer(), integer()) -> ok | {error, binary()}.
update_role(CurrentUid, Gid, UserId, Role) ->
    % 验证操作权限
    case validate_role_permission(CurrentUid, Gid) of
        {error, Reason} -> {error, Reason};
        ok ->
            % 获取当前时间
            Now = elib_dt:now(),
            % 使用事务执行角色更新操作
            case elib_pg:with_tx(fun(Conn) -> update_role_internal(Conn, CurrentUid, Gid, UserId, Role, Now) end) of
                {ok, _} ->
                    % 发送角色变更通知
                    role_change_notice(CurrentUid, Gid, UserId, Role),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 群成员禁言
%% 禁言指定群成员
%%
%% @param CurrentUid 当前操作用户ID
%% @param Gid 群组ID
%% @param UserId 要禁言的用户ID
%% @param Duration 禁言时长（秒）
%% @return ok | {error, binary()}
%% @end
-spec mute(integer(), integer(), integer(), integer()) -> ok | {error, binary()}.
mute(CurrentUid, Gid, UserId, Duration) ->
    % 验证操作权限
    case validate_mute_permission(CurrentUid, Gid) of
        {error, Reason} -> {error, Reason};
        ok ->
            % 获取当前时间
            Now = elib_dt:now(),
            % 计算禁言到期时间
            MuteUntil = Now + Duration * 1000,
            % 使用事务执行禁言操作
            case elib_pg:with_tx(fun(Conn) -> mute_internal(Conn, Gid, UserId, MuteUntil) end) of
                {ok, _} ->
                    % 发送禁言通知
                    mute_notice(CurrentUid, Gid, UserId, MuteUntil),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 检查用户是否被禁言
%%
%% @param Gid 群组ID
%% @param UserId 用户ID
%% @return true | false
%% @end
-spec check_mute(integer(), integer()) -> boolean().
check_mute(Gid, UserId) ->
    case group_member_ds:get_member_info(Gid, UserId, <<"mute_until">>) of
        {ok, MemberInfo} ->
            MuteUntil = maps:get(<<"mute_until">>, MemberInfo, null),
            case MuteUntil of
                null -> false;
                _ ->
                    % 如果未到期则返回 true
                    MuteUntil >= elib_dt:now()
            end;
        {error, _} ->
            false
    end.

%% @doc 内部禁言操作（事务内）
-spec mute_internal(pid(), integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
mute_internal(Conn, Gid, UserId, MuteUntil) ->
    % 使用 DS 层接口执行禁言
    group_member_ds:update_mute(Conn, Gid, UserId, MuteUntil).

%% @doc 验证禁言权限
-spec validate_mute_permission(integer(), integer()) -> ok | {error, binary()}.
validate_mute_permission(CurrentUid, Gid) ->
    % 检查当前用户是否是群管理员或群主
    case group_member_ds:get_member_info(Gid, CurrentUid, <<"role">>) of
        {ok, MemberInfo} ->
            Role = maps:get(<<"role">>, MemberInfo, 0),
            case Role >= ?ROLE_ADMIN andalso Role =< ?ROLE_VICE_OWNER of
                true -> ok;
                false -> {error, <<"你没有权限禁言群成员"/utf8>>}
            end;
        {error, _} ->
            {error, <<"你不是群成员"/utf8>>}
    end.

%% @doc 发送禁言通知
-spec mute_notice(integer(), integer(), integer(), integer()) -> ok.
mute_notice(AdminUid, Gid, _UserId, MuteUntil) ->
    % 获取群成员列表
    ToUidLi = group_ds:member_uids(Gid),
    Admin = user_ds:find_by_id(AdminUid, <<"nickname">>),
    Now = elib_dt:now(),

    % 计算剩余时间（转换为秒）
    RemainingSec = round((MuteUntil - Now) / 1000),
    DurationText = format_duration(RemainingSec),

    % 构建通知数据
    Payload = #{
        <<"gid">> => elib_hashids:encode(Gid),
        <<"mute_until">> => MuteUntil,
        <<"remaining_seconds">> => RemainingSec,
        <<"duration_text">> => DurationText,
        <<"admin_nickname">> => maps:get(<<"nickname">>, Admin, <<>>)
    },

    % 发送禁言通知
    Action = <<"group_member_mute">>,
    _ = msg_s2c_ds:send(AdminUid, ToUidLi, Action, <<>>, null, Payload, save),
    ok.

%% @doc 格式化禁言时长文本
-spec format_duration(integer()) -> binary().
format_duration(Seconds) when Seconds > 86400 ->
    Days = Seconds div 86400,
    elib_cnv:to_binary(Days) ++ <<"天"/utf8>>;
format_duration(Seconds) when Seconds > 3600 ->
    Hours = Seconds div 3600,
    elib_cnv:to_binary(Hours) ++ <<"小时"/utf8>>;
format_duration(Seconds) when Seconds > 60 ->
    Minutes = Seconds div 60,
    elib_cnv:to_binary(Minutes) ++ <<"分钟"/utf8>>;
format_duration(Seconds) ->
    elib_cnv:to_binary(Seconds) ++ <<"秒"/utf8>>.

%% ===================================================================
%% Internal
%% ===================================================================

%% 验证群组成员限制
-spec validate_group_limit(undefined | integer(), undefined | integer()) -> ok | {error, binary()}.
validate_group_limit(undefined, _Count) -> ok;
validate_group_limit(0, _Count) -> {error, <<"群不存在或群ID有误"/utf8>>};
validate_group_limit(Max, Count) when is_integer(Max), Max =< Count -> {error, <<"群成员已满"/utf8>>};
validate_group_limit(_, _) -> ok.

%% 发送加入通知
-spec group_member_join_notice(integer(), integer(), integer()) -> ok.
group_member_join_notice(Gid, Uid, Sum) ->
    ToUidLi = group_ds:member_uids(Gid),
    User = user_ds:find_by_id(Uid, <<"account,avatar,nickname">>),
    %% v2.0: 使用 send/7 API
    Action = <<"group_member_join">>,
    Payload = #{
        <<"gid">>          => elib_hashids:encode(Gid),
        <<"user_id_sum">>  => Sum,
        <<"nickname">>     => maps:get(<<"nickname">>, User, <<>>),
        <<"avatar">>       => maps:get(<<"avatar">>, User, <<>>),
        <<"account">>      => maps:get(<<"account">>, User, <<>>)
    },
    _ = msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, null, Payload, nosave),
    ok.

%% ===================================================================
%% EUnit 测试示例
%% ===================================================================
-ifdef(EUNIT).
join_group_test_() ->
    [
        ?_test(fun() ->
            % 模拟测试
            Result = validate_group_limit(100, 50),
            ?_assertEqual(ok, Result)
        end)
    ].
-endif.

%% @doc 内部角色更新操作（事务内）
-spec update_role_internal(pid(), integer(), integer(), integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
update_role_internal(Conn, _CurrentUid, Gid, UserId, Role, Now) ->
    % 使用 DS 层接口执行角色更新
    group_member_ds:update_role(Conn, Gid, UserId, Role, Now).

%% @doc 验证角色更新权限
-spec validate_role_permission(integer(), integer()) -> ok | {error, binary()}.
validate_role_permission(CurrentUid, Gid) ->
    % 检查当前用户是否是群管理员或群主
    case group_member_ds:get_member_info(Gid, CurrentUid, <<"role">>) of
        {ok, MemberInfo} ->
            Role = maps:get(<<"role">>, MemberInfo, 0),
            case Role >= ?ROLE_ADMIN andalso Role =< ?ROLE_VICE_OWNER of
                true -> ok;
                false -> {error, <<"你没有权限修改群成员角色"/utf8>>}
            end;
        {error, _} ->
            {error, <<"你不是群成员"/utf8>>}
    end.

%% @doc 发送角色变更通知
-spec role_change_notice(integer(), integer(), integer(), integer()) -> ok.
role_change_notice(AdminUid, Gid, UserId, Role) ->
    % 获取群成员列表
    ToUidLi = group_ds:member_uids(Gid),
    Admin = user_ds:find_by_id(AdminUid, <<"nickname">>),
    User = user_ds:find_by_id(UserId, <<"nickname">>),
    Now = elib_dt:now(),

    % 获取角色文本
    RoleText = group_member_logic:role_name(Role),

    % 构建通知数据
    Payload = #{
        <<"gid">> => elib_hashids:encode(Gid),
        <<"user_id">> => elib_hashids:encode(UserId),
        <<"role">> => Role,
        <<"role_text">> => RoleText,
        <<"nickname">> => maps:get(<<"nickname">>, User, <<>>),
        <<"admin_nickname">> => maps:get(<<"nickname">>, Admin, <<>>),
        <<"updated_at">> => Now
    },

    % 发送角色变更通知
    Action = <<"group_member_role">>,
    _ = msg_s2c_ds:send(AdminUid, ToUidLi, Action, <<>>, null, Payload, save),
    ok.

%% ===================================================================
%% 权限验证函数（增强功能）
%% ===================================================================

%% @doc 检查用户是否有管理权限（群主、副群主、管理员）
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return true | false
-spec has_admin_permission(integer(), integer()) -> boolean().
has_admin_permission(Gid, Uid) ->
    case group_member_ds:get_member_info(Gid, Uid, <<"role">>) of
        {ok, MemberInfo} ->
            Role = maps:get(<<"role">>, MemberInfo, 0),
            Role >= ?ROLE_ADMIN andalso Role =< ?ROLE_VICE_OWNER;
        {error, _} ->
            false
    end.

%% @doc 检查用户是否有高级管理权限（群主、副群主）
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return true | false
-spec has_senior_admin_permission(integer(), integer()) -> boolean().
has_senior_admin_permission(Gid, Uid) ->
    case group_member_ds:get_member_info(Gid, Uid, <<"role">>) of
        {ok, MemberInfo} ->
            Role = maps:get(<<"role">>, MemberInfo, 0),
            Role =:= ?ROLE_OWNER orelse Role =:= ?ROLE_VICE_OWNER;
        {error, _} ->
            false
    end.

%% @doc 检查用户是否是群主或副群主
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @return true | false
-spec is_owner_or_vice_owner(integer(), integer()) -> boolean().
is_owner_or_vice_owner(Gid, Uid) ->
    case group_member_ds:get_member_info(Gid, Uid, <<"role">>) of
        {ok, MemberInfo} ->
            Role = maps:get(<<"role">>, MemberInfo, 0),
            Role =:= ?ROLE_OWNER orelse Role =:= ?ROLE_VICE_OWNER;
        {error, _} ->
            false
    end.

%% @doc 获取角色名称
%% @param Role 角色值
%% @return binary() 角色名称
-spec role_name(integer()) -> binary().
role_name(Role) when Role >= ?ROLE_MEMBER andalso Role =< ?ROLE_VICE_OWNER ->
    maps:get(Role, ?ROLE_NAMES, <<"未知角色"/utf8>>);
role_name(_) ->
    <<"未知角色"/utf8>>.
