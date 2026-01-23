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
    list_member/1,
    list_member/2
]).

-include("log.hrl").

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
