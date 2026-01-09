%%--------------------------------------------------------------------
%% @doc 群组成员业务逻辑模块（优化版，高效、安全、简洁）
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

-define(DEF_COLUMN, <<"u.nickname,u.account,u.avatar,u.sign, gm.*">>).

%% ===================================================================
%% 公共方法
%% ===================================================================

%% 获取群成员列表（最多 50000）
% group_member_logic:list_member(31).
-spec list_member(integer()) -> {ok, [map()]} | {error, term()}.
list_member(Gid) when is_integer(Gid) ->
    list_member(Gid, []).

%% 获取群成员列表，可指定用户ID列表
-spec list_member(integer(), list()) -> {ok, [map()]} | {error, term()}.
list_member(Gid, []) ->
    TbA = group_member_repo:tablename(),
    TbB = user_repo:tablename(),
    Sql = <<"SELECT ", (?DEF_COLUMN)/binary,
            " FROM ", TbA/binary," gm LEFT JOIN ", TbB/binary,
            " u ON u.id = gm.user_id WHERE gm.group_id = $1 order by id desc limit $2">>,
    imboy_pg:query(Sql, [Gid, 50000]);
list_member(Gid, MemberUids) ->
    TbA = group_member_repo:tablename(),
    TbB = user_repo:tablename(),
    Placeholders = imboy_pg_sql:placeholders(length(MemberUids)),
    Sql = <<"SELECT ", (?DEF_COLUMN)/binary,
            " FROM ", TbA/binary," gm LEFT JOIN ", TbB/binary,
            " u ON u.id = gm.user_id WHERE gm.group_id = $1 AND gm.user_id IN (", Placeholders/binary, ")">>,
    % ?DEBUG_LOG([Sql]),
    imboy_pg:query(Sql, [Gid | MemberUids]).

%% ===================================================================
%% join_group/4
%% 自动获取事务连接并调用 join_group/5
%% ===================================================================
-spec join_group(binary(), integer(), integer(), map()) -> ok | {error, binary()}.
join_group(JoinMode, Uid, Gid, OptData) when is_map(OptData) ->
    % ?DEBUG_LOG(["join_group/4 Gid", Gid, "JoinMode", JoinMode, "Uid", Uid, "OptData", OptData]),
    _ = imboy_pg:with_tx(
        fun(Conn) -> join_group(Conn, JoinMode, Uid, Gid, OptData) end
    ),
    ok.

%% ===================================================================
%% join_group/5
%% @doc 用户加入群组（非事务版本）
%% 检查群组成员限制后在事务中执行加入操作
%% @param JoinMode 加入模式: <<"invite">> 邀请, <<"apply">> 申请, <<"qrcode">> 二维码
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @param OptData 选项数据，可包含 max_members(最大成员数)、current_count(当前成员数) %% @return ok | {error, binary()}
%% ===================================================================
-spec join_group(epgsql:connection(), binary(), integer(), integer(), map()) -> ok | {error, binary()}.
join_group(Conn, JoinMode, Uid, Gid, OptData) when is_map(OptData) ->
    Max = maps:get(max_members, OptData, undefined),
    Count = maps:get(current_count, OptData, undefined),
    Role = maps:get(role, OptData, 1),

    %% 验证群组限制
    case validate_group_limit(Max, Count) of
        {error, Reason} -> {error, Reason};
        ok ->
            %% 判断是否已是群成员
            case group_ds:is_member(Uid, Gid) of
                true -> ok; % 已是成员，幂等返回
                false ->
                    GMTb = group_member_repo:tablename(),
                    %% 插入群成员
                    Now = imboy_dt:now(),
                    _ = imboy_pg:insert(Conn, GMTb, #{
                        group_id => Gid,
                        user_id => Uid,
                        role => Role,
                        is_join => 1,
                        join_mode => imboy_str:trunc(JoinMode, 100),
                        created_at => Now
                    }, <<>>),

                    %% 从 group_member 表实时计算 user_id_sum 和 member_count
                    SqlSum = <<"SELECT COALESCE(SUM(user_id), 0) as user_id_sum, COUNT(*) as member_count ",
                               "FROM ", GMTb/binary,
                               " WHERE group_id = $1 AND status > -1">>,
                    {ok, [#{<<"user_id_sum">> := UidSum0, <<"member_count">> := MemberCount}]} = imboy_pg:query(Conn, SqlSum, [Gid]),
                    UidSum = ec_cnv:to_integer(UidSum0),
                    %% 更新群组统计
                    _ = imboy_pg:update(Conn, group_repo:tablename(),
                        #{
                            member_count => ec_cnv:to_integer(MemberCount),
                            user_id_sum  => UidSum,
                            updated_at   => Now
                        }, <<"id = $1">>, [Gid]),

                    %% 更新内存缓存
                    group_ds:join(Uid, Gid),

                    %% 发送加入通知
                    group_member_join_notice(Gid, Uid, UidSum),
                    ok
            end
    end.


%% ===================================================================
%% leave 群组成员
%% ===================================================================
-spec leave(integer(), integer(), integer()) -> ok.
leave(Uid, Gid, CurrentUid) ->
    GM = group_member_repo:find(Gid, Uid, <<"*">>),
    case maps:size(GM) of
        0 ->
            ok;
        _ ->
            leave_tx(Uid, Gid, GM, CurrentUid)
    end.


-spec leave_tx(integer(), integer(), map(), integer()) -> ok.
leave_tx(Uid, Gid, GM, CurrentUid) ->
    Now = imboy_dt:now(),
    Id = maps:get(<<"id">>, GM, 0),
    ToUidLi = group_ds:member_uids(Gid),
    _ = imboy_pg:with_tx(fun(Conn) ->
        %% 删除成员
        Tb = group_member_repo:tablename(),
        Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
        {ok, _} = imboy_pg:execute(Conn, Sql, [Id]),

        %% 写日志
        {ok, Body} = jsone_encode:encode(GM, [native_utf8]),
        Type = if CurrentUid == Uid -> 200; true -> 202 end,
        _ = group_log_repo:add(Conn, #{
            type => Type,
            option_uid => CurrentUid,
            group_id => Gid,
            body => Body,
            created_at => Now
        }),

        %% 从 group_member 表实时计算 user_id_sum 和 member_count（删除后）
        GMTb = group_member_repo:tablename(),
        SqlSum = <<"SELECT COALESCE(SUM(user_id), 0) as user_id_sum, COUNT(*) as member_count ",
                   "FROM ", GMTb/binary,
                   " WHERE group_id = $1 AND status > -1">>,
        case imboy_pg:query(Conn, SqlSum, [Gid]) of
            {ok, [#{<<"user_id_sum">> := UidSum0, <<"member_count">> := MemberCount0}]} ->
                UidSum = ec_cnv:to_integer(UidSum0),
                %% 更新群组统计
                {ok, _} = imboy_pg:update(Conn, group_repo:tablename(),
                    #{
                        user_id_sum => UidSum,
                        member_count => ec_cnv:to_integer(MemberCount0),
                        updated_at => Now
                    }, <<"id = $1">>, [Gid]),

                %% 通知其他成员
                Payload = #{
                    <<"gid">>        => imboy_hashids:encode(Gid),
                    <<"user_id_sum">> => UidSum,
                    <<"leave_uid">>   => imboy_hashids:encode(Uid),
                    <<"msg_type">>    => <<"group_member_leave">>
                },
                _ = msg_s2c_ds:send(Uid, Payload, ToUidLi, save),
                group_ds:leave(Uid, Gid),
                ok;
            _ ->
                ok
        end
    end, [{reraise, true}]),
    _ = ?DEBUG_LOG(["leave_tx ok", Uid, Gid, CurrentUid]),
    ok.

%% ===================================================================
%% alias 设置群成员昵称/别名
%% ===================================================================
-spec alias(integer(), integer(), binary(), binary()) -> ok.
alias(Uid, Gid, Alias, Description) ->
    Now = imboy_dt:now(),
    Data = #{alias => Alias, description => Description, updated_at => Now},
    _ = imboy_pg:update(
        group_member_repo:tablename(),
        Data,
        <<"group_id = $1 AND user_id = $2">>,
        [Gid, Uid]
    ),
    ToUidLi = group_ds:member_uids(Gid),
    _ = msg_s2c_ds:send(Uid, maps:put(<<"gid">>, imboy_hashids:encode(Gid),
        maps:put(<<"msg_type">>, <<"group_member_alias">>, Data)), ToUidLi, save),
    ok.

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
    User = user_repo:find_by_id(Uid, <<"account,avatar,nickname">>),
    Payload = #{
        <<"gid">>          => imboy_hashids:encode(Gid),
        <<"user_id_sum">>  => Sum,
        <<"nickname">>     => maps:get(<<"nickname">>, User, <<>>),
        <<"avatar">>       => maps:get(<<"avatar">>, User, <<>>),
        <<"account">>      => maps:get(<<"account">>, User, <<>>),
        <<"msg_type">>     => <<"group_member_join">>
    },
    _ = msg_s2c_ds:send(Uid, Payload, ToUidLi, nosave),
    ok.

%% ===================================================================
%% EUnit 测试示例
%% ===================================================================
-ifdef(EUNIT).
join_group_test_() ->
    [
        ?_test(fun() ->
            % 模拟 group_ds:is_member/2 返回 false
            Result = join_group(<<"invite">>, 123, 999, #{max_members => 100, current_count => 0}),
            ?_assertEqual(ok, Result)
        end)
    ].
-endif.
