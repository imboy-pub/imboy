-module(adm_stats_handler).
%%%
% adm_stats 控制器模块
% 统计 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        overview -> overview(Method, Req0, State);
        user -> user(Method, Req0, State);
        message -> message(Method, Req0, State);
        group -> group(Method, Req0, State);
        ranking -> ranking(Method, Req0, State);
        _ -> Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 总览统计
-spec overview(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
overview(<<"GET">>, Req0, _State) ->
    % 用户统计
    TotalUsers = count_table(<<"user">>),
    TodayUsers = count_today(<<"user">>),
    
    % 群组统计
    TotalGroups = count_table(<<"group">>),
    TodayGroups = count_today(<<"group">>),
    
    % 在线统计
    OnlineUsers = imboy_syn:count_user(),
    OnlineDevices = imboy_syn:count(),
    
    % 今日消息
    TodayC2C = count_today_messages(<<"msg_c2c">>),
    TodayC2G = count_today_messages(<<"msg_c2g">>),
    TodayMessages = TodayC2C + TodayC2G,
    
    Result = #{
        total_users => TotalUsers,
        today_users => TodayUsers,
        total_groups => TotalGroups,
        today_groups => TodayGroups,
        online_users => OnlineUsers,
        online_devices => OnlineDevices,
        today_messages => TodayMessages,
        today_c2c => TodayC2C,
        today_c2g => TodayC2G
    },
    elib_response:success(Req0, Result).

%% @doc 用户统计
-spec user(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
user(<<"GET">>, Req0, _State) ->
    {ok, Days0} = elib_param:int(days, Req0, 7),
    Days = Days0,

    % 每日新增用户
    DailyNew = count_daily_new(<<"user">>, Days),

    % 用户状态分布
    ActiveUsers = count_by_status(<<"user">>, 1),
    BannedUsers = count_by_status(<<"user">>, 0),
    DeletedUsers = count_by_status(<<"user">>, -1),

    Result = #{
        daily_new => DailyNew,
        active_users => ActiveUsers,
        banned_users => BannedUsers,
        deleted_users => DeletedUsers
    },
    elib_response:success(Req0, Result).

%% @doc 消息统计
-spec message(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
message(<<"GET">>, Req0, _State) ->
    {ok, Days0} = elib_param:int(days, Req0, 7),
    Days = Days0,

    % 每日消息量
    DailyC2C = count_daily_messages(<<"msg_c2c">>, Days),
    DailyC2G = count_daily_messages(<<"msg_c2g">>, Days),

    Result = #{
        daily_c2c => DailyC2C,
        daily_c2g => DailyC2G
    },
    elib_response:success(Req0, Result).

%% @doc 群组统计
-spec group(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
group(<<"GET">>, Req0, _State) ->
    {ok, Days0} = elib_param:int(days, Req0, 7),
    Days = Days0,

    % 每日新建群组
    DailyNew = count_daily_new(<<"group">>, Days),

    % 群组类型分布
    PublicGroups = count_by_type(<<"group">>, 1),
    PrivateGroups = count_by_type(<<"group">>, 2),

    Result = #{
        daily_new => DailyNew,
        public_groups => PublicGroups,
        private_groups => PrivateGroups
    },
    elib_response:success(Req0, Result).

%% @doc 排名统计
-spec ranking(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
ranking(<<"GET">>, Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Type = proplists:get_value(<<"type">>, Qs, <<"user">>),
    Metric = proplists:get_value(<<"metric">>, Qs, <<"count">>),
    LimitBin = proplists:get_value(<<"limit">>, Qs, <<"10">>),
    Limit = binary_to_integer(LimitBin),

    Result = case {Type, Metric} of
        {<<"user">>, <<"message">>} ->
            % 用户消息量排名
            get_user_message_ranking(Limit);
        {<<"user">>, <<"friend">>} ->
            % 用户好友数排名
            get_user_friend_ranking(Limit);
        {<<"group">>, <<"member">>} ->
            % 群组成员数排名
            get_group_member_ranking(Limit);
        {<<"group">>, <<"message">>} ->
            % 群组消息量排名
            get_group_message_ranking(Limit);
        {<<"channel">>, <<"subscriber">>} ->
            % 频道订阅数排名
            get_channel_subscriber_ranking(Limit);
        {<<"channel">>, <<"message">>} ->
            % 频道消息量排名
            get_channel_message_ranking(Limit);
        _ ->
            % 默认返回用户消息排名
            get_user_message_ranking(Limit)
    end,
    elib_response:success(Req0, #{items => Result}).

%% ===================================================================
%% Helper Functions
%% ===================================================================

%% @doc 统计表总数
count_table(Table) ->
    Sql = <<"SELECT COUNT(*) FROM ", Table/binary>>,
    case elib_pg:one(Sql, []) of
        {ok, #{count := Count}} -> Count;
        _ -> 0
    end.

%% @doc 统计今日新增
count_today(Table) ->
    Sql = <<"SELECT COUNT(*) FROM ", Table/binary, " WHERE created_at >= CURRENT_DATE">>,
    case elib_pg:one(Sql, []) of
        {ok, #{count := Count}} -> Count;
        _ -> 0
    end.

%% @doc 统计今日消息数
count_today_messages(Table) ->
    Sql = <<"SELECT COUNT(*) FROM ", Table/binary, " WHERE created_at >= CURRENT_DATE">>,
    case elib_pg:one(Sql, []) of
        {ok, #{count := Count}} -> Count;
        _ -> 0
    end.

%% @doc 统计每日新增
count_daily_new(Table, Days) ->
    Sql = iolist_to_binary([
        <<"SELECT DATE(created_at) as date, COUNT(*) as count FROM ", Table/binary>>,
        <<" WHERE created_at >= CURRENT_DATE - ">>, integer_to_binary(Days), <<"' GROUP BY DATE(created_at) ORDER BY date">>
    ]),
    case elib_pg:query(Sql, []) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 统计每日消息
count_daily_messages(Table, Days) ->
    Sql = iolist_to_binary([
        <<"SELECT DATE(created_at) as date, COUNT(*) as count FROM ", Table/binary>>,
        <<" WHERE created_at >= CURRENT_DATE - ">>, integer_to_binary(Days), <<"' GROUP BY DATE(created_at) ORDER BY date">>
    ]),
    case elib_pg:query(Sql, []) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 按状态统计
count_by_status(Table, Status) ->
    Sql = <<"SELECT COUNT(*) FROM ", Table/binary, " WHERE status = $1">>,
    case elib_pg:one(Sql, [Status]) of
        {ok, #{count := Count}} -> Count;
        _ -> 0
    end.

%% @doc 按类型统计
count_by_type(Table, Type) ->
    Sql = <<"SELECT COUNT(*) FROM ", Table/binary, " WHERE type = $1">>,
    case elib_pg:one(Sql, [Type]) of
        {ok, #{count := Count}} -> Count;
        _ -> 0
    end.

%% ===================================================================
%% 排名统计函数
%% ===================================================================

%% @doc 用户消息量排名
get_user_message_ranking(Limit) ->
    % 统计单聊和群聊消息总数
    Sql = <<
        "SELECT u.id, u.nickname, u.account, COALESCE(m.msg_count, 0) as metric "
        "FROM \"user\" u "
        "LEFT JOIN ("
        "    SELECT from_id as user_id, COUNT(*) as msg_count FROM msg_c2c GROUP BY from_id"
        "    UNION ALL"
        "    SELECT from_id as user_id, COUNT(*) as msg_count FROM msg_c2g GROUP BY from_id"
        ") m ON u.id = m.user_id "
        "WHERE u.status = 1 "
        "ORDER BY metric DESC "
        "LIMIT $1"
    >>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 用户好友数排名
get_user_friend_ranking(Limit) ->
    Sql = <<
        "SELECT u.id, u.nickname, u.account, COUNT(f.id) as metric "
        "FROM \"user\" u "
        "LEFT JOIN friend f ON u.id = f.user_id AND f.status = 1 "
        "WHERE u.status = 1 "
        "GROUP BY u.id, u.nickname, u.account "
        "ORDER BY metric DESC "
        "LIMIT $1"
    >>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 群组成员数排名
get_group_member_ranking(Limit) ->
    Sql = <<
        "SELECT g.id, g.title as name, g.member_count as metric "
        "FROM \"group\" g "
        "WHERE g.status = 1 "
        "ORDER BY metric DESC "
        "LIMIT $1"
    >>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 群组消息量排名
get_group_message_ranking(Limit) ->
    Sql = <<
        "SELECT g.id, g.title as name, COUNT(m.id) as metric "
        "FROM \"group\" g "
        "LEFT JOIN msg_c2g m ON g.id = m.to_gid "
        "WHERE g.status = 1 "
        "GROUP BY g.id, g.title "
        "ORDER BY metric DESC "
        "LIMIT $1"
    >>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 频道订阅数排名
get_channel_subscriber_ranking(Limit) ->
    Sql = <<
        "SELECT c.id, c.name, c.subscriber_count as metric "
        "FROM channel c "
        "WHERE c.status = 1 "
        "ORDER BY metric DESC "
        "LIMIT $1"
    >>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 频道消息量排名
get_channel_message_ranking(Limit) ->
    Sql = <<
        "SELECT c.id, c.name, COUNT(m.id) as metric "
        "FROM channel c "
        "LEFT JOIN channel_message m ON c.id = m.channel_id "
        "WHERE c.status = 1 "
        "GROUP BY c.id, c.name "
        "ORDER BY metric DESC "
        "LIMIT $1"
    >>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.
