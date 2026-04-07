-module(group_schedule_logic).
%% Stable group_collab schedule domain boundary.
%% HTTP adapters should call this module instead of repo internals.

%%%
% group_schedule 业务逻辑模块
% 提供群组日程的业务逻辑功能
%%%

-include("log.hrl").

%% 日程管理
-export([create_schedule/9]).
-export([update_schedule/7]).
-export([cancel_schedule/2]).
-export([get_schedule_detail/1]).
-export([list_group_schedules/3]).
-export([list_group_schedules/5]).
-export([list_my_schedules/3]).
-export([list_my_schedules/5]).

%% 参与人管理
-export([confirm_participation/3]).
-export([add_participants/2]).
-export([remove_participant/2]).

%% 提醒管理
-export([process_reminders/0]).
-export([schedule_reminders/4]).

%% 数据转换
-export([schedule_transfer/1]).

-define(MAX_PARTICIPANTS, 100).
-define(DEFAULT_REMIND_BEFORE, 15).

%% ===================================================================
%% 日志管理 API
%% ===================================================================

%% @doc 创建群组日程
-spec create_schedule(integer(), integer(), binary(), binary(), binary(),
                     binary(), binary(), integer(), [integer()]) ->
    {ok, map()} | {error, term()}.
create_schedule(GroupId, CreatorId, Title, Description, Location,
               StartAt, EndAt, RemindBefore, ParticipantIds) ->
    % 验证参与人数量
    case length(ParticipantIds) > ?MAX_PARTICIPANTS of
        true ->
            {error, too_many_participants};
        false ->
            do_create_schedule(GroupId, CreatorId, Title, Description, Location,
                             StartAt, EndAt, RemindBefore, ParticipantIds)
    end.

%% @doc 内部创建日程实现
do_create_schedule(GroupId, CreatorId, Title, Description, Location,
                  StartAt, EndAt, RemindBefore, ParticipantIds) ->
    % 生成 schedule_id
    ScheduleId = generate_schedule_id(),
    Now = elib_dt:now(),

    % 构建日程数据
    ScheduleData = #{
        group_id => GroupId,
        schedule_id => ScheduleId,
        title => Title,
        description => Description,
        location => Location,
        creator_id => CreatorId,
        start_at => StartAt,
        end_at => EndAt,
        remind_before => RemindBefore,
        status => 1,
        created_at => Now,
        updated_at => Now
    },

    % 插入日程
    case group_schedule_repo:insert(ScheduleData) of
        {ok, _ScheduleDbId, #{<<"id">> := ScheduleDbId}} ->
            % 插入创建者作为参与人
            CreatorParticipantData = #{
                schedule_id => ScheduleId,
                user_id => CreatorId,
                status => 1,  % 创建者默认参加
                created_at => Now,
                updated_at => Now
            },
            group_schedule_repo:insert_participant(CreatorParticipantData),

            % 插入其他参与人
            insert_participants(ScheduleId, ParticipantIds, Now),

            % 安排提醒
            schedule_reminders(ScheduleId, StartAt, RemindBefore, ParticipantIds ++ [CreatorId]),

            % 返回结果
            {ok, #{
                schedule_id => ScheduleId,
                id => ScheduleDbId
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 更新群组日程
-spec update_schedule(binary(), integer(), binary(), binary(), binary(),
                     binary(), binary()) -> ok | {error, term()}.
update_schedule(ScheduleId, CreatorId, Title, Description, Location,
               StartAt, EndAt) ->
    case group_schedule_repo:find_by_schedule_id(ScheduleId) of
        {error, not_found} ->
            {error, not_found};
        Schedule ->
            #{<<"creator_id">> := ScheduleCreatorId, <<"status">> := Status} = Schedule,
            % 验证权限
            case ScheduleCreatorId =:= CreatorId of
                false ->
                    {error, unauthorized};
                true ->
                    % 验证状态
                    case Status of
                        4 ->
                            {error, already_cancelled};
                        _ ->
                            do_update_schedule(ScheduleId, Title, Description, Location, StartAt, EndAt)
                    end
            end
    end.

%% @doc 内部更新日程实现
do_update_schedule(ScheduleId, Title, Description, Location, StartAt, EndAt) ->
    UpdateData = #{
        title => Title,
        description => Description,
        location => Location,
        start_at => StartAt,
        end_at => EndAt
    },

    #{<<"id">> := ScheduleDbId} = group_schedule_repo:find_by_schedule_id(ScheduleId),
    case group_schedule_repo:update(ScheduleDbId, UpdateData) of
        {ok, 1} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取消群组日程
-spec cancel_schedule(binary(), integer()) -> ok | {error, term()}.
cancel_schedule(ScheduleId, CreatorId) ->
    case group_schedule_repo:find_by_schedule_id(ScheduleId) of
        {error, not_found} ->
            {error, not_found};
        Schedule ->
            #{<<"creator_id">> := ScheduleCreatorId, <<"status">> := Status, <<"id">> := ScheduleDbId} = Schedule,
            % 验证权限
            case ScheduleCreatorId =:= CreatorId of
                false ->
                    {error, unauthorized};
                true ->
                    % 验证状态
                    case Status of
                        4 ->
                            {error, already_cancelled};
                        _ ->
                            case group_schedule_repo:update_status(ScheduleDbId, 4) of
                                {ok, 1} -> ok;
                                {error, Reason} -> {error, Reason}
                            end
                    end
            end
    end.

%% @doc 获取日程详情
-spec get_schedule_detail(binary()) -> {ok, map()} | {error, term()}.
get_schedule_detail(ScheduleId) ->
    case group_schedule_repo:find_by_schedule_id(ScheduleId) of
        {error, not_found} ->
            {error, schedule_not_found};
        Schedule ->
            case group_schedule_repo:list_participants(ScheduleId) of
                {ok, Participants} ->
                    {ok, Count} = group_schedule_repo:count_participants(ScheduleId),
                    {ok, #{
                        schedule => schedule_transfer(Schedule),
                        participants => [participant_transfer(P) || P <- Participants],
                        participant_count => Count
                    }};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 查询群组日程列表
-spec list_group_schedules(integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
list_group_schedules(GroupId, Page, Size) ->
    list_group_schedules(GroupId, undefined, undefined, Page, Size).

%% @doc 查询群组日程列表（支持时间窗口）
-spec list_group_schedules(integer(), binary() | undefined, binary() | undefined, integer(), integer()) ->
    {ok, map()} | {error, term()}.
list_group_schedules(GroupId, StartAt, EndAt, Page, Size) ->
    case group_schedule_repo:list_by_group_id(GroupId, StartAt, EndAt, Page, Size) of
        {ok, List} ->
            {ok, Count} = group_schedule_repo:count_by_group_id(GroupId, StartAt, EndAt),
            {ok, #{
                list => [schedule_transfer(S) || S <- List],
                total => Count,
                page => Page,
                size => Size
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查询我的日程列表
-spec list_my_schedules(integer(), integer(), integer()) ->
    {ok, list(map())} | {error, term()}.
list_my_schedules(UserId, Page, Size) ->
    list_my_schedules(UserId, undefined, undefined, Page, Size).

%% @doc 查询我的日程列表（支持时间窗口）
-spec list_my_schedules(integer(), binary() | undefined, binary() | undefined, integer(), integer()) ->
    {ok, list(map())} | {error, term()}.
list_my_schedules(UserId, StartAt, EndAt, Page, Size) ->
    case group_schedule_repo:list_by_user_id(UserId, StartAt, EndAt, Page, Size) of
        {ok, List} ->
            {ok, [schedule_transfer(S) || S <- List]};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% 参与人管理 API
%% ===================================================================

%% @doc 确认参与日程
-spec confirm_participation(binary(), integer(), boolean()) -> ok | {error, term()}.
confirm_participation(ScheduleId, UserId, Accept) ->
    case group_schedule_repo:find_by_schedule_id(ScheduleId) of
        {error, not_found} ->
            {error, schedule_not_found};
        _Schedule ->
            Status = case Accept of
                true -> 1;  % 参加
                false -> 2  % 不参加
            end,
            case group_schedule_repo:update_participant_status(ScheduleId, UserId, Status) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, participant_not_found};
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc 添加参与人
-spec add_participants(binary(), [integer()]) -> ok | {error, term()}.
add_participants(ScheduleId, UserIds) ->
    Now = elib_dt:now(),
    insert_participants(ScheduleId, UserIds, Now),
    ok.

%% @doc 移除参与人
-spec remove_participant(binary(), integer()) -> ok | {error, term()}.
remove_participant(ScheduleId, UserId) ->
    case group_schedule_repo:delete_participant(ScheduleId, UserId) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, participant_not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 提醒管理 API
%% ===================================================================

%% @doc 处理待发送的提醒
-spec process_reminders() -> {ok, non_neg_integer()}.
process_reminders() ->
    case group_schedule_repo:list_pending_reminds() of
        {ok, []} ->
            {ok, 0};
        {ok, Reminds} ->
            Count = process_remind_list(Reminds, 0),
            {ok, Count}
    end.

%% @doc 处理提醒列表
process_remind_list([], Acc) ->
    Acc;
process_remind_list([Remind | Rest], Acc) ->
    #{<<"id">> := RemindId,
      <<"schedule_id">> := ScheduleId,
      <<"user_id">> := UserId} = Remind,

    % 发送提醒通知
    send_remind_notification(ScheduleId, UserId),

    % 标记为已发送
    group_schedule_repo:update_remind_sent(RemindId),

    process_remind_list(Rest, Acc + 1).

%% @doc 安排提醒
-spec schedule_reminders(binary(), binary(), integer(), [integer()]) -> ok.
schedule_reminders(ScheduleId, StartAt, RemindBefore, UserIds) ->
    case RemindBefore of
        undefined ->
            ok;
        0 ->
            ok;
        _ ->
            NowTs = elib_dt:rfc3339_to(elib_dt:now()),
            StartTs = elib_dt:rfc3339_to(StartAt),
            RemindTs = StartTs - (RemindBefore * 60 * 1000),

            % 只为未来的日程创建提醒
            case is_integer(NowTs) andalso RemindTs > NowTs of
                true ->
                    RemindAt = elib_dt:to_rfc3339(RemindTs),
                    create_remind_records(ScheduleId, UserIds, RemindAt);
                false ->
                    ok
            end
    end.

%% @doc 创建提醒记录
create_remind_records(_ScheduleId, [], _RemindAt) ->
    ok;
create_remind_records(ScheduleId, [UserId | Rest], RemindAt) ->
    RemindData = #{
        schedule_id => ScheduleId,
        user_id => UserId,
        remind_at => RemindAt,
        is_sent => false
    },
    group_schedule_repo:insert_remind(RemindData),
    create_remind_records(ScheduleId, Rest, RemindAt).

%% @doc 发送提醒通知
send_remind_notification(ScheduleId, UserId) ->
    case group_schedule_repo:find_by_schedule_id(ScheduleId) of
        {error, _} ->
            skip;
        Schedule ->
            #{<<"title">> := Title, <<"start_at">> := StartAt} = Schedule,
            Payload = #{
                schedule_id => ScheduleId,
                title => Title,
                start_at => StartAt
            },
            % 发送系统通知
            msg_s2c_ds:send(0, [UserId], <<"schedule_remind">>, <<>>, null, Payload, save)
    end.

%% ===================================================================
%% 内部辅助函数
%% ===================================================================

%% @doc 插入参与人列表
insert_participants(_ScheduleId, [], _Now) ->
    ok;
insert_participants(ScheduleId, [UserId | Rest], Now) ->
    ParticipantData = #{
        schedule_id => ScheduleId,
        user_id => UserId,
        status => 0,  % 待确认
        created_at => Now,
        updated_at => Now
    },
    group_schedule_repo:insert_participant(ParticipantData),
    insert_participants(ScheduleId, Rest, Now).

%% @doc 生成日程ID
generate_schedule_id() ->
    Timestamp = elib_dt:timestamp(),
    Random = crypto:strong_rand_bytes(8),
    Hash = crypto:hash(md5, <<Timestamp:64, Random/binary>>),
    <<"sched_", (binary:encode_hex(Hash))/binary>>.

%% @doc 转换日程数据
schedule_transfer(Schedule) ->
    maps:remove(<<"updated_at">>, Schedule).

%% @doc 转换参与人数据
participant_transfer(Participant) ->
    Participant.
