-module(group_schedule_logic).
%% Stable group_collab schedule domain boundary.
%% HTTP adapters should call this module instead of repo internals.

%%%
% group_schedule 业务逻辑模块
% 提供群组日程的业务逻辑功能
%%%

-include("log.hrl").
-include("error_code.hrl").

%% 日程管理
-export([create_schedule/9]).
-export([update_schedule/7]).
-export([cancel_schedule/2]).
-export([get_schedule_detail/1]).
-export([get_schedule_detail/2]).
-export([list_group_schedules/3]).
-export([list_group_schedules/5]).
-export([list_group_schedules/6]).
-export([list_my_schedules/3]).
-export([list_my_schedules/5]).

%% 参与人管理
-export([confirm_participation/3]).

%% 提醒管理
-export([process_reminders/0]).

%% 查询辅助
-export([get_schedule_id_by_pk/1]).

%% 数据转换

-define(MAX_PARTICIPANTS, 100).
-define(DEFAULT_REMIND_BEFORE, 15).

%% ===================================================================
%% 日志管理 API
%% ===================================================================

%% @doc 创建群组日程
-spec create_schedule(
    integer(),
    integer(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    integer(),
    [integer()]
) ->
    {ok, map()} | {error, term()}.
create_schedule(
    GroupId,
    CreatorId,
    Title,
    Description,
    Location,
    StartAt,
    EndAt,
    RemindBefore,
    ParticipantIds
) ->
    % 验证发起人是否为该群成员，防止对任意群创建骚扰日程
    case group_ds:is_member(CreatorId, GroupId) of
        false ->
            {error, not_group_member};
        true ->
            % 验证参与人数量
            case length(ParticipantIds) > ?MAX_PARTICIPANTS of
                true ->
                    {error, too_many_participants};
                false ->
                    do_create_schedule(
                        GroupId,
                        CreatorId,
                        Title,
                        Description,
                        Location,
                        StartAt,
                        EndAt,
                        RemindBefore,
                        ParticipantIds
                    )
            end
    end.

%% @doc 内部创建日程实现
do_create_schedule(
    GroupId,
    CreatorId,
    Title,
    Description,
    Location,
    StartAt,
    EndAt,
    RemindBefore,
    ParticipantIds
) ->
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
    case normalize_write_result(group_schedule_ds:insert_schedule(ScheduleData)) of
        {ok, _ScheduleDbId, #{<<"id">> := ScheduleDbId}} ->
            finish_created_schedule(ScheduleData, ScheduleDbId, ParticipantIds);
        {error, Reason} ->
            {error, Reason}
    end.

finish_created_schedule(Data, ScheduleDbId, ParticipantIds) ->
    ScheduleId = maps:get(schedule_id, Data),
    CreatorId = maps:get(creator_id, Data),
    Now = maps:get(created_at, Data),
    CreatorParticipant = #{
        schedule_id => ScheduleId,
        user_id => CreatorId,
        status => 1,
        created_at => Now,
        updated_at => Now
    },
    _ = group_schedule_ds:insert_participant(CreatorParticipant),
    insert_participants(ScheduleId, ParticipantIds, Now),
    schedule_reminders(
        ScheduleId,
        maps:get(start_at, Data),
        maps:get(remind_before, Data),
        ParticipantIds ++ [CreatorId]
    ),
    publish_created_schedule(Data, ScheduleDbId).

publish_created_schedule(Data, ScheduleDbId) ->
    GroupId = maps:get(group_id, Data),
    CreatorId = maps:get(creator_id, Data),
    ScheduleId = maps:get(schedule_id, Data),
    Title = maps:get(title, Data),
    Result = #{schedule_id => ScheduleId, id => ScheduleDbId},
    Payload = #{
        <<"id">> => integer_to_binary(ScheduleDbId),
        <<"group_id">> => integer_to_binary(GroupId),
        <<"title">> => Title,
        <<"content">> => Title,
        <<"start_time">> => maps:get(start_at, Data)
    },
    case
        group_collab_message_logic:publish(
            GroupId,
            CreatorId,
            <<"groupSchedule">>,
            ScheduleId,
            Payload
        )
    of
        {ok, Message} -> {ok, Result#{chat_message => Message}};
        {error, _} -> {ok, Result#{message_status => <<"failed">>}}
    end.

%% @doc 更新群组日程
-spec update_schedule(
    binary(),
    integer(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary()
) -> ok | {error, term()}.
update_schedule(
    ScheduleId,
    CreatorId,
    Title,
    Description,
    Location,
    StartAt,
    EndAt
) ->
    case group_schedule_ds:find_by_schedule_id(ScheduleId) of
        {error, not_found} ->
            {error, not_found};
        M when map_size(M) =:= 0 ->
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
                            do_update_schedule(
                                ScheduleId, Title, Description, Location, StartAt, EndAt
                            )
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

    case group_schedule_ds:find_by_schedule_id(ScheduleId) of
        #{<<"id">> := ScheduleDbId} = Schedule ->
            case
                normalize_write_result(
                    group_schedule_ds:update_schedule(ScheduleDbId, UpdateData)
                )
            of
                {ok, 1} ->
                    % R18-1：改期后重排未发送提醒——作废旧未发送提醒，
                    % 按新 start_at 重建（复用 schedule_reminders 的未来提醒构造）。
                    % R18-5 规则：已发送（is_sent=true）的提醒不重排、不重发——
                    % 每个参与者每版日程至多一次提醒；改期不影响已提醒历史。
                    _ = group_schedule_ds:cancel_pending_reminds(ScheduleId),
                    RemindBefore = maps:get(<<"remind_before">>, Schedule, undefined),
                    ParticipantIds = participant_ids_of(ScheduleId),
                    schedule_reminders(
                        ScheduleId, StartAt, to_int_maybe(RemindBefore), ParticipantIds
                    ),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, not_found}
    end.

%% R18：重排提醒的参与人 = creator + 现有参与人（与创建口径一致）
-spec participant_ids_of(binary()) -> [integer()].
participant_ids_of(ScheduleId) ->
    case group_schedule_ds:list_participants(ScheduleId) of
        {ok, Rows} when is_list(Rows) ->
            [maps:get(<<"user_id">>, R) || R <- Rows];
        _ ->
            []
    end.

-spec to_int_maybe(term()) -> integer() | undefined.
to_int_maybe(V) when is_integer(V) ->
    V;
to_int_maybe(V) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        _:_ -> undefined
    end;
to_int_maybe(_) ->
    undefined.

%% @doc 取消群组日程
-spec cancel_schedule(binary(), integer()) -> ok | {error, term()}.
cancel_schedule(ScheduleId, CreatorId) ->
    case group_schedule_ds:find_by_schedule_id(ScheduleId) of
        {error, not_found} ->
            {error, not_found};
        M when map_size(M) =:= 0 ->
            {error, not_found};
        Schedule ->
            #{
                <<"creator_id">> := ScheduleCreatorId,
                <<"status">> := Status,
                <<"id">> := ScheduleDbId
            } = Schedule,
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
                            case
                                normalize_write_result(
                                    group_schedule_ds:update_status(ScheduleDbId, 4)
                                )
                            of
                                {ok, 1} ->
                                    % R18-2：取消日程即作废全部待发提醒
                                    % （is_sent=true 终态；已发送的不撤回）
                                    _ = group_schedule_ds:cancel_pending_reminds(ScheduleId),
                                    ok;
                                {error, Reason} ->
                                    {error, Reason}
                            end
                    end
            end
    end.

%% @doc 获取日程详情（内部/管理端原语，不做群成员校验，调用方需自行鉴权）
-spec get_schedule_detail(binary()) -> {ok, map()} | {error, term()}.
get_schedule_detail(ScheduleId) ->
    case group_schedule_ds:find_by_schedule_id(ScheduleId) of
        {error, not_found} ->
            {error, schedule_not_found};
        Schedule ->
            case group_schedule_ds:list_participants(ScheduleId) of
                {ok, Participants} ->
                    {ok, Count} = group_schedule_ds:count_participants(ScheduleId),
                    {ok, #{
                        schedule => schedule_transfer(Schedule),
                        participants => [participant_transfer(P) || P <- Participants],
                        participant_count => Count
                    }};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 获取日程详情（用户侧入口）
%% IDOR 防御：ViewerUid 必须是该日程所属群的成员，否则任意登录用户传任意
%% schedule_id 都能读取标题/地点/时间/完整参与人名单等内容。管理端
%% get_schedule_detail/1 保持不做该校验（由 adm_acl 权限门控收口）。
%% @param ScheduleId 日程ID
%% @param ViewerUid 查看者用户ID
-spec get_schedule_detail(binary(), integer()) -> {ok, map()} | {error, term()}.
get_schedule_detail(ScheduleId, ViewerUid) ->
    case get_schedule_detail(ScheduleId) of
        {ok, #{schedule := Schedule}} = Ok ->
            GroupId = maps:get(<<"group_id">>, Schedule, 0),
            case group_ds:is_member(ViewerUid, GroupId) of
                true -> Ok;
                false -> {error, unauthorized}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 查询群组日程列表
-spec list_group_schedules(integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
list_group_schedules(GroupId, Page, Size) ->
    list_group_schedules(GroupId, undefined, undefined, Page, Size).

%% @doc 查询群组日程列表（支持时间窗口，内部/管理端原语，不做群成员校验）
-spec list_group_schedules(
    integer(), binary() | undefined, binary() | undefined, integer(), integer()
) ->
    {ok, map()} | {error, term()}.
list_group_schedules(GroupId, StartAt, EndAt, Page, Size) ->
    case group_schedule_ds:list_by_group_id(GroupId, StartAt, EndAt, Page, Size) of
        {ok, List} ->
            {ok, Count} = group_schedule_ds:count_by_group_id(GroupId, StartAt, EndAt),
            {ok, #{
                list => [schedule_transfer(S) || S <- List],
                total => Count,
                page => Page,
                size => Size
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查询群组日程列表（用户侧入口，支持时间窗口）
%% IDOR 防御：ViewerUid 必须是 GroupId 的成员，否则任意登录用户传任意
%% group_id 都能分页拉取该群全部日程（含地点/时间）。管理端
%% list_group_schedules/5 保持不做该校验（由 adm_acl 权限门控收口）。
-spec list_group_schedules(
    integer(), integer(), binary() | undefined, binary() | undefined, integer(), integer()
) ->
    {ok, map()} | {error, term()}.
list_group_schedules(GroupId, ViewerUid, StartAt, EndAt, Page, Size) ->
    case group_ds:is_member(ViewerUid, GroupId) of
        false ->
            {error, unauthorized};
        true ->
            list_group_schedules(GroupId, StartAt, EndAt, Page, Size)
    end.

%% @doc 查询我的日程列表
-spec list_my_schedules(integer(), integer(), integer()) ->
    {ok, list(map())} | {error, term()}.
list_my_schedules(UserId, Page, Size) ->
    list_my_schedules(UserId, undefined, undefined, Page, Size).

%% @doc 查询我的日程列表（支持时间窗口）
-spec list_my_schedules(
    integer(), binary() | undefined, binary() | undefined, integer(), integer()
) ->
    {ok, list(map())} | {error, term()}.
list_my_schedules(UserId, StartAt, EndAt, Page, Size) ->
    case group_schedule_ds:list_by_user_id(UserId, StartAt, EndAt, Page, Size) of
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
    case group_schedule_ds:find_by_schedule_id(ScheduleId) of
        {error, not_found} ->
            {error, schedule_not_found};
        _Schedule ->
            Status =
                case Accept of
                    % 参加
                    true -> 1;
                    % 不参加
                    false -> 2
                end,
            case
                normalize_write_result(
                    group_schedule_ds:update_participant_status(ScheduleId, UserId, Status)
                )
            of
                {ok, 1} -> ok;
                {ok, 0} -> {error, participant_not_found};
                {error, Reason} -> {error, Reason}
            end
    end.

%% ===================================================================
%% 提醒管理 API
%% ===================================================================

%% @doc 处理待发送的提醒
-spec process_reminders() -> {ok, non_neg_integer()}.
process_reminders() ->
    case group_schedule_ds:list_pending_reminds() of
        {ok, []} ->
            {ok, 0};
        {ok, Reminds} ->
            Count = process_remind_list(Reminds, 0),
            {ok, Count}
    end.

%% @doc 处理提醒列表
%% T7 归档副作用门控：发送提醒通知（msg_s2c 推送 = 对外副作用）之前先经
%% workspace_guard:write_tx_or_skip 判定归档状态（{group_schedule_remind, Id}
%% → schedule → group → workspace 行锁线性化）——归档后零提醒推送泄漏
%% （不发送、不标记，提醒保持待发，恢复工作区后续跑）；active/personal
%% 照常发送并标记。
process_remind_list([], Acc) ->
    Acc;
process_remind_list([Remind | Rest], Acc) ->
    #{
        <<"id">> := RemindId,
        <<"schedule_id">> := ScheduleId,
        <<"user_id">> := UserId
    } = Remind,

    case
        workspace_guard:write_tx_or_skip({group_schedule_remind, RemindId}, fun(_Conn) -> ok end)
    of
        skipped ->
            %% 工作区已归档：不发送、不标记（读取不受影响）
            process_remind_list(Rest, Acc);
        {written, _} ->
            % R18-4：派发前原子认领（CAS：is_sent false→true）。
            % 认领成功才发送——发送成功后崩溃不重发（扫描不再命中已认领行）；
            % 认领失败（0）= 已被并发 worker 认领/已取消/已发送，跳过（R18-3 竞态）。
            case group_schedule_ds:claim_remind(RemindId) of
                {ok, 1} ->
                    % 发送提醒通知
                    send_remind_notification(ScheduleId, UserId),

                    process_remind_list(Rest, Acc + 1);
                _ ->
                    %% 认领失败：并发/取消/已发送，不再发送
                    process_remind_list(Rest, Acc)
            end
    end.

%% @doc 安排提醒
-spec schedule_reminders(binary(), binary(), integer() | undefined, [integer()]) -> ok.
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
    _ = group_schedule_ds:insert_remind(RemindData),
    create_remind_records(ScheduleId, Rest, RemindAt).

%% @doc 发送提醒通知
%% R18-3：派发前复查日程当前状态——已取消（status=4）的日程不发提醒
%% （worker 读到旧 remind 记录后日程被取消/改期的竞态兜底；
%% 认领 CAS 已保证不重发，此处保证取消后不漏发）。
%% R18-5 规则（记录在案）：已发送过的提醒不因改期而重发——改期只重排
%% is_sent=false 的未发送提醒，is_sent=true 的历史提醒保持终态。
send_remind_notification(ScheduleId, UserId) ->
    case group_schedule_ds:find_by_schedule_id(ScheduleId) of
        {error, _} ->
            skip;
        M when map_size(M) =:= 0 ->
            skip;
        #{<<"status">> := 4} ->
            %% 已取消：不发送（认领已提交，本条提醒作废）
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

%% @doc 根据数据库主键 ID 查询日程的对外 schedule_id 字段
%% 用于将数字主键（客户端可能传入）转换为标准 schedule_id（sched_xxx 格式）
-spec get_schedule_id_by_pk(integer()) -> #{binary() => binary()} | undefined | {error, term()}.
get_schedule_id_by_pk(Id) ->
    group_schedule_ds:find_by_id(Id, <<"schedule_id">>).

%% ===================================================================
%% 内部辅助函数
%% ===================================================================

%% @doc T7 归档写守卫：DS 写事务返回的稳定错误 {error, {980, Msg}} 归一为
%% 既有契约 {error, ?ERR_WORKSPACE_ARCHIVED}（handler 按 980 识别），
%% 其余结果原样透传。
normalize_write_result({error, {?ERR_WORKSPACE_ARCHIVED, _Msg}}) ->
    {error, ?ERR_WORKSPACE_ARCHIVED};
normalize_write_result(Other) ->
    Other.

%% @doc 插入参与人列表
insert_participants(_ScheduleId, [], _Now) ->
    ok;
insert_participants(ScheduleId, [UserId | Rest], Now) ->
    ParticipantData = #{
        schedule_id => ScheduleId,
        user_id => UserId,
        % 待确认
        status => 0,
        created_at => Now,
        updated_at => Now
    },
    _ = group_schedule_ds:insert_participant(ParticipantData),
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
