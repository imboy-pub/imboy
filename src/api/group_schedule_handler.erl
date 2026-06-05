-module(group_schedule_handler).
%% Thin HTTP adapter for the group_collab schedule boundary.
%% Keep feature-gated request parsing here and route schedule rules via group_schedule_logic.

%%%
% group_schedule 控制器模块
% group_schedule controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").
-include("error_code.hrl").

%% 导入错误处理函数
-import(imboy_error, [error_msg/1]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群组日程处理器
%% 根据请求中的 action 参数调用相应的处理函数
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case imboy_plugin_registry:required_feature(api, group_schedule_handler, Action) of
            undefined ->
                case Action of
                    create ->
                        create(Req0, State);
                    update ->
                        update(Req0, State);
                    cancel ->
                        cancel(Req0, State);
                    detail ->
                        detail(Req0, State);
                    list ->
                        list(Req0, State);
                    my_list ->
                        my_list(Req0, State);
                    confirm ->
                        confirm(Req0, State);
                    _ ->
                        Req0
                end;
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        case Action of
                            create ->
                                create(Req0, State);
                            update ->
                                update(Req0, State);
                            cancel ->
                                cancel(Req0, State);
                            detail ->
                                detail(Req0, State);
                            list ->
                                list(Req0, State);
                            my_list ->
                                my_list(Req0, State);
                            confirm ->
                                confirm(Req0, State);
                            _ ->
                                Req0
                        end;
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Action 处理函数
%% ===================================================================

%% @doc 创建群组日程
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),

    % 获取并验证参数
    GroupId = maps:get(<<"group_id">>, PostVals, undefined),
    Title = maps:get(<<"title">>, PostVals, <<>>),
    Description = maps:get(<<"description">>, PostVals, <<>>),
    Location = maps:get(<<"location">>, PostVals, <<>>),
    StartAt0 = maps_get(<<"start_at">>, PostVals),
    EndAt0 = maps_get(<<"end_at">>, PostVals),
    StartAt = normalize_time_value(StartAt0),
    EndAt = normalize_time_value(EndAt0),
    RemindBefore = elib_cnv:safe_to_integer(maps:get(<<"remind_before">>, PostVals, 15)),
    ParticipantIds0 = maps:get(<<"participant_ids">>, PostVals, []),

    % 解析 ID
    GroupId2 = elib_cnv:safe_to_integer(GroupId),
    ParticipantIds = [elib_cnv:safe_to_integer(Id) || Id <- ParticipantIds0],

    % 验证必填字段
    case {GroupId2, Title, StartAt, EndAt} of
        {0, _, _, _} ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        {_, <<>>, _, _} ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        {_, _, undefined, _} ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        {_, _, _, undefined} ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        {_, _, _, _} when RemindBefore < 0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case
                group_schedule_logic:create_schedule(
                    GroupId2,
                    Uid,
                    Title,
                    Description,
                    Location,
                    StartAt,
                    EndAt,
                    RemindBefore,
                    ParticipantIds
                )
            of
                {ok, Result} ->
                    elib_response:success(Req0, Result, <<"日程创建成功"/utf8>>);
                {error, too_many_participants} ->
                    elib_response:error(Req0, <<"参与人数量超过限制"/utf8>>, ?ERR_BAD_REQUEST);
                {error, {missing_field, Field}} ->
                    Msg = <<"缺少必填字段: ", (ec_cnv:to_binary(Field))/binary>>,
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {error, {invalid_time_range, _, _}} ->
                    elib_response:error(Req0, <<"结束时间必须晚于开始时间"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Reason} ->
                    ?LOG_ERROR("创建日程失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"创建日程失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 修改群组日程
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),

    ScheduleId0 = maps_get(<<"schedule_id">>, PostVals),
    ScheduleId = normalize_schedule_id(ScheduleId0),
    Title = maps_get(<<"title">>, PostVals),
    Description = maps:get(<<"description">>, PostVals, <<>>),
    Location = maps:get(<<"location">>, PostVals, <<>>),
    StartAt = normalize_time_value(maps_get(<<"start_at">>, PostVals)),
    EndAt = normalize_time_value(maps_get(<<"end_at">>, PostVals)),

    case ScheduleId of
        undefined ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case
                group_schedule_logic:update_schedule(
                    ScheduleId, Uid, Title, Description, Location, StartAt, EndAt
                )
            of
                ok ->
                    elib_response:success(Req0, #{schedule_id => ScheduleId}, <<"日程修改成功"/utf8>>);
                {error, not_found} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, unauthorized} ->
                    elib_response:error(Req0, error_msg(?ERR_FORBIDDEN), ?ERR_FORBIDDEN);
                {error, already_cancelled} ->
                    elib_response:error(Req0, <<"日程已取消，无法修改"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Reason} ->
                    ?LOG_ERROR("修改日程失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"修改日程失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 取消群组日程
-spec cancel(cowboy_req:req(), map()) -> cowboy_req:req().
cancel(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),

    ScheduleId = normalize_schedule_id(maps_get(<<"schedule_id">>, PostVals)),

    case ScheduleId of
        undefined ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_schedule_logic:cancel_schedule(ScheduleId, Uid) of
                ok ->
                    elib_response:success(Req0, #{schedule_id => ScheduleId}, <<"日程已取消"/utf8>>);
                {error, not_found} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, unauthorized} ->
                    elib_response:error(Req0, error_msg(?ERR_FORBIDDEN), ?ERR_FORBIDDEN);
                {error, already_cancelled} ->
                    elib_response:error(Req0, <<"日程已取消"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Reason} ->
                    ?LOG_ERROR("取消日程失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"取消日程失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 查询日程详情
-spec detail(cowboy_req:req(), map()) -> cowboy_req:req().
detail(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    ScheduleId0 = proplists:get_value(<<"schedule_id">>, Qs, undefined),
    ScheduleId = normalize_schedule_id(ScheduleId0),

    case ScheduleId of
        undefined ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_schedule_logic:get_schedule_detail(ScheduleId) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, schedule_not_found} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, Reason} ->
                    ?LOG_ERROR("查询日程详情失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"查询日程详情失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 查询群组日程列表
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    GroupId = proplists:get_value(<<"group_id">>, Qs, undefined),
    StartAt = normalize_time_value(proplists:get_value(<<"start_at">>, Qs, undefined)),
    EndAt = normalize_time_value(proplists:get_value(<<"end_at">>, Qs, undefined)),
    Page = elib_param:int(page, Qs, 1),
    Size = elib_param:int(size, Qs, 20),

    GroupId2 = elib_cnv:safe_to_integer(GroupId),

    case GroupId2 of
        0 ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_schedule_logic:list_group_schedules(GroupId2, StartAt, EndAt, Page, Size) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, Reason} ->
                    ?LOG_ERROR("查询群组日程列表失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"查询日程列表失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 查询我的日程列表
-spec my_list(cowboy_req:req(), map()) -> cowboy_req:req().
my_list(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    StartAt = normalize_time_value(proplists:get_value(<<"start_at">>, Qs, undefined)),
    EndAt = normalize_time_value(proplists:get_value(<<"end_at">>, Qs, undefined)),
    {Page, Size} = elib_param:page(Req0),

    case group_schedule_logic:list_my_schedules(Uid, StartAt, EndAt, Page, Size) of
        {ok, List} ->
            elib_response:success(Req0, #{list => List, page => Page, size => Size});
        {error, Reason} ->
            ?LOG_ERROR("查询我的日程列表失败: ~p", [Reason]),
            elib_response:error(Req0, <<"查询日程列表失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 确认参与日程
-spec confirm(cowboy_req:req(), map()) -> cowboy_req:req().
confirm(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),

    ScheduleId = normalize_schedule_id(maps_get(<<"schedule_id">>, PostVals)),
    Accept =
        case maps_get(<<"accept">>, PostVals) of
            undefined -> true;
            Value -> to_boolean(Value, true)
        end,

    case ScheduleId of
        undefined ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST);
        _ ->
            case group_schedule_logic:confirm_participation(ScheduleId, Uid, Accept) of
                ok ->
                    Msg =
                        case Accept of
                            true -> <<"已确认参加"/utf8>>;
                            false -> <<"已确认不参加"/utf8>>
                        end,
                    elib_response:success(Req0, #{schedule_id => ScheduleId}, Msg);
                {error, schedule_not_found} ->
                    elib_response:error(Req0, error_msg(?ERR_NOT_FOUND), ?ERR_NOT_FOUND);
                {error, participant_not_found} ->
                    elib_response:error(Req0, <<"您不在参与人列表中"/utf8>>, ?ERR_FORBIDDEN);
                {error, Reason} ->
                    ?LOG_ERROR("确认参与失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"确认参与失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% ===================================================================
%% 内部辅助函数
%% ===================================================================

%% @doc 安全获取 map 值，支持 undefined
maps_get(Key, Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> undefined;
        <<>> -> undefined;
        Value -> Value
    end.

%% @doc 统一时间参数，兼容:
%% - rfc3339
%% - 秒/毫秒时间戳
%% - 数字字符串
-spec normalize_time_value(term()) -> binary() | undefined.
normalize_time_value(undefined) ->
    undefined;
normalize_time_value(<<>>) ->
    undefined;
normalize_time_value(Value) when is_integer(Value), Value > 0 ->
    elib_dt:to_rfc3339(Value);
normalize_time_value(Value) when is_list(Value) ->
    normalize_time_value(ec_cnv:to_binary(Value));
normalize_time_value(Value) when is_binary(Value) ->
    case Value of
        <<>> -> undefined;
        _ -> elib_dt:to_rfc3339(Value)
    end;
normalize_time_value(_Value) ->
    undefined.

%% @doc 兼容 schedule_id:
%% - 原生 schedule_id (sched_xxx)
%% - 数据库主键 id（int/数字字符串）
%% - 主键 ID
-spec normalize_schedule_id(term()) -> binary() | undefined.
normalize_schedule_id(undefined) ->
    undefined;
normalize_schedule_id(<<>>) ->
    undefined;
normalize_schedule_id(Value) when is_integer(Value), Value > 0 ->
    schedule_id_by_pk(Value);
normalize_schedule_id(Value) when is_list(Value) ->
    normalize_schedule_id(ec_cnv:to_binary(Value));
normalize_schedule_id(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            undefined;
        <<"sched_", _/binary>> ->
            Value;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    schedule_id_by_pk(elib_cnv:safe_to_integer(Value));
                false ->
                    case elib_cnv:safe_to_integer(Value) of
                        0 ->
                            % 兼容历史非标准ID格式
                            Value;
                        Id ->
                            schedule_id_by_pk(Id)
                    end
            end
    end;
normalize_schedule_id(_Value) ->
    undefined.

-spec schedule_id_by_pk(integer()) -> binary() | undefined.
schedule_id_by_pk(Id) when is_integer(Id), Id > 0 ->
    case group_schedule_logic:get_schedule_id_by_pk(Id) of
        #{<<"schedule_id">> := ScheduleId} when is_binary(ScheduleId), ScheduleId =/= <<>> ->
            ScheduleId;
        _ ->
            undefined
    end;
schedule_id_by_pk(_) ->
    undefined.

%% @doc 将请求参数转换为 boolean()
to_boolean(true, _Default) -> true;
to_boolean(false, _Default) -> false;
to_boolean(1, _Default) -> true;
to_boolean(0, _Default) -> false;
to_boolean(<<"1">>, _Default) -> true;
to_boolean(<<"0">>, _Default) -> false;
to_boolean(<<"true">>, _Default) -> true;
to_boolean(<<"false">>, _Default) -> false;
to_boolean(<<"TRUE">>, _Default) -> true;
to_boolean(<<"FALSE">>, _Default) -> false;
to_boolean(_, Default) -> Default.
