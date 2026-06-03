-module(adm_group_schedule_handler).
%%% adm_group_schedule_handler

-behavior(cowboy_rest).

-export([init/2]).

-import(adm_group_helper, [
    audit_group_governance/5,
    build_governance_log_where_sql/1,
    calc_total_pages/2,
    extract_governance_log_filters/1,
    list_group_files_with_total/5,
    list_tasks_with_total/5,
    list_user_categories_with_total/4,
    normalize_album_pk/1,
    normalize_assignment_pk/1,
    normalize_category_pk/1,
    normalize_file_pk/1,
    normalize_governance_log_row/1,
    normalize_notice_pk/1,
    normalize_notice_row/1,
    normalize_page_payload/3,
    normalize_positive_int/1,
    normalize_restore_task_pk/1,
    normalize_schedule_id/1,
    normalize_task_pk/1,
    normalize_user_pk/1,
    parse_gid_param/1,
    resolve_category_uid/2,
    resolve_task_audit_meta/1,
    resolve_vote_group_id/1,
    task_group_id_by_uid/1,
    task_is_deleted/1
]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_group_sub_handler, Action) of
            undefined ->
                dispatch(Action, Method, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        dispatch(Action, Method, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%%

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(schedule_list, Method, Req0, State) -> schedule_list(Method, Req0, State);
dispatch(schedule_detail, Method, Req0, State) -> schedule_detail(Method, Req0, State);
dispatch(schedule_cancel, Method, Req0, State) -> schedule_cancel(Method, Req0, State);
dispatch(schedule_restore, Method, Req0, State) -> schedule_restore(Method, Req0, State);
dispatch(governance_log_list, Method, Req0, State) -> governance_log_list(Method, Req0, State);
dispatch(_, _Method, Req0, _State) -> Req0.

schedule_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:schedule:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_schedule_logic:list_group_schedules(Gid, Page, Size) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Reason} ->
                            ?ERROR_LOG(["adm schedule list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 群日程详情
-spec schedule_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
schedule_detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:schedule:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, ScheduleIdRaw} = elib_param:binary(schedule_id, Req0, <<>>),
            ScheduleId = normalize_schedule_id(ScheduleIdRaw),
            case ScheduleId of
                undefined ->
                    elib_response:error(Req0, "参数错误");
                _ ->
                    case group_schedule_logic:get_schedule_detail(ScheduleId) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, schedule_not_found} ->
                            elib_response:error(Req0, "日程不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm schedule detail error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 取消群日程
-spec schedule_cancel(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
schedule_cancel(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:schedule:cancel">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            ScheduleIdRaw = maps:get(<<"schedule_id">>, PostVals, <<>>),
            ScheduleId = normalize_schedule_id(ScheduleIdRaw),
            case ScheduleId of
                undefined ->
                    elib_response:error(Req0, "参数错误");
                _ ->
                    case group_schedule_ds:find_by_schedule_id(ScheduleId) of
                        #{<<"status">> := 4} ->
                            elib_response:error(Req0, "日程已取消");
                        #{<<"id">> := SchedulePk, <<"group_id">> := GroupId} ->
                            case group_schedule_ds:update_status(SchedulePk, 4) of
                                {ok, Affected} when Affected > 0 ->
                                    _ = audit_group_governance(
                                        maps:get(adm_user_id, State, 0),
                                        GroupId,
                                        <<"cancel_schedule">>,
                                        ScheduleId,
                                        #{
                                            <<"scope">> => <<"schedule">>,
                                            <<"schedule_pk">> => SchedulePk
                                        }
                                    ),
                                    elib_response:success(Req0, #{}, "操作成功");
                                {ok, 0} ->
                                    elib_response:error(Req0, "日程不存在");
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm schedule cancel error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        _ ->
                            elib_response:error(Req0, "日程不存在")
                    end
            end
    end.

%% @doc 恢复已取消群日程
-spec schedule_restore(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
schedule_restore(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:schedule:restore">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            ScheduleIdRaw = maps:get(<<"schedule_id">>, PostVals, <<>>),
            ScheduleId = normalize_schedule_id(ScheduleIdRaw),
            case ScheduleId of
                undefined ->
                    elib_response:error(Req0, "参数错误");
                _ ->
                    case group_schedule_ds:find_by_schedule_id(ScheduleId) of
                        #{<<"status">> := 4, <<"id">> := SchedulePk, <<"group_id">> := GroupId} ->
                            case group_schedule_ds:update_status(SchedulePk, 1) of
                                {ok, Affected} when Affected > 0 ->
                                    _ = audit_group_governance(
                                        maps:get(adm_user_id, State, 0),
                                        GroupId,
                                        <<"restore_schedule">>,
                                        ScheduleId,
                                        #{
                                            <<"scope">> => <<"schedule">>,
                                            <<"schedule_pk">> => SchedulePk,
                                            <<"previous_status">> => 4,
                                            <<"target_status">> => 1
                                        }
                                    ),
                                    elib_response:success(Req0, #{}, "操作成功");
                                {ok, 0} ->
                                    elib_response:error(Req0, "日程不存在");
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm schedule restore error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        #{<<"status">> := _} ->
                            elib_response:error(Req0, "日程未取消");
                        _ ->
                            elib_response:error(Req0, "日程不存在")
                    end
            end
    end.

%% @doc 群治理审计日志列表
-spec governance_log_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
governance_log_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_any_permission(State, [<<"groups:read">>, <<"logs:view">>], Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {Page, Size} = elib_param:page(Req0),
            Filters = extract_governance_log_filters(Req0),
            {WhereSql, Params} = build_governance_log_where_sql(Filters),
            case user_log_ds:page_group_governance_log(WhereSql, Params, Page, Size) of
                {ok, #{total := Total, list := Rows}} ->
                    Items = [normalize_governance_log_row(Row) || Row <- Rows],
                    elib_response:success(Req0, #{
                        list => Items,
                        total => Total,
                        page => Page,
                        size => Size,
                        total_pages => calc_total_pages(Total, Size)
                    });
                {error, Reason} ->
                    ?ERROR_LOG(["adm group governance log list error: ", Reason]),
                    elib_response:error(Req0, "查询失败")
            end
    end;
governance_log_list(_, Req0, _State) ->
    Req0.
