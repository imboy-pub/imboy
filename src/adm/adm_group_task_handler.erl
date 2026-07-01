-module(adm_group_task_handler).
-dialyzer({nowarn_function, [task_detail/3, task_review/3]}).
%%% 群组子资源 — adm_group_task_handler (从 adm_group_sub_handler 拆分 2026-06-03)

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
dispatch(task_list, Method, Req0, State) -> task_list(Method, Req0, State);
dispatch(task_detail, Method, Req0, State) -> task_detail(Method, Req0, State);
dispatch(task_pending_review, Method, Req0, State) -> task_pending_review(Method, Req0, State);
dispatch(task_review, Method, Req0, State) -> task_review(Method, Req0, State);
dispatch(task_delete, Method, Req0, State) -> task_delete(Method, Req0, State);
dispatch(task_restore, Method, Req0, State) -> task_restore(Method, Req0, State);
dispatch(task_close, Method, Req0, State) -> task_close(Method, Req0, State);
dispatch(_, _Method, Req0, _State) -> Req0.

task_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:task:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {ok, Status} = elib_param:int(status, Req0, -1),
            {ok, Deleted} = elib_param:int(deleted, Req0, 0),
            {Page, Size} = elib_param:page(Req0),
            case Gid > 0 andalso (Deleted =:= 0 orelse Deleted =:= 1) of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case list_tasks_with_total(Gid, Status, Deleted, Page, Size) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Reason} ->
                            ?ERROR_LOG(["adm task list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 群任务详情
-spec task_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
task_detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:task:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, TaskIdRaw} = elib_param:binary(task_id, Req0, <<>>),
            TaskPk = normalize_task_pk(TaskIdRaw),
            case TaskPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_task_logic:detail(TaskPk) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, _Reason} ->
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 群任务待批改列表（状态=已提交）
-spec task_pending_review(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
task_pending_review(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:task:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, TaskIdRaw} = elib_param:binary(task_id, Req0, <<>>),
            {Page, Size} = elib_param:page(Req0),
            TaskPk = normalize_task_pk(TaskIdRaw),
            case TaskPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_task_ds:find_by_id(TaskPk) of
                        {ok, Task} ->
                            TaskUid = maps:get(<<"task_id">>, Task, <<>>),
                            case group_task_logic:pending_review(TaskUid, Page, Size) of
                                {ok, Assignments} ->
                                    case group_task_ds:assignment_count_by_status(TaskUid, 2) of
                                        {ok, Total} ->
                                            elib_response:success(Req0, #{
                                                list => Assignments,
                                                total => Total,
                                                page => Page,
                                                size => Size,
                                                total_pages => calc_total_pages(Total, Size)
                                            });
                                        {error, Reason} ->
                                            ?ERROR_LOG([
                                                "adm task pending_review count error: ", Reason
                                            ]),
                                            FallbackTotal = length(Assignments),
                                            elib_response:success(Req0, #{
                                                list => Assignments,
                                                total => FallbackTotal,
                                                page => Page,
                                                size => Size,
                                                total_pages => calc_total_pages(FallbackTotal, Size)
                                            })
                                    end;
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm task pending_review list error: ", Reason]),
                                    elib_response:error(Req0, "查询失败")
                            end;
                        {error, not_found} ->
                            elib_response:error(Req0, "任务不存在")
                    end
            end
    end;
task_pending_review(_, Req0, _State) ->
    Req0.

%% @doc 批改群任务分配
-spec task_review(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
task_review(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:task:review">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            AssignmentIdRaw = maps:get(<<"assignment_id">>, PostVals, <<>>),
            AssignmentPk = normalize_assignment_pk(AssignmentIdRaw),
            Score = maps:get(<<"score">>, PostVals, undefined),
            Comment = maps:get(<<"comment">>, PostVals, undefined),
            case AssignmentPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_task_ds:assignment_find_by_id(AssignmentPk) of
                        {ok, Assignment} ->
                            TaskUid = maps:get(<<"task_id">>, Assignment, <<>>),
                            GroupId = task_group_id_by_uid(TaskUid),
                            PrevStatus = maps:get(<<"status">>, Assignment, 0),
                            ReviewData = maps:filter(
                                fun(_K, V) -> V =/= undefined end,
                                #{score => Score, comment => Comment}
                            ),
                            ReviewerId = maps:get(adm_user_id, State, 0),
                            case group_task_logic:review(AssignmentPk, ReviewerId, ReviewData) of
                                ok ->
                                    Extra = maps:filter(
                                        fun(_K, V) -> V =/= undefined end,
                                        #{
                                            <<"scope">> => <<"task_assignment">>,
                                            <<"task_id">> => TaskUid,
                                            <<"assignee_uid">> => maps:get(
                                                <<"user_id">>, Assignment, 0
                                            ),
                                            <<"previous_status">> => PrevStatus,
                                            <<"target_status">> => 3,
                                            <<"score">> => Score
                                        }
                                    ),
                                    _ = audit_group_governance(
                                        ReviewerId,
                                        GroupId,
                                        <<"review_task">>,
                                        AssignmentPk,
                                        Extra
                                    ),
                                    elib_response:success(Req0, #{}, "操作成功");
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm task review error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        {error, not_found} ->
                            elib_response:error(Req0, "作业分配不存在")
                    end
            end
    end;
task_review(_, Req0, _State) ->
    Req0.

%% @doc 删除群任务（软删除）
-spec task_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
task_delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:task:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            TaskIdRaw = maps:get(<<"task_id">>, PostVals, <<>>),
            TaskPk = normalize_task_pk(TaskIdRaw),
            case TaskPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    {GroupId, TaskUid} = resolve_task_audit_meta(TaskPk),
                    case group_task_ds:soft_delete(TaskPk) of
                        {ok, _} ->
                            _ = audit_group_governance(
                                maps:get(adm_user_id, State, 0),
                                GroupId,
                                <<"delete_task">>,
                                TaskPk,
                                #{
                                    <<"scope">> => <<"task">>,
                                    <<"task_id">> => TaskUid
                                }
                            ),
                            elib_response:success(Req0, #{}, "操作成功");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm task delete error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
                    end
            end
    end.

%% @doc 恢复已删除群任务
-spec task_restore(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
task_restore(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:task:restore">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            TaskIdRaw = maps:get(<<"task_id">>, PostVals, <<>>),
            TaskPk = normalize_restore_task_pk(TaskIdRaw),
            case TaskPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_task_ds:find_any_by_id(TaskPk) of
                        {ok, Task} ->
                            case task_is_deleted(Task) of
                                false ->
                                    elib_response:error(Req0, "任务未删除");
                                true ->
                                    GroupId = maps:get(<<"group_id">>, Task, 0),
                                    TaskUid = maps:get(<<"task_id">>, Task, <<>>),
                                    case group_task_ds:restore(TaskPk) of
                                        {ok, Affected} when Affected > 0 ->
                                            _ = audit_group_governance(
                                                maps:get(adm_user_id, State, 0),
                                                GroupId,
                                                <<"restore_task">>,
                                                TaskPk,
                                                #{
                                                    <<"scope">> => <<"task">>,
                                                    <<"task_id">> => TaskUid
                                                }
                                            ),
                                            elib_response:success(Req0, #{}, "操作成功");
                                        {ok, 0} ->
                                            elib_response:error(Req0, "任务不存在");
                                        {error, Reason} ->
                                            ?ERROR_LOG(["adm task restore error: ", Reason]),
                                            elib_response:error(Req0, "操作失败")
                                    end
                            end;
                        {error, not_found} ->
                            elib_response:error(Req0, "任务不存在")
                    end
            end
    end.

%% @doc 强制结束群任务
-spec task_close(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
task_close(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:task:close">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            TaskIdRaw = maps:get(<<"task_id">>, PostVals, <<>>),
            TaskPk = normalize_task_pk(TaskIdRaw),
            case TaskPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_task_ds:find_by_id(TaskPk) of
                        {ok, #{<<"status">> := 3}} ->
                            elib_response:error(Req0, "任务已结束");
                        {ok, Task} ->
                            GroupId = maps:get(<<"group_id">>, Task, 0),
                            TaskUid = maps:get(<<"task_id">>, Task, <<>>),
                            PrevStatus = maps:get(<<"status">>, Task, 1),
                            case group_task_ds:update_task(TaskPk, #{status => 3}) of
                                {ok, Affected} when Affected > 0 ->
                                    _ = audit_group_governance(
                                        maps:get(adm_user_id, State, 0),
                                        GroupId,
                                        <<"close_task">>,
                                        TaskPk,
                                        #{
                                            <<"scope">> => <<"task">>,
                                            <<"task_id">> => TaskUid,
                                            <<"previous_status">> => PrevStatus,
                                            <<"target_status">> => 3
                                        }
                                    ),
                                    elib_response:success(Req0, #{}, "操作成功");
                                {ok, 0} ->
                                    elib_response:error(Req0, "任务不存在");
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm task close error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        {error, not_found} ->
                            elib_response:error(Req0, "任务不存在")
                    end
            end
    end.
