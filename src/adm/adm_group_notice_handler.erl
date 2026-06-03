-module(adm_group_notice_handler).
%%% adm_group_notice_handler

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
dispatch(notice_list, Method, Req0, State) -> notice_list(Method, Req0, State);
dispatch(notice_detail, Method, Req0, State) -> notice_detail(Method, Req0, State);
dispatch(notice_delete, Method, Req0, State) -> notice_delete(Method, Req0, State);
dispatch(_, _Method, Req0, _State) -> Req0.

notice_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:notice:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_notice_ds:list_by_group_id(Gid, Page, Size) of
                        {ok, List} ->
                            NoticeList = [normalize_notice_row(Item) || Item <- List],
                            case group_notice_ds:count_by_group_id(Gid) of
                                {ok, Total} ->
                                    elib_response:success(Req0, #{
                                        list => NoticeList,
                                        total => Total,
                                        page => Page,
                                        size => Size,
                                        total_pages => calc_total_pages(Total, Size)
                                    });
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm notice list count error: ", Reason]),
                                    FallbackTotal = length(NoticeList),
                                    elib_response:success(Req0, #{
                                        list => NoticeList,
                                        total => FallbackTotal,
                                        page => Page,
                                        size => Size,
                                        total_pages => calc_total_pages(FallbackTotal, Size)
                                    })
                            end;
                        {error, Reason} ->
                            ?ERROR_LOG(["adm notice list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 群公告详情
-spec notice_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
notice_detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:notice:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, NoticeIdRaw} = elib_param:binary(notice_id, Req0, <<>>),
            NoticePk = normalize_notice_pk(NoticeIdRaw),
            case NoticePk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_notice_ds:find_by_id(NoticePk) of
                        {ok, Notice} ->
                            elib_response:success(Req0, normalize_notice_row(Notice));
                        {error, not_found} ->
                            elib_response:error(Req0, "公告不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm notice detail error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 删除群公告（软删除）
-spec notice_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
notice_delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:notice:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            NoticeIdRaw = maps:get(<<"notice_id">>, PostVals, <<>>),
            NoticePk = normalize_notice_pk(NoticeIdRaw),
            case NoticePk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_notice_ds:find_by_id(NoticePk) of
                        {ok, Notice} ->
                            GroupId = maps:get(<<"group_id">>, Notice, 0),
                            case group_notice_ds:soft_delete(NoticePk) of
                                {ok, Affected} when Affected > 0 ->
                                    _ = audit_group_governance(
                                        maps:get(adm_user_id, State, 0),
                                        GroupId,
                                        <<"delete_notice">>,
                                        NoticePk,
                                        #{<<"scope">> => <<"notice">>}
                                    ),
                                    elib_response:success(Req0, #{}, "操作成功");
                                {ok, 0} ->
                                    elib_response:error(Req0, "公告不存在");
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm notice delete error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        {error, not_found} ->
                            elib_response:error(Req0, "公告不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm notice delete query error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
                    end
            end
    end.
