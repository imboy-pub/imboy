-module(adm_group_sub_handler).
%%%
% adm_group_sub 控制器模块
% 群组子资源管理 API（投票、公告、标签、分类、文件、相册、日程、任务、治理日志）
% 从 adm_group_handler.erl 拆分（2026-06-03），降低单文件复杂度。
%%%
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

%% ===================================================================
%% API
%% ===================================================================

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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(vote_list, Method, Req0, State) -> vote_list(Method, Req0, State);
dispatch(vote_detail, Method, Req0, State) -> vote_detail(Method, Req0, State);
dispatch(vote_close, Method, Req0, State) -> vote_close(Method, Req0, State);
dispatch(notice_list, Method, Req0, State) -> notice_list(Method, Req0, State);
dispatch(notice_detail, Method, Req0, State) -> notice_detail(Method, Req0, State);
dispatch(notice_delete, Method, Req0, State) -> notice_delete(Method, Req0, State);
dispatch(category_list, Method, Req0, State) -> category_list(Method, Req0, State);
dispatch(category_delete, Method, Req0, State) -> category_delete(Method, Req0, State);
dispatch(tag_list, Method, Req0, State) -> tag_list(Method, Req0, State);
dispatch(tag_delete, Method, Req0, State) -> tag_delete(Method, Req0, State);
dispatch(file_list, Method, Req0, State) -> file_list(Method, Req0, State);
dispatch(file_detail, Method, Req0, State) -> file_detail(Method, Req0, State);
dispatch(file_delete, Method, Req0, State) -> file_delete(Method, Req0, State);
dispatch(album_list, Method, Req0, State) -> album_list(Method, Req0, State);
dispatch(album_detail, Method, Req0, State) -> album_detail(Method, Req0, State);
dispatch(album_delete, Method, Req0, State) -> album_delete(Method, Req0, State);
dispatch(schedule_list, Method, Req0, State) -> schedule_list(Method, Req0, State);
dispatch(schedule_detail, Method, Req0, State) -> schedule_detail(Method, Req0, State);
dispatch(schedule_cancel, Method, Req0, State) -> schedule_cancel(Method, Req0, State);
dispatch(schedule_restore, Method, Req0, State) -> schedule_restore(Method, Req0, State);
dispatch(governance_log_list, Method, Req0, State) -> governance_log_list(Method, Req0, State);
dispatch(task_list, Method, Req0, State) -> task_list(Method, Req0, State);
dispatch(task_detail, Method, Req0, State) -> task_detail(Method, Req0, State);
dispatch(task_pending_review, Method, Req0, State) -> task_pending_review(Method, Req0, State);
dispatch(task_review, Method, Req0, State) -> task_review(Method, Req0, State);
dispatch(task_restore, Method, Req0, State) -> task_restore(Method, Req0, State);
dispatch(task_close, Method, Req0, State) -> task_close(Method, Req0, State);
dispatch(task_delete, Method, Req0, State) -> task_delete(Method, Req0, State);
dispatch(_, _Method, Req0, _State) -> Req0.

vote_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:vote:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_vote_logic:list_votes(Gid, Page, Size) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Reason} ->
                            ?ERROR_LOG(["adm vote list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 群投票详情
-spec vote_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
vote_detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:vote:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, VoteId0} = elib_param:binary(vote_id, Req0, <<>>),
            VoteId = ec_cnv:to_binary(VoteId0),
            case VoteId of
                <<>> ->
                    elib_response:error(Req0, "参数错误");
                _ ->
                    case group_vote_logic:get_vote_detail(VoteId) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, vote_not_found} ->
                            elib_response:error(Req0, "投票不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm vote detail error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 结束群投票
-spec vote_close(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
vote_close(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:vote:close">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            VoteId = maps:get(<<"vote_id">>, PostVals, <<>>),
            case VoteId of
                <<>> ->
                    elib_response:error(Req0, "参数错误");
                _ ->
                    case group_vote_logic:close_vote(VoteId) of
                        ok ->
                            GroupId = resolve_vote_group_id(VoteId),
                            _ = audit_group_governance(
                                maps:get(adm_user_id, State, 0),
                                GroupId,
                                <<"close_vote">>,
                                VoteId,
                                #{<<"scope">> => <<"vote">>}
                            ),
                            elib_response:success(Req0, #{}, "操作成功");
                        {error, vote_not_found} ->
                            elib_response:error(Req0, "投票不存在");
                        {error, vote_already_closed} ->
                            elib_response:error(Req0, "投票已结束");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm vote close error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
                    end
            end
    end.

%% @doc 群公告列表
-spec notice_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
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

%% @doc 群分组（分类）列表（按用户维度）
-spec category_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
category_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:category:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, UidRaw} = elib_param:binary(uid, Req0, <<>>),
            Gid = parse_gid_param(Req0),
            {ok, Keyword0} = elib_param:binary(keyword, Req0, <<>>),
            {Page, Size} = elib_param:page(Req0),
            Uid = resolve_category_uid(UidRaw, Gid),
            Keyword = ec_cnv:to_binary(Keyword0),
            case Uid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case list_user_categories_with_total(Uid, Keyword, Page, Size) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload#{uid => Uid, gid => Gid});
                        {error, Reason} when is_binary(Reason) ->
                            elib_response:error(Req0, Reason);
                        {error, Reason} ->
                            ?ERROR_LOG(["adm category list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 删除群分组（分类）
-spec category_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
category_delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:category:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            UidRaw = maps:get(<<"uid">>, PostVals, <<>>),
            GidRaw = maps:get(<<"gid">>, PostVals, 0),
            CategoryIdRaw =
                case maps:find(<<"category_id">>, PostVals) of
                    {ok, Value} -> Value;
                    error -> maps:get(<<"id">>, PostVals, <<>>)
                end,
            Uid = normalize_user_pk(UidRaw),
            Gid = normalize_positive_int(GidRaw),
            CategoryId = normalize_category_pk(CategoryIdRaw),
            case Uid > 0 andalso CategoryId > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_category_logic:delete(Uid, CategoryId) of
                        ok ->
                            _ = audit_group_governance(
                                maps:get(adm_user_id, State, 0),
                                Gid,
                                <<"delete_category">>,
                                CategoryId,
                                #{
                                    <<"scope">> => <<"category">>,
                                    <<"uid">> => Uid
                                }
                            ),
                            elib_response:success(Req0, #{}, "操作成功");
                        {error, Reason} when is_binary(Reason) ->
                            elib_response:error(Req0, Reason);
                        {error, Reason} ->
                            ?ERROR_LOG(["adm category delete error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
                    end
            end
    end.

%% @doc 群标签列表
-spec tag_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
tag_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:tag:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    Column = <<"id, group_id, tag_name, created_by, created_at">>,
                    case group_tag_ds:list_by_group(Gid, Column) of
                        {ok, List} ->
                            case group_tag_ds:count_by_group(Gid) of
                                {ok, Total} ->
                                    elib_response:success(Req0, #{
                                        list => List,
                                        total => Total
                                    });
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm tag list count error: ", Reason]),
                                    elib_response:success(Req0, #{
                                        list => List,
                                        total => length(List)
                                    })
                            end;
                        {error, Reason} ->
                            ?ERROR_LOG(["adm tag list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 删除群标签
-spec tag_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
tag_delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:tag:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            Gid = maps:get(<<"gid">>, PostVals, 0),
            TagName0 = maps:get(<<"tag_name">>, PostVals, <<>>),
            TagName =
                case TagName0 of
                    V when is_binary(V), V =/= <<>> ->
                        V;
                    V when is_list(V), V =/= [] ->
                        ec_cnv:to_binary(V);
                    _ ->
                        <<>>
                end,
            GroupId = normalize_positive_int(Gid),
            case GroupId > 0 andalso TagName =/= <<>> of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_tag_ds:delete(GroupId, TagName) of
                        {ok, Affected} when Affected > 0 ->
                            _ = audit_group_governance(
                                maps:get(adm_user_id, State, 0),
                                GroupId,
                                <<"delete_tag">>,
                                TagName,
                                #{<<"scope">> => <<"tag">>}
                            ),
                            elib_response:success(Req0, #{}, "操作成功");
                        {ok, 0} ->
                            elib_response:error(Req0, "标签不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm tag delete error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
                    end
            end
    end.

%% @doc 群文件列表
-spec file_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
file_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:file:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {ok, Category0} = elib_param:binary(category, Req0, <<>>),
            {ok, Keyword0} = elib_param:binary(keyword, Req0, <<>>),
            {Page, Size} = elib_param:page(Req0),
            Category = ec_cnv:to_binary(Category0),
            Keyword = ec_cnv:to_binary(Keyword0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case list_group_files_with_total(Gid, Category, Keyword, Page, Size) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Reason} ->
                            ?ERROR_LOG(["adm file list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 群文件详情
-spec file_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
file_detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:file:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, FileIdRaw} = elib_param:binary(file_id, Req0, <<>>),
            FilePk = normalize_file_pk(FileIdRaw),
            case FilePk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_file_ds:find_by_id(FilePk) of
                        File when is_map(File), map_size(File) > 0 ->
                            elib_response:success(Req0, File);
                        _ ->
                            elib_response:error(Req0, "文件不存在")
                    end
            end
    end.

%% @doc 删除群文件（软删除）
-spec file_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
file_delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:file:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            FileIdRaw = maps:get(<<"file_id">>, PostVals, <<>>),
            FilePk = normalize_file_pk(FileIdRaw),
            case FilePk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_file_ds:find_by_id(FilePk) of
                        File when is_map(File), map_size(File) > 0 ->
                            GroupId = maps:get(<<"group_id">>, File, 0),
                            FileUid = maps:get(<<"file_id">>, File, <<>>),
                            case group_file_ds:soft_delete(FilePk) of
                                {ok, Affected} when Affected > 0 ->
                                    _ = audit_group_governance(
                                        maps:get(adm_user_id, State, 0),
                                        GroupId,
                                        <<"delete_file">>,
                                        FilePk,
                                        #{
                                            <<"scope">> => <<"file">>,
                                            <<"file_id">> => FileUid
                                        }
                                    ),
                                    elib_response:success(Req0, #{}, "操作成功");
                                {ok, 0} ->
                                    elib_response:error(Req0, "文件不存在");
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm file delete error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        _ ->
                            elib_response:error(Req0, "文件不存在")
                    end
            end
    end.

%% @doc 群相册列表
-spec album_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
album_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:album:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_album_ds:list_albums(Gid, Page, Size) of
                        {ok, Payload0} ->
                            Payload = normalize_page_payload(Payload0, Page, Size),
                            elib_response:success(Req0, Payload);
                        {error, Reason} ->
                            ?ERROR_LOG(["adm album list error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
                    end
            end
    end.

%% @doc 群相册详情
-spec album_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
album_detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:album:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, AlbumIdRaw} = elib_param:binary(album_id, Req0, <<>>),
            AlbumPk = normalize_album_pk(AlbumIdRaw),
            case AlbumPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_album_ds:find_album_by_id(AlbumPk) of
                        Album when is_map(Album), map_size(Album) > 0 ->
                            elib_response:success(Req0, Album);
                        _ ->
                            elib_response:error(Req0, "相册不存在")
                    end
            end
    end.

%% @doc 删除群相册（软删除）
-spec album_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
album_delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:album:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            AlbumIdRaw = maps:get(<<"album_id">>, PostVals, <<>>),
            AlbumPk = normalize_album_pk(AlbumIdRaw),
            case AlbumPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_album_ds:find_album_by_id(AlbumPk) of
                        Album when is_map(Album), map_size(Album) > 0 ->
                            GroupId = maps:get(<<"group_id">>, Album, 0),
                            AlbumUid = maps:get(<<"album_id">>, Album, <<>>),
                            case group_album_ds:delete_album(AlbumPk) of
                                {ok, Affected} when Affected > 0 ->
                                    _ = audit_group_governance(
                                        maps:get(adm_user_id, State, 0),
                                        GroupId,
                                        <<"delete_album">>,
                                        AlbumPk,
                                        #{
                                            <<"scope">> => <<"album">>,
                                            <<"album_id">> => AlbumUid
                                        }
                                    ),
                                    elib_response:success(Req0, #{}, "操作成功");
                                {ok, 0} ->
                                    elib_response:error(Req0, "相册不存在");
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm album delete error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        _ ->
                            elib_response:error(Req0, "相册不存在")
                    end
            end
    end.

%% @doc 群日程列表
-spec schedule_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
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

%% @doc 群任务列表
-spec task_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
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
                        {error, Msg, _Code} ->
                            elib_response:error(Req0, Msg);
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
                                {error, Msg, _Code} ->
                                    elib_response:error(Req0, Msg);
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm task pending_review list error: ", Reason]),
                                    elib_response:error(Req0, "查询失败")
                            end;
                        {error, not_found} ->
                            elib_response:error(Req0, "任务不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm task pending_review task query error: ", Reason]),
                            elib_response:error(Req0, "查询失败")
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
                                {error, Msg, _Code} ->
                                    elib_response:error(Req0, Msg);
                                {error, Reason} ->
                                    ?ERROR_LOG(["adm task review error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        {error, not_found} ->
                            elib_response:error(Req0, "作业分配不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm task review assignment query error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
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
                            elib_response:error(Req0, "任务不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm task restore query error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
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
                            elib_response:error(Req0, "任务不存在");
                        {error, Reason} ->
                            ?ERROR_LOG(["adm task close query error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
                    end
            end
    end.
