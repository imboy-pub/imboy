-module(adm_group_handler).
%%%
% adm_group 控制器模块
% 群组管理 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(ADM_GROUP_AUDIT_TYPE, 902).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_group_handler, Action) of
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
dispatch(list, Method, Req0, State) -> list(Method, Req0, State);
dispatch(detail, Method, Req0, State) -> detail(Method, Req0, State);
dispatch(dissolve, Method, Req0, State) -> dissolve(Method, Req0, State);
dispatch(search, Method, Req0, State) -> search(Method, Req0, State);
dispatch(members, Method, Req0, State) -> members(Method, Req0, State);
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

%% @doc 群组列表
-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"groups:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {Page, Size} = elib_param:page(Req0),
            {ok, Status} = elib_param:int(status, Req0, -1),
            {ok, Type} = elib_param:int(type, Req0, -1),

            Where = build_where(Status, Type),
            {ok, P} = group_repo:page(Page, Size, Where, <<"created_at DESC">>),
            P2 = normalize_group_payload(P),
            elib_response:success(Req0, P2)
    end.

%% @doc 群组详情
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"groups:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            case Gid > 0 of
                true ->
                    Column = <<"id,title,avatar,introduction,owner_uid,creator_uid,member_count,member_max,type,join_limit,status,created_at">>,
                    Group = group_repo:find_by_id(Gid, Column),
                    case map_size(Group) > 0 of
                        true ->
                            % 获取群主信息
                            OwnerUid = maps:get(<<"owner_uid">>, Group),
                            Owner = user_repo:find_by_id(OwnerUid, <<"id,nickname,avatar">>),
                            Group2 = normalize_group(Group),
                            Result = Group2#{owner => normalize_user(Owner)},
                            elib_response:success(Req0, Result);
                        false ->
                            elib_response:error(Req0, "群组不存在")
                    end;
                false ->
                    elib_response:error(Req0, "参数错误")
            end
    end.

%% @doc 解散群组
-spec dissolve(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dissolve(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"groups:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            case Gid > 0 of
                true ->
                    case group_repo:update(#{id => Gid, status => -1}) of
                        {ok, _} ->
                            _ = audit_group_governance(
                                maps:get(adm_user_id, State, 0),
                                Gid,
                                <<"dissolve_group">>,
                                Gid,
                                #{<<"scope">> => <<"group">>}
                            ),
                            elib_response:success(Req0, #{}, "操作成功");
                        {error, Reason} ->
                            ?ERROR_LOG(["dissolve group error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
                    end;
                false ->
                    elib_response:error(Req0, "参数错误")
            end
    end.

%% @doc 搜索群组
-spec search(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
search(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"groups:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
            {Page, Size} = elib_param:page(Req0),
            
            case byte_size(Keyword) > 0 of
                true ->
                    Where = #{
                        'or' => [
                            #{title => {like, <<"%", Keyword/binary, "%">>}},
                            #{introduction => {like, <<"%", Keyword/binary, "%">>}}
                        ]
                    },
                    {ok, P} = group_repo:page(Page, Size, Where, <<"created_at DESC">>),
                    P2 = normalize_group_payload(P),
                    elib_response:success(Req0, P2);
                false ->
                    elib_response:error(Req0, "请输入搜索关键词")
            end
    end.

%% @doc 群组成员列表
-spec members(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
members(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"groups:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),
            
            case Gid > 0 of
                true ->
                    Column = <<"user_id,nickname,avatar,role,joined_at">>,
                    {ok, P} = group_member_repo:page_by_gid(Gid, Page, Size, Column),
                    P2 = normalize_member_payload(P),
                    elib_response:success(Req0, P2);
                false ->
                    elib_response:error(Req0, "参数错误")
            end
    end.

%% @doc 群投票列表
-spec vote_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
vote_list(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"groups:vote:read">>, Req0) of
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
    case ensure_permission(State, <<"groups:vote:read">>, Req0) of
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
    case ensure_permission(State, <<"groups:vote:close">>, Req0) of
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
    case ensure_permission(State, <<"groups:notice:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_notice_repo:list_by_group_id(Gid, Page, Size) of
                        {ok, List} ->
                            NoticeList = [normalize_notice_row(Item) || Item <- List],
                            case group_notice_repo:count_by_group_id(Gid) of
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
    case ensure_permission(State, <<"groups:notice:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, NoticeIdRaw} = elib_param:binary(notice_id, Req0, <<>>),
            NoticePk = normalize_notice_pk(NoticeIdRaw),
            case NoticePk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_notice_repo:find_by_id(NoticePk) of
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
    case ensure_permission(State, <<"groups:notice:delete">>, Req0) of
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
                    case group_notice_repo:find_by_id(NoticePk) of
                        {ok, Notice} ->
                            GroupId = maps:get(<<"group_id">>, Notice, 0),
                            case group_notice_repo:soft_delete(NoticePk) of
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
    case ensure_permission(State, <<"groups:category:read">>, Req0) of
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
    case ensure_permission(State, <<"groups:category:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            UidRaw = maps:get(<<"uid">>, PostVals, <<>>),
            GidRaw = maps:get(<<"gid">>, PostVals, 0),
            CategoryIdRaw = case maps:find(<<"category_id">>, PostVals) of
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
    case ensure_permission(State, <<"groups:tag:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    Column = <<"id, group_id, tag_name, created_by, created_at">>,
                    case group_tag_repo:list_by_group(Gid, Column) of
                        {ok, List} ->
                            case group_tag_repo:count_by_group(Gid) of
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
    case ensure_permission(State, <<"groups:tag:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            Gid = maps:get(<<"gid">>, PostVals, 0),
            TagName0 = maps:get(<<"tag_name">>, PostVals, <<>>),
            TagName = case TagName0 of
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
                    case group_tag_repo:delete(GroupId, TagName) of
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
    case ensure_permission(State, <<"groups:file:read">>, Req0) of
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
    case ensure_permission(State, <<"groups:file:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, FileIdRaw} = elib_param:binary(file_id, Req0, <<>>),
            FilePk = normalize_file_pk(FileIdRaw),
            case FilePk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_file_repo:find_by_id(FilePk) of
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
    case ensure_permission(State, <<"groups:file:delete">>, Req0) of
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
                    case group_file_repo:find_by_id(FilePk) of
                        File when is_map(File), map_size(File) > 0 ->
                            GroupId = maps:get(<<"group_id">>, File, 0),
                            FileUid = maps:get(<<"file_id">>, File, <<>>),
                            case group_file_repo:soft_delete(FilePk) of
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
    case ensure_permission(State, <<"groups:album:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),
            case Gid > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_album_repo:list_albums(Gid, Page, Size) of
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
    case ensure_permission(State, <<"groups:album:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, AlbumIdRaw} = elib_param:binary(album_id, Req0, <<>>),
            AlbumPk = normalize_album_pk(AlbumIdRaw),
            case AlbumPk > 0 of
                false ->
                    elib_response:error(Req0, "参数错误");
                true ->
                    case group_album_repo:find_album_by_id(AlbumPk) of
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
    case ensure_permission(State, <<"groups:album:delete">>, Req0) of
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
                    case group_album_repo:find_album_by_id(AlbumPk) of
                        Album when is_map(Album), map_size(Album) > 0 ->
                            GroupId = maps:get(<<"group_id">>, Album, 0),
                            AlbumUid = maps:get(<<"album_id">>, Album, <<>>),
                            case group_album_repo:delete_album(AlbumPk) of
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
    case ensure_permission(State, <<"groups:schedule:read">>, Req0) of
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
    case ensure_permission(State, <<"groups:schedule:read">>, Req0) of
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
    case ensure_permission(State, <<"groups:schedule:cancel">>, Req0) of
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
                    case group_schedule_repo:find_by_schedule_id(ScheduleId) of
                        #{<<"status">> := 4} ->
                            elib_response:error(Req0, "日程已取消");
                        #{<<"id">> := SchedulePk, <<"group_id">> := GroupId} ->
                            case group_schedule_repo:update_status(SchedulePk, 4) of
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
    case ensure_permission(State, <<"groups:schedule:restore">>, Req0) of
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
                    case group_schedule_repo:find_by_schedule_id(ScheduleId) of
                        #{<<"status">> := 4, <<"id">> := SchedulePk, <<"group_id">> := GroupId} ->
                            case group_schedule_repo:update_status(SchedulePk, 1) of
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
    case ensure_any_permission(State, [<<"groups:read">>, <<"logs:view">>], Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {Page, Size} = elib_param:page(Req0),
            Filters = extract_governance_log_filters(Req0),
            {WhereSql, Params} = build_governance_log_where_sql(Filters),
            TbLog = user_log_repo:tablename(),
            TbAdmUser = adm_user_repo:tablename(),
            BaseFrom = iolist_to_binary([
                <<" FROM ">>, TbLog, <<" l LEFT JOIN ">>, TbAdmUser, <<" u ON u.id = l.uid ">>,
                <<"WHERE l.type = ">>, integer_to_binary(?ADM_GROUP_AUDIT_TYPE),
                <<" AND l.remark = 'adm_group_governance'">>,
                WhereSql
            ]),
            CountSql = iolist_to_binary([<<"SELECT COUNT(*) AS count">>, BaseFrom]),
            case elib_pg:one(CountSql, Params) of
                {ok, CountRow} ->
                    Total = governance_log_count(CountRow),
                    Offset = (Page - 1) * Size,
                    LimitPos = integer_to_binary(length(Params) + 1),
                    OffsetPos = integer_to_binary(length(Params) + 2),
                    DataSql = iolist_to_binary([
                        <<"SELECT l.uid, u.account, u.nickname, l.body, l.created_at ">>,
                        BaseFrom,
                        <<" ORDER BY l.created_at DESC, l.uid DESC ">>,
                        <<"LIMIT $">>, LimitPos, <<" OFFSET $">>, OffsetPos
                    ]),
                    case elib_pg:query(DataSql, Params ++ [Size, Offset]) of
                        {ok, Rows} ->
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
                    end;
                {error, Reason} ->
                    ?ERROR_LOG(["adm group governance log count error: ", Reason]),
                    elib_response:error(Req0, "查询失败")
            end
    end;
governance_log_list(_, Req0, _State) ->
    Req0.

%% @doc 群任务列表
-spec task_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
task_list(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"groups:task:read">>, Req0) of
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
    case ensure_permission(State, <<"groups:task:read">>, Req0) of
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
    case ensure_permission(State, <<"groups:task:read">>, Req0) of
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
                    case group_task_repo:find_by_id(TaskPk) of
                        {ok, Task} ->
                            TaskUid = maps:get(<<"task_id">>, Task, <<>>),
                            case group_task_logic:pending_review(TaskUid, Page, Size) of
                                {ok, Assignments} ->
                                    case group_task_assignment_repo:count_by_status(TaskUid, 2) of
                                        {ok, Total} ->
                                            elib_response:success(Req0, #{
                                                list => Assignments,
                                                total => Total,
                                                page => Page,
                                                size => Size,
                                                total_pages => calc_total_pages(Total, Size)
                                            });
                                        {error, Reason} ->
                                            ?ERROR_LOG(["adm task pending_review count error: ", Reason]),
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
    case ensure_permission(State, <<"groups:task:review">>, Req0) of
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
                    case group_task_assignment_repo:find_by_id(AssignmentPk) of
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
                                            <<"assignee_uid">> => maps:get(<<"user_id">>, Assignment, 0),
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
    case ensure_permission(State, <<"groups:task:delete">>, Req0) of
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
                    case group_task_repo:soft_delete(TaskPk) of
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
    case ensure_permission(State, <<"groups:task:restore">>, Req0) of
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
                    case group_task_repo:find_any_by_id(TaskPk) of
                        {ok, Task} ->
                            case task_is_deleted(Task) of
                                false ->
                                    elib_response:error(Req0, "任务未删除");
                                true ->
                                    GroupId = maps:get(<<"group_id">>, Task, 0),
                                    TaskUid = maps:get(<<"task_id">>, Task, <<>>),
                                    case group_task_repo:restore(TaskPk) of
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
    case ensure_permission(State, <<"groups:task:close">>, Req0) of
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
                    case group_task_repo:find_by_id(TaskPk) of
                        {ok, #{<<"status">> := 3}} ->
                            elib_response:error(Req0, "任务已结束");
                        {ok, Task} ->
                            GroupId = maps:get(<<"group_id">>, Task, 0),
                            TaskUid = maps:get(<<"task_id">>, Task, <<>>),
                            PrevStatus = maps:get(<<"status">>, Task, 1),
                            case group_task_repo:update(TaskPk, #{status => 3}) of
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

-spec extract_governance_log_filters(cowboy_req:req()) -> map().
extract_governance_log_filters(Req0) ->
    Uid = parse_positive_param(uid, Req0),
    GroupId = parse_positive_param(group_id, Req0),
    {ok, Action} = elib_param:binary(action, Req0, <<>>),
    {ok, TargetId} = elib_param:binary(target_id, Req0, <<>>),
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {ok, FromTs0} = elib_param:binary(from_ts, Req0, <<>>),
    {ok, ToTs0} = elib_param:binary(to_ts, Req0, <<>>),
    #{
        uid => uid_or_zero(Uid),
        group_id => uid_or_zero(GroupId),
        action => ec_cnv:to_binary(Action),
        target_id => ec_cnv:to_binary(TargetId),
        keyword_like => keyword_like(Keyword),
        from_ts => normalize_audit_ts(FromTs0),
        to_ts => normalize_audit_ts(ToTs0)
    }.

-spec build_governance_log_where_sql(map()) -> {binary(), list()}.
build_governance_log_where_sql(Filters) ->
    Uid = maps:get(uid, Filters, 0),
    GroupId = maps:get(group_id, Filters, 0),
    Action = maps:get(action, Filters, <<>>),
    TargetId = maps:get(target_id, Filters, <<>>),
    KeywordLike = maps:get(keyword_like, Filters, <<>>),
    FromTs = maps:get(from_ts, Filters, <<>>),
    ToTs = maps:get(to_ts, Filters, <<>>),
    {Idx1, Parts1, Params1} = maybe_add_uid(Uid > 0, Uid, 1, [], []),
    {Idx2, Parts2, Params2} = maybe_add_governance_keyword(KeywordLike =/= <<>>, KeywordLike, Idx1, Parts1, Params1),
    {Idx3, Parts3, Params3} = maybe_add_governance_action(Action =/= <<>>, Action, Idx2, Parts2, Params2),
    {Idx4, Parts4, Params4} = maybe_add_governance_group_id(GroupId > 0, GroupId, Idx3, Parts3, Params3),
    {Idx5, Parts5, Params5} = maybe_add_governance_target_id(TargetId =/= <<>>, TargetId, Idx4, Parts4, Params4),
    {Idx6, Parts6, Params6} = maybe_add_from_ts(FromTs =/= <<>>, FromTs, Idx5, Parts5, Params5),
    {_Idx7, Parts7, Params7} = maybe_add_to_ts(ToTs =/= <<>>, ToTs, Idx6, Parts6, Params6),
    {iolist_to_binary(Parts7), Params7}.

-spec maybe_add_uid(boolean(), integer(), pos_integer(), [binary()], list()) -> {pos_integer(), [binary()], list()}.
maybe_add_uid(false, _Uid, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_uid(true, Uid, Index, Parts, Params) ->
    Cond = <<" AND l.uid = $", (integer_to_binary(Index))/binary>>,
    {Index + 1, Parts ++ [Cond], Params ++ [Uid]}.

-spec maybe_add_governance_keyword(boolean(), binary(), pos_integer(), [binary()], list()) -> {pos_integer(), [binary()], list()}.
maybe_add_governance_keyword(false, _KeywordLike, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_governance_keyword(true, KeywordLike, Index, Parts, Params) ->
    Pos = integer_to_binary(Index),
    Cond = <<
        " AND (u.account ILIKE $", Pos/binary,
        " OR u.nickname ILIKE $", Pos/binary,
        " OR l.body ILIKE $", Pos/binary, ")"
    >>,
    {Index + 1, Parts ++ [Cond], Params ++ [KeywordLike]}.

-spec maybe_add_governance_action(boolean(), binary(), pos_integer(), [binary()], list()) -> {pos_integer(), [binary()], list()}.
maybe_add_governance_action(false, _Action, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_governance_action(true, Action, Index, Parts, Params) ->
    Pattern = <<"%\"action\":\"", Action/binary, "\"%">>,
    Cond = <<" AND l.body ILIKE $", (integer_to_binary(Index))/binary>>,
    {Index + 1, Parts ++ [Cond], Params ++ [Pattern]}.

-spec maybe_add_governance_group_id(boolean(), integer(), pos_integer(), [binary()], list()) -> {pos_integer(), [binary()], list()}.
maybe_add_governance_group_id(false, _GroupId, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_governance_group_id(true, GroupId, Index, Parts, Params) ->
    Pattern = <<"%\"group_id\":", (integer_to_binary(GroupId))/binary, "%">>,
    Cond = <<" AND l.body ILIKE $", (integer_to_binary(Index))/binary>>,
    {Index + 1, Parts ++ [Cond], Params ++ [Pattern]}.

-spec maybe_add_governance_target_id(boolean(), binary(), pos_integer(), [binary()], list()) -> {pos_integer(), [binary()], list()}.
maybe_add_governance_target_id(false, _TargetId, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_governance_target_id(true, TargetId, Index, Parts, Params) ->
    Pos1 = integer_to_binary(Index),
    Pos2 = integer_to_binary(Index + 1),
    PatternAsNumber = <<"%\"target_id\":", TargetId/binary, "%">>,
    PatternAsString = <<"%\"target_id\":\"", TargetId/binary, "\"%">>,
    Cond = <<" AND (l.body ILIKE $", Pos1/binary, " OR l.body ILIKE $", Pos2/binary, ")">>,
    {Index + 2, Parts ++ [Cond], Params ++ [PatternAsNumber, PatternAsString]}.

-spec maybe_add_from_ts(boolean(), binary(), pos_integer(), [binary()], list()) -> {pos_integer(), [binary()], list()}.
maybe_add_from_ts(false, _FromTs, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_from_ts(true, FromTs, Index, Parts, Params) ->
    Cond = <<" AND l.created_at >= $", (integer_to_binary(Index))/binary, "::timestamptz">>,
    {Index + 1, Parts ++ [Cond], Params ++ [FromTs]}.

-spec maybe_add_to_ts(boolean(), binary(), pos_integer(), [binary()], list()) -> {pos_integer(), [binary()], list()}.
maybe_add_to_ts(false, _ToTs, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_to_ts(true, ToTs, Index, Parts, Params) ->
    Cond = <<" AND l.created_at <= $", (integer_to_binary(Index))/binary, "::timestamptz">>,
    {Index + 1, Parts ++ [Cond], Params ++ [ToTs]}.

-spec uid_or_zero(integer()) -> integer().
uid_or_zero(Value) when is_integer(Value), Value > 0 ->
    Value;
uid_or_zero(_) ->
    0.

-spec normalize_audit_ts(binary()) -> binary().
normalize_audit_ts(<<>>) ->
    <<>>;
normalize_audit_ts(Ts0) ->
    case elib_type:is_numeric(Ts0) of
        true ->
            elib_dt:to_rfc3339(Ts0);
        false ->
            case elib_dt:rfc3339_to(Ts0, microsecond) of
                Ts when is_integer(Ts) ->
                    Ts0;
                _ ->
                    <<>>
            end
    end.

-spec keyword_like(binary()) -> binary().
keyword_like(<<>>) ->
    <<>>;
keyword_like(Keyword) ->
    <<"%", Keyword/binary, "%">>.

-spec governance_log_count(term()) -> non_neg_integer().
governance_log_count(Row) when is_map(Row) ->
    case maps:get(<<"count">>, Row, maps:get(count, Row, 0)) of
        Count when is_integer(Count), Count >= 0 ->
            Count;
        Count ->
            ec_cnv:to_integer(Count)
    end;
governance_log_count(_) ->
    0.

-spec normalize_governance_log_row(map()) -> map().
normalize_governance_log_row(Row) ->
    BodyBin = row_value(Row, <<"body">>, <<"{}">>),
    BodyMap = decode_audit_body(BodyBin),
    #{
        uid => row_value(Row, <<"uid">>, 0),
        account => row_value(Row, <<"account">>, <<>>),
        nickname => row_value(Row, <<"nickname">>, <<>>),
        action => maps:get(<<"action">>, BodyMap, <<>>),
        operator_uid => maps:get(<<"operator_uid">>, BodyMap, 0),
        group_id => maps:get(<<"group_id">>, BodyMap, 0),
        target_id => maps:get(<<"target_id">>, BodyMap, <<>>),
        occurred_at => maps:get(<<"occurred_at">>, BodyMap, <<>>),
        extra => maps:get(<<"extra">>, BodyMap, #{}),
        created_at => row_value(Row, <<"created_at">>, <<>>),
        body => BodyBin
    }.

-spec decode_audit_body(binary()) -> map().
decode_audit_body(BodyBin) ->
    try jsone:decode(BodyBin, [{object_format, map}]) of
        Decoded when is_map(Decoded) ->
            Decoded;
        _ ->
            #{}
    catch
        _:_ ->
            #{}
    end.

-spec row_value(map(), binary(), any()) -> any().
row_value(Row, Key, Default) ->
    case maps:find(Key, Row) of
        {ok, Value} ->
            Value;
        error ->
            case maybe_existing_atom(Key) of
                undefined ->
                    Default;
                AtomKey ->
                    maps:get(AtomKey, Row, Default)
            end
    end.

-spec maybe_existing_atom(binary()) -> atom() | undefined.
maybe_existing_atom(Key) ->
    try binary_to_existing_atom(Key, utf8) of
        Atom when is_atom(Atom) ->
            Atom
    catch
        _:_ ->
            undefined
    end.

-spec ensure_permission(map(), binary(), cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_permission(State, Permission, Req0) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    case has_permission(AdmUserId, Permission) of
        true ->
            ok;
        false ->
            {error, elib_response:error(Req0, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)}
    end.

-spec ensure_any_permission(map(), [binary()], cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_any_permission(State, Permissions, Req0) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    case has_any_permission(AdmUserId, Permissions) of
        true ->
            ok;
        false ->
            {error, elib_response:error(Req0, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)}
    end.

-spec has_permission(term(), binary()) -> boolean().
has_permission(AdmUserId, Permission) when is_integer(AdmUserId), AdmUserId > 0, is_binary(Permission) ->
    Permissions = resolve_permissions_by_adm_user_id(AdmUserId),
    lists:member(Permission, Permissions);
has_permission(_, _) ->
    false.

-spec has_any_permission(term(), [binary()]) -> boolean().
has_any_permission(AdmUserId, Permissions)
    when is_integer(AdmUserId), AdmUserId > 0, is_list(Permissions) ->
    UserPermissions = resolve_permissions_by_adm_user_id(AdmUserId),
    lists:any(fun(Permission) -> lists:member(Permission, UserPermissions) end, Permissions);
has_any_permission(_, _) ->
    false.

-spec resolve_permissions_by_adm_user_id(integer()) -> list(binary()).
resolve_permissions_by_adm_user_id(AdmUserId) ->
    Key = {adm_user_group_permission, AdmUserId},
    case catch adm_user_logic:find(AdmUserId, <<"id,role_id">>, Key) of
        AdmUser when is_map(AdmUser) ->
            RoleIds = normalize_role_ids(maps:get(<<"role_id">>, AdmUser, 0)),
            lists:usort(lists:append([role_permissions(RoleId) || RoleId <- RoleIds]));
        _ ->
            []
    end.

-spec role_permissions(integer()) -> list(binary()).
role_permissions(RoleId) ->
    try adm_index_handler:role_acl(RoleId) of
        {_RoleName, Permissions, _MenuPaths} when is_list(Permissions) ->
            Permissions;
        _ ->
            []
    catch
        _:_ ->
            []
    end.

-spec normalize_role_ids(term()) -> list(integer()).
normalize_role_ids(RoleId) when is_integer(RoleId), RoleId > 0 ->
    [RoleId];
normalize_role_ids(RoleIds) when is_list(RoleIds) ->
    lists:usort([Id || Value <- RoleIds, Id <- [normalize_role_id(Value)], Id > 0]);
normalize_role_ids(RoleId) ->
    case normalize_role_id(RoleId) of
        Id when Id > 0 ->
            [Id];
        _ ->
            []
    end.

-spec normalize_role_id(term()) -> integer().
normalize_role_id(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_role_id(Value) when is_binary(Value); is_list(Value) ->
    try ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    catch
        _:_ ->
            0
    end;
normalize_role_id(_) ->
    0.

%% @doc 构建查询条件
-spec build_where(integer(), integer()) -> map().
build_where(Status, Type) when Status >= 0, Type >= 0 ->
    #{status => Status, type => Type};
build_where(Status, _Type) when Status >= 0 ->
    #{status => Status};
build_where(_Status, Type) when Type >= 0 ->
    #{type => Type};
build_where(_Status, _Type) ->
    #{}.

-spec list_tasks_with_total(integer(), integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_tasks_with_total(Gid, Status, Deleted, Page, Size) when Deleted =:= 1 ->
    case Status of
        S when is_integer(S), S >= 1, S =< 3 ->
            case group_task_repo:list_deleted_by_group_id(Gid, S, Page, Size) of
                {ok, List} ->
                    case group_task_repo:count_deleted_by_group_id(Gid, S) of
                        {ok, Total} ->
                            {ok, #{
                                list => List,
                                total => Total,
                                page => Page,
                                size => Size,
                                total_pages => calc_total_pages(Total, Size)
                            }};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            case group_task_repo:list_deleted_by_group_id(Gid, Page, Size) of
                {ok, List} ->
                    case group_task_repo:count_deleted_by_group_id(Gid) of
                        {ok, Total} ->
                            {ok, #{
                                list => List,
                                total => Total,
                                page => Page,
                                size => Size,
                                total_pages => calc_total_pages(Total, Size)
                            }};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end;
list_tasks_with_total(Gid, Status, _Deleted, Page, Size) ->
    case Status of
        S when is_integer(S), S >= 1, S =< 3 ->
            case group_task_repo:list_by_group_id(Gid, S, Page, Size) of
                {ok, List} ->
                    case group_task_repo:count_by_group_id(Gid, S) of
                        {ok, Total} ->
                            {ok, #{
                                list => List,
                                total => Total,
                                page => Page,
                                size => Size,
                                total_pages => calc_total_pages(Total, Size)
                            }};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            case group_task_repo:list_by_group_id(Gid, Page, Size) of
                {ok, List} ->
                    case group_task_repo:count_by_group_id(Gid) of
                        {ok, Total} ->
                            {ok, #{
                                list => List,
                                total => Total,
                                page => Page,
                                size => Size,
                                total_pages => calc_total_pages(Total, Size)
                            }};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end.

-spec calc_total_pages(non_neg_integer(), pos_integer()) -> non_neg_integer().
calc_total_pages(Total, Size) when is_integer(Total), is_integer(Size), Size > 0 ->
    case Total of
        0 -> 0;
        _ -> (Total + Size - 1) div Size
    end.

-spec normalize_page_payload(map(), integer(), integer()) -> map().
normalize_page_payload(Payload0, Page, Size) when is_map(Payload0) ->
    Items = maps:get(items, Payload0, maps:get(list, Payload0, [])),
    Total = maps:get(total, Payload0, length(Items)),
    #{
        list => Items,
        total => Total,
        page => maps:get(page, Payload0, Page),
        size => maps:get(size, Payload0, Size),
        total_pages => calc_total_pages(Total, maps:get(size, Payload0, Size))
    };
normalize_page_payload(_, Page, Size) ->
    #{
        list => [],
        total => 0,
        page => Page,
        size => Size,
        total_pages => 0
    }.

-spec list_user_categories_with_total(integer(), binary(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_user_categories_with_total(Uid, Keyword, Page, Size) ->
    case group_category_logic:list(Uid) of
        {ok, Categories0} when is_list(Categories0) ->
            Categories1 = [normalize_category_row(Item) || Item <- Categories0],
            Categories2 = filter_categories_by_keyword(Categories1, Keyword),
            Total = length(Categories2),
            Items = paginate_items(Categories2, Page, Size),
            {ok, #{
                list => Items,
                total => Total,
                page => Page,
                size => Size,
                total_pages => calc_total_pages(Total, Size)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec filter_categories_by_keyword(list(map()), binary()) -> list(map()).
filter_categories_by_keyword(Categories, Keyword) when Keyword =:= <<>> ->
    Categories;
filter_categories_by_keyword(Categories, Keyword) ->
    [Category || Category <- Categories, category_name_contains(Category, Keyword)].

-spec category_name_contains(map(), binary()) -> boolean().
category_name_contains(Category, Keyword) ->
    Name = ec_cnv:to_binary(maps:get(<<"category_name">>, Category, <<>>)),
    binary:match(Name, Keyword) =/= nomatch.

-spec paginate_items(list(), integer(), integer()) -> list().
paginate_items(Items, Page, Size) ->
    SafePage = case Page of
        P when is_integer(P), P > 0 -> P;
        _ -> 1
    end,
    SafeSize = case Size of
        S when is_integer(S), S > 0 -> S;
        _ -> 10
    end,
    Offset = (SafePage - 1) * SafeSize,
    lists:sublist(drop_items(Items, Offset), SafeSize).

-spec drop_items(list(), non_neg_integer()) -> list().
drop_items(Items, Offset) when Offset =< 0 ->
    Items;
drop_items([], _Offset) ->
    [];
drop_items([_Head | Tail], Offset) ->
    drop_items(Tail, Offset - 1).

-spec parse_gid_param(cowboy_req:req()) -> integer().
parse_gid_param(Req0) ->
    parse_positive_param(gid, Req0).

-spec parse_positive_param(atom(), cowboy_req:req()) -> integer().
parse_positive_param(Key, Req0) ->
    Value0 =
        case catch elib_param:binary(Key, Req0, <<>>) of
            {ok, Value} ->
                Value;
            _ ->
                case catch elib_param:int(Key, Req0, 0) of
                    {ok, Value} -> Value;
                    _ -> 0
                end
        end,
    normalize_positive_int(Value0).

-spec resolve_category_uid(term(), integer()) -> integer().
resolve_category_uid(UidRaw, Gid) ->
    case normalize_user_pk(UidRaw) of
        Uid when Uid > 0 ->
            Uid;
        _ when is_integer(Gid), Gid > 0 ->
            owner_uid_by_gid(Gid);
        _ ->
            0
    end.

-spec owner_uid_by_gid(integer()) -> integer().
owner_uid_by_gid(Gid) when is_integer(Gid), Gid > 0 ->
    case group_repo:find_by_id(Gid, <<"owner_uid">>) of
        #{<<"owner_uid">> := OwnerUid} when is_integer(OwnerUid), OwnerUid > 0 ->
            OwnerUid;
        _ ->
            0
    end;
owner_uid_by_gid(_) ->
    0.

-spec normalize_positive_int(term()) -> integer().
normalize_positive_int(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_positive_int(Value) when is_list(Value) ->
    normalize_positive_int(ec_cnv:to_binary(Value));
normalize_positive_int(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            ec_cnv:to_integer(Value);
        false ->
            case catch elib_hashids:decode(Value) of
                Id when is_integer(Id), Id > 0 -> Id;
                _ -> 0
            end
    end;
normalize_positive_int(_) ->
    0.

-spec normalize_user_pk(term()) -> integer().
normalize_user_pk(undefined) ->
    0;
normalize_user_pk(<<>>) ->
    0;
normalize_user_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_user_pk(Value) when is_list(Value) ->
    normalize_user_pk(ec_cnv:to_binary(Value));
normalize_user_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case elib_hashids:decode(Value) of
                        Id when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            0
                    end
            end
    end;
normalize_user_pk(_) ->
    0.

-spec normalize_category_row(map()) -> map().
normalize_category_row(Row) when is_map(Row) ->
    case maps:get(<<"id">>, Row, undefined) of
        Id when is_integer(Id), Id >= 0 ->
            Row#{<<"category_id">> => Id};
        _ ->
            Row
    end;
normalize_category_row(Row) ->
    Row.

-spec normalize_category_pk(term()) -> integer().
normalize_category_pk(undefined) ->
    0;
normalize_category_pk(<<>>) ->
    0;
normalize_category_pk(Value) when is_integer(Value), Value >= 0 ->
    Value;
normalize_category_pk(Value) when is_list(Value) ->
    normalize_category_pk(ec_cnv:to_binary(Value));
normalize_category_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case elib_hashids:decode(Value) of
                        Id when is_integer(Id), Id >= 0 ->
                            Id;
                        _ ->
                            0
                    end
            end
    end;
normalize_category_pk(_) ->
    0.

-spec list_group_files_with_total(integer(), binary(), binary(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_group_files_with_total(Gid, _Category, Keyword, Page, Size) when Keyword =/= <<>> ->
    case group_file_repo:search_by_name(Gid, Keyword, Page, Size) of
        {ok, List} ->
            Total = length(List),
            {ok, #{
                list => List,
                total => Total,
                page => Page,
                size => Size,
                total_pages => calc_total_pages(Total, Size)
            }};
        Error ->
            Error
    end;
list_group_files_with_total(Gid, Category, _Keyword, Page, Size) when Category =/= <<>> ->
    case group_file_repo:list_by_category(Gid, Category, Page, Size) of
        {ok, List} ->
            Total = length(List),
            {ok, #{
                list => List,
                total => Total,
                page => Page,
                size => Size,
                total_pages => calc_total_pages(Total, Size)
            }};
        Error ->
            Error
    end;
list_group_files_with_total(Gid, _Category, _Keyword, Page, Size) ->
    case group_file_repo:list_by_group(Gid, Page, Size, #{}) of
        {ok, List} ->
            case group_file_repo:count_by_group(Gid) of
                {ok, Total} ->
                    {ok, #{
                        list => List,
                        total => Total,
                        page => Page,
                        size => Size,
                        total_pages => calc_total_pages(Total, Size)
                    }};
                _ ->
                    Total = length(List),
                    {ok, #{
                        list => List,
                        total => Total,
                        page => Page,
                        size => Size,
                        total_pages => calc_total_pages(Total, Size)
                    }}
            end;
        Error ->
            Error
    end.

%% @doc 兼容 schedule_id:
%% - 原生 schedule_id (sched_xxx)
%% - 数字主键（int/数字字符串）
%% - hashid 主键
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
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    schedule_id_by_pk(ec_cnv:to_integer(Value));
                false ->
                    case group_schedule_repo:find_by_schedule_id(Value) of
                        #{<<"schedule_id">> := _} ->
                            Value;
                        _ ->
                            case elib_hashids:decode(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    schedule_id_by_pk(Id);
                                _ ->
                                    undefined
                            end
                    end
            end
    end;
normalize_schedule_id(_) ->
    undefined.

-spec normalize_notice_pk(term()) -> integer().
normalize_notice_pk(undefined) ->
    0;
normalize_notice_pk(<<>>) ->
    0;
normalize_notice_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_notice_pk(Value) when is_list(Value) ->
    normalize_notice_pk(ec_cnv:to_binary(Value));
normalize_notice_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case elib_hashids:decode(Value) of
                        Id when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            0
                    end
            end
    end;
normalize_notice_pk(_) ->
    0.

-spec normalize_notice_row(map()) -> map().
normalize_notice_row(Notice) when is_map(Notice) ->
    case maps:get(<<"id">>, Notice, undefined) of
        Id when is_integer(Id), Id > 0 ->
            Notice#{<<"notice_id">> => Id};
        _ ->
            Notice
    end;
normalize_notice_row(Row) ->
    Row.

-spec normalize_file_pk(term()) -> integer().
normalize_file_pk(undefined) ->
    0;
normalize_file_pk(<<>>) ->
    0;
normalize_file_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_file_pk(Value) when is_list(Value) ->
    normalize_file_pk(ec_cnv:to_binary(Value));
normalize_file_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case group_file_repo:find_by_file_id(Value) of
                        #{<<"id">> := Id} when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            case elib_hashids:decode(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    Id;
                                _ ->
                                    0
                            end
                    end
            end
    end;
normalize_file_pk(_) ->
    0.

-spec normalize_album_pk(term()) -> integer().
normalize_album_pk(undefined) ->
    0;
normalize_album_pk(<<>>) ->
    0;
normalize_album_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_album_pk(Value) when is_list(Value) ->
    normalize_album_pk(ec_cnv:to_binary(Value));
normalize_album_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case group_album_repo:find_album_by_album_id(Value) of
                        #{<<"id">> := Id} when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            case elib_hashids:decode(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    Id;
                                _ ->
                                    0
                            end
                    end
            end
    end;
normalize_album_pk(_) ->
    0.

-spec schedule_id_by_pk(integer()) -> binary() | undefined.
schedule_id_by_pk(Id) when is_integer(Id), Id > 0 ->
    case group_schedule_repo:find_by_id(Id, <<"schedule_id">>) of
        #{<<"schedule_id">> := ScheduleId} when is_binary(ScheduleId), ScheduleId =/= <<>> ->
            ScheduleId;
        _ ->
            undefined
    end;
schedule_id_by_pk(_) ->
    undefined.

-spec normalize_task_pk(term()) -> integer().
normalize_task_pk(undefined) ->
    0;
normalize_task_pk(<<>>) ->
    0;
normalize_task_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_task_pk(Value) when is_list(Value) ->
    normalize_task_pk(ec_cnv:to_binary(Value));
normalize_task_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case group_task_repo:find_by_task_id(Value) of
                        {ok, #{<<"id">> := Id}} when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            case elib_hashids:decode(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    Id;
                                _ ->
                                    0
                            end
                    end
            end
    end;
normalize_task_pk(_) ->
    0.

-spec normalize_restore_task_pk(term()) -> integer().
normalize_restore_task_pk(undefined) ->
    0;
normalize_restore_task_pk(<<>>) ->
    0;
normalize_restore_task_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_restore_task_pk(Value) when is_list(Value) ->
    normalize_restore_task_pk(ec_cnv:to_binary(Value));
normalize_restore_task_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case group_task_repo:find_any_by_task_id(Value) of
                        {ok, #{<<"id">> := Id}} when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            case elib_hashids:decode(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    Id;
                                _ ->
                                    0
                            end
                    end
            end
    end;
normalize_restore_task_pk(_) ->
    0.

-spec normalize_assignment_pk(term()) -> integer().
normalize_assignment_pk(undefined) ->
    0;
normalize_assignment_pk(<<>>) ->
    0;
normalize_assignment_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_assignment_pk(Value) when is_list(Value) ->
    normalize_assignment_pk(ec_cnv:to_binary(Value));
normalize_assignment_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case elib_hashids:decode(Value) of
                        Id when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            0
                    end
            end
    end;
normalize_assignment_pk(_) ->
    0.

-spec task_is_deleted(map()) -> boolean().
task_is_deleted(Task) when is_map(Task) ->
    case maps:get(<<"deleted_at">>, Task, null) of
        undefined ->
            false;
        null ->
            false;
        <<>> ->
            false;
        _ ->
            true
    end;
task_is_deleted(_) ->
    false.

-spec resolve_vote_group_id(binary()) -> integer().
resolve_vote_group_id(VoteId) when is_binary(VoteId), VoteId =/= <<>> ->
    case group_vote_repo:find_by_vote_id(VoteId) of
        {ok, Vote} ->
            maps:get(<<"group_id">>, Vote, 0);
        _ ->
            0
    end;
resolve_vote_group_id(_) ->
    0.

-spec resolve_task_audit_meta(integer()) -> {integer(), binary()}.
resolve_task_audit_meta(TaskPk) when is_integer(TaskPk), TaskPk > 0 ->
    case group_task_repo:find_by_id(TaskPk) of
        {ok, Task} ->
            {
                maps:get(<<"group_id">>, Task, 0),
                maps:get(<<"task_id">>, Task, <<>>)
            };
        _ ->
            {0, <<>>}
    end;
resolve_task_audit_meta(_) ->
    {0, <<>>}.

-spec task_group_id_by_uid(binary()) -> integer().
task_group_id_by_uid(TaskUid) when is_binary(TaskUid), TaskUid =/= <<>> ->
    case group_task_repo:find_by_task_id(TaskUid) of
        {ok, Task} ->
            maps:get(<<"group_id">>, Task, 0);
        _ ->
            0
    end;
task_group_id_by_uid(_) ->
    0.

-spec audit_group_governance(integer(), integer(), binary(), term(), map()) -> ok.
audit_group_governance(AdmUserId, _GroupId, _Action, _TargetId, _Extra)
    when not is_integer(AdmUserId); AdmUserId =< 0 ->
    ok;
audit_group_governance(AdmUserId, GroupId, Action, TargetId, Extra) ->
    Now = elib_dt:now(),
    AuditBody = #{
        <<"action">> => Action,
        <<"operator_uid">> => AdmUserId,
        <<"group_id">> => GroupId,
        <<"target_id">> => TargetId,
        <<"occurred_at">> => Now,
        <<"extra">> => Extra
    },
    try
        _ = user_log_repo:add(#{
            type => ?ADM_GROUP_AUDIT_TYPE,
            uid => AdmUserId,
            body => jsone:encode(AuditBody, [native_utf8]),
            remark => <<"adm_group_governance">>,
            created_at => Now
        }),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?DEBUG_LOG("群治理审计日志写入失败: ~p", [{Class, Reason, Stacktrace}]),
            ok
    end.

%% ===================================================================
%% 数据规范化函数（ID编码）
%% ===================================================================

%% @doc 规范化群组分页数据（编码ID字段）
-spec normalize_group_payload(map()) -> map().
normalize_group_payload(Payload) ->
    List = maps:get(list, Payload, []),
    List2 = [normalize_group(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

%% @doc 规范化单条群组数据（编码ID字段）
-spec normalize_group(map()) -> map().
normalize_group(Group) ->
    elib_hashids:replace_fields(Group, [<<"id">>, <<"owner_uid">>, <<"creator_uid">>]).

%% @doc 规范化群成员分页数据（编码ID字段）
-spec normalize_member_payload(map()) -> map().
normalize_member_payload(Payload) ->
    List = maps:get(list, Payload, []),
    List2 = [normalize_member(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

%% @doc 规范化单条群成员数据（编码ID字段）
-spec normalize_member(map()) -> map().
normalize_member(Member) ->
    elib_hashids:replace_fields(Member, [<<"user_id">>]).

%% @doc 规范化用户数据（编码ID字段）
-spec normalize_user(map()) -> map().
normalize_user(User) ->
    elib_hashids:replace_fields(User, [<<"id">>]).
