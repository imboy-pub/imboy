-module(adm_group_content_handler).
-dialyzer({nowarn_function, [category_list/3, category_delete/3, tag_list/3]}).
%%% 群组子资源 — adm_group_content_handler

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
dispatch(_, _Method, Req0, _State) -> Req0.

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
