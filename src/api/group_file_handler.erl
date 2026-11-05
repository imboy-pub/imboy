-module(group_file_handler).

-moduledoc "群文件 API 处理器 / Group file API —— 按 action 分发的群文件管理端点。".
-dialyzer({nowarn_function, [handle_action/3, download/2]}).

-behaviour(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("error_code.hrl").
-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群文件处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(bind, Req, State) -> bind(Req, State);
handle_action(download, Req, State) -> download(Req, State);
handle_action(list, Req, State) -> list(Req, State);
handle_action(delete, Req, State) -> delete(Req, State);
handle_action(search, Req, State) -> search(Req, State);
handle_action(categories, Req, State) -> categories(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Action 处理函数
%% ===================================================================

%% C08（V3-12）：multipart 正文上传（file/upload，"登记即绑定"旧链）已退役。
%% 群文件唯一引用入口为 direct-v1 confirmed 逻辑引用绑定（file/bind）。

%% direct-v1 确认引用绑定（step-11）：JSON 引用 bind，不收正文。
%% POST /v1/group/file/bind {gid, file_name, file_type, object_key, file_size, file_md5}
-spec bind(cowboy_req:req(), map()) -> cowboy_req:req().
bind(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),
    Gid = elib_cnv:safe_to_integer(maps:get(<<"gid">>, Body, 0)),
    ObjectKey = maps:get(<<"object_key">>, Body, <<>>),
    FileName = maps:get(<<"file_name">>, Body, <<>>),
    FileType = maps:get(<<"file_type">>, Body, <<>>),
    FileSize = elib_cnv:safe_to_integer(maps:get(<<"file_size">>, Body, 0)),
    FileMd5 = maps:get(<<"file_md5">>, Body, <<>>),
    case {Gid, FileName, ObjectKey} of
        {0, _, _} ->
            elib_response:error(Req1, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        {_, <<>>, _} ->
            elib_response:error(Req1, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        {_, _, <<>>} ->
            elib_response:error(Req1, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        _ ->
            case
                group_file_ds:bind_file(
                    Gid,
                    CurrentUid,
                    FileName,
                    FileType,
                    ObjectKey,
                    FileSize,
                    FileMd5
                )
            of
                {ok, FileId} ->
                    elib_response:success(Req1, #{<<"file_id">> => FileId});
                {error, not_member} ->
                    elib_response:error(Req1, <<"非群组成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER);
                {error, not_confirmed} ->
                    elib_response:error(Req1, <<"附件未确认"/utf8>>, ?ERR_BAD_REQUEST);
                {error, cross_subject} ->
                    elib_response:error(Req1, <<"附件归属不匹配"/utf8>>, ?ERR_FORBIDDEN);
                {error, invalid_file_type} ->
                    elib_response:error(Req1, <<"不允许的文件类型"/utf8>>, ?ERR_FILE_TYPE_NOT_ALLOWED);
                {error, ?ERR_WORKSPACE_ARCHIVED} ->
                    elib_response:error(
                        Req1,
                        imboy_error:error_msg(?ERR_WORKSPACE_ARCHIVED),
                        ?ERR_WORKSPACE_ARCHIVED
                    );
                {error, {?ERR_WORKSPACE_ARCHIVED, _}} ->
                    elib_response:error(
                        Req1,
                        imboy_error:error_msg(?ERR_WORKSPACE_ARCHIVED),
                        ?ERR_WORKSPACE_ARCHIVED
                    );
                {error, _Reason} ->
                    elib_response:error(Req1, <<"绑定失败"/utf8>>, ?ERR_FILE_UPLOAD_FAILED)
            end
    end.

%% @doc 下载文件
%% GET /v1/group/file/download?file_id=xxx
-spec download(cowboy_req:req(), map()) -> cowboy_req:req().
download(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    FileIdStr = proplists:get_value(<<"file_id">>, Qs, <<>>),

    % 验证参数
    case FileIdStr of
        <<>> ->
            elib_response:error(Req0, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        _ ->
            FileId = elib_cnv:safe_to_integer(FileIdStr),
            % 调用 Logic 层下载文件
            case group_file_logic:download(FileId, CurrentUid) of
                {ok, FileUrl} ->
                    % 重定向到文件URL
                    cowboy_req:reply(302, #{<<"location">> => FileUrl}, Req0);
                {error, not_member} ->
                    elib_response:error(Req0, <<"非群组成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER);
                {error, not_found} ->
                    elib_response:error(Req0, <<"文件不存在"/utf8>>, ?ERR_FILE_NOT_FOUND);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"文件下载失败"/utf8>>, ?ERR_FILE_DOWNLOAD_FAILED)
            end
    end.

%% @doc 查询文件列表
%% GET /v1/group/file/list?gid=xxx&page=1&size=20&category=document
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs, <<>>),
    PageStr = proplists:get_value(<<"page">>, Qs, <<"1">>),
    SizeStr = proplists:get_value(<<"size">>, Qs, <<"20">>),
    Category = proplists:get_value(<<"category">>, Qs, undefined),

    % 验证参数
    case Gid of
        <<>> ->
            elib_response:error(Req0, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        _ ->
            Page = safe_page(PageStr, 1),
            Size = safe_page(SizeStr, 20),
            Options =
                case Category of
                    undefined -> #{};
                    _ -> #{category => Category}
                end,

            % 调用 Logic 层查询文件列表
            case group_file_logic:list(Gid, CurrentUid, Page, Size, Options) of
                {ok, FileList} ->
                    elib_response:success(Req0, FileList);
                {error, not_member} ->
                    elib_response:error(Req0, <<"非群组成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"服务器内部错误"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 删除文件
%% POST /v1/group/file/delete
%% Body: {"file_id": 123}
-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),

    % 解析请求体
    {ok, Body, Req1} = elib_req:body(Req0, []),
    FileId = maps:get(<<"file_id">>, Body, undefined),

    % 验证参数
    case FileId of
        undefined ->
            elib_response:error(Req1, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        _ ->
            % 调用 Logic 层删除文件
            case group_file_logic:delete(FileId, CurrentUid) of
                ok ->
                    elib_response:success(Req1, #{<<"deleted">> => true});
                {error, not_member} ->
                    elib_response:error(Req1, <<"非群组成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER);
                {error, permission_denied} ->
                    elib_response:error(Req1, <<"群组权限不足"/utf8>>, ?ERR_GROUP_PERMISSION_DENIED);
                {error, not_found} ->
                    elib_response:error(Req1, <<"文件不存在"/utf8>>, ?ERR_FILE_NOT_FOUND);
                {error, ?ERR_WORKSPACE_ARCHIVED} ->
                    elib_response:error(
                        Req1,
                        imboy_error:error_msg(?ERR_WORKSPACE_ARCHIVED),
                        ?ERR_WORKSPACE_ARCHIVED
                    );
                {error, _Reason} ->
                    elib_response:error(Req1, <<"文件删除失败"/utf8>>, ?ERR_FILE_DELETE_FAILED)
            end
    end.

%% @doc 搜索文件
%% GET /v1/group/file/search?gid=xxx&keyword=xxx&page=1&size=20
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs, <<>>),
    Keyword = proplists:get_value(<<"keyword">>, Qs, <<>>),
    PageStr = proplists:get_value(<<"page">>, Qs, <<"1">>),
    SizeStr = proplists:get_value(<<"size">>, Qs, <<"20">>),

    % 验证参数
    case {Gid, Keyword} of
        {<<>>, _} ->
            elib_response:error(Req0, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        {_, <<>>} ->
            elib_response:error(Req0, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        {_, _} ->
            Page = safe_page(PageStr, 1),
            Size = safe_page(SizeStr, 20),

            % 调用 Logic 层搜索文件（内部校验群成员身份）
            case group_file_logic:search(Gid, Keyword, Page, Size, CurrentUid) of
                {ok, Files} ->
                    elib_response:success(Req0, #{<<"items">> => Files});
                {error, not_member} ->
                    elib_response:error(Req0, <<"你不是该群成员"/utf8>>, ?ERR_FORBIDDEN);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"服务器内部错误"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 获取文件分类统计
%% GET /v1/group/file/categories?gid=xxx
-spec categories(cowboy_req:req(), map()) -> cowboy_req:req().
categories(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs, <<>>),

    % 验证参数
    case Gid of
        <<>> ->
            elib_response:error(Req0, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        _ ->
            % 调用 Logic 层获取分类统计
            case group_file_logic:get_categories(Gid, CurrentUid) of
                {ok, Stats} ->
                    elib_response:success(Req0, #{<<"items">> => Stats});
                {error, not_member} ->
                    elib_response:error(Req0, <<"非群组成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"服务器内部错误"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 安全解析分页参数，非法或小于 1 时回退默认值，避免 binary_to_integer badarg 崩溃
-spec safe_page(binary(), pos_integer()) -> pos_integer().
safe_page(Bin, Default) ->
    case elib_cnv:safe_to_integer(Bin) of
        N when N >= 1 ->
            N;
        _ ->
            Default
    end.
