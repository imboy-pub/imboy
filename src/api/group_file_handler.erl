-module(group_file_handler).

-dialyzer({nowarn_function, [handle_action/3, parse_multipart/1]}).

-behavior(cowboy_rest).

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
handle_action(upload, Req, State) -> upload(Req, State);
handle_action(download, Req, State) -> download(Req, State);
handle_action(list, Req, State) -> list(Req, State);
handle_action(delete, Req, State) -> delete(Req, State);
handle_action(search, Req, State) -> search(Req, State);
handle_action(categories, Req, State) -> categories(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Action 处理函数
%% ===================================================================

%% @doc 上传文件
%% POST /v1/group/file/upload
%% Content-Type: multipart/form-data
%% 参数: gid, file
-spec upload(cowboy_req:req(), map()) -> cowboy_req:req().
upload(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),

    % 解析 multipart 表单数据
    {ok, Parts, Req1} = cowboy_req:read_part(Req0),

    % 提取参数
    {Gid, FileName, FileBinary, FileType} = parse_multipart(Parts),

    % 验证参数
    case {Gid, FileName, FileBinary} of
        {undefined, _, _} ->
            elib_response:error(Req1, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        {_, undefined, _} ->
            elib_response:error(Req1, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        {_, _, undefined} ->
            elib_response:error(Req1, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM);
        {_, _, _} ->
            % 调用 Logic 层上传文件
            case group_file_logic:upload(Gid, CurrentUid, FileName, FileBinary, FileType) of
                {ok, FileData} ->
                    elib_response:success(Req1, FileData);
                {error, not_member} ->
                    elib_response:error(Req1, <<"非群组成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER);
                {error, file_too_large} ->
                    elib_response:error(Req1, <<"文件大小超出限制"/utf8>>, ?ERR_FILE_SIZE_EXCEEDED);
                {error, invalid_file_type} ->
                    elib_response:error(Req1, <<"不允许的文件类型"/utf8>>, ?ERR_FILE_TYPE_NOT_ALLOWED);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"文件上传失败"/utf8>>, ?ERR_FILE_UPLOAD_FAILED)
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
            FileId = binary_to_integer(FileIdStr),
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
            Page = binary_to_integer(PageStr),
            Size = binary_to_integer(SizeStr),
            Options = case Category of
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
                {error, _Reason} ->
                    elib_response:error(Req1, <<"文件删除失败"/utf8>>, ?ERR_FILE_DELETE_FAILED)
            end
    end.

%% @doc 搜索文件
%% GET /v1/group/file/search?gid=xxx&keyword=xxx&page=1&size=20
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, _State) ->
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
            Page = binary_to_integer(PageStr),
            Size = binary_to_integer(SizeStr),

            % 调用 Logic 层搜索文件
            case group_file_logic:search(Gid, Keyword, Page, Size) of
                {ok, Files} ->
                    elib_response:success(Req0, #{<<"items">> => Files});
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

%% @doc 解析 multipart 表单数据（兼容测试桩和简化结构）
%% @param Parts cowboy_req 读到的部分
%% @return {Gid, FileName, FileBinary, FileType}
-spec parse_multipart(list()) -> {term(), term(), term(), term()}.
parse_multipart(Parts) ->
    parse_multipart(Parts, undefined, undefined, undefined, undefined).

parse_multipart([], Gid, FileName, FileBinary, FileType) ->
    FinalType = case FileType of
        undefined -> <<"application/octet-stream">>;
        <<>> -> <<"application/octet-stream">>;
        _ -> FileType
    end,
    {Gid, FileName, FileBinary, FinalType};
parse_multipart([Part | Rest], Gid, FileName, FileBinary, FileType) ->
    case Part of
        {<<"gid">>, Value} ->
            parse_multipart(Rest, Value, FileName, FileBinary, FileType);
        {<<"file_name">>, Value} ->
            parse_multipart(Rest, Gid, Value, FileBinary, FileType);
        {<<"file_type">>, Value} ->
            parse_multipart(Rest, Gid, FileName, FileBinary, Value);
        {<<"file">>, Value} ->
            {FileName2, FileBinary2, FileType2} = normalize_file_part(Value, FileName, FileType),
            parse_multipart(Rest, Gid, FileName2, FileBinary2, FileType2);
        _ ->
            parse_multipart(Rest, Gid, FileName, FileBinary, FileType)
    end.

normalize_file_part(Value, FileName, FileType) when is_binary(Value) ->
    {FileName, Value, FileType};
normalize_file_part({Name, Binary}, FileName, FileType) when is_binary(Binary) ->
    {choose_binary(Name, FileName), Binary, FileType};
normalize_file_part({Name, Binary, Type}, FileName, FileType) when is_binary(Binary) ->
    {choose_binary(Name, FileName), Binary, choose_binary(Type, FileType)};
normalize_file_part(Value, FileName, FileType) when is_map(Value) ->
    Name = maps:get(filename, Value, maps:get(<<"filename">>, Value, FileName)),
    Binary = maps:get(data, Value, maps:get(<<"data">>, Value, undefined)),
    Type = maps:get(content_type, Value, maps:get(<<"content_type">>, Value, FileType)),
    {Name, Binary, Type};
normalize_file_part(_Value, FileName, FileType) ->
    {FileName, undefined, FileType}.

choose_binary(Value, _Default) when is_binary(Value), Value =/= <<>> ->
    Value;
choose_binary(_Value, Default) ->
    Default.
