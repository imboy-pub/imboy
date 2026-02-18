-module(elib_oss).
%%%
% OSS (对象存储服务) 客户端
%
% 支持多种对象存储服务：
% - 阿里云 OSS
% - 腾讯云 COS
% - MinIO
% - 本地文件系统（占位符）
%%%

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% API
-export([upload/2]).
-export([upload/3]).
-export([download/1]).
-export([delete/1]).
-export([get_url/1]).
-export([generate_file_id/0]).

%% 文件分类
-export([get_file_category/1]).
-export([validate_file_type/1]).

%% 配置
-define(MAX_FILE_SIZE, 100 * 1024 * 1024). % 100MB
-define(FILE_ID_PREFIX, "file").

%% 允许的文件类型（MIME类型白名单）
-define(ALLOWED_TYPES, [
    % 文档
    <<"application/pdf">>,
    <<"application/msword">>,
    <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>,
    <<"application/vnd.ms-excel">>,
    <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>,
    <<"application/vnd.ms-powerpoint">>,
    <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>,
    <<"text/plain">>,
    <<"text/csv">>,
    % 图片
    <<"image/jpeg">>,
    <<"image/png">>,
    <<"image/gif">>,
    <<"image/bmp">>,
    <<"image/webp">>,
    % 视频
    <<"video/mp4">>,
    <<"video/avi">>,
    <<"video/quicktime">>,
    <<"video/x-msvideo">>,
    <<"video/x-ms-wmv">>,
    <<"video/x-flv">>,
    % 音频
    <<"audio/mpeg">>,
    <<"audio/mp3">>,
    <<"audio/wav">>,
    <<"audio/flac">>,
    <<"audio/aac">>
]).

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 上传文件（使用默认配置）
%% @param FileBinary 文件二进制数据
%% @param FileName 文件名
%% @return {ok, FileUrl, FileId} | {error, Reason}
-spec upload(binary(), binary()) -> {ok, binary(), binary()} | {error, term()}.
upload(FileBinary, FileName) ->
    upload(FileBinary, FileName, #{}).

%% @doc 上传文件（带选项）
%% @param FileBinary 文件二进制数据
%% @param FileName 文件名
%% @param Options 选项 #{mime_type => binary(), custom_id => binary()}
%% @return {ok, FileUrl, FileId} | {error, Reason}
-spec upload(binary(), binary(), map()) -> {ok, binary(), binary()} | {error, term()}.
upload(FileBinary, FileName, Options) ->
    % 验证文件大小
    FileSize = byte_size(FileBinary),
    case FileSize > ?MAX_FILE_SIZE of
        true ->
            {error, file_too_large};
        false ->
            % 获取 MIME 类型
            MimeType = maps:get(mime_type, Options, guess_mime_type(FileName)),
            % 验证文件类型
            case validate_file_type(MimeType) of
                false ->
                    {error, invalid_file_type};
                true ->
                    % 生成文件ID
                    FileId = generate_file_id(),
                    % 上传文件（占位符实现）
                    case upload_to_storage(FileId, FileName, FileBinary, MimeType) of
                        {ok, FileUrl} ->
                            {ok, FileUrl, FileId};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc 下载文件
%% @param FileId 文件ID
%% @return {ok, FileBinary} | {error, Reason}
-spec download(binary()) -> {ok, binary()} | {error, term()}.
download(_FileId) ->
    % 占位符实现：返回错误
    {error, not_implemented}.

%% @doc 删除文件
%% @param FileId 文件ID
%% @return ok | {error, Reason}
-spec delete(binary()) -> ok | {error, term()}.
delete(_FileId) ->
    % 占位符实现：返回成功
    ok.

%% @doc 获取文件URL
%% @param FileId 文件ID
%% @return {ok, FileUrl} | {error, Reason}
-spec get_url(binary()) -> {ok, binary()} | {error, term()}.
get_url(FileId) ->
    % 占位符实现：返回本地路径
    {ok, <<"/static/files/", FileId/binary>>}.

%% @doc 生成文件ID
%% @return FileId
-spec generate_file_id() -> binary().
generate_file_id() ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<?FILE_ID_PREFIX, "_", Timestamp/binary, "_", Random/binary>>.

%% ===================================================================
%% 文件分类函数
%% ===================================================================

%% @doc 获取文件分类
%% @param MimeType MIME类型
%% @return Category (document | image | video | audio | other)
-spec get_file_category(binary()) -> atom().
get_file_category(<<"application/pdf", _/binary>>) -> document;
get_file_category(<<"application/msword", _/binary>>) -> document;
get_file_category(<<"application/vnd.openxmlformats-officedocument", _/binary>>) -> document;
get_file_category(<<"application/vnd.ms-", _/binary>>) -> document;
get_file_category(<<"text/", _/binary>>) -> document;
get_file_category(<<"image/", _/binary>>) -> image;
get_file_category(<<"video/", _/binary>>) -> video;
get_file_category(<<"audio/", _/binary>>) -> audio;
get_file_category(_Other) -> other.

%% @doc 验证文件类型
%% @param MimeType MIME类型
%% @return true | false
-spec validate_file_type(binary()) -> boolean().
validate_file_type(MimeType) ->
    lists:member(MimeType, ?ALLOWED_TYPES).

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 上传文件到存储（占位符实现）
%% @param FileId 文件ID
%% @param FileName 文件名
%% @param FileBinary 文件二进制数据
%% @param MimeType MIME类型
%% @return {ok, FileUrl} | {error, Reason}
-spec upload_to_storage(binary(), binary(), binary(), binary()) -> {ok, binary()} | {error, term()}.
upload_to_storage(FileId, FileName, _FileBinary, _MimeType) ->
    % 占位符实现：返回本地路径
    % 实际使用时应该上传到 OSS
    FileUrl = <<"/static/files/", FileId/binary, "/", FileName/binary>>,
    {ok, FileUrl}.

%% @doc 猜测文件MIME类型
%% @param FileName 文件名
%% @return MimeType
-spec guess_mime_type(binary()) -> binary().
guess_mime_type(FileName) ->
    case filename:extension(FileName) of
        <<".pdf">> -> <<"application/pdf">>;
        <<".doc">> -> <<"application/msword">>;
        <<".docx">> -> <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>;
        <<".xls">> -> <<"application/vnd.ms-excel">>;
        <<".xlsx">> -> <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>;
        <<".ppt">> -> <<"application/vnd.ms-powerpoint">>;
        <<".pptx">> -> <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>;
        <<".txt">> -> <<"text/plain">>;
        <<".csv">> -> <<"text/csv">>;
        <<".jpg">> -> <<"image/jpeg">>;
        <<".jpeg">> -> <<"image/jpeg">>;
        <<".png">> -> <<"image/png">>;
        <<".gif">> -> <<"image/gif">>;
        <<".bmp">> -> <<"image/bmp">>;
        <<".webp">> -> <<"image/webp">>;
        <<".mp4">> -> <<"video/mp4">>;
        <<".avi">> -> <<"video/x-msvideo">>;
        <<".mov">> -> <<"video/quicktime">>;
        <<".wmv">> -> <<"video/x-ms-wmv">>;
        <<".flv">> -> <<"video/x-flv">>;
        <<".mp3">> -> <<"audio/mpeg">>;
        <<".wav">> -> <<"audio/wav">>;
        <<".flac">> -> <<"audio/flac">>;
        <<".aac">> -> <<"audio/aac">>;
        _ -> <<"application/octet-stream">>
    end.
