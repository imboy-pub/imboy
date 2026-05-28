-module(elib_oss).
%%%
% OSS (对象存储服务) 客户端 — Garage S3 兼容后端
%
% 上传流程：
%   服务端上传：upload/2,3 → upload_to_storage/4 → httpc PUT → Garage
%   Flutter 直传：presign_put/3 → 返回 presigned PUT URL → Flutter 直接 PUT Garage
%
% Garage 使用 path-style URL：endpoint/bucket/key
%%%

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-export([upload/2, upload/3]).
-export([get_url/1]).
-export([presign_put/3, presign_put_for_key/3]).
-export([delete_object/1]).
-export([generate_file_id/0]).
-export([validate_file_id/1]).
-export([get_file_category/1]).
-export([validate_file_type/1]).

-define(MAX_FILE_SIZE, 100 * 1024 * 1024).
-define(FILE_ID_PREFIX, "file").

-define(ALLOWED_TYPES, [
    <<"application/pdf">>,
    <<"application/msword">>,
    <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>,
    <<"application/vnd.ms-excel">>,
    <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>,
    <<"application/vnd.ms-powerpoint">>,
    <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>,
    <<"text/plain">>,
    <<"text/csv">>,
    <<"image/jpeg">>,
    <<"image/png">>,
    <<"image/gif">>,
    <<"image/bmp">>,
    <<"image/webp">>,
    <<"video/mp4">>,
    <<"video/avi">>,
    <<"video/quicktime">>,
    <<"video/x-msvideo">>,
    <<"video/x-ms-wmv">>,
    <<"video/x-flv">>,
    <<"audio/mpeg">>,
    <<"audio/mp3">>,
    <<"audio/wav">>,
    <<"audio/flac">>,
    <<"audio/aac">>
]).

%% ===================================================================
%% API 函数
%% ===================================================================

-spec upload(binary(), binary()) -> {ok, binary(), binary()} | {error, term()}.
upload(FileBinary, FileName) ->
    upload(FileBinary, FileName, #{}).

-spec upload(binary(), binary(), map()) -> {ok, binary(), binary()} | {error, term()}.
upload(FileBinary, FileName, Options) ->
    FileSize = byte_size(FileBinary),
    case FileSize > ?MAX_FILE_SIZE of
        true ->
            {error, file_too_large};
        false ->
            MimeType = maps:get(mime_type, Options, guess_mime_type(FileName)),
            case validate_file_type(MimeType) of
                false ->
                    {error, invalid_file_type};
                true ->
                    FileId = generate_file_id(),
                    case upload_to_storage(FileId, FileName, FileBinary, MimeType) of
                        {ok, FileUrl} -> {ok, FileUrl, FileId};
                        {error, Reason} -> {error, Reason}
                    end
            end
    end.

%% @doc 生成 presigned PUT URL（Flutter 直传，不经 Erlang 代理）
-spec presign_put(binary(), binary(), pos_integer()) -> binary().
presign_put(FileName, MimeType, ExpiresSeconds) ->
    Cfg = garage_config(),
    Endpoint = maps:get(endpoint, Cfg, <<"http://127.0.0.1:3900">>),
    Bucket = maps:get(bucket, Cfg, <<"imboy">>),
    FileId = generate_file_id(),
    SafeName = filename:basename(FileName),
    ObjectKey = <<FileId/binary, "/", SafeName/binary>>,
    elib_s3_sign:presign_put(Endpoint, Bucket, ObjectKey, MimeType, ExpiresSeconds).

%% @doc 用指定 ObjectKey 生成 presigned PUT URL（attach_handler 用，避免双重生成 FileId）
-spec presign_put_for_key(binary(), binary(), pos_integer()) -> binary().
presign_put_for_key(ObjectKey, MimeType, ExpiresSeconds) ->
    Cfg = garage_config(),
    Endpoint = maps:get(endpoint, Cfg, <<"http://127.0.0.1:3900">>),
    Bucket = maps:get(bucket, Cfg, <<"imboy">>),
    elib_s3_sign:presign_put(Endpoint, Bucket, ObjectKey, MimeType, ExpiresSeconds).

%% @doc 物理删除存储桶中的对象
-spec delete_object(binary()) -> ok | {error, term()}.
delete_object(ObjectKey) ->
    ok = assert_garage_configured(),
    Cfg = garage_config(),
    Endpoint = maps:get(endpoint, Cfg, <<"http://127.0.0.1:3900">>),
    Bucket = maps:get(bucket, Cfg, <<"imboy">>),
    AccessKey = maps:get(access_key, Cfg, <<>>),
    SecretKey = maps:get(secret_key, Cfg, <<>>),

    Url = <<Endpoint/binary, "/", Bucket/binary, "/", ObjectKey/binary>>,
    Now = calendar:universal_time(),
    AmzDate = elib_s3_sign:format_amz_date(Now),
    AuthHeader = elib_s3_sign:authorization_header(
        <<"DELETE">>, Bucket, ObjectKey, <<>>, AmzDate, AccessKey, SecretKey
    ),

    Headers = [
        {"x-amz-date", binary_to_list(AmzDate)},
        {"authorization", binary_to_list(AuthHeader)}
    ],
    case httpc:request(delete, {binary_to_list(Url), Headers}, [{timeout, 10000}], []) of
        {ok, {{_, Code, _}, _, _}} when Code =:= 204; Code =:= 200 ->
            ok;
        {ok, {{_, Code, _}, _, Body}} ->
            ?ERROR_LOG(["elib_oss delete_object failed: code=", Code, " body=", Body]),
            {error, {http_error, Code, Body}};
        {error, Reason} ->
            ?ERROR_LOG(["elib_oss delete_object httpc error: ", Reason]),
            {error, Reason}
    end.

%% @doc 获取文件公开 URL（Bucket 公开读时直接访问）
-spec get_url(binary()) -> {ok, binary()}.
get_url(ObjectKey) ->
    Cfg = garage_config(),
    Endpoint = maps:get(endpoint, Cfg, <<"http://127.0.0.1:3900">>),
    Bucket = maps:get(bucket, Cfg, <<"imboy">>),
    {ok, <<Endpoint/binary, "/", Bucket/binary, "/", ObjectKey/binary>>}.

-spec generate_file_id() -> binary().
generate_file_id() ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<?FILE_ID_PREFIX, "_", Timestamp/binary, "_", Random/binary>>.

-spec validate_file_id(binary()) -> ok | {error, binary()}.
validate_file_id(FileId) ->
    case re:run(FileId, <<"^file_[0-9]+_[0-9]+$">>, [{capture, none}]) of
        match -> ok;
        nomatch -> {error, <<"invalid_file_id">>}
    end.

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

-spec validate_file_type(binary()) -> boolean().
validate_file_type(MimeType) ->
    lists:member(MimeType, ?ALLOWED_TYPES).

%% ===================================================================
%% 内部函数
%% ===================================================================

-spec upload_to_storage(binary(), binary(), binary(), binary()) -> {ok, binary()} | {error, term()}.
upload_to_storage(FileId, FileName, FileBinary, MimeType) ->
    ok = assert_garage_configured(),
    Cfg = garage_config(),
    Endpoint = maps:get(endpoint, Cfg, <<"http://127.0.0.1:3900">>),
    Bucket = maps:get(bucket, Cfg, <<"imboy">>),
    AccessKey = maps:get(access_key, Cfg, <<>>),
    SecretKey = maps:get(secret_key, Cfg, <<>>),

    SafeName = filename:basename(FileName),
    ObjectKey = <<FileId/binary, "/", SafeName/binary>>,
    Url = <<Endpoint/binary, "/", Bucket/binary, "/", ObjectKey/binary>>,

    Now = calendar:universal_time(),
    AmzDate = elib_s3_sign:format_amz_date(Now),
    AuthHeader = elib_s3_sign:authorization_header(
        <<"PUT">>, Bucket, ObjectKey, MimeType, AmzDate, AccessKey, SecretKey
    ),

    Headers = [
        {"content-type", binary_to_list(MimeType)},
        {"x-amz-date", binary_to_list(AmzDate)},
        {"authorization", binary_to_list(AuthHeader)}
    ],
    case
        httpc:request(
            put,
            {binary_to_list(Url), Headers, binary_to_list(MimeType), FileBinary},
            [{timeout, 30000}],
            []
        )
    of
        {ok, {{_, Code, _}, _, _}} when Code =:= 200; Code =:= 201; Code =:= 204 ->
            {ok, public_url(Endpoint, Bucket, ObjectKey)};
        {ok, {{_, Code, _}, _, Body}} ->
            ?ERROR_LOG(["elib_oss upload_to_storage failed: code=", Code, " body=", Body]),
            {error, {http_error, Code}};
        {error, Reason} ->
            ?ERROR_LOG(["elib_oss upload_to_storage httpc error: ", Reason]),
            {error, Reason}
    end.

-spec public_url(binary(), binary(), binary()) -> binary().
public_url(Endpoint, Bucket, ObjectKey) ->
    <<Endpoint/binary, "/", Bucket/binary, "/", ObjectKey/binary>>.

-spec garage_config() -> map().
garage_config() ->
    application:get_env(imboy, garage, #{}).

%% @doc 校验 Garage 配置完整性，access_key/secret_key 不得为空
-spec assert_garage_configured() -> ok.
assert_garage_configured() ->
    Cfg = garage_config(),
    case {maps:get(access_key, Cfg, <<>>), maps:get(secret_key, Cfg, <<>>)} of
        {<<>>, _} -> erlang:error({garage_not_configured, access_key_missing});
        {_, <<>>} -> erlang:error({garage_not_configured, secret_key_missing});
        _ -> ok
    end.

-spec guess_mime_type(binary()) -> binary().
guess_mime_type(FileName) ->
    case filename:extension(FileName) of
        <<".pdf">> ->
            <<"application/pdf">>;
        <<".doc">> ->
            <<"application/msword">>;
        <<".docx">> ->
            <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>;
        <<".xls">> ->
            <<"application/vnd.ms-excel">>;
        <<".xlsx">> ->
            <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>;
        <<".ppt">> ->
            <<"application/vnd.ms-powerpoint">>;
        <<".pptx">> ->
            <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>;
        <<".txt">> ->
            <<"text/plain">>;
        <<".csv">> ->
            <<"text/csv">>;
        <<".jpg">> ->
            <<"image/jpeg">>;
        <<".jpeg">> ->
            <<"image/jpeg">>;
        <<".png">> ->
            <<"image/png">>;
        <<".gif">> ->
            <<"image/gif">>;
        <<".bmp">> ->
            <<"image/bmp">>;
        <<".webp">> ->
            <<"image/webp">>;
        <<".mp4">> ->
            <<"video/mp4">>;
        <<".avi">> ->
            <<"video/x-msvideo">>;
        <<".mov">> ->
            <<"video/quicktime">>;
        <<".wmv">> ->
            <<"video/x-ms-wmv">>;
        <<".flv">> ->
            <<"video/x-flv">>;
        <<".mp3">> ->
            <<"audio/mpeg">>;
        <<".wav">> ->
            <<"audio/wav">>;
        <<".flac">> ->
            <<"audio/flac">>;
        <<".aac">> ->
            <<"audio/aac">>;
        _ ->
            <<"application/octet-stream">>
    end.
