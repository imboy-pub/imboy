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
-export([presign_put/3, presign_put_for_key/3, presign_put_for_key/4]).
-export([presign_get_for_key/2, presign_get_for_key/3]).
-export([build_object_key/2, build_object_key/4, owner_of_key/1]).
-export([get_bucket/1, public_base_url/0, public_url_for_key/1]).
-export([delete_object/1, delete_object/2, head_object/1, head_object/2, parse_head_response/1]).
-export([max_file_size/0]).
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
    <<"image/heic">>,
    <<"image/heif">>,
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

%% @doc 用指定 ObjectKey 生成 presigned PUT URL（默认私桶，向后兼容入口）
-spec presign_put_for_key(binary(), binary(), pos_integer()) -> binary().
presign_put_for_key(ObjectKey, MimeType, ExpiresSeconds) ->
    presign_put_for_key(default_bucket(), ObjectKey, MimeType, ExpiresSeconds).

%% @doc 指定桶 + ObjectKey 生成 presigned PUT URL（多桶上传链路用）
-spec presign_put_for_key(binary(), binary(), binary(), pos_integer()) -> binary().
presign_put_for_key(Bucket, ObjectKey, MimeType, ExpiresSeconds) ->
    Endpoint = endpoint(),
    elib_s3_sign:presign_put(Endpoint, Bucket, ObjectKey, MimeType, ExpiresSeconds).

%% @doc 生成绑定 uid 的 ObjectKey（向后兼容入口，等价 scope=private）。
-spec build_object_key(integer() | binary(), binary()) -> binary().
build_object_key(Uid, FileName) ->
    build_object_key(Uid, <<"private">>, undefined, FileName).

%% @doc 生成绑定 uid + scope 的 ObjectKey。
%% 不变量：第一段恒为 u<Uid>/（兼容 owner_of_key/1 写归属判定），scope 段、<Ymd>
%% 日期目录一律放其后。各 scope 命名规范见 docs/architecture/resource-access-control.md。
%%   public  -> u<Uid>/avatar/<Ymd>/<hex>.<ext>
%%   private -> u<Uid>/file/<Ymd>/<FileId>/<SafeName>
%%   c2c     -> u<Uid>/c2c/<Ymd>/<FileId>/<SafeName>
%%   group   -> u<Uid>/g<Gid>/<Ymd>/<FileId>/<SafeName>   (Gid 来自 ScopeRef)
-spec build_object_key(integer() | binary(), binary(), binary() | undefined, binary()) -> binary().
build_object_key(Uid, Scope, ScopeRef, FileName) ->
    UidBin = to_bin(Uid),
    Ymd = ymd(),
    Seg = scope_segment(Scope, ScopeRef),
    case Scope of
        <<"public">> ->
            %% 头像等公开资源：随机名 + 原扩展名，无内层 FileId 目录
            Ext = filename:extension(filename:basename(FileName)),
            Hex = binary:encode_hex(crypto:strong_rand_bytes(8)),
            <<"u", UidBin/binary, "/", Seg/binary, "/", Ymd/binary, "/", Hex/binary, Ext/binary>>;
        _ ->
            Ts = integer_to_binary(erlang:system_time(millisecond)),
            Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
            FileId = <<?FILE_ID_PREFIX, "_", Ts/binary, "_", Rand/binary>>,
            SafeName = filename:basename(FileName),
            <<"u", UidBin/binary, "/", Seg/binary, "/", Ymd/binary, "/", FileId/binary, "/",
                SafeName/binary>>
    end.

%% @doc scope → object_key 第二段。group 用 g<Gid> 实体段（Gid 取自 ScopeRef）。
-spec scope_segment(binary(), binary() | undefined) -> binary().
scope_segment(<<"public">>, _) -> <<"avatar">>;
scope_segment(<<"private">>, _) -> <<"file">>;
scope_segment(<<"c2c">>, _) -> <<"c2c">>;
scope_segment(<<"group">>, ScopeRef) -> <<"g", (to_bin(ScopeRef))/binary>>.

%% @doc 当日 UTC 日期目录 <Ymd>（YYYYMMDD）。
-spec ymd() -> binary().
ymd() ->
    {{Y, M, D}, _} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0w~2..0w~2..0w", [Y, M, D])).

-spec to_bin(integer() | binary()) -> binary().
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) when is_binary(V) -> V.

%% @doc 从 ObjectKey 反解归属 uid（u<Uid>/... → Uid），用于 confirm 防越权
-spec owner_of_key(binary()) -> {ok, integer()} | {error, invalid_key}.
owner_of_key(<<"u", Rest/binary>>) ->
    case binary:split(Rest, <<"/">>) of
        [UidBin, _] ->
            try
                {ok, binary_to_integer(UidBin)}
            catch
                _:_ -> {error, invalid_key}
            end;
        _ ->
            {error, invalid_key}
    end;
owner_of_key(_) ->
    {error, invalid_key}.

%% @doc 生成短时下载 presigned GET URL（默认私桶，向后兼容入口）
-spec presign_get_for_key(binary(), pos_integer()) -> binary().
presign_get_for_key(ObjectKey, ExpiresSeconds) ->
    presign_get_for_key(default_bucket(), ObjectKey, ExpiresSeconds).

%% @doc 指定桶生成短时下载 presigned GET URL（受限资源读鉴权链路用）
-spec presign_get_for_key(binary(), binary(), pos_integer()) -> binary().
presign_get_for_key(Bucket, ObjectKey, ExpiresSeconds) ->
    Endpoint = endpoint(),
    elib_s3_sign:presign_get(Endpoint, Bucket, ObjectKey, ExpiresSeconds).

%% @doc 单文件最大尺寸（字节），供 confirm 阶段服务端核实真实大小复用
-spec max_file_size() -> pos_integer().
max_file_size() ->
    ?MAX_FILE_SIZE.

%% @doc 向 Garage 发 HEAD 请求核实对象真实存在性、大小与类型。
%% 用于 confirm 阶段不信任客户端自报 size/mime_type 的服务端校验。
-spec head_object(binary()) -> {ok, map()} | {error, not_found | term()}.
head_object(ObjectKey) ->
    head_object(default_bucket(), ObjectKey).

%% @doc 指定桶向 Garage 发 HEAD 核实对象（多桶 confirm 链路用）
-spec head_object(binary(), binary()) -> {ok, map()} | {error, not_found | term()}.
head_object(Bucket, ObjectKey) ->
    ok = assert_garage_configured(),
    Endpoint = endpoint(),
    %% 用 presigned GET（query 签名，仅签 host）+ Range: bytes=0-0 核实对象。
    %% 不走 header 鉴权 HEAD：经 nginx 反代后 HEAD 在 Garage 端按 GET 校验，
    %% 方法行与签名不符 → "Invalid signature" 403（query 预签名链路同 PUT 已验证可用）。
    Url = elib_s3_sign:presign_get(Endpoint, Bucket, ObjectKey, 60),
    Headers = [{"range", "bytes=0-0"}],
    parse_head_response(
        httpc:request(get, {binary_to_list(Url), Headers}, [{timeout, 10000}], [])
    ).

%% @doc 解析 HEAD 响应为 {ok, #{size, content_type}}（纯函数，便于单测）
-spec parse_head_response(term()) -> {ok, map()} | {error, not_found | term()}.
parse_head_response({ok, {{_, Code, _}, Headers, _}}) when Code =:= 200; Code =:= 206 ->
    {ok, #{
        size => head_object_size(Headers),
        content_type => head_bin(Headers, "content-type", <<"application/octet-stream">>)
    }};
parse_head_response({ok, {{_, 404, _}, _, _}}) ->
    {error, not_found};
parse_head_response({ok, {{_, Code, _}, _, _}}) ->
    {error, {http_error, Code}};
parse_head_response({error, Reason}) ->
    {error, Reason}.

%% HTTP 响应头大小写不敏感查找（httpc 通常返回小写键，但兼容混合大小写）
-spec head_lookup([{string(), string()}], string()) -> string() | undefined.
head_lookup(Headers, Key) ->
    LKey = string:lowercase(Key),
    case lists:dropwhile(fun({K, _}) -> string:lowercase(K) =/= LKey end, Headers) of
        [{_, V} | _] -> V;
        [] -> undefined
    end.

-spec head_int([{string(), string()}], string(), integer()) -> integer().
head_int(Headers, Key, Default) ->
    case head_lookup(Headers, Key) of
        undefined ->
            Default;
        V ->
            try
                list_to_integer(string:trim(V))
            catch
                _:_ -> Default
            end
    end.

%% @doc 对象真实大小：优先取 content-range 的总长（Range 请求返回 206 时），
%% 回退 content-length（完整 200 响应时）。
-spec head_object_size([{string(), string()}]) -> integer().
head_object_size(Headers) ->
    Fallback = head_int(Headers, "content-length", 0),
    case head_lookup(Headers, "content-range") of
        undefined ->
            Fallback;
        Range ->
            %% 形如 "bytes 0-0/22" → 取 "/" 后总长；"bytes 0-0/*" 则回退
            case string:split(Range, "/") of
                [_, Total] ->
                    try
                        list_to_integer(string:trim(Total))
                    catch
                        _:_ -> Fallback
                    end;
                _ ->
                    Fallback
            end
    end.

-spec head_bin([{string(), string()}], string(), binary()) -> binary().
head_bin(Headers, Key, Default) ->
    case head_lookup(Headers, Key) of
        undefined -> Default;
        V -> iolist_to_binary(V)
    end.

%% @doc 物理删除存储桶中的对象
-spec delete_object(binary()) -> ok | {error, term()}.
delete_object(ObjectKey) ->
    delete_object(default_bucket(), ObjectKey).

%% @doc 指定桶物理删除存储对象（多桶孤儿清理/校验失败回滚用）
-spec delete_object(binary(), binary()) -> ok | {error, term()}.
delete_object(Bucket, ObjectKey) ->
    ok = assert_garage_configured(),
    Cfg = garage_config(),
    Endpoint = maps:get(endpoint, Cfg, <<"http://127.0.0.1:3900">>),
    AccessKey = maps:get(access_key, Cfg, <<>>),
    SecretKey = maps:get(secret_key, Cfg, <<>>),

    %% 请求 URL 的对象路径必须与签名 Canonical URI 编码方式一致
    Url =
        <<Endpoint/binary, "/", Bucket/binary, "/",
            (elib_s3_sign:uri_encode_path(ObjectKey))/binary>>,
    Now = calendar:universal_time(),
    AmzDate = elib_s3_sign:format_amz_date(Now),
    AuthHeader = elib_s3_sign:authorization_header(
        <<"DELETE">>, Bucket, ObjectKey, <<>>, AmzDate, AccessKey, SecretKey
    ),

    Headers = [
        {"x-amz-date", binary_to_list(AmzDate)},
        {"x-amz-content-sha256", "UNSIGNED-PAYLOAD"},
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
    {ok, public_url(Endpoint, Bucket, ObjectKey)}.

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
    %% 请求 URL 的对象路径必须与签名 Canonical URI 编码方式一致
    Url =
        <<Endpoint/binary, "/", Bucket/binary, "/",
            (elib_s3_sign:uri_encode_path(ObjectKey))/binary>>,

    Now = calendar:universal_time(),
    AmzDate = elib_s3_sign:format_amz_date(Now),
    AuthHeader = elib_s3_sign:authorization_header(
        <<"PUT">>, Bucket, ObjectKey, MimeType, AmzDate, AccessKey, SecretKey
    ),

    Headers = [
        {"content-type", binary_to_list(MimeType)},
        {"x-amz-date", binary_to_list(AmzDate)},
        {"x-amz-content-sha256", "UNSIGNED-PAYLOAD"},
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

%% @doc Garage S3 端点（私桶/公桶共用同一 endpoint，仅 bucket 不同）
-spec endpoint() -> binary().
endpoint() ->
    maps:get(endpoint, garage_config(), <<"http://127.0.0.1:3900">>).

%% @doc 默认（私有）bucket
-spec default_bucket() -> binary().
default_bucket() ->
    maps:get(bucket, garage_config(), <<"imboy">>).

%% @doc 按 scope 选桶：public → 公开读桶，其余 → 私有桶。
%% bucket 完全由 scope 派生，不落库（见设计文档第 6 节）。
-spec get_bucket(binary()) -> binary().
get_bucket(<<"public">>) ->
    maps:get(public_bucket, garage_config(), <<"imboy-public">>);
get_bucket(_Scope) ->
    default_bucket().

%% @doc 公开资源访问基址（如 https://s3.imboy.pub），客户端直读/可 CDN
-spec public_base_url() -> binary().
public_base_url() ->
    maps:get(public_base_url, garage_config(), <<"https://s3.imboy.pub">>).

%% @doc 由 object_key 拼接公开资源完整 URL（纯字符串，零 DB/网络）
-spec public_url_for_key(binary()) -> binary().
public_url_for_key(ObjectKey) ->
    <<(public_base_url())/binary, "/", ObjectKey/binary>>.

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
