-module(attach_handler).

%%%
% 附件 presigned URL 生成接口
% 供 Flutter 客户端获取 Garage S3 直传 URL
%
% GET /v1/attachment/presign?filename=x.jpg&mime_type=image/jpeg&expires=3600
% 响应: { put_url, object_key, expires_at }
%   附加 &method=multipart 时响应多一个 upload_url（指向本端点、query 带
%   object_key/mime_type）：客户端以 multipart/form-data（文件字段名 file）
%   POST 上传，之后照旧 confirm 落库。大文件流式、带原生上传进度回调，
%   适合 wx.uploadFile 等无法自设 PUT Content-Type 的环境。
% POST /v1/attachment/upload?object_key=...&mime_type=...  multipart/form-data
%   流式收文件 → 临时文件 → 转入 Garage；不做任何落库（confirm 是唯一真源）
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").
-include("common.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            presign -> presign(Method, Req0, State);
            confirm -> confirm(Method, Req0, State);
            view_url -> view_url(Method, Req0, State);
            upload -> upload(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% @doc GET /v1/attachment/presign?filename=x.jpg&mime_type=image/jpeg
%% 生成绑定当前 uid 的上传 presigned PUT URL
-spec presign(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
presign(<<"GET">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    FileName = proplists:get_value(<<"filename">>, Qs, <<"file">>),
    MimeType = proplists:get_value(<<"mime_type">>, Qs, <<"application/octet-stream">>),
    Scope = proplists:get_value(<<"scope">>, Qs, <<"private">>),
    ScopeRef = proplists:get_value(<<"scope_ref">>, Qs, undefined),
    case attach_logic:presign(Uid, FileName, MimeType, Scope, ScopeRef) of
        {ok, Data0} ->
            %% &method=multipart → 附带 upload_url（put_url 保留，向后兼容）
            Data = maybe_upload_url(
                proplists:get_value(<<"method">>, Qs, <<"put">>), Req0, MimeType, Data0
            ),
            elib_response:success(Req0, Data, "success.");
        {error, invalid_file_type} ->
            elib_response:error(Req0, <<"不支持的文件类型"/utf8>>, ?ERR_BAD_REQUEST);
        {error, upload_not_supported} ->
            elib_response:error(Req0, <<"该资源类型暂不支持上传"/utf8>>, ?ERR_BAD_REQUEST);
        {error, forbidden} ->
            elib_response:error(Req0, <<"无权向该范围上传"/utf8>>, ?ERR_FORBIDDEN)
    end;
presign(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc multipart 上传 URL：以当前请求 Host 派生绝对地址（同源部署形态），
%% query 携带 object_key 与 presign 时声明的 mime_type（mime 以 presign 为准，
%% 不信任上传时客户端给的 Content-Type）。
-spec maybe_upload_url(binary(), cowboy_req:req(), binary(), map()) -> map().
maybe_upload_url(<<"multipart">>, Req0, MimeType, #{<<"object_key">> := ObjectKey} = Data) ->
    Qs =
        <<"object_key=", (cow_qs:urlencode(ObjectKey))/binary, "&mime_type=",
            (cow_qs:urlencode(MimeType))/binary>>,
    %% cowboy_req:uri/2 返回 uri_string 解析结构而非二进制，直接手拼绝对地址。
    %% scheme/1 返回的是 binary（原子匹配恒落 http）；反代形态真源是
    %% x-forwarded-proto，有该头时优先（否则 TLS 部署会签发出 http:// 明文地址）。
    Scheme =
        case cowboy_req:header(<<"x-forwarded-proto">>, Req0) of
            <<"https">> ->
                <<"https">>;
            _ ->
                case cowboy_req:scheme(Req0) of
                    <<"https">> -> <<"https">>;
                    _ -> <<"http">>
                end
        end,
    Host = cowboy_req:host(Req0),
    HostPart =
        case cowboy_req:port(Req0) of
            P when
                (P =:= 80 andalso Scheme =:= <<"http">>) orelse
                    (P =:= 443 andalso Scheme =:= <<"https">>)
            ->
                Host;
            P ->
                <<Host/binary, ":", (integer_to_binary(P))/binary>>
        end,
    UploadUrl = <<Scheme/binary, "://", HostPart/binary, "/api/v1/attachment/upload?", Qs/binary>>,
    Data#{<<"upload_url">> => UploadUrl};
maybe_upload_url(_Method, _Req0, _MimeType, Data) ->
    Data.

%% @doc POST /v1/attachment/confirm
%% body: { object_key, file_hash256, mime_type, size }（旧客户端 md5 双读兼容）
%% 客户端 PUT 直传成功后回调，落库附件元数据
-spec confirm(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
confirm(<<"POST">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    ObjectKey = maps:get(<<"object_key">>, PostVals, <<>>),
    Scope = maps:get(<<"scope">>, PostVals, <<"private">>),
    ScopeRef = maps:get(<<"scope_ref">>, PostVals, undefined),
    case attach_logic:confirm(Uid, ObjectKey, Scope, ScopeRef, PostVals) of
        {ok, Data} ->
            elib_response:success(Req0, Data, "success.");
        {error, forbidden_key} ->
            elib_response:error(Req0, <<"非法对象归属"/utf8>>, ?ERR_BAD_REQUEST);
        {error, forbidden} ->
            elib_response:error(Req0, <<"无权向该范围上传"/utf8>>, ?ERR_FORBIDDEN);
        {error, upload_not_supported} ->
            elib_response:error(Req0, <<"该资源类型暂不支持上传"/utf8>>, ?ERR_BAD_REQUEST);
        {error, invalid_key} ->
            elib_response:error(Req0, <<"非法对象键"/utf8>>, ?ERR_BAD_REQUEST);
        {error, object_not_found} ->
            elib_response:error(Req0, <<"对象不存在或未完成上传"/utf8>>, ?ERR_BAD_REQUEST);
        {error, file_too_large} ->
            elib_response:error(Req0, <<"文件超过大小限制"/utf8>>, ?ERR_BAD_REQUEST);
        {error, invalid_file_type} ->
            elib_response:error(Req0, <<"不支持的文件类型"/utf8>>, ?ERR_BAD_REQUEST);
        {error, unsupported_cipher} ->
            %% fail-closed：宁可拒绝 confirm，也不把密文对象落成「明文」
            elib_response:error(Req0, <<"不支持的附件加密套件"/utf8>>, ?ERR_BAD_REQUEST);
        {error, attachment_anchor_required} ->
            elib_response:error(Req0, <<"群附件缺少消息锚点"/utf8>>, ?ERR_BAD_REQUEST);
        {error, _Reason} ->
            elib_response:error(Req0, <<"附件落库失败"/utf8>>, ?ERR_BAD_REQUEST)
    end;
confirm(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc GET /v1/attachment/view_url?object_key=xxx
%% 签发短时下载 presigned GET URL（替代 bucket 公开读）
-spec view_url(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
view_url(<<"GET">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    case proplists:get_value(<<"object_key">>, Qs, <<>>) of
        <<>> ->
            elib_response:error(Req0, <<"缺少 object_key"/utf8>>, ?ERR_BAD_REQUEST);
        ObjectKey ->
            case attach_logic:view_url(Uid, ObjectKey) of
                {ok, Url} ->
                    elib_response:success(Req0, #{<<"url">> => Url}, "success.");
                {error, forbidden} ->
                    %% fail-closed：非归属或归属校验失败，拒绝签发下载 URL
                    elib_response:error(Req0, <<"无权访问该附件"/utf8>>, ?ERR_BAD_REQUEST)
            end
    end;
view_url(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% POST /v1/attachment/upload — multipart/form-data 流式直传
%% ===================================================================

%% @doc POST /v1/attachment/upload?object_key=xxx&mime_type=image/jpeg
%% Content-Type: multipart/form-data; boundary=...（文件字段名 file）
%% 契约：客户端先 presign（&method=multipart）拿 upload_url 与 object_key，
%% 再把文件 POST 到该 URL，成功后照旧 confirm 落库。本端点只收文件：
%% 流式读 body → 临时文件 → elib_oss 从文件 PUT Garage，全程不驻留内存；
%% 失败可重传（幂等，覆盖 Garage 同 key 对象）。
-spec upload(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
upload(<<"POST">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    ObjectKey = proplists:get_value(<<"object_key">>, Qs, <<>>),
    MimeType = proplists:get_value(<<"mime_type">>, Qs, <<>>),
    %% 先鉴权后收流：object_key 前缀快检不合法直接拒绝，不吃满请求体流量。
    %% 归属（pending 行 creator）/范围/mime 白名单的完整校验仍在收流后执行。
    case elib_oss:owner_of_key(ObjectKey) of
        {ok, Uid} ->
            case multipart_boundary(Req0) of
                {ok, Boundary} ->
                    upload_stream(Uid, ObjectKey, MimeType, Boundary, Req0);
                {error, invalid_content_type} ->
                    elib_response:error(
                        Req0, <<"Content-Type 必须为 multipart/form-data"/utf8>>, ?ERR_BAD_REQUEST
                    )
            end;
        {ok, _OtherUid} ->
            elib_response:error(Req0, <<"非法对象归属"/utf8>>, ?ERR_BAD_REQUEST);
        {error, invalid_key} ->
            elib_response:error(Req0, <<"非法对象键"/utf8>>, ?ERR_BAD_REQUEST)
    end;
upload(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 从 Content-Type 提取 multipart boundary（含引号剥离）。
-spec multipart_boundary(cowboy_req:req()) ->
    {ok, binary()} | {error, invalid_content_type}.
multipart_boundary(Req0) ->
    case cowboy_req:header(<<"content-type">>, Req0) of
        CT when is_binary(CT) ->
            case binary:split(CT, <<"boundary=">>) of
                [_Main, Rest] ->
                    Part =
                        case binary:split(Rest, <<";">>) of
                            [P, _] -> P;
                            [P] -> P
                        end,
                    case strip_quotes(Part) of
                        <<>> -> {error, invalid_content_type};
                        Boundary -> {ok, Boundary}
                    end;
                _ ->
                    {error, invalid_content_type}
            end;
        _ ->
            {error, invalid_content_type}
    end.

-spec strip_quotes(binary()) -> binary().
strip_quotes(Part0) ->
    Part = string:trim(Part0),
    case Part of
        <<$", Inner/binary>> ->
            %% 引号包裹：取闭合引号前的值（其后若还有 ; charset=... 一并忽略）
            case binary:split(Inner, <<34>>) of
                [Value, _Rest] -> Value;
                [Value] -> Value
            end;
        _ ->
            Part
    end.

%% @doc 流式收 body：cowboy 按小块读 → elib_multipart 提取 file part 写临时
%% 文件 → logic 校验并转入 Garage。临时文件用后即删（含校验失败路径）。
-spec upload_stream(
    integer(), binary(), binary(), binary(), cowboy_req:req()
) -> cowboy_req:req().
upload_stream(Uid, ObjectKey, MimeType, Boundary, Req0) ->
    TmpPath = tmp_path(),
    case file:open(TmpPath, [write, binary, raw, exclusive, {mode, 8#0600}]) of
        {ok, Fd} ->
            upload_stream_fd(Fd, TmpPath, Uid, ObjectKey, MimeType, Boundary, Req0);
        {error, OpenReason} ->
            %% 临时文件创建失败（磁盘满/tmp 不可写）= 服务端故障，映射 5xx
            ?ERROR_LOG([
                "attach_handler tmp file open failed: ",
                TmpPath,
                OpenReason
            ]),
            elib_response:error(
                Req0, <<"附件写入服务端临时存储失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
            )
    end.

%% @doc 持有 Fd 的收包主体：无论成功失败，退出时关闭并删除临时文件。
-spec upload_stream_fd(
    file:io_device(), string(), integer(), binary(), binary(), binary(), cowboy_req:req()
) -> cowboy_req:req().
upload_stream_fd(Fd, TmpPath, Uid, ObjectKey, MimeType, Boundary, Req0) ->
    St0 = elib_multipart:new(
        Boundary,
        fun(Data) -> ok = file:write(Fd, Data) end,
        elib_oss:max_file_size()
    ),
    Resp =
        try
            case stream_body(Req0, St0) of
                {ok, St} ->
                    file:sync(Fd),
                    case elib_multipart:result(St) of
                        {ok, #{size := Size}} ->
                            case
                                attachment_upload_logic:upload_multipart(
                                    Uid, ObjectKey, MimeType, TmpPath, Size
                                )
                            of
                                {ok, Data} ->
                                    elib_response:success(Req0, Data, "success.");
                                {error, Reason} ->
                                    upload_error(Req0, Reason)
                            end;
                        {error, no_file_part} ->
                            elib_response:error(
                                Req0, <<"缺少 multipart 文件字段 file"/utf8>>, ?ERR_BAD_REQUEST
                            )
                    end;
                {error, file_too_large} ->
                    elib_response:error(Req0, <<"文件超过大小限制"/utf8>>, ?ERR_PAYLOAD_TOO_LARGE);
                {error, {bad_part, _Reason}} ->
                    elib_response:error(Req0, <<"multipart 请求体不合法"/utf8>>, ?ERR_BAD_REQUEST);
                {error, {write_failed, _WErr}} ->
                    %% WriteFun 故障（磁盘满/IO 错误）是服务端故障，映射 5xx
                    elib_response:error(
                        Req0, <<"附件写入服务端临时存储失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
                    );
                {error, {read_body, _ReadReason}} ->
                    elib_response:error(Req0, <<"请求体读取失败"/utf8>>, ?ERR_BAD_REQUEST)
            end
        after
            _ = file:close(Fd),
            _ = file:delete(TmpPath)
        end,
    Resp.

%% 循环 cowboy_req:read_body 直到读完；每块喂给 elib_multipart。
%% 读完了解析仍未 done（缺 closing boundary）→ incomplete_body。
-spec stream_body(cowboy_req:req(), elib_multipart:state()) ->
    {ok, elib_multipart:state()}
    | {error, file_too_large | {bad_part, term()} | incomplete_body | {read_body, term()}}.
stream_body(Req0, St) ->
    %% read_length 256KB：控制单次读块；length 8MB：cowboy 内部聚合上限
    Opts = #{length => 8388608, period => 30000, read_length => 262144, read_timeout => 60000},
    case cowboy_req:read_body(Req0, Opts) of
        {ok, Data, _Req} ->
            case elib_multipart:stream(Data, St) of
                {done, St2} -> {ok, St2};
                {more, _} -> {error, incomplete_body};
                {error, R} -> {error, R}
            end;
        {more, Data, Req} ->
            case elib_multipart:stream(Data, St) of
                {more, St2} -> stream_body(Req, St2);
                {done, St2} -> {ok, St2};
                {error, R} -> {error, R}
            end;
        {error, R} ->
            {error, {read_body, R}}
    end.

%% 错误映射与 confirm 同口径（forbidden_key/invalid_file_type 等共用文案），
%% 超限用 413 与全局语义对齐。
-spec upload_error(cowboy_req:req(), term()) -> cowboy_req:req().
upload_error(Req0, invalid_key) ->
    elib_response:error(Req0, <<"非法对象键"/utf8>>, ?ERR_BAD_REQUEST);
upload_error(Req0, forbidden_key) ->
    elib_response:error(Req0, <<"非法对象归属"/utf8>>, ?ERR_BAD_REQUEST);
upload_error(Req0, object_not_found) ->
    elib_response:error(Req0, <<"对象未登记或已完成上传"/utf8>>, ?ERR_BAD_REQUEST);
upload_error(Req0, invalid_file_type) ->
    elib_response:error(Req0, <<"不支持的文件类型"/utf8>>, ?ERR_BAD_REQUEST);
upload_error(Req0, file_too_large) ->
    elib_response:error(Req0, <<"文件超过大小限制"/utf8>>, ?ERR_PAYLOAD_TOO_LARGE);
upload_error(Req0, {db_error, _Reason}) ->
    elib_response:error(Req0, <<"附件服务暂不可用，请稍后重试"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR);
upload_error(Req0, {storage_error, _Reason}) ->
    elib_response:error(
        Req0, <<"附件存储服务暂不可用，请稍后重试"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR
    );
upload_error(Req0, _Reason) ->
    elib_response:error(Req0, <<"附件上传失败"/utf8>>, ?ERR_BAD_REQUEST).

%% @doc 临时落盘路径（/tmp 写完即删，不依赖任何应用配置）。
-spec tmp_path() -> string().
tmp_path() ->
    Rand = binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(8))),
    filename:join([
        "/tmp",
        "attach_upload_" ++ integer_to_list(erlang:unique_integer([positive])) ++ "_" ++ Rand ++
            ".part"
    ]).
