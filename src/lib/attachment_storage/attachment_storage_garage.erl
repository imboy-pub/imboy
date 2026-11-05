%%% Garage I/O 使用已校验位置与凭据快照，不在派发时重读 registry/global endpoint。
-module(attachment_storage_garage).
-behaviour(attachment_storage_driver).
-export([
    put/4,
    head/3,
    head_bounded/1,
    head_bounded/3,
    copy_dispatch/4,
    copy_cross/5,
    delete/3,
    presign_put/4,
    presign_get/4,
    read/3,
    put_snapshot/4,
    read_snapshot/1,
    delete_snapshot/1,
    head_snapshot/1,
    presign_snapshot/2,
    presign_put_snapshot/3
]).

%% direct-v1 有界控制面常量：错误体/XML 响应硬上限与单次请求预算（协议 §4，
%% T44：控制内存有界，不完整缓冲超限响应）。
-define(CONTROL_MAX_BODY, 65536).
-define(CONTROL_TIMEOUT_MS, 15000).
read(Ns, C, K) -> with_snapshot(Ns, C, K, fun read_snapshot/1).
read_snapshot(I) ->
    Url = signed(I, <<"GET">>, <<>>, calendar:universal_time(), 60),
    case
        httpc:request(get, {binary_to_list(Url), []}, [{timeout, 30000}], [{body_format, binary}])
    of
        {ok, {{_, 200, _}, _, Bytes}} -> {ok, Bytes};
        {ok, {{_, 404, _}, _, _}} -> {error, not_found};
        {ok, {{_, Status, _}, _, _}} -> {error, {storage_status, Status}};
        {error, R} -> {error, R}
    end.
put(Ns, C, K, Body) ->
    Now = calendar:universal_time(),
    Exp = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Now) + 300),
    with_snapshot(Ns, C, K, fun(I) ->
        put_snapshot(I, Body, <<"application/octet-stream">>, #{issued_at => Now, expires_at => Exp})
    end).
put_snapshot(I, Body, Mime, #{issued_at := Issued, expires_at := Expiry}) ->
    Deadline = erlang:monotonic_time(millisecond) + 30000,
    try
        Diff =
            calendar:datetime_to_gregorian_seconds(Expiry) -
                calendar:datetime_to_gregorian_seconds(Issued),
        ExpiryMs = (calendar:datetime_to_gregorian_seconds(Expiry) - 62167219200) * 1000,
        case Diff > 0 andalso Diff =< 604800 andalso erlang:system_time(millisecond) < ExpiryMs of
            true ->
                attachment_upload_spool:with_source(Body, fun(Source) ->
                    put_source(I, Source, Mime, Issued, Diff, ExpiryMs, Deadline)
                end);
            false ->
                {error, invalid_signature_context}
        end
    catch
        error:badarg -> {error, invalid_signature_context};
        error:function_clause -> {error, invalid_signature_context}
    end;
put_snapshot(_, _, _, _) ->
    {error, invalid_signature_context}.

put_source(I, Source, Mime, Issued, Ttl, ExpiryMs, Deadline) ->
    case request_window(ExpiryMs, Deadline) of
        {ok, Timeout} ->
            %% Signed times and identity are frozen; retries only rewind the same inode.
            %% 签名时间和位置不刷新；HTTP 重试从同一已校验句柄的偏移 0 开始。
            Url = signed(I, <<"PUT">>, Mime, Issued, Ttl),
            Producer = fun(Offset) -> produce_chunk(Source, Offset, ExpiryMs, Deadline) end,
            Headers = [{"content-length", integer_to_list(attachment_upload_spool:size(Source))}],
            case
                httpc:request(
                    put,
                    {binary_to_list(Url), Headers, binary_to_list(Mime), {Producer, 0}},
                    [{timeout, Timeout}, {connect_timeout, Timeout}, {autoredirect, false}],
                    []
                )
            of
                {ok, {{_, Status, _}, _, _}} when Status >= 200, Status < 300 -> ok;
                {ok, {{_, Status, _}, _, _}} -> {error, {storage_status, Status}};
                {error, R} -> {error, R}
            end;
        Error ->
            Error
    end.

produce_chunk(Source, Offset, ExpiryMs, Deadline) ->
    case request_window(ExpiryMs, Deadline) of
        {ok, _} ->
            case attachment_upload_spool:read(Source, Offset) of
                {ok, Bytes} -> {ok, Bytes, Offset + byte_size(Bytes)};
                eof -> eof;
                {error, Reason} -> erlang:error({upload_read_failed, Reason})
            end;
        {error, Reason} ->
            erlang:error(Reason)
    end.

request_window(ExpiryMs, Deadline) ->
    ExpiryLeft = ExpiryMs - erlang:system_time(millisecond),
    BudgetLeft = Deadline - erlang:monotonic_time(millisecond),
    case {ExpiryLeft > 0, BudgetLeft > 0} of
        {false, _} -> {error, invalid_signature_context};
        {_, false} -> {error, timeout};
        {true, true} -> {ok, min(ExpiryLeft, BudgetLeft)}
    end.

head(Ns, C, K) ->
    head_bounded(Ns, C, K).

%% @doc direct-v1 真 HEAD（step-5 / R02）：presigned HEAD（query 签名方法
%% HEAD），只消费响应元数据，不读取正文。头集合受信：
%% content-length 是对象长度而非已传输字节。签名与 GET/PUT 的方法行严格
%% 一致；若 A0/5a 实测反代会把 HEAD 改写为 GET，该合同须按 profile 显式
%% 登记（不得静默回落 read）。
head_bounded(Ns, C, K) ->
    with_snapshot(Ns, C, K, fun head_bounded/1).

head_bounded(I) ->
    Url = signed(I, <<"HEAD">>, <<>>, calendar:universal_time(), 60),
    case bounded_control_request(head, Url, [], 0) of
        {ok, 200, Headers, <<>>} -> head_from_headers(Headers);
        {ok, 404, _, _} -> {error, not_found};
        {ok, Status, _, _} -> {error, {storage_status, Status}};
        {error, R} -> {error, R}
    end.

head_from_headers(Headers) ->
    case content_length(Headers) of
        {ok, Size} -> {ok, head_from_headers(Headers, Size)};
        {error, _} = Error -> Error
    end.

head_from_headers(Headers, Size) ->
    Type =
        case header_value(Headers, "content-type") of
            undefined -> <<"application/octet-stream">>;
            T -> iolist_to_binary(T)
        end,
    Etag =
        case header_value(Headers, "etag") of
            undefined -> undefined;
            E -> iolist_to_binary(E)
        end,
    case Etag of
        undefined -> #{size => Size, content_type => Type};
        _ -> #{size => Size, content_type => Type, etag => Etag}
    end.

content_length(Headers) ->
    case [Value || {Name, Value} <- Headers, string:lowercase(Name) =:= "content-length"] of
        [[Digit | Rest]] when Digit >= $0, Digit =< $9 -> content_length(Rest, Digit - $0);
        _ -> {error, invalid_content_length}
    end.

content_length([], Size) ->
    {ok, Size};
content_length([Digit | Rest], Size) when
    Digit >= $0, Digit =< $9, Size =< (9223372036854775807 - (Digit - $0)) div 10
->
    content_length(Rest, Size * 10 + Digit - $0);
content_length(_, _) ->
    {error, invalid_content_length}.

header_value(Headers, Name) ->
    case lists:search(fun({Key, _}) -> string:lowercase(Key) =:= Name end, Headers) of
        {value, {_, V}} -> V;
        false -> undefined
    end.

%% @doc direct-v1 单次 Copy dispatch（协议 §1.3 / §3.2）：staging source →
%% candidate target 的服务端 CopyObject。一次授权一次请求：autoredirect
%% false、无重试、有界 XML 响应（?CONTROL_MAX_BODY）；HTTP 200 但 XML 为
%% 错误形态（T16）→ {error, copy_xml_error}，不当作成功。调用方对超时/
%% 响应丢失必须记 UNKNOWN_QUARANTINED，不得据此重发原 target。
copy_dispatch(Ns, C, SourceKey, TargetKey) ->
    with_snapshot(Ns, C, TargetKey, fun(I) ->
        Cfg = maps:get(garage_config, I, #{}),
        {AK, SK, _Region} = garage_creds(Cfg),
        AmzDate = elib_s3_sign:format_amz_date(calendar:universal_time()),
        CopySource = <<"/", C/binary, "/", (elib_s3_sign:uri_encode_path(SourceKey))/binary>>,
        Auth = elib_s3_sign:authorization_header_extra(
            <<"PUT">>,
            C,
            TargetKey,
            undefined,
            AmzDate,
            AK,
            SK,
            [
                {<<"x-amz-copy-source">>, CopySource},
                {<<"x-amz-copy-source-if-match">>, <<"*">>}
            ]
        ),
        Url = iolist_to_binary([
            maps:get(endpoint, I),
            "/",
            C,
            "/",
            elib_s3_sign:uri_encode_path(TargetKey)
        ]),
        Headers = [
            {"authorization", binary_to_list(Auth)},
            {"x-amz-copy-source", binary_to_list(CopySource)},
            {"x-amz-copy-source-if-match", "*"},
            {"x-amz-date", binary_to_list(AmzDate)},
            {"x-amz-content-sha256", "UNSIGNED-PAYLOAD"}
        ],
        case bounded_control_request(put, Url, Headers, ?CONTROL_MAX_BODY) of
            {ok, Status, _, Body} when Status >= 200, Status < 300 ->
                parse_copy_result(Body);
            {ok, 404, _, _} ->
                {error, not_found};
            {ok, Status, _, _} ->
                {error, {storage_status, Status}};
            {error, R} ->
                {error, R}
        end
    end).

parse_copy_result(Body) ->
    case byte_size(Body) =:= 0 of
        true ->
            {error, copy_no_result};
        false ->
            HasError = binary:match(Body, <<"<Error">>) =/= nomatch,
            HasOk = binary:match(Body, <<"<CopyObjectResult">>) =/= nomatch,
            case {HasError, HasOk} of
                {true, _} -> {error, copy_xml_error};
                {_, true} -> {ok, #{etag => copy_etag(Body)}};
                {false, false} -> {error, copy_xml_unrecognized}
            end
    end.

%% @doc V3-13（G13）：跨桶服务端 CopyObject——private winner → 公开桶发布。
%% 与 copy_dispatch/4（同桶 staging→candidate）的差异：
%%   * 源/目标 bucket 显式分离（copy_dispatch 的 with_snapshot 要求目标桶
%%     == registry 记录桶，跨桶需独立签名路径；凭据仍是同实例 garage config）；
%%   * 语义相同：一次授权一次请求、autoredirect false、无重试、有界 XML
%%     响应、if-match "*"（源必须存在）、错误 XML 不当作成功；
%%   * 零正文过 BEAM：请求/响应都是控制面（≤ ?CONTROL_MAX_BODY）。
-spec copy_cross(
    Ns :: binary(),
    SourceBucket :: binary(),
    SourceKey :: binary(),
    TargetBucket :: binary(),
    TargetKey :: binary()
) -> {ok, #{etag => binary()}} | {error, term()}.
copy_cross(Ns, SourceBucket, SourceKey, TargetBucket, TargetKey) ->
    case attachment_storage:resolve(Ns) of
        {ok, #{driver := <<"garage">>, endpoint := EP}} ->
            Cfg = application:get_env(imboy, garage, #{}),
            case maps:get(endpoint, Cfg, undefined) =:= EP of
                true ->
                    copy_cross_req(Cfg, SourceBucket, SourceKey, TargetBucket, TargetKey);
                false ->
                    {error, {not_dispatched, location_changed}}
            end;
        _ ->
            {error, {not_dispatched, location_unverified}}
    end.

copy_cross_req(Cfg, SourceBucket, SourceKey, TargetBucket, TargetKey) ->
    {AK, SK, _Region} = garage_creds(Cfg),
    AmzDate = elib_s3_sign:format_amz_date(calendar:universal_time()),
    CopySource =
        <<"/", SourceBucket/binary, "/", (elib_s3_sign:uri_encode_path(SourceKey))/binary>>,
    Auth = elib_s3_sign:authorization_header_extra(
        <<"PUT">>,
        TargetBucket,
        TargetKey,
        undefined,
        AmzDate,
        AK,
        SK,
        [
            {<<"x-amz-copy-source">>, CopySource},
            {<<"x-amz-copy-source-if-match">>, <<"*">>}
        ]
    ),
    Url = iolist_to_binary([
        maps:get(endpoint, Cfg),
        "/",
        TargetBucket,
        "/",
        elib_s3_sign:uri_encode_path(TargetKey)
    ]),
    Headers = [
        {"authorization", binary_to_list(Auth)},
        {"x-amz-copy-source", binary_to_list(CopySource)},
        {"x-amz-copy-source-if-match", "*"},
        {"x-amz-date", binary_to_list(AmzDate)},
        {"x-amz-content-sha256", "UNSIGNED-PAYLOAD"}
    ],
    case bounded_control_request(put, Url, Headers, ?CONTROL_MAX_BODY) of
        {ok, Status, _, Body} when Status >= 200, Status < 300 ->
            parse_copy_result(Body);
        {ok, 404, _, _} ->
            {error, not_found};
        {ok, Status, _, _} ->
            {error, {storage_status, Status}};
        {error, R} ->
            {error, R}
    end.

copy_etag(Body) ->
    case binary:split(Body, <<"<ETag>">>) of
        [_, Rest] when byte_size(Rest) > 0 ->
            case binary:split(Rest, <<"</ETag>">>) of
                [E, _] -> E;
                _ -> undefined
            end;
        _ ->
            undefined
    end.

garage_creds(Cfg) ->
    {
        maps:get(access_key, Cfg, <<>>),
        maps:get(secret_key, Cfg, <<>>),
        maps:get(region, Cfg, <<"garage">>)
    }.

%% HEAD uses Gun because httpc discards duplicate Content-Length headers.
%% Copy uses async inets: 超限即取消（T44）。inets 合同：
%% 2xx 以 {stream_start, Headers, Pid} 四元组交付并需 stream_next 流控；
%% 非 2xx 以完整 result 形态交付（错误体尺寸由 Garage 自身决定，收到后仍
%% 做上限复核；对恶意存储端的收前硬上限依赖受信存储假设，A0/5a 实测登记）。
%% 不做自动重定向、不在本层重试（上层按状态机裁决）。
bounded_control_request(head, Url, [], 0) ->
    #{scheme := Scheme, host := Host} = Uri = uri_string:parse(Url),
    Transport =
        case Scheme of
            <<"https">> -> tls;
            <<"http">> -> tcp
        end,
    DefaultPort =
        case Transport of
            tls -> 443;
            tcp -> 80
        end,
    Options = #{
        transport => Transport,
        protocols => [http],
        retry => 0,
        connect_timeout => 10000,
        http_opts => #{max_headers => 100, max_header_block_size => ?CONTROL_MAX_BODY}
    },
    TlsOptions =
        case Transport of
            tcp ->
                Options;
            tls ->
                Options#{
                    tls_opts => [
                        {verify, verify_peer},
                        {cacerts, public_key:cacerts_get()},
                        {server_name_indication, binary_to_list(Host)},
                        {customize_hostname_check, [
                            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                        ]}
                    ]
                }
        end,
    {ok, _} = application:ensure_all_started(gun),
    Deadline = erlang:monotonic_time(millisecond) + ?CONTROL_TIMEOUT_MS,
    case gun:open(binary_to_list(Host), maps:get(port, Uri, DefaultPort), TlsOptions) of
        {ok, Connection} ->
            try
                head_response(Connection, Uri, Deadline)
            after
                gun:close(Connection),
                gun:flush(Connection)
            end;
        {error, _} = Error ->
            Error
    end;
bounded_control_request(Method, Url, Headers, MaxBody) ->
    Request = {binary_to_list(Url), Headers, "application/octet-stream", <<>>},
    case
        httpc:request(
            Method,
            Request,
            [{connect_timeout, 10000}, {autoredirect, false}],
            [{sync, false}, {stream, {self, once}}, {body_format, binary}]
        )
    of
        {ok, ReqId} -> collect_control(ReqId, MaxBody, ?CONTROL_TIMEOUT_MS);
        {error, R} -> {error, R}
    end.

head_response(Connection, Uri, Deadline) ->
    case gun:await_up(Connection, max(0, Deadline - erlang:monotonic_time(millisecond))) of
        {ok, http} ->
            Path = maps:get(path, Uri, <<"/">>),
            Query = maps:get(query, Uri, <<>>),
            Stream = gun:head(Connection, <<Path/binary, "?", Query/binary>>, []),
            await_head_response(Connection, Stream, Deadline);
        {error, _} = Error ->
            Error
    end.

await_head_response(Connection, Stream, Deadline) ->
    case Deadline - erlang:monotonic_time(millisecond) of
        Remaining when Remaining > 0 ->
            case gun:await(Connection, Stream, Remaining) of
                {inform, _, _} ->
                    await_head_response(Connection, Stream, Deadline);
                {response, fin, Status, Headers} ->
                    case lists:sum([byte_size(K) + byte_size(V) + 4 || {K, V} <- Headers]) of
                        Size when Size =< ?CONTROL_MAX_BODY ->
                            {ok, Status,
                                [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
                                <<>>};
                        _ ->
                            {error, headers_too_large}
                    end;
                {response, nofin, _, _} ->
                    {error, body_too_large};
                {error, _} = Error ->
                    Error;
                _ ->
                    {error, invalid_head_response}
            end;
        _ ->
            {error, timeout}
    end.

collect_control(ReqId, MaxBody, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    collect_first(ReqId, MaxBody, Deadline).

collect_first(ReqId, MaxBody, Deadline) ->
    Timeout = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {http, {ReqId, stream_start, Hs, Pid}} ->
            %% 流式交付隐含 2xx（错误状态走完整 result 形态）
            httpc:stream_next(Pid),
            collect_stream(ReqId, Pid, MaxBody, Deadline, Hs, <<>>);
        {http, {ReqId, stream_start, Hs}} ->
            collect_stream(ReqId, undefined, MaxBody, Deadline, Hs, <<>>);
        {http, {ReqId, {{_Ver, Status, _Reason}, Hs, Body}}} when is_binary(Body) ->
            case byte_size(Body) > MaxBody of
                true -> {error, body_too_large};
                false -> {ok, Status, Hs, Body}
            end;
        {http, {ReqId, {{_Ver, Status, _Reason}, Hs, _Other}}} ->
            {ok, Status, Hs, <<>>};
        {http, {ReqId, {error, R}}} ->
            {error, R};
        {http, {ReqId, saved_to_file, _}} ->
            try
                httpc:cancel_request(ReqId)
            catch
                _:_ -> ok
            end,
            {error, unexpected_saved_to_file}
    after Timeout ->
        try
            httpc:cancel_request(ReqId)
        catch
            _:_ -> ok
        end,
        {error, timeout}
    end.

collect_stream(ReqId, Pid, MaxBody, Deadline, Hs, Acc) ->
    Timeout = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {http, {ReqId, stream, Bin}} when is_binary(Bin) ->
            NewAcc = <<Acc/binary, Bin/binary>>,
            case byte_size(NewAcc) > MaxBody of
                true ->
                    try
                        httpc:cancel_request(ReqId)
                    catch
                        _:_ -> ok
                    end,
                    {error, body_too_large};
                false ->
                    case Pid of
                        undefined -> ok;
                        _ -> httpc:stream_next(Pid)
                    end,
                    collect_stream(ReqId, Pid, MaxBody, Deadline, Hs, NewAcc)
            end;
        {http, {ReqId, stream_end, _Hs}} ->
            {ok, 200, Hs, Acc};
        {http, {ReqId, {error, R}}} ->
            {error, R}
    after Timeout ->
        try
            httpc:cancel_request(ReqId)
        catch
            _:_ -> ok
        end,
        {error, timeout}
    end.

signed(#{endpoint := EP, container := C, key := K, garage_config := Cfg}, Method, Mime, Now, Ttl) ->
    elib_s3_sign:presign_url_at(Method, EP, C, K, Mime, Now, Ttl, Cfg).
with_snapshot(Ns, C, K, Fun) ->
    case attachment_storage:resolve(Ns) of
        {ok, #{driver := <<"garage">>, endpoint := EP, container := C}} ->
            Cfg = application:get_env(imboy, garage, #{}),
            case maps:get(endpoint, Cfg, undefined) =:= EP of
                true -> Fun(#{endpoint => EP, container => C, key => K, garage_config => Cfg});
                false -> {error, location_changed}
            end;
        _ ->
            {error, location_unverified}
    end.

delete(Ns, C, K) -> with_snapshot(Ns, C, K, fun delete_snapshot/1).
delete_snapshot(I) ->
    Url = signed(I, <<"DELETE">>, <<>>, calendar:universal_time(), 60),
    case httpc:request(delete, {binary_to_list(Url), []}, [{timeout, 30000}], []) of
        {ok, {{_, Status, _}, _, _}} when Status >= 200, Status < 300 -> ok;
        {ok, {{_, 404, _}, _, _}} -> {error, not_found};
        {ok, {{_, Status, _}, _, _}} -> {error, {storage_status, Status}};
        {error, R} -> {error, R}
    end.

%% @doc O(1) 元数据 stat（W2-02 confirm 锁后复核用）：与 read_snapshot 同一
%% 冻结身份 I 直接 presigned HEAD（不经 with_snapshot 重解析——二次 stat 必须
%% 与首次 stat 同一对象位置）。统一形态 #{size, mtime, etag}；last-modified
%% /etag 缺头时值为 undefined，对照层按缺证据 fail-closed。
head_snapshot(I) ->
    Url = signed(I, <<"HEAD">>, <<>>, calendar:universal_time(), 60),
    case bounded_control_request(head, Url, [], 0) of
        {ok, 200, Headers, <<>>} ->
            stat_from_headers(Headers);
        {ok, 404, _, _} ->
            {error, not_found};
        {ok, Status, _, _} ->
            {error, {storage_status, Status}};
        {error, R} ->
            {error, R}
    end.

stat_from_headers(Headers) ->
    case content_length(Headers) of
        {ok, Size} -> {ok, stat_from_headers(Headers, Size)};
        {error, _} = Error -> Error
    end.

stat_from_headers(Headers, Size) ->
    #{
        size => Size,
        mtime =>
            case header_value(Headers, "last-modified") of
                undefined ->
                    undefined;
                Date ->
                    case httpd_util:convert_request_date(Date) of
                        {{_, _, _}, {_, _, _}} = Dt ->
                            calendar:datetime_to_gregorian_seconds(Dt) - 62167219200;
                        _ ->
                            undefined
                    end
            end,
        etag =>
            case header_value(Headers, "etag") of
                undefined -> undefined;
                E -> iolist_to_binary(E)
            end
    }.

presign_put(Ns, C, K, T) ->
    with_snapshot(Ns, C, K, fun(I) ->
        {ok, signed(I, <<"PUT">>, <<"application/octet-stream">>, calendar:universal_time(), T)}
    end).
presign_get(Ns, C, K, T) ->
    with_snapshot(Ns, C, K, fun(I) ->
        {ok, signed(I, <<"GET">>, <<>>, calendar:universal_time(), T)}
    end).
presign_snapshot(I, Expiry) -> {ok, signed(I, <<"GET">>, <<>>, calendar:universal_time(), Expiry)}.

%% V3-14 S3-only 直传签发：按已冻结位置快照 I（endpoint/container/key 已
%% 校验）+ garage env 凭据签短时 PUT。Mime 为登记冻结值；Ttl 由调用方按
%% 会话冻结 expires_at 计算（<=0 拒绝）。不经 with_snapshot 重解析——
%% 调用方（internal/OA presign COMMIT 后签发）已持同一锁域验证过的位置。
presign_put_snapshot(I, Mime, Ttl) when is_map(I), is_binary(Mime), is_integer(Ttl), Ttl > 0 ->
    {ok, signed(I, <<"PUT">>, Mime, calendar:universal_time(), Ttl)};
presign_put_snapshot(_I, _Mime, _Ttl) ->
    {error, invalid_signature_context}.
