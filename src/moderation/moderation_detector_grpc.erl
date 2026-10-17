%%% @doc TextGuard gRPC 薄适配，共用受监督的 grpcbox channel。
%%% 成功响应按 optional presence 和白名单校验；服务错误必须携带 x-textguard 元数据。
-module(moderation_detector_grpc).

-behaviour(moderation_detector_transport).

-export([check/2, info/1]).

-define(DEFAULT_TIMEOUT_MS, 150).
-define(ERROR_CODE_METADATA_KEY, <<"x-textguard-error-code">>).
-define(REQUEST_ID_METADATA_KEY, <<"x-textguard-request-id">>).

%% ===================================================================
%% behaviour 实现
%% ===================================================================

%% @doc 一次文本检测：CheckText 语义 + 严格响应校验。
-spec check(
    moderation_detector_transport:detection(),
    moderation_detector_transport:ctx()
) ->
    {ok, moderation_detector_transport:check_result()}
    | {error, moderation_detector_transport:error_reason()}.
check(Detection, Ctx) ->
    with_config(
        fun(Endpoint, Transport, Deadline) ->
            case build_check_request(Detection) of
                {ok, Request} ->
                    call(
                        Transport,
                        Endpoint,
                        check_text,
                        Request,
                        Deadline,
                        fun(Response) ->
                            moderation_detector_transport:validate_check(Response, Detection, grpc)
                        end
                    );
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        Ctx
    ).

%% @doc 服务元信息：GetInfo 语义 + 严格响应校验。
-spec info(moderation_detector_transport:ctx()) ->
    {ok, moderation_detector_transport:info_result()}
    | {error, moderation_detector_transport:error_reason()}.
info(Ctx) ->
    with_config(
        fun(Endpoint, Transport, Deadline) ->
            call(
                Transport,
                Endpoint,
                get_info,
                #{},
                Deadline,
                fun(Response) -> moderation_detector_transport:validate_info(Response, grpc) end
            )
        end,
        Ctx
    ).

%% ===================================================================
%% 配置装载（fail-closed：缺配置 = not_ready）
%% ===================================================================

-spec with_config(
    fun((map(), fun(), pos_integer()) -> R),
    moderation_detector_transport:ctx()
) -> R.
with_config(Fun, Ctx) ->
    Conf = config_ds:env(moderation_detector, []),
    Host = proplists:get_value(grpc_host, Conf),
    Port = proplists:get_value(grpc_port, Conf),
    case {Host, Port} of
        {H, P} when is_binary(H), H =/= <<>>, is_integer(P), P > 0, P =< 65535 ->
            case resolve_transport(Conf) of
                {ok, Transport} ->
                    Deadline = deadline_ms(Ctx, Conf),
                    Fun(#{host => H, port => P}, Transport, Deadline);
                error ->
                    {error, not_ready}
            end;
        _ ->
            {error, not_ready}
    end.

%% transport fun 解析：直接 fun/4 或 {M, F} 间接引用（sys.config 可表达）。
-spec resolve_transport(proplists:proplist()) -> {ok, fun()} | error.
resolve_transport(Conf) ->
    case proplists:get_value(grpc_transport, Conf) of
        Fun when is_function(Fun, 4) ->
            {ok, Fun};
        {M, F} when is_atom(M), is_atom(F) ->
            {ok, fun M:F/4};
        undefined ->
            {ok, fun moderation_detector_grpcbox:call/4};
        _ ->
            error
    end.

%% 总 deadline：Ctx 显式值 > 配置 timeout_ms > 默认 150（与 HTTP 适配一致）。
-spec deadline_ms(moderation_detector_transport:ctx(), proplists:proplist()) ->
    pos_integer().
deadline_ms(Ctx, Conf) ->
    Raw = ctx_deadline(Ctx, proplists:get_value(timeout_ms, Conf, ?DEFAULT_TIMEOUT_MS)),
    case Raw of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_TIMEOUT_MS
    end.

-spec ctx_deadline(moderation_detector_transport:ctx(), term()) -> term().
ctx_deadline(#{deadline_ms := D} = _Ctx, _ConfDefault) when is_integer(D), D > 0 ->
    D;
ctx_deadline(_Ctx, ConfDefault) ->
    ConfDefault.

%% ===================================================================
%% 请求构造（proto 字段名 snake_case map）
%% ===================================================================

-spec build_check_request(moderation_detector_transport:detection()) ->
    {ok, map()} | {error, protocol_error}.
build_check_request(Detection) when is_map(Detection) ->
    ReqId = maps:get(request_id, Detection, undefined),
    Scene = maps:get(scene, Detection, undefined),
    Fields = maps:get(fields, Detection, undefined),
    case valid_request_shape(ReqId, Scene, Fields) of
        ok ->
            {ok, SceneBin} = moderation_detector_transport:scene_to_binary(Scene),
            {ok, FieldMaps} = normalize_fields(Fields),
            {ok, #{
                request_id => ReqId,
                scene => SceneBin,
                fields => FieldMaps
            }};
        error ->
            {error, protocol_error}
    end;
build_check_request(_NotAMap) ->
    {error, protocol_error}.

-spec valid_request_shape(term(), term(), term()) -> ok | error.
valid_request_shape(ReqId, Scene, Fields) when
    is_binary(ReqId), is_atom(Scene), is_list(Fields)
->
    case moderation_detector_transport:scene_to_binary(Scene) of
        {ok, _} -> ok;
        error -> error
    end;
valid_request_shape(_, _, _) ->
    error.

%% {FieldId, Text} 全二进制；空表放行（裁决权在服务端）。
-spec normalize_fields([{binary(), binary()}]) -> {ok, [map()]} | error.
normalize_fields(Fields) ->
    normalize_fields(Fields, []).

normalize_fields([], Acc) ->
    {ok, lists:reverse(Acc)};
normalize_fields([{FieldId, Text} | Rest], Acc) when
    is_binary(FieldId), is_binary(Text)
->
    normalize_fields(Rest, [#{field_id => FieldId, text => Text} | Acc]);
normalize_fields(_, _Acc) ->
    error.

%% ===================================================================
%% 调用（错误闭集映射；传输崩溃/形状非法 → internal）
%% ===================================================================

-spec call(
    fun(),
    map(),
    check_text | get_info,
    map(),
    pos_integer(),
    fun((map()) -> R)
) -> R.
call(Transport, Endpoint, Method, Request, Deadline, Validator) ->
    try Transport(Endpoint, Method, Request, Deadline) of
        {ok, Response} ->
            Validator(Response);
        {error, GrpcError} ->
            {error, map_grpc_error(GrpcError, maps:get(request_id, Request, <<>>))};
        _BadShape ->
            {error, internal}
    catch
        _:_Crash ->
            %% 传输层崩溃 = fail-closed internal（不重试、不上抛崩溃）
            {error, internal}
    end.

%% ===================================================================
%% gRPC 状态码映射（闭集原子，见模块头映射表）
%% ===================================================================

map_grpc_error(connection_failed, _) ->
    connection_failed;
map_grpc_error(timeout, _) ->
    timeout;
map_grpc_error(not_ready, _) ->
    not_ready;
map_grpc_error(text_too_large, _) ->
    text_too_large;
map_grpc_error(#{grpc_status := Status, metadata := Metadata}, ExpectedRequestId) ->
    try
        Code = metadata_value(maps:get(?ERROR_CODE_METADATA_KEY, Metadata)),
        RequestId = metadata_value(maps:get(?REQUEST_ID_METADATA_KEY, Metadata)),
        true = RequestId =:= <<>> orelse RequestId =:= ExpectedRequestId,
        true = RequestId =:= <<>> orelse moderation_detector_transport:valid_request_id(RequestId),
        true = lists:member(Status, error_statuses(Code)),
        moderation_detector_transport:error_code_to_reason(Code)
    catch
        _:_ -> protocol_error
    end;
map_grpc_error(_, _) ->
    protocol_error.

metadata_value(Value) when is_binary(Value) -> Value;
metadata_value([Value | Rest]) when is_binary(Value) ->
    true = lists:all(fun(Other) -> Other =:= Value end, Rest),
    Value.

error_statuses(<<"INVALID_ARGUMENT">>) -> [3];
error_statuses(<<"INVALID_UTF8">>) -> [3];
error_statuses(<<"PROTOCOL_ERROR">>) -> [13, 15];
error_statuses(<<"TEXT_TOO_LARGE">>) -> [8];
error_statuses(<<"NORMALIZATION_LIMIT">>) -> [8];
error_statuses(<<"MATCH_BUDGET_EXCEEDED">>) -> [8];
error_statuses(<<"OVERLOADED">>) -> [8];
error_statuses(<<"UNAUTHENTICATED">>) -> [16];
error_statuses(<<"PERMISSION_DENIED">>) -> [7];
error_statuses(<<"NOT_READY">>) -> [14];
error_statuses(<<"CONNECTION_FAILED">>) -> [14];
error_statuses(<<"CHECK_DEADLINE">>) -> [4];
error_statuses(<<"INTERNAL_ERROR">>) -> [13, 15];
error_statuses(<<"NORMALIZER_INCOMPATIBLE">>) -> [13, 15];
error_statuses(<<"UNSUPPORTED_RULESET">>) -> [9].
