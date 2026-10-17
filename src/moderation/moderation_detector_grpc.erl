%%% @doc TextGuard 检测器 gRPC 适配（T15，AC-060/061 的 imboy 侧薄客户端）。
%%%
%%% == 依赖决策（T15 勘察结论）==
%%%
%%% 仓内依赖树存在 grpcbox 0.18.0（include/deps.mk），但它**仅为
%%% opentelemetry_exporter 的编译期传递依赖**（deps.mk 注释明示「运行时走
%%% OTLP/HTTP（httpc），grpcbox 仅编译期在位、不启动」；src/ 与 test/ 零
%%% grpcbox 引用，relx 运行时也未接线）。真用 grpcbox 需要：textguard
%%% proto 进 proto/ + gpb/grpcbox 客户端代码生成 + grpcbox client/channel
%%% 运行时装配——这是 T19 集成装配的职责。因此本模块按注入式薄适配实现：
%%% **报文映射与状态码语义全部落在本模块（可全量单测），真正的 gRPC/h2
%%% 传输经配置注入的 transport fun 承载，T19 装配时绑定 grpcbox**。
%%%
%%% == transport fun 契约（注入方必须遵守）==
%%%
%%% 配置 `{grpc_transport, Fun4 | {M, F}}`（Fun4 = fun/4）：
%%%
%%%   Fun4(Endpoint, Method, Request, DeadlineMs) ->
%%%       {ok, ResponseMap}
%%%     | {error, GrpcError}
%%%
%%%   Endpoint  :: #{host => binary(), port => inet:port_number()}
%%%                （来自 {grpc_host, …}/{grpc_port, …} 配置）
%%%   Method    :: check_text | get_info（proto 服务方法名 CheckText/GetInfo
%%%                的 snake_case 原子）
%%%   DeadlineMs:: pos_integer() —— **单一总预算（AC-061，默认 150ms 或
%%%                Ctx/配置覆盖）；注入方必须把它下推为客户端层超时**
%%%                （grpcbox 即 ctx deadline / http2 超时），不得自行重试。
%%%
%%% Request/Response 均为 snake_case 原子键 map（对齐 T13 proto 字段名；
%%% gpb maps 模式同形）：
%%%
%%%   check_text 请求：#{request_id => binary(), scene => binary(),
%%%                      fields => [#{field_id => binary(), text => binary()}]}
%%%   check_text 响应：#{decision => binary()（"allow|block|review"）,
%%%                       scan_complete => boolean()}
%%%                   —— scan_complete 为 proto optional 字段：**缺失即
%%%                   protocol_error**（AC-060），decision 缺失/未知枚举
%%%                   同为 protocol_error，绝不折算 allow。
%%%   get_info 请求：#{}；响应：#{version => binary(), ready => boolean()}
%%%
%%%   GrpcError :: #{grpc_status => integer()（gRPC 状态码，必填）,
%%%                  message => binary()（可选）,
%%%                  metadata => #{binary() => binary() | [binary()]}（可选，
%%%                                h2 trailer 元数据，键小写）}
%%%               —— tonic 风格；grpcbox 原生形状
%%%               `{error, {Status, Message}, #{headers, trailers}}` 由
%%%               T19 绑定层换算成本 map（见 grpcbox_client:unary/4）。
%%%
%%% == gRPC 状态码映射表（闭集原子；T13 Rust 侧文档表对齐）==
%%%
%%%   4  DEADLINE_EXCEEDED  → timeout
%%%   8  RESOURCE_EXHAUSTED → metadata `textguard-error-code`：
%%%                             "OVERLOADED"     → overloaded
%%%                             "TEXT_TOO_LARGE" → text_too_large
%%%                             缺失/其它        → overloaded（资源耗尽默认）
%%%   11 OUT_OF_RANGE       → text_too_large（帧大小超限）
%%%   16 UNAUTHENTICATED    → unauthenticated
%%%   7  PERMISSION_DENIED  → permission_denied
%%%   14 UNAVAILABLE        → connection_failed
%%%   其余（含 CANCELLED/UNKNOWN/INTERNAL…）→ internal
%%%
%%% 传输 fun 崩溃或返回形状非法 → `{error, internal}`（fail-closed）。
%%% 错误理由全部来自上表与白名单查表的**既有原子**；远端二进制串永不
%%% 动态成原子。
%%%
%%% 配置（`{imboy, moderation_detector}`，全键见 behaviour 模块头）：
%%% grpc_host/grpc_port/grpc_transport 任一缺失或非法 → `{error, not_ready}`。
-module(moderation_detector_grpc).

-behaviour(moderation_detector_transport).

-export([check/2, info/1]).

-define(DEFAULT_TIMEOUT_MS, 150).
-define(ERROR_CODE_METADATA_KEY, <<"textguard-error-code">>).

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
                        fun validate_check_response/1
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
                fun validate_info_response/1
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
            {error, map_grpc_error(GrpcError)};
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

-spec map_grpc_error(term()) -> moderation_detector_transport:error_reason().
map_grpc_error(#{grpc_status := Status} = Err) when is_integer(Status) ->
    map_status(Status, Err);
map_grpc_error(_BadShape) ->
    internal.

-spec map_status(integer(), map()) -> moderation_detector_transport:error_reason().
map_status(4, _Err) ->
    %% DEADLINE_EXCEEDED
    timeout;
map_status(8, Err) ->
    %% RESOURCE_EXHAUSTED：textguard-error-code 元数据区分两种耗尽
    case metadata_code(Err) of
        <<"OVERLOADED">> -> overloaded;
        <<"TEXT_TOO_LARGE">> -> text_too_large;
        _UndefinedOrOther -> overloaded
    end;
map_status(11, _Err) ->
    %% OUT_OF_RANGE（帧大小超限）
    text_too_large;
map_status(16, _Err) ->
    %% UNAUTHENTICATED
    unauthenticated;
map_status(7, _Err) ->
    %% PERMISSION_DENIED
    permission_denied;
map_status(14, _Err) ->
    %% UNAVAILABLE
    connection_failed;
map_status(_Other, _Err) ->
    internal.

%% trailer 元数据取值：单值 binary 或首值（gRPC 元数据多值形态）。
-spec metadata_code(map()) -> binary() | undefined.
metadata_code(#{metadata := Metadata} = _Err) when is_map(Metadata) ->
    case Metadata of
        #{?ERROR_CODE_METADATA_KEY := Value} ->
            normalize_metadata_value(Value);
        _ ->
            undefined
    end;
metadata_code(_Err) ->
    undefined.

-spec normalize_metadata_value(term()) -> binary() | undefined.
normalize_metadata_value(V) when is_binary(V) -> V;
normalize_metadata_value([V | _]) when is_binary(V) -> V;
normalize_metadata_value(_) -> undefined.

%% ===================================================================
%% 响应严格校验（scan_complete 缺失 = protocol_error，AC-060）
%% ===================================================================

%% gRPC 响应 map 形状由 proto 编解码治理（不会出现未知键），
%% 这里只做必填字段的存在性与类型校验——与 HTTP 侧「未知键拒绝」
%% 的差异在模块头有说明。
-spec validate_check_response(term()) ->
    {ok, moderation_detector_transport:check_result()}
    | {error, protocol_error}.
validate_check_response(#{decision := DecBin, scan_complete := SC} = _Resp) when
    is_binary(DecBin), is_boolean(SC)
->
    case moderation_detector_transport:decision_to_atom(DecBin) of
        {ok, Decision} -> {ok, #{decision => Decision, scan_complete => SC}};
        error -> {error, protocol_error}
    end;
validate_check_response(_MissingOrMalformed) ->
    %% 含 scan_complete 缺失（proto optional 未置位）
    {error, protocol_error}.

-spec validate_info_response(term()) ->
    {ok, moderation_detector_transport:info_result()}
    | {error, protocol_error}.
validate_info_response(#{version := Version, ready := Ready} = _Resp) when
    is_binary(Version), is_boolean(Ready)
->
    {ok, #{version => Version, ready => Ready}};
validate_info_response(_MissingOrMalformed) ->
    {error, protocol_error}.
