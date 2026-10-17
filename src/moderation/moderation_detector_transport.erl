%%% @doc 内容安全检测器（TextGuard）传输适配 behaviour（T15）。
%%%
%%% imboy 侧对 Rust TextGuard 服务的**薄传输客户端**契约：
%%%
%%%   * `check/2` —— 一次文本检测（归一化请求 → 远端裁决）；
%%%   * `info/1`  —— 服务元信息/就绪探测（GET /v1/info、GetInfo）。
%%%
%%% 实现模块：
%%%   * `moderation_detector_http` —— HTTP 适配（POST /v1/check、GET /v1/info）；
%%%   * `moderation_detector_grpc` —— gRPC 适配（CheckText/GetInfo，传输函数
%%%     经配置注入，见该模块头注释的装配说明）；
%%%   * 本模块自带 `legacy` 实现（`legacy_check/2` / `legacy_info/1`）：
%%%     返回 `{ok, legacy}` 标记——**不发起任何远程调用**，调用方据此走
%%%     既有敏感词链路，绝不把 legacy 误当 allow/block 裁决。
%%%
%%% 选择器 `select/0` 读取 `{imboy, moderation_detector}` 配置的
%%% `{transport, legacy | http | grpc}`；缺省/未知值一律回落 legacy
%%% （未配置 = 不出网）。
%%%
%%% 错误闭集（AC-060）：`check/info` 的错误理由**只能是**下列固定原子，
%%% 远端二进制串永远不得动态成原子（禁止 binary_to_atom/list_to_atom 于
%%% 远端数据；白名单查表命中既有原子是唯一映射方式）：
%%%
%%%   overloaded | text_too_large | unauthenticated | permission_denied |
%%%   not_ready | protocol_error | timeout | connection_failed | internal
%%%
%%% 总预算（AC-061）：每次调用强制一个「调用方提供或配置」的总 deadline
%%% （默认 150ms，连接+请求共用），失败立即上抛——**不做任何自动重试**；
%%% 熔断/重试策略由调用方配置（T19 装配），本层不「好心」重试。
%%%
%%% 配置（application env `{imboy, moderation_detector}`，proplists）：
%%%
%%%   `{transport, legacy | http | grpc}`        传输选择，默认 legacy
%%%   `{timeout_ms, pos_integer()}`              总 deadline 默认 150ms
%%%   `{token, binary()}`                        Bearer 令牌（占位符部署）
%%%   `{http_base_url, binary()}`                如 <<"http://127.0.0.1:7463">>
%%%   `{grpc_host, binary()}` / `{grpc_port, ...}` gRPC 端点
%%%   `{grpc_transport, fun/4 | {M, F}}`         gRPC 传输函数注入（T19）
%%%   `{max_response_bytes, pos_integer()}`      响应体上限，默认 65536
%%%
%%% 配置缺失时适配器返回 `{error, not_ready}`（fail-closed，绝不静默放行）。
-module(moderation_detector_transport).

%% 选择器 + legacy/noop 实现（本模块即 legacy 实现）。
-export([select/0, legacy_check/2, legacy_info/1]).
%% 适配器共享的白名单查表（单一真源，两个适配模块复用）。
-export([scene_to_binary/1, decision_to_atom/1]).

-export_type([
    scene/0,
    decision/0,
    detection/0,
    ctx/0,
    check_result/0,
    info_result/0,
    error_reason/0
]).

%% ===================================================================
%% 类型（T15 冻结；T19 装配两侧共用）
%% ===================================================================

%% 检测场景：群/频道消息、时刻帖子、资料字段。
-type scene() :: channel_message | moment_post | profile_field.

%% 远端裁决值（HTTP/gRPC 白名单查表映射后的原子）。
%% review = 送人工复审队列（对齐既有 moderation review 域）。
-type decision() :: allow | block | review.

%% 归一化检测请求：fields 为 {FieldId, Text} 显式文本对。
-type detection() :: #{
    request_id := binary(),
    scene := scene(),
    fields := [{FieldId :: binary(), Text :: binary()}]
}.

%% 调用上下文：deadline_ms 覆盖配置的总预算（毫秒）。
-type ctx() :: #{deadline_ms => pos_integer()}.

%% 检测结果：scan_complete 缺失的远端响应一律 protocol_error（AC-060）。
-type check_result() :: #{decision := decision(), scan_complete := boolean()}.

%% 服务元信息。
-type info_result() :: #{version := binary(), ready := boolean()}.

%% 错误闭集（见模块头）：固定原子，无二进制成员。
-type error_reason() ::
    overloaded
    | text_too_large
    | unauthenticated
    | permission_denied
    | not_ready
    | protocol_error
    | timeout
    | connection_failed
    | internal.

%% ===================================================================
%% behaviour 回调
%% ===================================================================

-callback check(Detection :: detection(), Ctx :: ctx()) ->
    {ok, check_result() | legacy} | {error, error_reason()}.

-callback info(Ctx :: ctx()) ->
    {ok, info_result() | legacy} | {error, error_reason()}.

%% ===================================================================
%% 选择器
%% ===================================================================

%% @doc 按 `{transport, ...}` 配置选择实现模块。
%% 缺省/未知值回落本模块（legacy，零远程调用）。
-spec select() -> module().
select() ->
    Conf = config_ds:env(moderation_detector, []),
    case proplists:get_value(transport, Conf, legacy) of
        http -> moderation_detector_http;
        grpc -> moderation_detector_grpc;
        _ -> ?MODULE
    end.

%% ===================================================================
%% legacy / noop 实现：返回标记，绝不出网
%% ===================================================================

%% @doc legacy 标记：调用方走既有敏感词链路，不得当 TextGuard 裁决用。
-spec legacy_check(detection(), ctx()) -> {ok, legacy}.
legacy_check(_Detection, _Ctx) ->
    {ok, legacy}.

%% @doc legacy 标记（info 面同理，不出网）。
-spec legacy_info(ctx()) -> {ok, legacy}.
legacy_info(_Ctx) ->
    {ok, legacy}.

%% ===================================================================
%% 适配器共享白名单（闭集查表；远端二进制永不动态成原子）
%% ===================================================================

%% @doc 场景原子 → 线上枚举串（白名单，未知场景 error）。
-spec scene_to_binary(scene()) -> {ok, binary()} | error.
scene_to_binary(channel_message) -> {ok, <<"channel_message">>};
scene_to_binary(moment_post) -> {ok, <<"moment_post">>};
scene_to_binary(profile_field) -> {ok, <<"profile_field">>};
scene_to_binary(_) -> error.

%% @doc 裁决串 → 原子（白名单，未知值 error——绝不 allow 兜底）。
-spec decision_to_atom(binary()) -> {ok, decision()} | error.
decision_to_atom(<<"allow">>) -> {ok, allow};
decision_to_atom(<<"block">>) -> {ok, block};
decision_to_atom(<<"review">>) -> {ok, review};
decision_to_atom(_) -> error.
