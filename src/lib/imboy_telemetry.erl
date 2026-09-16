-module(imboy_telemetry).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_telemetry - OpenTelemetry 遥测（对接 Uptrace，trace.imboy.pub）
%%% OpenTelemetry telemetry bootstrap (Uptrace APM integration)
%%%
%%% 设计约束 / Design:
%%%   1. 零侵入：IMBOY_UPTRACE_DSN 未配置时 init/0 直接返回，不启动任何
%%%      otel 组件，未对接 Uptrace 的环境行为与此前完全一致。
%%%   2. 不阻断启动：init 由 imboy_app:start/2 在 maybe_migrate 之后调用，
%%%      内部异常全部捕获并记日志，遥测故障不影响 IM 主链路。
%%%   3. 敏感值不落仓：DSN（含项目 token）只经 IMBOY_UPTRACE_DSN 环境变量
%%%      注入（imboy_env:override_from_env/0），sys.config 不携带。
%%%   4. OTLP/HTTP 协议直连同机 Uptrace（默认 http://127.0.0.1:14318），
%%%      DSN 以 uptrace-dsn 头鉴权；跨机上报可覆盖 endpoint（走 https）。
%%%
%%% 使用方式：
%%%   imboy_telemetry:with_span(<<"user.login">>, fun() -> ... end)
%%%   imboy_telemetry:with_span(<<"msg.route">>, #{attrs => #{...}}, fun)
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-export([init/0, started/0, with_span/2, with_span/3]).

-include_lib("kernel/include/logger.hrl").

%% 默认 OTLP endpoint：prod/dev 实例与 Uptrace 同机（106.53.76.53）
-define(DEFAULT_OTLP_ENDPOINT, <<"http://127.0.0.1:14318">>).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc 遥测初始化入口。按 uptrace_dsn 是否配置决定是否启用。
%% ⚠️ opentelemetry 在 imboy.app 的 applications 依赖链里，会在本模块
%% init 之前被 OTP 以默认（空 exporter）配置自动拉起，因此不能以
%% "app 是否在跑"判断启用状态——只要配了 DSN 就始终注入配置并重启 SDK。
-spec init() -> ok.
init() ->
    case application:get_env(imboy, uptrace_dsn, <<>>) of
        <<>> ->
            ?LOG_INFO("telemetry: IMBOY_UPTRACE_DSN not set, OpenTelemetry disabled"),
            ok;
        _Dsn ->
            try enable() of
                ok ->
                    ?LOG_INFO("telemetry: OpenTelemetry enabled (service=~ts)", [service_name()])
            catch
                Class:Reason:Stack ->
                    ?LOG_WARNING(
                        "telemetry: init failed, disabled (class=~p reason=~p stack=~120p)",
                        [Class, Reason, Stack]
                    ),
                    ok
            end
    end.

%% @doc OpenTelemetry SDK 是否已在本节点启用。
-spec started() -> boolean().
started() ->
    case application:which_applications() of
        Apps when is_list(Apps) ->
            lists:keymember(opentelemetry, 1, Apps);
        _ ->
            false
    end.

%% @doc 业务侧便捷打点：在当前进程创建 span 并执行 Fun。
%% 遥测未启用时直接执行 Fun，零开销。
-spec with_span(otel_span:name(), fun(() -> T)) -> T.
with_span(Name, Fun) ->
    with_span(Name, #{}, Fun).

%% @doc 同 with_span/2，Opts 支持 #{attrs => #{Key => Val}} 与
%% 标准 otel span 选项（kind、links 等）。遥测未启用时直接执行 Fun。
-spec with_span(otel_span:name(), map(), fun(() -> T)) -> T.
with_span(Name, Opts, Fun) ->
    case started() of
        true ->
            Tracer = opentelemetry:get_tracer(),
            SpanOpts = span_opts(Opts),
            otel_tracer:with_span(Tracer, Name, SpanOpts, fun(_) -> Fun() end);
        false ->
            Fun()
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 启用 OpenTelemetry SDK 并上报首个 boot span。
%% 首个 span 用于「对接是否打通」的直观验证（Uptrace UI 应出现
%% imboy.boot trace）。
-spec enable() -> ok.
enable() ->
    Endpoint = application:get_env(imboy, uptrace_otlp_endpoint, ?DEFAULT_OTLP_ENDPOINT),
    Dsn = application:get_env(imboy, uptrace_dsn, <<>>),
    %% SDK 启动时读取 OTEL_SERVICE_NAME 构造 resource，比改 SDK resource
    %% detector 配置更简单可靠。
    _ = os:putenv("OTEL_SERVICE_NAME", binary_to_list(service_name())),
    ok = application:set_env(opentelemetry, processors, [
        {otel_batch_processor, #{
            %% ⚠️ 1.10 起 exporter 按 signals 拆分，traces 必须用
            %% otel_exporter_traces_otlp（otel_exporter_otlp 已无 export/3，
            %% 配错会静默 undef：span 入队但永不导出）。
            exporter =>
                {otel_exporter_traces_otlp, #{
                    endpoints => [Endpoint],
                    headers => [{<<"uptrace-dsn">>, Dsn}],
                    protocol => http_protobuf,
                    %% 同机上报，超时收紧避免拖累 batch 进程
                    timeout => 5000
                }},
            %% 上报窗口：5 秒批量上报，兼顾实时性与吞吐
            scheduled_delay_ms => 5000
        }}
    ]),
    %% ⚠️ opentelemetry 在 imboy.app 的 applications 依赖链里，会在
    %% imboy_app:start 回调执行**之前**被 OTP 以默认配置自动拉起；
    %% 此处 set_env 已晚于其 sup 初始化，必须显式重启 SDK 才能加载
    %% 上面注入的 processors（exporter/endpoint/DSN）。
    _ =
        case started() of
            true ->
                ok = application:stop(opentelemetry),
                application:ensure_all_started(opentelemetry);
            false ->
                application:ensure_all_started(opentelemetry)
        end,
    _ = boot_span(),
    ok.

%% @doc 环境名 -> service.name：imboy-<IMBOYENV>（如 imboy-prod / imboy-dev）。
-spec service_name() -> binary().
service_name() ->
    <<"imboy-", (imboy_env:current())/binary>>.

%% @doc 启动标记 span：标记一次节点启动，兼作对接验证探针。
-spec boot_span() -> reference() | undefined.
boot_span() ->
    Tracer = opentelemetry:get_tracer(),
    SpanCtx = otel_tracer:start_span(Tracer, <<"imboy.boot">>, #{
        attributes => #{
            <<"service.name">> => service_name(),
            <<"imboy.version">> => imboy_version()
        }
    }),
    otel_span:end_span(SpanCtx).

%% @doc 把 #{attrs => Attrs} 翻译为 otel span 选项；未知键原样透传。
-spec span_opts(map()) -> map().
span_opts(Opts) ->
    case maps:take(attrs, Opts) of
        {Attrs, Rest} when is_map(Attrs) ->
            Rest#{attributes => Attrs};
        _Rest ->
            Opts
    end.

%% @doc 当前发布版本（VERSION 文件内容），读取失败不致命。
-spec imboy_version() -> binary().
imboy_version() ->
    case application:get_key(imboy, vsn) of
        {ok, Vsn} -> imboy_telemetry_bin(Vsn);
        undefined -> <<>>
    end.

-spec imboy_telemetry_bin(term()) -> binary().
imboy_telemetry_bin(V) when is_binary(V) -> V;
imboy_telemetry_bin(V) when is_list(V) -> unicode:characters_to_binary(V);
imboy_telemetry_bin(V) when is_atom(V) -> atom_to_binary(V, utf8);
imboy_telemetry_bin(_) -> <<>>.
