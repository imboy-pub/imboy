-module(moderation_detector_transport_tests).

%%%
%%% T15 — TextGuard 检测器传输适配定向测试（AC-060/061 的 imboy 侧切片）。
%%%
%%% 覆盖面（定向，不跑全量）：
%%%   * HTTP 适配（moderation_detector_http）：真 socket 对端夹具
%%%     ewh_http_fixture（test/common，零网络 mock）——happy path、
%%%     请求线格式、全故障族（缺键/多余键/未知枚举/截断/垃圾字节/
%%%     非 UTF-8/超限体）、错误信封与状态码映射、deadline 强制与
%%%     **零重试**（服务端恰被命中 1 次）、连接拒绝、配置缺失
%%%     fail-closed、远端原子注入不变量（atom_count 不变）。
%%%   * gRPC 适配（moderation_detector_grpc）：按 T15 依赖决策以注入
%%%     transport fun 驱动（grpcbox 仅编译期在位，证据见该模块头）——
%%%     proto 字段名契约、deadline 透传、scan_complete 缺失 = 协议违约、
%%%     tonic 风格状态码/元数据映射（RESOURCE_EXHAUSTED 的
%%%     OVERLOADED/TEXT_TOO_LARGE 区分、OUT_OF_RANGE、DEADLINE_EXCEEDED…）、
%%%     传输崩溃/坏形状 fail-closed、{M, F} 间接注入。
%%%   * behaviour（moderation_detector_transport）：select 默认 legacy、
%%%     legacy 标记零远程调用、白名单闭集。
%%%
%%% 运行：make eunit t=moderation_detector_transport_tests
%%%（纯函数 + 本地真 socket，无 DB / imboy app 启动依赖）
%%%

-include_lib("eunit/include/eunit.hrl").

%% MFA 注入形态的桩（动态 M:F 调用要求导出）
-export([grpc_mfa_stub/4]).

-define(BEHAVIOUR, moderation_detector_transport).
-define(HTTP_ADAPTER, moderation_detector_http).
-define(GRPC_ADAPTER, moderation_detector_grpc).
-define(TOKEN, <<"tg-token-placeholder">>).
-define(REQ_ID, <<"req-t15-0001">>).

%% ===================================================================
%% 公共构造
%% ===================================================================

-spec det() -> moderation_detector_transport:detection().
det() ->
    #{
        request_id => ?REQ_ID,
        scene => channel_message,
        fields => [
            {<<"body">>, <<"你好 textguard"/utf8>>},
            {<<"nickname">>, <<"bob">>}
        ]
    }.

-spec check() -> term().
check() ->
    ?HTTP_ADAPTER:check(det(), #{}).

-spec reply(binary(), integer(), binary()) -> fun().
reply(Path, Code, Body) ->
    fun(Req) ->
        case maps:get(path, Req) of
            Path -> {reply, Code, [], Body};
            Other -> {reply, 404, [], iolist_to_binary(["wrong-path:", Other])}
        end
    end.

-spec check_ok_body() -> binary().
check_ok_body() ->
    <<"{\"decision\":\"block\",\"scan_complete\":true}">>.

%% 进程字典清理：适配器按 {scheme,host,port} 缓存 gun 连接。
-spec clean_gun_cache() -> ok.
clean_gun_cache() ->
    [
        erase(K)
     || {K, _} <- get(),
        is_tuple(K) andalso element(1, K) =:= gun_conn
    ],
    ok.

%% HTTP 夹具 + 配置注入（每用例独立端口 → 互不串连接缓存）。
%% Instantiator 返回 fun/0（eunit 语义：arity-1 instantiator 的返回值
%% 必须是 test，不能是断言结果 ok）。
-spec with_http([{term(), term()}], fun(), fun((term()) -> term())) -> term().
with_http(ExtraConf, Responder, TestFun) ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(gun),
            H = ewh_http_fixture:start(Responder),
            PortBin = integer_to_binary(ewh_http_fixture:port(H)),
            BaseUrl = <<"http://127.0.0.1:", PortBin/binary>>,
            application:set_env(imboy, moderation_detector, [
                {transport, http},
                {http_base_url, BaseUrl},
                {token, ?TOKEN}
                | ExtraConf
            ]),
            H
        end,
        fun(H) ->
            application:unset_env(imboy, moderation_detector),
            clean_gun_cache(),
            ewh_http_fixture:stop(H)
        end,
        fun(H) -> fun() -> TestFun(H) end end}.

%% gRPC 注入夹具：transport fun 捕获调用参数并返回预设 Outcome。
-spec with_grpc([{term(), term()}], term(), fun(() -> term())) -> term().
with_grpc(Conf, Outcome, TestFun) ->
    {setup,
        fun() ->
            application:set_env(imboy, moderation_detector, [
                {transport, grpc},
                {grpc_host, <<"127.0.0.1">>},
                {grpc_port, 7464},
                {grpc_transport, fun(Endpoint, Method, Request, Deadline) ->
                    self() ! {t15_grpc_call, {Endpoint, Method, Request, Deadline}},
                    case Outcome of
                        crash -> erlang:error(t15_transport_crash);
                        _ -> Outcome
                    end
                end}
                | Conf
            ]),
            ok
        end,
        fun(_) ->
            application:unset_env(imboy, moderation_detector)
        end,
        fun() -> TestFun() end}.

-spec recv_grpc_call() -> term().
recv_grpc_call() ->
    receive
        {t15_grpc_call, Captured} -> Captured
    after 2000 ->
        erlang:error({capture_timeout, t15_grpc_call})
    end.

-spec first_request(term()) -> map().
first_request(H) ->
    case ewh_http_fixture:requests(H) of
        [Req | _] -> Req;
        [] -> erlang:error(no_request_captured)
    end.

%% 夹具的 headers 字段恒为 []（split_once 返回 list 而 comprehension 匹配
%% tuple 的存量缺陷，见 T15 报告）；这里直接从 raw 解析目标头。
-spec header(term(), binary()) -> binary() | undefined.
header(Req, Name) ->
    Raw = maps:get(raw, Req),
    [HeadBin | _] = binary:split(Raw, <<"\r\n\r\n">>),
    [_RequestLine | HeaderLines] = binary:split(HeadBin, <<"\r\n">>, [global]),
    case [V || L <- HeaderLines, [K, V] <- [split_header_line(L)], lc(K) =:= Name] of
        [Value | _] -> trim_space(Value);
        [] -> undefined
    end.

-spec split_header_line(binary()) -> [binary()].
split_header_line(L) ->
    case binary:split(L, <<":">>) of
        [A, B] -> [A, B];
        [A] -> [A, <<>>]
    end.

-spec lc(binary()) -> binary().
lc(B) ->
    list_to_binary(string:lowercase(binary_to_list(B))).

-spec trim_space(binary()) -> binary().
trim_space(B) ->
    list_to_binary(string:trim(binary_to_list(B), leading)).

%% ===================================================================
%% 生成器
%% ===================================================================

moderation_detector_transport_test_() ->
    [
        %% -- HTTP：happy path 与请求线格式 ------------------------------
        http_check_happy_path_block(),
        http_check_happy_path_allow_review(),
        http_check_request_shape(),
        http_two_checks_reopen_after_server_close(),
        %% -- HTTP：200 故障体 → protocol_error（绝不 allow）--------------
        http_bad_body_table(),
        http_non_utf8_body(),
        http_oversized_shape_valid_body(),
        %% -- HTTP：非 200 错误面映射 ------------------------------------
        http_error_envelope_table(),
        http_error_status_only_table(),
        http_error_envelope_malformed_falls_back_to_status(),
        %% -- HTTP：deadline / 零重试 / 连接失败 -------------------------
        http_deadline_timeout_zero_retry(),
        http_ctx_deadline_overrides_config(),
        http_closed_without_response(),
        http_connection_refused(),
        %% -- HTTP：配置 fail-closed -------------------------------------
        http_config_not_ready(),
        %% -- HTTP：原子注入不变量 ---------------------------------------
        http_atom_injection_never_creates_atoms(),
        %% -- HTTP：info 面 ----------------------------------------------
        http_info_happy_path(),
        http_info_bad_body_table(),
        %% -- gRPC：注入 transport fun 面 --------------------------------
        grpc_check_success_and_contract(),
        grpc_ctx_deadline_passthrough(),
        grpc_mfa_transport_resolution(),
        grpc_bad_response_table(),
        grpc_status_code_table(),
        grpc_transport_bad_shape_or_crash(),
        grpc_info_face(),
        grpc_config_not_ready(),
        %% -- behaviour：选择器与 legacy 标记 -----------------------------
        behaviour_select_and_legacy(),
        behaviour_whitelist_closed_set()
    ].

%% ===================================================================
%% HTTP：happy path 与请求线格式
%% ===================================================================

http_check_happy_path_block() ->
    with_http([], reply(<<"/v1/check">>, 200, check_ok_body()), fun(_H) ->
        ?assertEqual(
            {ok, #{decision => block, scan_complete => true}},
            check()
        )
    end).

http_check_happy_path_allow_review() ->
    with_http(
        [],
        reply(
            <<"/v1/check">>,
            200,
            <<"{\"decision\":\"review\",\"scan_complete\":false}">>
        ),
        fun(_H) ->
            D = maps:put(scene, moment_post, det()),
            ?assertEqual(
                {ok, #{decision => review, scan_complete => false}},
                ?HTTP_ADAPTER:check(D, #{})
            ),
            D2 = maps:put(scene, profile_field, det()),
            ?assertEqual(
                {ok, #{decision => review, scan_complete => false}},
                ?HTTP_ADAPTER:check(D2, #{})
            )
        end
    ).

http_check_request_shape() ->
    with_http([], reply(<<"/v1/check">>, 200, check_ok_body()), fun(H) ->
        ?assertEqual({ok, #{decision => block, scan_complete => true}}, check()),
        Req = first_request(H),
        ?assertEqual(<<"POST">>, maps:get(method, Req)),
        ?assertEqual(<<"/v1/check">>, maps:get(path, Req)),
        ?assertEqual(<<"Bearer ", ?TOKEN/binary>>, header(Req, <<"authorization">>)),
        ?assertEqual(<<"application/json">>, header(Req, <<"content-type">>)),
        Json = jsone:decode(maps:get(body, Req)),
        ?assertEqual(?REQ_ID, maps:get(<<"request_id">>, Json)),
        ?assertEqual(<<"channel_message">>, maps:get(<<"scene">>, Json)),
        ?assertEqual(
            [
                #{<<"field_id">> => <<"body">>, <<"text">> => <<"你好 textguard"/utf8>>},
                #{<<"field_id">> => <<"nickname">>, <<"text">> => <<"bob">>}
            ],
            maps:get(<<"fields">>, Json)
        )
    end).

%% 对端每响应即关连接（夹具 connection: close）：同进程连续两次检查
%% 均成功——连接缓存失效后按需重建，无请求级泄漏累积。
http_two_checks_reopen_after_server_close() ->
    with_http([], reply(<<"/v1/check">>, 200, check_ok_body()), fun(H) ->
        ?assertMatch({ok, _}, check()),
        ?assertMatch({ok, _}, check()),
        ?assertEqual(2, ewh_http_fixture:request_count(H))
    end).

%% ===================================================================
%% HTTP：200 故障体 → protocol_error（AC-060：绝不折算 allow）
%% ===================================================================

http_bad_body_table() ->
    Cases = [
        {missing_decision, <<"{\"scan_complete\":true}">>},
        {missing_scan_complete, <<"{\"decision\":\"allow\"}">>},
        {unknown_top_level_key,
            <<"{\"decision\":\"allow\",\"scan_complete\":true,\"surprise\":1}">>},
        {scan_complete_wrong_type, <<"{\"decision\":\"allow\",\"scan_complete\":\"yes\"}">>},
        {decision_wrong_type, <<"{\"decision\":42,\"scan_complete\":true}">>},
        {unknown_decision_value, <<"{\"decision\":\"definitely\",\"scan_complete\":true}">>},
        {truncated_json, <<"{\"decision\":\"all">>},
        {empty_body, <<>>},
        {garbage_bytes, <<0, 1, 2, 255, 254>>},
        {not_a_json_object, <<"[\"allow\",true]">>},
        {error_envelope_on_200, <<"{\"error\":{\"code\":\"OVERLOADED\"}}">>}
    ],
    [
        {
            lists:flatten(io_lib:format("http_bad_body_~p", [Name])),
            with_http([], reply(<<"/v1/check">>, 200, Body), fun(_H) ->
                ?assertEqual({error, protocol_error}, check())
            end)
        }
     || {Name, Body} <- Cases
    ].

http_non_utf8_body() ->
    %% jsone 对非 UTF-8 字节直通（实测），适配器必须自行拦截
    Body = <<"{\"decision\":\"", 16#FF, 16#FE, "\",\"scan_complete\":true}">>,
    with_http([], reply(<<"/v1/check">>, 200, Body), fun(_H) ->
        ?assertEqual({error, protocol_error}, check())
    end).

%% 上限先于形状校验：前导空白（JSON 合法）+ 合法形状，但体超
%% max_response_bytes —— 必须按超限拒绝，而不是放行为 allow。
http_oversized_shape_valid_body() ->
    Pad = binary:copy(<<" ">>, 4096),
    Body = <<Pad/binary, "{\"decision\":\"allow\",\"scan_complete\":true}">>,
    with_http([{max_response_bytes, 256}], reply(<<"/v1/check">>, 200, Body), fun(_H) ->
        ?assertEqual({error, protocol_error}, check())
    end).

%% ===================================================================
%% HTTP：非 200 错误面映射（信封优先，其次状态码表）
%% ===================================================================

http_error_envelope_table() ->
    Cases = [
        {401, <<"UNAUTHENTICATED">>, unauthenticated},
        {403, <<"PERMISSION_DENIED">>, permission_denied},
        {413, <<"TEXT_TOO_LARGE">>, text_too_large},
        {503, <<"OVERLOADED">>, overloaded},
        {500, <<"NOT_READY">>, not_ready},
        %% 信封未知 code：白名单查表回落 internal（不按状态码改判）
        {503, <<"TOTALLY_UNKNOWN_CODE">>, internal},
        {401, <<"SOME_EVIL_ATOM">>, internal}
    ],
    [
        {
            lists:flatten(io_lib:format("http_envelope_~p_~s", [Status, Code])),
            with_http([], envelope_reply(<<"/v1/check">>, Status, Code), fun(_H) ->
                ?assertEqual({error, Reason}, check())
            end)
        }
     || {Status, Code, Reason} <- Cases
    ].

http_error_status_only_table() ->
    Cases = [
        {401, unauthenticated},
        {403, permission_denied},
        {413, text_too_large},
        {429, overloaded},
        {503, overloaded},
        {504, timeout},
        {500, internal},
        {404, internal}
    ],
    [
        {
            lists:flatten(io_lib:format("http_status_~p", [Status])),
            with_http([], reply(<<"/v1/check">>, Status, <<>>), fun(_H) ->
                ?assertEqual({error, Reason}, check())
            end)
        }
     || {Status, Reason} <- Cases
    ].

%% 信封体损坏（非 JSON）→ 视为无信封，回落状态码表。
http_error_envelope_malformed_falls_back_to_status() ->
    with_http([], reply(<<"/v1/check">>, 503, <<"not json at all">>), fun(_H) ->
        ?assertEqual({error, overloaded}, check())
    end).

-spec envelope_reply(binary(), integer(), binary()) -> fun().
envelope_reply(Path, Status, Code) ->
    Body = <<"{\"error\":{\"code\":\"", Code/binary, "\",\"message\":\"m\"}}">>,
    reply(Path, Status, Body).

%% ===================================================================
%% HTTP：deadline / 零重试 / 连接失败
%% ===================================================================

%% 真挂死对端（夹具 responder 永不返回 → socket 保持打开）：总预算封顶
%% 等待 → timeout；且**零重试**——服务端恰被命中 1 次。
http_deadline_timeout_zero_retry() ->
    BlockForever = fun(_Req) ->
        receive
            never -> ok
        end
    end,
    with_http([{timeout_ms, 200}], BlockForever, fun(H) ->
        T0 = erlang:monotonic_time(millisecond),
        Result = check(),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assertEqual({error, timeout}, Result),
        %% 预算被真正用满（而非瞬间失败），且被 200ms 封顶
        ?assert(Elapsed >= 100),
        ?assert(Elapsed =< 2000),
        ?assertEqual(1, ewh_http_fixture:request_count(H))
    end).

%% 请求被收下后对端不回即断连（夹具 hang：读请求→关 socket）→
%% connection_failed（区别于预算耗尽的 timeout），且零重试恰命中 1 次。
http_closed_without_response() ->
    with_http([], fun(_Req) -> hang end, fun(H) ->
        ?assertEqual({error, connection_failed}, check()),
        ?assertEqual(1, ewh_http_fixture:request_count(H))
    end).

%% Ctx 显式 deadline 覆盖配置：慢 120ms 的对端在 30ms 配置下超时、
%% 在 900ms 的调用级预算下成功。
http_ctx_deadline_overrides_config() ->
    Slow = fun(_Req) ->
        timer:sleep(120),
        {reply, 200, [], check_ok_body()}
    end,
    [
        with_http([{timeout_ms, 30}], Slow, fun(_H) ->
            ?assertEqual({error, timeout}, check())
        end),
        with_http([{timeout_ms, 30}], Slow, fun(_H) ->
            ?assertEqual(
                {ok, #{decision => block, scan_complete => true}},
                ?HTTP_ADAPTER:check(det(), #{deadline_ms => 900})
            )
        end)
    ].

http_connection_refused() ->
    {setup,
        fun() ->
            %% 取一个已释放的本地端口：无监听 → 连接层失败
            {ok, S} = gen_tcp:listen(0, [binary, {reuseaddr, true}]),
            {ok, Port} = inet:port(S),
            ok = gen_tcp:close(S),
            PortBin = integer_to_binary(Port),
            application:set_env(imboy, moderation_detector, [
                {transport, http},
                {http_base_url, <<"http://127.0.0.1:", PortBin/binary>>},
                {token, ?TOKEN}
            ]),
            Port
        end,
        fun(_) ->
            application:unset_env(imboy, moderation_detector),
            clean_gun_cache()
        end,
        fun(_) ->
            fun() ->
                ?assertEqual({error, connection_failed}, check()),
                ?assertEqual({error, connection_failed}, ?HTTP_ADAPTER:info(#{}))
            end
        end}.

%% ===================================================================
%% HTTP：配置 fail-closed（缺配置 = not_ready，绝不静默放行）
%% ===================================================================

http_config_not_ready() ->
    {setup, fun() -> application:unset_env(imboy, moderation_detector) end,
        fun(_) -> application:unset_env(imboy, moderation_detector) end, fun() ->
            %% 完全缺配置
            ?assertEqual({error, not_ready}, check()),
            ?assertEqual({error, not_ready}, ?HTTP_ADAPTER:info(#{})),
            %% 缺 token
            application:set_env(imboy, moderation_detector, [
                {transport, http}, {http_base_url, <<"http://127.0.0.1:1">>}
            ]),
            ?assertEqual({error, not_ready}, check()),
            %% 缺 base_url
            application:set_env(imboy, moderation_detector, [
                {transport, http}, {token, ?TOKEN}
            ]),
            ?assertEqual({error, not_ready}, check()),
            %% base_url 非法（不可解析 / 非 http 方案）
            application:set_env(imboy, moderation_detector, [
                {transport, http},
                {http_base_url, <<"::::">>},
                {token, ?TOKEN}
            ]),
            ?assertEqual({error, not_ready}, check()),
            application:set_env(imboy, moderation_detector, [
                {transport, http},
                {http_base_url, <<"ftp://127.0.0.1:1">>},
                {token, ?TOKEN}
            ]),
            ?assertEqual({error, not_ready}, check()),
            ok
        end}.

%% ===================================================================
%% HTTP：远端原子注入不变量（闭集原子；远端串永不 intern 成原子）
%% ===================================================================

http_atom_injection_never_creates_atoms() ->
    CodeOf =
        fun() -> persistent_term:get({?MODULE, evil_code}, <<"warmup-code">>) end,
    Responder = fun(_Req) ->
        Code = CodeOf(),
        {reply, 503, [], <<"{\"error\":{\"code\":\"", Code/binary, "\"}}">>}
    end,
    DecisionOf =
        fun() -> persistent_term:get({?MODULE, evil_decision}, <<"warmup-decision">>) end,
    DecisionResponder = fun(_Req) ->
        Dec = DecisionOf(),
        {reply, 200, [], <<"{\"decision\":\"", Dec/binary, "\",\"scan_complete\":true}">>}
    end,
    EnvelopeCase = with_http([], Responder, fun(_H) ->
        %% 预热（枪/解码器惰性原子落位）
        persistent_term:put({?MODULE, evil_code}, <<"warmup-code-1">>),
        ?assertEqual({error, internal}, check()),
        %% 正式测量：未知 code 串 → internal，且不创建任何新原子
        persistent_term:put({?MODULE, evil_code}, <<"evil_atom_probe_t15_code">>),
        Count0 = erlang:system_info(atom_count),
        ?assertEqual({error, internal}, check()),
        Count1 = erlang:system_info(atom_count),
        ?assertEqual(Count0, Count1)
    end),
    DecisionCase = with_http([], DecisionResponder, fun(_H) ->
        persistent_term:put({?MODULE, evil_decision}, <<"warmup-decision-1">>),
        ?assertEqual({error, protocol_error}, check()),
        persistent_term:put({?MODULE, evil_decision}, <<"evil_atom_probe_t15_decision">>),
        Count0 = erlang:system_info(atom_count),
        ?assertEqual({error, protocol_error}, check()),
        Count1 = erlang:system_info(atom_count),
        ?assertEqual(Count0, Count1)
    end),
    {setup, fun() -> ok end,
        fun(_) ->
            persistent_term:erase({?MODULE, evil_code}),
            persistent_term:erase({?MODULE, evil_decision})
        end,
        [EnvelopeCase, DecisionCase]}.

%% ===================================================================
%% HTTP：info 面
%% ===================================================================

http_info_happy_path() ->
    Paths = [
        {<<"/v1/check">>, 200, check_ok_body()},
        {<<"/v1/info">>, 200, <<"{\"version\":\"0.2.1\",\"ready\":true}">>}
    ],
    Responder = fun(Req) ->
        Path = maps:get(path, Req),
        case lists:keyfind(Path, 1, Paths) of
            {_, Code, Body} -> {reply, Code, [], Body};
            false -> {reply, 404, [], <<"nope">>}
        end
    end,
    with_http([], Responder, fun(H) ->
        ?assertEqual(
            {ok, #{version => <<"0.2.1">>, ready => true}},
            ?HTTP_ADAPTER:info(#{})
        ),
        Req = first_request(H),
        ?assertEqual(<<"GET">>, maps:get(method, Req)),
        ?assertEqual(<<"/v1/info">>, maps:get(path, Req)),
        ?assertEqual(<<"Bearer ", ?TOKEN/binary>>, header(Req, <<"authorization">>))
    end).

http_info_bad_body_table() ->
    Cases = [
        {missing_ready, <<"{\"version\":\"0.2.1\"}">>},
        {unknown_key, <<"{\"version\":\"0.2.1\",\"ready\":true,\"x\":1}">>},
        {garbage, <<0, 255, 254>>},
        {truncated, <<"{\"version\":">>}
    ],
    [
        {
            lists:flatten(io_lib:format("http_info_bad_~p", [Name])),
            with_http([], reply(<<"/v1/info">>, 200, Body), fun(_H) ->
                ?assertEqual({error, protocol_error}, ?HTTP_ADAPTER:info(#{}))
            end)
        }
     || {Name, Body} <- Cases
    ].

%% ===================================================================
%% gRPC：注入 transport fun 面
%% ===================================================================

grpc_check_success_and_contract() ->
    Outcome = {ok, #{decision => <<"allow">>, scan_complete => true}},
    with_grpc([], Outcome, fun() ->
        ?assertEqual(
            {ok, #{decision => allow, scan_complete => true}},
            ?GRPC_ADAPTER:check(det(), #{})
        ),
        {Endpoint, Method, Request, Deadline} = recv_grpc_call(),
        %% 端点来自配置
        ?assertEqual(#{host => <<"127.0.0.1">>, port => 7464}, Endpoint),
        %% proto 方法名（CheckText 的 snake_case）
        ?assertEqual(check_text, Method),
        %% proto 字段名（snake_case 原子键）+ 显式文本
        ?assertEqual(
            #{
                request_id => ?REQ_ID,
                scene => <<"channel_message">>,
                fields => [
                    #{field_id => <<"body">>, text => <<"你好 textguard"/utf8>>},
                    #{field_id => <<"nickname">>, text => <<"bob">>}
                ]
            },
            Request
        ),
        %% 默认总预算（plan §5：150ms）
        ?assertEqual(150, Deadline)
    end).

grpc_ctx_deadline_passthrough() ->
    Outcome = {ok, #{decision => <<"block">>, scan_complete => true}},
    with_grpc([{timeout_ms, 250}], Outcome, fun() ->
        ?assertMatch(
            {ok, _},
            ?GRPC_ADAPTER:check(det(), #{deadline_ms => 77})
        ),
        {_, _, _, Deadline} = recv_grpc_call(),
        ?assertEqual(77, Deadline)
    end).

%% {M, F} 间接注入（sys.config 可表达的形态）。
grpc_mfa_transport_resolution() ->
    {setup,
        fun() ->
            application:set_env(imboy, moderation_detector, [
                {transport, grpc},
                {grpc_host, <<"127.0.0.1">>},
                {grpc_port, 7464},
                {grpc_transport, {?MODULE, grpc_mfa_stub}}
            ]),
            ok
        end,
        fun(_) -> application:unset_env(imboy, moderation_detector) end, fun() ->
            ?assertEqual(
                {ok, #{decision => block, scan_complete => true}},
                ?GRPC_ADAPTER:check(det(), #{})
            )
        end}.

-spec grpc_mfa_stub(map(), check_text | get_info, map(), pos_integer()) ->
    {ok, map()}.
grpc_mfa_stub(_Endpoint, check_text, _Request, _Deadline) ->
    {ok, #{decision => <<"block">>, scan_complete => true}};
grpc_mfa_stub(_Endpoint, get_info, _Request, _Deadline) ->
    {ok, #{version => <<"0.2.1">>, ready => true}}.

grpc_bad_response_table() ->
    Cases = [
        {missing_scan_complete, {ok, #{decision => <<"allow">>}}, protocol_error},
        {unknown_decision, {ok, #{decision => <<"bogus">>, scan_complete => true}}, protocol_error},
        {scan_complete_wrong_type, {ok, #{decision => <<"allow">>, scan_complete => 1}},
            protocol_error},
        {decision_wrong_type, {ok, #{decision => allow, scan_complete => true}}, protocol_error}
    ],
    [
        {
            lists:flatten(io_lib:format("grpc_bad_resp_~p", [Name])),
            with_grpc([], Outcome, fun() ->
                ?assertEqual({error, Reason}, ?GRPC_ADAPTER:check(det(), #{}))
            end)
        }
     || {Name, Outcome, Reason} <- Cases
    ].

grpc_status_code_table() ->
    Cases = [
        {4, #{}, timeout},
        {8, #{}, overloaded},
        {8, #{<<"textguard-error-code">> => <<"OVERLOADED">>}, overloaded},
        {8, #{<<"textguard-error-code">> => <<"TEXT_TOO_LARGE">>}, text_too_large},
        {8, #{<<"textguard-error-code">> => [<<"TEXT_TOO_LARGE">>]}, text_too_large},
        {8, #{<<"textguard-error-code">> => <<"WHATEVER_ELSE">>}, overloaded},
        {11, #{}, text_too_large},
        {16, #{}, unauthenticated},
        {7, #{}, permission_denied},
        {14, #{}, connection_failed},
        {13, #{}, internal},
        {2, #{}, internal},
        {1, #{}, internal}
    ],
    [
        {
            lists:flatten(io_lib:format("grpc_status_~p_meta_~p", [Status, maps:size(Meta)])),
            with_grpc(
                [],
                {error, #{grpc_status => Status, message => <<"m">>, metadata => Meta}},
                fun() ->
                    ?assertEqual({error, Reason}, ?GRPC_ADAPTER:check(det(), #{}))
                end
            )
        }
     || {Status, Meta, Reason} <- Cases
    ].

grpc_transport_bad_shape_or_crash() ->
    %% 成功分支的坏载荷走响应校验 → protocol_error；错误分支的坏形状/崩溃
    %% → internal（fail-closed）。
    Cases = [
        {ok_return_not_a_map, {ok, <<"junk">>}, protocol_error},
        {bare_ok, ok, internal},
        {error_not_a_map, {error, <<"grpc-ish">>}, internal},
        {error_map_without_status, {error, #{message => <<"m">>}}, internal}
    ],
    ShapeCases = [
        {
            lists:flatten(io_lib:format("grpc_bad_shape_~p", [Name])),
            with_grpc([], Outcome, fun() ->
                ?assertEqual({error, Reason}, ?GRPC_ADAPTER:check(det(), #{}))
            end)
        }
     || {Name, Outcome, Reason} <- Cases
    ],
    CrashCase = with_grpc([], crash, fun() ->
        %% 传输崩溃 → fail-closed internal（不重试、不上抛崩溃）
        ?assertEqual({error, internal}, ?GRPC_ADAPTER:check(det(), #{}))
    end),
    ShapeCases ++ [CrashCase].

grpc_info_face() ->
    [
        with_grpc([], {ok, #{version => <<"0.2.1">>, ready => true}}, fun() ->
            ?assertEqual(
                {ok, #{version => <<"0.2.1">>, ready => true}},
                ?GRPC_ADAPTER:info(#{})
            ),
            {_, Method, Request, _Deadline} = recv_grpc_call(),
            ?assertEqual(get_info, Method),
            ?assertEqual(#{}, Request)
        end),
        with_grpc([], {ok, #{version => <<"0.2.1">>}}, fun() ->
            ?assertEqual({error, protocol_error}, ?GRPC_ADAPTER:info(#{}))
        end),
        with_grpc([], {error, #{grpc_status => 14}}, fun() ->
            ?assertEqual({error, connection_failed}, ?GRPC_ADAPTER:info(#{}))
        end)
    ].

grpc_config_not_ready() ->
    {setup, fun() -> ok end, fun(_) -> application:unset_env(imboy, moderation_detector) end,
        fun() ->
            %% 缺 transport fun
            application:set_env(imboy, moderation_detector, [
                {transport, grpc}, {grpc_host, <<"127.0.0.1">>}, {grpc_port, 7464}
            ]),
            ?assertEqual({error, not_ready}, ?GRPC_ADAPTER:check(det(), #{})),
            %% 缺 host/port
            application:set_env(imboy, moderation_detector, [
                {transport, grpc},
                {grpc_transport, fun(_, _, _, _) -> {ok, #{}} end}
            ]),
            ?assertEqual({error, not_ready}, ?GRPC_ADAPTER:check(det(), #{})),
            %% 非法端口
            application:set_env(imboy, moderation_detector, [
                {transport, grpc},
                {grpc_host, <<"127.0.0.1">>},
                {grpc_port, 0},
                {grpc_transport, fun(_, _, _, _) -> {ok, #{}} end}
            ]),
            ?assertEqual({error, not_ready}, ?GRPC_ADAPTER:check(det(), #{})),
            ok
        end}.

%% ===================================================================
%% behaviour：选择器与 legacy 标记
%% ===================================================================

behaviour_select_and_legacy() ->
    {setup, fun() -> application:unset_env(imboy, moderation_detector) end,
        fun(_) -> application:unset_env(imboy, moderation_detector) end, fun() ->
            %% 未配置 → legacy（零远程调用标记）
            ?assertEqual(?BEHAVIOUR, ?BEHAVIOUR:select()),
            ?assertEqual({ok, legacy}, ?BEHAVIOUR:legacy_check(det(), #{})),
            ?assertEqual({ok, legacy}, ?BEHAVIOUR:legacy_info(#{})),
            %% 显式 legacy
            application:set_env(imboy, moderation_detector, [{transport, legacy}]),
            ?assertEqual(?BEHAVIOUR, ?BEHAVIOUR:select()),
            %% http / grpc
            application:set_env(imboy, moderation_detector, [{transport, http}]),
            ?assertEqual(?HTTP_ADAPTER, ?BEHAVIOUR:select()),
            application:set_env(imboy, moderation_detector, [{transport, grpc}]),
            ?assertEqual(?GRPC_ADAPTER, ?BEHAVIOUR:select()),
            %% 未知值 → fail-safe 回落 legacy
            application:set_env(imboy, moderation_detector, [{transport, carrier_pigeon}]),
            ?assertEqual(?BEHAVIOUR, ?BEHAVIOUR:select()),
            ok
        end}.

behaviour_whitelist_closed_set() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        ?assertEqual(
            {ok, <<"channel_message">>},
            ?BEHAVIOUR:scene_to_binary(channel_message)
        ),
        ?assertEqual(
            {ok, <<"moment_post">>},
            ?BEHAVIOUR:scene_to_binary(moment_post)
        ),
        ?assertEqual(
            {ok, <<"profile_field">>},
            ?BEHAVIOUR:scene_to_binary(profile_field)
        ),
        ?assertEqual(error, ?BEHAVIOUR:scene_to_binary(bogus_scene)),
        ?assertEqual({ok, allow}, ?BEHAVIOUR:decision_to_atom(<<"allow">>)),
        ?assertEqual({ok, block}, ?BEHAVIOUR:decision_to_atom(<<"block">>)),
        ?assertEqual({ok, review}, ?BEHAVIOUR:decision_to_atom(<<"review">>)),
        ?assertEqual(error, ?BEHAVIOUR:decision_to_atom(<<"ALLOW">>)),
        ?assertEqual(error, ?BEHAVIOUR:decision_to_atom(<<"evil_probe">>)),
        ok
    end}.
