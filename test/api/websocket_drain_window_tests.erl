-module(websocket_drain_window_tests).

%%%===================================================================
%%% @doc T02：WS ACK 大小写兼容 + 跨 drain 窗口行为。
%%%
%%% 任务3（ACK 大小写）：wire 规范形态是大写 "CLIENT_ACK,TYPE,..."
%%% （imboyapp ack_manager generateAckMessage 全量发送大写），但前缀与
%%% type 只认一种大小写会把变体帧误入业务消息路径（JSON 解码失败回
%%% invalid_json）。这里钉住：前缀大小写不敏感、type 大小写不敏感、
%%% 回执语义（CLIENT_ACK_CONFIRM / CLIENT_ACK_ERROR）不变。
%%%
%%% 任务4（跨 drain 窗口）：
%%%   - 新 WS 握手（HTTP 升级请求）在 draining 时被 lifecycle_middleware
%%%     拒绝（503），websocket_handler 不在豁免名单；
%%%   - 已连接客户端在 draining 时新业务写被 close 1012 service_restart
%%%     （可重连退出），而最后 ACK（CLIENT_ACK 帧）仍被完整处理——
%%%     持久化确认 + CONFIRM 回执不受 drain 影响。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").
-include("imboy_frame.hrl").

%% 公共依赖 mock：认证、ACK 持久化链、日志。全部 no_link；
%% 未列出的函数 passthrough 到真实实现。
base_mocks() ->
    [
        {auth_ds, [
            {'current_uid', 1, fun(#{current_uid := Uid}) -> Uid end}
        ]},
        {msg_s2c_logic, [
            {'s2c_client_ack', 3, fun(_MsgId, _Uid, _Did) -> ok end}
        ]},
        {msg_c2c_logic, [
            {'c2c_client_ack', 3, fun(_MsgId, _Uid, _Did) -> ok end}
        ]},
        {websocket_logic, [
            {'cancel_timer', 3, fun(_Uid, _Did, _MsgId) -> ok end}
        ]},
        {throttle, [
            {'check', 2, fun(_Scope, _Key) -> ok end}
        ]},
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _M, _L) -> ok end},
            {'internal_log', 5, fun(_Level, _Fmt, _Args, _M, _L) -> ok end}
        ]}
    ].

ack_state() ->
    #{
        did => <<"did1">>,
        dtype => <<"ios">>,
        current_uid => 42,
        protocol => json,
        framing => none
    }.

v2_state() ->
    S = ack_state(),
    S#{protocol => protobuf, framing => v2}.

with_mocks(Extra, Fun) ->
    Configs = base_mocks() ++ Extra,
    [
        begin
            {ok, _} = meck_helper:setup_mock(M, Expects),
            ok
        end
     || {M, Expects} <- Configs
    ],
    try
        Fun()
    after
        [meck_helper:cleanup_mock(M) || {M, _} <- Configs]
    end.

%% 直调 cowboy 回调（合法的单元级入口：帧处理不依赖 socket 本体）。
text_frame(MsgBin, State) ->
    websocket_handler:websocket_handle({text, MsgBin}, State).

confirm_type({reply, {text, Bin}, _, _}) ->
    #{<<"type">> := Type, <<"action">> := Action} = jsone:decode(Bin),
    {Type, Action}.

%%%===================================================================
%%% 任务3：ACK 前缀 / type 大小写兼容
%%%===================================================================

ack_prefix_case_insensitive_test_() ->
    [
        ?_test(check_ack_prefix(Prefix))
     || Prefix <-
            [<<"CLIENT_ACK,">>, <<"client_ack,">>, <<"Client_Ack,">>, <<"cLiEnT_aCk,">>]
    ].

check_ack_prefix(Prefix) ->
    with_mocks([], fun() ->
        Frame = <<Prefix/binary, "S2C,t02mid1,did1">>,
        Result = text_frame(Frame, ack_state()),
        ?assertEqual(
            {<<"CLIENT_ACK_CONFIRM">>, <<"CLIENT_ACK_CONFIRM">>},
            confirm_type(Result)
        ),
        %% ACK 不走业务限流（变体帧不得误入 throttle 路径）
        ?assertEqual(0, meck:num_calls(throttle, check, 2)),
        %% 持久化确认恰好发生一次
        ?assertEqual(1, meck:num_calls(msg_s2c_logic, s2c_client_ack, 3))
    end).

ack_type_case_insensitive_test_() ->
    [
        ?_test(check_ack_type(Type))
     || Type <- [<<"S2C">>, <<"s2c">>, <<"S2c">>, <<"C2C">>, <<"c2c">>, <<"WEBRTC">>, <<"webrtc">>]
    ].

check_ack_type(Type) ->
    with_mocks([], fun() ->
        Frame = <<"client_ack,", Type/binary, ",t02mid2,did1">>,
        Result = text_frame(Frame, ack_state()),
        ?assertEqual(
            {<<"CLIENT_ACK_CONFIRM">>, <<"CLIENT_ACK_CONFIRM">>},
            confirm_type(Result)
        )
    end).

ack_unknown_type_rejected_test() ->
    with_mocks([], fun() ->
        Frame = <<"client_ack,s2x,t02mid3,did1">>,
        Result = text_frame(Frame, ack_state()),
        {Type, _} = confirm_type(Result),
        ?assertEqual(<<"CLIENT_ACK_ERROR">>, Type)
    end).

%% validate_ack_params 合同（经 CLIENT_ACK_ERROR 的 reason 间接断言，
%% 不给生产代码开 -ifdef(TEST) 后门）：type 大小写归一后，其余规则
%% （msgid 非空、did 匹配、type 合法集合）不变。
validate_ack_params_case_test_() ->
    State = ack_state(),
    %% 空 msgid：任意大小写的合法 type 都必须按 invalid_msgid 拒绝
    %% （先过 type 归一，再过参数检查——归一不得改变参数校验次序语义）。
    [
        ?_test(begin
            Frame = <<"CLIENT_ACK,", Type/binary, ",,did1">>,
            {reply, {text, Bin}, _, _} = text_frame(Frame, State),
            #{<<"type">> := <<"CLIENT_ACK_ERROR">>, <<"reason">> := Reason} = jsone:decode(Bin),
            ?assertEqual(<<"invalid_msgid">>, Reason)
        end)
     || Type <-
            [
                <<"C2C">>,
                <<"c2c">>,
                <<"C2G">>,
                <<"c2g">>,
                <<"S2C">>,
                <<"s2c">>,
                <<"C2S">>,
                <<"c2s">>,
                <<"WEBRTC">>,
                <<"webrtc">>
            ]
    ] ++
        [
            ?_test(begin
                Frame = <<"CLIENT_ACK,", Type/binary, ",t02msg,", Did/binary>>,
                {reply, {text, Bin}, _, _} = text_frame(Frame, State),
                #{<<"type">> := <<"CLIENT_ACK_ERROR">>, <<"reason">> := Reason} = jsone:decode(Bin),
                ?assertEqual(Expect, Reason)
            end)
         || {Type, Did, Expect} <-
                [
                    {<<"xx">>, <<"did1">>, <<"invalid_type">>},
                    {<<"s2x">>, <<"did1">>, <<"invalid_type">>},
                    {<<"S2C">>, <<"did2">>, <<"did_mismatch">>},
                    {<<"s2c">>, <<"did2">>, <<"did_mismatch">>},
                    {<<"S2C">>, <<>>, <<"invalid_did">>}
                ]
        ].

%% v2 framing：MSG_C2S 帧内的文本 CLIENT_ACK payload 同样大小写不敏感。
v2_c2s_client_ack_case_insensitive_test_() ->
    [
        ?_test(check_v2_ack(Prefix))
     || Prefix <-
            [<<"CLIENT_ACK,">>, <<"client_ack,">>, <<"Client_Ack,">>]
    ].

check_v2_ack(Prefix) ->
    with_mocks([], fun() ->
        Payload = <<Prefix/binary, "s2c,t02mid4,did1">>,
        Frame = imboy_codec:wrap_v2_frame(?FRAME_TYPE_MSG_C2S, 0, Payload),
        {reply, {binary, V2Bin}, _, _} =
            websocket_handler:websocket_handle({binary, Frame}, v2_state()),
        %% v2 回执经 imboy_frame 包裹，payload 为 JSON 文本
        {ok, Decoded} = imboy_codec:unwrap_v2_frame(V2Bin),
        Inner = imboy_frame:payload(Decoded),
        #{<<"type">> := Type} = jsone:decode(Inner),
        ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, Type),
        ?assertEqual(1, meck:num_calls(msg_s2c_logic, s2c_client_ack, 3))
    end).

%% 分派正确性：小写 type 必须落进对应 logic（c2c→c2c_client_ack），
%% 而不是被 unknown 分支吞掉。
ack_type_routes_to_correct_logic_test() ->
    with_mocks([], fun() ->
        _ = text_frame(<<"client_ack,c2c,t02mid5,did1">>, ack_state()),
        ?assertEqual(1, meck:num_calls(msg_c2c_logic, c2c_client_ack, 3)),
        ?assertEqual(0, meck:num_calls(msg_s2c_logic, s2c_client_ack, 3))
    end).

%%%===================================================================
%%% 任务4：跨 drain 窗口
%%%===================================================================

%% 新 WS 握手在 draining 时被 lifecycle_middleware 拒绝：握手是普通 HTTP
%% 升级请求，lifecycle 中间件在 auth 之前，因此无需有效 token 即被拒。
%% websocket_handler / healthz_handler 都用真实模块名（不构造桩值）。
handshake_rejected_during_drain_test() ->
    Prev = lifecycle_env(),
    ok = application:set_env(imboy, lifecycle, draining),
    meck_helper_drain(fun() ->
        ?assertEqual(
            {stop, handshake_rejected},
            lifecycle_middleware:execute(fake_req, #{handler => websocket_handler})
        ),
        %% 豁免名单不含 websocket_handler：healthz/metrics 照常放行（探针
        %% 必须能在 drain 期上报 503，否则负载均衡无法感知摘除进度）。
        ?assertEqual(
            {ok, fake_req, #{handler => healthz_handler}},
            lifecycle_middleware:execute(fake_req, #{handler => healthz_handler})
        ),
        ?assertEqual(
            {ok, fake_req, #{handler => metrics_handler}},
            lifecycle_middleware:execute(fake_req, #{handler => metrics_handler})
        )
    end),
    restore_lifecycle(Prev).

meck_helper_drain(Fun) ->
    Mocks = [
        {imboy_app, [
            {'readiness', 0, fun() -> draining end}
        ]},
        {elib_response, [
            {'error_with_status', 4, fun(_Req, 503, _Msg, 503) -> handshake_rejected end}
        ]}
    ],
    [
        begin
            {ok, _} = meck_helper:setup_mock(M, E),
            ok
        end
     || {M, E} <- Mocks
    ],
    try
        Fun()
    after
        [meck_helper:cleanup_mock(M) || {M, _} <- Mocks]
    end.

%% 已连接客户端：draining 中新业务写（C2C JSON 消息）→ close 1012
%% service_restart（可重连退出）。走真实 route 链：decode/convert 真实，
%% validate/inject/stamp 用 passthrough mock 固定输入，route 的 drain 拒绝
%% 分支是真实代码。
connected_new_write_rejected_during_drain_test() ->
    Prev = lifecycle_env(),
    ok = application:set_env(imboy, lifecycle, draining),
    try
        with_mocks(
            [
                {websocket_logic, [
                    {'validate_message', 1, fun(Data) -> {ok, Data} end},
                    {'inject_sender_device', 2, fun(Payload, _State) -> Payload end},
                    {'stamp_sender_device', 2, fun(Data, _State) -> Data end}
                ]}
            ],
            fun() ->
                Json = jsone:encode(#{
                    <<"id">> => <<"t02mid6">>,
                    <<"type">> => <<"C2C">>,
                    <<"to">> => <<"43">>,
                    <<"payload">> => #{<<"text">> => <<"hi">>}
                }),
                ?assertEqual(
                    {reply, {close, 1012, <<"service_restart">>}, ack_state()},
                    text_frame(Json, ack_state())
                ),
                %% 新写不进入业务 logic
                ?assertEqual(0, meck:num_calls(msg_c2c_logic, c2c_client_ack, 3))
            end
        )
    after
        restore_lifecycle(Prev)
    end.

%% 已连接客户端：draining 中最后 ACK 语义保持——CLIENT_ACK 仍持久化确认
%% 并回 CONFIRM（客户端据此安全重连补拉，不丢已送达状态）。
connected_ack_still_confirmed_during_drain_test() ->
    Prev = lifecycle_env(),
    ok = application:set_env(imboy, lifecycle, draining),
    try
        with_mocks([], fun() ->
            Result = text_frame(<<"client_ack,s2c,t02mid7,did1">>, ack_state()),
            ?assertEqual(
                {<<"CLIENT_ACK_CONFIRM">>, <<"CLIENT_ACK_CONFIRM">>},
                confirm_type(Result)
            ),
            ?assertEqual(1, meck:num_calls(msg_s2c_logic, s2c_client_ack, 3))
        end)
    after
        restore_lifecycle(Prev)
    end.

%% drain 窗口内的 ACK action 放行对照（message_router_logic 真实代码）：
%% 小写 action（wire 规范形态）在 draining 时仍放行到业务 logic。
connected_ack_action_lowercase_passes_during_drain_test() ->
    Prev = lifecycle_env(),
    ok = application:set_env(imboy, lifecycle, draining),
    try
        {ok, _} = meck_helper:setup_mock(msg_c2c_logic, [
            {'c2c_revoke_ack', 3, fun(_MsgId, _Uid, _Data) -> ok end}
        ]),
        try
            Data = #{
                <<"id">> => <<"t02mid8">>,
                <<"type">> => <<"C2C">>,
                <<"action">> => <<"message_revoke_ack">>,
                <<"payload">> => #{}
            },
            ?assertEqual(
                ok,
                message_router_logic:route(<<"t02mid8">>, 42, Data, <<"C2C">>, <<"{}">>)
            ),
            ?assertEqual(1, meck:num_calls(msg_c2c_logic, c2c_revoke_ack, 3))
        after
            meck_helper:cleanup_mock(msg_c2c_logic)
        end
    after
        restore_lifecycle(Prev)
    end.

lifecycle_env() ->
    application:get_env(imboy, lifecycle, undefined).

restore_lifecycle(undefined) -> application:unset_env(imboy, lifecycle);
restore_lifecycle(Prev) -> application:set_env(imboy, lifecycle, Prev).
