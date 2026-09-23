-module(auth_middleware_api_v1_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% WH-02/MCP-01：auth_middleware_api_v1 免认证直通契约（静态源码断言，
%%% 范式同 bot_e2e_tests）。
%%%   - /api/v1/mcp（MCP credential 认证收敛于 mcp_handler）
%%%   - /api/v1/webhook/channel/ 前缀（token 即凭证）
%%%   - /api/v1/payment/callback/ 前缀（第三方回调）

middleware_exports_test() ->
    _ = code:ensure_loaded(auth_middleware_api_v1),
    ?assert(erlang:function_exported(auth_middleware_api_v1, execute, 2)).

mcp_path_passthrough_test() ->
    {ok, Source} = file:read_file("src/api/auth_middleware_api_v1.erl"),
    ?assert(binary:match(Source, <<"IsMcpPath">>) =/= nomatch),
    ?assert(
        binary:match(Source, <<"IsChannelWebhook orelse IsMcpPath">>) =/=
            nomatch
    ).

channel_webhook_passthrough_test() ->
    {ok, Source} = file:read_file("src/api/auth_middleware_api_v1.erl"),
    ?assert(binary:match(Source, <<"IsChannelWebhook">>) =/= nomatch).

payment_callback_passthrough_test() ->
    {ok, Source} = file:read_file("src/api/auth_middleware_api_v1.erl"),
    ?assert(binary:match(Source, <<"IsPaymentCallback">>) =/= nomatch).

web_qr_login_paths_skip_device_signature_test_() ->
    Paths = [
        <<"/api/v1/passport/qr_login/create">>,
        <<"/api/v1/passport/qr_login/status">>,
        <<"/api/v1/passport/qr_login/cancel">>,
        <<"/api/v1/passport/qr_login/subscribe">>
    ],
    [qr_login_auth_case(Path, open) || Path <- Paths].

mobile_qr_login_paths_keep_device_signature_test_() ->
    Paths = [
        <<"/api/v1/passport/qr_login/scan">>,
        <<"/api/v1/passport/qr_login/confirm">>
    ],
    [qr_login_auth_case(Path, protected) || Path <- Paths].

%% 2026-09-23 生产 902 修复：Web 坐席面（imboyadmin seat/ 浏览器端，无法持有
%% APP 设备签名密钥）跳过 verify_sign，但 JWT 门保持强制——condition 收到
%% (Option=false, Open=false)，缺 Bearer 照常 401（do_authorization 语义）。
web_seat_paths_skip_device_signature_keep_jwt_gate_test_() ->
    Paths = [
        <<"/api/v1/cs/me/seat-contexts">>,
        <<"/api/v1/enterprise/organizations/123/conversations/456/messages">>
    ],
    [web_seat_auth_case(Path) || Path <- Paths].

web_seat_auth_case(Path) ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> Path end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, _Default) -> <<"on">> end}
            ]},
            {imboy_router, [
                {'open', 0, fun() -> [] end},
                {'option', 0, fun() -> [] end}
            ]},
            {auth_ds, [
                {'remove_last_forward_slash', 1, fun(Value) -> Value end},
                {'verify_sign', 2, fun(Req, Env) ->
                    {stop, Req#{auth_error => 902, env => Env}}
                end},
                {'condition', 5, fun(Optional, Open, _Auth, Req, Env) ->
                    {ok, Req#{jwt_gate => {Optional, Open}, env => Env}}
                end}
            ]}
        ],
        fun() ->
            Result = auth_middleware_api_v1:execute(#{}, #{}),
            %% 免设备签名：verify_sign 一次都不进（修复前 902 拦死点）。
            ?assertEqual(0, meck:num_calls(auth_ds, verify_sign, 2)),
            %% JWT 门不放宽：非 option 非 open → do_authorization 强校验分支。
            ?assertMatch({ok, #{jwt_gate := {false, false}}}, Result)
        end
    ).

qr_login_auth_case(Path, Expected) ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> Path end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, _Default) -> <<"on">> end}
            ]},
            {imboy_router, [
                {'open', 0, fun() ->
                    [
                        <<"/api/v1/passport/qr_login/create">>,
                        <<"/api/v1/passport/qr_login/status">>,
                        <<"/api/v1/passport/qr_login/cancel">>,
                        <<"/api/v1/passport/qr_login/subscribe">>
                    ]
                end},
                {'option', 0, fun() -> [] end}
            ]},
            {auth_ds, [
                {'remove_last_forward_slash', 1, fun(Value) -> Value end},
                {'verify_sign', 2, fun(Req, Env) ->
                    {stop, Req#{auth_error => 902, env => Env}}
                end},
                {'condition', 5, fun(_Optional, true, _Auth, Req, Env) ->
                    {ok, Req, Env}
                end}
            ]}
        ],
        fun() ->
            Result = auth_middleware_api_v1:execute(#{}, #{}),
            case Expected of
                open ->
                    ?assertEqual({ok, #{}, #{}}, Result),
                    ?assertEqual(0, meck:num_calls(auth_ds, verify_sign, 2));
                protected ->
                    ?assertMatch({stop, #{auth_error := 902}}, Result),
                    ?assertEqual(1, meck:num_calls(auth_ds, verify_sign, 2))
            end
        end
    ).
