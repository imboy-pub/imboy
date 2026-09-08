-module(auth_middleware_api_v1_tests).
-include_lib("eunit/include/eunit.hrl").

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
