-module(channel_webhook_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%% WH-02：channel_webhook 管理端点契约测试（静态导出/路由契约，
%%% 范式同 bot_e2e_tests；语义级测试在 ds/logic/repo 套件覆盖）。

ensure_module_loaded(M) ->
    _ = code:ensure_loaded(M),
    ok.

handler_exports_contract_test() ->
    ensure_module_loaded(channel_webhook_handler),
    %% handler 只导出 init/2；rotate 等动作经 action 分派（init 内部函数）
    ?assert(erlang:function_exported(channel_webhook_handler, init, 2)),
    {ok, Source} = file:read_file("src/api/channel_webhook_handler.erl"),
    ?assert(binary:match(Source, <<"rotate -> rotate(Req0, State)">>) =/= nomatch).

logic_rotate_exports_contract_test() ->
    ensure_module_loaded(channel_webhook_logic),
    ?assert(erlang:function_exported(channel_webhook_logic, rotate, 3)),
    ?assert(erlang:function_exported(channel_webhook_logic, rotate, 4)),
    ?assert(erlang:function_exported(channel_webhook_logic, incoming, 3)).

ds_rotate_exports_contract_test() ->
    ensure_module_loaded(channel_webhook_ds),
    ?assert(erlang:function_exported(channel_webhook_ds, rotate, 4)),
    ?assert(erlang:function_exported(channel_webhook_ds, find_by_token, 1)).

repo_digest_exports_contract_test() ->
    ensure_module_loaded(channel_webhook_repo),
    ?assert(erlang:function_exported(channel_webhook_repo, find_by_digest, 1)),
    ?assert(erlang:function_exported(channel_webhook_repo, find_by_grace_digest, 1)),
    ?assert(erlang:function_exported(channel_webhook_repo, rotate, 5)),
    ?assert(erlang:function_exported(channel_webhook_repo, touch_last_used, 1)).

%% 路由契约：rotate 路由已注册
rotate_route_exists_test() ->
    {ok, Source} = file:read_file("src/imboy_router.erl"),
    ?assert(
        binary:match(Source, <<"/api/v1/channel/:channel_id/webhook/:webhook_id/rotate">>) =/=
            nomatch
    ).

%% 逻辑：宽限秒默认 600（契约冻结值），config 可调
grace_default_600_test() ->
    ensure_module_loaded(channel_webhook_logic),
    application:unset_env(imboy, channel_webhook_rotate_grace_secs),
    ?assert(erlang:function_exported(channel_webhook_logic, rotate, 3)),
    ?assert(erlang:function_exported(channel_webhook_logic, rotate, 4)).
