-module(push_cleanup_logic_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(Modules)
        end
    end)()
).

%%%===================================================================
%%% @doc
%%% push_cleanup_logic（G3/Q09：过期推送 token 定时清理）的 EUnit 测试
%%%
%%% 覆盖：配置读取与下限钳制、DS 调用参数、错误吞掉（ecron 作业不 crash）
%%%===================================================================

%% ecron 入口使用配置天数（可覆盖），原样传给 DS
run_uses_configured_days_test() ->
    ?WITH_MECKS([config_ds, push_notification_ds], fun() ->
        meck:expect(config_ds, env, fun(push_token_cleanup_inactive_days, _Default) -> 14 end),
        meck:expect(push_notification_ds, cleanup_inactive_tokens, fun(14) -> {ok, 3} end),
        ?assertEqual(ok, push_cleanup_logic:run_inactive_token_cleanup()),
        ?assert(meck:called(push_notification_ds, cleanup_inactive_tokens, [14]))
    end).

%% 未配置时使用默认 30 天
run_uses_default_days_test() ->
    ?WITH_MECKS([config_ds, push_notification_ds], fun() ->
        meck:expect(config_ds, env, fun(push_token_cleanup_inactive_days, Default) -> Default end),
        meck:expect(push_notification_ds, cleanup_inactive_tokens, fun(30) -> {ok, 0} end),
        ?assertEqual(ok, push_cleanup_logic:run_inactive_token_cleanup()),
        ?assert(meck:called(push_notification_ds, cleanup_inactive_tokens, [30]))
    end).

%% 下限钳制：配置过小（如 1）强制抬到 7 天，防误删近期活跃 token
run_clamps_days_to_minimum_test() ->
    ?WITH_MECKS([config_ds, push_notification_ds], fun() ->
        meck:expect(config_ds, env, fun(push_token_cleanup_inactive_days, _Default) -> 1 end),
        meck:expect(push_notification_ds, cleanup_inactive_tokens, fun(7) -> {ok, 0} end),
        ?assertEqual(ok, push_cleanup_logic:run_inactive_token_cleanup()),
        ?assert(meck:called(push_notification_ds, cleanup_inactive_tokens, [7]))
    end).

%% DS 失败只记日志、返回 ok——ecron 作业永不 crash（同 attach_cleanup_logic 口径）
run_swallows_ds_error_test() ->
    ?WITH_MECKS([config_ds, push_notification_ds], fun() ->
        meck:expect(config_ds, env, fun(push_token_cleanup_inactive_days, _Default) -> 14 end),
        meck:expect(push_notification_ds, cleanup_inactive_tokens, fun(_) -> {error, db_down} end),
        ?assertEqual(ok, push_cleanup_logic:run_inactive_token_cleanup())
    end).

%% 手动入口 /1：指定天数直调（钳制同样生效），供运维与测试使用
run_with_explicit_days_test() ->
    ?WITH_MECKS([push_notification_ds], fun() ->
        meck:expect(push_notification_ds, cleanup_inactive_tokens, fun(90) -> {ok, 7} end),
        ?assertEqual(ok, push_cleanup_logic:run_inactive_token_cleanup(90)),
        ?assert(meck:called(push_notification_ds, cleanup_inactive_tokens, [90])),
        %% 钳制对手动入口同样生效
        meck:expect(push_notification_ds, cleanup_inactive_tokens, fun(7) -> {ok, 0} end),
        ?assertEqual(ok, push_cleanup_logic:run_inactive_token_cleanup(2)),
        ?assert(meck:called(push_notification_ds, cleanup_inactive_tokens, [7]))
    end).
