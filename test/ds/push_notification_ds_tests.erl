-module(push_notification_ds_tests).

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

%% ===================================================================
%% Tests
%% ===================================================================

send_to_user_no_token_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, []} end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, <<"title">>, <<"body">>))
    end).

send_to_user_with_token_test() ->
    ?WITH_MECKS([push_token_repo, elib_async], fun() ->
        Rows = [#{<<"platform">> => <<"fcm">>, <<"token">> => <<"token1">>}],
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, Rows} end),
        %% Mock elib_async:async_retry to skip actual push
        meck:expect(elib_async, async_retry, fun(_Fun, _Retry, _Delay) -> self() end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, <<"title">>, <<"body">>)),
        ?assert(meck:called(elib_async, async_retry, '_'))
    end).

send_to_users_empty_test() ->
    ?assertEqual(ok, push_notification_ds:send_to_users([], <<"title">>, <<"body">>)).

send_fcm_not_configured_test() ->
    %% 确保 push 配置不存在
    application:unset_env(imboy, push),
    ?assertEqual(
        {error, not_configured}, push_notification_ds:send_fcm(<<"token">>, <<"title">>, <<"body">>)
    ).

send_apns_not_configured_test() ->
    application:unset_env(imboy, push),
    ?assertEqual(
        {error, not_configured},
        push_notification_ds:send_apns(<<"token">>, <<"title">>, <<"body">>)
    ).

extract_push_info_tuple4_test() ->
    ?WITH_MECKS([push_token_repo, elib_async], fun() ->
        Rows = [#{<<"platform">> => <<"apns">>, <<"token">> => <<"apns_token">>}],
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, Rows} end),
        meck:expect(elib_async, async_retry, fun(_Fun, _Retry, _Delay) -> self() end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, <<"title">>, <<"body">>)),
        ?assert(meck:called(elib_async, async_retry, '_'))
    end).

send_to_users_with_tokens_test() ->
    ?WITH_MECKS([push_token_repo, elib_async], fun() ->
        Rows = [#{<<"platform">> => <<"fcm">>, <<"token">> => <<"token1">>}],
        meck:expect(push_token_repo, list_by_uids, fun([1, 2]) -> {ok, Rows} end),
        meck:expect(elib_async, async_retry, fun(_Fun, _Retry, _Delay) -> self() end),
        ?assertEqual(ok, push_notification_ds:send_to_users([1, 2], <<"title">>, <<"body">>))
    end).

%% send_to_user_with_data：Data 原样贯穿到 provider（jpush 分支全链）。
send_to_user_with_data_threads_data_test() ->
    ?WITH_MECKS([push_token_repo, elib_async, push_provider_jpush], fun() ->
        Rows = [#{<<"platform">> => <<"jpush">>, <<"token">> => <<"rid-1">>}],
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, Rows} end),
        %% async_retry 真执行 fun（默认 mock 不执行，provider 不会被调）
        meck:expect(elib_async, async_retry, fun(F, _Retry, _Delay) ->
            F(),
            self()
        end),
        meck:expect(push_provider_jpush, send, 4, fun(_Tok, T, B, D) ->
            self() ! {jpush_called, T, B, D},
            ok
        end),
        Data = #{<<"notify_type">> => <<"org_invite">>},
        ?assertEqual(
            ok, push_notification_ds:send_to_user_with_data(1, <<"title">>, <<"body">>, Data)
        ),
        receive
            {jpush_called, <<"title">>, <<"body">>, Data} -> ok
        after 1000 ->
            erlang:error(data_not_threaded)
        end
    end).

%% ===================================================================
%% G4/Q09 · FCM access token 来源优先级（service account 刷新 > 静态配置）
%% 凭据隔离：全部占位值，gun 桩拦截一切出网（L3 注入点配方）
%% ===================================================================

stub_gun_ok_capturing_headers() ->
    meck:expect(gun, open, fun(_Host, _Port, _Opts) -> {ok, self()} end),
    meck:expect(gun, await_up, fun(_Pid, _Timeout) -> {ok, http2} end),
    meck:expect(gun, post, fun(_Pid, _Path, Headers, _Body) ->
        self() ! {fcm_headers, Headers},
        stream_ref
    end),
    meck:expect(gun, await, fun(_Pid, _Ref, _Timeout) ->
        {response, fin, 200, []}
    end),
    meck:expect(gun, close, fun(_Pid) -> ok end).

%% 配置了 service account 时：Authorization 必须用动态刷新 token，
%% 静态 fcm_access_token（即使存在）不得使用。
send_fcm_prefers_service_account_token_test() ->
    ?WITH_MECKS([push_provider_fcm_token, gun], fun() ->
        application:set_env(imboy, push, [
            {fcm_project_id, <<"fake-project-placeholder">>},
            {fcm_service_account_json, <<"{\"placeholder\":true}">>},
            {fcm_access_token, <<"static-should-not-be-used">>}
        ]),
        meck:expect(push_provider_fcm_token, get_access_token, fun() ->
            self() ! dyn_token_refreshed,
            {ok, <<"dyn-access-placeholder">>}
        end),
        stub_gun_ok_capturing_headers(),
        ?assertEqual(
            ok, push_notification_ds:send_fcm(<<"tok-placeholder">>, <<"t">>, <<"b">>)
        ),
        ?assertEqual(1, meck:num_calls(push_provider_fcm_token, get_access_token, 0)),
        receive
            dyn_token_refreshed -> ok
        after 1000 ->
            erlang:error(refresh_not_used)
        end,
        receive
            {fcm_headers, Headers} ->
                ?assertEqual(
                    <<"Bearer dyn-access-placeholder">>,
                    proplists:get_value(<<"authorization">>, Headers)
                )
        after 1000 ->
            erlang:error(headers_not_captured)
        end
    end).

%% 向后兼容：仅静态 token（无 service account）时刷新链路零调用，
%% Authorization 用静态配置值。
send_fcm_static_token_backcompat_test() ->
    ?WITH_MECKS([push_provider_fcm_token, gun], fun() ->
        application:set_env(imboy, push, [
            {fcm_project_id, <<"fake-project-placeholder">>},
            {fcm_access_token, <<"static-access-placeholder">>}
        ]),
        meck:expect(push_provider_fcm_token, get_access_token, fun() ->
            erlang:error(unexpected_refresh_call)
        end),
        stub_gun_ok_capturing_headers(),
        ?assertEqual(
            ok, push_notification_ds:send_fcm(<<"tok-placeholder">>, <<"t">>, <<"b">>)
        ),
        ?assertEqual(0, meck:num_calls(push_provider_fcm_token, get_access_token, 0)),
        receive
            {fcm_headers, Headers} ->
                ?assertEqual(
                    <<"Bearer static-access-placeholder">>,
                    proplists:get_value(<<"authorization">>, Headers)
                )
        after 1000 ->
            erlang:error(headers_not_captured)
        end
    end).

%% 刷新链路失败（配置类错误）：零出网、错误原样返回（fail-closed）
send_fcm_refresh_config_error_not_retried_test() ->
    ?WITH_MECKS([push_provider_fcm_token, gun], fun() ->
        application:set_env(imboy, push, [
            {fcm_project_id, <<"fake-project-placeholder">>},
            {fcm_service_account_json, <<"{\"broken\"}">>}
        ]),
        meck:expect(push_provider_fcm_token, get_access_token, fun() ->
            {error, {config_error, missing_required_field}}
        end),
        meck:expect(gun, open, fun(_, _, _) -> erlang:error(unexpected_real_gun_open) end),
        ?assertEqual(
            {error, {config_error, missing_required_field}},
            push_notification_ds:send_fcm(<<"tok-placeholder">>, <<"t">>, <<"b">>)
        ),
        ?assertEqual(0, meck:num_calls(gun, open, '_'))
    end).
