%%%-------------------------------------------------------------------
%%% @doc QR 登录会话事件 DS EUnit 测试（Phase 2 PR-1 RED）
%%%
%%% 锚定 `src/ds/qr_login_event_ds.erl` 的两个纯函数契约：
%%%   - topic_for/1：SessionToken → syn topic 元组
%%%   - event/2：Status + Token → 客户端可消费事件 map
%%%
%%% 测试不依赖 cowboy / syn / cache，仅纯 EUnit。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_event_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% syn scope 必须与 src/ds/qr_login_event_ds.erl 内 ?QR_LOGIN_SCOPE 同值
-define(QR_LOGIN_SCOPE, imboy_qr_login).

%% ===================================================================
%% topic_for/1: SessionToken → {qr_login, SessionToken}
%% ===================================================================

topic_for_returns_qr_login_tuple_test() ->
    ?assertEqual({qr_login, <<"sess_abc">>}, qr_login_event_ds:topic_for(<<"sess_abc">>)).

topic_for_preserves_long_base64_session_token_test() ->
    SessionToken = <<"V2hhdHNBcHBXZWJTdHlsZUxvbmdTZXNzaW9uVG9rZW5XaXRoVGltZXN0YW1w">>,
    ?assertEqual({qr_login, SessionToken}, qr_login_event_ds:topic_for(SessionToken)).

topic_for_throws_on_empty_binary_test() ->
    %% Let It Crash：空 binary 是非法输入，调用方不应传入。
    ?assertError(badarg, qr_login_event_ds:topic_for(<<>>)).

topic_for_throws_on_non_binary_test() ->
    %% 类型违反必须立即崩溃，便于上游 supervisor 捕获。
    ?assertError(badarg, qr_login_event_ds:topic_for("not_a_binary")),
    ?assertError(badarg, qr_login_event_ds:topic_for(undefined)),
    ?assertError(badarg, qr_login_event_ds:topic_for(12345)).

%% ===================================================================
%% event/2: 5 status × token 矩阵
%% ===================================================================

event_waiting_returns_status_only_test() ->
    %% waiting 状态不携带 token 字段（与 handle_status 响应一致）。
    ?assertEqual(#{<<"status">> => <<"waiting">>},
                 qr_login_event_ds:event(waiting, undefined)).

event_waiting_ignores_token_when_provided_test() ->
    %% 即使误传 token，waiting 状态也不应泄漏到事件中。
    ?assertEqual(#{<<"status">> => <<"waiting">>},
                 qr_login_event_ds:event(waiting, <<"jwt_leaked">>)).

event_scanned_returns_status_only_test() ->
    ?assertEqual(#{<<"status">> => <<"scanned">>},
                 qr_login_event_ds:event(scanned, undefined)).

event_expired_returns_status_only_test() ->
    ?assertEqual(#{<<"status">> => <<"expired">>},
                 qr_login_event_ds:event(expired, undefined)).

event_cancelled_returns_status_only_test() ->
    ?assertEqual(#{<<"status">> => <<"cancelled">>},
                 qr_login_event_ds:event(cancelled, undefined)).

event_confirmed_includes_token_test() ->
    %% 仅 confirmed 状态附 token（对齐 handle_status:134-145 的 payload 结构）。
    Event = qr_login_event_ds:event(confirmed, <<"jwt_xyz_real">>),
    ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, Event)),
    ?assertEqual(<<"jwt_xyz_real">>, maps:get(<<"token">>, Event)).

event_confirmed_with_undefined_token_throws_test() ->
    %% 协议违反：confirmed 必须携带 token，缺失时立即 crash 便于诊断。
    ?assertError(protocol_violation,
                 qr_login_event_ds:event(confirmed, undefined)).

event_confirmed_with_empty_binary_throws_test() ->
    %% 空 binary token 等同于无效，仍按协议违反处理。
    ?assertError(protocol_violation,
                 qr_login_event_ds:event(confirmed, <<>>)).

event_unknown_status_throws_test() ->
    %% 未知 status atom 必须立即 crash，避免静默生成无效事件。
    ?assertError(unknown_status,
                 qr_login_event_ds:event(some_unknown_status, undefined)),
    ?assertError(unknown_status,
                 qr_login_event_ds:event(failed, undefined)),
    ?assertError(unknown_status,
                 qr_login_event_ds:event(<<"waiting">>, undefined)).  %% binary 而非 atom

%% ===================================================================
%% 跨函数集成场景
%% ===================================================================

topic_and_event_can_be_combined_for_publish_pattern_test() ->
    %% 模拟 PR-2 调用模式：scan 时构造 topic + event 准备 syn:publish。
    SessionToken = <<"sess_integration_1">>,
    Topic = qr_login_event_ds:topic_for(SessionToken),
    Event = qr_login_event_ds:event(scanned, undefined),
    ?assertEqual({qr_login, <<"sess_integration_1">>}, Topic),
    ?assertEqual(#{<<"status">> => <<"scanned">>}, Event).

%% ===================================================================
%% subscribe/2: PR-3 SSE handler 加入 group 的 syn 包装
%% ===================================================================

subscribe_calls_syn_join_with_qr_login_scope_test_() ->
    SessionToken = <<"sess_sub_1">>,
    SelfPid = self(),
    ?WITH_MECKS([
        {syn, [
            {join, 3, fun(Scope, Topic, Pid) ->
                ?assertEqual(?QR_LOGIN_SCOPE, Scope),
                ?assertEqual({qr_login, SessionToken}, Topic),
                ?assertEqual(SelfPid, Pid),
                ok
            end}
        ]}
    ], fun() ->
        ?assertEqual(ok, qr_login_event_ds:subscribe(SessionToken, SelfPid))
    end).

subscribe_returns_error_when_syn_throws_test_() ->
    %% scope 未注册场景：syn:join 抛异常 → 返回 {error, Reason} 而非 crash
    ?WITH_MECKS([
        {syn, [
            {join, 3, fun(_, _, _) -> erlang:error({invalid_scope, ?QR_LOGIN_SCOPE}) end}
        ]}
    ], fun() ->
        Result = qr_login_event_ds:subscribe(<<"sess_err">>, self()),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% unsubscribe/2: PR-3 SSE handler 在 terminate 时离开 group
%% ===================================================================

unsubscribe_calls_syn_leave_with_qr_login_scope_test_() ->
    SessionToken = <<"sess_unsub_1">>,
    SelfPid = self(),
    ?WITH_MECKS([
        {syn, [
            {leave, 3, fun(Scope, Topic, Pid) ->
                ?assertEqual(?QR_LOGIN_SCOPE, Scope),
                ?assertEqual({qr_login, SessionToken}, Topic),
                ?assertEqual(SelfPid, Pid),
                ok
            end}
        ]}
    ], fun() ->
        ?assertEqual(ok, qr_login_event_ds:unsubscribe(SessionToken, SelfPid))
    end).

unsubscribe_returns_error_when_syn_throws_test_() ->
    ?WITH_MECKS([
        {syn, [
            {leave, 3, fun(_, _, _) -> erlang:throw({not_in_group, foo}) end}
        ]}
    ], fun() ->
        Result = qr_login_event_ds:unsubscribe(<<"sess_err">>, self()),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% notify/2: handler 调用，向 group 内所有订阅者广播事件
%% ===================================================================

notify_calls_syn_publish_with_qr_login_scope_and_event_test_() ->
    SessionToken = <<"sess_notify_1">>,
    Event = #{<<"status">> => <<"scanned">>},
    ?WITH_MECKS([
        {syn, [
            {publish, 3, fun(Scope, Topic, Msg) ->
                ?assertEqual(?QR_LOGIN_SCOPE, Scope),
                ?assertEqual({qr_login, SessionToken}, Topic),
                ?assertEqual(Event, Msg),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 1}, qr_login_event_ds:notify(SessionToken, Event))
    end).

notify_returns_zero_when_syn_throws_test_() ->
    %% scope 未注册时 silent 返回 {ok, 0}，与 imboy_syn:publish 兜底语义一致
    Event = #{<<"status">> => <<"confirmed">>, <<"token">> => <<"jwt_x">>},
    ?WITH_MECKS([
        {syn, [
            {publish, 3, fun(_, _, _) -> erlang:error({invalid_scope, ?QR_LOGIN_SCOPE}) end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 0}, qr_login_event_ds:notify(<<"sess_err">>, Event))
    end).

notify_returns_zero_when_no_subscribers_test_() ->
    %% syn:publish 返回 {ok, 0} 表示没有订阅者（waiting 阶段 / SSE 未连）
    ?WITH_MECKS([
        {syn, [
            {publish, 3, fun(_, _, _) -> {ok, 0} end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 0},
                     qr_login_event_ds:notify(<<"sess_lone">>,
                                              #{<<"status">> => <<"waiting">>}))
    end).

notify_propagates_subscriber_count_test_() ->
    %% 多端订阅（如用户开了 2 个标签页）— 返回真实投递数
    ?WITH_MECKS([
        {syn, [
            {publish, 3, fun(_, _, _) -> {ok, 3} end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 3},
                     qr_login_event_ds:notify(<<"sess_multi">>,
                                              #{<<"status">> => <<"scanned">>}))
    end).
