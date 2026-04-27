%%%-------------------------------------------------------------------
%%% @doc QR 登录 SSE Handler EUnit（Phase 2 PR-3α RED）
%%%
%%% 测试 cowboy_loop 三个 callback：
%%%   - init/2: 参数解析 + stream_reply + subscribe
%%%   - info/3: Event → SSE chunk + 终态判断
%%%   - terminate/3: unsubscribe
%%%
%%% 不依赖真实 cowboy / syn，全部 mock。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_sse_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% Helpers: 通用 mock 模板
%% ===================================================================

cowboy_req_mocks() ->
    [
        {cowboy_req, [
            {parse_qs, 1, fun(Req) -> maps:get(qs, Req, []) end},
            {stream_reply, 3, fun(_Status, _Headers, Req) ->
                Req#{stream_reply_called => true}
            end},
            {stream_body, 3, fun(Body, _IsFin, Req) ->
                Chunks = maps:get(chunks, Req, []),
                Req#{chunks => Chunks ++ [Body]}
            end},
            {reply, 4, fun(Status, Headers, Body, Req) ->
                Req#{reply_called => {Status, Headers, Body}}
            end}
        ]}
    ].

req(Qs) ->
    #{qs => Qs, chunks => []}.

%% ===================================================================
%% init/2: 5 个分支
%% ===================================================================

init_with_valid_session_token_returns_cowboy_loop_test_() ->
    SessionToken = <<"sess_init_ok">>,
    ?WITH_MECKS(cowboy_req_mocks() ++ [
        {qr_login_event_ds, [
            {subscribe, 2, fun(K, _Pid) when K =:= SessionToken -> ok end}
        ]}
    ], fun() ->
        Req0 = req([{<<"session_token">>, SessionToken}]),
        Result = qr_login_sse_handler:init(Req0, []),
        ?assertMatch({cowboy_loop, _, _}, Result),
        {cowboy_loop, Req1, State} = Result,
        ?assertEqual(true, maps:get(stream_reply_called, Req1)),
        ?assertEqual(SessionToken, maps:get(session_token, State))
    end).

init_calls_stream_reply_with_sse_headers_test_() ->
    SessionToken = <<"sess_init_headers">>,
    ?WITH_MECKS([
        {cowboy_req, [
            {parse_qs, 1, fun(Req) -> maps:get(qs, Req, []) end},
            {stream_reply, 3, fun(_Status, _Headers, Req) -> Req end}
        ]},
        {qr_login_event_ds, [
            {subscribe, 2, fun(_, _) -> ok end}
        ]}
    ], fun() ->
        Req0 = req([{<<"session_token">>, SessionToken}]),
        _ = qr_login_sse_handler:init(Req0, []),
        ?assert(meck:called(cowboy_req, stream_reply, [200, '_', '_']),
                "stream_reply 未被以 200 调用"),
        Calls = [{S, H} || {_, {cowboy_req, stream_reply, [S, H, _]}, _}
                          <- meck:history(cowboy_req)],
        [{200, Headers} | _] = Calls,
        ?assertEqual(<<"text/event-stream">>, maps:get(<<"content-type">>, Headers)),
        ?assertEqual(<<"no-cache">>, maps:get(<<"cache-control">>, Headers))
    end).

init_calls_subscribe_with_session_token_and_self_test_() ->
    SessionToken = <<"sess_init_subscribe">>,
    ?WITH_MECKS(cowboy_req_mocks() ++ [
        {qr_login_event_ds, [
            {subscribe, 2, fun(_, _) -> ok end}
        ]}
    ], fun() ->
        Req0 = req([{<<"session_token">>, SessionToken}]),
        _ = qr_login_sse_handler:init(Req0, []),
        ?assert(meck:called(qr_login_event_ds, subscribe, [SessionToken, '_']),
                "subscribe 未被以正确 session_token 调用"),
        Args = [A || {_, {qr_login_event_ds, subscribe, A}, _}
                     <- meck:history(qr_login_event_ds), length(A) =:= 2],
        [[K, Pid] | _] = Args,
        ?assertEqual(SessionToken, K),
        ?assert(is_pid(Pid))
    end).

init_without_session_token_returns_400_test_() ->
    ?WITH_MECKS(cowboy_req_mocks() ++ [
        {qr_login_event_ds, [
            {subscribe, 2, fun(_, _) -> ok end}
        ]}
    ], fun() ->
        Req0 = req([]),  %% 无 session_token
        Result = qr_login_sse_handler:init(Req0, []),
        ?assertMatch({ok, _, _}, Result),
        {ok, Req1, _} = Result,
        ?assertMatch({400, _, _}, maps:get(reply_called, Req1))
    end).

init_with_empty_session_token_returns_400_test_() ->
    ?WITH_MECKS(cowboy_req_mocks() ++ [
        {qr_login_event_ds, [
            {subscribe, 2, fun(_, _) -> ok end}
        ]}
    ], fun() ->
        Req0 = req([{<<"session_token">>, <<>>}]),
        Result = qr_login_sse_handler:init(Req0, []),
        ?assertMatch({ok, _, _}, Result),
        {ok, Req1, _} = Result,
        ?assertMatch({400, _, _}, maps:get(reply_called, Req1))
    end).

%% ===================================================================
%% info/3: 6 个分支（5 status + 容错）
%% ===================================================================

info_scanned_event_streams_chunk_and_keeps_loop_test_() ->
    Event = #{<<"status">> => <<"scanned">>},
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        State = #{session_token => <<"s1">>},
        Result = qr_login_sse_handler:info(Event, Req0, State),
        %% 继续 loop（waiting/scanned/cancelled 都返回 ok）
        ?assertMatch({ok, _, _}, Result),
        {ok, Req1, _} = Result,
        Chunks = maps:get(chunks, Req1, []),
        ?assertEqual(1, length(Chunks)),
        [Chunk] = Chunks,
        %% SSE 格式：data: <json>\n\n
        ?assertMatch(<<"data: ", _/binary>>, Chunk),
        ?assert(binary:match(Chunk, <<"\"status\":\"scanned\"">>) =/= nomatch),
        ?assert(binary:match(Chunk, <<"\n\n">>) =/= nomatch)
    end).

info_confirmed_event_streams_chunk_and_stops_test_() ->
    Event = #{<<"status">> => <<"confirmed">>, <<"token">> => <<"jwt_xyz">>},
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        State = #{session_token => <<"s1">>},
        Result = qr_login_sse_handler:info(Event, Req0, State),
        %% confirmed 终态 → stop
        ?assertMatch({stop, _, _}, Result),
        {stop, Req1, _} = Result,
        [Chunk] = maps:get(chunks, Req1),
        ?assert(binary:match(Chunk, <<"\"token\":\"jwt_xyz\"">>) =/= nomatch)
    end).

info_expired_event_streams_chunk_and_stops_test_() ->
    Event = #{<<"status">> => <<"expired">>},
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        State = #{session_token => <<"s1">>},
        Result = qr_login_sse_handler:info(Event, Req0, State),
        ?assertMatch({stop, _, _}, Result)
    end).

info_cancelled_event_streams_chunk_and_keeps_loop_test_() ->
    %% cancelled 不是终态，Web 端可能会重新 generate QR
    Event = #{<<"status">> => <<"cancelled">>},
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        State = #{session_token => <<"s1">>},
        Result = qr_login_sse_handler:info(Event, Req0, State),
        ?assertMatch({ok, _, _}, Result)
    end).

info_waiting_event_streams_chunk_and_keeps_loop_test_() ->
    Event = #{<<"status">> => <<"waiting">>},
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        State = #{session_token => <<"s1">>},
        Result = qr_login_sse_handler:info(Event, Req0, State),
        ?assertMatch({ok, _, _}, Result)
    end).

info_non_map_message_keeps_loop_silently_test_() ->
    %% syn 多版本 / system 消息容错：非 map 消息应被静默忽略，继续 loop
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        State = #{session_token => <<"s1">>},
        Result = qr_login_sse_handler:info({tcp_closed, fake_socket}, Req0, State),
        ?assertMatch({ok, _, _}, Result),
        {ok, Req1, _} = Result,
        ?assertEqual([], maps:get(chunks, Req1)),
        Result2 = qr_login_sse_handler:info(some_atom, Req0, State),
        ?assertMatch({ok, _, _}, Result2)
    end).

%% ===================================================================
%% terminate/3: 2 个场景
%% ===================================================================

terminate_unsubscribes_when_session_token_present_test_() ->
    SessionToken = <<"sess_term_ok">>,
    ?WITH_MECKS([
        {qr_login_event_ds, [
            {unsubscribe, 2, fun(_, _) -> ok end}
        ]}
    ], fun() ->
        State = #{session_token => SessionToken},
        ?assertEqual(ok, qr_login_sse_handler:terminate(normal, #{}, State)),
        ?assert(meck:called(qr_login_event_ds, unsubscribe, [SessionToken, '_']),
                "unsubscribe 未被 terminate 调用")
    end).

%% ===================================================================
%% Heartbeat: 防 cowboy 默认 60s idle_timeout 断连
%% ===================================================================

init_starts_heartbeat_timer_test_() ->
    %% init 后 state 必须含 heartbeat_timer 引用
    SessionToken = <<"sess_hb_init">>,
    ?WITH_MECKS(cowboy_req_mocks() ++ [
        {qr_login_event_ds, [
            {subscribe, 2, fun(_, _) -> ok end}
        ]}
    ], fun() ->
        Req0 = req([{<<"session_token">>, SessionToken}]),
        Result = qr_login_sse_handler:init(Req0, []),
        {cowboy_loop, _, State} = Result,
        ?assertMatch(#{heartbeat_timer := _}, State),
        Timer = maps:get(heartbeat_timer, State),
        ?assert(is_reference(Timer)),
        %% 测试结束立即 cancel 防泄漏
        erlang:cancel_timer(Timer)
    end).

info_heartbeat_emits_sse_comment_chunk_test_() ->
    %% qr_sse_heartbeat 消息 → 发 ": heartbeat\n\n" SSE comment
    %% 浏览器原生 EventSource 自动过滤 comment，不触发 onmessage
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        OldTimer = erlang:send_after(60000, self(), dummy),
        State = #{session_token => <<"s1">>, heartbeat_timer => OldTimer},
        Result = qr_login_sse_handler:info(qr_sse_heartbeat, Req0, State),
        ?assertMatch({ok, _, _}, Result),
        {ok, Req1, _} = Result,
        Chunks = maps:get(chunks, Req1, []),
        ?assertEqual([<<": heartbeat\n\n">>], Chunks),
        %% 清理新建的 timer
        {ok, _, NewState} = Result,
        case maps:get(heartbeat_timer, NewState, undefined) of
            undefined -> ok;
            T -> erlang:cancel_timer(T)
        end
    end).

info_heartbeat_keeps_loop_test_() ->
    %% 心跳后必须保持 cowboy_loop（不能 stop）
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        OldTimer = erlang:send_after(60000, self(), dummy),
        State = #{session_token => <<"s1">>, heartbeat_timer => OldTimer},
        Result = qr_login_sse_handler:info(qr_sse_heartbeat, Req0, State),
        ?assertMatch({ok, _, _}, Result),
        {ok, _, NewState} = Result,
        case maps:get(heartbeat_timer, NewState, undefined) of
            undefined -> ok;
            T -> erlang:cancel_timer(T)
        end
    end).

info_heartbeat_reschedules_timer_test_() ->
    %% 心跳后必须重新启动 timer（state.heartbeat_timer 是新引用）
    ?WITH_MECKS(cowboy_req_mocks(), fun() ->
        Req0 = req([]),
        OldTimer = erlang:send_after(60000, self(), old_dummy),
        State = #{session_token => <<"s1">>, heartbeat_timer => OldTimer},
        {ok, _, NewState} = qr_login_sse_handler:info(qr_sse_heartbeat, Req0, State),
        NewTimer = maps:get(heartbeat_timer, NewState),
        ?assert(is_reference(NewTimer), "新 timer 必须是 reference"),
        ?assertNotEqual(OldTimer, NewTimer, "必须替换为新 timer 引用"),
        erlang:cancel_timer(NewTimer)
    end).

terminate_cancels_heartbeat_timer_test_() ->
    %% terminate 必须 cancel timer 防止心跳消息发到已死进程
    ?WITH_MECKS([
        {qr_login_event_ds, [
            {unsubscribe, 2, fun(_, _) -> ok end}
        ]}
    ], fun() ->
        Timer = erlang:send_after(60000, self(), dummy_hb),
        State = #{session_token => <<"sess_term_hb">>, heartbeat_timer => Timer},
        ?assertEqual(ok, qr_login_sse_handler:terminate(normal, #{}, State)),
        %% Timer 应已被 cancel；erlang:cancel_timer 第二次调用返回 false
        Result = erlang:cancel_timer(Timer),
        ?assertEqual(false, Result, "terminate 应已 cancel timer，二次 cancel 应返回 false")
    end).

terminate_does_not_call_unsubscribe_when_state_lacks_token_test_() ->
    %% init 失败路径：state 可能为 [] 或 #{} 不含 session_token
    ?WITH_MECKS([
        {qr_login_event_ds, [
            {unsubscribe, 2, fun(_, _) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(ok, qr_login_sse_handler:terminate(normal, #{}, [])),
        ?assertEqual(ok, qr_login_sse_handler:terminate(normal, #{}, #{})),
        ?assertNot(meck:called(qr_login_event_ds, unsubscribe, ['_', '_']),
                   "无 session_token 时不应调 unsubscribe")
    end).
