-module(mcp_handler_sse_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc mcp_handler SSE/会话/重放纯逻辑测试（Phase 3 T3.6）
%%% SSE 长连接本身难 EUnit（需真 cowboy 传输），此处测可测的纯逻辑：
%%%   - sse_frame/2 帧格式（含 id: 行，Last-Event-ID 依赖它）
%%%   - process/3 会话线程化：initialize 惰性建会话并回带 SessionId
%%%   - events_since 重放：增量返回 + 越界 truncated
%%%===================================================================

%% ---- 纯函数：SSE 帧格式（无需启动任何服务）----------------------

sse_frame_test() ->
    %% id: 行必须在 data: 行之前，两行各自换行，帧尾双换行
    Frame = mcp_handler:sse_frame(<<"42">>, <<"{\"a\":1}">>),
    ?assertEqual(<<"id: 42\ndata: {\"a\":1}\n\n">>, Frame).

sse_frame_has_id_line_test() ->
    %% Last-Event-ID 靠 id: 行；确保不是只有 data:（qr 样板的坑）
    Frame = mcp_handler:sse_frame(<<"evt-1">>, <<"{}">>),
    ?assert(binary:match(Frame, <<"id: evt-1\n">>) =/= nomatch),
    ?assert(binary:match(Frame, <<"data: {}\n\n">>) =/= nomatch).

%% ---- 需会话管理器：process/3 + events_since ---------------------

session_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"process/3 initialize 无头 → 惰性建会话回带 SessionId", fun test_initialize_creates_session/0},
        {"process/3 tools/list 无头 → 无状态不建会话", fun test_no_session_for_stateless/0},
        {"process/3 沿用请求头带的 SessionId", fun test_reuse_header_session/0},
        {"events_since 增量重放（oldest first）", fun test_replay_incremental/0},
        {"events_since 越界 → truncated", fun test_replay_truncated/0}
    ]}.

setup() ->
    _ = start_srv(barrel_mcp_registry),
    _ = start_srv(barrel_mcp_session),
    ok = barrel_mcp_registry:wait_for_ready(),
    ok.

start_srv(Mod) ->
    case Mod:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

cleanup(_) ->
    ok.

test_initialize_creates_session() ->
    Req = <<
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\","
        "\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
        "\"clientInfo\":{\"name\":\"t\",\"version\":\"1\"}}}"
    >>,
    {Code, _Resp, SessionId} = mcp_handler:process(Req, 0, undefined),
    ?assertEqual(200, Code),
    ?assert(is_binary(SessionId)),
    %% generate_id/0 前缀约定
    ?assertMatch(<<"mcp_", _/binary>>, SessionId),
    %% 会话确实落库
    ?assertMatch({ok, _}, barrel_mcp_session:get(SessionId)).

test_no_session_for_stateless() ->
    Req = <<"{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}">>,
    {Code, _Resp, SessionId} = mcp_handler:process(Req, 0, undefined),
    ?assertEqual(200, Code),
    ?assertEqual(undefined, SessionId).

test_reuse_header_session() ->
    {ok, Sid} = barrel_mcp_session:create(#{}),
    Req = <<"{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/list\"}">>,
    {Code, _Resp, SessionId} = mcp_handler:process(Req, 0, Sid),
    ?assertEqual(200, Code),
    ?assertEqual(Sid, SessionId).

test_replay_incremental() ->
    {ok, Sid} = barrel_mcp_session:create(#{}),
    ok = barrel_mcp_session:record_sse_event(Sid, <<"1">>, #{<<"n">> => 1}),
    ok = barrel_mcp_session:record_sse_event(Sid, <<"2">>, #{<<"n">> => 2}),
    ok = barrel_mcp_session:record_sse_event(Sid, <<"3">>, #{<<"n">> => 3}),
    %% 客户端最后见到 "1"，重放应返回 2、3（chronological, oldest first）
    ?assertEqual(
        {ok, [{<<"2">>, #{<<"n">> => 2}}, {<<"3">>, #{<<"n">> => 3}}]},
        barrel_mcp_session:events_since(Sid, <<"1">>)
    ).

test_replay_truncated() ->
    {ok, Sid} = barrel_mcp_session:create(#{}),
    ok = barrel_mcp_session:record_sse_event(Sid, <<"10">>, #{}),
    %% LastId 不在缓冲窗口 → truncated（handler 据此提示客户端全量重拉）
    ?assertEqual(truncated, barrel_mcp_session:events_since(Sid, <<"unknown">>)).
