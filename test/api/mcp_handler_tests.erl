-module(mcp_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc mcp_handler 桥接测试（Phase 3 T3.2）
%%% 验收：POST body(JSON-RPC) → decode/handle/encode → 同步 JSON。
%%% 覆盖 initialize（200 + result）、tools/list（200 + tools 数组）、
%%%      notification（202 空体）、解析失败（400 + -32700）。
%%%===================================================================

bridge_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"initialize → 200 + protocolVersion", fun test_initialize/0},
        {"tools/list → 200 + tools 数组", fun test_tools_list/0},
        {"notification（无 id）→ 202 空体", fun test_notification/0},
        {"非法 JSON → 400 + JSON-RPC -32700", fun test_parse_error/0}
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

test_initialize() ->
    Req = <<
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\","
        "\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
        "\"clientInfo\":{\"name\":\"test\",\"version\":\"1\"}}}"
    >>,
    {Code, Body} = mcp_handler:process(Req),
    ?assertEqual(200, Code),
    Resp = json:decode(Body),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Resp)),
    ?assertEqual(1, maps:get(<<"id">>, Resp)),
    ?assert(maps:is_key(<<"result">>, Resp)),
    ?assert(maps:is_key(<<"protocolVersion">>, maps:get(<<"result">>, Resp))).

test_tools_list() ->
    Req = <<"{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}">>,
    {Code, Body} = mcp_handler:process(Req),
    ?assertEqual(200, Code),
    Resp = json:decode(Body),
    Result = maps:get(<<"result">>, Resp),
    ?assert(is_list(maps:get(<<"tools">>, Result))).

test_notification() ->
    %% 无 id 的通知：JSON-RPC 规定无响应 → 202 空体
    Req = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}">>,
    {Code, Body} = mcp_handler:process(Req),
    ?assertEqual(202, Code),
    ?assertEqual(<<>>, Body).

test_parse_error() ->
    {Code, Body} = mcp_handler:process(<<"not json{{{">>),
    ?assertEqual(400, Code),
    Resp = json:decode(Body),
    Err = maps:get(<<"error">>, Resp),
    ?assertEqual(-32700, maps:get(<<"code">>, Err)).
