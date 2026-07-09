%%%-------------------------------------------------------------------
%%% @doc Tests for barrel_mcp_protocol module.
%%% @end
%%%-------------------------------------------------------------------
-module(barrel_mcp_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test handlers (exported for MCP registry)
-export([sample_tool/1]).

%%====================================================================
%% Test Fixtures
%%====================================================================

protocol_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Decode valid JSON", fun test_decode_valid/0},
        {"Decode invalid JSON", fun test_decode_invalid/0},
        {"Encode response", fun test_encode/0},
        {"Handle initialize", fun test_handle_initialize/0},
        {"Handle ping", fun test_handle_ping/0},
        {"Handle tools/list", fun test_handle_tools_list/0},
        {"Handle tools/call", fun test_handle_tools_call/0},
        {"Handle tools/call not found", fun test_handle_tools_call_not_found/0},
        {"Handle resources/list", fun test_handle_resources_list/0},
        {"Handle prompts/list", fun test_handle_prompts_list/0},
        {"Handle notification", fun test_handle_notification/0},
        {"Handle unknown method", fun test_handle_unknown_method/0},
        {"Handle invalid request", fun test_handle_invalid_request/0}
    ]}.

setup() ->
    %% vendored 上下文无 barrel_mcp OTP app：直接启协议引擎所需的 gen_server
    %% （registry + session），避开 application:ensure_all_started(barrel_mcp)。
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
    %% Clean up registered handlers
    lists:foreach(
        fun({Name, _}) ->
            barrel_mcp_registry:unreg(tool, Name)
        end,
        barrel_mcp_registry:all(tool)
    ),
    ok.

%%====================================================================
%% Test Helpers
%%====================================================================

sample_tool(Args) ->
    <<"Result: ", (maps:get(<<"input">>, Args, <<"default">>))/binary>>.

%%====================================================================
%% Tests
%%====================================================================

test_decode_valid() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}">>,
    {ok, #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := <<"ping">>}} =
        barrel_mcp_protocol:decode(Json).

test_decode_invalid() ->
    {error, parse_error} = barrel_mcp_protocol:decode(<<"not json">>).

test_encode() ->
    Response = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => #{}},
    Encoded = barrel_mcp_protocol:encode(Response),
    ?assert(is_binary(Encoded)).

test_handle_initialize() ->
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{}
    },
    Response = barrel_mcp_protocol:handle(Request),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(1, maps:get(<<"id">>, Response)),
    Result = maps:get(<<"result">>, Response),
    ?assertEqual(<<"2025-11-25">>, maps:get(<<"protocolVersion">>, Result)),
    ?assert(maps:is_key(<<"capabilities">>, Result)),
    ?assert(maps:is_key(<<"serverInfo">>, Result)).

test_handle_ping() ->
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"ping">>
    },
    Response = barrel_mcp_protocol:handle(Request),
    ?assertEqual(#{}, maps:get(<<"result">>, Response)).

test_handle_tools_list() ->
    %% Register a tool first
    ok = barrel_mcp_registry:reg(tool, <<"test_tool">>, ?MODULE, sample_tool, #{
        description => <<"A test tool">>
    }),
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/list">>
    },
    Response = barrel_mcp_protocol:handle(Request),
    Result = maps:get(<<"result">>, Response),
    Tools = maps:get(<<"tools">>, Result),
    ?assert(length(Tools) >= 1),
    barrel_mcp_registry:unreg(tool, <<"test_tool">>).

test_handle_tools_call() ->
    %% `tools/call' is now async: handle/2 returns {async, AsyncPlan}
    %% with a `spawn' fun the transport invokes. Drive it inline here.
    ok = barrel_mcp_registry:reg(tool, <<"echo">>, ?MODULE, sample_tool, #{}),
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"echo">>,
            <<"arguments">> => #{<<"input">> => <<"hello">>}
        }
    },
    {async, Plan} = barrel_mcp_protocol:handle(Request),
    Self = self(),
    Ctx = #{
        request_id => 1,
        reply_to => Self,
        session_id => undefined,
        progress_token => undefined,
        emit_progress => fun(_, _, _) -> ok end
    },
    _Pid = (maps:get(spawn, Plan))(Ctx),
    receive
        {tool_result, 1, Result} ->
            Content = barrel_mcp_protocol:format_tool_result_external(Result),
            ?assert(length(Content) >= 1)
    after 2000 ->
        ?assert(false)
    end,
    barrel_mcp_registry:unreg(tool, <<"echo">>).

test_handle_tools_call_not_found() ->
    %% A missing tool surfaces via the worker as `tool_failed' with
    %% the registry's not_found error.
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"nonexistent">>,
            <<"arguments">> => #{}
        }
    },
    {async, Plan} = barrel_mcp_protocol:handle(Request),
    Self = self(),
    Ctx = #{
        request_id => 1,
        reply_to => Self,
        session_id => undefined,
        progress_token => undefined,
        emit_progress => fun(_, _, _) -> ok end
    },
    _Pid = (maps:get(spawn, Plan))(Ctx),
    receive
        {tool_failed, 1, {error, {not_found, tool, <<"nonexistent">>}}} -> ok
    after 2000 ->
        ?assert(false)
    end.

test_handle_resources_list() ->
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"resources/list">>
    },
    Response = barrel_mcp_protocol:handle(Request),
    Result = maps:get(<<"result">>, Response),
    ?assert(maps:is_key(<<"resources">>, Result)).

test_handle_prompts_list() ->
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"prompts/list">>
    },
    Response = barrel_mcp_protocol:handle(Request),
    Result = maps:get(<<"result">>, Response),
    ?assert(maps:is_key(<<"prompts">>, Result)).

test_handle_notification() ->
    %% Notifications have no id
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialized">>,
        <<"params">> => #{}
    },
    Response = barrel_mcp_protocol:handle(Request),
    ?assertEqual(no_response, Response).

test_handle_unknown_method() ->
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"unknown/method">>
    },
    Response = barrel_mcp_protocol:handle(Request),
    ?assert(maps:is_key(<<"error">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)).

test_handle_invalid_request() ->
    %% Missing method
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1},
    Response = barrel_mcp_protocol:handle(Request),
    ?assert(maps:is_key(<<"error">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32600, maps:get(<<"code">>, Error)).
