-module(mcp_handler_auth_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc mcp_handler JWT/AuthInfo 注入测试（Phase 3 T3.3）
%%% 验收：调用者 uid 经 drive_async_plan/3 注入 tool Ctx.auth_info，
%%%       tool 内拿得到 uid（越权校验的地基）；未认证=0。
%%%===================================================================

%% 测试用 tool：回显 Ctx.auth_info（arity 2 → run_tool 传 Ctx）
-export([whoami/2]).

whoami(_Args, Ctx) ->
    case maps:get(auth_info, Ctx, undefined) of
        Uid when is_integer(Uid) -> <<"uid:", (integer_to_binary(Uid))/binary>>;
        _ -> <<"uid:none">>
    end.

auth_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"tool Ctx.auth_info = 调用者 uid", fun test_auth_flows_to_tool/0},
        {"未认证 uid=0 仍可区分", fun test_unauth_uid_zero/0}
    ]}.

setup() ->
    _ = start_srv(barrel_mcp_registry),
    _ = start_srv(barrel_mcp_session),
    ok = barrel_mcp_registry:wait_for_ready(),
    ok = barrel_mcp_registry:reg(tool, <<"whoami">>, ?MODULE, whoami, #{
        description => <<"echo caller uid">>
    }),
    ok.

start_srv(Mod) ->
    case Mod:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

cleanup(_) ->
    catch barrel_mcp_registry:unreg(tool, <<"whoami">>),
    ok.

call_whoami(AuthInfo) ->
    Req = <<
        "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\","
        "\"params\":{\"name\":\"whoami\",\"arguments\":{}}}"
    >>,
    {Code, Body} = mcp_handler:process(Req, AuthInfo),
    ?assertEqual(200, Code),
    Resp = json:decode(Body),
    %% 结果在 result.content[].text
    Result = maps:get(<<"result">>, Resp),
    [#{<<"text">> := Text} | _] = maps:get(<<"content">>, Result),
    Text.

test_auth_flows_to_tool() ->
    ?assertEqual(<<"uid:42">>, call_whoami(42)).

test_unauth_uid_zero() ->
    ?assertEqual(<<"uid:0">>, call_whoami(0)).
