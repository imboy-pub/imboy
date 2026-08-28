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
    %% 套件隔离治理：本 fixture 启动的 registry/session 必须在 cleanup 停掉
    %% （normal exit 信号不会终止被链接者，不停即成不死孤儿）。
    Reg = start_srv(barrel_mcp_registry),
    Sess = start_srv(barrel_mcp_session),
    ok = barrel_mcp_registry:wait_for_ready(),
    ok = barrel_mcp_registry:reg(tool, <<"whoami">>, ?MODULE, whoami, #{
        description => <<"echo caller uid">>
    }),
    #{reg => Reg, sess => Sess}.

start_srv(Mod) ->
    case Mod:start_link() of
        {ok, Pid} -> {owned, Pid};
        {error, {already_started, Pid}} -> {borrowed, Pid}
    end.

%% @doc 只回收本 fixture 启动的实例；borrowed 归其所有者（app/更早 fixture）。
stop_owned({owned, Pid}) ->
    try
        gen:stop(Pid, normal, 1000)
    catch
        _:_ -> ok
    end,
    ok;
stop_owned(_) ->
    ok.

cleanup(Ctx) ->
    catch barrel_mcp_registry:unreg(tool, <<"whoami">>),
    stop_owned(maps:get(reg, Ctx, undefined)),
    stop_owned(maps:get(sess, Ctx, undefined)),
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
