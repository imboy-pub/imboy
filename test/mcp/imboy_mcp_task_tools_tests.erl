-module(imboy_mcp_task_tools_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% MCP-02：Agent Task tools（真库；身份取 Ctx Principal；幂等/审批闭环）。

principal() ->
    Hex = binary:encode_hex(crypto:strong_rand_bytes(6), lowercase),
    #{owner_uid => 42, client_id => 7, client_key => <<"mck-", Hex/binary>>}.

gid() -> erlang:unique_integer([positive]) + 900000.

%% A01+A03：create → 等待并唯一审批 → poll 读到 approved；
%% 重放相同 idempotency_key → 同一 task/correlation，不产生第二任务。
create_approve_poll_replay_test_() ->
    ?TEST_WITH_DB(fun() ->
        setup_group_ds([10, 11]),
        P = principal(),
        Ctx = #{auth_info => P},
        Args = #{
            <<"group_id">> => gid(),
            <<"tool">> => <<"mcp.report">>,
            <<"idempotency_key">> => <<"k1">>
        },
        {structured, Created} = imboy_mcp_tools:create_agent_task(Args, Ctx),
        TaskId = maps:get(<<"task_id">>, Created),
        Corr = maps:get(<<"correlation_id">>, Created),
        %% 重放同 key：同一任务、同一 correlation（A03）；created=false 标记重放
        {structured, Replayed} = imboy_mcp_tools:create_agent_task(Args, Ctx),
        ?assertEqual(false, maps:get(<<"created">>, Replayed)),
        ?assertEqual(maps:get(<<"task_id">>, Created), maps:get(<<"task_id">>, Replayed)),
        ?assertEqual(Corr, maps:get(<<"correlation_id">>, Replayed)),
        ?assertEqual(TaskId, maps:get(<<"task_id">>, Replayed)),
        %% working → 请求审批
        {structured, _} = imboy_mcp_tools:update_agent_task(
            #{<<"task_id">> => TaskId, <<"action">> => <<"start">>}, Ctx
        ),
        {structured, _} = imboy_mcp_tools:request_task_approval(
            #{<<"task_id">> => TaskId}, Ctx
        ),
        %% 人工审批（first-writer-wins）
        ?assertEqual({ok, approved}, agent_task_observer:approve(TaskId, 10)),
        ?assertEqual({error, already_decided}, agent_task_observer:approve(TaskId, 11)),
        %% poll 权威兜底：状态与决定一致
        {structured, St} = imboy_mcp_tools:get_agent_task(#{<<"task_id">> => TaskId}, Ctx),
        ?assertEqual(<<"approved">>, maps:get(<<"status">>, St)),
        ?assertEqual(10, maps:get(<<"decided_by">>, St)),
        %% correlation 不变
        {ok, Row} = agent_task_repo:get_task(TaskId),
        ?assertEqual(Corr, maps:get(<<"correlation_id">>, Row)),
        cleanup_group_ds()
    end).

%% A01 负例：未审批（pending）client 的任务可建但 gate 会拦 tools/call（MCP-01 已测）；
%% 此处验证 create 幂等键跨 client 隔离（同 key 不同 client = 两个任务）
idem_scoped_per_client_test_() ->
    ?TEST_WITH_DB(fun() ->
        P1 = principal(),
        P2 = principal(),
        Args1 = #{<<"group_id">> => gid(), <<"idempotency_key">> => <<"kA">>},
        {structured, C1} = imboy_mcp_tools:create_agent_task(Args1, #{auth_info => P1}),
        {structured, C2} = imboy_mcp_tools:create_agent_task(Args1, #{auth_info => P2}),
        ?assertNotEqual(maps:get(<<"task_id">>, C1), maps:get(<<"task_id">>, C2))
    end).

%% 身份不可自报：无 auth_info / Args 伪造 → 拒绝
unauthenticated_rejected_test_() ->
    ?TEST_WITH_DB(fun() ->
        {tool_error, _} = imboy_mcp_tools:create_agent_task(
            #{<<"idempotency_key">> => <<"k">>}, #{}
        ),
        {tool_error, _} = imboy_mcp_tools:get_agent_task(
            #{<<"task_id">> => <<"x">>}, #{}
        ),
        ok
    end).

setup_group_ds(Members) ->
    try
        meck_helper:cleanup_mock(group_ds)
    catch
        _:_ -> ok
    end,
    case
        meck_helper:setup_mock(
            group_ds, [{'member_uids', 1, fun(_) -> Members end}]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({group_ds_mock, Reason})
    end.

cleanup_group_ds() ->
    try
        meck_helper:cleanup_mock(group_ds)
    catch
        _:_ -> ok
    end,
    ok.
