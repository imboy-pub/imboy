-module(workspace_guard_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% 双体验 v2.5.2 WP4/T7 — workspace_guard 单元测试
%%% 覆盖：personal 放行（回归红线）、workspace active 放行、archived 拒绝
%%% （稳定错误码 980）、资源不存在放行、FOR UPDATE 行锁 SQL 形态、
%%% abort_on_error 助手、archived_error_code 导出。

-define(WS_ID, 800001).
-define(GID, 777001).
-define(CID, 666001).

%%% resolver mock：按资源返回 scope 解析结果
resolver_mocks(GroupScope, ChannelScope) ->
    [
        {workspace_resolver, [
            {'resolve_workspace', 1, fun
                ({group, ?GID}) when GroupScope =:= workspace -> {ok, ?WS_ID};
                ({group, ?GID}) when GroupScope =:= personal -> personal;
                ({group, _}) -> {error, not_found};
                ({channel, ?CID}) when ChannelScope =:= workspace -> {ok, ?WS_ID};
                ({channel, ?CID}) when ChannelScope =:= personal -> personal;
                ({channel, _}) -> {error, not_found};
                ({workspace, ?WS_ID}) -> {ok, ?WS_ID};
                (_) -> personal
            end}
        ]}
    ].

guard_mocks(Status) ->
    resolver_mocks(workspace, workspace) ++
        [
            {elib_pg, [
                {'one', 2, fun(<<"SELECT status FROM workspace", _/binary>>, _) ->
                    case Status of
                        none -> {error, db_error};
                        _ -> {ok, #{<<"status">> => Status}}
                    end
                end},
                %% 仅接受 FOR UPDATE 行锁语句（SQL 形态断言：非 FOR UPDATE 会 function_clause）
                {'query', 3, fun(
                    fake_conn, <<"SELECT status FROM workspace WHERE id = $1 FOR UPDATE">>, [?WS_ID]
                ) ->
                    {ok, [#{<<"status">> => Status}]}
                end}
            ]}
        ].

%% ⚠️ eunit 不解释 {Desc, fun} 返回的 {setup,...} spec（探针实证），
%% ?WITH_MECKS 包在 {Desc, fun} 体内 = 静默空转。此 helper 立即执行等价语义：
%% setup → 执行断言 → cleanup，使断言真实生效（simple fun 与 generator 同进程，
%% Self 哨兵可用，无需改进程字典）。
run_with_mocks(MockConfigs, TestFun) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    erlang:error({mock_setup_failed, Module, Reason})
            end
        end,
        MockConfigs
    ),
    try
        TestFun()
    after
        lists:foreach(
            fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
            MockConfigs
        )
    end.

%%% ===================================================================
%%% 稳定错误码
%%% ===================================================================

error_code_test_() ->
    [
        {"archived_error_code is 980", fun() ->
            ?assertEqual(980, workspace_guard:archived_error_code())
        end},
        {"archived_error carries code and message", fun() ->
            ?assertMatch({error, {980, _}}, workspace_guard:archived_error())
        end},
        {"is_archived_error recognizes code only", fun() ->
            ?assert(workspace_guard:is_archived_error({error, {980, <<"x">>}})),
            ?assertNot(workspace_guard:is_archived_error({error, {403, <<"x">>}})),
            ?assertNot(workspace_guard:is_archived_error(ok))
        end}
    ].

%%% ===================================================================
%%% 自动提交版（前置检查）
%%% ===================================================================

ensure_writable_test_() ->
    [
        {"personal group passes (regression red line)", fun() ->
            run_with_mocks(resolver_mocks(personal, personal), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable({group, ?GID}))
            end)
        end},
        {"active workspace passes", fun() ->
            run_with_mocks(guard_mocks(<<"active">>), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable({group, ?GID}))
            end)
        end},
        {"archived workspace rejected with 980", fun() ->
            run_with_mocks(guard_mocks(<<"archived">>), fun() ->
                ?assertMatch(
                    {error, {980, _}}, workspace_guard:ensure_writable({group, ?GID})
                )
            end)
        end},
        {"resource not found passes (existing 404 flow)", fun() ->
            run_with_mocks(resolver_mocks(workspace, workspace), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable({group, 999999}))
            end)
        end},
        {"workspace channel guarded too", fun() ->
            run_with_mocks(guard_mocks(<<"archived">>), fun() ->
                ?assertMatch(
                    {error, {980, _}}, workspace_guard:ensure_writable({channel, ?CID})
                )
            end)
        end},
        {"personal channel passes even when workspace archived", fun() ->
            run_with_mocks(resolver_mocks(workspace, personal), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable({channel, ?CID}))
            end)
        end}
    ].

%%% ===================================================================
%%% 事务版（FOR UPDATE 行锁）
%%% ===================================================================

ensure_writable_tx_test_() ->
    [
        {"tx guard uses FOR UPDATE row lock", fun() ->
            %% mock 里 query/3 只接受 FOR UPDATE 语句（SQL 形态断言：
            %% 非行锁形态会 function_clause 崩掉测试）
            run_with_mocks(guard_mocks(<<"active">>), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable_tx(fake_conn, {group, ?GID}))
            end)
        end},
        {"tx guard archived rejected 980", fun() ->
            run_with_mocks(guard_mocks(<<"archived">>), fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    workspace_guard:ensure_writable_tx(fake_conn, {group, ?GID})
                )
            end)
        end},
        {"tx guard personal passes without lock query", fun() ->
            run_with_mocks(resolver_mocks(personal, personal), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable_tx(fake_conn, {group, ?GID}))
            end)
        end}
    ].

%%% ===================================================================
%%% SEC-03 fail-closed：归属解析失败 / status 读失败 → 503 拒绝
%%% ===================================================================

fail_closed_test_() ->
    [
        {"resolver db error rejected with 503", fun() ->
            run_with_mocks(
                [
                    {workspace_resolver, [
                        {'resolve_workspace', 1, fun
                            %% resolver 真实 DB 故障形态=throw（见其模块头 M-1 契约），
                            %% 由 guard catch 臂承接 fail-closed
                            ({group, ?GID}) -> erlang:error({resolver_db_error, pool_exhausted});
                            (_) -> personal
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {503, _}}, workspace_guard:ensure_writable({group, ?GID})
                    )
                end
            )
        end},
        {"resolver unsupported resource rejected with 503", fun() ->
            run_with_mocks(
                [
                    {workspace_resolver, [
                        {'resolve_workspace', 1, fun
                            ({moment, 1}) -> {error, {unsupported_resource, {moment, 1}}};
                            (_) -> personal
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {503, _}}, workspace_guard:ensure_writable({moment, 1})
                    )
                end
            )
        end},
        {"status read db error rejected with 503 (autocommit guard)", fun() ->
            %% guard_mocks(none)：status 查询返回 {error, db_error}
            run_with_mocks(guard_mocks(none), fun() ->
                ?assertMatch(
                    {error, {503, _}}, workspace_guard:ensure_writable({group, ?GID})
                )
            end)
        end},
        {"missing workspace row keeps passthrough semantics", fun() ->
            %% workspace 行不存在（{ok, #{} 默认值）：既有语义放行（非 DB 故障）
            run_with_mocks(
                resolver_mocks(workspace, workspace) ++
                    [
                        {elib_pg, [
                            {'one', 2, fun(<<"SELECT status FROM workspace", _/binary>>, _) ->
                                {ok, #{}}
                            end}
                        ]}
                    ],
                fun() ->
                    ?assertEqual(ok, workspace_guard:ensure_writable({group, ?GID}))
                end
            )
        end},
        {"tx guard resolver db error rejected with 503", fun() ->
            run_with_mocks(
                [
                    {workspace_resolver, [
                        {'resolve_workspace', 1, fun
                            %% 同上：真实形态=throw，catch 臂承接
                            ({channel, ?CID}) -> erlang:error({resolver_db_error, no_connection});
                            (_) -> personal
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, {503, _}},
                        workspace_guard:ensure_writable_tx(fake_conn, {channel, ?CID})
                    )
                end
            )
        end},
        {"tx guard lock failure stays 503 (regression)", fun() ->
            run_with_mocks(
                resolver_mocks(workspace, workspace) ++
                    [
                        {elib_pg, [
                            {'one', 2, fun(_, _) -> {ok, #{<<"status">> => <<"active">>}} end},
                            {'query', 3, fun(_, _, _) -> {error, lock_timeout} end}
                        ]}
                    ],
                fun() ->
                    ?assertMatch(
                        {error, {503, _}},
                        workspace_guard:ensure_writable_tx(fake_conn, {group, ?GID})
                    )
                end
            )
        end}
    ].

%%% ===================================================================
%%% abort_on_error 助手
%%% ===================================================================

abort_on_error_test_() ->
    [
        {"ok passthrough", fun() ->
            ?assertEqual(ok, workspace_guard:abort_on_error(ok))
        end},
        {"error throws abort_tx", fun() ->
            ?assertThrow(
                {abort_tx, {980, <<"x">>}},
                workspace_guard:abort_on_error({error, {980, <<"x">>}})
            )
        end}
    ].

%%% ===================================================================
%%% M-1 收口：DB 异常 fail-closed（原 one_row 吞异常 → not_found → 放行）
%%% ===================================================================

ensure_writable_resolver_db_error_returns_503_test_() ->
    ?WITH_MECKS(
        [
            {workspace_resolver, [
                %% 模拟 one_row 抛出的解析层 DB 异常
                {'resolve_workspace', 1, fun(_) ->
                    erlang:error({resolver_db_error, simulated_db_down})
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {503, _}}, workspace_guard:ensure_writable({group, ?GID})
            )
        end
    ).

ensure_writable_status_query_error_returns_503_test_() ->
    %% guard_mocks(none)：resolve 成功但 workspace 状态查询返回 {error, db_error}
    ?WITH_MECKS(guard_mocks(none), fun() ->
        ?assertMatch(
            {error, {503, _}}, workspace_guard:ensure_writable({group, ?GID})
        )
    end).

ensure_writable_tx_resolver_db_error_returns_503_test_() ->
    ?WITH_MECKS(
        [
            {workspace_resolver, [
                {'resolve_workspace', 1, fun(_) ->
                    erlang:error({resolver_db_error, simulated_db_down})
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {503, _}},
                workspace_guard:ensure_writable_tx(fake_conn, {group, ?GID})
            )
        end
    ).
