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
            ?WITH_MECKS(resolver_mocks(personal, personal), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable({group, ?GID}))
            end)
        end},
        {"active workspace passes", fun() ->
            ?WITH_MECKS(guard_mocks(<<"active">>), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable({group, ?GID}))
            end)
        end},
        {"archived workspace rejected with 980", fun() ->
            ?WITH_MECKS(guard_mocks(<<"archived">>), fun() ->
                ?assertMatch(
                    {error, {980, _}}, workspace_guard:ensure_writable({group, ?GID})
                )
            end)
        end},
        {"resource not found passes (existing 404 flow)", fun() ->
            ?WITH_MECKS(resolver_mocks(workspace, workspace), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable({group, 999999}))
            end)
        end},
        {"workspace channel guarded too", fun() ->
            ?WITH_MECKS(guard_mocks(<<"archived">>), fun() ->
                ?assertMatch(
                    {error, {980, _}}, workspace_guard:ensure_writable({channel, ?CID})
                )
            end)
        end},
        {"personal channel passes even when workspace archived", fun() ->
            ?WITH_MECKS(resolver_mocks(workspace, personal), fun() ->
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
            ?WITH_MECKS(guard_mocks(<<"active">>), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable_tx(fake_conn, {group, ?GID}))
            end)
        end},
        {"tx guard archived rejected 980", fun() ->
            ?WITH_MECKS(guard_mocks(<<"archived">>), fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    workspace_guard:ensure_writable_tx(fake_conn, {group, ?GID})
                )
            end)
        end},
        {"tx guard personal passes without lock query", fun() ->
            ?WITH_MECKS(resolver_mocks(personal, personal), fun() ->
                ?assertEqual(ok, workspace_guard:ensure_writable_tx(fake_conn, {group, ?GID}))
            end)
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
