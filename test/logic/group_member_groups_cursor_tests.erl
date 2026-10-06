-module(group_member_groups_cursor_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% 游标契约（与 channel 列表一致，WorkspaceApi._memberGroupRows 依赖）：
%% has_more=true → next_cursor=末行 id 续页；has_more=false（含非空末页）→ next_cursor=0。
%% 回归背景（EO-20261006 WS 段 r6）：非空末页曾返回末行 id，客户端按终止
%% 标记校验失败抛 503 网络错误。原契约断言在孤儿套件
%% workspace_member_groups_pg_tests（run/1 无人接线）中，从未入全量门。

member_groups_cursor_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'member_groups', 5, fun(_Ws, _Uid, _After, _Limit, _Preview) ->
                    Count =
                        case get(row_count) of
                            undefined -> 0;
                            N -> N
                        end,
                    {ok, [
                        #{<<"id">> => I, <<"title">> => <<"g">>}
                     || I <- lists:seq(901, 900 + Count)
                    ]}
                end}
            ]}
        ],
        fun() ->
            %% 非空末页：ds 恰好返回 Limit 行 → has_more=false、next_cursor=0
            put(row_count, 3),
            ?assertMatch(
                {ok, #{has_more := false, next_cursor := 0}},
                group_logic:list_member_workspace_groups(100, 1, 0, 3, false)
            ),
            %% 空末页：next_cursor=0
            put(row_count, 0),
            ?assertEqual(
                {ok, #{list => [], has_more => false, next_cursor => 0}},
                group_logic:list_member_workspace_groups(100, 1, 0, 3, false)
            ),
            %% 续页：ds 返回 Limit+1 行 → has_more=true、next_cursor=第 Limit 行 id
            put(row_count, 4),
            ?assertMatch(
                {ok, #{has_more := true, next_cursor := 903}},
                group_logic:list_member_workspace_groups(100, 1, 0, 3, false)
            )
        end
    ).
