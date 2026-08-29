-module(project_channel_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% ZC-04 — project_channel_logic / project_channel_ds 单元测试（mock，不起 HTTP）
%%%
%%% 覆盖（W2 计划 TDD 用例 1/2/3/5/7/8/10 的应用层语义 + SQL 查询数上限）：
%%%   * 权限矩阵：Project Owner / active Project Member 可写；workspace role=guest
%%%     只读（写 403）；无 active project_member 关系 → 403；项目不存在 → 404；
%%%     Owner 的 ws 身份失效时读写均 403 fail-closed（M-1：读路径同写路径）
%%%   * link 事件原子性：channel_linked 事件与 rel 插入同一事务连接（fake_conn）
%%%   * 重复 link 幂等：existing 不重复写事件
%%%   * unlink 缺失关联 → 404（决策：unlink 定向删除语义取 404，link 幂等吸收）
%%%   * 跨 Workspace / personal 频道前置校验 400（DB 23503 语义的等价前置）
%%%   * update-links 应用层形状校验（DB trg_project_links_shape 之外的入参防线）
%%%   * Activity/Resources/Related Posts 输出不含正文字段（列白名单 + payload 清洗）
%%%   * SQL 查询数上限：每个聚合请求固定 1-2 条 SQL（无 N+1，与数据量无关）
%%%
%%% 结构说明（ZC-09R H-1 改造）：?WITH_MECK_TESTS 是本文件本地宏
%%%（{setup, S, C, [用例]} 规范 context 结构）——"{Desc, fun() -> fixture end}"
%%% 包装式会让 EUnit 空转判 ok（内层断言从不执行），勿回退该形态。
%%% 同组内用例共享同一 meck 实例与测试进程：断言 num_calls 的用例开头必须
%%% meck:reset（防跨用例累积），断言收发消息的用例开头必须 drain_msgs
%%%（防跨用例邮箱串态）。

-define(WS_ID, 810001).
-define(WS2_ID, 810002).
-define(OWNER, 910001).
-define(MEMBER2, 910002).
-define(GUEST, 910003).
-define(OUTSIDER, 910004).
-define(PROJECT_ID, 710001).
-define(CH_ID, 610001).

%% 本地宏：一次 mock 安装 + 多个真实执行的内层用例（EUnit 规范 context）
-define(WITH_MECK_TESTS(MockConfigs, Tests),
    {setup,
        fun() ->
            lists:foreach(
                fun({Module, Expectations}) ->
                    case meck_helper:setup_mock(Module, Expectations) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            ?debugFmt("Mock setup failed for ~p: ~p", [Module, Reason])
                    end
                end,
                MockConfigs
            )
        end,
        fun(_) ->
            lists:foreach(
                fun({Module, _Expectations}) -> meck_helper:cleanup_mock(Module) end,
                MockConfigs
            )
        end,
        Tests}
).

%%% ===================================================================
%%% 用例间复位（同组用例共享进程：邮箱串态防线）
%%% ===================================================================

drain_msgs() ->
    receive
        _ -> drain_msgs()
    after 0 -> ok
    end.

%%% ===================================================================
%%% 权限矩阵（logic 层，mock repo）
%%% ===================================================================

%% 项目行（find_by_id 返回）
project_row() ->
    #{<<"id">> => ?PROJECT_ID, <<"workspace_id">> => ?WS_ID, <<"owner_id">> => ?OWNER}.

%% project_member 行（find_project_member 返回）
pm_row(Status) ->
    #{
        <<"project_id">> => ?PROJECT_ID,
        <<"user_id">> => ?MEMBER2,
        <<"workspace_id">> => ?WS_ID,
        <<"status">> => Status
    }.

%% 权限相关 mock（可按用例覆写）
perm_mocks() ->
    perm_mocks(#{}).

perm_mocks(Over) ->
    ProjectRow = maps:get(project_row, Over, project_row()),
    PmRow = maps:get(pm_row, Over, pm_row(<<"active">>)),
    WsRole = maps:get(ws_role, Over, <<"member">>),
    WsStatus = maps:get(ws_status, Over, <<"active">>),
    [
        {project_repo, [
            {'find_by_id', 2, fun
                (?PROJECT_ID, _) -> ProjectRow;
                (_, _) -> #{}
            end}
        ]},
        {project_channel_rel_repo, [
            {'find_project_member', 2, fun
                (_Pid, ?OWNER) ->
                    (pm_row(<<"active">>))#{<<"user_id">> => ?OWNER};
                (_Pid, Uid) when Uid =:= ?MEMBER2; Uid =:= ?GUEST ->
                    %% Over.pm_row 覆写命中对应 uid 时生效（原实现 PmRow 计算后
                    %% 未使用——ZC-09R 真实执行暴露，removed 用例假绿）
                    case maps:get(<<"user_id">>, PmRow, undefined) of
                        Uid -> PmRow;
                        _ -> (pm_row(<<"active">>))#{<<"user_id">> => Uid}
                    end;
                (_, _) ->
                    #{}
            end}
        ]},
        {workspace_member_repo, [
            {'find', 3, fun(_Ws, _Uid, _Col) ->
                #{<<"role">> => WsRole, <<"status">> => WsStatus}
            end}
        ]},
        {project_channel_ds, [
            {'link', 3, fun(_Uid, _Pid, _Chid) -> {ok, created} end},
            {'unlink', 3, fun(_Uid, _Pid, _Chid) -> {ok, unlinked} end},
            {'update_links', 3, fun(_Uid, _Pid, Links) -> {ok, Links} end},
            {'list_channels', 3, fun(_Pid, _Page, _Size) -> {ok, #{list => []}} end},
            {'pinned', 3, fun(_Pid, _Page, _Size) -> {ok, #{list => []}} end},
            {'related_posts', 1, fun(_Pid) -> {ok, []} end},
            {'activity', 3, fun(_Pid, _Page, _Size) -> {ok, #{list => []}} end},
            {'resources', 1, fun(_Pid) -> {ok, []} end}
        ]}
    ].

%%% 写权限（默认 mock：owner/member 可写、无关系 403）

write_permission_default_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(), [
        {"owner can link", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, created},
                project_channel_logic:link(?OWNER, ?PROJECT_ID, ?CH_ID)
            )
        end},
        {"active project member can link", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, created},
                project_channel_logic:link(?MEMBER2, ?PROJECT_ID, ?CH_ID)
            )
        end},
        {"no project membership: link 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:link(?OUTSIDER, ?PROJECT_ID, ?CH_ID)
            )
        end}
    ]).

write_permission_guest_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(#{ws_role => <<"guest">>}), [
        {"guest (ws role) project member read-only: link 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:link(?GUEST, ?PROJECT_ID, ?CH_ID)
            )
        end}
    ]).

write_permission_removed_pm_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(#{pm_row => pm_row(<<"removed">>)}), [
        {"removed project member: link 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:link(?MEMBER2, ?PROJECT_ID, ?CH_ID)
            )
        end}
    ]).

write_permission_unknown_project_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(#{project_row => #{}}), [
        {"unknown project: 404", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {404, _}},
                project_channel_logic:link(?OWNER, ?PROJECT_ID, ?CH_ID)
            )
        end}
    ]).

write_permission_owner_ws_inactive_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(#{ws_status => <<"removed">>}), [
        {"owner with no active ws membership: write 403 (fail-closed)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:link(?OWNER, ?PROJECT_ID, ?CH_ID)
            )
        end}
    ]).

%%% 读权限（M-1：owner 读分支同样要求 active ws membership）

read_permission_guest_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(#{ws_role => <<"guest">>}), [
        {"guest project member can read resources (read-only)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, _}, project_channel_logic:resources(?GUEST, ?PROJECT_ID)
            )
        end},
        {"guest can read pinned aggregation", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, _}, project_channel_logic:pinned(?GUEST, ?PROJECT_ID, 1, 20)
            )
        end}
    ]).

read_permission_default_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(), [
        {"removed project member cannot read: 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:resources(?OUTSIDER, ?PROJECT_ID)
            )
        end}
    ]).

read_permission_unknown_project_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(#{project_row => #{}}), [
        {"unknown project read: 404", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {404, _}},
                project_channel_logic:resources(?OWNER, ?PROJECT_ID)
            )
        end}
    ]).

read_permission_owner_ws_inactive_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(#{ws_status => <<"removed">>}), [
        {"owner with no active ws membership: read 403 (fail-closed, M-1)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:resources(?OWNER, ?PROJECT_ID)
            )
        end}
    ]).

%%% ===================================================================
%%% update-links 应用层形状校验（case 7 应用侧防线）
%%% ===================================================================

valid_link() ->
    #{<<"name">> => <<"Docs">>, <<"url">> => <<"https://example.com/a">>}.

long_name_link() ->
    #{<<"name">> => binary:copy(<<"n">>, 201), <<"url">> => <<"https://e.com">>}.

long_url_link() ->
    #{
        <<"name">> => <<"n">>,
        <<"url">> => <<"https://e.com/", (binary:copy(<<"u">>, 2048))/binary>>
    }.

too_many_links() ->
    [
        #{
            <<"name">> => <<"n", (integer_to_binary(I))/binary>>,
            <<"url">> => <<"https://e.com/", (integer_to_binary(I))/binary>>
        }
     || I <- lists:seq(1, 21)
    ].

links_validation_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(), [
        {"valid links pass through to ds", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            Links = [valid_link()],
            ?assertMatch({ok, _}, project_channel_logic:update_links(?OWNER, ?PROJECT_ID, Links)),
            ?assertEqual(1, meck:num_calls(project_channel_ds, update_links, 3))
        end},
        {"empty list allowed (clear links)", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            ?assertMatch({ok, []}, project_channel_logic:update_links(?OWNER, ?PROJECT_ID, [])),
            ?assertEqual(1, meck:num_calls(project_channel_ds, update_links, 3))
        end},
        {"non-list links rejected 400 without ds call", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(?OWNER, ?PROJECT_ID, <<"not-a-list">>)
            ),
            ?assertEqual(0, meck:num_calls(project_channel_ds, update_links, 3))
        end},
        {"element not an object rejected 400", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(?OWNER, ?PROJECT_ID, [<<"str">>])
            ),
            ?assertEqual(0, meck:num_calls(project_channel_ds, update_links, 3))
        end},
        {"missing url rejected 400", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(?OWNER, ?PROJECT_ID, [
                    #{<<"name">> => <<"Docs">>}
                ])
            ),
            ?assertEqual(0, meck:num_calls(project_channel_ds, update_links, 3))
        end},
        {"empty name rejected 400", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(?OWNER, ?PROJECT_ID, [
                    #{<<"name">> => <<>>, <<"url">> => <<"https://e.com">>}
                ])
            ),
            ?assertEqual(0, meck:num_calls(project_channel_ds, update_links, 3))
        end},
        {"name over 200 chars rejected 400", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(?OWNER, ?PROJECT_ID, [long_name_link()])
            )
        end},
        {"url over 2048 chars rejected 400", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(?OWNER, ?PROJECT_ID, [long_url_link()])
            )
        end},
        {"more than 20 links rejected 400", fun() ->
            drain_msgs(),
            meck:reset(project_channel_ds),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(?OWNER, ?PROJECT_ID, too_many_links())
            ),
            ?assertEqual(0, meck:num_calls(project_channel_ds, update_links, 3))
        end}
    ]).

%%% ===================================================================
%%% link/unlink 透传 + DS 事务事件原子性（fake_conn 模式，镜像 project_task_logic_tests）
%%% ===================================================================

tx_fun() ->
    fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end.

ws_guard_mocks(WsStatus) ->
    [
        {workspace_guard, [
            {'ensure_writable_tx', 2, fun(_Conn, _Target) ->
                case WsStatus of
                    <<"archived">> -> {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}};
                    _ -> ok
                end
            end},
            {'abort_on_error', 1, fun
                (ok) -> ok;
                ({error, Reason}) -> throw({abort_tx, Reason})
            end}
        ]}
    ].

link_tx_mocks(Extra) ->
    InsertResult = maps:get(insert_result, Extra, {ok, 1}),
    ChannelRow = maps:get(channel_row, Extra, #{
        <<"id">> => ?CH_ID,
        <<"workspace_id">> => ?WS_ID,
        <<"scope">> => <<"workspace">>,
        <<"status">> => 1,
        <<"name">> => <<"ch">>
    }),
    DeleteResult = maps:get(delete_result, Extra, {ok, 1}),
    WsMocks = ws_guard_mocks(<<"active">>),
    WsMocks ++
        [
            {project_repo, [
                {'find_by_id', 2, fun(?PROJECT_ID, _) -> project_row() end},
                {'find_tx', 3, fun(_Conn, ?PROJECT_ID, _) ->
                    #{<<"id">> => ?PROJECT_ID, <<"workspace_id">> => ?WS_ID}
                end},
                {'update_fields_tx', 3, fun(_Conn, _Pid, _Data) -> {ok, 1} end}
            ]},
            {project_channel_rel_repo, [
                {'find_project_member', 2, fun(_Pid, _Uid) -> pm_row(<<"active">>) end},
                {'find_channel_tx', 3, fun(_Conn, _Chid, _Col) -> ChannelRow end},
                {'insert_on_conflict_tx', 5, fun(_Conn, _Ws, _Pid, _Chid, _By) ->
                    %% self() 在 mock 调用时求值 = 测试用例进程（勿在构建期闭包捕获）
                    self() ! {rel_insert, InsertResult},
                    InsertResult
                end},
                {'delete_tx', 3, fun(_Conn, _Pid, _Chid) ->
                    self() ! {rel_delete, DeleteResult},
                    DeleteResult
                end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(_Ws, _Uid, _Col) ->
                    #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                end}
            ]},
            {project_event_repo, [
                {'insert_tx', 2, fun(Conn, Data) ->
                    self() ! {event_insert, Conn, Data},
                    {ok, 520001}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, tx_fun()},
                {'query', 3, fun(_C, _S, _P) -> {ok, []} end},
                {'execute', 3, fun(_C, _S, _P) -> {ok, 1} end}
            ]}
        ].

link_tx_event_atomicity_test_() ->
    ?WITH_MECK_TESTS(link_tx_mocks(#{}), [
        {"link insert + channel_linked event in same tx conn (case 1)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, created}, project_channel_logic:link(?OWNER, ?PROJECT_ID, ?CH_ID)
            ),
            receive
                {event_insert, Conn, Data} ->
                    %% 事件与 rel 插入同一事务连接（fake_conn）
                    ?assertEqual(fake_conn, Conn),
                    ?assertEqual(<<"channel_linked">>, maps:get(<<"event_type">>, Data)),
                    ?assertEqual(?PROJECT_ID, maps:get(<<"project_id">>, Data)),
                    ?assertEqual(?CH_ID, maps:get(<<"target_id">>, Data)),
                    ?assertEqual(?OWNER, maps:get(<<"actor_id">>, Data)),
                    Payload = jsone:decode(maps:get(<<"payload">>, Data), [{object_format, map}]),
                    ?assertEqual(?CH_ID, maps:get(<<"channel_id">>, Payload))
            after 500 ->
                ?assert(false, "channel_linked event not written")
            end
        end},
        {"unlink writes channel_unlinked event in same tx (case 5)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, unlinked}, project_channel_logic:unlink(?OWNER, ?PROJECT_ID, ?CH_ID)
            ),
            receive
                {event_insert, Conn, Data} ->
                    ?assertEqual(fake_conn, Conn),
                    ?assertEqual(<<"channel_unlinked">>, maps:get(<<"event_type">>, Data))
            after 500 ->
                ?assert(false, "channel_unlinked event not written")
            end
        end},
        {"update_links writes links_updated event in same tx", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, _},
                project_channel_logic:update_links(?OWNER, ?PROJECT_ID, [valid_link()])
            ),
            receive
                {event_insert, Conn, Data} ->
                    ?assertEqual(fake_conn, Conn),
                    ?assertEqual(<<"links_updated">>, maps:get(<<"event_type">>, Data))
            after 500 ->
                ?assert(false, "links_updated event not written")
            end
        end}
    ]).

link_duplicate_idempotent_test_() ->
    ?WITH_MECK_TESTS(link_tx_mocks(#{insert_result => {ok, 0}}), [
        {"duplicate link idempotent: no second event (case 3)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, existing}, project_channel_logic:link(?OWNER, ?PROJECT_ID, ?CH_ID)
            ),
            receive
                {rel_insert, {ok, 0}} -> ok
            after 500 ->
                ?assert(false, "insert not attempted")
            end,
            receive
                {event_insert, _, _} ->
                    ?assert(false, "duplicate event on idempotent link")
            after 0 ->
                ok
            end
        end}
    ]).

link_cross_workspace_test_() ->
    CrossCh = #{
        <<"id">> => ?CH_ID,
        <<"workspace_id">> => ?WS2_ID,
        <<"scope">> => <<"workspace">>,
        <<"status">> => 1,
        <<"name">> => <<"ch2">>
    },
    ?WITH_MECK_TESTS(link_tx_mocks(#{channel_row => CrossCh}), [
        {"cross workspace channel rejected 400 before insert (case 2)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:link(?OWNER, ?PROJECT_ID, ?CH_ID)
            ),
            receive
                {rel_insert, _} -> ?assert(false, "insert should be pre-checked")
            after 0 ->
                ok
            end
        end}
    ]).

link_personal_channel_test_() ->
    PersonalCh = #{
        <<"id">> => ?CH_ID,
        <<"workspace_id">> => null,
        <<"scope">> => <<"personal">>,
        <<"status">> => 1,
        <<"name">> => <<"p">>
    },
    ?WITH_MECK_TESTS(link_tx_mocks(#{channel_row => PersonalCh}), [
        {"personal channel rejected 400", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:link(?OWNER, ?PROJECT_ID, ?CH_ID)
            )
        end}
    ]).

link_archived_ws_test_() ->
    ?WITH_MECK_TESTS(link_tx_mocks(#{}) ++ ws_guard_mocks(<<"archived">>), [
        {"archived workspace: link 980 and no insert", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {980, _}},
                project_channel_logic:link(?OWNER, ?PROJECT_ID, ?CH_ID)
            ),
            receive
                {rel_insert, _} -> ?assert(false, "archived ws must reject before insert")
            after 0 ->
                ok
            end
        end}
    ]).

unlink_missing_test_() ->
    ?WITH_MECK_TESTS(link_tx_mocks(#{delete_result => {ok, 0}}), [
        {"unlink missing rel: 404 and no event (case 5 decision)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {404, _}},
                project_channel_logic:unlink(?OWNER, ?PROJECT_ID, ?CH_ID)
            ),
            receive
                {event_insert, _, _} -> ?assert(false, "no event for missing rel")
            after 0 ->
                ok
            end
        end}
    ]).

%%% ===================================================================
%%% 输出边界：Related Posts / Activity 无正文字段（case 8/9 元数据断言）
%%% ===================================================================

metadata_only_test_() ->
    ?WITH_MECK_TESTS(perm_mocks(), [
        {"related_posts strips content/payload columns (case 9)", fun() ->
            drain_msgs(),
            %% mock 忠实模拟 DS 列白名单的真实输出（SELECT 只含元数据列；
            %% 真库证据见 project_channel_agg_integration_tests
            %% related_posts_bounded_test_）——本用例验证 logic 透传
            %% 不引入正文字段（ZC-09R：原 mock 返回含 content 的行并断言
            %% 输出干净，剥离逻辑在被 mock 的 DS 层，断言对象错位）
            Row = #{
                <<"id">> => 1,
                <<"channel_id">> => 2,
                <<"author_id">> => 3,
                <<"msg_type">> => <<"text">>,
                <<"created_at">> => <<"2026-08-29T00:00:00Z">>
            },
            meck:expect(project_channel_ds, related_posts, fun(_Pid) -> {ok, [Row]} end),
            {ok, [Item]} = project_channel_logic:related_posts(?OWNER, ?PROJECT_ID),
            ?assertEqual(false, maps:is_key(<<"content">>, Item)),
            ?assertEqual(false, maps:is_key(<<"payload">>, Item)),
            ?assertEqual(1, maps:get(<<"id">>, Item))
        end},
        {"activity payload sanitized of content-like keys (case 8)", fun() ->
            drain_msgs(),
            Payload = jsone:encode(
                #{<<"channel_id">> => 1, <<"content">> => <<"body">>, <<"message">> => <<"m">>},
                [native_utf8]
            ),
            Row = #{
                <<"id">> => 1,
                <<"event_type">> => <<"channel_linked">>,
                <<"actor_id">> => ?OWNER,
                <<"target_id">> => 2,
                <<"payload">> => Payload,
                <<"created_at">> => <<"2026-08-29T00:00:00Z">>
            },
            meck:expect(project_channel_ds, activity, fun(_Pid, _P, _S) ->
                {ok, #{list => [Row], page => 1, size => 20, total => 1, total_page => 1}}
            end),
            {ok, #{list := [Item]}} = project_channel_logic:activity(?OWNER, ?PROJECT_ID, 1, 20),
            P2 = maps:get(<<"payload">>, Item),
            ?assertEqual(false, maps:is_key(<<"content">>, P2)),
            ?assertEqual(false, maps:is_key(<<"message">>, P2)),
            ?assertEqual(1, maps:get(<<"channel_id">>, P2))
        end}
    ]).

%%% ===================================================================
%%% SQL 查询数上限（case 10：固定条数 SQL，无 N+1）
%%% ===================================================================

sql_bound_mocks() ->
    [
        {project_repo, [
            {'find_by_id', 2, fun(?PROJECT_ID, _) -> project_row() end}
        ]},
        {project_channel_rel_repo, [
            {'find_project_member', 2, fun(_Pid, _Uid) -> pm_row(<<"active">>) end}
        ]},
        {workspace_member_repo, [
            {'find', 3, fun(_Ws, _Uid, _Col) ->
                #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, []} end},
            {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"count">> => 0}} end},
            {'with_tx', 1, fun(_Fun) -> ok end}
        ]}
    ].

sql_bound_test_() ->
    ?WITH_MECK_TESTS(sql_bound_mocks(), [
        {"pinned: fixed 2 SQL per request regardless of page (case 10)", fun() ->
            drain_msgs(),
            meck:reset(elib_pg),
            {ok, _} = project_channel_logic:pinned(?OWNER, ?PROJECT_ID, 1, 20),
            ?assertEqual(1, meck:num_calls(elib_pg, one, 2), "pinned count SQL"),
            ?assertEqual(1, meck:num_calls(elib_pg, query, 2), "pinned data SQL"),
            {ok, _} = project_channel_logic:pinned(?OWNER, ?PROJECT_ID, 2, 20),
            ?assertEqual(2, meck:num_calls(elib_pg, one, 2)),
            ?assertEqual(2, meck:num_calls(elib_pg, query, 2))
        end},
        {"related_posts: exactly 1 SQL (single window query)", fun() ->
            drain_msgs(),
            meck:reset(elib_pg),
            {ok, _} = project_channel_logic:related_posts(?OWNER, ?PROJECT_ID),
            ?assertEqual(1, meck:num_calls(elib_pg, query, 2)),
            ?assertEqual(0, meck:num_calls(elib_pg, one, 2))
        end},
        {"activity: fixed 2 SQL per request", fun() ->
            drain_msgs(),
            meck:reset(elib_pg),
            {ok, _} = project_channel_logic:activity(?OWNER, ?PROJECT_ID, 1, 20),
            ?assertEqual(1, meck:num_calls(elib_pg, one, 2)),
            ?assertEqual(1, meck:num_calls(elib_pg, query, 2))
        end},
        {"list_channels: fixed 2 SQL per request", fun() ->
            drain_msgs(),
            meck:reset(elib_pg),
            {ok, _} = project_channel_logic:list_channels(?OWNER, ?PROJECT_ID, 1, 20),
            ?assertEqual(1, meck:num_calls(elib_pg, one, 2)),
            ?assertEqual(1, meck:num_calls(elib_pg, query, 2))
        end}
    ]).
