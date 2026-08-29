-module(project_channel_rel_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% ZC-04 — Channel 关联（link/unlink/update-links）真库集成测试（PG 不可用自动 skip）
%%%
%%% 覆盖（W2 计划 TDD 用例 1/2/3/4/5/7）：
%%%   1. 同 Workspace 关联成功：rel 行 + channel_linked 事件同事务
%%%   2. 跨 Workspace 关联 400（应用层前置 + DB 23503 复合 FK 双兜底）
%%%   3. 重复 link 幂等：不重复写行、不重复写事件
%%%   4. 并发唯一：两连接同时 link 同一对 → 一方 created 一方 existing，单行单事件
%%%   5. unlink：解除 + channel_unlinked 事件同事务；解除不存在的关联 → 404（决策）
%%%   7. update-links：全量替换 + 应用层形状校验（DB trg_project_links_shape 之外防线）
%%%
%%% 造数 autocommit；try/after 级联清理（channel_message 无 FK 需手动清，
%%% workspace 删除级联带走 workspace_member/project/project_member/rel/event/channel）。

-define(SETUP_TAG, <<"ZC04-rel">>).

%%% ===================================================================
%%% 用例 1/3：同 Workspace link 成功 + 事件同事务；重复 link 幂等
%%% ===================================================================

link_same_workspace_success_and_idempotent_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(?SETUP_TAG),
        #{conn := Conn, project_id := ProjId, owner := Owner, ch1 := Ch1} = Ctx,
        try
            %% 用例 1：link 成功
            ?assertMatch(
                {ok, created},
                project_channel_logic:link(Owner, ProjId, Ch1)
            ),
            {ok, _, [{RelCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_channel_rel",
                    " WHERE project_id = $1 AND channel_id = $2">>,
                [ProjId, Ch1]
            ),
            ?assertEqual(1, RelCnt, "rel 行应存在"),
            {ok, _, [{EvCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'channel_linked'",
                    "   AND target_id = $2 AND actor_id = $3">>,
                [ProjId, Ch1, Owner]
            ),
            ?assertEqual(1, EvCnt, "channel_linked 事件应与 rel 同事务落库"),
            %% 事件 payload 只含元数据（channel_id/channel_name）
            {ok, _, [{Pl}]} = epgsql:equery(
                Conn,
                <<"SELECT payload FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'channel_linked' LIMIT 1">>,
                [ProjId]
            ),
            Payload = jsone:decode(Pl, [{object_format, map}]),
            ?assertEqual(Ch1, maps:get(<<"channel_id">>, Payload)),
            ?assertEqual(false, maps:is_key(<<"content">>, Payload)),

            %% 用例 3：重复 link 幂等
            ?assertMatch(
                {ok, existing},
                project_channel_logic:link(Owner, ProjId, Ch1)
            ),
            {ok, _, [{RelCnt2}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_channel_rel",
                    " WHERE project_id = $1 AND channel_id = $2">>,
                [ProjId, Ch1]
            ),
            ?assertEqual(1, RelCnt2, "重复 link 不重复写行"),
            {ok, _, [{EvCnt2}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'channel_linked'">>,
                [ProjId]
            ),
            ?assertEqual(1, EvCnt2, "重复 link 不重复写事件")
        after
            cleanup_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 用例 2：跨 Workspace / personal 频道 → 400（应用层前置；
%%% DB 23503 复合 FK 由 w2_schema_contract_tests 承接）
%%% ===================================================================

link_cross_workspace_and_personal_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(<<"ZC04-xws">>),
        #{
            conn := Conn,
            project_id := ProjId,
            owner := Owner,
            ch_personal := ChPersonal,
            ch_ws2 := ChWs2
        } = Ctx,
        try
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:link(Owner, ProjId, ChWs2)
            ),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:link(Owner, ProjId, ChPersonal)
            ),
            {ok, _, [{RelCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_channel_rel WHERE project_id = $1">>,
                [ProjId]
            ),
            ?assertEqual(0, RelCnt, "非法关联不得写行"),
            {ok, _, [{EvCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'channel_linked'">>,
                [ProjId]
            ),
            ?assertEqual(0, EvCnt, "非法关联不得写事件")
        after
            cleanup_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 用例 2 补充：已删除频道（status=-1）不可关联
%%% ===================================================================

link_deleted_channel_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(<<"ZC04-dead">>),
        #{conn := Conn, project_id := ProjId, owner := Owner, ch2 := Ch2} = Ctx,
        try
            {ok, 1} = epgsql:equery(
                Conn, <<"UPDATE channel SET status = -1 WHERE id = $1">>, [Ch2]
            ),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:link(Owner, ProjId, Ch2)
            ),
            {ok, _, [{RelCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_channel_rel WHERE project_id = $1">>,
                [ProjId]
            ),
            ?assertEqual(0, RelCnt)
        after
            cleanup_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 用例 4：两连接并发 link 同一对 → 一方 created 一方 existing
%%% ===================================================================

concurrent_link_unique_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(<<"ZC04-conc">>),
        #{conn := Conn, project_id := ProjId, owner := Owner, ch2 := Ch2} = Ctx,
        try
            Self = self(),
            Pids = [
                spawn(fun() ->
                    Self ! {link_done, project_channel_ds:link(Owner, ProjId, Ch2)}
                end)
             || _ <- lists:seq(1, 2)
            ],
            Results = [
                receive
                    {link_done, R} -> R
                after 20000 ->
                    ?assert(false, "concurrent link timed out")
                end
             || _ <- Pids
            ],
            ?assertEqual(
                [{ok, created}, {ok, existing}],
                lists:sort(Results),
                io_lib:format("并发 link 结果应恰为 created+existing: ~p", [Results])
            ),
            {ok, _, [{RelCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_channel_rel",
                    " WHERE project_id = $1 AND channel_id = $2">>,
                [ProjId, Ch2]
            ),
            ?assertEqual(1, RelCnt, "并发 link 只落一行"),
            {ok, _, [{EvCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'channel_linked'">>,
                [ProjId]
            ),
            ?assertEqual(1, EvCnt, "并发 link 只写一个事件")
        after
            cleanup_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 用例 5：unlink 解除 + 事件同事务；缺失关联 404（决策）
%%% ===================================================================

unlink_lifecycle_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(<<"ZC04-unlink">>),
        #{conn := Conn, project_id := ProjId, owner := Owner, ch1 := Ch1} = Ctx,
        try
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch1),
            ?assertMatch(
                {ok, unlinked},
                project_channel_logic:unlink(Owner, ProjId, Ch1)
            ),
            {ok, _, [{RelCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_channel_rel",
                    " WHERE project_id = $1 AND channel_id = $2">>,
                [ProjId, Ch1]
            ),
            ?assertEqual(0, RelCnt, "unlink 后 rel 行应删除"),
            {ok, _, [{UnlinkEvCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'channel_unlinked'",
                    "   AND target_id = $2">>,
                [ProjId, Ch1]
            ),
            ?assertEqual(1, UnlinkEvCnt, "channel_unlinked 事件应同事务落库"),
            %% 决策：解除不存在的关联 → 404（unlink 定向删除语义；
            %% link 才是幂等吸收方向）
            ?assertMatch(
                {error, {404, _}},
                project_channel_logic:unlink(Owner, ProjId, Ch1)
            ),
            {ok, _, [{UnlinkEvCnt2}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'channel_unlinked'">>,
                [ProjId]
            ),
            ?assertEqual(1, UnlinkEvCnt2, "404 分支不得再写事件")
        after
            cleanup_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 用例 7：update-links 全量替换 + 应用层校验 + 原样返回
%%% ===================================================================

update_links_roundtrip_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(<<"ZC04-links">>),
        #{conn := Conn, project_id := ProjId, owner := Owner, member2 := Member2} = Ctx,
        try
            Links1 = [
                #{<<"name">> => <<"Design Doc">>, <<"url">> => <<"https://docs.example.com/a">>},
                #{<<"name">> => <<"Repo">>, <<"url">> => <<"https://git.example.com/b">>}
            ],
            ?assertMatch(
                {ok, _}, project_channel_logic:update_links(Owner, ProjId, Links1)
            ),
            {ok, Stored} = project_channel_logic:resources(Owner, ProjId),
            ?assertEqual(Links1, Stored, "links 应全量替换并原样返回（用户配置 url 不改写）"),
            {ok, _, [{EvCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'links_updated'">>,
                [ProjId]
            ),
            ?assertEqual(1, EvCnt, "links_updated 事件应同事务落库"),

            %% active project member 也可写 links
            ?assertMatch(
                {ok, _}, project_channel_logic:update_links(Member2, ProjId, [])
            ),
            {ok, []} = project_channel_logic:resources(Owner, ProjId),

            %% 应用层校验：非法形状 400，且不改动既有 links（先恢复 Links1）
            ?assertMatch(
                {ok, _}, project_channel_logic:update_links(Owner, ProjId, Links1)
            ),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(Owner, ProjId, [
                    #{<<"name">> => <<"Bad">>}
                ])
            ),
            ?assertMatch(
                {error, {400, _}},
                project_channel_logic:update_links(Owner, ProjId, [
                    #{<<"name">> => <<>>, <<"url">> => <<"https://e.com">>}
                ])
            ),
            {ok, Stored2} = project_channel_logic:resources(Owner, ProjId),
            ?assertEqual(Links1, Stored2, "校验失败不得改动既有 links"),
            {ok, _, [{EvCnt2}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'links_updated'">>,
                [ProjId]
            ),
            %% 3 次：Links1 全量替换 + Member2 清空 + 恢复 Links1（两次非法 400 不写）
            ?assertEqual(3, EvCnt2, "非法 update 不得写事件")
        after
            cleanup_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 关联列表（list 端点语义）：JOIN channel 元数据
%%% ===================================================================

list_channels_returns_metadata_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(<<"ZC04-list">>),
        #{project_id := ProjId, owner := Owner, ch1 := Ch1, ch2 := Ch2} = Ctx,
        try
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch1),
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch2),
            {ok, #{list := Rows} = Page} = project_channel_logic:list_channels(
                Owner, ProjId, 1, 20
            ),
            ?assertEqual(2, length(Rows)),
            ?assertEqual(2, maps:get(total, Page)),
            ChanIds = [maps:get(<<"channel_id">>, R) || R <- Rows],
            ?assertEqual(lists:sort([Ch1, Ch2]), lists:sort(ChanIds)),
            %% 行内只有频道元数据，无消息正文字段
            lists:foreach(
                fun(R) ->
                    ?assert(maps:is_key(<<"name">>, R)),
                    ?assertEqual(false, maps:is_key(<<"content">>, R))
                end,
                Rows
            )
        after
            cleanup_world(conn(Ctx), Ctx)
        end
    end).

%%% ===================================================================
%%% 权限集成：guest 只读（写 403）、非成员 403
%%% ===================================================================

permission_integration_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(<<"ZC04-perm">>),
        #{
            project_id := ProjId,
            owner := Owner,
            guest := Guest,
            outsider := Outsider,
            ch1 := Ch1
        } = Ctx,
        try
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch1),
            %% guest 是 active project_member 但 ws role=guest：可读不可写
            ?assertMatch({ok, _}, project_channel_logic:resources(Guest, ProjId)),
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:link(Guest, ProjId, Ch1)
            ),
            %% 非项目成员：读/写均 403
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:resources(Outsider, ProjId)
            ),
            ?assertMatch(
                {error, {403, _}},
                project_channel_logic:link(Outsider, ProjId, Ch1)
            )
        after
            cleanup_world(conn(Ctx), Ctx)
        end
    end).

%%% ===================================================================
%%% Internal：造数与清理
%%% ===================================================================

conn(Ctx) ->
    maps:get(conn, Ctx).

%% @doc 造一个完整世界：
%%   ws1(owner+member2+guest 的 active workspace_member)
%%   + project(owner) + 三人 active project_member
%%   + ch1/ch2（ws1 workspace 频道）+ ch_personal（personal）+ ws2/chws2（跨工作区）
setup_world(Tag) ->
    {ok, Conn} = take_conn(),
    {ok, _, [{Owner}, {Member2}, {Guest}, {Outsider}]} = epgsql:equery(
        Conn, <<"SELECT id FROM \"user\" ORDER BY id ASC LIMIT 4">>, []
    ),
    WsId = elib_tsid:generate(workspace),
    Ws2Id = elib_tsid:generate(workspace),
    ProjId = elib_tsid:generate(project),
    Ch1 = elib_tsid:generate(channel),
    Ch2 = elib_tsid:generate(channel),
    ChPersonal = elib_tsid:generate(channel),
    ChWs2 = elib_tsid:generate(channel),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, status)", " VALUES ($1, $2, $3, 'active')">>,
        [WsId, Tag, Owner]
    ),
    {ok, 3} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, joined_at, status)",
            " VALUES ($1, $2, 'owner', NULL, CURRENT_TIMESTAMP, 'active'),",
            "        ($1, $3, 'member', $2, CURRENT_TIMESTAMP, 'active'),",
            "        ($1, $4, 'guest', $2, CURRENT_TIMESTAMP, 'active')">>,
        [WsId, Owner, Member2, Guest]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, status)", " VALUES ($1, $2, $3, 'active')">>,
        [Ws2Id, <<Tag/binary, "-ws2">>, Owner]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, joined_at, status)",
            " VALUES ($1, $2, 'owner', NULL, CURRENT_TIMESTAMP, 'active')">>,
        [Ws2Id, Owner]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project (id, workspace_id, name, owner_id, status)",
            " VALUES ($1, $2, $3, $4, 'active')">>,
        [ProjId, WsId, Tag, Owner]
    ),
    %% Owner/Member2/Guest 均 active project_member（权限矩阵的被测关系）
    {ok, 3} = epgsql:equery(
        Conn,
        <<"INSERT INTO project_member (workspace_id, project_id, user_id, joined_at, status)",
            " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active'),",
            "        ($1, $2, $4, CURRENT_TIMESTAMP, 'active'),",
            "        ($1, $2, $5, CURRENT_TIMESTAMP, 'active')">>,
        [WsId, ProjId, Owner, Member2, Guest]
    ),
    {ok, 2} = epgsql:equery(
        Conn,
        <<"INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id)",
            " VALUES ($1, $2, $3, 1, 'workspace', $4),",
            "        ($5, $6, $3, 1, 'workspace', $4)">>,
        [Ch1, <<Tag/binary, "-ch1">>, Owner, WsId, Ch2, <<Tag/binary, "-ch2">>]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO channel (id, name, creator_uid, status, scope)",
            " VALUES ($1, $2, $3, 1, 'personal')">>,
        [ChPersonal, <<Tag/binary, "-personal">>, Owner]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id)",
            " VALUES ($1, $2, $3, 1, 'workspace', $4)">>,
        [ChWs2, <<Tag/binary, "-ws2ch">>, Owner, Ws2Id]
    ),
    #{
        conn => Conn,
        ws_id => WsId,
        ws2_id => Ws2Id,
        project_id => ProjId,
        owner => Owner,
        member2 => Member2,
        guest => Guest,
        outsider => Outsider,
        ch1 => Ch1,
        ch2 => Ch2,
        ch_personal => ChPersonal,
        ch_ws2 => ChWs2
    }.

cleanup_world(Conn, Ctx) ->
    ChIds = [
        maps:get(K, Ctx)
     || K <- [ch1, ch2, ch_personal, ch_ws2], maps:is_key(K, Ctx)
    ],
    %% channel_message 无 FK，先手动清（防跨用例残留）
    catch epgsql:equery(
        Conn,
        <<"DELETE FROM channel_message WHERE channel_id = ANY($1::bigint[])">>,
        [ChIds]
    ),
    catch epgsql:equery(
        Conn, <<"DELETE FROM channel WHERE id = ANY($1::bigint[])">>, [ChIds]
    ),
    catch epgsql:equery(
        Conn, <<"DELETE FROM workspace WHERE id = $1">>, [maps:get(ws2_id, Ctx)]
    ),
    %% workspace 级联带走 wm/project/project_member/rel/event/其余 channel
    catch epgsql:equery(
        Conn, <<"DELETE FROM workspace WHERE id = $1">>, [maps:get(ws_id, Ctx)]
    ),
    ok = pooler:return_member(pgsql, Conn, ok),
    ok.

-spec take_conn() -> {ok, pid()} | {error, term()}.
take_conn() ->
    case pooler:take_member(pgsql) of
        error_no_members ->
            timer:sleep(200),
            case pooler:take_member(pgsql) of
                error_no_members -> {error, no_connection};
                Conn -> {ok, Conn}
            end;
        Conn ->
            {ok, Conn}
    end.
