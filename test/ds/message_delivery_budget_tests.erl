-module(message_delivery_budget_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% BE-14（LOCAL-CARD-BE-14）：慢端预算与热点公平调度
%%%
%%% 判据映射（gate assertions）：
%%% - LOCAL:01 slow_vs_normal_same_group / budget_recover_after_drain /
%%%   catch_up_pull_after_backlog：同群接收慢端受限（预算内停推）、正常端
%%%   全额直达（符合预算）；慢端排空后同连接恢复直达、经补拉（pull）
%%%   补齐滞留事实，已删消息不复活。
%%% - LOCAL:02 hot_group_vs_direct_fairness / notice_merge_*：热点群
%%%   （bulk 通道）让出 direct 预留（普通会话不饿死）；超预算降级为
%%%   **合并轻通知**（单连接至多一条在途），不写库（elib_pg 硬禁）、
%%%   不写 ack_received（不虚增已读/送达）。
%%% - LOCAL:03 control_frames_bypass / e2ee_frame_passthrough /
%%%   deferral_leaves_no_zombie / terminate_drops_budget_rows：控制帧
%%%   （撤权 close/kick、ack_cancel）不受预算影响；E2EE 帧字节级透传、
%%%   轻通知不携带密文；defer 不留 timer/缓存残尸，删除（ack_received）
%%%   后同 MsgId 不复活；连接终止清理 pid 键预算行。
%%% - PLANNED gateway_hop_and_call_count_oracle：重试跳数有界 +
%%%   counters 恒等式（delivered+deferred==尝试数，notice 合并计数）。
%%%
%%% 轨道：纯行为用例跑 peer VM（无 PG/无 syn 节点；imboy_syn:list_by_uid
%%% 与 syn:members 定向 meck，imboy_syn 其余函数 passthrough 走真实现，
%%% 预算门/timer/轻通知/计数全部真实代码路径）。PG 用例 TEST_WITH_DB
%%% 走标准 eunit-local 库：验证降级期间事实在库滞留可回读、删除被读
%%% 路径尊重。
%%%===================================================================

-define(UID_SLOW, 93001401).
-define(UID_NORMAL, 93001402).
-define(UID_SENDER, 93001403).
-define(GID, 93001501).

-define(BE14_PG_GID, 93001601).
-define(BE14_PG_UID, 93001602).
-define(BE14_PG_SENDER, 93001603).
-define(BE14_PG_AT, <<"2026-11-21T03:00:00Z">>).

%% ===================================================================
%% 纯行为轨道（peer VM）
%% ===================================================================

%% LOCAL:01：同群慢端受限、正常端符合预算（不越限）。
%% bulk 有效上限 = Max-Reserve = 5：慢端预置 4 帧积压后仅 1 帧入内；
%% 正常端从 0 起收满 5 帧预算后停推（第 6 帧降级），不发生无界积压。
%% 两端排空后预算恢复（正常端补收演示）。
slow_vs_normal_same_group_test_() ->
    {"BE-14:LOCAL:01 slow restricted, normal within budget", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            Normal = member_pid(?UID_NORMAL, Members),
            stuff(Slow, 4),
            Msgs = [group_msg(I) || I <- lists:seq(1, 6)],
            [
                ok = message_ds:send_next(U, Id, Json, [0])
             || {Id, Json} <- Msgs, U <- [?UID_SLOW, ?UID_NORMAL]
            ],
            settle(),

            %% 正常端：收满 bulk 预算 5 帧（符合预算，不越限）。
            ?assertEqual(5, length(data_frames(Normal))),
            %% 慢端：仅预算内 1 帧（先占 4 帧积压），其后受限。
            ?assertEqual(1, length(data_frames(Slow))),
            %% 轻通知合并：各端恰好 1 条（非 6 条）。
            ?assertEqual(1, length(notice_frames(Slow))),
            ?assertEqual(1, length(notice_frames(Normal))),

            C0 = imboy_syn:delivery_budget_counters(),
            ?assertEqual(6, maps:get(bulk_delivered, C0, 0)),
            ?assertEqual(6, maps:get(bulk_deferred, C0, 0)),
            %% 可解释性恒等式：delivered + deferred == 尝试数（6 消息 x 2 端）。
            ?assertEqual(
                12,
                maps:get(bulk_delivered, C0, 0) + maps:get(bulk_deferred, C0, 0)
            ),

            %% 正常端排空后预算恢复，补收后续同群消息。
            _ = drain(Normal),
            {MoreId, MoreJson} = group_msg(<<"m7">>),
            ok = message_ds:send_next(?UID_NORMAL, MoreId, MoreJson, [0]),
            settle(),
            ?assertEqual(1, length(data_frames(Normal)))
        end)
    end}.

%% LOCAL:01：慢端排空后预算恢复（同连接新帧恢复直达）。
budget_recover_after_drain_test_() ->
    {"BE-14:LOCAL:01 budget recovers after slow device drains", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            stuff(Slow, 6),
            {Id1, Json1} = group_msg(<<"m1">>),
            ok = message_ds:send_next(?UID_SLOW, Id1, Json1, [0]),
            ?assertEqual(0, length(data_frames(Slow))),
            %% 慢端消费排空（模拟从慢恢复）：6 junk + 1 条合并轻通知。
            Frames = drain(Slow),
            ?assertEqual(7, length(Frames)),
            DrainedNotices =
                [B || {timeout, _, B} <- Frames, is_binary(B), is_notice(B)],
            ?assertEqual(1, length(DrainedNotices)),
            {Id2, Json2} = group_msg(<<"m2">>),
            ok = message_ds:send_next(?UID_SLOW, Id2, Json2, [0]),
            settle(),
            ?assertEqual(1, length(data_frames(Slow))),
            C0 = imboy_syn:delivery_budget_counters(),
            ?assertEqual(1, maps:get(bulk_delivered, C0, 0)),
            ?assertEqual(1, maps:get(bulk_deferred, C0, 0))
        end)
    end}.

%% LOCAL:01/03：积压补齐——轻通知引导 pull，补拉推送恢复滞留事实，
%% 已删消息不复活，且不写 ack_received（不虚增已读）。
catch_up_pull_after_backlog_test_() ->
    {"BE-14:LOCAL:01 catch-up pull restores facts, deleted stays deleted", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            stuff(Slow, 6),
            %% 3 条消息因预算被降级（事实仍在“库”——以读路径回读模拟），
            %% m3 已删除：读路径不再返回。
            [
                ok = message_ds:send_next(?UID_SLOW, Id, Json, [0])
             || {Id, Json} <- [group_msg(<<"m1">>), group_msg(<<"m2">>), group_msg(<<"m3">>)]
            ],
            ?assertEqual(3, maps:get(bulk_deferred, imboy_syn:delivery_budget_counters(), 0)),

            %% 慢端恢复并消费掉积压与轻通知后触发补拉（登录/上线同入口）。
            _ = drain(Slow),
            ok = message_ds:check_and_notify_offline_msgs(?UID_SLOW, did_slow()),
            settle(),
            PulledIds = [envelope_id(F) || F <- data_frames(Slow)],
            ?assert(lists:member(<<"m1">>, PulledIds)),
            ?assert(lists:member(<<"m2">>, PulledIds)),
            %% 删除不复活：m3 不在补齐结果里。
            ?assertNot(lists:member(<<"m3">>, PulledIds)),
            %% 不虚增已读：补齐推送未写任何 ack_received。
            [
                ?assertEqual(
                    undefined,
                    ack_retry_cache:get({ack_received, ?UID_SLOW, did_slow(), Id})
                )
             || Id <- [<<"m1">>, <<"m2">>, <<"m3">>]
            ]
        end)
    end}.

%% LOCAL:02：热点群（bulk）饱和不饿死普通会话（direct 预留生效）。
hot_group_vs_direct_fairness_test_() ->
    {"BE-14:LOCAL:02 bulk saturates while direct keeps reserved share", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            %% QLen=6：bulk 需 <5 → 拒；direct 需 <8 → 通。
            stuff(Slow, 6),
            {G1Id, G1Json} = group_msg(<<"g1">>),
            {D1Id, D1Json} = direct_msg(<<"d1">>),
            ok = message_ds:send_next(?UID_SLOW, G1Id, G1Json, [0]),
            ok = message_ds:send_next(?UID_SLOW, D1Id, D1Json, [0]),
            ?assertEqual(1, length(data_frames(Slow))),
            C0 = imboy_syn:delivery_budget_counters(),
            ?assertEqual(1, maps:get(bulk_deferred, C0, 0)),
            ?assertEqual(1, maps:get(direct_delivered, C0, 0)),

            %% 总上限仍兜底：QLen 到 8 后 direct 亦受限（预算硬界不失效）。
            stuff(Slow, 1),
            {D2Id, D2Json} = direct_msg(<<"d2">>),
            ok = message_ds:send_next(?UID_SLOW, D2Id, D2Json, [0]),
            ?assertEqual(1, length(data_frames(Slow))),
            ?assertEqual(1, maps:get(direct_deferred, imboy_syn:delivery_budget_counters(), 0))
        end)
    end}.

%% LOCAL:02：轻通知合并——多条降级合并为单条；无库写、无已读/送达虚增。
%% ePG 全程 meck raise 硬禁（run_budget_case）：用例通过即证明零库写。
notice_merge_single_frame_no_side_effects_test_() ->
    {"BE-14:LOCAL:02 merged light notice, no fact/read inflation", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            stuff(Slow, 6),
            [
                ok = message_ds:send_next(?UID_SLOW, Id, Json, [0])
             || {Id, Json} <- [group_msg(I) || I <- lists:seq(1, 5)]
            ],
            settle(),

            Notices = notice_frames(Slow),
            ?assertEqual(1, length(Notices)),
            [Notice] = Notices,
            ?assertEqual(<<"pull_offline_msg">>, map_field(Notice, <<"action">>)),
            ?assertEqual(<<"S2C">>, map_field(Notice, <<"type">>)),
            ?assertEqual(
                <<"delivery_budget">>,
                nested_field(Notice, [<<"payload">>, <<"reason">>])
            ),
            %% 合并计数：首个降级携带当时滞留计数 1，其余 4 次合并。
            ?assertEqual(1, nested_field(Notice, [<<"payload">>, <<"deferred">>])),
            C0 = imboy_syn:delivery_budget_counters(),
            ?assertEqual(5, maps:get(bulk_deferred, C0, 0)),
            ?assertEqual(1, maps:get(notice_sent, C0, 0)),
            ?assertEqual(4, maps:get(notice_coalesced, C0, 0)),
            %% 轻通知不携带任何业务载荷字段（无密文/无正文泄露面）。
            ?assertEqual(nomatch, binary:match(Notice, [<<"content">>, <<"ciphertext">>])),
            %% 不虚增已读/送达：无 ack_received 残留。
            [
                ?assertEqual(
                    undefined,
                    ack_retry_cache:get({ack_received, ?UID_SLOW, did_slow(), integer_to_binary(I)})
                )
             || I <- lists:seq(1, 5)
            ]
        end)
    end}.

%% LOCAL:03：控制帧（撤权 close / kick_device）不受预算影响。
control_frames_bypass_budget_test_() ->
    {"BE-14:LOCAL:03 revocation control frames bypass budget", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            stuff(Slow, 6),
            {G1Id, G1Json} = group_msg(<<"g1">>),
            ok = message_ds:send_next(?UID_SLOW, G1Id, G1Json, [0]),
            ?assertEqual(1, maps:get(bulk_deferred, imboy_syn:delivery_budget_counters(), 0)),

            State = #{current_uid => ?UID_SLOW, did => did_slow(), dtype => <<"ios">>},
            %% 撤权/会话失效：close 控制帧必须照常关连接（不被预算吞掉）。
            ?assertMatch(
                {reply, {close, 4001, <<"session_revoked">>}, _},
                websocket_handler:websocket_info({close, 4001, <<"session_revoked">>}, State)
            ),
            %% 设备踢出（撤权）：仍是 reply+close，且不因慢端积压被降级。
            ?assertMatch(
                {reply, _, _, hibernate},
                websocket_handler:websocket_info(
                    {kick_device, #{<<"reason">> => <<"device_revoked">>}}, State
                )
            ),
            %% ack_cancel（删除/撤回联动）：控制类投递绕过预算直达邮箱。
            ok = imboy_syn:broadcast_ack_cancel(?UID_SLOW, did_slow(), G1Id),
            CancelFrames =
                [M || {ack_cancel, _, _, <<"g1">>, _} = M <- raw_messages(Slow)],
            ?assertEqual(1, length(CancelFrames)),
            %% 控制路径不产生任何预算判定（计数保持 1 deferred 不变）。
            ?assertEqual(1, maps:get(bulk_deferred, imboy_syn:delivery_budget_counters(), 0))
        end)
    end}.

%% LOCAL:03：E2EE 帧字节级透传；超预算降级的轻通知不携带密文。
e2ee_frame_passthrough_byte_exact_test_() ->
    {"BE-14:LOCAL:03 e2ee frames pass through byte-exact", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            Cipher = <<"BE14CIPHER", (binary:copy(<<"z">>, 180))/binary>>,
            E2eeJson = e2ee_msg(<<"e1">>, Cipher),
            %% 预算内：投递帧与原 JSON 字节一致（无重建/裁剪）。
            stuff(Slow, 2),
            ok = message_ds:send_next(?UID_SLOW, <<"e1">>, E2eeJson, [0]),
            settle(),
            [Delivered] = data_frames(Slow),
            ?assertEqual(E2eeJson, Delivered),
            %% 超预算降级：轻通知不得携带任何密文片段。
            stuff(Slow, 5),
            E2ee2 = e2ee_msg(<<"e2">>, Cipher),
            ok = message_ds:send_next(?UID_SLOW, <<"e2">>, E2ee2, [0]),
            settle(),
            ?assertEqual(1, length(data_frames(Slow))),
            [Notice] = notice_frames(Slow),
            ?assertEqual(nomatch, binary:match(Notice, Cipher))
        end)
    end}.

%% LOCAL:03：defer 不留僵尸 timer；删除（ack_received）后同 MsgId 不复活。
deferral_leaves_no_zombie_and_no_revival_test_() ->
    {"BE-14:LOCAL:03 no zombie timers; deleted msgid stays dead", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            stuff(Slow, 6),
            %% MsLi=[0,40]：hop0 与 hop1 续排都应被预算拦下。
            {Z1Id, Z1Json} = group_msg(<<"z1">>),
            ok = message_ds:send_next(?UID_SLOW, Z1Id, Z1Json, [0, 40]),
            ?assertEqual(0, length(data_frames(Slow))),
            ?assertEqual(undefined, ack_retry_cache:get({?UID_SLOW, did_slow(), <<"z1">>})),
            timer:sleep(90),
            ?assertEqual(0, length(data_frames(Slow))),
            ?assertEqual(0, length(retry_frames(Slow))),

            %% 删除/撤回路径：ack_cancel 置 ack_received 后，同 MsgId 永不复活。
            State = #{current_uid => ?UID_SLOW, did => did_slow(), dtype => <<"ios">>},
            ?assertMatch(
                {ok, _State, hibernate},
                websocket_handler:websocket_info(
                    {ack_cancel, ?UID_SLOW, did_slow(), <<"z1">>,
                        erlang:monotonic_time(millisecond)},
                    State
                )
            ),
            {ok, true} =
                ack_retry_cache:get({ack_received, ?UID_SLOW, did_slow(), <<"z1">>}),
            %% 慢端已排空（预算全通），但已删 MsgId 仍不得投递。
            _ = drain(Slow),
            ok = message_ds:send_next(?UID_SLOW, Z1Id, Z1Json, [0]),
            settle(),
            ?assertEqual(0, length(data_frames(Slow)))
        end)
    end}.

%% PLANNED：网关批次数与调用数 oracle——重试跳数有界 + counters 恒等。
gateway_hop_and_call_count_oracle_test_() ->
    {"BE-14:PLANNED hop count and call count oracle", fun() ->
        run_budget_case(#{max_count => 64, reserve_count => 8}, fun(Members) ->
            Normal = member_pid(?UID_NORMAL, Members),
            %% 未 ACK 未消费时：每条消息 hop0 直投帧 + hop1 续排的 timer 帧
            %% （hop2 由消费侧续排，假设备不消费 → 有界 2 跳）。
            [
                ok = message_ds:send_next(?UID_NORMAL, Id, Json, [0, 30])
             || {Id, Json} <- [direct_msg(I) || I <- lists:seq(1, 2)]
            ],
            timer:sleep(100),
            ?assertEqual(2, length(data_frames(Normal))),
            ?assertEqual(2, length(retry_frames(Normal))),

            %% 慢端超预算：两跳全拦，最终 0 帧（hop 有界且不迟到）。
            Slow = member_pid(?UID_SLOW, Members),
            stuff(Slow, 64),
            {H1Id, H1Json} = direct_msg(<<"h1">>),
            ok = message_ds:send_next(?UID_SLOW, H1Id, H1Json, [0, 30]),
            timer:sleep(100),
            ?assertEqual(0, length(data_frames(Slow))),
            ?assertEqual(0, length(retry_frames(Slow))),

            %% 调用数恒等式：每通道 delivered+deferred == 尝试数
            %% （normal 2 消息 x 2 跳 = 4 allow；slow 1 消息 x 2 跳 = 2 defer）。
            C0 = imboy_syn:delivery_budget_counters(),
            ?assertEqual(
                6,
                maps:get(direct_delivered, C0, 0) + maps:get(direct_deferred, C0, 0)
            ),
            ?assertEqual(4, maps:get(direct_delivered, C0, 0))
        end)
    end}.

%% LOCAL:03：连接终止清理预算行（pid 键不残留，聚合计数保留可解释）。
terminate_drops_budget_rows_test_() ->
    {"BE-14:LOCAL:03 terminate cleans budget rows", fun() ->
        run_budget_case(#{max_count => 8, reserve_count => 3}, fun(Members) ->
            Slow = member_pid(?UID_SLOW, Members),
            stuff(Slow, 6),
            {T1Id, T1Json} = group_msg(<<"t1">>),
            ok = message_ds:send_next(?UID_SLOW, T1Id, T1Json, [0]),
            %% 触发过预算门：聚合计数非空。
            ?assert(maps:size(imboy_syn:delivery_budget_counters()) > 0),
            %% 无 current_uid 的 State：terminate 仍清理预算行。
            ok = websocket_handler:terminate(normal, req_stub, #{}),
            %% 聚合计数保留（不随连接消失——接受事实仍可解释）。
            ?assertEqual(1, maps:get(bulk_deferred, imboy_syn:delivery_budget_counters(), 0)),
            %% 清理后该连接恢复满预算（轻通知槽一并释放）：
            %% 先排空真实邮箱积压，再发新帧应直达。
            _ = drain(Slow),
            {T2Id, T2Json} = group_msg(<<"t2">>),
            ok = message_ds:send_next(?UID_SLOW, T2Id, T2Json, [0]),
            settle(),
            ?assertEqual(1, length(data_frames(Slow)))
        end)
    end}.

%% ===================================================================
%% PG 轨道（标准 eunit-local 库）：事实滞留与删除尊重
%% ===================================================================

%% LOCAL:01/03：被降级帧的事实源在库中滞留可回读（慢端之后可补齐的
%% 事实基础）；timeline 删除被读路径尊重（删除不复活）。
pg_facts_survive_and_deletion_honored_test_() ->
    {"BE-14:LOCAL:01/03 facts stay queryable, deletion honored (PG)",
        {setup,
            fun() ->
                case eunit_runner:eunit_setup_with_db() of
                    {ok, Conn} -> {ok, Conn};
                    {error, _Reason} -> skip
                end
            end,
            fun
                ({ok, Conn}) -> eunit_runner:eunit_cleanup_db(Conn);
                (skip) -> ok
            end, fun
                ({ok, _Conn}) -> ?_test(pg_body());
                (skip) -> []
            end}}.

pg_body() ->
    ok = pg_cleanup(),
    ok = pg_setup(),
    try
        [ok = pg_insert_msg(I) || I <- lists:seq(1, 3)],
        %% 事实滞留：慢端降级期间事实仍可回读（补齐来源不丢）。
        Rows1 = msg_c2g_ds:read_msg(?BE14_PG_UID, 10, undefined),
        ?assertEqual(3, length(Rows1)),
        %% 删除：timeline 行删除后被读路径尊重（不复活）。
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM msg_c2g_timeline WHERE to_uid = $1 AND msg_id = $2">>,
            [?BE14_PG_UID, <<"be14pg2">>]
        ),
        Rows2 = msg_c2g_ds:read_msg(?BE14_PG_UID, 10, undefined),
        ?assertEqual(2, length(Rows2)),
        Ids = [maps:get(<<"msg_id">>, R) || R <- Rows2],
        ?assertNot(lists:member(<<"be14pg2">>, Ids)),
        ?assert(lists:member(<<"be14pg1">>, Ids)),
        ?assert(lists:member(<<"be14pg3">>, Ids))
    after
        ok = pg_cleanup()
    end.

%% ===================================================================
%% Fixture：纯行为轨道
%% ===================================================================

run_budget_case(Budget, Test) ->
    pure_mock_fixture:run(fun() ->
        application:set_env(imboy, delivery_budget_max_count, maps:get(max_count, Budget, 8)),
        application:set_env(
            imboy,
            delivery_budget_reserve_count,
            maps:get(reserve_count, Budget, 3)
        ),
        application:set_env(imboy, delivery_budget_max_bytes, 100000000),
        application:set_env(imboy, delivery_budget_reserve_bytes, 1),
        application:set_env(imboy, msg_store_worker_tick_ms, 0),
        ok = imboy_syn:reset_delivery_budget_state(),
        Slow = start_device(),
        Normal = start_device(),
        Members = #{
            ?UID_SLOW => [{Slow, {<<"ios">>, did_slow()}}],
            ?UID_NORMAL => [{Normal, {<<"android">>, did_normal()}}]
        },
        Mocks = [
            elib_pg,
            elib_log,
            elib_metric,
            elib_retry_config,
            msg_c2c_ds,
            msg_c2g_ds,
            msg_s2c_ds,
            micro_community_access_logic,
            syn
        ],
        [meck:new(M, [no_link, non_strict]) || M <- Mocks],
        %% ePG 硬禁：预算/降级/补齐推送路径不得写库（不虚增任何事实）。
        [
            meck:expect(elib_pg, F, A, meck:raise(error, {elib_pg_forbidden_in_budget_path, F, A}))
         || {F, A} <- elib_pg:module_info(exports), F =/= module_info
        ],
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, 5, fun(_, _, _, _, _) -> ok end),
        meck:expect(elib_metric, increment, 1, fun(_) -> ok end),
        meck:expect(elib_metric, increment, 2, fun(_, _) -> ok end),
        meck:expect(elib_retry_config, intervals, 1, fun(_) -> [0] end),
        meck:expect(msg_c2g_ds, read_msg, 3, fun(_, _, _) -> [] end),
        meck:expect(msg_s2c_ds, read_msg_for_device, 4, fun(_, _, _, _) -> [] end),
        %% 补拉事实源：m1/m2 存活，m3 已删除（读路径不返回）。
        SlowDid = did_slow(),
        meck:expect(
            msg_c2c_ds,
            read_msg_for_device,
            4,
            fun(Uid, Did, _Limit, _LastMsgAt) when
                Uid =:= ?UID_SLOW, Did =:= SlowDid
            ->
                [offline_row(<<"m1">>), offline_row(<<"m2">>)]
            end
        ),
        %% 授权守卫默认放行（授权负例由既有 e2ee/撤权套件覆盖；本卡
        %% 断言预算不绕过授权的结构性位置：门在投递上游、授权在下游）。
        meck:expect(
            micro_community_access_logic,
            delivery_allowed,
            2,
            fun(_Uid, _Msg) -> true end
        ),
        %% syn 成员查询定向打桩（broadcast_ack_cancel 直查 syn:members）。
        meck:expect(
            syn,
            members,
            fun
                (_Scope, ?UID_SLOW) -> maps:get(?UID_SLOW, Members);
                (_Scope, ?UID_NORMAL) -> maps:get(?UID_NORMAL, Members);
                (_Scope, _Uid) -> []
            end
        ),
        %% imboy_syn：仅 list_by_uid 定向打桩，其余（预算门/timer/轻通知/
        %% broadcast）passthrough 走真实现。
        meck:new(imboy_syn, [no_link, passthrough, non_strict]),
        meck:expect(imboy_syn, list_by_uid, fun(Uid) -> maps:get(Uid, Members, []) end),
        try
            Test(Members)
        after
            meck:unload(imboy_syn),
            [meck:unload(M) || M <- Mocks],
            stop_device(Slow),
            stop_device(Normal),
            ok = imboy_syn:reset_delivery_budget_state()
        end
    end).

did_slow() -> <<"did_be14_slow">>.
did_normal() -> <<"did_be14_normal">>.

member_pid(Uid, Members) ->
    [{Pid, _Meta}] = maps:get(Uid, Members),
    Pid.

%% 慢端设备 actor：不主动消费邮箱（receive 阻塞=慢），按指令排空。
start_device() ->
    Parent = self(),
    Pid = spawn(fun() -> device_loop(Parent) end),
    receive
        {device_started, Pid} -> ok
    after 2000 -> error(device_not_started)
    end,
    Pid.

device_loop(Parent) ->
    Parent ! {device_started, self()},
    receive
        {drain, _ReplyTo} = Cmd ->
            handle_drain(Cmd),
            device_loop(Parent);
        stop ->
            exit(normal)
    end.

handle_drain({drain, ReplyTo}) ->
    ReplyTo ! {drained, drain_all([])}.

drain_all(Frames) ->
    receive
        Msg -> drain_all([Msg | Frames])
    after 0 ->
        lists:reverse(Frames)
    end.

drain(Pid) ->
    Pid ! {drain, self()},
    receive
        {drained, Frames} -> Frames
    after 2000 -> error(drain_timeout)
    end.

stop_device(Pid) ->
    Mref = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Mref, process, Pid, _} -> ok
    after 1000 -> exit(Pid, kill)
    end.

%% 预置积压：junk 帧不可被 device_loop 消费，形成稳定 QLen。
stuff(Pid, N) ->
    [Pid ! {junk, erlang:unique_integer()} || _ <- lists:seq(1, N)],
    ok.

raw_messages(Pid) ->
    {messages, Msgs} = erlang:process_info(Pid, messages),
    Msgs.

%% 投递管道帧：{timeout, Ref, MsgBinary}；排除 junk/控制/轻通知。
data_frames(Pid) ->
    [Msg || {timeout, _Ref, Msg} <- raw_messages(Pid), is_binary(Msg), not is_notice(Msg)].

%% 重试续排帧：{timeout, Ref, {MsLi, TimerKey, Msg}}（hop>=1 timer fire）。
retry_frames(Pid) ->
    [M || {timeout, _Ref, {_MsLi, _TimerKey, _Msg} = M} <- raw_messages(Pid)].

notice_frames(Pid) ->
    [Msg || {timeout, _Ref, Msg} <- raw_messages(Pid), is_binary(Msg), is_notice(Msg)].

is_notice(Bin) ->
    try jsone:decode(Bin, [{object_format, map}]) of
        #{<<"action">> := <<"pull_offline_msg">>} -> true;
        _ -> false
    catch
        _:_ -> false
    end.

map_field(Bin, Key) when is_binary(Bin) ->
    map_field(jsone:decode(Bin, [{object_format, map}]), Key);
map_field(Map, Key) when is_map(Map) ->
    maps:get(Key, Map).

nested_field(Bin, Path) when is_binary(Bin) ->
    nested_field(jsone:decode(Bin, [{object_format, map}]), Path);
nested_field(Map, [Key]) ->
    maps:get(Key, Map);
nested_field(Map, [Key | Rest]) ->
    nested_field(maps:get(Key, Map), Rest).

envelope_id(FrameBin) ->
    map_field(FrameBin, <<"id">>).

%% hop0/轻通知现为同步直达（BE-14 FIX）；hop>=1 仍经真实 timer，
%% 断言前稍作稳定等待以覆盖 timer fire 延迟。
settle() ->
    timer:sleep(60).

%% ===================================================================
%% Fixture：PG 轨道
%% ===================================================================

pg_setup() ->
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO \"group\" (id, owner_uid, creator_uid, title, status) ",
            "VALUES ($1, $2, $2, 'be14_fixture', 1)">>,
        [?BE14_PG_GID, ?BE14_PG_SENDER]
    ),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO group_member (id, group_id, user_id, role, is_join, status) ",
            "VALUES ($1, $2, $3, 4, true, 1), ($4, $2, $5, 1, true, 1)">>,
        [93110001, ?BE14_PG_GID, ?BE14_PG_SENDER, 93110002, ?BE14_PG_UID]
    ),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO group_member_generation (group_id, user_id, generation_no, start_seq) ",
            "VALUES ($1, $2, 1, 1), ($1, $3, 1, 1)">>,
        [?BE14_PG_GID, ?BE14_PG_SENDER, ?BE14_PG_UID]
    ),
    ok.

pg_cleanup() ->
    _ = elib_pg:execute(<<"DELETE FROM msg_c2g_timeline WHERE to_uid = $1">>, [?BE14_PG_UID]),
    _ = elib_pg:execute(<<"DELETE FROM msg_c2g WHERE to_id = $1">>, [?BE14_PG_GID]),
    _ = elib_pg:execute(
        <<"DELETE FROM group_member_generation WHERE group_id = $1">>, [?BE14_PG_GID]
    ),
    _ = elib_pg:execute(<<"DELETE FROM group_member WHERE group_id = $1">>, [?BE14_PG_GID]),
    _ = elib_pg:execute(<<"DELETE FROM \"group\" WHERE id = $1">>, [?BE14_PG_GID]),
    ok.

pg_insert_msg(Seq) ->
    MsgId = <<"be14pg", (integer_to_binary(Seq))/binary>>,
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO msg_c2g (id, from_id, to_id, msg_id, msg_type, payload, created_at) ",
            "VALUES ($1, $2, $3, $4, 'text', '{\"content\":\"be14\"}', $5)">>,
        [93120000 + Seq, ?BE14_PG_SENDER, ?BE14_PG_GID, MsgId, ?BE14_PG_AT]
    ),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO msg_c2g_timeline (msg_id, to_uid, to_gid, created_at, conv_seq) ",
            "VALUES ($1, $2, $3, $4, $5)">>,
        [MsgId, ?BE14_PG_UID, ?BE14_PG_GID, ?BE14_PG_AT, Seq]
    ),
    ok.

%% ===================================================================
%% 消息构造
%% ===================================================================

group_msg(Id) ->
    {Id,
        jsone:encode(
            #{
                <<"ver">> => 2,
                <<"id">> => Id,
                <<"type">> => <<"C2G">>,
                <<"from">> => ?UID_SENDER,
                <<"to">> => ?GID,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null,
                <<"payload">> => #{<<"content">> => <<"be14-group">>},
                <<"server_ts">> => 1
            },
            [native_utf8]
        )}.

direct_msg(Id) ->
    {Id,
        jsone:encode(
            #{
                <<"ver">> => 2,
                <<"id">> => Id,
                <<"type">> => <<"C2C">>,
                <<"from">> => ?UID_SENDER,
                <<"to">> => ?UID_SLOW,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null,
                <<"payload">> => #{<<"content">> => <<"be14-direct">>},
                <<"server_ts">> => 1
            },
            [native_utf8]
        )}.

e2ee_msg(Id, Cipher) ->
    jsone:encode(
        #{
            <<"ver">> => 2,
            <<"id">> => Id,
            <<"type">> => <<"C2C">>,
            <<"from">> => ?UID_SENDER,
            <<"to">> => ?UID_SLOW,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<>>,
            <<"e2ee">> => #{
                <<"meta_version">> => 3,
                <<"protected_header">> => <<"b64hdr">>,
                <<"header_hash">> => <<"b64hash">>,
                <<"ciphertext">> => Cipher
            },
            <<"payload">> => <<>>,
            <<"server_ts">> => 1
        },
        [native_utf8]
    ).

%% 补拉读路径的离线行（offline_envelope 的输入形状，含 conv_seq/sender_did）。
offline_row(MsgId) ->
    SeqDigit = binary_to_integer(binary:part(MsgId, 1, 1)),
    #{
        <<"id">> => 93100000 + SeqDigit,
        <<"msg_id">> => MsgId,
        <<"from_id">> => ?UID_SENDER,
        <<"to_id">> => ?UID_SLOW,
        <<"msg_type">> => <<"text">>,
        <<"payload">> => <<"{\"content\":\"be14-backlog\"}">>,
        <<"e2ee">> => null,
        <<"created_at">> => <<"2026-11-21T02:00:00Z">>,
        <<"server_ts">> => <<"2026-11-21T02:00:00Z">>,
        <<"conv_seq">> => 42,
        <<"sender_did">> => <<"did_sender">>
    }.
