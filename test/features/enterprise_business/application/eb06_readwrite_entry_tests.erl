%%% @doc EB-06 重开（第二次）：§5 读/改入口与 A04 收口的验收套件（真库 + 静态）。
%%%
%%% 覆盖本轮新增的 11 条验收里尚未落在既有套件中的部分：
%%%   * **A09** `after_id` 键集分页全链（Port→adapter→usecase→facade）：游标指向
%%%     不存在/已删消息时行为明确；**改成 offset 实现必然被同一条断言抓住**。
%%%   * **A10** 只读历史入口经契约公开，且**只读**（seen/status/version 逐字不变）。
%%%   * **A12** `fetch_hold/3` 精确错误语义：不存在 / 已释放 / 跨 Org 三者可区分。
%%%   * **A13** A04 收口：application 层 `eb_pg_` **代码位置**归零（剥离注释后）；
%%%     两处直连改走 `eb_tx_port` / `eb_purge_port`。
%%%   * **A14** 投递 ACK 经契约，且**ACK 与 canonical 分离**（ACK 不删/不改 canonical）。
%%%   * **A16** retention policy 的 open/latest 经契约（latest 不再是私有函数）。
%%%   * **A17** `consent_evidence_kind` 消费：应用层只可能产出 `synthetic`/`undefined`；
%%%     无 consent 不得写证据；报告措辞仍为 `synthetic_state_machine_only`。
%%%   * **A19** HOLD-RELEASE-VS-PURGE 端到端（active 阻断 / 并发竞争 / release 后成功 /
%%%     历史 hold 行与原 resource_id 保留）。
%%%
%%% 时间全部由调用方注入；合成租户由 `eb_pg_test_fixture` 随机 TSID 隔离。
-module(eb06_readwrite_entry_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).
-define(SECONDS_PER_DAY, 86400).
-define(DAYS_1095, 1095).
-define(APP_REL, "src/features/enterprise_business/application").

readwrite_entry_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            case ?FIX:ensure_purge_role() of
                ok -> {ok, Conn};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 90, fun a09_after_id_keyset_paging_is_stable_across_inserts/0},
        {timeout, 90, fun a09_cursor_semantics_for_missing_and_deleted_ids/0},
        {timeout, 60, fun a09_offset_implementation_would_be_caught/0},
        {timeout, 60, fun a10_readonly_history_entry_touches_nothing/0},
        {timeout, 60, fun a10_readonly_entry_is_public_and_contract_backed/0},
        {timeout, 60, fun a12_fetch_hold_distinguishes_three_states/0},
        {timeout, 60, fun a12_hold_verdict_negative_control/0},
        {timeout, 60, fun a13_application_code_positions_have_zero_pg_modules/0},
        {timeout, 60, fun a13_direct_connects_are_replaced_by_use_case_ports/0},
        {timeout, 90, fun a14_ack_is_contract_backed_and_canonical_separated/0},
        {timeout, 90, fun a14_null_device_replay_observation/0},
        {timeout, 60, fun a16_policy_latest_is_public_and_contract_backed/0},
        {timeout, 60, fun a17_consent_evidence_kind_is_synthetic_or_absent/0},
        {timeout, 90, fun a19_hold_blocks_then_release_allows_purge/0},
        {timeout, 120, fun a19_concurrent_hold_insert_wins_over_purge/0}
    ];
cases(Other) ->
    erlang:error({eb06_readwrite_suite_db_unavailable, Other}).

%% ===================================================================
%% A09：after_id 键集分页（真实 DB + 不漂移）
%% ===================================================================

a09_after_id_keyset_paging_is_stable_across_inserts() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Base = ?FIX:id(),
        %% 5 条已到期的合成消息，id 留出间隔（便于插到游标之前）
        Ids = [Base + N * 1000 || N <- lists:seq(1, 5)],
        [insert_msg(Scope, <<"a09-", (integer_to_binary(I))/binary>>, I, due_at()) || I <- Ids],
        %% 第 1 页（键集：after_id 缺省 = 首页）
        {ok, Page1} = list_messages(Scope, #{limit => 2}),
        ?assertEqual([hd(Ids), lists:nth(2, Ids)], message_ids(Page1)),
        %% 第 2 页（游标 = 第 1 页末条）
        Cursor = lists:nth(2, Ids),
        {ok, Page2Before} = list_messages(Scope, #{after_id => Cursor, limit => 2}),
        ?assertEqual([lists:nth(3, Ids), lists:nth(4, Ids)], message_ids(Page2Before)),
        %% 翻页之间插入 id **小于游标** 的行（键集语义：不得影响第 2 页）
        LateId = hd(Ids) + 500,
        insert_msg(Scope, <<"a09-late">>, LateId, due_at()),
        {ok, Page2After} = list_messages(Scope, #{after_id => Cursor, limit => 2}),
        ?assertEqual(message_ids(Page2Before), message_ids(Page2After)),
        ?assertEqual(Page2Before, Page2After),
        %% 新增行确实存在（正控制：证明上面的「不变」不是因为它没写进去）
        {ok, All} = list_messages(Scope, #{limit => 200}),
        ?assert(lists:member(LateId, message_ids(All))),
        %% 分页边界：limit 越界 / after_id 非法一律 fail-closed（不静默当首页）
        ?assertMatch({error, {invalid_limit, 0}}, list_messages(Scope, #{limit => 0})),
        ?assertMatch({error, {invalid_limit, 999}}, list_messages(Scope, #{limit => 999})),
        ?assertMatch(
            {error, {invalid_after_id, -1}}, list_messages(Scope, #{after_id => -1})
        ),
        ?assertMatch(
            {error, {invalid_after_id, <<"x">>}},
            list_messages(Scope, #{after_id => <<"x">>})
        ),
        ?assertMatch(
            {error, {invalid_conversation_id, undefined}},
            list_messages(Scope, #{conversation_id => undefined})
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% A09 的边界语义：游标指向**不存在**或**已被物理删除**的消息时，行为是明确的
%% 「位置语义」（`id > after_id`），与「该 id 从未存在」逐字相同。
a09_cursor_semantics_for_missing_and_deleted_ids() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Base = ?FIX:id(),
        Ids = [Base + N * 1000 || N <- lists:seq(1, 5)],
        [insert_msg(Scope, <<"a09b-", (integer_to_binary(I))/binary>>, I, due_at()) || I <- Ids],
        %% ① 游标指向**从未存在**的 id（落在 I2 与 I3 之间）：返回 I3..I5
        Missing = lists:nth(2, Ids) + 500,
        {ok, R1} = list_messages(Scope, #{after_id => Missing}),
        ?assertEqual([lists:nth(3, Ids), lists:nth(4, Ids), lists:nth(5, Ids)], message_ids(R1)),
        %% ② 物理删除 I3（bounded purge 真删），再用 I3 作为游标：
        %%    结果与「用 I2 作为游标」逐字相同 —— 游标是位置，不是引用。
        ok = make_due(Scope, lists:nth(3, Ids)),
        {ok, Purged} = eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => now_secs()}),
        ?assert(maps:get(deleted, Purged) >= 1),
        ?assertEqual(0, alive(Org, Ws, lists:nth(3, Ids))),
        {ok, AfterDeleted} = list_messages(Scope, #{after_id => lists:nth(3, Ids)}),
        {ok, AfterPrev} = list_messages(Scope, #{after_id => lists:nth(2, Ids)}),
        ?assertEqual(message_ids(AfterPrev), message_ids(AfterDeleted)),
        %% ③ 游标大于所有 id ⇒ 空页（不是报错，也不是从头开始）
        {ok, EmptyPage} = list_messages(Scope, #{after_id => lists:last(Ids) + 100000}),
        ?assertEqual([], EmptyPage)
    after
        ?FIX:cleanup(Scope)
    end.

%% A09 负例（load-bearing）：把同一份数据换成 **offset** 实现，上面那条
%% 「翻页间插入 id<游标的行 ⇒ Page2 逐字不变」的断言必须判红。
%% 这里用两个纯函数把「键集 vs offset」的差别做成可机械判定的事实。
a09_offset_implementation_would_be_caught() ->
    Rows = [1000, 2000, 3000, 4000, 5000],
    Page1Keyset = keyset_page(Rows, 0, 2),
    Cursor = lists:last(Page1Keyset),
    ?assertEqual([1000, 2000], Page1Keyset),
    %% 翻页之间插入 1500（id < 游标）
    Rows2 = [1000, 1500, 2000, 3000, 4000, 5000],
    KeysetPage2 = keyset_page(Rows2, Cursor, 2),
    %% 键集：第 2 页逐字不变 ⇒ 断言成立
    ?assertEqual(keyset_page(Rows, Cursor, 2), KeysetPage2),
    ?assertEqual([3000, 4000], KeysetPage2),
    %% offset：第 2 页漂移 ⇒ 同一断言判红（这正是负例要证明的）
    OffsetPage2Before = offset_page(Rows, 1, 2),
    OffsetPage2After = offset_page(Rows2, 1, 2),
    ?assertEqual([2000, 3000], OffsetPage2Before),
    ?assertEqual([1500, 2000], OffsetPage2After),
    ?assertNotEqual(OffsetPage2Before, OffsetPage2After),
    %% 实现侧也必须是键集：SQL 里不得出现 OFFSET，且 after_id 是绑定参数
    Sql = binary:join(eb_pg_message_ext:sql_statements(), <<" ">>),
    Upper = string:uppercase(binary_to_list(Sql)),
    ?assertEqual(nomatch, string:find(Upper, "OFFSET")),
    ?assertNotEqual(nomatch, string:find(Sql, <<"id > $4">>)),
    %% 租户键必须显式贯穿（铁律 6）：语句里同时带 Org 与 Workspace 约束
    ?assertNotEqual(
        nomatch,
        re:run(Sql, <<"where organization_id = \\$1 and workspace_id = \\$2">>, [
            caseless, {capture, none}
        ])
    ),
    ok.

keyset_page(Rows, Cursor, Limit) ->
    lists:sublist([R || R <- lists:sort(Rows), R > Cursor], Limit).

offset_page(Rows, Offset, Limit) ->
    lists:sublist(lists:nthtail(Offset, lists:sort(Rows)), Limit).

%% ===================================================================
%% A10：只读历史入口
%% ===================================================================

a10_readonly_history_entry_touches_nothing() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        MsgId = insert_msg(Scope, <<"a10-readonly">>, ?FIX:id(), now_secs() + 86400),
        %% ① list_messages（键集）只读：行/字段 hash 与 delivery 行数逐字不变
        Before = message_row(Org, Ws, MsgId),
        DeliveriesBefore = deliveries(Org, Ws, MsgId),
        {ok, Listed} = eb_message_app:list_messages(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            after_id => MsgId - 1,
            limit => 10,
            key_ref => ?FIX:keyring_ref(Scope)
        }),
        ?assert(lists:member(MsgId, message_ids(Listed))),
        ?assertEqual(Before, message_row(Org, Ws, MsgId)),
        ?assertEqual(DeliveriesBefore, deliveries(Org, Ws, MsgId)),
        %% ② fetch_message（单条）只读：同上
        {ok, Fetched} = eb_message_app:fetch_message(Org, #{
            workspace_id => Ws, message_id => MsgId
        }),
        ?assertEqual(MsgId, maps:get(id, Fetched)),
        ?assertEqual(Before, message_row(Org, Ws, MsgId)),
        ?assertEqual(DeliveriesBefore, deliveries(Org, Ws, MsgId)),
        %% ③ version / visibility / retain_until 等字段逐字不变（不是「看起来没变」）
        ?assertEqual(maps:get(version, Before), maps:get(version, Fetched)),
        ?assertEqual(maps:get(visibility, Before), maps:get(visibility, Fetched)),
        ?assertEqual(maps:get(retain_until, Before), maps:get(retain_until, Fetched)),
        ?assertEqual(maps:get(content_hash, Before), maps:get(content_hash, Fetched)),
        %% ④ 不存在的消息 ⇒ not_found（也不产生任何行）
        Missing = ?FIX:id(),
        ?assertEqual(
            {error, not_found},
            eb_message_app:fetch_message(Org, #{workspace_id => Ws, message_id => Missing})
        ),
        ?assertEqual(0, deliveries(Org, Ws, Missing)),
        %% ⑤ 跨租户读（另一 Org 的 Workspace）⇒ 不返回本 Org 的消息
        OtherWs = maps:get(other_workspace_id, Scope),
        ?assertMatch(
            {error, _},
            eb_message_app:fetch_message(Org, #{workspace_id => OtherWs, message_id => MsgId})
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% 只读入口必须**经契约公开**（facade + application + Port 声明三处齐），
%% 且只读路径在源码上没有任何写形状的调用。
a10_readonly_entry_is_public_and_contract_backed() ->
    %% ① Port 声明
    Declared = eb_store_port:behaviour_info(callbacks),
    ?assert(lists:member({fetch_message, 3}, Declared)),
    ?assert(lists:member({list_messages_after, 3}, Declared)),
    %% ② registry
    Contracts = maps:get(eb_store_port, eb_ports:contracts()),
    ?assert(lists:member({fetch_message, 3}, Contracts)),
    ?assert(lists:member({list_messages_after, 3}, Contracts)),
    %% ③ application 公开入口
    ?assertMatch({module, eb_message_app}, code:ensure_loaded(eb_message_app)),
    ?assert(erlang:function_exported(eb_message_app, fetch_message, 2)),
    ?assert(erlang:function_exported(eb_message_app, list_messages, 2)),
    %% ④ facade 公开入口（形状收敛后委派）—— `function_exported/3` 对未加载模块恒假，
    %%    故先确保加载，否则断言恒真（假绿）。
    ?assertMatch(
        {module, enterprise_business_facade}, code:ensure_loaded(enterprise_business_facade)
    ),
    ?assert(erlang:function_exported(enterprise_business_facade, fetch_message, 2)),
    ?assert(erlang:function_exported(enterprise_business_facade, list_messages, 2)),
    %% ⑤ **行为**判定（比文本切片强）：用记账假 store 走一遍两个只读入口，断言
    %%    只调用了读 callback，且一次也没有碰到任何写形状 —— 写 callback 一旦被调到
    %%    就 raise（见 eb06_port_probe）。
    ok = eb06_port_probe:reset(#{tx_mode => ok}),
    {ok, _} = eb_message_app:list_messages(4242, #{
        workspace_id => 7, conversation_id => 9, store => eb06_port_probe
    }),
    {ok, _} = eb_message_app:fetch_message(4242, #{
        workspace_id => 7, message_id => 11, store => eb06_port_probe
    }),
    ?assertEqual(1, eb06_port_probe:count(store_read_list_messages_after)),
    ?assertEqual(1, eb06_port_probe:count(store_read_fetch_message)),
    ?assertEqual(0, eb06_port_probe:count(store_write_attempted)),
    %% 负例（load-bearing）：ACK 路径必须**确实**会碰到写 callback（同一记账假 store
    %% 会让它 raise）——证明上面的「0 次」不是因为这个假 store 根本记不到账。
    ?assertError(
        {write_shape_called_through_store, ack_delivery},
        eb_message_app:ack_delivery(4242, #{
            workspace_id => 7,
            message_id => 11,
            recipient_ref => <<"contact:1">>,
            acked_at => 1700000000,
            store => eb06_port_probe
        })
    ),
    ?assert(eb06_port_probe:count(store_write_attempted) >= 1).

%% ===================================================================
%% A12：fetch_hold/3 精确错误语义
%% ===================================================================

a12_fetch_hold_distinguishes_three_states() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        MsgId = insert_msg(Scope, <<"a12-hold">>, ?FIX:id(), due_at()),
        {ok, Created} = eb_retention_app:create_hold(Org, #{
            workspace_id => Ws,
            scope => <<"message">>,
            scope_message_id => MsgId,
            reason_code => <<"eb06-synthetic-hold-a12">>,
            synthetic => true,
            actor_user_id => maps:get(actor_user_id, Scope)
        }),
        HoldId = maps:get(hold_id, Created),
        %% ① active
        {ok, Active} = eb_retention_app:fetch_hold(Org, #{workspace_id => Ws, hold_id => HoldId}),
        ?assertEqual(active, maps:get(status, Active)),
        ?assertEqual(undefined, maps:get(released_at, Active)),
        %% ② 已释放
        {ok, _Released} = eb_retention_app:release_hold(Org, #{
            workspace_id => Ws,
            hold_id => HoldId,
            synthetic => true,
            actor_user_id => maps:get(actor_user_id, Scope)
        }),
        {ok, ReleasedView} = eb_retention_app:fetch_hold(Org, #{
            workspace_id => Ws, hold_id => HoldId
        }),
        ?assertEqual(released, maps:get(status, ReleasedView)),
        ?assert(is_integer(maps:get(released_at, ReleasedView))),
        %% ③ 不存在（本租户内无此 id）
        MissingHold = ?FIX:id(),
        ?assertEqual(
            {error, {hold_not_found, MissingHold}},
            eb_retention_app:fetch_hold(Org, #{workspace_id => Ws, hold_id => MissingHold})
        ),
        %% ④ 跨 Org（租户对不成立）⇒ 与「不存在」不同的错误
        OtherWs = maps:get(other_workspace_id, Scope),
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_retention_app:fetch_hold(Org, #{workspace_id => OtherWs, hold_id => HoldId})
        ),
        %% ⑤ 二次 release 的错误点名「已释放」（不再压成统一 not_releasable）
        ?assertMatch(
            {error, {hold_already_released, HoldId, _}},
            eb_retention_app:release_hold(Org, #{
                workspace_id => Ws,
                hold_id => HoldId,
                synthetic => true,
                actor_user_id => maps:get(actor_user_id, Scope)
            })
        ),
        %% ⑥ facade 入口存在且透传同一结论（先 ensure_loaded，防 function_exported 恒假）
        ?assertMatch(
            {module, enterprise_business_facade}, code:ensure_loaded(enterprise_business_facade)
        ),
        ?assert(erlang:function_exported(enterprise_business_facade, fetch_hold, 2)),
        ?assertEqual(
            {error, {hold_not_found, HoldId + 1}},
            enterprise_business_facade:fetch_hold(Org, #{workspace_id => Ws, hold_id => HoldId + 1})
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% A12 负例（load-bearing）：把三态压成同一个错误（旧实现的做法）⇒ 同一判定必须判红。
a12_hold_verdict_negative_control() ->
    Active = eb_retention_app:hold_lookup_verdict(
        ok, {ok, #{id => 7, released_at => undefined}}, 7
    ),
    Released = eb_retention_app:hold_lookup_verdict(ok, {ok, #{id => 7, released_at => 12345}}, 7),
    Missing = eb_retention_app:hold_lookup_verdict(ok, {error, not_found}, 7),
    CrossOrg = eb_retention_app:hold_lookup_verdict(
        {error, {workspace_not_in_org, 9}}, {error, scope_short_circuit}, 7
    ),
    ?assertEqual(active, maps:get(status, element(2, Active))),
    ?assertEqual(released, maps:get(status, element(2, Released))),
    ?assertEqual({error, {hold_not_found, 7}}, Missing),
    ?assertEqual({error, {workspace_not_in_org, 9}}, CrossOrg),
    %% 三种（不存在 / 已释放 / 跨 Org）两两不同 —— 这是 A12 的机械判据
    Three = [Released, Missing, CrossOrg],
    ?assertNotEqual(Missing, CrossOrg),
    ?assertNotEqual(Released, Missing),
    ?assertNotEqual(Released, CrossOrg),
    ?assertEqual(3, length(lists:usort(Three))),
    %% 负例：旧的「统一 not_releasable」实现会让上面 3 条互不相同这一条判红
    Collapsing = fun(_Scope, _Fetch, HoldId) -> {error, {hold_not_releasable, HoldId}} end,
    Collapsed = [
        Collapsing(ok, {ok, #{released_at => 1}}, 7),
        Collapsing(ok, {error, not_found}, 7),
        Collapsing({error, {workspace_not_in_org, 9}}, {error, scope_short_circuit}, 7)
    ],
    ?assertEqual(1, length(lists:usort(Collapsed))),
    ?assertNotEqual(length(lists:usort(Three)), length(lists:usort(Collapsed))).

%% ===================================================================
%% A13：A04 收口（application 层代码位置零 eb_pg_）
%% ===================================================================

a13_application_code_positions_have_zero_pg_modules() ->
    Files = filelib:wildcard(?APP_REL ++ "/**/*.erl"),
    ?assert(length(Files) > 0),
    Hits = [
        {F, Line}
     || F <- Files,
        {_N, Line} <- numbered_lines(without_comments(read_app(F))),
        binary:match(Line, <<"eb_pg_">>) =/= nomatch
    ],
    ?assertEqual([], Hits),
    %% 负例①：注释里的**边界声明**不得判红（不得靠删注释过门）
    ?assertEqual([], pg_hits(<<"%%% 本模块不触 eb_pg_store，零 SQL\n">>)),
    %% 负例②：代码位置命中必须判红（判定非恒真）
    ?assertEqual([<<"X = eb_pg_store:go(1) ">>], pg_hits(<<"X = eb_pg_store:go(1) % ok\n">>)).

pg_hits(Source) ->
    [
        L
     || {_N, L} <- numbered_lines(without_comments(Source)),
        binary:match(L, <<"eb_pg_">>) =/= nomatch
    ].

%% 两处**真代码**必须已经不在了；且默认装配改为用例级 Port。
a13_direct_connects_are_replaced_by_use_case_ports() ->
    MsgSrc = app_source(eb_message_app),
    RetSrc = app_source(eb_retention_app),
    %% ① 曾经的直连默认值不再出现（代码位置）
    ?assertEqual(
        nomatch, re:run(without_comments(MsgSrc), <<"eb_pg_canonical_tx">>, [{capture, none}])
    ),
    ?assertEqual(
        nomatch, re:run(without_comments(RetSrc), <<"eb_pg_purge[^_]">>, [{capture, none}])
    ),
    %% ② 改为经**用例级 Port**装配
    ?assertNotEqual(
        nomatch,
        re:run(without_comments(MsgSrc), <<"eb_infra_ports:resolve\\(tx\\)">>, [{capture, none}])
    ),
    ?assertNotEqual(
        nomatch,
        re:run(without_comments(RetSrc), <<"eb_infra_ports:resolve\\(purge\\)">>, [{capture, none}])
    ),
    %% ③ 端口实现 = EB-03R 交付的用例级实现；导出面只有具名用例（无通用事务接口）
    ?assertEqual({ok, eb_pg_tx}, eb_infra_ports:resolve(tx)),
    ?assertEqual({ok, eb_pg_purge_port}, eb_infra_ports:resolve(purge)),
    ?assertEqual(
        [{accept_message, 3}, {append_conversation_audit, 3}],
        lists:sort([{N, A} || {N, A} <- eb_pg_tx:module_info(exports), N =/= module_info])
    ),
    ?assertEqual(
        [{purge_batch, 4}],
        lists:sort([{N, A} || {N, A} <- eb_pg_purge_port:module_info(exports), N =/= module_info])
    ),
    %% ④ 负例（load-bearing）：把直连字符串注入「剥离注释后」的文本，判定必须命中
    ?assertNotEqual(
        nomatch,
        re:run(
            <<(without_comments(MsgSrc))/binary, "_ -> {ok, eb_pg_canonical_tx}">>,
            <<"eb_pg_canonical_tx">>,
            [{capture, none}]
        )
    ),
    %% ⑤ purge 信封是 /4（A18 同口径）
    ?assertMatch(
        {ok, _},
        begin
            Scope = ?FIX:new_scope(),
            try
                {Org, Ws} = tenant(Scope),
                eb_retention_app:purge_batch(Org, #{
                    workspace_id => Ws, now => now_secs(), batch_limit => 5
                })
            after
                ?FIX:cleanup(Scope)
            end
        end
    ).

%% ===================================================================
%% A14：ACK 经契约 + ACK/canonical 分离
%% ===================================================================

a14_ack_is_contract_backed_and_canonical_separated() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        MsgId = insert_msg(Scope, <<"a14-ack">>, ?FIX:id(), now_secs() + 86400),
        BeforeRow = message_row(Org, Ws, MsgId),
        BeforeCount = ?FIX:count(Org, Ws, messages),
        BeforeDeliveries = deliveries(Org, Ws, MsgId),
        RecipientRef = <<"contact:", (integer_to_binary(maps:get(contact_id, Scope)))/binary>>,
        %% 幂等键 = (message, recipient, device)（迁移 116 的 uq_emd_org_message_recipient_device）
        Device = <<"eb06-device-1">>,
        {ok, First} = eb_message_app:ack_delivery(Org, #{
            workspace_id => Ws,
            message_id => MsgId,
            recipient_ref => RecipientRef,
            device_id => Device,
            acked_at => now_secs()
        }),
        ?assertEqual(true, maps:get(canonical_unchanged, First)),
        AfterDeliveries = deliveries(Org, Ws, MsgId),
        ?assertEqual(BeforeDeliveries + 1, AfterDeliveries),
        %% 幂等：第二次不增 delivery 行
        {ok, Second} = eb_message_app:ack_delivery(Org, #{
            workspace_id => Ws,
            message_id => MsgId,
            recipient_ref => RecipientRef,
            device_id => Device,
            acked_at => now_secs()
        }),
        ?assertEqual(true, maps:get(canonical_unchanged, Second)),
        ?assertEqual(AfterDeliveries, deliveries(Org, Ws, MsgId)),
        %% ACK **不删/不改** canonical：行数与整行 hash 逐字不变
        ?assertEqual(BeforeCount, ?FIX:count(Org, Ws, messages)),
        ?assertEqual(BeforeRow, message_row(Org, Ws, MsgId)),
        %% 契约面：ack_delivery/3 在 Port 声明 + registry + 实现导出（三处齐）
        ?assert(lists:member({ack_delivery, 3}, eb_store_port:behaviour_info(callbacks))),
        ?assert(lists:member({ack_delivery, 3}, maps:get(eb_store_port, eb_ports:contracts()))),
        ?assert(
            lists:member(
                {ack_delivery, 3},
                [{N, A} || {N, A} <- eb_pg_store:module_info(exports), N =/= module_info]
            )
        ),
        %% domain 判据有牙齿：改写一行 canonical 必须被抓
        Before = eb_message_app:canonical_view([message_row(Org, Ws, MsgId)]),
        Mutated = eb_message_app:canonical_view([
            (message_row(Org, Ws, MsgId))#{content_hash => <<"tampered">>}
        ]),
        ?assertMatch({error, _}, eb_message:ack_preserves_canonical(Before, Mutated)),
        ?assertEqual(ok, eb_message:ack_preserves_canonical(Before, Before)),
        %% 消息仍可读且 conversation 关联不变（ACK 不迁移/不改归属）
        {ok, Row} = eb_message_app:fetch_message(Org, #{workspace_id => Ws, message_id => MsgId}),
        ?assertEqual(Conv, maps:get(conversation_id, Row)),
        ?assertEqual(Ws, maps:get(workspace_id, Row))
    after
        ?FIX:cleanup(Scope)
    end.

%% A14：BUG-01 修复后的**幂等通过判据**（原 EB06-C3 偏离观察已闭环）。
%% 原观察：迁移 116 的普通 UNIQUE 下 device_id=NULL 重放插双行（RESULT.json
%% findings[EB06-C3] 登记「只记录不修」）。迁移 00000123（BUG-01，
%% RULING-2026-09-15 授权）改为 UNIQUE NULLS NOT DISTINCT 后，同一逻辑键的
%% NULL 重放命中 ON CONFLICT 目标 ⇒ 幂等。观察用例按登记提示同步翻转为
%% 通过判据：两次 ACK 恰一行，canonical 仍逐字不变。
a14_null_device_replay_observation() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        MsgId = insert_msg(Scope, <<"a14-null-device">>, ?FIX:id(), now_secs() + 86400),
        RecipientRef = <<"contact:", (integer_to_binary(maps:get(contact_id, Scope)))/binary>>,
        ?assertEqual(0, deliveries(Org, Ws, MsgId)),
        {ok, _} = eb_message_app:ack_delivery(Org, #{
            workspace_id => Ws,
            message_id => MsgId,
            recipient_ref => RecipientRef,
            acked_at => now_secs()
        }),
        ?assertEqual(1, deliveries(Org, Ws, MsgId)),
        {ok, _} = eb_message_app:ack_delivery(Org, #{
            workspace_id => Ws,
            message_id => MsgId,
            recipient_ref => RecipientRef,
            acked_at => now_secs()
        }),
        %% BUG-01 修复后：NULL device 重放两轮恰一行（幂等通过判据）。
        %% canonical 仍逐字不变（这一条仍是**通过判据**）。
        ?assertEqual(1, deliveries(Org, Ws, MsgId)),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_message"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
                >>,
                [Org, Ws, MsgId]
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A16：policy open / latest 经契约
%% ===================================================================

a16_policy_latest_is_public_and_contract_backed() ->
    Scope = ?FIX:new_scope(#{with_policy => false}),
    try
        {Org, Ws} = tenant(Scope),
        %% ① 无策略 ⇒ not_found（没有默认保留期）
        ?assertEqual(
            {error, not_found},
            eb_retention_app:latest_retention_policy(Org, #{
                workspace_id => Ws, data_class => <<"enterprise_message">>
            })
        ),
        %% ② open（经 Store:insert_policy/3）
        {ok, Opened} = eb_retention_app:open_retention_policy(Org, #{
            workspace_id => Ws,
            data_class => <<"enterprise_message">>,
            retention_days => ?DAYS_1095,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(1, maps:get(version, Opened)),
        %% ③ latest 是**公开**入口（不再是私有函数），且读数与写入一致
        {ok, Latest} = eb_retention_app:latest_retention_policy(Org, #{
            workspace_id => Ws, data_class => <<"enterprise_message">>
        }),
        ?assertEqual(1, maps:get(version, Latest)),
        ?assertEqual(?DAYS_1095, maps:get(retention_days, Latest)),
        ?assertEqual(maps:get(policy_id, Opened), maps:get(id, Latest)),
        %% ④ 延长后 latest 跟进；缩短被拒（既有语义不变）
        {ok, _V2} = eb_retention_app:open_retention_policy(Org, #{
            workspace_id => Ws,
            data_class => <<"enterprise_message">>,
            retention_days => ?DAYS_1095 + 30,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        {ok, Latest2} = eb_retention_app:latest_retention_policy(Org, #{
            workspace_id => Ws, data_class => <<"enterprise_message">>
        }),
        ?assertEqual(2, maps:get(version, Latest2)),
        ?assertMatch(
            {error, _},
            eb_retention_app:open_retention_policy(Org, #{
                workspace_id => Ws,
                data_class => <<"enterprise_message">>,
                retention_days => ?DAYS_1095 - 1,
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        ),
        %% ⑤ 未知 data_class ⇒ 早失败
        ?assertMatch(
            {error, {unknown_data_class, <<"nope">>}},
            eb_retention_app:latest_retention_policy(Org, #{
                workspace_id => Ws, data_class => <<"nope">>
            })
        ),
        %% ⑥ facade 入口存在（§5 公开面）；先 ensure_loaded 防 function_exported 恒假
        ?assertMatch(
            {module, enterprise_business_facade}, code:ensure_loaded(enterprise_business_facade)
        ),
        ?assert(erlang:function_exported(enterprise_business_facade, latest_retention_policy, 2)),
        %% ⑦ 「经契约」的可判定事实：读只经 latest_policy/3 callback；调用方契约门由
        %%    scripts/check_eb_port_closure.sh 的 A01 机械判定（调用点 ⊆ 声明）。
        ?assert(lists:member({latest_policy, 3}, maps:get(eb_store_port, eb_ports:contracts()))),
        ?assert(lists:member({insert_policy, 3}, maps:get(eb_store_port, eb_ports:contracts())))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A17：consent_evidence_kind 消费
%% ===================================================================

a17_consent_evidence_kind_is_synthetic_or_absent() ->
    %% ① 应用层**唯一**可表达的取值：synthetic | undefined（对敌意输入同样成立）
    Adversarial = [
        #{kind => real},
        #{consent_evidence_kind => <<"real">>},
        #{consent_evidence_kind => <<"verified_real">>},
        #{kind => <<"real">>},
        #{kind => real, synthetic => false},
        #{consent_at => 1700000000, notice_version => <<"v">>, consent_subject => <<"x">>},
        #{},
        undefined,
        null
    ],
    lists:foreach(
        fun(Input) ->
            Kind = eb_consent_app:evidence_kind(Input),
            ?assert(
                Kind =:= undefined orelse Kind =:= <<"synthetic">>
            )
        end,
        Adversarial
    ),
    %% ② 有 consent 且是合成件 ⇒ synthetic
    Synthetic = eb_consent_app:synthetic_fields(4242, #{consent_at => 1700000000}),
    {ok, Consent} = Synthetic,
    ?assertEqual(<<"synthetic">>, eb_consent_app:evidence_kind(Consent)),
    %% ③ 无 consent ⇒ undefined（不得写证据）
    ?assertEqual(undefined, eb_consent_app:evidence_kind(#{})),
    %% ④ 消费 EB-03R 的 M2 列（机械判定，不扫普通文本）：
    %%    (a) 唯一写入路径的 SQL 只有字面量 'synthetic'、无 DEFAULT、要求 consent 非空
    Sqls = eb_pg_consent_evidence:sql_statements(),
    Write = hd(Sqls),
    ?assertNotEqual(nomatch, re:run(Write, <<"'synthetic'">>, [{capture, none}])),
    ?assertEqual(nomatch, re:run(Write, <<"DEFAULT">>, [{capture, none}])),
    ?assertEqual(nomatch, re:run(Write, <<"'real'">>, [{capture, none}])),
    ?assertNotEqual(nomatch, re:run(Write, <<"consent_at IS NOT NULL">>, [{capture, none}])),
    %%    (b) DB CHECK 的非空取值集合恰为 {synthetic}
    Def = ?FIX:scalar(
        <<
            "SELECT pg_get_constraintdef(oid) FROM pg_constraint"
            " WHERE conname='ck_ec_consent_evidence_kind'"
        >>,
        []
    ),
    ?assertNotEqual(undefined, Def),
    ?assertNotEqual(nomatch, re:run(Def, <<"'synthetic'">>, [{capture, none}])),
    ?assertEqual(nomatch, re:run(Def, <<"'real'">>, [{capture, none}])),
    ?assertEqual(nomatch, re:run(Def, <<"verified_real">>, [{capture, none}])),
    %%    (c) 无 consent 的行该列必须为 NULL（DB CHECK 与写入语句双保险）
    ?assertEqual(
        0,
        ?FIX:scalar(
            <<
                "SELECT count(*) FROM enterprise_conversation"
                " WHERE consent_at IS NULL AND consent_evidence_kind IS NOT NULL"
            >>,
            []
        )
    ),
    %%    (d) 报告措辞仍为 synthetic_state_machine_only（不得升级）
    Evidence = eb_consent_app:evidence(Consent),
    ?assertEqual(synthetic_state_machine_only, maps:get(status, Evidence)),
    ?assertEqual(false, maps:get(real_consent_claim, Evidence)),
    ?assertEqual(false, maps:get(compliance_claim, Evidence)),
    %% ⑤ 负例：把任意一个对抗输入喂给「会升级」的实现必须与上面的断言冲突
    Upgradeable = fun(Term) ->
        case maps:get(kind, eb_consent_app:verdict(Term)) of
            synthetic -> <<"synthetic">>;
            _ -> <<"real">>
        end
    end,
    ?assertEqual(<<"real">>, Upgradeable(#{})).

%% ===================================================================
%% A19：HOLD-RELEASE-VS-PURGE 端到端
%% ===================================================================

a19_hold_blocks_then_release_allows_purge() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Actor = maps:get(actor_user_id, Scope),
        MsgId = insert_msg(Scope, <<"a19-e2e">>, ?FIX:id(), due_at()),
        {ok, Created} = eb_retention_app:create_hold(Org, #{
            workspace_id => Ws,
            scope => <<"message">>,
            scope_message_id => MsgId,
            reason_code => <<"eb06-synthetic-hold-a19">>,
            synthetic => true,
            actor_user_id => Actor
        }),
        HoldId = maps:get(hold_id, Created),
        %% (a) active hold ⇒ DB 层拒绝：一行不删，且被点名 skipped
        {ok, Sum1} = eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => now_secs()}),
        ?assertEqual(0, maps:get(deleted, Sum1)),
        ?assertEqual(1, alive(Org, Ws, MsgId)),
        ?assert(lists:member({MsgId, {ineligible, active_hold}}, maps:get(skipped, Sum1))),
        %% (c) release 且仍到期 ⇒ purge 成功
        {ok, _Released} = eb_retention_app:release_hold(Org, #{
            workspace_id => Ws, hold_id => HoldId, synthetic => true, actor_user_id => Actor
        }),
        {ok, Sum2} = eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => now_secs()}),
        ?assertEqual(1, maps:get(deleted, Sum2)),
        ?assertEqual(0, alive(Org, Ws, MsgId)),
        %% (d) 历史 hold 行与原 resource_id 仍在（审计可追；M1 派生列方案）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<"SELECT count(*) FROM enterprise_retention_hold WHERE organization_id=$1 AND id=$2">>,
                [Org, HoldId]
            )
        ),
        ?assertEqual(
            MsgId,
            ?FIX:scalar(
                <<"SELECT scope_message_id FROM enterprise_retention_hold WHERE organization_id=$1 AND id=$2">>,
                [Org, HoldId]
            )
        ),
        %% 派生列在 released 后为 NULL（正是它让引用不再阻断 purge）
        ?assertEqual(
            null,
            ?FIX:scalar(
                <<
                    "SELECT active_scope_message_id FROM enterprise_retention_hold"
                    " WHERE organization_id=$1 AND id=$2"
                >>,
                [Org, HoldId]
            )
        ),
        %% fetch_hold 仍能读到该历史行（已释放态）
        {ok, View} = eb_retention_app:fetch_hold(Org, #{workspace_id => Ws, hold_id => HoldId}),
        ?assertEqual(released, maps:get(status, View)),
        ?assertEqual(MsgId, maps:get(scope_message_id, maps:get(hold, View)))
    after
        ?FIX:cleanup(Scope)
    end.

%% (b) 并发竞争：purge 与 insert_hold 同时进行 ⇒ 被 hold 的 resource 不得被删
%%（防「先查后删」窗口；DB 行锁 + RI 最新快照复核共同序列化）。
a19_concurrent_hold_insert_wins_over_purge() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        MsgId = insert_msg(Scope, <<"a19-race">>, ?FIX:id(), due_at()),
        Now = now_secs(),
        HoldId = ?FIX:id(),
        Parent = self(),
        Holder = spawn(fun() ->
            _ = elib_pg:with_tx(
                fun(Conn) ->
                    {ok, _} = elib_pg:execute(
                        Conn,
                        <<
                            "INSERT INTO enterprise_retention_hold"
                            " (id,organization_id,workspace_id,scope_type,scope_message_id,"
                            "  reason_code,actor_user_id,version)"
                            " VALUES ($1,$2,$3,'message',$4,'eb06-a19-race',$5,1)"
                        >>,
                        [HoldId, Org, Ws, MsgId, maps:get(actor_user_id, Scope)]
                    ),
                    Parent ! {hold_inserted, self()},
                    timer:sleep(1200),
                    ok
                end,
                [{reraise, false}]
            ),
            Parent ! holder_done
        end),
        receive
            {hold_inserted, _Pid} -> ok
        after 10000 -> erlang:error(a19_hold_holder_never_inserted)
        end,
        {ok, Summary} = eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => Now}),
        %% 被 hold 的行一行都不许删（SKIP LOCKED 跳过 / DB 守卫拒绝，两条路都多保留）
        ?assertEqual(0, maps:get(deleted, Summary)),
        ?assertEqual(1, alive(Org, Ws, MsgId)),
        receive
            holder_done -> ok
        after 10000 -> ok
        end,
        %% 持锁方提交后 hold 生效中：第二次 purge 仍不得删
        {ok, Second} = eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => Now}),
        ?assertEqual(0, maps:get(deleted, Second)),
        ?assertEqual(1, alive(Org, Ws, MsgId)),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_retention_hold"
                    " WHERE organization_id=$1 AND id=$2 AND released_at IS NULL"
                >>,
                [Org, HoldId]
            )
        ),
        _ = Holder,
        ok
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

now_secs() ->
    eb_system_clock:now().

due_at() ->
    now_secs() - 120.

%% 直接走 store 的 append_message/3 造消息（与 canonical 事务共用同一套 sender 合同）。
insert_msg(Scope, ClientMsgId, MsgId, RetainUntil) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    Aad = #{
        organization_id => Org, workspace_id => Ws, conversation_id => Conv, message_id => MsgId
    },
    {ok, Sealed} = eb_managed_crypto:seal(Aad, <<"eb06-rw-body">>, ?FIX:keyring_ref(Scope)),
    {ok, _Row} = eb_pg_store:append_message(Org, Ws, #{
        id => MsgId,
        conversation_id => Conv,
        client_msg_id => ClientMsgId,
        sender_type => <<"contact">>,
        sender_contact_id => maps:get(contact_id, Scope),
        sender_business_identity_id => null,
        actor_user_id => null,
        body_cipher => maps:get(cipher, Sealed),
        key_version => maps:get(key_version, Sealed),
        aad_hash => maps:get(aad_hash, Sealed),
        content_hash => binary:encode_hex(crypto:hash(sha256, maps:get(cipher, Sealed))),
        policy_id => maps:get(policy_id, Scope),
        policy_version => 1,
        retention_days => 1095,
        retain_until => RetainUntil
    }),
    MsgId.

%% 把某条合成消息改成「已到期」（直接改 retain_until；本套件只验证机制）。
make_due(Scope, MsgId) ->
    ok = ?FIX:exec(
        <<
            "UPDATE enterprise_message SET retain_until = to_timestamp($4)"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [maps:get(org_id, Scope), maps:get(workspace_id, Scope), MsgId, due_at()]
    ).

list_messages(Scope, Extra) ->
    %% 显式回传 scope 级 keyring（与 insert_msg 同一把 key）：读面解密真正
    %% 执行且与 IMBOY_EB_ENTERPRISE_KEYRING_FILE 是否导出无关——修掉
    %% 「keyring 缺席=密文投影假绿 / keyring 在场=随机单 key 必挂」的二态
    %% 依赖（单跑 3 FAIL 的治根）。
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    eb_message_app:list_messages(
        Org,
        maps:merge(
            #{
                workspace_id => Ws,
                conversation_id => Conv,
                key_ref => ?FIX:keyring_ref(Scope)
            },
            Extra
        )
    ).

message_ids(Rows) ->
    [maps:get(id, R) || R <- Rows].

message_row(Org, Ws, MsgId) ->
    {ok, Row} = eb_pg_store:fetch_message(Org, Ws, MsgId),
    Row.

deliveries(Org, Ws, MsgId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_message_delivery"
            " WHERE organization_id=$1 AND workspace_id=$2 AND message_id=$3"
        >>,
        [Org, Ws, MsgId]
    ).

alive(Org, Ws, MsgId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MsgId]
    ).

app_source(Mod) ->
    Rel = ?APP_REL ++ "/" ++ module_rel(Mod),
    [Path | _] = [P || P <- [Rel, "../../" ++ Rel], filelib:is_file(P)],
    {ok, Bin} = file:read_file(Path),
    Bin.

module_rel(eb_message_app) -> "message/eb_message_app.erl";
module_rel(eb_retention_app) -> "retention/eb_retention_app.erl";
module_rel(eb_consent_app) -> "consent/eb_consent_app.erl";
module_rel(eb_conversation_app) -> "conversation/eb_conversation_app.erl".

read_app(Rel) ->
    {ok, Bin} = file:read_file(Rel),
    Bin.

%% 去掉 Erlang 行注释（保留其余文本）—— A13 判定只针对**代码位置**。
without_comments(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    iolist_to_binary([
        [re:replace(Line, <<"%.*$">>, <<>>, [{return, binary}]), <<"\n">>]
     || Line <- Lines
    ]).

numbered_lines(Bin) ->
    lists:zip(
        lists:seq(1, length(binary:split(Bin, <<"\n">>, [global]))),
        binary:split(Bin, <<"\n">>, [global])
    ).
