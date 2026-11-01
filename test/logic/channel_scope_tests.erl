-module(channel_scope_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T5 — channel scope 感知单元测试
%%% 覆盖：scope 创建校验（Guest 拒建/非成员拒建/非法 workspace_id 400/
%%% 非法 scope 400）、personal 默认零变化、workspace 创建注入 scope 字段、
%%% scope 不可变（update_channel_checked 400）。

-define(UID, 900001).
-define(WS_ID, 800001).
-define(CID, 666001).

%% J27.a/c/f: real handler + response encoder + cursor signing; synthetic DS only.
j27_a_legacy_channel_lists_are_not_truncated_test_() ->
    j27_channel_fixture(fun() ->
        Rows = [#{<<"id">> => I} || I <- lists:seq(1, 61)],
        put(j27_channel_rows, Rows),
        Sub = j27_channel_request(
            subscribed,
            [{<<"cursor">>, <<"7">>}, {<<"limit">>, <<"2">>}]
        ),
        ?assertEqual(0, maps:get(<<"code">>, Sub)),
        ?assertEqual(
            #{<<"list">> => Rows, <<"cursor">> => 7, <<"limit">> => 2},
            maps:get(<<"payload">>, Sub)
        ),
        Managed = j27_channel_request(managed, []),
        ?assertEqual(#{<<"list">> => Rows}, maps:get(<<"payload">>, Managed)),
        ?assertEqual(0, meck:num_calls(channel_ds, list_subscribed_keyset, '_')),
        ?assertEqual(0, meck:num_calls(channel_ds, list_managed_keyset, '_'))
    end).

j27_c_channel_cursor_progress_and_foreign_rejection_test_() ->
    j27_channel_fixture(fun() ->
        lists:foreach(
            fun({Action, Kind}) ->
                put(j27_channel_rows, [
                    #{<<"id">> => I, <<"cursor_id">> => I}
                 || I <- [30, 20, 10]
                ]),
                Qs = [
                    {<<"pagination">>, <<"cursor">>},
                    {<<"limit">>, <<"2">>},
                    {<<"q">>, <<"needle">>}
                ],
                Page = maps:get(<<"payload">>, j27_channel_request(Action, Qs)),
                ?assertEqual(true, maps:get(<<"has_more">>, Page)),
                ?assertEqual([30, 20], [
                    maps:get(<<"id">>, R)
                 || R <- maps:get(<<"list">>, Page)
                ]),
                Token = maps:get(<<"next_cursor">>, Page),
                Ctx = #{scope => <<"personal">>, workspace_id => 0, q => <<"needle">>},
                ?assertEqual({ok, 20}, list_cursor:resolve(Token, ?UID, Kind, Ctx)),
                put(j27_channel_rows, [#{<<"id">> => 10, <<"cursor_id">> => 10}]),
                _ = j27_channel_request(Action, [{<<"cursor">>, Token} | Qs]),
                ?assertEqual({?UID, 20, 3, <<"needle">>}, get(j27_channel_keyset)),
                lists:foreach(
                    fun(ForeignCtx) ->
                        {ok, Foreign} = list_cursor:issue(
                            ?UID,
                            Kind,
                            ForeignCtx#{upper => 20},
                            os:system_time(second)
                        ),
                        erase(j27_channel_keyset),
                        Error = j27_channel_request(Action, [{<<"cursor">>, Foreign} | Qs]),
                        ?assertEqual(400, maps:get(<<"code">>, Error)),
                        ?assertEqual(undefined, get(j27_channel_keyset))
                    end,
                    [
                        Ctx#{scope => <<"workspace">>, workspace_id => ?WS_ID},
                        Ctx#{q => <<"different">>}
                    ]
                )
            end,
            [{subscribed, <<"channel_subscribed">>}, {managed, <<"channel_managed">>}]
        )
    end).

j27_c_channel_terminal_cursor_is_json_null_test_() ->
    j27_channel_fixture(fun() ->
        put(j27_channel_rows, [#{<<"id">> => 10, <<"cursor_id">> => 10}]),
        lists:foreach(
            fun(Action) ->
                Page = maps:get(
                    <<"payload">>,
                    j27_channel_request(
                        Action,
                        [{<<"pagination">>, <<"cursor">>}, {<<"limit">>, <<"2">>}]
                    )
                ),
                ?assertEqual(false, maps:get(<<"has_more">>, Page)),
                %% Plan J27.c oracle: empty string is NOT JSON null.
                ?assertEqual(null, maps:get(<<"next_cursor">>, Page))
            end,
            [subscribed, managed]
        )
    end).

%% Handler legacy reads use synthetic DS rows; contract gating belongs to the App.
j27_f_channel_enabled_nonempty_legacy_handler_reads_test_() ->
    j27_channel_fixture(fun() ->
        {ok, _} = meck_helper:setup_mock(list_cursor, [passthrough, no_link], []),
        try
            put(j27_channel_enabled, true),
            Rows = [#{<<"id">> => ?CID, <<"name">> => <<"synthetic-channel">>}],
            put(j27_channel_rows, Rows),
            ?assertEqual(true, maps:get(channel, imboy_policy:effective_features())),
            Sub = j27_channel_request(
                subscribed,
                [{<<"cursor">>, <<"7">>}, {<<"limit">>, <<"2">>}]
            ),
            ?assertEqual(0, maps:get(<<"code">>, Sub)),
            ?assertEqual(
                #{<<"list">> => Rows, <<"cursor">> => 7, <<"limit">> => 2},
                maps:get(<<"payload">>, Sub)
            ),
            Managed = j27_channel_request(managed, []),
            ?assertEqual(0, maps:get(<<"code">>, Managed)),
            ?assertEqual(#{<<"list">> => Rows}, maps:get(<<"payload">>, Managed)),
            ?assertEqual(1, meck:num_calls(channel_ds, list_subscribed, '_')),
            ?assertEqual(1, meck:num_calls(channel_ds, list_managed, '_')),
            ?assertEqual(0, meck:num_calls(channel_ds, list_subscribed_keyset, '_')),
            ?assertEqual(0, meck:num_calls(channel_ds, list_managed_keyset, '_')),
            ?assertEqual(0, meck:num_calls(list_cursor, issue, '_')),
            ?assertEqual(0, meck:num_calls(list_cursor, resolve, '_'))
        after
            meck_helper:cleanup_mock(list_cursor)
        end
    end).

j27_f_channel_disabled_blocks_direct_reads_and_writes_test_() ->
    j27_channel_fixture(fun() ->
        put(j27_channel_enabled, false),
        lists:foreach(
            fun(Action) ->
                Response = j27_channel_request(Action, []),
                ?assertEqual(5190, maps:get(<<"code">>, Response))
            end,
            [create, show, subscribed]
        ),
        ?assertEqual(0, meck:num_calls(workspace_resolver, guard_channel_binding, '_')),
        ?assertEqual(0, meck:num_calls(channel_ds, list_subscribed, '_'))
    end).

j27_channel_fixture(Test) ->
    Mocks = [
        {cowboy_req, [
            {parse_qs, 1, fun(Req) -> maps:get(qs, Req, []) end},
            {read_body, 2, fun(Req, _Opts) ->
                {ok, maps:get(body, Req), Req}
            end},
            {reply, 4, fun(Status, _Headers, Body, Req) ->
                Req#{status => Status, response => jsone:decode(Body)}
            end}
        ]},
        {workspace_resolver, [{guard_channel_binding, 2, fun(_, _) -> ok end}]},
        {imboy_policy, [
            {effective_features, 0, fun() ->
                #{channel => get(j27_channel_enabled) =/= false}
            end}
        ]},
        {config_ds, [
            {env, 3, fun(imboy, list_cursor_signing_key, _) ->
                <<"j27-synthetic-signing-key-32-bytes-only">>
            end}
        ]},
        {channel_logic_common, [{channel_transfer, 1, fun(Row) -> Row end}]},
        {channel_ds, [
            {list_subscribed, 3, fun(?UID, _, <<>>) ->
                {ok, get(j27_channel_rows)}
            end},
            {list_managed, 2, fun(?UID, <<>>) -> {ok, get(j27_channel_rows)} end},
            {list_subscribed_keyset, 4, fun j27_channel_keyset/4},
            {list_managed_keyset, 4, fun j27_channel_keyset/4}
        ]}
    ],
    {setup,
        fun() ->
            lists:foreach(
                fun({Mod, Exp}) ->
                    {ok, _} = meck_helper:setup_mock(Mod, Exp)
                end,
                Mocks
            )
        end,
        fun(_) ->
            meck_helper:cleanup_mocks([Mod || {Mod, _} <- Mocks])
        end,
        fun(_) -> ?_test(Test()) end}.

j27_channel_keyset(Uid, Before, Limit, Q) ->
    put(j27_channel_keyset, {Uid, Before, Limit, Q}),
    {ok, get(j27_channel_rows)}.

j27_channel_request(Action, Qs) ->
    erase({elib_param, post_vals}),
    Request = #{
        qs => Qs,
        headers => #{<<"content-type">> => <<"application/json">>},
        body => <<"{\"name\":\"j27-synthetic-channel\"}">>
    },
    try
        {ok, Req, _} = channel_handler:init(
            Request,
            #{action => Action, current_uid => ?UID}
        ),
        ?assertEqual(200, maps:get(status, Req)),
        maps:get(response, Req)
    after
        erase({elib_param, post_vals})
    end.

create_scope_validation_test_() ->
    %% ⚠️ TestFun 须单表达式直接断言：{Desc, fun} 列表会被 ?_test 吞掉
    %% 静默空转；哨兵经进程字典传递（generator 与执行异进程，Self 收不到）
    ?WITH_MECKS(
        [
            {workspace_logic, [
                {'ensure_can_create_resource', 2, fun(WsId, Uid) ->
                    put(t_cs_ensure_create, {WsId, Uid}),
                    case {WsId, Uid} of
                        {?WS_ID, 900002} -> {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                        {?WS_ID, 900003} -> {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}};
                        _ -> ok
                    end
                end}
            ]},
            {channel_logic_message, [
                {'create_channel', 4, fun(Uid, Name, Opts, Max) ->
                    put(t_cs_legacy_create, {Uid, Name, Opts, Max}),
                    {ok, #{<<"id">> => ?CID, <<"name">> => Name}}
                end}
            ]}
        ],
        fun() -> create_scope_validation_body() end
    ).

create_scope_validation_body() ->
    begin
        %% guest cannot create workspace channel (403)
        ?assertMatch(
            {error, {403, _}},
            channel_logic:create_channel(
                900002, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
            )
        ),
        %% non workspace member cannot create workspace channel (403)
        ?assertMatch(
            {error, {403, _}},
            channel_logic:create_channel(
                900003, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
            )
        ),
        %% workspace scope without workspace_id is 400
        ?assertMatch(
            {error, {400, _}},
            channel_logic:create_channel(
                ?UID, <<"ws-ch">>, #{}, 20, {<<"workspace">>, 0}
            )
        ),
        %% invalid scope value is 400
        ?assertMatch(
            {error, {400, _}},
            channel_logic:create_channel(?UID, <<"ch">>, #{}, 20, {<<"org">>, ?WS_ID})
        ),
        %% owner/member creates workspace channel with scope fields
        ?assertMatch(
            {ok, _},
            channel_logic:create_channel(
                ?UID, <<"ws-ch">>, #{}, 20, {<<"workspace">>, ?WS_ID}
            )
        ),
        {?UID, <<"ws-ch">>, Opts, 20} = erase(t_cs_legacy_create),
        ?assertEqual(<<"workspace">>, maps:get(scope, Opts)),
        ?assertEqual(?WS_ID, maps:get(workspace_id, Opts)),

        %% personal default path is zero-change (no role check)
        erase(t_cs_ensure_create),
        ?assertMatch(
            {ok, _},
            channel_logic:create_channel(
                ?UID, <<"my-ch">>, #{}, 20, {<<"personal">>, 0}
            )
        ),
        ?assert(
            undefined =:= get(t_cs_ensure_create),
            "personal must not check workspace role"
        ),
        %% personal 路径不注入 scope 键（落 DB 默认值）
        {?UID, <<"my-ch">>, Opts2, 20} = erase(t_cs_legacy_create),
        ?assertEqual(false, maps:is_key(scope, Opts2)),
        ?assertEqual(false, maps:is_key(workspace_id, Opts2)),
        ok
    end.

update_rejects_scope_mutation_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                %% 不该走到真实 update_channel（meck 拦截防 DB 访问）
                {'update_channel', 3, fun(_, _, _) -> {error, must_not_reach} end}
            ]},
            {channel_logic_subscription, [
                {'update_channel', 3, fun(_, _, _) -> {error, must_not_reach} end}
            ]}
        ],
        fun() -> update_rejects_scope_mutation_body() end
    ).

update_rejects_scope_mutation_body() ->
    begin
        %% scope immutable on update (400)
        ?assertMatch(
            {error, {400, _}},
            channel_logic:update_channel_checked(?UID, <<"1">>, #{
                <<"scope">> => <<"workspace">>
            })
        ),
        %% workspace_id immutable on update (400)
        ?assertMatch(
            {error, {400, _}},
            channel_logic:update_channel_checked(
                ?UID, <<"1">>, #{<<"workspace_id">> => 123}
            )
        ),
        ok
    end.

list_workspace_channels_partitions_by_scope_test_() ->
    ?WITH_MECKS(
        [
            {channel_repo, [
                {'tablename', 0, fun() -> <<"public.channel">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [?WS_ID, Limit]) ->
                    put(t_cs_list_sql, {Sql, Limit}),
                    {ok, [#{<<"id">> => ?CID, <<"name">> => <<"Announcements">>}]}
                end}
            ]},
            {channel_logic_common, [
                {'channel_transfer', 1, fun(C) -> C#{<<"transferred">> => true} end}
            ]}
        ],
        fun() -> list_workspace_channels_partitions_by_scope_body() end
    ).

list_workspace_channels_partitions_by_scope_body() ->
    begin
        %% workspace channel list filters scope strictly
        ?assertMatch(
            {ok, [#{<<"transferred">> := true}]},
            channel_logic:list_workspace_channels(?WS_ID, 50)
        ),
        {Sql, 50} = erase(t_cs_list_sql),
        ?assert(binary:match(Sql, <<"scope = 'workspace'">>) =/= nomatch),
        ?assert(binary:match(Sql, <<"workspace_id = $1">>) =/= nomatch),
        ok
    end.

%% GZAPP-05：治理面归档可见性过滤 —— status 三值折叠 + 默认零行为变化。
%% 原实现把 `c.status = 1` 写死在 SQL 里，归档频道在任何入口都不可枚举，
%% 「归档后可恢复」在 UI 上不可达；本用例锁死折叠契约与默认分支。
list_workspace_channels_status_filter_test_() ->
    ?WITH_MECKS(
        [
            {channel_repo, [
                {'tablename', 0, fun() -> <<"public.channel">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [?WS_ID, _Limit]) ->
                    put(t_cs_status_sql, Sql),
                    {ok, []}
                end}
            ]},
            {channel_logic_common, [
                {'channel_transfer', 1, fun(C) -> C end}
            ]}
        ],
        fun() -> list_workspace_channels_status_filter_body() end
    ).

list_workspace_channels_status_filter_body() ->
    begin
        %% 默认（/2 入口）：仍是 status = 1，公开面行为不变
        {ok, []} = channel_logic:list_workspace_channels(?WS_ID, 50),
        DefaultSql = erase(t_cs_status_sql),
        ?assert(binary:match(DefaultSql, <<"c.status = 1 ">>) =/= nomatch),
        %% 显式 active 同默认
        {ok, []} = channel_logic:list_workspace_channels(?WS_ID, 50, <<"active">>),
        ?assertEqual(DefaultSql, erase(t_cs_status_sql)),
        %% archived：仅归档集合，出现在 SQL 且不残留 status = 1
        {ok, []} = channel_logic:list_workspace_channels(?WS_ID, 50, <<"archived">>),
        ArchivedSql = erase(t_cs_status_sql),
        ?assert(binary:match(ArchivedSql, <<"c.status = 0 ">>) =/= nomatch),
        ?assertEqual(nomatch, binary:match(ArchivedSql, <<"c.status = 1 ">>)),
        %% all：无状态子句（active + archived 全量）
        {ok, []} = channel_logic:list_workspace_channels(?WS_ID, 50, <<"all">>),
        AllSql = erase(t_cs_status_sql),
        ?assertEqual(nomatch, binary:match(AllSql, <<"c.status">>)),
        %% 未知值折叠为 active（不接受外部拼接语义）
        {ok, []} = channel_logic:list_workspace_channels(
            ?WS_ID, 50, <<"'; DROP TABLE channel; --">>
        ),
        ?assertEqual(DefaultSql, erase(t_cs_status_sql)),
        ok
    end.

%%%===================================================================
%%% J24.b/J24.c（channel-group-ux v2.2 §6.4/§9 BS 落点）：server 受理
%%% 7d 边界、删除/撤权不泄露、规范等价（trim/tags 顺序/重复）与不换键。
%%% 离线 mock 同 group_scope_tests 的 j24 段：elib_pg:query/2 回放合成
%%% receipt 行、channel_ds 只答 status/creator 判定；真 PG 向量由
%%% channel_creation_recovery_tests（TEST_WITH_DB）车道主控执行。
%%%===================================================================

-define(CH_REC_REQ, <<"cri_j24_channel_1">>).

j24_b_channel_server_clock_and_delete_exception_test_() ->
    ?WITH_MECKS(j24_channel_receipt_mocks(), fun j24_b_channel_server_clock_body/0).

j24_b_channel_server_clock_body() ->
    j24_ch_reset(),
    %% —— 未过窗（<7d）：完整结果保留为同 ID replayed；创建零二次发生 ——
    put(j24_receipt_row, j24_channel_row(j24_ch_digest(), #{<<"expired">> => false})),
    ?assertMatch(
        {ok, #{<<"id">> := ?CID}, replayed},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),
    %% 6d23:59:59.999→7d 边界钉在 SQL：CURRENT_TIMESTAMP - INTERVAL
    %% '7 days'，入参仅身份三元组——客户端时钟不入参（两个时钟独立）。
    Sql = erase(j24_find_sql),
    ?assert(binary:match(Sql, <<"CURRENT_TIMESTAMP - INTERVAL '7 days'">>) =/= nomatch),
    ?assertEqual(1, erase(j24_pg_calls)),
    ?assert(undefined =:= erase(j24_ch_created), "未过窗重放不得二次建频道"),
    ?assert(undefined =:= erase(j24_tx_sentinel), "重放路径不得开事务"),

    %% —— 只查（recover_only=true）7d 内有权 → 同结果；只读一次，零副作用 ——
    put(j24_receipt_row, j24_channel_row(j24_ch_digest(), #{<<"expired">> => false})),
    ?assertMatch(
        {ok, #{<<"id">> := ?CID}, replayed},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            true
        )
    ),
    ?assertEqual(1, erase(j24_pg_calls)),

    %% —— 7d 过窗：只给核对元信息 create_recovery_expired，重放/只查同型 ——
    put(j24_receipt_row, j24_channel_row(j24_ch_digest(), #{<<"expired">> => true})),
    ?assertEqual(
        {error, {create_recovery_expired, ?CID}},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),
    ?assertEqual(
        {error, {create_recovery_expired, ?CID}},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            true
        )
    ),
    ?assert(undefined =:= erase(j24_ch_created)),

    %% —— 删除例外优先（delete_channel 同事务置位；deleted 与 expired 同置
    %%    → unavailable 而非 expired）：不复活、不泄露、错误不改为首次创建 ——
    put(
        j24_receipt_row,
        j24_channel_row(j24_ch_digest(), #{
            <<"expired">> => true, <<"resource_deleted">> => true
        })
    ),
    ?assertEqual(
        {error, {create_resource_unavailable, ?CID}},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),
    ?assertEqual(
        {error, {create_resource_unavailable, ?CID}},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            true
        )
    ),

    %% —— 撤权（频道活、创建者已易主）：统一不可用，不泄露内容 ——
    put(j24_receipt_row, j24_channel_row(j24_ch_digest(), #{<<"expired">> => false})),
    put(j24_ch_creator, 900099),
    ?assertEqual(
        {error, {create_resource_unavailable, ?CID}},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),
    put(j24_ch_creator, ?UID),

    %% —— 软删/审核（status≠1）：同键不复活 ——
    put(j24_ch_status, 0),
    ?assertEqual(
        {error, {create_resource_unavailable, ?CID}},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),
    ok.

j24_c_channel_canonical_equivalence_and_no_key_swap_test_() ->
    ?WITH_MECKS(j24_channel_receipt_mocks(), fun j24_c_channel_canonical_body/0).

j24_c_channel_canonical_body() ->
    j24_ch_reset(),
    Row = j24_channel_row(j24_ch_digest(), #{<<"expired">> => false}),

    %% —— 名称仅首尾空白 → trim 规范等价重放（J24.c）——
    put(j24_receipt_row, Row),
    ?assertMatch(
        {ok, #{<<"id">> := ?CID}, replayed},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"  原名  "/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),

    %% —— tags 顺序不同/重复 → 规范等价重放（服务端去重升序）——
    put(
        j24_receipt_row,
        j24_channel_row(
            j24_ch_digest_with_tags([<<"a">>, <<"b">>]),
            #{<<"expired">> => false}
        )
    ),
    ?assertMatch(
        {ok, #{<<"id">> := ?CID}, replayed},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{tags => [<<"b">>, <<"a">>, <<"a">>]},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),

    %% —— 同键可见性变化（真实载荷变化）→ request_id_conflict，不剥键 ——
    put(j24_receipt_row, Row),
    ?assertEqual(
        {error, request_id_conflict},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{visibility => 1},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),

    %% —— scope/workspace 归属变化 → conflict（workspace 资格检查只属首
    %%    受理；重放先按规范摘要判冲突，不触达 workspace_logic）——
    ?assertEqual(
        {error, request_id_conflict},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"workspace">>, ?WS_ID},
            ?CH_REC_REQ,
            false
        )
    ),

    %% —— 只查未见受理 → create_not_accepted；只读 SELECT 一次、零副作用 ——
    %% （清零分段累计计数，本段只数本段调用——同 j24_b 段模式）
    _ = erase(j24_pg_calls),
    put(j24_receipt_row, undefined),
    ?assertEqual(
        {error, create_not_accepted},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            true
        )
    ),
    RoSql = erase(j24_find_sql),
    ?assert(binary:match(RoSql, <<"AS expired FROM">>) =/= nomatch),
    ?assertEqual(1, erase(j24_pg_calls)),
    ?assert(undefined =:= erase(j24_ch_created)),
    ?assert(undefined =:= erase(j24_tx_sentinel)),

    %% —— 迟到同原键请求最多受理一次：receipt 命中即重放，同键零二次创建 ——
    put(j24_receipt_row, Row),
    ?assertMatch(
        {ok, #{<<"id">> := ?CID}, replayed},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),
    ?assertMatch(
        {ok, #{<<"id">> := ?CID}, replayed},
        channel_logic:create_channel_with_receipt(
            ?UID,
            <<"原名"/utf8>>,
            #{},
            20,
            {<<"personal">>, 0},
            ?CH_REC_REQ,
            false
        )
    ),
    ?assert(undefined =:= erase(j24_ch_created), "迟到同键请求不得二次受理创建"),
    ok.

%%%-------------------------------------------------------------------
%%% J24 段夹具：合成 receipt 行经 elib_pg:query/2 回放给真实 find/3；
%%% canonical_channel_digest 为纯函数直接调用（默认选项与服务端
%%% receipt_channel_digest 的内部化默认一致：visibility/access_type/
%%% join_policy=0、description/avatar/custom_id 空串、tags 去重升序）。
%%%-------------------------------------------------------------------

j24_channel_receipt_mocks() ->
    [
        {elib_pg, [
            {'query', 2, fun
                (Sql, [?UID, <<"create_channel">>, _RequestId]) ->
                    put(j24_find_sql, Sql),
                    put(j24_pg_calls, j24_bump(j24_pg_calls)),
                    case get(j24_receipt_row) of
                        undefined -> {ok, []};
                        Row -> {ok, [Row]}
                    end;
                (Sql, _Params) ->
                    error({j24_unexpected_pg_query, Sql})
            end},
            {'with_tx', 1, fun(_Fun) ->
                put(j24_tx_sentinel, reached),
                error(j24_with_tx_must_not_run)
            end}
        ]},
        {channel_ds, [
            {'find_by_id', 2, fun
                (_Cid, <<"status,creator_uid">>) ->
                    #{
                        <<"status">> => j24_get(j24_ch_status, 1),
                        <<"creator_uid">> => j24_get(j24_ch_creator, ?UID)
                    };
                (_Cid, _Columns) ->
                    #{<<"id">> => ?CID, <<"name">> => <<"原名"/utf8>>}
            end},
            {'create_channel_tx', 4, fun(_Conn, _Uid, _Name, _Opts) ->
                put(j24_ch_created, true),
                {ok, ?CID}
            end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(C) -> C end}
        ]}
    ].

j24_ch_digest() ->
    j24_ch_digest_with_tags([]).

j24_ch_digest_with_tags(Tags) ->
    resource_create_receipt_repo:canonical_channel_digest(#{
        name => <<"原名"/utf8>>,
        scope => <<"personal">>,
        workspace_id => 0,
        visibility => 0,
        access_type => 0,
        join_policy => 0,
        description => <<>>,
        avatar => <<>>,
        custom_id => <<>>,
        tags => Tags
    }).

j24_channel_row(Digest, Overrides) ->
    maps:merge(
        #{
            <<"uid">> => ?UID,
            <<"operation">> => <<"create_channel">>,
            <<"request_id">> => ?CH_REC_REQ,
            <<"request_digest">> => Digest,
            <<"resource_id">> => ?CID,
            <<"server_accepted_at">> => {{2026, 10, 25}, {12, 0, 0}},
            <<"resource_deleted">> => false,
            <<"created_at">> => {{2026, 10, 25}, {12, 0, 0}},
            <<"expired">> => false
        },
        Overrides
    ).

j24_ch_reset() ->
    lists:foreach(
        fun(Key) -> erase(Key) end,
        [
            j24_receipt_row,
            j24_find_sql,
            j24_pg_calls,
            j24_tx_sentinel,
            j24_ch_created,
            j24_ch_status,
            j24_ch_creator
        ]
    ),
    ok.

j24_get(Key, Default) ->
    case get(Key) of
        undefined -> Default;
        V -> V
    end.

j24_bump(Key) ->
    case erase(Key) of
        undefined -> 1;
        N -> N + 1
    end.
