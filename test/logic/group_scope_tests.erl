-module(group_scope_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T5 — group scope 感知单元测试
%%% 覆盖：group_logic:add/5 scope 校验（Guest/非成员 403、非法 scope/ws_id 400、
%%% personal 分流零变化、workspace 创建走 create_scoped_group）、
%%% edit_checked scope 不可变、list_workspace_groups scope 严格分区。

-define(UID, 900001).
-define(WS_ID, 800001).
-define(GID, 777001).

j27_a_legacy_group_page_envelopes_test_() ->
    j27_group_fixture(fun() ->
        lists:foreach(
            fun(Attr) ->
                erase(j27_group_cursor_fetch),
                Qs = [{<<"attr">>, Attr}, {<<"page">>, <<"2">>}, {<<"size">>, <<"7">>}],
                Response = j27_group_request(Qs),
                ?assertEqual(0, maps:get(<<"code">>, Response)),
                ?assertEqual(
                    #{
                        <<"total">> => 61,
                        <<"page">> => 2,
                        <<"size">> => 7,
                        <<"list">> => [#{<<"id">> => ?GID}]
                    },
                    maps:get(<<"payload">>, Response)
                ),
                ?assertEqual({?UID, 2, 7, <<>>}, get(j27_group_legacy_fetch)),
                ?assertEqual(undefined, get(j27_group_cursor_fetch))
            end,
            [<<"owner">>, <<"join">>, <<"manager">>]
        )
    end).

j27_c_group_cursor_binding_and_progress_test_() ->
    j27_group_fixture(fun() ->
        lists:foreach(
            fun({Attr, Kind}) ->
                put(j27_group_more, true),
                Qs = [
                    {<<"pagination">>, <<"cursor">>},
                    {<<"attr">>, Attr},
                    {<<"limit">>, <<"2">>},
                    {<<"q">>, <<"needle">>}
                ],
                Page = maps:get(<<"payload">>, j27_group_request(Qs)),
                ?assertEqual(true, maps:get(<<"has_more">>, Page)),
                ?assertEqual(
                    [#{<<"id">> => 30}, #{<<"id">> => 20}],
                    maps:get(<<"list">>, Page)
                ),
                Token = maps:get(<<"next_cursor">>, Page),
                Ctx = #{scope => <<"personal">>, workspace_id => 0, q => <<"needle">>},
                ?assertEqual({ok, 20}, list_cursor:resolve(Token, ?UID, Kind, Ctx)),
                put(j27_group_more, false),
                _ = j27_group_request([{<<"cursor">>, Token} | Qs]),
                ?assertEqual({?UID, 20, 2, <<"needle">>}, get(j27_group_cursor_fetch)),
                lists:foreach(
                    fun(ForeignCtx) ->
                        {ok, Foreign} = list_cursor:issue(
                            ?UID,
                            Kind,
                            ForeignCtx#{upper => 20},
                            os:system_time(second)
                        ),
                        erase(j27_group_cursor_fetch),
                        erase(j27_group_legacy_fetch),
                        Error = j27_group_request([{<<"cursor">>, Foreign} | Qs]),
                        ?assertEqual(400, maps:get(<<"code">>, Error)),
                        ?assertEqual(undefined, get(j27_group_cursor_fetch)),
                        ?assertEqual(undefined, get(j27_group_legacy_fetch))
                    end,
                    [
                        Ctx#{scope => <<"workspace">>, workspace_id => ?WS_ID},
                        Ctx#{q => <<"different">>}
                    ]
                )
            end,
            [
                {<<"owner">>, <<"group_owned">>},
                {<<"join">>, <<"group_joined">>},
                {<<"manager">>, <<"group_managed">>}
            ]
        )
    end).

j27_c_group_terminal_cursor_is_json_null_test_() ->
    j27_group_fixture(fun() ->
        put(j27_group_more, false),
        lists:foreach(
            fun(Attr) ->
                Page = maps:get(
                    <<"payload">>,
                    j27_group_request([
                        {<<"pagination">>, <<"cursor">>}, {<<"attr">>, Attr}
                    ])
                ),
                ?assertEqual(false, maps:get(<<"has_more">>, Page)),
                ?assertEqual(null, maps:get(<<"next_cursor">>, Page))
            end,
            [<<"owner">>, <<"join">>, <<"manager">>]
        )
    end).

j27_group_fixture(Test) ->
    Mocks = [
        {cowboy_req, [
            {method, 1, fun(_) -> <<"GET">> end},
            {parse_qs, 1, fun(Req) -> maps:get(qs, Req, []) end},
            {reply, 4, fun(Status, _Headers, Body, Req) ->
                Req#{status => Status, response => jsone:decode(Body)}
            end}
        ]},
        {auth_ds, [{current_uid, 1, fun(#{current_uid := Uid}) -> Uid end}]},
        {config_ds, [
            {env, 3, fun(imboy, list_cursor_signing_key, _) ->
                <<"j27-synthetic-signing-key-32-bytes-only">>
            end}
        ]},
        {group_logic, [
            {group_transfer, 1, fun(Row) -> Row end},
            {page_by_owner, 4, fun j27_group_legacy_fetch/4},
            {page_joined, 4, fun j27_group_legacy_fetch/4},
            {page_managed, 4, fun j27_group_legacy_fetch/4},
            {page_by_owner_cursor, 4, fun j27_group_cursor_fetch/4},
            {page_joined_cursor, 4, fun j27_group_cursor_fetch/4},
            {page_managed_cursor, 4, fun j27_group_cursor_fetch/4}
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

j27_group_legacy_fetch(Uid, Page, Size, Q) ->
    put(j27_group_legacy_fetch, {Uid, Page, Size, Q}),
    {ok, #{total => 61, list => [#{<<"id">> => ?GID}]}}.

j27_group_cursor_fetch(Uid, Before, Limit, Q) ->
    put(j27_group_cursor_fetch, {Uid, Before, Limit, Q}),
    More = get(j27_group_more) =:= true,
    Rows =
        case More of
            true -> [#{<<"id">> => 30}, #{<<"id">> => 20}];
            false -> [#{<<"id">> => 10}]
        end,
    {ok, #{
        list => Rows,
        has_more => More,
        upper =>
            case More of
                true -> 20;
                false -> 0
            end
    }}.

j27_group_request(Qs) ->
    {ok, Req, _} = group_handler:init(
        #{qs => Qs},
        #{action => page, current_uid => ?UID}
    ),
    ?assertEqual(200, maps:get(status, Req)),
    maps:get(response, Req).

add_scope_validation_test_() ->
    %% ⚠️ TestFun 须单表达式直接断言：{Desc, fun} 列表会被 ?_test 吞掉
    %% 静默空转；哨兵经进程字典传递（generator 与执行异进程，Self 收不到）
    ?WITH_MECKS(
        [
            {workspace_logic, [
                {'ensure_can_create_resource', 2, fun(WsId, Uid) ->
                    put(t_gs_ensure_create, {WsId, Uid}),
                    case {WsId, Uid} of
                        {?WS_ID, 900002} -> {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                        {?WS_ID, 900003} -> {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}};
                        _ -> ok
                    end
                end}
            ]},
            {group_ds, [
                {'create_scoped_group', 7, fun(_Conn, Gid, Uid, _Now, _Type, Scope, WsId) ->
                    put(t_gs_scoped_created, {Gid, Uid, Scope, WsId}),
                    Gid
                end},
                {'find_by_creator_and_sum', 2, fun(_, _) -> 0 end},
                {'create_group', 6, fun(_Conn, _Gid, Uid, _Now, _Type, _Jl) ->
                    put(t_gs_legacy_created, Uid),
                    ?GID
                end}
            ]},
            {user_ds, [
                {'title', 1, fun(_) -> <<"t">> end}
            ]},
            {group_member_ds, [
                {'join_group', 5, fun(_Conn, _Mode, Uid, Gid, _) ->
                    put(t_gs_join, {Uid, Gid}),
                    {ok, Uid}
                end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(_, _, _) -> #{} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    %% Match production rollback handling for both business
                    %% aborts and runtime errors; no fake connection reaches PG.
                    %% 同生产事务包装一样处理业务中止和运行时异常。
                    try Fun(fake_conn) of
                        Result ->
                            put(t_gs_tx_outcome, committed),
                            Result
                    catch
                        throw:{abort_tx, Reason} ->
                            put(t_gs_tx_outcome, rolled_back),
                            {error, Reason};
                        Class:Reason ->
                            put(t_gs_tx_outcome, rolled_back),
                            {error, {db_exception, Class, Reason}}
                    end
                end},
                %% workspace_guard:ensure_writable_tx 依赖（原 mock 缺失会
                %% passthrough 打真库/假连接崩溃）：
                %% resolve_workspace({workspace, WsId}) → SELECT id FROM workspace
                {'one', 2, fun
                    (<<"SELECT id FROM workspace", _/binary>>, [?WS_ID]) ->
                        {ok, #{<<"id">> => ?WS_ID}};
                    (_, _) ->
                        {ok, #{}}
                end},
                %% FOR UPDATE status 行锁查询
                {'query', 3, fun(_Conn, <<"SELECT status FROM workspace", _/binary>>, [?WS_ID]) ->
                    {ok, [#{<<"status">> => <<"active">>}]}
                end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(group_info) -> ?GID end}
            ]}
        ],
        fun() -> add_scope_validation_body() end
    ).

add_scope_validation_body() ->
    begin
        %% guest cannot create workspace group (403)
        ?assertMatch(
            {error, {403, _}},
            group_logic:add(0, 900002, 2, [], {<<"workspace">>, ?WS_ID})
        ),
        %% non workspace member cannot create workspace group (403)
        ?assertMatch(
            {error, {403, _}},
            group_logic:add(0, 900003, 2, [], {<<"workspace">>, ?WS_ID})
        ),
        %% workspace scope without workspace_id is 400
        ?assertMatch(
            {error, {400, _}},
            group_logic:add(0, ?UID, 2, [], {<<"workspace">>, 0})
        ),
        %% invalid scope value is 400
        ?assertMatch(
            {error, {400, _}},
            group_logic:add(0, ?UID, 2, [], {<<"team">>, ?WS_ID})
        ),
        %% owner creates workspace group with scope fields
        ?assertMatch(
            {ok, ?GID},
            group_logic:add(0, ?UID, 2, [], {<<"workspace">>, ?WS_ID})
        ),
        ?assertEqual(
            {?GID, ?UID, <<"workspace">>, ?WS_ID},
            erase(t_gs_scoped_created),
            "scoped group not created"
        ),
        ?assertEqual(committed, erase(t_gs_tx_outcome)),
        %% An invalid returned result raises badmatch inside the transaction;
        %% the real wrapper catches runtime errors and rolls back as well.
        %% 非法返回值触发事务内 badmatch；真实包装同样捕获并回滚。
        meck(group_member_ds, [
            {'join_group', 5, fun(_, _, _, _, _) -> {error, simulated_member_failure} end}
        ]),
        ?assertMatch(
            {error, _},
            group_logic:add(0, ?UID, 2, [<<"900009">>], {<<"workspace">>, ?WS_ID})
        ),
        ?assertEqual(rolled_back, erase(t_gs_tx_outcome)),
        %% workspace group create blocked by membership subset rolls back
        meck(group_member_ds, [
            {'join_group', 5, fun(_, _, _, _, _) ->
                throw({abort_tx, workspace_membership_required})
            end}
        ]),
        ?assertMatch(
            {error, {409, _}},
            group_logic:add(0, ?UID, 2, [<<"900009">>], {<<"workspace">>, ?WS_ID})
        ),
        ?assertEqual(rolled_back, erase(t_gs_tx_outcome)),
        %% personal scope delegates to legacy add/4 (zero change)
        %% 段内哨兵先清零（先前段遗留 put 会使反向断言误报）
        erase(t_gs_ensure_create),
        erase(t_gs_scoped_created),
        ?assertMatch(
            {ok, ?GID},
            group_logic:add(0, ?UID, 2, [], {<<"personal">>, 0})
        ),
        ?assertEqual(?UID, erase(t_gs_legacy_created)),
        ?assert(undefined =:= get(t_gs_ensure_create), "personal must not check role"),
        ?assert(
            undefined =:= get(t_gs_scoped_created),
            "personal must not create scoped group"
        ),
        %% creation limit still enforced on workspace path
        ?assertMatch(
            {error, <<"每人最多创建100个群"/utf8>>},
            group_logic:add(101, ?UID, 2, [], {<<"workspace">>, ?WS_ID})
        ),
        ok
    end.

edit_checked_rejects_scope_mutation_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                %% edit/3 真实实现会查群成员；提交 scope 时必须早退不触达
                {'find_by_gid_and_uid', 3, fun(_, _, _) -> throw(must_not_reach) end}
            ]}
        ],
        fun() -> edit_checked_rejects_scope_mutation_body() end
    ).

edit_checked_rejects_scope_mutation_body() ->
    begin
        %% scope immutable on group edit (400)
        ?assertMatch(
            {error, {400, _}},
            group_logic:edit_checked(?UID, ?GID, #{<<"scope">> => <<"workspace">>})
        ),
        %% workspace_id immutable on group edit (400)
        ?assertMatch(
            {error, {400, _}},
            group_logic:edit_checked(?UID, ?GID, #{<<"workspace_id">> => 1})
        ),
        %% normal fields pass through
        meck(group_member_ds, [
            {'find_by_gid_and_uid', 3, fun(_, _, _) ->
                #{<<"id">> => 1, <<"role">> => 4}
            end}
        ]),
        meck(group_ds, [
            {'exists', 1, fun(_) -> true end},
            {'update_by_id', 2, fun(_, _) -> {ok, 1} end},
            {'member_uids', 1, fun(_) -> [] end}
        ]),
        meck(msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> ok end}
        ]),
        ?assertMatch(
            ok, group_logic:edit_checked(?UID, ?GID, #{<<"title">> => <<"new">>})
        ),
        %% 中段 re-mock 不在 WITH_MECKS 清理清单内，必须显式卸载，
        %% 否则泄漏到后续套件（实测污染 closure 套件的 group_ds 守卫）
        meck_helper:cleanup_mock(group_ds),
        meck_helper:cleanup_mock(msg_s2c_ds),
        meck_helper:cleanup_mock(group_member_ds),
        ok
    end.

list_workspace_groups_partitions_by_scope_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [?WS_ID, Limit]) ->
                    put(t_gs_list_sql, {Sql, Limit}),
                    {ok, [#{<<"id">> => ?GID, <<"title">> => <<"General">>}]}
                end}
            ]},
            {group_logic, []}
        ],
        fun() -> list_workspace_groups_partitions_by_scope_body() end
    ).

list_workspace_groups_partitions_by_scope_body() ->
    begin
        %% workspace group list filters scope strictly
        ?assertMatch(
            {ok, [#{<<"title">> := <<"General">>}]},
            group_logic:list_workspace_groups(?WS_ID, 50)
        ),
        {Sql, 50} = erase(t_gs_list_sql),
        ?assert(binary:match(Sql, <<"scope = 'workspace'">>) =/= nomatch),
        ?assert(binary:match(Sql, <<"workspace_id = $1">>) =/= nomatch),
        ok
    end.

%%%===================================================================
%%% J24.b/J24.c（channel-group-ux v2.2 §6.4/§9 BG 落点）：server 受理
%%% 6d23:59:59.999/7d 边界、删除/撤权不泄露、规范等价重放与不换键。
%%% 离线 mock：elib_pg:query/2 捕获 find SQL（钉服务端 7d 年龄边界）并
%%% 回放合成 receipt 行；判定面（replay_receipt/replay_decision）走真实
%%% 逻辑；真 PG 事务/迁移向量由 group_creation_recovery_pg_tests 车道
%%% 主控执行，本段不触库。
%%%===================================================================

-define(REC_REQ, <<"cri_j24_group_1">>).

j24_b_group_server_clock_and_delete_exception_test_() ->
    ?WITH_MECKS(j24_group_receipt_mocks(), fun j24_b_group_server_clock_body/0).

j24_b_group_server_clock_body() ->
    j24_reset(),
    %% J24.b 向量统一用「creator-only（无初始成员）」规范载荷行，摘要自洽。
    %% —— 未过窗（<7d）：完整结果保留为同 ID replayed；创建零二次发生 ——
    put(j24_receipt_row, j24_group_row(j24_group_digest([]), #{<<"expired">> => false})),
    ?assertMatch(
        {ok, ?GID, replayed},
        group_logic:add_with_receipt(
            0,
            ?UID,
            2,
            [],
            {<<"personal">>, 0},
            ?REC_REQ,
            <<"原名"/utf8>>,
            false
        )
    ),
    %% 6d23:59:59.999→7d 边界钉在 SQL：CURRENT_TIMESTAMP - INTERVAL
    %% '7 days'，入参仅身份三元组——客户端时钟不入参（两个时钟独立）。
    Sql = erase(j24_find_sql),
    ?assert(binary:match(Sql, <<"CURRENT_TIMESTAMP - INTERVAL '7 days'">>) =/= nomatch),
    ?assertEqual(1, erase(j24_pg_calls)),
    ?assert(undefined =:= erase(j24_created), "未过窗重放不得二次建群"),
    ?assert(undefined =:= erase(j24_tx_sentinel), "重放路径不得开事务"),

    %% —— 只查（recover_only=true）7d 内有权 → 同结果；只读一次，零副作用 ——
    put(j24_receipt_row, j24_group_row(j24_group_digest([]), #{<<"expired">> => false})),
    ?assertMatch(
        {ok, ?GID, replayed},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, true
        )
    ),
    ?assertEqual(1, erase(j24_pg_calls)),

    %% —— 7d 过窗：只给核对元信息 create_recovery_expired，重放/只查同型；
    %%    receipt 事实仍在、不产生第二资源 ——
    put(j24_receipt_row, j24_group_row(j24_group_digest([]), #{<<"expired">> => true})),
    ?assertEqual(
        {error, {create_recovery_expired, ?GID}},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, false
        )
    ),
    ?assertEqual(
        {error, {create_recovery_expired, ?GID}},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, true
        )
    ),
    ?assert(undefined =:= erase(j24_created)),

    %% —— 删除例外优先（deleted 与 expired 同置 → unavailable 而非 expired）：
    %%    不复活、不泄露名称/成员、错误不改为首次创建 ——
    put(
        j24_receipt_row,
        j24_group_row(j24_group_digest([]), #{
            <<"expired">> => true, <<"resource_deleted">> => true
        })
    ),
    ?assertEqual(
        {error, {create_resource_unavailable, ?GID}},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, false
        )
    ),
    ?assertEqual(
        {error, {create_resource_unavailable, ?GID}},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, true
        )
    ),

    %% —— 撤权（群活、调用者已非成员）：统一不可用，不泄露内容 ——
    put(j24_receipt_row, j24_group_row(j24_group_digest([]), #{<<"expired">> => false})),
    put(j24_is_member, false),
    ?assertEqual(
        {error, {create_resource_unavailable, ?GID}},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, false
        )
    ),
    put(j24_is_member, true),

    %% —— 解散（status≠1）：同键不复活 ——
    put(j24_group_alive, 0),
    ?assertEqual(
        {error, {create_resource_unavailable, ?GID}},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, false
        )
    ),
    ok.

j24_c_group_canonical_equivalence_and_no_key_swap_test_() ->
    ?WITH_MECKS(j24_group_receipt_mocks(), fun j24_c_group_canonical_body/0).

j24_c_group_canonical_body() ->
    j24_reset(),
    %% 规范载荷行：初始成员 {880014, 880015}（摘要按十进制升序规范形式）。
    Row = j24_group_row(j24_group_digest([880014, 880015]), #{<<"expired">> => false}),

    %% —— 成员顺序不同 → 规范等价重放（J24.c）——
    put(j24_receipt_row, Row),
    ?assertMatch(
        {ok, ?GID, replayed},
        group_logic:add_with_receipt(
            0,
            ?UID,
            2,
            [<<"880015">>, <<"880014">>],
            {<<"personal">>, 0},
            ?REC_REQ,
            <<"原名"/utf8>>,
            false
        )
    ),

    %% —— 重复成员 → 规范等价重放（十进制去重，不构成新意图）——
    ?assertMatch(
        {ok, ?GID, replayed},
        group_logic:add_with_receipt(
            0,
            ?UID,
            2,
            [<<"880014">>, <<"880015">>, <<"880014">>],
            {<<"personal">>, 0},
            ?REC_REQ,
            <<"原名"/utf8>>,
            false
        )
    ),

    %% —— 同键改名（真实载荷变化）→ request_id_conflict，不剥键不换键 ——
    ?assertEqual(
        {error, request_id_conflict},
        group_logic:add_with_receipt(
            0,
            ?UID,
            2,
            [<<"880014">>, <<"880015">>],
            {<<"personal">>, 0},
            ?REC_REQ,
            <<"改名"/utf8>>,
            false
        )
    ),

    %% —— scope/workspace 归属变化 → conflict（workspace 资格检查只属首
    %%    受理；重放先按规范摘要判冲突，不触达 workspace_logic）——
    ?assertEqual(
        {error, request_id_conflict},
        group_logic:add_with_receipt(
            0,
            ?UID,
            2,
            [<<"880014">>, <<"880015">>],
            {<<"workspace">>, ?WS_ID},
            ?REC_REQ,
            <<"原名"/utf8>>,
            false
        )
    ),

    %% —— 只查未见受理 → create_not_accepted；只读 SELECT 一次、零副作用
    %%    （不新增占位/资源/事务；不能证明在途请求不会迟到，§6.4）——
    %% （清零分段累计计数，本段只数本段调用——同 j24_b 段模式）
    _ = erase(j24_pg_calls),
    put(j24_receipt_row, undefined),
    ?assertEqual(
        {error, create_not_accepted},
        group_logic:add_with_receipt(
            0,
            ?UID,
            2,
            [<<"880014">>, <<"880015">>],
            {<<"personal">>, 0},
            ?REC_REQ,
            <<"原名"/utf8>>,
            true
        )
    ),
    RoSql = erase(j24_find_sql),
    ?assert(binary:match(RoSql, <<"AS expired FROM">>) =/= nomatch),
    ?assertEqual(1, erase(j24_pg_calls)),
    ?assert(undefined =:= erase(j24_created)),
    ?assert(undefined =:= erase(j24_tx_sentinel)),

    %% —— 迟到同原键请求最多受理一次：receipt 命中即重放，同键零二次创建
    %%    （本向量用 creator-only 行，摘要与调用载荷自洽）——
    put(j24_receipt_row, j24_group_row(j24_group_digest([]), #{<<"expired">> => false})),
    ?assertMatch(
        {ok, ?GID, replayed},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, false
        )
    ),
    ?assertMatch(
        {ok, ?GID, replayed},
        group_logic:add_with_receipt(
            0, ?UID, 2, [], {<<"personal">>, 0}, ?REC_REQ, <<"原名"/utf8>>, false
        )
    ),
    ?assert(undefined =:= erase(j24_created), "迟到同键请求不得二次受理创建"),
    ok.

%%%-------------------------------------------------------------------
%%% J24 段夹具：合成 receipt 行经 elib_pg:query/2 回放给真实 find/3；
%%% canonical_*_digest 为纯函数直接调用。哨兵置/清都在测试执行进程。
%%%-------------------------------------------------------------------

j24_group_receipt_mocks() ->
    [
        {elib_pg, [
            {'query', 2, fun
                (Sql, [?UID, <<"create_group">>, _RequestId]) ->
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
        {group_ds, [
            {'find_by_id', 2, fun(_Gid, <<"status">>) ->
                #{<<"status">> => j24_get(j24_group_alive, 1)}
            end},
            {'create_group', 6, fun(_Conn, _Gid, Uid, _Now, _Type, _Jl) ->
                put(j24_created, Uid),
                ?GID
            end}
        ]},
        {group_member_ds, [
            {'is_member', 2, fun(_Gid, _Uid) -> j24_get(j24_is_member, true) end}
        ]}
    ].

j24_group_digest(Members) ->
    resource_create_receipt_repo:canonical_group_digest(#{
        title => <<"原名"/utf8>>,
        scope => <<"personal">>,
        workspace_id => 0,
        members => Members
    }).

j24_group_row(Digest, Overrides) ->
    maps:merge(
        #{
            <<"uid">> => ?UID,
            <<"operation">> => <<"create_group">>,
            <<"request_id">> => ?REC_REQ,
            <<"request_digest">> => Digest,
            <<"resource_id">> => ?GID,
            <<"server_accepted_at">> => {{2026, 10, 25}, {12, 0, 0}},
            <<"resource_deleted">> => false,
            <<"created_at">> => {{2026, 10, 25}, {12, 0, 0}},
            <<"expired">> => false
        },
        Overrides
    ).

j24_reset() ->
    lists:foreach(
        fun(Key) -> erase(Key) end,
        [
            j24_receipt_row,
            j24_find_sql,
            j24_pg_calls,
            j24_created,
            j24_tx_sentinel,
            j24_is_member,
            j24_group_alive,
            j24_ch_created,
            j24_ch_status,
            j24_ch_creator
        ]
    ),
    ok.

j24_bump(Key) ->
    case erase(Key) of
        undefined -> 1;
        N -> N + 1
    end.

j24_get(Key, Default) ->
    case get(Key) of
        undefined -> Default;
        V -> V
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec meck(atom(), list()) -> ok.
meck(Module, Expectations) ->
    {ok, _} = meck_helper:setup_mock(Module, Expectations),
    ok.
