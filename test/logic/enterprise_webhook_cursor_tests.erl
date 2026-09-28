%% enterprise_webhook_cursor_tests
%% CP-CON-02 — INT-23（GET /api/internal/v1/webhook/deliveries）CURSOR-V2
%% 冻结合同（§10.1；DEC-INT23-COMPAT）的游标五类用例 + 旧 offset 参数
%% versioned 400 + 稳定排序/边界翻页用例。
%%
%% 单元层真链路：logic ↔ 真 enterprise_cursor_v2（非 mock）；仓储 meck
%% （无 PG 依赖；真库归属隔离/边界翻页由 enterprise_webhook_governance_pg_tests
%% 的 read_surface_oracle 承担）。错误形态钉住 logic 的对外契约：
%%   malformed / tampered / foreign-family / foreign 绑定 / expired /
%%   page_size 越界一律 {error, {<<"invalid_request">>, _}}（handler 映射
%%   400，不回显原因）；签名密钥缺失/非法 → {error,
%%   {<<"security_gate_closed">>, _}}（handler 映射 503，fail-closed）。
%%   旧 offset 参数（page / size）→ {error, {<<"cursor_required_v1">>, _}}
%%   （DEC-INT23-COMPAT：versioned 400）。
%%
%% 用例族（对应卡片验收）：
%%   ① legacy-params   —— page / size 任一出现即 cursor_required_v1
%%   ② valid           —— 真签游标续页：pivot {CreatedAt, DeliveryId} 原样
%%                        回传 repo；next_cursor 双段 base64url 无 padding
%%   ③ tampered        —— 篡改 1 字节（payload 首字符与签名段首字符）
%%   ④ malformed       —— 垃圾串（无点分隔/三点/空段/非 base64url 字符）
%%   ⑤ foreign-family  —— 别的页族（identity_mappings 签的游标喂本族）
%%   ⑥ expired         —— issued_at 早于 24h 窗口的真签游标（边界内对照放行）
%%   ⑦ binding         —— 真签名但 org/app/filter(status) 与当前请求不符
%%                        （含 sort_tuple 形状非法）→ 拒
%%   ⑧ 稳定排序        —— 重复 created_at 时 pivot 携带 delivery_id tiebreak
%%   ⑨ 边界翻页        —— has_more 多取一行判定；末页 next_cursor=null
%%   ⑩ 密钥门          —— key 缺失：续页 503；has_more 首页签发下一页 503
-module(enterprise_webhook_cursor_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEY_CFG, enterprise_internal_cursor_signing_key).
-define(KEY, <<"cpcon02_cursor_signing_key_0123456789abcdef">>).

-define(ORG, 994301).
-define(APP, 994302).
-define(CTX, #{organization_id => ?ORG, application_id => ?APP}).

-define(FAMILY, <<"webhook_deliveries">>).

%% 排序键冻结：created_at DESC, delivery_id DESC（DEC-INT23-COMPAT）。

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    ok = application:set_env(imboy, ?KEY_CFG, ?KEY),
    meck:new(enterprise_webhook_repo, [no_passthrough_cover]),
    %% 统计读面恒成功（deliveries 附带 summary 与分页无关）
    meck:expect(
        enterprise_webhook_repo,
        delivery_stats_tx,
        fun(_Conn, _O, _A) ->
            {ok, #{
                <<"status_counts">> => #{<<"success">> => 1},
                <<"attempt_count">> => 1,
                <<"retry_count">> => 0,
                <<"attempt_rows">> => 1,
                <<"dead_letter_count">> => 0
            }}
        end
    ),
    ok.

teardown(_) ->
    try
        meck:unload(enterprise_webhook_repo)
    catch
        _:_ -> ok
    end,
    application:unset_env(imboy, ?KEY_CFG),
    ok.

%%%===================================================================
%%% ① legacy-params：page / size → versioned 400（cursor_required_v1）
%%%===================================================================

legacy_page_param_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        ?assertMatch(
            {error, {<<"cursor_required_v1">>, _}},
            enterprise_webhook_logic:deliveries_tx(test_conn, ?CTX, #{page => 2}, 20)
        ),
        ?assertEqual(0, repo_page_calls(), "旧 page 参数不得触达仓储")
    end}.

legacy_size_param_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        ?assertMatch(
            {error, {<<"cursor_required_v1">>, _}},
            enterprise_webhook_logic:deliveries_tx(test_conn, ?CTX, #{size => 20}, 20)
        ),
        ?assertMatch(
            {error, {<<"cursor_required_v1">>, _}},
            %% page 与 size 同现：同样拒绝（任一出现即拒）
            enterprise_webhook_logic:deliveries_tx(
                test_conn, ?CTX, #{page => 1, size => 20}, 20
            )
        ),
        ?assertEqual(0, repo_page_calls())
    end}.

%%%===================================================================
%%% ② valid：真签游标续页（pivot = {CreatedAt, DeliveryId}）
%%%===================================================================

cursor_valid_signed_walk_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun
                (_Conn, ?ORG, ?APP, undefined, undefined, 2) ->
                    {ok, [row(<<"d-01">>), row(<<"d-02">>)]};
                (_Conn, ?ORG, ?APP, undefined, Pivot, 2) ->
                    put(t_pivot, Pivot),
                    {ok, [row(<<"d-02">>)]}
            end
        ),
        {ok, P1} = enterprise_webhook_logic:deliveries_tx(
            test_conn, ?CTX, #{page_size => 1}, 20
        ),
        ?assertEqual(1, length(maps:get(<<"items">>, P1))),
        ?assertEqual(true, maps:get(<<"has_more">>, P1)),
        ?assertEqual(1, maps:get(<<"page_size">>, P1)),
        ?assert(is_map(maps:get(<<"summary">>, P1)), "摘要随页返回"),
        C1 = maps:get(<<"next_cursor">>, P1),
        ?assert(is_binary(C1), "has_more 页必须签发下一页游标"),
        %% §10.1 冻结编码：恰好一个点分隔两段 base64url、无 padding
        ?assertEqual(2, length(binary:split(C1, <<".">>, [global]))),
        [EncP, EncM] = binary:split(C1, <<".">>, [global]),
        ?assertEqual(nomatch, binary:match(EncP, <<"=">>)),
        ?assertEqual(nomatch, binary:match(EncM, <<"=">>)),
        %% 续页：真签游标放行，repo 收到的 pivot 是上一页末行 keyset 值
        {ok, P2} = enterprise_webhook_logic:deliveries_tx(
            test_conn, ?CTX, #{cursor => C1, page_size => 1}, 20
        ),
        ?assertEqual(false, maps:get(<<"has_more">>, P2)),
        ?assertEqual(null, maps:get(<<"next_cursor">>, P2)),
        ?assertEqual(
            {ts(1), <<"d-01">>},
            get(t_pivot),
            "pivot 是 {created_at, delivery_id}（末行 keyset 值）"
        )
    end}.

%%%===================================================================
%%% ③ tampered：篡改 1 字节
%%%===================================================================

cursor_tampered_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        C = signed(?FAMILY, #{}, [ts(1), <<"d-01">>]),
        [EncP, EncM] = binary:split(C, <<".">>, [global]),
        %% 翻转首字符（首字符 6 bit 恒为有效载荷位；尾字符可能含无显著性填充位）
        TamperedPayload = <<(flip_first(EncP))/binary, ".", EncM/binary>>,
        TamperedMac = <<EncP/binary, ".", (flip_first(EncM))/binary>>,
        lists:foreach(
            fun(Cursor) ->
                ?assertMatch(
                    {error, {<<"invalid_request">>, _}},
                    enterprise_webhook_logic:deliveries_tx(
                        test_conn, ?CTX, #{cursor => Cursor}, 20
                    ),
                    {tampered, Cursor}
                )
            end,
            [TamperedPayload, TamperedMac]
        ),
        ?assertEqual(0, repo_page_calls(), "验签失败的游标不得触达仓储")
    end}.

%%%===================================================================
%%% ④ malformed：垃圾串
%%%===================================================================

cursor_malformed_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        lists:foreach(
            fun(Bad) ->
                ?assertMatch(
                    {error, {<<"invalid_request">>, _}},
                    enterprise_webhook_logic:deliveries_tx(
                        test_conn, ?CTX, #{cursor => Bad}, 20
                    ),
                    {malformed, Bad}
                )
            end,
            [
                <<"not-a-cursor">>,
                <<"a.b.c">>,
                <<".">>,
                <<"!!!.???">>,
                <<"====.====">>,
                base64:encode(<<"1">>),
                base64:encode(<<"page:2">>)
            ]
        ),
        ?assertEqual(0, repo_page_calls(), "畸形游标不得触达仓储")
    end}.

%%%===================================================================
%%% ⑤ foreign-family：别的页族签发的 cursor
%%%===================================================================

cursor_foreign_family_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        %% identity_mappings 族（§10.2 白名单内别族）签发 → 喂 webhook_deliveries
        Foreign = signed(<<"identity_mappings">>, #{}, [<<"ext-01">>]),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_webhook_logic:deliveries_tx(test_conn, ?CTX, #{cursor => Foreign}, 20)
        ),
        %% 白名单外族（human_directory 也拒——本读面只认 webhook_deliveries）
        ForeignHuman = signed(<<"human_directory">>, #{}, [<<"ext-01">>]),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_webhook_logic:deliveries_tx(
                test_conn, ?CTX, #{cursor => ForeignHuman}, 20
            )
        ),
        %% 真实链路：本族首页签出的游标换 status 过滤复用 → 绑定拒
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun(_Conn, ?ORG, ?APP, undefined, undefined, 2) ->
                {ok, [row(<<"d-01">>), row(<<"d-02">>)]}
            end
        ),
        {ok, P1} = enterprise_webhook_logic:deliveries_tx(
            test_conn, ?CTX, #{page_size => 1}, 20
        ),
        RealCursor = maps:get(<<"next_cursor">>, P1),
        ?assert(is_binary(RealCursor)),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_webhook_logic:deliveries_tx(
                test_conn, ?CTX, #{cursor => RealCursor, status => <<"dead">>}, 20
            ),
            "本族真签游标换 status 过滤复用必须拒"
        ),
        ?assertEqual(1, repo_page_calls())
    end}.

%%%===================================================================
%%% ⑥ expired：issued_at 超出 24h 窗口（边界内 1 秒对照放行）
%%%===================================================================

cursor_expired_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        Now = os:system_time(second),
        Ttl = enterprise_cursor_v2:ttl_seconds(),
        Expired = signed(?FAMILY, #{}, [ts(1), <<"d-01">>], Now - Ttl - 1),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_webhook_logic:deliveries_tx(test_conn, ?CTX, #{cursor => Expired}, 20)
        ),
        %% 边界内 1 秒仍放行（对照）
        Fresh = signed(?FAMILY, #{}, [ts(1), <<"d-01">>], Now - Ttl + 1),
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun(_Conn, ?ORG, ?APP, _Status, _After, _Limit) -> {ok, []} end
        ),
        ?assertMatch(
            {ok, _},
            enterprise_webhook_logic:deliveries_tx(test_conn, ?CTX, #{cursor => Fresh}, 20)
        ),
        ?assertEqual(86400, Ttl, "§10.1 冻结 24h")
    end}.

%%%===================================================================
%%% ⑦ binding：真签名但 org/app/filter(status) 与当前请求不符
%%%===================================================================

cursor_binding_mismatches_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        Now = os:system_time(second),
        ForeignOrg =
            enterprise_cursor_v2:build_payload(
                ?FAMILY, ?ORG + 1, ?APP, #{}, [ts(1), <<"d-01">>], Now
            ),
        ForeignApp =
            enterprise_cursor_v2:build_payload(
                ?FAMILY, ?ORG, ?APP + 1, #{}, [ts(1), <<"d-01">>], Now
            ),
        ForeignFilter =
            enterprise_cursor_v2:build_payload(
                ?FAMILY,
                ?ORG,
                ?APP,
                #{<<"status">> => <<"dead">>},
                [ts(1), <<"d-01">>],
                Now
            ),
        BadSortTuple =
            enterprise_cursor_v2:build_payload(
                ?FAMILY, ?ORG, ?APP, #{}, [ts(1)], Now
            ),
        BadSortTuple2 =
            enterprise_cursor_v2:build_payload(
                ?FAMILY, ?ORG, ?APP, #{}, [123, <<"d-01">>], Now
            ),
        lists:foreach(
            fun(Payload) ->
                {ok, Cursor} = enterprise_cursor_v2:sign(Payload, ?KEY),
                ?assertMatch(
                    {error, {<<"invalid_request">>, _}},
                    enterprise_webhook_logic:deliveries_tx(
                        test_conn, ?CTX, #{cursor => Cursor}, 20
                    )
                )
            end,
            [ForeignOrg, ForeignApp, ForeignFilter, BadSortTuple, BadSortTuple2]
        ),
        %% 换 status 过滤的旧游标不得翻新过滤行集（对照：同 status 放行）
        WithDead = signed(?FAMILY, #{<<"status">> => <<"dead">>}, [ts(1), <<"d-01">>]),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_webhook_logic:deliveries_tx(
                test_conn, ?CTX, #{cursor => WithDead, status => <<"success">>}, 20
            )
        ),
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun(_Conn, ?ORG, ?APP, <<"dead">>, _After, _Limit) -> {ok, []} end
        ),
        ?assertMatch(
            {ok, _},
            enterprise_webhook_logic:deliveries_tx(
                test_conn, ?CTX, #{cursor => WithDead, status => <<"dead">>}, 20
            )
        ),
        ?assertEqual(1, repo_page_calls(), "绑定不符的游标不得触达仓储")
    end}.

%%%===================================================================
%%% ⑧ 稳定排序：重复 created_at 时 pivot 携带 delivery_id tiebreak
%%%===================================================================

stable_sort_tiebreak_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        %% 同一 created_at 的三行（ts(2)），delivery_id 单调递减序返回
        SameTs = ts(2),
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun
                (_Conn, ?ORG, ?APP, undefined, undefined, 3) ->
                    {ok, [
                        row(SameTs, <<"tie-c">>),
                        row(SameTs, <<"tie-b">>),
                        row(SameTs, <<"tie-a">>)
                    ]};
                (_Conn, ?ORG, ?APP, undefined, Pivot, 3) ->
                    put(t_pivot, Pivot),
                    {ok, []}
            end
        ),
        {ok, P1} = enterprise_webhook_logic:deliveries_tx(
            test_conn, ?CTX, #{page_size => 2}, 20
        ),
        ?assertEqual(2, length(maps:get(<<"items">>, P1))),
        ?assertEqual(true, maps:get(<<"has_more">>, P1)),
        C1 = maps:get(<<"next_cursor">>, P1),
        ?assert(is_binary(C1)),
        %% 用 next_cursor 续页：pivot 必须是 (same ts, tie-b)——
        %% delivery_id 兜底排序键原样进入 keyset，不允许只用 created_at
        {ok, _P2} = enterprise_webhook_logic:deliveries_tx(
            test_conn, ?CTX, #{cursor => C1, page_size => 2}, 20
        ),
        ?assertEqual({SameTs, <<"tie-b">>}, get(t_pivot))
    end}.

%%%===================================================================
%%% ⑨ 边界翻页：has_more 多取一行判定；末页 next_cursor=null
%%%===================================================================

boundary_paging_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        %% 行数恰等于 page_size（无额外行）→ has_more=false、next_cursor=null
        %% （page_size=2 → repo Limit=2+1；返回恰好 2 行）
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun(_Conn, ?ORG, ?APP, undefined, undefined, 3) ->
                {ok, [row(<<"d-01">>), row(<<"d-02">>)]}
            end
        ),
        {ok, Exact} = enterprise_webhook_logic:deliveries_tx(
            test_conn, ?CTX, #{page_size => 2}, 20
        ),
        ?assertEqual(false, maps:get(<<"has_more">>, Exact)),
        ?assertEqual(null, maps:get(<<"next_cursor">>, Exact)),
        %% 多取的额外行只用于 has_more 判定，不进入 items
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun(_Conn, ?ORG, ?APP, undefined, undefined, 3) ->
                {ok, [row(<<"d-01">>), row(<<"d-02">>), row(<<"d-03">>)]}
            end
        ),
        {ok, P1} = enterprise_webhook_logic:deliveries_tx(
            test_conn, ?CTX, #{page_size => 2}, 20
        ),
        ?assertEqual(
            [<<"d-01">>, <<"d-02">>],
            [maps:get(<<"delivery_id">>, R) || R <- maps:get(<<"items">>, P1)],
            "Limit+1 的额外行只判 has_more，不进入 items"
        )
    end}.

%%%===================================================================
%%% page_size 契约：默认/拒绝（不静默截断）
%%%===================================================================

page_size_contract_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun(_Conn, ?ORG, ?APP, undefined, _After, 21) -> {ok, []} end
        ),
        %% 缺省：DefaultSize=20 → repo Limit=20+1（has_more 判定行）
        {ok, Def} = enterprise_webhook_logic:deliveries_tx(test_conn, ?CTX, #{}, 20),
        ?assertEqual(20, maps:get(<<"page_size">>, Def)),
        lists:foreach(
            fun(Bad) ->
                ?assertMatch(
                    {error, {<<"invalid_request">>, _}},
                    enterprise_webhook_logic:deliveries_tx(
                        test_conn, ?CTX, #{page_size => Bad}, 20
                    ),
                    {page_size, Bad}
                )
            end,
            [0, -1, 51, 999, <<"20">>, 2.0]
        ),
        ?assertEqual(1, repo_page_calls(), "越界 page_size 不得触达仓储")
    end}.

%%%===================================================================
%%% ⑩ 密钥门：signing key 缺失 → 503 security_gate_closed（fail-closed）
%%%===================================================================

signing_key_missing_is_503_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        %% 续页（游标在场）：verify 前置门立即 503
        application:unset_env(imboy, ?KEY_CFG),
        ?assertMatch(
            {error, {<<"security_gate_closed">>, _}},
            enterprise_webhook_logic:deliveries_tx(
                test_conn, ?CTX, #{cursor => <<"any-cursor">>}, 20
            )
        ),
        %% has_more 首页（无游标）：无法签发下一页 → 503，不得伪装成末页
        application:unset_env(imboy, ?KEY_CFG),
        meck:expect(
            enterprise_webhook_repo,
            page_deliveries_tx,
            fun(_Conn, ?ORG, ?APP, undefined, undefined, 2) ->
                {ok, [row(<<"d-01">>), row(<<"d-02">>)]}
            end
        ),
        ?assertMatch(
            {error, {<<"security_gate_closed">>, _}},
            enterprise_webhook_logic:deliveries_tx(
                test_conn, ?CTX, #{page_size => 1}, 20
            )
        )
    end}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 行夹具：created_at 用定长可比 binary（repo 层是 RFC3339 binary，形状无差）
ts(N) ->
    <<"2026-09-27T00:00:0", (integer_to_binary(N))/binary, ".000000+00:00">>.

row(Id) ->
    row(ts(1), Id).

row(CreatedAt, Id) ->
    #{
        <<"delivery_id">> => Id,
        <<"event_type">> => <<"file.confirmed">>,
        <<"status">> => <<"success">>,
        <<"attempt_count">> => 1,
        <<"webhook_host">> => <<"hook.example.com">>,
        <<"ewh_replay_of">> => null,
        <<"ewh_ledger_version">> => 1,
        <<"ewh_claimed_at">> => null,
        <<"created_at">> => CreatedAt,
        <<"updated_at">> => CreatedAt
    }.

signed(Family, Filter, SortTuple) ->
    signed(Family, Filter, SortTuple, os:system_time(second)).

signed(Family, Filter, SortTuple, IssuedAt) ->
    Payload = enterprise_cursor_v2:build_payload(
        Family, ?ORG, ?APP, Filter, SortTuple, IssuedAt
    ),
    {ok, Cursor} = enterprise_cursor_v2:sign(Payload, ?KEY),
    Cursor.

%% 翻转首字符（与 enterprise_cursor_v2_tests 同款：避开尾字符低位填充比特
%% 可能无显著性的问题，首字符 6 bit 恒为有效载荷位）。
flip_first(Bin) ->
    Size = byte_size(Bin),
    Head = binary:part(Bin, 1, Size - 1),
    <<(flip(binary:first(Bin))):8, Head/binary>>.

flip(C) when C >= $a, C =< $y -> C + 1;
flip($z) -> $a;
flip(C) when C >= $A, C =< $Y -> C + 1;
flip($Z) -> $A;
flip(C) when C >= $0, C =< $8 -> C + 1;
flip($9) -> $0;
flip(C) -> C bxor 1.

repo_page_calls() ->
    meck:num_calls(enterprise_webhook_repo, page_deliveries_tx, 6).
