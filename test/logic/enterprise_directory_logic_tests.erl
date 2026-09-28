%% enterprise_directory_logic_tests
%% CP-CON-01 — INT-16/17（POST /api/internal/v1/identity-mappings/directory 与
%% POST /api/internal/v1/directory/users）CURSOR-V2 冻结合同（§10.1）的
%% 游标五类用例 + 旧 unsigned 形态拒绝 + 密钥门。
%%
%% 单元层真链路：logic ↔ 真 enterprise_cursor_v2（非 mock）；仓储与计量
%% meck（无 PG 依赖）。错误形态钉住 logic 的对外契约：
%%   malformed / tampered / foreign-family / foreign 绑定 / expired /
%%   旧 unsigned 一律 {error, {<<"invalid_request">>, _}}（handler 映射 400，
%%   不回显原因）；签名密钥缺失/非法 → {error, {<<"security_gate_closed">>, _}}
%%   （handler 映射 503，fail-closed，绝不降级签发/验签）。
%%
%% 五类游标用例（对应卡片验收）：
%%   ① valid          —— 真签游标续页：pivot 原样回传 repo（binary/integer 各族），
%%                        next_cursor 为双段 base64url 无 padding 形态
%%   ② tampered       —— 篡改 1 字节（payload 首字符与签名段首字符分别翻转）
%%   ③ malformed      —— 垃圾串（无点分隔/三点/空段/非 base64url 字符）
%%   ④ foreign-family —— 别的页族（directory_users ↔ identity_mappings 互喂）
%%   ⑤ expired        —— issued_at 早于 24h 窗口的真签游标
%%   ⑥ legacy-unsigned —— 旧 base64("<tag>:<keyset>") 无验签形态必须 400
%%   ⑦ 绑定           —— 真签名但 organization_id / application_id / filter
%%                        与当前请求不符（含 sort_tuple 形状非法）→ 400
%%   ⑧ 密钥门         —— signing key 缺失：续页 503；has_more 首页签发下一页 503
-module(enterprise_directory_logic_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEY_CFG, enterprise_internal_cursor_signing_key).
-define(KEY, <<"cpcon01_cursor_signing_key_0123456789abcdef">>).

-define(ORG, 992601).
-define(APP, 992602).
-define(CTX, #{organization_id => ?ORG, application_id => ?APP}).

-define(FAMILY_MAPPINGS, <<"identity_mappings">>).
-define(FAMILY_USERS, <<"directory_users">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    ok = application:set_env(imboy, ?KEY_CFG, ?KEY),
    meck:new(enterprise_directory_repo, [no_passthrough_cover]),
    meck:new(enterprise_application_usage_repo, [no_passthrough_cover]),
    meck:expect(
        enterprise_application_usage_repo, bump_tx, fun(_Conn, _O, _A, _M) -> ok end
    ),
    ok.

teardown(_) ->
    lists:foreach(
        fun(Mod) ->
            try
                meck:unload(Mod)
            catch
                _:_ -> ok
            end
        end,
        [enterprise_directory_repo, enterprise_application_usage_repo]
    ),
    application:unset_env(imboy, ?KEY_CFG),
    ok.

%%%===================================================================
%%% ① valid：真签游标续页（两族 pivot 形态）
%%%===================================================================

cursor_valid_signed_walk_mappings_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        LastAfter =
            fun() ->
                meck:expect(
                    enterprise_directory_repo,
                    page_mappings_tx,
                    fun
                        (_Conn, ?ORG, ?APP, undefined, 2) ->
                            {ok, [map_row(<<"ext-01">>, 101), map_row(<<"ext-02">>, 102)]};
                        (_Conn, ?ORG, ?APP, After, 2) ->
                            put(t_after, After),
                            {ok, [map_row(<<"ext-02">>, 102)]}
                    end
                )
            end,
        LastAfter(),
        {ok, P1} = enterprise_directory_logic:page_mappings_tx(
            test_conn, ?CTX, #{page_size => 1}
        ),
        ?assertEqual(1, length(maps:get(<<"items">>, P1))),
        ?assertEqual(true, maps:get(<<"has_more">>, P1)),
        C1 = maps:get(<<"next_cursor">>, P1),
        ?assert(is_binary(C1), "has_more 页必须签发下一页游标"),
        %% §10.1 冻结编码：恰好一个点分隔两段 base64url、无 padding
        ?assertEqual(2, length(binary:split(C1, <<".">>, [global]))),
        [EncP, EncM] = binary:split(C1, <<".">>, [global]),
        ?assertEqual(nomatch, binary:match(EncP, <<"=">>)),
        ?assertEqual(nomatch, binary:match(EncM, <<"=">>)),
        %% 续页：真签游标放行，repo 收到的 pivot 是上一页末行 keyset 值
        {ok, P2} = enterprise_directory_logic:page_mappings_tx(
            test_conn, ?CTX, #{cursor => C1, page_size => 1}
        ),
        ?assertEqual(false, maps:get(<<"has_more">>, P2)),
        ?assertEqual(null, maps:get(<<"next_cursor">>, P2)),
        ?assertEqual(<<"ext-01">>, get(t_after), "mappings 族 pivot 是 external_user_id")
    end}.

cursor_valid_signed_walk_users_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        meck:expect(
            enterprise_directory_repo,
            page_members_tx,
            fun
                (_Conn, ?ORG, ?APP, undefined, 2) ->
                    {ok, [member_row(101, <<"ext-01">>), member_row(102, null)]};
                (_Conn, ?ORG, ?APP, After, 2) ->
                    put(t_after, After),
                    {ok, [member_row(102, null)]}
            end
        ),
        {ok, P1} = enterprise_directory_logic:page_users_tx(
            test_conn, ?CTX, #{page_size => 1}
        ),
        C1 = maps:get(<<"next_cursor">>, P1),
        ?assert(is_binary(C1)),
        {ok, P2} = enterprise_directory_logic:page_users_tx(
            test_conn, ?CTX, #{cursor => C1, page_size => 1}
        ),
        ?assertEqual(null, maps:get(<<"next_cursor">>, P2)),
        ?assertEqual(101, get(t_after), "users 族 pivot 是 user_id（整数，非 base64 串）")
    end}.

%%%===================================================================
%%% ② tampered：篡改 1 字节
%%%===================================================================

cursor_tampered_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        C = signed_mappings_cursor(#{}, [<<"ext-01">>]),
        [EncP, EncM] = binary:split(C, <<".">>, [global]),
        %% 翻转首字符（首字符 6 bit 恒为有效载荷位；尾字符可能含无显著性填充位）
        TamperedPayload = <<(flip_first(EncP))/binary, ".", EncM/binary>>,
        TamperedMac = <<EncP/binary, ".", (flip_first(EncM))/binary>>,
        lists:foreach(
            fun(Cursor) ->
                ?assertMatch(
                    {error, {<<"invalid_request">>, _}},
                    enterprise_directory_logic:page_mappings_tx(
                        test_conn, ?CTX, #{cursor => Cursor}
                    ),
                    {tampered, Cursor}
                )
            end,
            [TamperedPayload, TamperedMac]
        ),
        ?assertEqual(0, repo_calls(), "验签失败的游标不得触达仓储")
    end}.

%%%===================================================================
%%% ③ malformed：垃圾串
%%%===================================================================

cursor_malformed_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        lists:foreach(
            fun(Bad) ->
                lists:foreach(
                    fun(Page) ->
                        ?assertMatch(
                            {error, {<<"invalid_request">>, _}},
                            Page(test_conn, ?CTX, #{cursor => Bad}),
                            {malformed, Bad}
                        )
                    end,
                    [
                        fun enterprise_directory_logic:page_mappings_tx/3,
                        fun enterprise_directory_logic:page_users_tx/3
                    ]
                )
            end,
            [
                <<"not-a-cursor">>,
                <<"a.b.c">>,
                <<".">>,
                <<"!!!.???">>,
                <<"====.====">>,
                base64:encode(<<"mappings">>),
                base64:encode(<<"users:">>)
            ]
        ),
        ?assertEqual(0, repo_calls(), "畸形游标不得触达仓储")
    end}.

%%%===================================================================
%%% ④ foreign-family：别的页族签发的 cursor
%%%===================================================================

cursor_foreign_family_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        %% users 族签发 → 喂 mappings
        UsersCursor = signed(?FAMILY_USERS, #{}, [101]),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_directory_logic:page_mappings_tx(
                test_conn, ?CTX, #{cursor => UsersCursor}
            )
        ),
        %% mappings 族签发 → 喂 users（反向）
        MappingsCursor = signed(?FAMILY_MAPPINGS, #{}, [<<"ext-01">>]),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_directory_logic:page_users_tx(
                test_conn, ?CTX, #{cursor => MappingsCursor}
            )
        ),
        %% 真实链路交叉：mappings 首页签出的游标喂 users 同样拒
        meck:expect(
            enterprise_directory_repo,
            page_mappings_tx,
            fun(_Conn, ?ORG, ?APP, undefined, 2) ->
                {ok, [map_row(<<"ext-01">>, 101), map_row(<<"ext-02">>, 102)]}
            end
        ),
        {ok, P1} = enterprise_directory_logic:page_mappings_tx(
            test_conn, ?CTX, #{page_size => 1}
        ),
        RealCursor = maps:get(<<"next_cursor">>, P1),
        ?assert(is_binary(RealCursor)),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_directory_logic:page_users_tx(
                test_conn, ?CTX, #{cursor => RealCursor}
            )
        ),
        ?assertEqual(1, repo_calls())
    end}.

%%%===================================================================
%%% ⑤ expired：issued_at 超出 24h 窗口
%%%===================================================================

cursor_expired_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        Now = os:system_time(second),
        Ttl = enterprise_cursor_v2:ttl_seconds(),
        Expired = signed(?FAMILY_MAPPINGS, #{}, [<<"ext-01">>], Now - Ttl - 1),
        lists:foreach(
            fun(Page) ->
                ?assertMatch(
                    {error, {<<"invalid_request">>, _}},
                    Page(test_conn, ?CTX, #{cursor => Expired})
                )
            end,
            [
                fun enterprise_directory_logic:page_mappings_tx/3,
                fun enterprise_directory_logic:page_users_tx/3
            ]
        ),
        %% 边界内 1 秒仍放行（对照）
        Fresh = signed(?FAMILY_MAPPINGS, #{}, [<<"ext-01">>], Now - Ttl + 1),
        meck:expect(
            enterprise_directory_repo,
            page_mappings_tx,
            fun(_Conn, ?ORG, ?APP, _After, _Limit) -> {ok, []} end
        ),
        ?assertMatch(
            {ok, _},
            enterprise_directory_logic:page_mappings_tx(
                test_conn, ?CTX, #{cursor => Fresh}
            )
        ),
        ?assertEqual(86400, Ttl, "§10.1 冻结 24h")
    end}.

%%%===================================================================
%%% ⑥ legacy-unsigned：旧无验签形态必须 400
%%%===================================================================

legacy_unsigned_cursor_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        lists:foreach(
            fun(Bad) ->
                ?assertMatch(
                    {error, {<<"invalid_request">>, _}},
                    enterprise_directory_logic:page_mappings_tx(
                        test_conn, ?CTX, #{cursor => Bad}
                    ),
                    {legacy_unsigned, Bad}
                )
            end,
            [
                base64:encode(<<"mappings:f02-ext-a1">>),
                base64:encode(<<"mappings:", (base64:encode(<<"f02-ext-a1">>))/binary>>)
            ]
        ),
        ?assertMatch(
            {error, {<<"invalid_request">>, _}},
            enterprise_directory_logic:page_users_tx(
                test_conn, ?CTX, #{cursor => base64:encode(<<"users:992011">>)}
            )
        ),
        ?assertEqual(0, repo_calls(), "旧 unsigned 游标不得触达仓储")
    end}.

%%%===================================================================
%%% ⑦ 绑定：真签名但 org/app/filter/sort_tuple 与当前请求不符
%%%===================================================================

cursor_binding_mismatches_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        Now = os:system_time(second),
        ForeignOrg =
            enterprise_cursor_v2:build_payload(
                ?FAMILY_MAPPINGS, ?ORG + 1, ?APP, #{}, [<<"ext-01">>], Now
            ),
        ForeignApp =
            enterprise_cursor_v2:build_payload(
                ?FAMILY_MAPPINGS, ?ORG, ?APP + 1, #{}, [<<"ext-01">>], Now
            ),
        ForeignFilter =
            enterprise_cursor_v2:build_payload(
                ?FAMILY_MAPPINGS,
                ?ORG,
                ?APP,
                #{<<"workspace_id">> => 992701},
                [<<"ext-01">>],
                Now
            ),
        BadSortTuple =
            enterprise_cursor_v2:build_payload(
                ?FAMILY_USERS, ?ORG, ?APP, #{}, [<<"not-an-integer">>], Now
            ),
        lists:foreach(
            fun(Payload) ->
                {ok, Cursor} = enterprise_cursor_v2:sign(Payload, ?KEY),
                lists:foreach(
                    fun(Page) ->
                        ?assertMatch(
                            {error, {<<"invalid_request">>, _}},
                            Page(test_conn, ?CTX, #{cursor => Cursor})
                        )
                    end,
                    [
                        fun enterprise_directory_logic:page_mappings_tx/3,
                        fun enterprise_directory_logic:page_users_tx/3
                    ]
                )
            end,
            [ForeignOrg, ForeignApp, ForeignFilter, BadSortTuple]
        ),
        ?assertEqual(0, repo_calls(), "绑定不符的游标不得触达仓储")
    end}.

%%%===================================================================
%%% ⑧ 密钥门：signing key 缺失 → 503 security_gate_closed（fail-closed）
%%%===================================================================

signing_key_missing_is_503_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        %% 续页（游标在场）：verify 前置门立即 503
        application:unset_env(imboy, ?KEY_CFG),
        ?assertMatch(
            {error, {<<"security_gate_closed">>, _}},
            enterprise_directory_logic:page_mappings_tx(
                test_conn, ?CTX, #{cursor => <<"any-cursor">>}
            )
        ),
        %% has_more 首页（无游标）：无法签发下一页 → 503，不得伪装成末页
        application:unset_env(imboy, ?KEY_CFG),
        meck:expect(
            enterprise_directory_repo,
            page_mappings_tx,
            fun(_Conn, ?ORG, ?APP, undefined, 2) ->
                {ok, [map_row(<<"ext-01">>, 101), map_row(<<"ext-02">>, 102)]}
            end
        ),
        ?assertMatch(
            {error, {<<"security_gate_closed">>, _}},
            enterprise_directory_logic:page_mappings_tx(
                test_conn, ?CTX, #{page_size => 1}
            )
        )
    end}.

%%%===================================================================
%%% Internal
%%%===================================================================

map_row(Ext, Uid) ->
    #{<<"external_user_id">> => Ext, <<"user_id">> => Uid}.

member_row(Uid, Ext) ->
    #{
        <<"user_id">> => Uid,
        <<"external_user_id">> => Ext,
        <<"member_role">> => <<"member">>,
        <<"member_status">> => <<"active">>
    }.

signed_mappings_cursor(Filter, SortTuple) ->
    signed(?FAMILY_MAPPINGS, Filter, SortTuple).

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

repo_calls() ->
    lists:sum(
        [
            meck:num_calls(enterprise_directory_repo, page_mappings_tx, 5),
            meck:num_calls(enterprise_directory_repo, page_members_tx, 5),
            meck:num_calls(enterprise_directory_repo, page_members_in_workspace_tx, 6)
        ]
    ).
