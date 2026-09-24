%%% @doc W2 C1~C4 列表契约套件（冻结合同 contracts-w2.md C1~C4；零 DB，fake store）。
%%%
%%% 覆盖：
%%%   * **C1 平台 session 列表**：`cs_session_app:list_sessions/2` 的投影白名单
%%%     **逐字键集相等**（禁止 visit_token_id / close_reason / 任何 digest 泄漏）、
%%%     status 白名单（queued|active|closed，非法 422 原子）、after_id 非 TSID
%%%     422 原子、limit 1..200 缺省 50 越界 422 原子、DESC 键集 + next_after_id；
%%%   * **C2 shop key 列表**：投影白名单 = {id, display_hint, status, created_at,
%%%     updated_at}，key_digest 绝不出现；
%%%   * **C3 visit token 列表**：投影白名单 = {id, contact_id, expires_at,
%%%     revoked_at, created_at}，token_digest 绝不出现；
%%%   * **C4 seats 分页**：`cs_seat_app:list_dispatchable_seats/2` 返回
%%%     `{seats, next_after_id}`，既有投影字段不变，limit 同口径；
%%%   * 端口契约四方一致性：`cs_store_port` callback ↔ `cs_ports:contracts()`
%%%     ↔ `cs_pg_store` 实现 ↔ fake store（cs_closure_tests 全量核对，这里断言
%%%     新条目存在）；
%%%   * 动作表/调用点表：p_session_list / shop_key_list / visit_token_list 登记，
%%%     facade 函数在 cs_facade_call:actions()；
%%%   * cs_http 新错误原子显式映射（{invalid_after_id,_}/{invalid_limit,_}/
%%%     {invalid_status,_} → 422，无兜底）。
-module(cs_list_contract_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FAKE, cs_fake_store).
-define(ORG, 820000000000001).
-define(WS, 820000000000002).
-define(ID1, 820000000000011).
-define(ID2, 820000000000012).
-define(ID3, 820000000000013).
-define(CONTACT, 820000000000003).
-define(CONV, 820000000000004).

%% C1 冻结投影白名单（contracts-w2.md C1，逐字）。
-define(C1_SESSION_KEYS, [
    id,
    organization_id,
    workspace_id,
    contact_id,
    business_identity_id,
    status,
    rating,
    queued_at,
    claimed_at,
    closed_at,
    version
]).

%% C2 冻结投影白名单（contracts-w2.md C2，逐字）。
-define(C2_SHOP_KEY_KEYS, [id, display_hint, status, created_at, updated_at]).

%% C3 冻结投影白名单（contracts-w2.md C3，逐字）。
-define(C3_VISIT_TOKEN_KEYS, [id, contact_id, expires_at, revoked_at, created_at]).

%% application 用例统一注入 fake store（纯套件零 DB）；参数键 `store` 是
%% cs_app_support:port/2 的注入面。
p(Opts) -> maps:merge(#{store => ?FAKE, workspace_id => ?WS}, Opts).

list_contract_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_S) -> cases() end}.

setup() ->
    ok = ?FAKE:init(),
    ok.

cleanup(_) ->
    ?FAKE:destroy(),
    ok.

cases() ->
    [
        {timeout, 30, fun c1_projection_is_exact_whitelist/0},
        {timeout, 30, fun c1_status_whitelist_rejects_unknown/0},
        {timeout, 30, fun c1_invalid_after_id_is_422_atom/0},
        {timeout, 30, fun c1_limit_bounds_default_50/0},
        {timeout, 30, fun c1_desc_keyset_and_next_after_id/0},
        {timeout, 30, fun c1_empty_list_has_null_cursor/0},
        {timeout, 30, fun c2_projection_never_leaks_digest/0},
        {timeout, 30, fun c2_pagination_cursor/0},
        {timeout, 30, fun c3_projection_never_leaks_digest/0},
        {timeout, 30, fun c3_pagination_cursor/0},
        {timeout, 30, fun c4_seats_page_shape_and_limits/0},
        {timeout, 30, fun c5_platform_seats_page_cross_org/0},
        {timeout, 30, fun port_contract_has_new_callbacks/0},
        {timeout, 30, fun actions_and_facade_call_are_registered/0},
        {timeout, 30, fun http_error_atoms_are_explicit_422/0}
    ].

%% ===================================================================
%% C1：投影白名单逐字（防 close_reason / visit_token_id / digest 泄漏）
%% ===================================================================

c1_projection_is_exact_whitelist() ->
    %% store 行故意携带敏感键（close_reason / visit_token_id / conversation_id /
    %% rating_at / updated_at）——投影必须把它们全部裁掉。
    SessionId = ?ID1,
    ok = ?FAKE:put_session_for_list(#{
        id => SessionId,
        organization_id => ?ORG,
        workspace_id => ?WS,
        contact_id => ?CONTACT,
        conversation_id => ?CONV,
        business_identity_id => undefined,
        visit_token_id => 999888777,
        status => queued,
        rating => undefined,
        rating_at => undefined,
        queued_at => 1700000000,
        claimed_at => undefined,
        closed_at => undefined,
        close_reason => <<"internal-reason-never-leak">>,
        version => 3,
        updated_at => 1700000001
    }),
    {ok, #{sessions := [Row], next_after_id := _}} =
        cs_session_app:list_sessions(?ORG, p(#{limit => 10})),
    ?assertEqual(
        lists:sort(?C1_SESSION_KEYS),
        lists:sort(maps:keys(Row))
    ),
    %% 红线逐键复核：下列键一个都不许出现。
    lists:foreach(
        fun(Key) -> ?assertNot(is_map_key(Key, Row)) end,
        [visit_token_id, close_reason, key_digest, token_digest, secret, cipher]
    ).

c1_status_whitelist_rejects_unknown() ->
    ?assertMatch(
        {error, {invalid_status, <<"drafting">>}},
        cs_session_app:list_sessions(?ORG, p(#{status => <<"drafting">>}))
    ),
    %% 合法白名单值照常透传（store 收到的是 binary 形态，镜像 SQL text 参数）。
    lists:foreach(
        fun(StatusBin) ->
            {ok, _} = cs_session_app:list_sessions(?ORG, p(#{status => StatusBin, limit => 5}))
        end,
        [<<"queued">>, <<"active">>, <<"closed">>]
    ).

c1_invalid_after_id_is_422_atom() ->
    ?assertMatch(
        {error, {invalid_after_id, <<"not-a-tsid">>}},
        cs_session_app:list_sessions(?ORG, #{
            workspace_id => ?WS, after_id => <<"not-a-tsid">>
        })
    ),
    ?assertMatch(
        {error, {invalid_after_id, _}},
        cs_session_app:list_sessions(?ORG, p(#{after_id => <<"-1">>}))
    ).

c1_limit_bounds_default_50() ->
    %% 缺省 50：store 收到 50（fake 记录最近一次调用参数）。
    {ok, _} = cs_session_app:list_sessions(?ORG, p(#{})),
    ?assertEqual(50, ?FAKE:last_page_limit()),
    %% 越界（0 / 201 / 非整数串）一律 422 原子。
    ?assertMatch(
        {error, {invalid_limit, <<"0">>}},
        cs_session_app:list_sessions(?ORG, p(#{limit => <<"0">>}))
    ),
    ?assertMatch(
        {error, {invalid_limit, <<"201">>}},
        cs_session_app:list_sessions(?ORG, p(#{limit => <<"201">>}))
    ),
    ?assertMatch(
        {error, {invalid_limit, <<"abc">>}},
        cs_session_app:list_sessions(?ORG, p(#{limit => <<"abc">>}))
    ),
    %% 边界内 1 与 200 合法。
    {ok, _} = cs_session_app:list_sessions(?ORG, p(#{limit => <<"1">>})),
    {ok, _} = cs_session_app:list_sessions(?ORG, p(#{limit => <<"200">>})).

c1_desc_keyset_and_next_after_id() ->
    Ids = [?ID1, ?ID2, ?ID3],
    lists:foreach(
        fun(Id) ->
            ok = ?FAKE:put_session_for_list(#{
                id => Id,
                organization_id => ?ORG,
                workspace_id => ?WS,
                contact_id => ?CONTACT,
                conversation_id => ?CONV,
                business_identity_id => undefined,
                visit_token_id => undefined,
                status => queued,
                rating => undefined,
                rating_at => undefined,
                queued_at => 1700000000,
                claimed_at => undefined,
                closed_at => undefined,
                close_reason => undefined,
                version => 1,
                updated_at => 1700000000
            })
        end,
        Ids
    ),
    %% DESC：首行 id 最大；满页（== limit）时 next_after_id = 本页最后一行（最小 id）。
    {ok, #{sessions := Rows1, next_after_id := Next1}} =
        cs_session_app:list_sessions(?ORG, p(#{limit => 2})),
    ?assertEqual([?ID3, ?ID2], [maps:get(id, R) || R <- Rows1]),
    ?assertEqual(?ID2, Next1),
    %% 翻页：after_id=上页尾 → 剩 1 行，不足一页 → next_after_id 结束（undefined）。
    {ok, #{sessions := Rows2, next_after_id := Next2}} =
        cs_session_app:list_sessions(?ORG, p(#{limit => 2, after_id => ?ID2})),
    ?assertEqual([?ID1], [maps:get(id, R) || R <- Rows2]),
    ?assertEqual(undefined, Next2).

c1_empty_list_has_null_cursor() ->
    EmptyOrg = 820000000000099,
    {ok, #{sessions := Rows, next_after_id := Next}} =
        cs_session_app:list_sessions(EmptyOrg, p(#{limit => 5})),
    ?assertEqual([], Rows),
    ?assertEqual(undefined, Next).

%% ===================================================================
%% C2：shop key 列表（digest 绝不外泄）
%% ===================================================================

c2_projection_never_leaks_digest() ->
    KeyId = ?ID1,
    ok = ?FAKE:put_shop_key_for_list(#{
        id => KeyId,
        organization_id => ?ORG,
        key_digest => <<"deadbeef-never-leak">>,
        display_hint => <<"shop-12">>,
        status => active,
        revoked_at => undefined,
        version => 1,
        created_at => 1700000000,
        updated_at => 1700000000
    }),
    {ok, #{shop_keys := [Row], next_after_id := _}} =
        cs_access_app:list_shop_keys(?ORG, p(#{limit => 10})),
    ?assertEqual(lists:sort(?C2_SHOP_KEY_KEYS), lists:sort(maps:keys(Row))),
    ?assertEqual(<<"shop-12">>, maps:get(display_hint, Row)),
    lists:foreach(
        fun(Key) -> ?assertNot(is_map_key(Key, Row)) end,
        [key_digest, secret, organization_id, version, revoked_at]
    ).

c2_pagination_cursor() ->
    lists:foreach(
        fun(Id) ->
            ok = ?FAKE:put_shop_key_for_list(#{
                id => Id,
                organization_id => ?ORG,
                key_digest => <<"d-", (integer_to_binary(Id))/binary>>,
                display_hint => undefined,
                status => active,
                revoked_at => undefined,
                version => 1,
                created_at => 1700000000,
                updated_at => 1700000000
            })
        end,
        [?ID1, ?ID2]
    ),
    {ok, #{shop_keys := Rows, next_after_id := Next}} =
        cs_access_app:list_shop_keys(?ORG, p(#{limit => <<"1">>})),
    ?assertEqual([?ID2], [maps:get(id, R) || R <- Rows]),
    ?assertEqual(?ID2, Next),
    {ok, #{shop_keys := Rows2, next_after_id := Next2}} =
        cs_access_app:list_shop_keys(?ORG, p(#{limit => 1, after_id => ?ID2})),
    ?assertEqual([?ID1], [maps:get(id, R) || R <- Rows2]),
    %% 空页终止协议：整除边界的最后一页仍满页（游标=本页尾），再翻一页
    %% 得空列表 + undefined 游标终止。
    ?assertEqual(?ID1, Next2),
    {ok, #{shop_keys := Rows3, next_after_id := Next3}} =
        cs_access_app:list_shop_keys(?ORG, p(#{limit => 1, after_id => ?ID1})),
    ?assertEqual([], Rows3),
    ?assertEqual(undefined, Next3),
    %% limit 校验同 C1 口径。
    ?assertMatch(
        {error, {invalid_limit, <<"9999">>}},
        cs_access_app:list_shop_keys(?ORG, p(#{limit => <<"9999">>}))
    ),
    ?assertMatch(
        {error, {invalid_after_id, <<"zz">>}},
        cs_access_app:list_shop_keys(?ORG, p(#{after_id => <<"zz">>}))
    ).

%% ===================================================================
%% C3：visit token 列表（token_digest 绝不外泄）
%% ===================================================================

c3_projection_never_leaks_digest() ->
    TokenId = ?ID1,
    ok = ?FAKE:put_visit_token_for_list(#{
        id => TokenId,
        organization_id => ?ORG,
        contact_id => ?CONTACT,
        token_digest => <<"cafebabe-never-leak">>,
        display_hint => undefined,
        expires_at => 1800000000,
        revoked_at => 1700000050,
        created_by_business_identity_id => 12345,
        version => 2,
        created_at => 1700000000,
        updated_at => 1700000050
    }),
    {ok, #{visit_tokens := [Row], next_after_id := _}} =
        cs_access_app:list_visit_tokens(?ORG, p(#{limit => 10})),
    ?assertEqual(lists:sort(?C3_VISIT_TOKEN_KEYS), lists:sort(maps:keys(Row))),
    lists:foreach(
        fun(Key) -> ?assertNot(is_map_key(Key, Row)) end,
        [
            token_digest,
            secret,
            organization_id,
            version,
            display_hint,
            created_by_business_identity_id,
            updated_at
        ]
    ).

c3_pagination_cursor() ->
    lists:foreach(
        fun(Id) ->
            ok = ?FAKE:put_visit_token_for_list(#{
                id => Id,
                organization_id => ?ORG,
                contact_id => ?CONTACT,
                token_digest => <<"t-", (integer_to_binary(Id))/binary>>,
                display_hint => undefined,
                expires_at => 1800000000,
                revoked_at => undefined,
                created_by_business_identity_id => undefined,
                version => 1,
                created_at => 1700000000,
                updated_at => 1700000000
            })
        end,
        [?ID1, ?ID2, ?ID3]
    ),
    {ok, #{visit_tokens := Rows, next_after_id := Next}} =
        cs_access_app:list_visit_tokens(?ORG, p(#{limit => 2})),
    ?assertEqual([?ID3, ?ID2], [maps:get(id, R) || R <- Rows]),
    ?assertEqual(?ID2, Next).

%% ===================================================================
%% C4：seats 分页（响应形状 {seats, next_after_id}，既有投影字段不变）
%% ===================================================================

c4_seats_page_shape_and_limits() ->
    lists:foreach(
        fun(IdentityId) ->
            ok = ?FAKE:put_seat_for_list(?ORG, #{
                organization_id => ?ORG,
                business_identity_id => IdentityId,
                function_key => customer_service,
                enabled => true,
                max_concurrent => 2,
                version => 1,
                created_at => 1700000000,
                updated_at => 1700000000
            })
        end,
        [?ID1, ?ID2, ?ID3]
    ),
    {ok, #{seats := Rows, next_after_id := Next}} =
        cs_seat_app:list_dispatchable_seats(?ORG, p(#{limit => <<"2">>})),
    %% 模板口径（ASC）：首行 business_identity_id 最小；满页 next = 本页尾。
    ?assertEqual([?ID1, ?ID2], [maps:get(business_identity_id, R) || R <- Rows]),
    ?assertEqual(?ID2, Next),
    %% 现有投影字段一个不少。
    lists:foreach(
        fun(Row) ->
            lists:foreach(
                fun(Key) -> ?assert(is_map_key(Key, Row)) end,
                [business_identity_id, function_key, enabled, max_concurrent, active_count]
            )
        end,
        Rows
    ),
    %% 翻页 + 边界。
    {ok, #{seats := Rows2, next_after_id := Next2}} =
        cs_seat_app:list_dispatchable_seats(?ORG, p(#{limit => 2, after_id => ?ID2})),
    ?assertEqual([?ID3], [maps:get(business_identity_id, R) || R <- Rows2]),
    ?assertEqual(undefined, Next2),
    ?assertMatch(
        {error, {invalid_limit, <<"0">>}},
        cs_seat_app:list_dispatchable_seats(?ORG, p(#{limit => <<"0">>}))
    ),
    ?assertMatch(
        {error, {invalid_after_id, <<"junk">>}},
        cs_seat_app:list_dispatchable_seats(?ORG, p(#{after_id => <<"junk">>}))
    ).

%% ===================================================================
%% C5：平台运营面坐席分页（跨企业可选 Org 过滤；含已停用；投影带
%% organization_name / display_name / workspace_id）
%% ===================================================================

c5_platform_seats_page_cross_org() ->
    Org2 = ?ORG + 1,
    Org2Identity = ?ID3 + 1,
    ok = ?FAKE:seed_org(?ORG, <<"org-a">>),
    ok = ?FAKE:seed_org(Org2, <<"org-b">>),
    ok = ?FAKE:seed_workspace(?ORG, ?WS),
    lists:foreach(
        fun({Org, IdentityId, Enabled}) ->
            ok = ?FAKE:put_seat_for_list(Org, #{
                organization_id => Org,
                business_identity_id => IdentityId,
                function_key => customer_service,
                enabled => Enabled,
                max_concurrent => 2,
                version => 1,
                created_at => 1700000000,
                updated_at => 1700000000
            }),
            ok = ?FAKE:put_identity_display(Org, IdentityId, <<"seat-name">>)
        end,
        [
            {?ORG, ?ID1, true},
            {?ORG, ?ID2, false},
            {Org2, Org2Identity, true}
        ]
    ),
    %% 全局视图（OrgFilter=0）：跨企业升序，且**已停用坐席也在列**。
    {ok, #{seats := AllRows, next_after_id := undefined}} =
        cs_seat_app:list_platform_seats(0, p(#{limit => 10})),
    %% C4 与本用例共享 fake store 种子：只断言本用例种子按序出现在全局列表。
    Seeded = [?ID1, ?ID2, Org2Identity],
    ?assertEqual(
        Seeded,
        [Id || Id <- [maps:get(business_identity_id, R) || R <- AllRows], lists:member(Id, Seeded)]
    ),
    {value, DisabledRow} =
        lists:search(
            fun(R) -> maps:get(business_identity_id, R) =:= ?ID2 end,
            AllRows
        ),
    ?assertNot(maps:get(enabled, DisabledRow)),
    %% 投影逐键（白名单 + 可读字段 + 默认 workspace）。
    lists:foreach(
        fun(Row) ->
            lists:foreach(
                fun(Key) -> ?assert(is_map_key(Key, Row)) end,
                [
                    organization_id,
                    organization_name,
                    display_name,
                    business_identity_id,
                    function_key,
                    enabled,
                    max_concurrent,
                    active_count,
                    workspace_id
                ]
            )
        end,
        AllRows
    ),
    ?assertEqual(<<"org-a">>, maps:get(organization_name, hd(AllRows))),
    ?assertEqual(?WS, maps:get(workspace_id, hd(AllRows))),
    %% org 过滤收窄：只见该企业的行。
    {ok, #{seats := OrgRows}} =
        cs_seat_app:list_platform_seats(Org2, p(#{limit => 10})),
    ?assertEqual([Org2Identity], [maps:get(business_identity_id, R) || R <- OrgRows]),
    %% 分页/游标/边界与 C4 同口径（在单企业过滤作用域内断言，避开 C4 残留种子的干扰）。
    {ok, #{seats := Page1, next_after_id := Next1}} =
        cs_seat_app:list_platform_seats(?ORG, p(#{limit => <<"2">>})),
    ?assertEqual([?ID1, ?ID2], [maps:get(business_identity_id, R) || R <- Page1]),
    ?assertEqual(?ID2, Next1),
    {ok, #{seats := Page2, next_after_id := undefined}} =
        cs_seat_app:list_platform_seats(?ORG, p(#{limit => 2, after_id => ?ID2})),
    ?assertEqual([?ID3], [maps:get(business_identity_id, R) || R <- Page2]),
    ?assertMatch(
        {error, {invalid_limit, <<"0">>}},
        cs_seat_app:list_platform_seats(0, p(#{limit => <<"0">>}))
    ),
    ?assertMatch(
        {error, {invalid_organization_id, -5}},
        cs_seat_app:list_platform_seats(-5, p(#{}))
    ).

%% ===================================================================
%% 端口契约 / 动作表 / 调用点表 / 错误映射
%% ===================================================================

port_contract_has_new_callbacks() ->
    StoreContracts = maps:get(cs_store_port, cs_ports:contracts()),
    lists:foreach(
        fun(Cb) -> ?assert(lists:member(Cb, StoreContracts)) end,
        [
            {list_sessions_page, 5},
            {list_shop_keys_page, 3},
            {list_visit_tokens_page, 3},
            {list_dispatchable_seats_page, 3},
            {list_all_seats_page, 3},
            %% CSB-02R：坐席工作台分页 + widget 装配缺省 Workspace 解析。
            {seat_session_page, 5},
            {default_workspace, 1}
        ]
    ),
    BehaviourCallbacks = cs_store_port:behaviour_info(callbacks),
    ?assert(lists:member({list_sessions_page, 5}, BehaviourCallbacks)),
    ?assert(lists:member({list_shop_keys_page, 3}, BehaviourCallbacks)),
    ?assert(lists:member({list_visit_tokens_page, 3}, BehaviourCallbacks)),
    ?assert(lists:member({list_dispatchable_seats_page, 3}, BehaviourCallbacks)),
    ?assert(lists:member({list_all_seats_page, 3}, BehaviourCallbacks)),
    ?assert(lists:member({seat_session_page, 5}, BehaviourCallbacks)),
    ?assert(lists:member({default_workspace, 1}, BehaviourCallbacks)),
    %% PG 装配与 fake 都实现新 callback（behaviour 编译期已查；这里对导出再取证）。
    PgExports = cs_pg_store:module_info(exports),
    lists:foreach(
        fun({F, A}) -> ?assert(lists:member({F, A}, PgExports)) end,
        [
            {seat_session_page, 5},
            {default_workspace, 1},
            {list_sessions_page, 5},
            {list_shop_keys_page, 3},
            {list_visit_tokens_page, 3},
            {list_dispatchable_seats_page, 3},
            {list_all_seats_page, 3}
        ]
    ).

actions_and_facade_call_are_registered() ->
    %% C1：平台动作 + platform_admin 只读权限。
    {ok, PEntry} = cs_actions:platform(p_session_list),
    PAuth = maps:get(auth, PEntry),
    ?assertEqual(platform_admin, maps:get(auth_context, PAuth)),
    ?assertEqual(<<"customer_service:read">>, maps:get(required_permission, PAuth)),
    %% C5：平台全局面坐席分页（org_source=param_optional，只读）。
    {ok, PSeatEntry} = cs_actions:platform(p_platform_seats),
    PSeatAuth = maps:get(auth, PSeatEntry),
    ?assertEqual(platform_admin, maps:get(auth_context, PSeatAuth)),
    ?assertEqual(<<"customer_service:read">>, maps:get(required_permission, PSeatAuth)),
    ?assertEqual(param_optional, cs_actions:org_source(PSeatEntry)),
    %% C2/C3：租户治理动作 + owner/admin。
    lists:foreach(
        fun(Action) ->
            {ok, Entry} = cs_actions:tenant(Action),
            Auth = maps:get(auth, Entry),
            ?assertEqual(enterprise_owner_admin, maps:get(auth_context, Auth)),
            ?assertEqual([<<"owner">>, <<"admin">>], maps:get(required_governance, Auth))
        end,
        [shop_key_list, visit_token_list]
    ),
    %% facade 用例全部在调用点表（动作表 ⊆ 调用点）。
    Declared = cs_facade_call:actions(),
    lists:foreach(
        fun(Fn) -> ?assert(lists:member(Fn, Declared)) end,
        [list_sessions, list_shop_keys, list_visit_tokens, list_platform_seats]
    ),
    %% facade 真实导出。
    Exports = customer_service_facade:module_info(exports),
    lists:foreach(
        fun(Fn) -> ?assert(lists:member({Fn, 2}, Exports)) end,
        [list_sessions, list_shop_keys, list_visit_tokens, list_platform_seats]
    ).

http_error_atoms_are_explicit_422() ->
    ?assertEqual(422, cs_http:status({invalid_after_id, <<"x">>})),
    ?assertEqual(422, cs_http:status({invalid_limit, <<"0">>})),
    ?assertEqual(422, cs_http:status({invalid_status, <<"drafting">>})),
    %% 显式映射的对外标签可区分（无兜底：不是 internal_error）。
    ?assertEqual(<<"invalid_after_id">>, cs_http:tag({invalid_after_id, <<"x">>})),
    ?assertEqual(<<"invalid_limit">>, cs_http:tag({invalid_limit, 0})),
    ?assertEqual(<<"invalid_status">>, cs_http:tag({invalid_status, <<"x">>})).
