%%% @doc Human Organization Directory 真库套件（app→pg 全链，mock 游标注入）。
%%%
%%% 覆盖（§14.2 行为合同）：
%%%   * 授权门矩阵：org 缺 404 / archived 403 / 非成员 403 / removed 403 /
%%%     suspended 403 / active 放行
%%%   * departments：只回直接 active 子部门；archived/跨 Org 排除；
%%%     member_count 只计 active 成员（removed/suspended 排除）
%%%   * members：本级 active；根成员=未挂任何 active 部门者；
%%%     display_name 回落 account；department_ids 批量补齐
%%%   * /me：active 部门；只挂 archived 部门 → 空数组
%%%   * search：部门名/昵称/账号三列命中；email/mobile 不可搜（U-02）；
%%%     LIKE %/_ 字面量转义；跨 Org 隔离
%%%   * keyset 分页走页：无重复无遗漏，跨 kind 边界正确
%%%   * N+1 真实计数：全部端点 SQL 次数与页大小无关（meck passthrough 计数）
%%%   * search SQL 白名单：语句不触碰 email/mobile 列
-module(organization_directory_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, organization_directory_fixture).
-define(APP, organization_directory_app).
-define(MOCK_CURSOR, organization_directory_cursor_mock).
-define(KEY, <<"pg-test-signing-key-0123456789abcdef">>).

directory_pg_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            ok = application:set_env(imboy, enterprise_internal_cursor_signing_key, ?KEY),
            {ok, Conn};
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({ok, Conn}) ->
    application:unset_env(imboy, enterprise_internal_cursor_signing_key),
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 30, fun d01_gate_matrix/0},
        {timeout, 30, fun d02_departments_root/0},
        {timeout, 30, fun d03_departments_children_and_counts/0},
        {timeout, 30, fun d04_members_of_department/0},
        {timeout, 30, fun d05_members_root_default/0},
        {timeout, 30, fun d06_my_departments/0},
        {timeout, 30, fun d07_search_hits/0},
        {timeout, 30, fun d08_search_u02_no_email_mobile/0},
        {timeout, 30, fun d09_search_like_escaping/0},
        {timeout, 30, fun d10_keyset_walk/0},
        {timeout, 30, fun d11_n_plus_1_real_counts/0},
        {timeout, 30, fun d12_search_sql_whitelist/0}
    ];
cases(Other) ->
    erlang:error({orgdir_db_unavailable, Other}).

%% app 调用捷径：恒注入 mock 游标模块。
deps(OrgId, Uid, Params) ->
    ?APP:list_departments(OrgId, Uid, Params#{cursor_mod => ?MOCK_CURSOR}).

members(OrgId, Uid, Params) ->
    ?APP:list_members(OrgId, Uid, Params#{cursor_mod => ?MOCK_CURSOR}).

me(OrgId, Uid) ->
    ?APP:my_departments(OrgId, Uid).

search(OrgId, Uid, Params) ->
    ?APP:search(OrgId, Uid, Params#{cursor_mod => ?MOCK_CURSOR}).

%% ===================================================================
%% D01 授权门矩阵
%% ===================================================================

d01_gate_matrix() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        %% org 不存在 → 404（TSID 同毫秒近似 +1 递增，须避开本 scope
        %% 已生成的相邻 id，取全新随机 TSID）。
        ?assertEqual(
            {error, <<"resource_not_found">>},
            deps(?FIX:id(), maps:get(owner_user_id, Scope), #{})
        ),
        %% archived org → 403 organization_disabled（owner 本人亦拒）。
        ?assertEqual(
            {error, <<"organization_disabled">>},
            deps(maps:get(archived_org_id, Scope), maps:get(owner_user_id, Scope), #{})
        ),
        %% 跨 Org 非成员 → 403。
        ?assertEqual(
            {error, <<"insufficient_scope">>},
            deps(maps:get(other_org_id, Scope), maps:get(owner_user_id, Scope), #{})
        ),
        %% removed / suspended → 403。
        ?assertEqual(
            {error, <<"insufficient_scope">>},
            deps(Org, maps:get(m_removed, Scope), #{})
        ),
        ?assertEqual(
            {error, <<"insufficient_scope">>},
            deps(Org, maps:get(m_suspended, Scope), #{})
        ),
        %% active 成员放行。
        ?assertMatch(
            {ok, _},
            deps(Org, maps:get(m_root, Scope), #{})
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D02 departments：根级只回 active 直接子部门
%% ===================================================================

d02_departments_root() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        {ok, #{list := List}} = deps(Org, maps:get(owner_user_id, Scope), #{}),
        Ids = [maps:get(id, D) || D <- List],
        Expected = lists:sort([
            maps:get(root_a, Scope),
            maps:get(search_dept, Scope),
            maps:get(root_c, Scope),
            maps:get(root_d, Scope)
        ]),
        ?assertEqual(Expected, lists:sort(Ids)),
        %% id ASC 全序；archived / 跨 Org 部门不出现。
        ?assertEqual(Ids, lists:sort(Ids)),
        ?assertNot(lists:member(maps:get(root_b, Scope), Ids)),
        ?assertNot(lists:member(maps:get(other_org_dept, Scope), Ids)),
        ?assertNot(lists:member(maps:get(child_a1, Scope), Ids)),
        %% 投影白名单。
        ?assertMatch(
            [#{id := _, name := _, parent_id := _, member_count := _} | _],
            List
        ),
        ?assertEqual(null, maps:get(parent_id, hd(List)))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D03 departments：指定 parent + member_count 口径（active 成员）
%% ===================================================================

d03_departments_children_and_counts() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        {ok, #{list := List}} = deps(Org, maps:get(owner_user_id, Scope), #{
            parent_id => maps:get(root_a, Scope)
        }),
        ById = maps:from_list([{maps:get(id, D), D} || D <- List]),
        ?assertEqual(2, map_size(ById)),
        %% child_a1：m_child + m_dual（均 active）→ 2。
        C1 = maps:get(maps:get(child_a1, Scope), ById),
        ?assertEqual(2, maps:get(member_count, C1)),
        %% child_a2：m_dual(active) + m_removed + m_suspended（排除）→ 1。
        C2 = maps:get(maps:get(child_a2, Scope), ById),
        ?assertEqual(1, maps:get(member_count, C2)),
        %% 孙级/兄弟级不出现；父级自身不出现。
        ?assertNot(maps:is_key(maps:get(root_a, Scope), ById))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D04 members：部门本级 active，user_id ASC
%% ===================================================================

d04_members_of_department() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        {ok, #{list := List}} = members(Org, maps:get(owner_user_id, Scope), #{
            department_id => maps:get(child_a1, Scope)
        }),
        Uids = [maps:get(user_id, M) || M <- List],
        ?assertEqual(
            lists:sort([maps:get(m_child, Scope), maps:get(m_dual, Scope)]),
            lists:sort(Uids)
        ),
        ?assertEqual(Uids, lists:sort(Uids)),
        %% department_ids：m_dual 同时挂 child_a1+child_a2（active 全集）。
        DualRow = lists:keyfind(
            maps:get(m_dual, Scope),
            1,
            [{maps:get(user_id, M), M} || M <- List]
        ),
        ?assertMatch(
            {_, #{department_ids := [_, _]}},
            DualRow
        ),
        ?assertEqual(
            lists:sort([maps:get(child_a1, Scope), maps:get(child_a2, Scope)]),
            lists:sort(maps:get(department_ids, element(2, DualRow)))
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D05 members：缺省=根成员（未挂任何 active 部门）
%% ===================================================================

d05_members_root_default() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        {ok, #{list := List}} = members(Org, maps:get(owner_user_id, Scope), #{}),
        Uids = [maps:get(user_id, M) || M <- List],
        %% owner / m_root / m_search（无部门）+ m_arch_only（只挂 archived）。
        Expected =
            lists:sort([
                maps:get(owner_user_id, Scope),
                maps:get(m_root, Scope),
                maps:get(m_arch_only, Scope),
                maps:get(m_search, Scope)
            ]),
        ?assertEqual(Expected, lists:sort(Uids)),
        %% 挂了 active 部门的 m_child / m_dual 不在根；离场成员不在。
        lists:foreach(
            fun(K) ->
                ?assertNot(lists:member(maps:get(K, Scope), Uids), {unexpected_root, K})
            end,
            [m_child, m_dual, m_removed, m_suspended, other_member]
        ),
        %% 空昵称 → display_name 回落 account。
        Arch = lists:keyfind(
            maps:get(m_arch_only, Scope),
            1,
            [{maps:get(user_id, M), M} || M <- List]
        ),
        ?assertEqual(
            <<"orgdir-acc-", (integer_to_binary(maps:get(m_arch_only, Scope)))/binary>>,
            maps:get(display_name, element(2, Arch))
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D06 /me：active 部门；archived-only → 空数组
%% ===================================================================

d06_my_departments() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        %% m_dual：child_a1 + child_a2。
        {ok, #{organization_id := Org, list := DualList}} =
            me(Org, maps:get(m_dual, Scope)),
        ?assertEqual(
            lists:sort([maps:get(child_a1, Scope), maps:get(child_a2, Scope)]),
            lists:sort([maps:get(id, D) || D <- DualList])
        ),
        %% m_arch_only：只挂 archived root_b → 空数组不报错。
        {ok, #{list := []}} = me(Org, maps:get(m_arch_only, Scope)),
        %% m_root：无任何部门 → 空数组。
        {ok, #{list := []}} = me(Org, maps:get(m_root, Scope)),
        %% 非成员 → 403。
        ?assertEqual(
            {error, <<"insufficient_scope">>},
            me(maps:get(other_org_id, Scope), maps:get(m_root, Scope))
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D07 search：部门名/昵称/账号命中 + 跨 Org 隔离
%% ===================================================================

d07_search_hits() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Tok = maps:get(token, Scope),
        {ok, #{list := List}} = search(Org, maps:get(owner_user_id, Scope), #{q => Tok}),
        DeptIds = [maps:get(id, I) || I <- List, maps:get(type, I) =:= department],
        MemberUids = [maps:get(user_id, I) || I <- List, maps:get(type, I) =:= member],
        %% 部门命中：名字含 token 的 active 部门（root_a/search_dept/
        %% child_a1/child_a2——子部门同样可被检索）；archived/跨 Org 排除。
        ?assertEqual(
            lists:sort([
                maps:get(root_a, Scope),
                maps:get(search_dept, Scope),
                maps:get(child_a1, Scope),
                maps:get(child_a2, Scope)
            ]),
            lists:sort(DeptIds)
        ),
        ?assertNot(lists:member(maps:get(root_b, Scope), DeptIds)),
        ?assertNot(lists:member(maps:get(other_org_dept, Scope), DeptIds)),
        %% 成员命中：m_search(nickname) + m_dual(account)。
        ?assertEqual(
            lists:sort([maps:get(m_search, Scope), maps:get(m_dual, Scope)]),
            lists:sort(MemberUids)
        ),
        %% 跨 Org 同名昵称成员不泄漏。
        ?assertNot(lists:member(maps:get(other_member, Scope), MemberUids)),
        %% email/mobile 含 token 的 m_root 不命中（U-02，见 D08 细项）。
        ?assertNot(lists:member(maps:get(m_root, Scope), MemberUids)),
        %% 排序：departments 在前（kind ASC），组内 id ASC。
        Types = [maps:get(type, I) || I <- List],
        {Front, Back} = lists:split(length(DeptIds), Types),
        ?assertEqual([department || _ <- Front], Front),
        ?assert(lists:all(fun(T) -> T =:= member end, Back))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D08 U-02：email/mobile 不可搜
%% ===================================================================

d08_search_u02_no_email_mobile() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Tok = maps:get(token, Scope),
        %% m_root 的 email = <token>@mail.invalid、mobile = +86<token>。
        {ok, #{list := List}} = search(Org, maps:get(owner_user_id, Scope), #{q => Tok}),
        Uids = [maps:get(user_id, I) || I <- List, maps:get(type, I) =:= member],
        ?assertNot(lists:member(maps:get(m_root, Scope), Uids)),
        %% 反证夹具本身有效：m_root 行确实带 token 邮箱/手机。
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<"SELECT count(*) FROM \"user\" WHERE id=$1 AND email=$2">>,
                [maps:get(m_root, Scope), <<Tok/binary, "@mail.invalid">>]
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D09 LIKE 转义：% 与 _ 仅字面量
%% ===================================================================

d09_search_like_escaping() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Tok = maps:get(token, Scope),
        %% 未转义的 %组 会命中「后端组-token」；转义后按字面量匹配 → 空。
        {ok, #{list := []}} = search(Org, maps:get(owner_user_id, Scope), #{q => <<"%组"/utf8>>}),
        %% 未转义的 <token>_ 会命中任何 <token>+单字符（如 研发-token 的下一字符
        %% 不存在，但 nickname 张三-token 末尾无字符——此处用 <token>- 前缀的反例：
        %% q=<token>_ 若通配生效将命中「张三-token」类行；字面量语义 → 空）。
        {ok, #{list := []}} = search(Org, maps:get(owner_user_id, Scope), #{
            q => <<Tok/binary, "_">>
        }),
        %% 反证：不带通配字符的 token 子串可命中（夹具有效）。
        {ok, #{list := [_ | _]}} = search(Org, maps:get(owner_user_id, Scope), #{q => Tok})
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% D10 keyset 分页走页：departments / members / search 无重复无遗漏
%% ===================================================================

d10_keyset_walk() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Owner = maps:get(owner_user_id, Scope),

        %% departments：根 4 个 active 子部门，limit 2 → 恰 2 页。
        DeptPages = walk(fun(C) -> deps(Org, Owner, #{limit => 2, cursor => C}) end),
        ?assertEqual(2, length(DeptPages)),
        DeptAll = lists:append(DeptPages),
        ?assertEqual(4, length(DeptAll)),
        ?assertEqual(4, length(lists:usort([maps:get(id, D) || D <- DeptAll]))),
        DeptIds = [maps:get(id, D) || D <- DeptAll],
        ?assertEqual(DeptIds, lists:sort(DeptIds)),

        %% members：根成员 4 人，limit 2 → 2 页。
        MemPages = walk(fun(C) -> members(Org, Owner, #{limit => 2, cursor => C}) end),
        ?assertEqual(2, length(MemPages)),
        MemAll = lists:append(MemPages),
        ?assertEqual(4, length(MemAll)),
        ?assertEqual(4, length(lists:usort([maps:get(user_id, M) || M <- MemAll]))),
        MemUids = [maps:get(user_id, M) || M <- MemAll],
        ?assertEqual(MemUids, lists:sort(MemUids)),

        %% search：4 部门 + 2 成员 = 6 命中，limit 3 → 2 页；
        %% 第 2 页先排完剩余部门再接成员（页内跨 kind 边界）。
        Tok = maps:get(token, Scope),
        SearchPages = walk(fun(C) ->
            search(Org, Owner, #{q => Tok, limit => 3, cursor => C})
        end),
        ?assertEqual(2, length(SearchPages)),
        SearchAll = lists:append(SearchPages),
        ?assertEqual(6, length(SearchAll)),
        Types = [maps:get(type, I) || I <- SearchAll],
        ?assertEqual(
            [department, department, department, department, member, member],
            Types
        )
    after
        ?FIX:cleanup(Scope)
    end.

walk(Fetch) ->
    walk(Fetch, undefined, []).

walk(Fetch, Cursor, Acc) ->
    {ok, #{list := List, cursor := Next, has_more := More}} = Fetch(Cursor),
    case More of
        true when is_binary(Next) -> walk(Fetch, Next, Acc ++ [List]);
        _ -> Acc ++ [List]
    end.

%% ===================================================================
%% D11 N+1 真实计数：SQL 次数与页大小无关
%% ===================================================================

%% meck passthrough 计数：elib_pg:query/2 + elib_pg:one/3 是 directory pg 的
%% 全部 SQL 入口（gate=one/3；列表/批量=query/2）。计数在测试进程 pdict，
%% 后台进程不串扰。
d11_n_plus_1_real_counts() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Tok = maps:get(token, Scope),

        ok = meck:new(elib_pg, [no_passthrough_cover, passthrough]),
        meck:expect(elib_pg, query, fun(Sql, Params) ->
            bump(query),
            meck:passthrough([Sql, Params])
        end),
        meck:expect(elib_pg, one, fun(Sql, Params, Default) ->
            bump(one),
            meck:passthrough([Sql, Params, Default])
        end),

        %% departments：gate(1) + page(1) = 2，与 limit 无关。
        ?assertMatch(
            {{ok, _}, 2},
            with_count(fun() ->
                deps(Org, Owner, #{limit => 2})
            end)
        ),
        ?assertMatch(
            {{ok, _}, 2},
            with_count(fun() ->
                deps(Org, Owner, #{limit => 4})
            end)
        ),

        %% members：gate(1) + page(1) + batch(1) = 3，与页大小无关。
        ?assertMatch(
            {{ok, _}, 3},
            with_count(fun() ->
                members(Org, Owner, #{limit => 2})
            end)
        ),
        ?assertMatch(
            {{ok, _}, 3},
            with_count(fun() ->
                members(Org, Owner, #{limit => 4})
            end)
        ),
        %% 指定部门同样 3。
        ?assertMatch(
            {{ok, _}, 3},
            with_count(fun() ->
                members(Org, Owner, #{department_id => maps:get(child_a1, Scope), limit => 2})
            end)
        ),

        %% /me：gate(1) + mine(1) = 2。
        ?assertMatch(
            {{ok, _}, 2},
            with_count(fun() ->
                me(Org, maps:get(m_dual, Scope))
            end)
        ),

        %% search：gate(1) + search(1) = 2；页内无成员命中时空批短路，
        %% batch 不发 SQL（limit 2/4 的页全是部门命中）。
        ?assertMatch(
            {{ok, _}, 2},
            with_count(fun() ->
                search(Org, Owner, #{q => Tok, limit => 2})
            end)
        ),
        ?assertMatch(
            {{ok, _}, 2},
            with_count(fun() ->
                search(Org, Owner, #{q => Tok, limit => 4})
            end)
        ),
        %% limit 5：页含 1 名成员 → gate + search + batch = 3。
        ?assertMatch(
            {{ok, _}, 3},
            with_count(fun() ->
                search(Org, Owner, #{q => Tok, limit => 5})
            end)
        ),

        ok = meck:unload(elib_pg)
    after
        try
            meck:unload(elib_pg)
        catch
            _:_ -> ok
        end,
        ?FIX:cleanup(Scope)
    end.

bump(K) ->
    Prev =
        case get(K) of
            undefined -> 0;
            N when is_integer(N) -> N
        end,
    put(K, Prev + 1),
    ok.

with_count(Fun) ->
    erase(query),
    erase(one),
    Result = Fun(),
    Cnt = fun(K) ->
        case get(K) of
            N when is_integer(N) -> N;
            _ -> 0
        end
    end,
    {Result, Cnt(query) + Cnt(one)}.

%% ===================================================================
%% D12 search SQL 白名单：不触碰 email/mobile 列（语句级 U-02 断言）
%% ===================================================================

d12_search_sql_whitelist() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Tok = maps:get(token, Scope),

        ok = meck:new(elib_pg, [no_passthrough_cover, passthrough]),
        meck:expect(elib_pg, query, fun(Sql, Params) ->
            case is_binary(Sql) andalso binary:match(Sql, <<"ILIKE">>) =/= nomatch of
                true -> put(t_search_sql, Sql);
                false -> ok
            end,
            meck:passthrough([Sql, Params])
        end),
        {ok, _} = search(Org, Owner, #{q => Tok}),
        ok = meck:unload(elib_pg),

        Sql = get(t_search_sql),
        ?assert(is_binary(Sql), search_sql_not_captured),
        %% 命中面只允许三列：department.name / user.nickname / user.account。
        ?assert(binary:match(Sql, <<"d.name ILIKE">>) =/= nomatch),
        ?assert(binary:match(Sql, <<"u.nickname ILIKE">>) =/= nomatch),
        ?assert(binary:match(Sql, <<"u.account ILIKE">>) =/= nomatch),
        %% 禁列：email / mobile / 职位类列名一律不得出现。
        ?assertEqual(nomatch, binary:match(Sql, <<"email">>)),
        ?assertEqual(nomatch, binary:match(Sql, <<"mobile">>)),
        ?assertEqual(nomatch, binary:match(Sql, <<"title">>)),
        ?assertEqual(nomatch, binary:match(Sql, <<"position">>))
    after
        try
            meck:unload(elib_pg)
        catch
            _:_ -> ok
        end,
        ?FIX:cleanup(Scope)
    end.
