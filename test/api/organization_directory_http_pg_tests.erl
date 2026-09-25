%%% @doc INT-BE-04（INT-API-02C）：Human Organization Directory 真实
%%% HTTP/JWT/PG 闭环套件（organization_directory_http_support 同款 harness）。
%%%
%%% 覆盖验收：
%%%   * 四接口均经**真实 router + Human JWT + PG**（真 Cowboy 全量路由 +
%%%     同序中间件链 + token_ds 真签发 Bearer + disposable marker PG）；
%%%   * active member 的 departments（root/child 结构）/ members
%%%     （cursor/limit keyset 分页）/ me / search（q 命中）正确；
%%%   * suspended / removed / 非成员（outsider）fail-closed（403
%%%     insufficient_scope）；missing org 与跨 Org 同体不泄漏；
%%%   * 无凭证 401；U-02 负例：search 不命中 email/mobile；
%%%   * cursor/limit 翻页至尽：页数有限、无重复、总数与种子一致
%%%     （查询数不随成员数线性增长由 keyset limit+1 语义保证）。
%%%
%%% 只读无写入：四接口全 GET 只读路径（repo 只读 SQL，pg_tests 覆盖）；
%%% 套件自身对 marker 库只做种子 INSERT（fixture）与 SELECT 断言。
-module(organization_directory_http_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SUP, organization_directory_http_support).

orgdir_http_pg_test_() ->
    {setup, fun ?SUP:setup_all/0, fun ?SUP:teardown_all/1, fun cases/1}.

cases(State) ->
    [
        {timeout, 900, fun() -> four_endpoints_via_real_http(State) end},
        {timeout, 300, fun() -> suspended_and_removed_fail_closed(State) end},
        {timeout, 300, fun() -> outsider_and_missing_org_same_shape(State) end},
        {timeout, 300, fun() -> unauthenticated_is_401(State) end},
        {timeout, 300, fun() -> archived_org_is_organization_disabled(State) end},
        {timeout, 300, fun() -> search_hits_identity_not_contact(State) end},
        {timeout, 300, fun() -> members_cursor_limit_exhausts_finite(State) end},
        {timeout, 300, fun() -> departments_root_child_shape(State) end},
        {timeout, 300, fun() -> me_returns_active_departments(State) end}
    ].

%% ===================================================================
%% 四接口真 HTTP 正例（active member = owner）
%% ===================================================================

four_endpoints_via_real_http(State) ->
    S = scope(State),
    Org = maps:get(org_id, S),
    Owner = maps:get(owner_user_id, S),
    B = org_path(Org),
    %% departments：200 + payload.list（真 router + Human JWT + PG）。
    Deps = ?SUP:http(State, <<"GET">>, <<B/binary, "/departments">>, #{
        headers => ?SUP:bearer(Owner)
    }),
    ?assertEqual(200, maps:get(status, Deps), maps:get(body, Deps)),
    ?assertMatch(#{<<"payload">> := #{<<"list">> := _}}, maps:get(json, Deps)),
    %% members：200 + payload.list。
    Mems = ?SUP:http(State, <<"GET">>, <<B/binary, "/members">>, #{headers => ?SUP:bearer(Owner)}),
    ?assertEqual(200, maps:get(status, Mems), maps:get(body, Mems)),
    ?assertMatch(#{<<"payload">> := #{<<"list">> := _}}, maps:get(json, Mems)),
    %% me：200（owner 无部门 → 空数组不报错）。
    Me = ?SUP:http(State, <<"GET">>, <<B/binary, "/me">>, #{headers => ?SUP:bearer(Owner)}),
    ?assertEqual(200, maps:get(status, Me), maps:get(body, Me)),
    %% search：200 + q 命中（q 用 scope token；主 Org 内 nickname/account
    %% 命中 m_search/m_dual，跨 Org 的 other_member 不泄漏）。
    Q = maps:get(token, S),
    Search = ?SUP:http(State, <<"GET">>, <<B/binary, "/search?q=", Q/binary>>, #{
        headers => ?SUP:bearer(Owner)
    }),
    ?assertEqual(200, maps:get(status, Search), maps:get(body, Search)),
    %% search 是联合检索（kind 0=部门名 / 1=昵称+账号）：q=Tok 命中
    %% active 且名含 Tok 的部门（root_a/child_a1/child_a2/search_dept）
    %% 与成员（m_search/m_dual）共 6 项；archived root_b 不命中。
    Hits = payload_ids(Search),
    ?assertEqual(
        lists:sort([
            maps:get(root_a, S),
            maps:get(child_a1, S),
            maps:get(child_a2, S),
            maps:get(search_dept, S),
            maps:get(m_search, S),
            maps:get(m_dual, S)
        ]),
        lists:sort(Hits),
        {search_hits, maps:get(body, Search)}
    ).

%% ===================================================================
%% suspended / removed fail-closed（403 insufficient_scope）
%% ===================================================================

suspended_and_removed_fail_closed(State) ->
    S = scope(State),
    Org = maps:get(org_id, S),
    B = org_path(Org),
    lists:foreach(
        fun(UidKey) ->
            Uid = maps:get(UidKey, S),
            R = ?SUP:http(State, <<"GET">>, <<B/binary, "/members">>, #{
                headers => ?SUP:bearer(Uid)
            }),
            ?assertEqual(403, maps:get(status, R), {UidKey, maps:get(body, R)}),
            ?assertMatch(
                #{<<"error">> := #{<<"code">> := <<"insufficient_scope">>}},
                maps:get(json, R)
            )
        end,
        [m_suspended, m_removed]
    ).

%% ===================================================================
%% 跨 Org / missing org 同体不泄漏
%% ===================================================================

outsider_and_missing_org_same_shape(State) ->
    S = scope(State),
    OtherMember = maps:get(other_member, S),
    %% outsider（次 Org active member）访问主 Org → 403。
    R1 = ?SUP:http(State, <<"GET">>, <<(org_path(maps:get(org_id, S)))/binary, "/members">>, #{
        headers => ?SUP:bearer(OtherMember)
    }),
    ?assertEqual(403, maps:get(status, R1), maps:get(body, R1)),
    ?assertMatch(
        #{<<"error">> := #{<<"code">> := <<"insufficient_scope">>}}, maps:get(json, R1)
    ),
    %% missing org（不存在的 org id）→ 与 403 同型信封、状态 404：
    %% 不区分「org 不存在」与「成员不可见」以外的事实（同体不泄漏由
    %% stable 码表统一——resource_not_found）。
    Missing = missing_org_id(S),
    R2 = ?SUP:http(State, <<"GET">>, <<(org_path(Missing))/binary, "/members">>, #{
        headers => ?SUP:bearer(maps:get(owner_user_id, S))
    }),
    ?assertEqual(404, maps:get(status, R2), maps:get(body, R2)),
    ?assertMatch(#{<<"error">> := #{<<"code">> := <<"resource_not_found">>}}, maps:get(json, R2)).

%% ===================================================================
%% 无凭证 401（auth_middleware Human JWT 门）
%% ===================================================================

unauthenticated_is_401(State) ->
    S = scope(State),
    B = org_path(maps:get(org_id, S)),
    R = ?SUP:http(State, <<"GET">>, <<B/binary, "/departments">>, #{}),
    ?assertEqual(401, maps:get(status, R), maps:get(body, R)).

%% ===================================================================
%% archived org → 403 organization_disabled
%% ===================================================================

archived_org_is_organization_disabled(State) ->
    S = scope(State),
    Owner = maps:get(owner_user_id, S),
    R = ?SUP:http(
        State, <<"GET">>, <<(org_path(maps:get(archived_org_id, S)))/binary, "/members">>, #{
            headers => ?SUP:bearer(Owner)
        }
    ),
    ?assertEqual(403, maps:get(status, R), maps:get(body, R)),
    ?assertMatch(
        #{<<"error">> := #{<<"code">> := <<"organization_disabled">>}}, maps:get(json, R)
    ).

%% ===================================================================
%% U-02 负例：search 不命中 email/mobile（q=m_root 的 email token 段）
%% ===================================================================

search_hits_identity_not_contact(State) ->
    S = scope(State),
    Owner = maps:get(owner_user_id, S),
    Tok = maps:get(token, S),
    B = org_path(maps:get(org_id, S)),
    %% m_root 的 email/mobile 都含 Tok，但昵称/账号不含——q=Tok 的命中集
    %% 只有 m_search(nickname) 与 m_dual(account)，绝无 m_root。
    R = ?SUP:http(State, <<"GET">>, <<B/binary, "/search?q=", Tok/binary>>, #{
        headers => ?SUP:bearer(Owner)
    }),
    ?assertEqual(200, maps:get(status, R), maps:get(body, R)),
    Hits = payload_ids(R),
    ?assertNot(lists:member(maps:get(m_root, S), Hits), {email_mobile_leak, maps:get(body, R)}).

%% ===================================================================
%% members cursor/limit keyset 翻页至尽（页数有限、无重复、总数正确）
%% ===================================================================

members_cursor_limit_exhausts_finite(State) ->
    S = scope(State),
    Owner = maps:get(owner_user_id, S),
    B = org_path(maps:get(org_id, S)),
    %% 根成员期望集：owner + m_root + m_arch_only + m_search（child/removed/
    %% suspended 不在根级 active 集）。
    Expected = lists:sort([
        Owner,
        maps:get(m_root, S),
        maps:get(m_arch_only, S),
        maps:get(m_search, S)
    ]),
    Page1 = ?SUP:http(State, <<"GET">>, <<B/binary, "/members?limit=2">>, #{
        headers => ?SUP:bearer(Owner)
    }),
    ?assertEqual(200, maps:get(status, Page1), maps:get(body, Page1)),
    {Ids1, Cursor} = page_of(Page1),
    ?assertEqual(2, length(Ids1)),
    ?assertNotEqual(undefined, Cursor, "4 个根成员 limit=2 首页必有下一页游标"),
    CursorQ = iolist_to_binary(uri_string:quote(binary_to_list(Cursor))),
    Page2 = ?SUP:http(
        State, <<"GET">>, <<B/binary, "/members?limit=2&cursor=", CursorQ/binary>>, #{
            headers => ?SUP:bearer(Owner)
        }
    ),
    ?assertEqual(200, maps:get(status, Page2), maps:get(body, Page2)),
    {Ids2, Cursor2} = page_of(Page2),
    All = lists:sort(Ids1 ++ Ids2),
    ?assertEqual(Expected, All, {paged_ids, All}),
    %% 游标语义：末页无下一页游标（全部取尽）。
    ?assertEqual(undefined, Cursor2, {should_be_exhausted, maps:get(body, Page2)}).

%% ===================================================================
%% departments root/child 结构（root 级 active；child 按 parent_id）
%% ===================================================================

departments_root_child_shape(State) ->
    S = scope(State),
    Owner = maps:get(owner_user_id, S),
    B = org_path(maps:get(org_id, S)),
    %% 根级 active：root_a / search_dept / root_c / root_d（root_b archived 不显示）。
    Root = ?SUP:http(State, <<"GET">>, <<B/binary, "/departments">>, #{
        headers => ?SUP:bearer(Owner)
    }),
    ?assertEqual(200, maps:get(status, Root), maps:get(body, Root)),
    RootIds = payload_ids(Root),
    ?assertEqual(
        lists:sort([
            maps:get(root_a, S),
            maps:get(search_dept, S),
            maps:get(root_c, S),
            maps:get(root_d, S)
        ]),
        lists:sort(RootIds),
        {root_ids, maps:get(body, Root)}
    ),
    %% child：parent_id=root_a → child_a1 + child_a2。
    P = integer_to_binary(maps:get(root_a, S)),
    Child = ?SUP:http(State, <<"GET">>, <<B/binary, "/departments?parent_id=", P/binary>>, #{
        headers => ?SUP:bearer(Owner)
    }),
    ?assertEqual(200, maps:get(status, Child), maps:get(body, Child)),
    ?assertEqual(
        lists:sort([maps:get(child_a1, S), maps:get(child_a2, S)]),
        lists:sort(payload_ids(Child)),
        {child_ids, maps:get(body, Child)}
    ).

%% ===================================================================
%% me：当前用户 active 部门（m_child → child_a1）
%% ===================================================================

me_returns_active_departments(State) ->
    S = scope(State),
    B = org_path(maps:get(org_id, S)),
    R = ?SUP:http(State, <<"GET">>, <<B/binary, "/me">>, #{
        headers => ?SUP:bearer(maps:get(m_child, S))
    }),
    ?assertEqual(200, maps:get(status, R), maps:get(body, R)),
    Json = maps:get(json, R),
    ?assertMatch(#{<<"payload">> := #{<<"list">> := _}}, Json),
    #{<<"payload">> := #{<<"list">> := List}} = Json,
    DeptIds = [maps:get(<<"id">>, D) || D <- List, is_map_key(<<"id">>, D)],
    ?assertEqual([maps:get(child_a1, S)], DeptIds, {me_depts, maps:get(body, R)}).

%% ===================================================================
%% helpers
%% ===================================================================

scope(State) -> maps:get(scope, State).

org_path(Org) ->
    <<"/api/v1/organizations/", (integer_to_binary(Org))/binary, "/directory">>.

missing_org_id(S) ->
    %% 与种子空间同量级但不存在的 id（随机 TSID 碰撞概率可忽略）。
    Candidate = organization_directory_fixture:id(),
    case Candidate =/= maps:get(org_id, S) of
        true -> Candidate;
        false -> Candidate + 1
    end.

payload_ids(R) ->
    #{<<"payload">> := #{<<"list">> := List}} = maps:get(json, R),
    [row_id(Item) || Item <- List, is_map(Item)].

%% 部门投影键是 id；成员/人投影键是 user_id（project_human/project_search_row）。
row_id(Item) ->
    case is_map_key(<<"id">>, Item) of
        true -> maps:get(<<"id">>, Item);
        false -> maps:get(<<"user_id">>, Item)
    end.

page_of(R) ->
    Payload = maps:get(<<"payload">>, maps:get(json, R)),
    Ids = [row_id(I) || I <- maps:get(<<"list">>, Payload), is_map(I)],
    Cursor =
        case is_map_key(<<"cursor">>, Payload) of
            true -> maps:get(<<"cursor">>, Payload);
            false -> maps:get(<<"next_cursor">>, Payload, undefined)
        end,
    %% JSON null（末页）与缺失键同义：归一为 undefined。
    case Cursor of
        null -> {Ids, undefined};
        Other -> {Ids, Other}
    end.
