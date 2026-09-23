%%% @doc Human Organization Directory 用例层单元测试（meck PG + 真实 mock 游标）。
%%%
%%% 覆盖矩阵（§14.2 行为合同）：
%%%   * 授权门：404 / archived 403 / 非成员 403 / removed 403 / suspended 403，
%%%     且门先于任何目录读（读函数被调即破防）
%%%   * 参数门：limit 缺省 50 / 越界 400 不截断；parent_id/department_id 形状；
%%%     q trim 后 2..64
%%%   * 投影：departments id/name/parent_id/member_count；
%%%     members user_id/display_name/avatar/department_ids；/me 空数组
%%%   * N+1（单元级）：department_ids 每页恰好一次批量调用（全页 uid 入参）
%%%   * 游标：roundtrip（domain/endpoint/filter/org/uid/sort_tuple 绑定）
%%%     与负例（tampered/foreign domain/foreign org/foreign user/foreign
%%%     endpoint/foreign filter/expired/密钥缺失 503/sort_tuple 形状）
-module(organization_directory_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, organization_directory_pg).
-define(MOCK_CURSOR, organization_directory_cursor_mock).
-define(ORG, 9001).
-define(UID, 9002).
-define(DEPT_ROOT, 9101).
-define(DEPT_CHILD, 9102).
-define(KEY, <<"unit-test-signing-key-0123456789abcdef">>).

%% PG 行原料（二进制列名，同 elib_pg 返回形状）。
dept_row(Id, Name, Parent, Count) ->
    #{
        <<"id">> => Id,
        <<"parent_id">> => Parent,
        <<"name">> => Name,
        <<"member_count">> => Count
    }.

human_row(Uid, Nickname, Account) ->
    #{
        <<"user_id">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => Account,
        <<"avatar">> => <<"https://a.example.com/x.png">>
    }.

gate_ok() ->
    meck:expect(?PG, gate, fun(_O, _U) ->
        {ok, #{<<"org_status">> => <<"active">>, <<"member_status">> => <<"active">>}}
    end).

%% ===================================================================
%% 授权门
%% ===================================================================

gate_not_found_is_404_test_() ->
    with_mocks(
        fun() ->
            meck:expect(?PG, gate, fun(_O, _U) -> {error, not_found} end),
            forbid_reads(),
            ?assertEqual(
                {error, <<"resource_not_found">>},
                deps(#{})
            )
        end
    ).

gate_archived_org_is_organization_disabled_test_() ->
    with_mocks(
        fun() ->
            meck:expect(?PG, gate, fun(_O, _U) ->
                {ok, #{<<"org_status">> => <<"archived">>, <<"member_status">> => <<"active">>}}
            end),
            forbid_reads(),
            lists:foreach(
                fun(F) ->
                    ?assertEqual(
                        {error, <<"organization_disabled">>},
                        F()
                    )
                end,
                [
                    fun() ->
                        deps(#{})
                    end,
                    fun() -> mem(#{}) end,
                    fun() -> organization_directory_app:my_departments(?ORG, ?UID) end,
                    fun() -> srch(#{q => <<"ab">>}) end
                ]
            )
        end
    ).

gate_nonmember_removed_suspended_all_403_test_() ->
    with_mocks(
        fun() ->
            forbid_reads(),
            lists:foreach(
                fun(MemberStatus) ->
                    meck:expect(?PG, gate, fun(_O, _U) ->
                        {ok, #{
                            <<"org_status">> => <<"active">>, <<"member_status">> => MemberStatus
                        }}
                    end),
                    ?assertEqual(
                        {error, <<"insufficient_scope">>},
                        deps(#{})
                    )
                end,
                [null, <<"removed">>, <<"suspended">>]
            )
        end
    ).

%% 门必须先于目录读：gate 失败时读函数被调即 erlang:error 破防。
gate_precedes_any_directory_read_test_() ->
    with_mocks(
        fun() ->
            meck:expect(?PG, gate, fun(_O, _U) -> {error, not_found} end),
            meck:expect(?PG, list_my_departments, fun(_O, _U) ->
                erlang:error(ungated_directory_read)
            end),
            ?assertEqual(
                {error, <<"resource_not_found">>},
                organization_directory_app:my_departments(?ORG, ?UID)
            )
        end
    ).

gate_db_error_is_internal_error_test_() ->
    with_mocks(
        fun() ->
            meck:expect(?PG, gate, fun(_O, _U) -> {error, {epgsql, boom}} end),
            ?assertEqual(
                {error, <<"internal_error">>},
                deps(#{})
            )
        end
    ).

%% ===================================================================
%% limit / filter 参数门
%% ===================================================================

limit_out_of_range_400_no_truncation_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            forbid_reads(),
            lists:foreach(
                fun(Limit) ->
                    ?assertEqual(
                        {error, <<"invalid_request">>},
                        deps(#{limit => Limit})
                    )
                end,
                [0, -1, 101, 1000, <<"abc">>, <<"1.5">>]
            )
        end
    ).

limit_default_50_and_boundaries_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            Fetched =
                [
                    begin
                        meck:expect(?PG, list_children_departments, fun(_O, _P, _A, Fetch) ->
                            put(t_fetch, Fetch),
                            {ok, []}
                        end),
                        Params =
                            case Limit of
                                undefined -> #{};
                                L -> #{limit => L}
                            end,
                        {ok, _} =
                            deps(Params),
                        erase(t_fetch)
                    end
                 || Limit <- [undefined, 1, 100]
                ],
            %% fetch 恒为 limit+1（has_more 判定），limit 由入参/缺省决定。
            ?assertEqual([51, 2, 101], Fetched)
        end
    ).

parent_id_shape_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            meck:expect(?PG, list_children_departments, fun(_O, Parent, _A, _F) ->
                put(t_parent, Parent),
                {ok, []}
            end),
            %% 非法形状 → 400。
            lists:foreach(
                fun(Bad) ->
                    ?assertEqual(
                        {error, <<"invalid_request">>},
                        deps(#{parent_id => Bad})
                    )
                end,
                [-1, <<"abc">>, <<"1.5">>]
            ),
            %% 缺省/空串 → 根级 null；合法整数 → 原样。
            {ok, _} = deps(#{}),
            ?assertEqual(null, get(t_parent)),
            {ok, _} = deps(#{parent_id => integer_to_binary(?DEPT_ROOT)}),
            ?assertEqual(?DEPT_ROOT, get(t_parent))
        end
    ).

%% ===================================================================
%% departments 投影 / 分页 / 游标 roundtrip
%% ===================================================================

departments_projection_and_end_cursor_null_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            meck:expect(?PG, list_children_departments, fun(_O, null, 0, _F) ->
                {ok, [
                    dept_row(?DEPT_ROOT, <<"研发"/utf8>>, null, 3),
                    dept_row(?DEPT_CHILD, <<"后端"/utf8>>, ?DEPT_ROOT, 0)
                ]}
            end),
            {ok, Result} = deps(#{}),
            ?assertEqual(false, maps:get(has_more, Result)),
            ?assertEqual(null, maps:get(cursor, Result)),
            ?assertEqual(
                [
                    #{
                        id => ?DEPT_ROOT,
                        name => <<"研发"/utf8>>,
                        parent_id => null,
                        member_count => 3
                    },
                    #{
                        id => ?DEPT_CHILD,
                        name => <<"后端"/utf8>>,
                        parent_id => ?DEPT_ROOT,
                        member_count => 0
                    }
                ],
                maps:get(list, Result)
            )
        end
    ).

departments_has_more_signs_bound_cursor_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            %% 单一 expect 覆盖两页：After=0 返回 3 行（limit=2 → has_more），
            %% After=12 返回末页 1 行。
            meck:expect(?PG, list_children_departments, fun
                (_O, null, 0, _F) ->
                    {ok, [
                        dept_row(11, <<"a">>, null, 0),
                        dept_row(12, <<"b">>, null, 0),
                        dept_row(13, <<"c">>, null, 0)
                    ]};
                (_O, null, 12, _F) ->
                    {ok, [dept_row(13, <<"c">>, null, 0)]}
            end),
            {ok, #{list := List, cursor := Cursor, has_more := true}} =
                deps(#{limit => 2}),
            ?assertEqual([11, 12], [maps:get(id, D) || D <- List]),
            ?assert(is_binary(Cursor)),
            %% 解签回读绑定：Human 域 + 本端点 + 本过滤 + keyset 位置。
            {ok, Key} = ?MOCK_CURSOR:signing_key(),
            {ok, Payload} = ?MOCK_CURSOR:verify(Cursor, Key),
            ?assertEqual(2, maps:get(<<"v">>, Payload)),
            ?assertEqual(<<"human_directory">>, maps:get(<<"domain">>, Payload)),
            ?assertEqual(<<"departments">>, maps:get(<<"endpoint">>, Payload)),
            ?assertEqual(?ORG, maps:get(<<"organization_id">>, Payload)),
            ?assertEqual(?UID, maps:get(<<"user_id">>, Payload)),
            ?assertEqual(null, maps:get(<<"filter">>, Payload)),
            ?assertEqual([12], maps:get(<<"sort_tuple">>, Payload)),
            %% 第二页：带游标请求 → pg 收到 After=12（expect 已按 After 分支覆盖）。
            {ok, Page2} = deps(#{cursor => Cursor, limit => 2}),
            ?assertEqual(false, maps:get(has_more, Page2)),
            ?assertEqual(null, maps:get(cursor, Page2))
        end
    ).

%% ===================================================================
%% members：缺省根成员 / 指定部门 / department_ids 批量补齐（N+1 断言）
%% ===================================================================

members_root_default_and_dept_branch_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            meck:expect(?PG, list_root_humans, fun(_O, 0, _F) ->
                {ok, [human_row(21, <<"张三"/utf8>>, <<"acc21">>)]}
            end),
            meck:expect(?PG, list_department_humans, fun(_O, ?DEPT_CHILD, 0, _F) ->
                {ok, [human_row(31, <<>>, <<"acc31">>)]}
            end),
            meck:expect(?PG, batch_user_departments, fun(_O, Uids) ->
                put(t_batch_uids, Uids),
                {ok, #{31 => [?DEPT_CHILD, ?DEPT_ROOT]}}
            end),
            %% 缺省 → 根成员查询；display_name 回落 account。
            {ok, Root} = mem(#{}),
            ?assertMatch(
                [
                    #{
                        user_id := 21,
                        display_name := <<"张三"/utf8>>,
                        avatar := <<"https://a.example.com/x.png">>,
                        department_ids := []
                    }
                ],
                maps:get(list, Root)
            ),
            %% 指定部门 → 部门成员查询 + 批量补齐 department_ids。
            {ok, Dept} = mem(#{department_id => integer_to_binary(?DEPT_CHILD)}),
            ?assertMatch(
                [
                    #{
                        user_id := 31,
                        display_name := <<"acc31">>,
                        department_ids := [?DEPT_CHILD, ?DEPT_ROOT]
                    }
                ],
                maps:get(list, Dept)
            ),
            %% N+1 断言：两次请求各恰好一次批量调用（每页 1 次，非每行）。
            ?assertEqual(2, meck:num_calls(?PG, batch_user_departments, 2)),
            ?assertEqual([31], get(t_batch_uids))
        end
    ).

members_page_batch_uses_all_uids_once_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            PageRows = [human_row(U, <<"n">>, <<"a">>) || U <- lists:seq(1, 5)],
            meck:expect(?PG, list_root_humans, fun(_O, 0, _F) -> {ok, PageRows} end),
            meck:expect(?PG, batch_user_departments, fun(_O, Uids) ->
                put(t_batch_uids, Uids),
                {ok, maps:from_list([{U, [77]} || U <- Uids])}
            end),
            {ok, Result} = mem(#{}),
            ?assertEqual(5, length(maps:get(list, Result))),
            lists:foreach(
                fun(#{department_ids := Ids}) -> ?assertEqual([77], Ids) end,
                maps:get(list, Result)
            ),
            ?assertEqual(1, meck:num_calls(?PG, batch_user_departments, 2)),
            ?assertEqual(lists:seq(1, 5), get(t_batch_uids))
        end
    ).

%% ===================================================================
%% /me：空数组语义
%% ===================================================================

me_empty_departments_returns_empty_list_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            meck:expect(?PG, list_my_departments, fun(_O, _U) -> {ok, []} end),
            ?assertEqual(
                {ok, #{organization_id => ?ORG, list => []}},
                organization_directory_app:my_departments(?ORG, ?UID)
            )
        end
    ).

me_projection_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            meck:expect(?PG, list_my_departments, fun(_O, _U) ->
                {ok, [dept_row(?DEPT_CHILD, <<"后端"/utf8>>, ?DEPT_ROOT, 2)]}
            end),
            ?assertEqual(
                {ok, #{
                    organization_id => ?ORG,
                    list => [
                        #{
                            id => ?DEPT_CHILD,
                            name => <<"后端"/utf8>>,
                            parent_id => ?DEPT_ROOT,
                            member_count => 2
                        }
                    ]
                }},
                organization_directory_app:my_departments(?ORG, ?UID)
            )
        end
    ).

%% ===================================================================
%% search：q 校验 / 分型投影 / 游标 roundtrip
%% ===================================================================

search_query_length_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            forbid_reads(),
            %% 少于 2 / 超过 64 / 非二进制 / 缺参 → 400。
            lists:foreach(
                fun(Q) ->
                    ?assertEqual(
                        {error, <<"invalid_request">>},
                        srch(#{q => Q})
                    )
                end,
                [<<"a">>, <<" ">>, binary:copy(<<"x">>, 65), 123, undefined]
            ),
            %% trim 后 2 字符可通过；64 字符边界可通过。
            lists:foreach(
                fun(Q) ->
                    meck:expect(?PG, search_directory, fun(_O, _P, 0, 0, _F) -> {ok, []} end),
                    ?assertMatch(
                        {ok, #{list := []}},
                        srch(#{q => Q})
                    )
                end,
                [<<"  ab  ">>, binary:copy(<<"x">>, 64)]
            )
        end
    ).

search_mixed_projection_and_cursor_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            DeptHit = #{
                <<"kind">> => 0,
                <<"id">> => ?DEPT_ROOT,
                <<"name">> => <<"研发部"/utf8>>,
                <<"parent_id">> => null,
                <<"member_count">> => 3,
                <<"user_id">> => null,
                <<"nickname">> => null,
                <<"account">> => null,
                <<"avatar">> => null
            },
            MemberHit = #{
                <<"kind">> => 1,
                <<"id">> => 31,
                <<"name">> => null,
                <<"parent_id">> => null,
                <<"member_count">> => null,
                <<"user_id">> => 31,
                <<"nickname">> => <<"张三"/utf8>>,
                <<"account">> => <<"acc31">>,
                <<"avatar">> => <<>>
            },
            meck:expect(?PG, search_directory, fun(_O, _Pattern, 0, 0, _F) ->
                {ok, [DeptHit, MemberHit]}
            end),
            meck:expect(?PG, batch_user_departments, fun(_O, [31]) ->
                {ok, #{31 => [?DEPT_CHILD]}}
            end),
            {ok, Result} = srch(#{q => <<"张三"/utf8>>}),
            ?assertEqual(false, maps:get(has_more, Result)),
            ?assertEqual(null, maps:get(cursor, Result)),
            ?assertEqual(
                [
                    #{
                        type => department,
                        id => ?DEPT_ROOT,
                        name => <<"研发部"/utf8>>,
                        parent_id => null,
                        member_count => 3
                    },
                    #{
                        type => member,
                        user_id => 31,
                        display_name => <<"张三"/utf8>>,
                        avatar => <<>>,
                        department_ids => [?DEPT_CHILD]
                    }
                ],
                maps:get(list, Result)
            ),
            %% 部门命中不触发 department_ids 批量；成员命中整页一次。
            ?assertEqual(1, meck:num_calls(?PG, batch_user_departments, 2))
        end
    ).

search_cursor_roundtrip_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            Full = [
                #{
                    <<"kind">> => 0,
                    <<"id">> => 5,
                    <<"name">> => <<"d">>,
                    <<"parent_id">> => null,
                    <<"member_count">> => 0,
                    <<"user_id">> => null,
                    <<"nickname">> => null,
                    <<"account">> => null,
                    <<"avatar">> => null
                },
                #{
                    <<"kind">> => 1,
                    <<"id">> => 31,
                    <<"name">> => null,
                    <<"parent_id">> => null,
                    <<"member_count">> => null,
                    <<"user_id">> => 31,
                    <<"nickname">> => <<"n">>,
                    <<"account">> => <<"a">>,
                    <<"avatar">> => <<>>
                },
                #{
                    <<"kind">> => 1,
                    <<"id">> => 32,
                    <<"name">> => null,
                    <<"parent_id">> => null,
                    <<"member_count">> => null,
                    <<"user_id">> => 32,
                    <<"nickname">> => <<"n">>,
                    <<"account">> => <<"a">>,
                    <<"avatar">> => <<>>
                }
            ],
            meck:expect(?PG, search_directory, fun
                (_O, _Pattern, 0, 0, _F) ->
                    {ok, Full};
                (_O, _P, 1, 31, _F) ->
                    put(t_search_after, {1, 31}),
                    {ok, [lists:last(Full)]}
            end),
            meck:expect(?PG, batch_user_departments, fun(_O, _U) -> {ok, #{}} end),
            {ok, #{cursor := Cursor, has_more := true}} =
                srch(#{q => <<"ab">>, limit => 2}),
            {ok, _} = srch(#{q => <<"ab">>, limit => 2, cursor => Cursor}),
            ?assertEqual({1, 31}, get(t_search_after))
        end
    ).

%% ===================================================================
%% 游标负例：六项绑定 + 密钥门（全部 400 invalid_request / 503）
%% ===================================================================

cursor_negatives_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            meck:expect(?PG, list_children_departments, fun(_O, null, _A, _F) ->
                put(t_read_reached, true),
                {ok, []}
            end),
            {ok, Key} = ?MOCK_CURSOR:signing_key(),

            ValidPayload = #{
                <<"v">> => 2,
                <<"domain">> => <<"human_directory">>,
                <<"organization_id">> => ?ORG,
                <<"user_id">> => ?UID,
                <<"endpoint">> => <<"departments">>,
                <<"filter">> => null,
                <<"sort_tuple">> => [42],
                <<"issued_at">> => os:system_time(second)
            },
            Signed = fun(P) ->
                {ok, C} = ?MOCK_CURSOR:sign(P, Key),
                C
            end,

            %% tampered：翻转尾字符。
            Valid = Signed(ValidPayload),
            Tampered = flip_last(Valid),
            %% foreign domain（Internal 游标跨用）。
            ForeignDomain = Signed(ValidPayload#{<<"domain">> => <<"internal_directory">>}),
            %% 跨 Org / 跨用户 / 跨端点 / 跨过滤。
            ForeignOrg = Signed(ValidPayload#{<<"organization_id">> => ?ORG + 1}),
            ForeignUser = Signed(ValidPayload#{<<"user_id">> => ?UID + 1}),
            ForeignEndpoint = Signed(ValidPayload#{<<"endpoint">> => <<"members">>}),
            ForeignFilter = Signed(ValidPayload#{<<"filter">> => 999}),
            %% sort_tuple 形状非法。
            BadSortTuple = Signed(ValidPayload#{<<"sort_tuple">> => [<<"x">>]}),

            Bad = [
                {garbage, <<"not-a-cursor">>},
                {tampered, Tampered},
                {foreign_domain, ForeignDomain},
                {foreign_org, ForeignOrg},
                {foreign_user, ForeignUser},
                {foreign_endpoint, ForeignEndpoint},
                {foreign_filter, ForeignFilter},
                {bad_sort_tuple, BadSortTuple}
            ],
            lists:foreach(
                fun({Label, Cursor}) ->
                    put(t_read_reached, false),
                    ?assertEqual(
                        {error, <<"invalid_request">>},
                        deps(#{cursor => Cursor}),
                        {cursor_negative_failed, Label}
                    ),
                    ?assertEqual(false, get(t_read_reached), {read_reached, Label})
                end,
                Bad
            ),

            %% 合法游标放行（对照：非游标原因导致 400 之外的路径正常）。
            put(t_read_reached, false),
            ?assertMatch(
                {ok, _},
                deps(#{cursor => Valid})
            ),
            ?assertEqual(true, get(t_read_reached))
        end
    ).

cursor_expired_rejected_by_mock_verify_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            {ok, Key} = ?MOCK_CURSOR:signing_key(),
            Expired = signed_payload(
                Key, #{<<"issued_at">> => os:system_time(second) - 25 * 3600}
            ),
            ?assertMatch(
                {error, <<"invalid_request">>},
                deps(#{cursor => Expired})
            )
        end
    ).

%% 应用层自身的 24h 新鲜度校验（verify 放行旧 issued_at 时仍必须拒）。
cursor_expired_rejected_by_app_check_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            OldPayload = #{
                <<"v">> => 2,
                <<"domain">> => <<"human_directory">>,
                <<"organization_id">> => ?ORG,
                <<"user_id">> => ?UID,
                <<"endpoint">> => <<"departments">>,
                <<"filter">> => null,
                <<"sort_tuple">> => [42],
                <<"issued_at">> => os:system_time(second) - 25 * 3600
            },
            %% meck 游标模块的 verify「放行」过期 payload，逼出 app 层校验。
            ok = meck:new(?MOCK_CURSOR, [no_passthrough_cover, passthrough]),
            meck:expect(?MOCK_CURSOR, verify, fun(_C, _K) -> {ok, OldPayload} end),
            ?assertMatch(
                {error, <<"invalid_request">>},
                deps(#{cursor => <<"x.y">>, cursor_mod => ?MOCK_CURSOR})
            ),
            ok = meck:unload(?MOCK_CURSOR)
        end
    ).

%% 请求游标在场而密钥缺失 → 503（decode 前置门立即触发）。
signing_key_missing_is_503_test_() ->
    with_mocks(
        fun() ->
            gate_ok(),
            application:unset_env(imboy, enterprise_internal_cursor_signing_key),
            ?assertMatch(
                {error, <<"security_gate_closed">>},
                organization_directory_app:list_departments(
                    ?ORG,
                    ?UID,
                    #{cursor => <<"any-cursor">>, cursor_mod => ?MOCK_CURSOR}
                )
            ),
            %% has_more 页在密钥缺失下签发下一页 → 同样 503（fail-closed）。
            ok = application:set_env(imboy, enterprise_internal_cursor_signing_key, ?KEY),
            meck:expect(?PG, list_children_departments, fun(_O, null, 0, _F) ->
                {ok, [dept_row(11, <<"a">>, null, 0), dept_row(12, <<"b">>, null, 0)]}
            end),
            {ok, #{cursor := First, has_more := true}} =
                deps(#{limit => 1, cursor_mod => ?MOCK_CURSOR}),
            ?assert(is_binary(First)),
            application:unset_env(imboy, enterprise_internal_cursor_signing_key),
            meck:expect(?PG, list_children_departments, fun(_O, null, 0, _F) ->
                {ok, [dept_row(11, <<"a">>, null, 0), dept_row(12, <<"b">>, null, 0)]}
            end),
            ?assertMatch(
                {error, <<"security_gate_closed">>},
                deps(#{limit => 1, cursor_mod => ?MOCK_CURSOR})
            )
        end
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

%% 带 mock 游标模块的 app 调用捷径（免逐处显式注入）。
deps(P) ->
    organization_directory_app:list_departments(?ORG, ?UID, P#{cursor_mod => ?MOCK_CURSOR}).

mem(P) ->
    organization_directory_app:list_members(?ORG, ?UID, P#{cursor_mod => ?MOCK_CURSOR}).

srch(P) ->
    organization_directory_app:search(?ORG, ?UID, P#{cursor_mod => ?MOCK_CURSOR}).

signed_payload(Key, Overrides) ->
    Base = #{
        <<"v">> => 2,
        <<"domain">> => <<"human_directory">>,
        <<"organization_id">> => ?ORG,
        <<"user_id">> => ?UID,
        <<"endpoint">> => <<"departments">>,
        <<"filter">> => null,
        <<"sort_tuple">> => [42],
        <<"issued_at">> => os:system_time(second)
    },
    {ok, Cursor} = ?MOCK_CURSOR:sign(maps:merge(Base, Overrides), Key),
    Cursor.

%% 篡改游标中段字节：base64 末字符可能只承载 padding 位（如 'Q'→'R'
%% 解码后字节不变，HMAC 仍通过），中段翻转保证动到有效载荷或 MAC。
flip_last(Bin) when is_binary(Bin), byte_size(Bin) > 4 ->
    Mid = byte_size(Bin) div 2,
    <<Pre:Mid/binary, C, Rest/binary>> = Bin,
    Flipped =
        if
            C >= $a, C =< $z -> C - 32;
            true -> C + 1
        end,
    <<Pre/binary, Flipped, Rest/binary>>;
flip_last(_Other) ->
    <<"x">>.

%% 门失败/参数失败时任何目录读被调即破防（error 顶掉测试）。
forbid_reads() ->
    Boom = fun boom/1,
    Reads = [
        {list_children_departments, 4},
        {list_department_humans, 4},
        {list_root_humans, 3},
        {batch_user_departments, 2},
        {list_my_departments, 2},
        {search_directory, 5}
    ],
    lists:foreach(
        fun({F, Arity}) ->
            meck:expect(?PG, F, Boom(Arity))
        end,
        Reads
    ).

boom(2) -> fun(_, _) -> erlang:error(ungated_directory_read) end;
boom(3) -> fun(_, _, _) -> erlang:error(ungated_directory_read) end;
boom(4) -> fun(_, _, _, _) -> erlang:error(ungated_directory_read) end;
boom(5) -> fun(_, _, _, _, _) -> erlang:error(ungated_directory_read) end.

with_mocks(Body) ->
    {setup,
        fun() ->
            ok = application:set_env(imboy, enterprise_internal_cursor_signing_key, ?KEY),
            meck:new(?PG, [no_passthrough_cover]),
            gate_ok(),
            ok
        end,
        fun(_) ->
            catch meck:unload(?MOCK_CURSOR),
            catch meck:unload(?PG),
            application:unset_env(imboy, enterprise_internal_cursor_signing_key),
            ok
        end,
        Body}.
