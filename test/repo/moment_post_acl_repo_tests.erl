-module(moment_post_acl_repo_tests).
%%%
%%% moment_post_acl repo —— insert_acl_batch id 生成专项 eunit
%%% GAP（moment-acl-first-closure.json still_open[1]）补齐：id 为应用侧 TSID
%%% （elib_tsid:generate(moment_post_acl)），本表 DDL 的 id 无默认值，生成路径
%%% 断裂（名字漏注册 / 返回值未落列）即任何带 allow_uids/deny_uids 的发帖整
%%% 事务失败——与 trust_audit 漏注册（GZAPP-09 同族）同一缺陷面。
%%% 以导出的 replace_for_post/4 驱动私有 insert_acl_batch（纯 meck，不触库、
%%% 不占独占进程，可安全参与全量轨道）。
%%% 注意：delete_by_post 走 elib_pg:execute_count/3（8c4d7168 计数契约），
%%% insert 走 elib_pg:execute/3，两者须同时 mock，未声明入口会 fail-closed，不透传触库。
%%%

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% elib_pg 双入口 mock：调用记录入 Exec 表 {序号, del|ins, SqlBin, Params}
mock_pg_exec(Exec) ->
    {elib_pg, [
        {'execute_count', 3, fun(_Conn, Sql, Params) ->
            N = ets:info(Exec, size) + 1,
            ets:insert(Exec, {N, del, iolist_to_binary(Sql), Params}),
            {ok, N}
        end},
        {'execute', 3, fun(_Conn, Sql, Params) ->
            N = ets:info(Exec, size) + 1,
            ets:insert(Exec, {N, ins, iolist_to_binary(Sql), Params}),
            {ok, N}
        end}
    ]}.

replace_for_post_generates_unique_tsid_per_row_test_() ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            Gen = ets:new(mpars_tsid_calls, [public, ordered_set]),
            Exec = ets:new(mpars_exec_calls, [public, ordered_set]),
            pure_mock_fixture:with_mocks(
                [
                    {elib_tsid, [
                        {'generate', 1, fun(Name) ->
                            N = ets:info(Gen, size) + 1,
                            ets:insert(Gen, {N, Name}),
                            900000 + N
                        end}
                    ]},
                    mock_pg_exec(Exec)
                ],
                fun() ->
                    _ = application:load(imboy),
                    ?assertEqual(
                        ok,
                        moment_post_acl_repo:replace_for_post(conn, 42, [100, 200], [300])
                    ),
                    %% 逐行生成且入参恒为注册名 moment_post_acl（漏注册同族缺陷守门）
                    ?assertEqual(
                        [{1, moment_post_acl}, {2, moment_post_acl}, {3, moment_post_acl}],
                        lists:sort(ets:tab2list(Gen))
                    ),
                    %% 调用序：delete → allow 批 → deny 批
                    [
                        {1, del, DelSql, DelParams},
                        {2, ins, AllowSql, AllowParams},
                        {3, ins, DenySql, DenyParams}
                    ] = lists:sort(ets:tab2list(Exec)),
                    ?assertMatch(
                        {_, _},
                        binary:match(DelSql, <<"DELETE FROM public.moment_post_acl">>)
                    ),
                    ?assertEqual([42], DelParams),
                    %% id（每行首参）来自 elib_tsid:generate 返回序列且批量内唯一
                    ?assertMatch(
                        {_, _},
                        binary:match(AllowSql, <<"INSERT INTO public.moment_post_acl">>)
                    ),
                    ?assertMatch(
                        {_, _},
                        binary:match(
                            AllowSql, <<"ON CONFLICT (post_id, uid, acl_type) DO NOTHING">>
                        )
                    ),
                    ?assertMatch(
                        [900001, 42, 100, 1, _Now, 900002, 42, 200, 1, _Now],
                        AllowParams
                    ),
                    ?assertMatch(
                        {_, _},
                        binary:match(DenySql, <<"INSERT INTO public.moment_post_acl">>)
                    ),
                    ?assertMatch([900003, 42, 300, 2, _Now], DenyParams),
                    ets:delete(Gen),
                    ets:delete(Exec)
                end
            )
        end)
    end}.

replace_for_post_empty_deny_uids_short_circuits_test_() ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            Gen = ets:new(mpars_tsid_calls, [public, ordered_set]),
            Exec = ets:new(mpars_exec_calls, [public, ordered_set]),
            pure_mock_fixture:with_mocks(
                [
                    {elib_tsid, [
                        {'generate', 1, fun(Name) ->
                            N = ets:info(Gen, size) + 1,
                            ets:insert(Gen, {N, Name}),
                            N
                        end}
                    ]},
                    mock_pg_exec(Exec)
                ],
                fun() ->
                    _ = application:load(imboy),
                    ?assertEqual(ok, moment_post_acl_repo:replace_for_post(conn, 7, [100], [])),
                    %% deny 空列表短路：仅 delete + allow 一批，不产生第二次 INSERT
                    ?assertEqual(1, ets:info(Gen, size)),
                    ?assertEqual(2, ets:info(Exec, size)),
                    ets:delete(Gen),
                    ets:delete(Exec)
                end
            )
        end)
    end}.

replace_for_post_all_empty_only_deletes_test_() ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            Gen = ets:new(mpars_tsid_calls, [public, ordered_set]),
            Exec = ets:new(mpars_exec_calls, [public, ordered_set]),
            pure_mock_fixture:with_mocks(
                [
                    {elib_tsid, [
                        {'generate', 1, fun(Name) ->
                            N = ets:info(Gen, size) + 1,
                            ets:insert(Gen, {N, Name}),
                            N
                        end}
                    ]},
                    mock_pg_exec(Exec)
                ],
                fun() ->
                    _ = application:load(imboy),
                    ?assertEqual(ok, moment_post_acl_repo:replace_for_post(conn, 7, [], [])),
                    ?assertEqual(0, ets:info(Gen, size)),
                    ?assertEqual(1, ets:info(Exec, size)),
                    ets:delete(Gen),
                    ets:delete(Exec)
                end
            )
        end)
    end}.

replace_for_post_normalizes_uids_dedup_filter_test_() ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            Gen = ets:new(mpars_tsid_calls, [public, ordered_set]),
            Exec = ets:new(mpars_exec_calls, [public, ordered_set]),
            pure_mock_fixture:with_mocks(
                [
                    {elib_tsid, [
                        {'generate', 1, fun(Name) ->
                            N = ets:info(Gen, size) + 1,
                            ets:insert(Gen, {N, Name}),
                            N
                        end}
                    ]},
                    mock_pg_exec(Exec)
                ],
                fun() ->
                    _ = application:load(imboy),
                    ?assertEqual(
                        ok,
                        moment_post_acl_repo:replace_for_post(
                            conn, 9, [200, 100, 200, -5, <<"bad">>], []
                        )
                    ),
                    %% normalize_uids：非整数与 <=0 过滤、usort 去重升序 → 仅 100/200 两行
                    ?assertEqual(2, ets:info(Gen, size)),
                    [{2, ins, _AllowSql, AllowParams}] = ets:lookup(Exec, 2),
                    ?assertMatch([1, 9, 100, 1, _Now, 2, 9, 200, 1, _Now], AllowParams),
                    ets:delete(Gen),
                    ets:delete(Exec)
                end
            )
        end)
    end}.

replace_for_post_allow_insert_failure_short_circuits_test_() ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            Gen = ets:new(mpars_tsid_calls, [public, ordered_set]),
            Exec = ets:new(mpars_exec_calls, [public, ordered_set]),
            pure_mock_fixture:with_mocks(
                [
                    {elib_tsid, [
                        {'generate', 1, fun(Name) ->
                            N = ets:info(Gen, size) + 1,
                            ets:insert(Gen, {N, Name}),
                            N
                        end}
                    ]},
                    {elib_pg, [
                        {'execute_count', 3, fun(_C, _S, _P) -> {ok, 1} end},
                        {'execute', 3, fun(_Conn, _Sql, _Params) -> {error, acl_insert_failed} end}
                    ]}
                ],
                fun() ->
                    _ = application:load(imboy),
                    %% allow 批失败即整体失败，deny 批不再执行
                    ?assertEqual(
                        {error, acl_insert_failed},
                        moment_post_acl_repo:replace_for_post(conn, 7, [100], [200])
                    ),
                    ?assertEqual(1, ets:info(Gen, size)),
                    ets:delete(Gen),
                    ets:delete(Exec)
                end
            )
        end)
    end}.
