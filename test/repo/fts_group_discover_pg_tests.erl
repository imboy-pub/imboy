-module(fts_group_discover_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% DB 不可用时按 TEST_WITH_DB 惯例返回 [] skip，不得计为通过。
%% elib_pg:query/2 经 WITH_MECKS 重定向到测试事务 Conn，使 repo 读到未提交行。
discover_category_pg_test_() ->
    {timeout, 60,
        {setup, fun eunit_runner:eunit_setup_with_db/0,
            fun
                ({ok, Conn}) -> eunit_runner:eunit_cleanup_db(Conn);
                (_) -> ok
            end, fun
                ({error, _}) ->
                    [];
                ({ok, Conn}) ->
                    ?WITH_MECKS(
                        [
                            {elib_pg, [
                                {query, 2, fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end}
                            ]}
                        ],
                        fun() -> discover_category(Conn) end
                    )
            end}}.

discover_category(Conn) ->
    {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
    try
        {ok, 4} = epgsql:equery(
            Conn,
            <<
                "INSERT INTO public.\"group\" "
                "(id, owner_uid, creator_uid, title, member_count, category_id, type, status) "
                "VALUES ($1,1,1,'one',20,$5,1,1),($2,1,1,'two',10,$5,1,1),"
                "($3,1,1,'other',30,$6,1,1),($4,1,1,'private',40,$5,2,1)"
            >>,
            [
                890000000001,
                890000000002,
                890000000003,
                890000000004,
                890000000101,
                890000000102
            ]
        ),
        {ok, First} = fts_group_repo:group_search_page(<<>>, 1, 0, 890000000101),
        {ok, Second} = fts_group_repo:group_search_page(<<>>, 1, 1, 890000000101),
        {ok, Other} = fts_group_repo:group_search_page(<<>>, 10, 0, 890000000102),
        {ok, Empty} = fts_group_repo:group_search_page(<<>>, 10, 0, 890000000103),
        {ok, All} = fts_group_repo:group_search_page(<<>>, 1000, 0),
        ?assertEqual([890000000001], ids(First)),
        ?assertEqual([890000000002], ids(Second)),
        ?assertEqual([890000000003], ids(Other)),
        ?assertEqual([], Empty),
        ?assert(lists:member(890000000001, ids(All))),
        ?assert(lists:member(890000000003, ids(All))),
        ?assertNot(lists:member(890000000004, ids(All)))
    after
        epgsql:squery(Conn, "ROLLBACK")
    end.

ids(Rows) -> [maps:get(<<"id">>, Row) || Row <- Rows].
