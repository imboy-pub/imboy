-module(ai_agent_role_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

page_uses_parameterized_queries_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"ai_agent_role">>) -> <<"public.ai_agent_role">>;
                    (<<"ai_agent">>) -> <<"public.ai_agent">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"ILIKE">>)),
                    ?assertEqual([<<"%doctor%">>, 1], Params),
                    {ok, [#{<<"total">> => 0}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{total := 0, page := 2, size := 10, list := []}},
                ai_agent_role_repo:page(2, 10, #{keyword => <<"doctor">>, status => 1})
            )
        end
    ).

find_returns_notfound_without_crashing_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"ai_agent_role">>) -> <<"public.ai_agent_role">>;
                    (<<"ai_agent_role_version">>) -> <<"public.ai_agent_role_version">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, [<<"doctor">>]) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, notfound}, ai_agent_role_repo:find(<<"doctor">>))
        end
    ).

find_published_only_uses_active_published_version_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"ai_agent_role">>) -> <<"public.ai_agent_role">>;
                    (<<"ai_agent_role_version">>) -> <<"public.ai_agent_role_version">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [<<"doctor">>]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"v.version = r.active_version">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"v.state = 'published'">>)),
                    {ok, [#{<<"version">> => 2, <<"state">> => <<"published">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, _}, ai_agent_role_repo:find_published(<<"doctor">>))
        end
    ).

publish_runs_all_state_changes_in_one_transaction_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"ai_agent_role">>) -> <<"public.ai_agent_role">>;
                    (<<"ai_agent_role_version">>) -> <<"public.ai_agent_role_version">>
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'query', 3, fun publish_query/3}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                ai_agent_role_repo:publish(<<"doctor">>, 3, 99)
            ),
            ?assert(meck:called(elib_pg, with_tx, '_'))
        end
    ).

publish_query(_Conn, Sql, Params) ->
    case
        {
            binary:match(Sql, <<"state = 'published'">>),
            binary:match(Sql, <<"active_version">>),
            Params
        }
    of
        {_, nomatch, [<<"doctor">>, 3]} ->
            {ok, []};
        {_, nomatch, [<<"doctor">>, 3, 99]} ->
            {ok, [#{<<"role_code">> => <<"doctor">>}]};
        {nomatch, nomatch, [<<"doctor">>, 3, 99]} ->
            {ok, [#{<<"role_code">> => <<"doctor">>}]};
        {nomatch, _, [<<"doctor">>, 3]} ->
            {ok, [#{<<"code">> => <<"doctor">>}]}
    end.
