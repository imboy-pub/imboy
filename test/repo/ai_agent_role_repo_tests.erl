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
