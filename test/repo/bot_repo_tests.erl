-module(bot_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% create
%% ===================================================================

create_inserts_bot_record_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"INSERT INTO">>)),
                    ?assertEqual(
                        1, proplists:get_value(1, lists:zip(lists:seq(1, length(Params)), Params))
                    ),
                    {ok, [#{<<"user_id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Rows} = bot_repo:create(#{
                user_id => 1,
                name => <<"TestBot">>,
                username => <<"testbot">>,
                owner_uid => 100
            }),
            ?assertEqual(1, length(Rows))
        end
    ).

%% ===================================================================
%% find
%% ===================================================================

find_returns_bot_by_user_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [1]) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE user_id = $1">>)),
                    {ok, [#{<<"user_id">> => 1, <<"name">> => <<"TestBot">>}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Bot} = bot_repo:find(1),
            ?assertEqual(<<"TestBot">>, maps:get(<<"name">>, Bot))
        end
    ).

find_returns_notfound_on_empty_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, [999]) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, notfound}, bot_repo:find(999))
        end
    ).

%% ===================================================================
%% find_by_username
%% ===================================================================

find_by_username_returns_bot_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [<<"testbot">>]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE username = $1">>)),
                    {ok, [#{<<"user_id">> => 1, <<"username">> => <<"testbot">>}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Bot} = bot_repo:find_by_username(<<"testbot">>),
            ?assertEqual(<<"testbot">>, maps:get(<<"username">>, Bot))
        end
    ).

find_by_username_returns_notfound_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, [<<"nonexistent">>]) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, notfound}, bot_repo:find_by_username(<<"nonexistent">>))
        end
    ).

%% ===================================================================
%% find_by_token
%% ===================================================================

find_by_token_returns_bot_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [<<"tok123">>]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE api_token = $1">>)),
                    {ok, [#{<<"user_id">> => 1, <<"status">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Bot} = bot_repo:find_by_token(<<"tok123">>),
            ?assertEqual(1, maps:get(<<"user_id">>, Bot))
        end
    ).

find_by_token_returns_notfound_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, [<<"bad">>]) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, notfound}, bot_repo:find_by_token(<<"bad">>))
        end
    ).

%% ===================================================================
%% update
%% ===================================================================

update_partial_update_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"UPDATE">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"SET">>)),
                    ?assertEqual(<<"NewName">>, hd(Params)),
                    {ok, [#{<<"user_id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = bot_repo:update(1, #{name => <<"NewName">>})
        end
    ).

update_with_empty_data_returns_ok_test_() ->
    ?WITH_MECKS([], fun() ->
        {ok, []} = bot_repo:update(1, #{})
    end).

%% ===================================================================
%% set_status
%% ===================================================================

set_status_updates_status_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"bot">>) -> <<"public.bot">> end}
            ]},
            {elib_pg, [
                {'update', 4, fun(Tb, Data, _Where, _Params) ->
                    ?assertEqual(<<"public.bot">>, Tb),
                    ?assertEqual(0, maps:get(status, Data)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            {ok, 1} = bot_repo:set_status(1, 0)
        end
    ).

%% ===================================================================
%% page
%% ===================================================================

page_returns_paginated_results_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"bot">>) -> <<"public.bot">>;
                    (<<"user">>) -> <<"public.user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    case binary:match(Sql, <<"count(*)">>) of
                        nomatch ->
                            {ok, [#{<<"user_id">> => 1, <<"name">> => <<"TestBot">>}]};
                        _ ->
                            {ok, [#{<<"total">> => 1}]}
                    end
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_repo:page(1, 20),
            ?assertEqual(1, maps:get(total, Result))
        end
    ).

page_returns_empty_on_zero_count_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"bot">>) -> <<"public.bot">>;
                    (<<"user">>) -> <<"public.user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    case binary:match(Sql, <<"count(*)">>) of
                        nomatch -> {ok, []};
                        _ -> {ok, [#{<<"total">> => 0}]}
                    end
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_repo:page(1, 20),
            ?assertEqual(0, maps:get(total, Result)),
            ?assertEqual([], maps:get(list, Result))
        end
    ).

%% ===================================================================
%% page_by_owner
%% ===================================================================

page_by_owner_returns_owner_bots_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"bot">>) -> <<"public.bot">>;
                    (<<"user">>) -> <<"public.user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    case binary:match(Sql, <<"count(*)">>) of
                        nomatch ->
                            ?assertEqual([100], Params),
                            {ok, [#{<<"user_id">> => 1}]};
                        _ ->
                            ?assertEqual([100], Params),
                            {ok, [#{<<"total">> => 1}]}
                    end
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_repo:page_by_owner(1, 20, 100),
            ?assertEqual(1, maps:get(total, Result))
        end
    ).

%% ===================================================================
%% search
%% ===================================================================

search_returns_matching_bots_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"bot">>) -> <<"public.bot">>;
                    (<<"user">>) -> <<"public.user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [Like]) ->
                    case binary:match(Sql, <<"count(*)">>) of
                        nomatch ->
                            ?assertNotEqual(nomatch, binary:match(Like, <<"test">>)),
                            {ok, [#{<<"user_id">> => 1}]};
                        _ ->
                            {ok, [#{<<"total">> => 1}]}
                    end
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_repo:search(<<"test">>, 1, 20),
            ?assertEqual(1, maps:get(total, Result))
        end
    ).

search_returns_empty_on_no_matches_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun
                    (<<"bot">>) -> <<"public.bot">>;
                    (<<"user">>) -> <<"public.user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    case binary:match(Sql, <<"count(*)">>) of
                        nomatch -> {ok, []};
                        _ -> {ok, [#{<<"total">> => 0}]}
                    end
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_repo:search(<<"nonexistent">>, 1, 20),
            ?assertEqual(0, maps:get(total, Result))
        end
    ).
