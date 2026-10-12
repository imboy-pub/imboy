-module(eb_message_history_pagination_tests).
-include_lib("eunit/include/eunit.hrl").
-export([list_messages_after/3, fetch_conversation_session/3, fetch_visitor_message_reads/4]).

history_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            fun latest_four_pages/0,
            fun legacy_ascending/0,
            fun invalid_cursors/0,
            fun scope_is_bound/0,
            fun seat_error_status/0,
            fun cs_facade_history/0,
            fun cs_facade_invalid_cursors/0
        ]
    end}.
setup() ->
    ok = meck:new(elib_pg, [non_strict, no_link]),
    ok = meck:expect(elib_pg, query, fun query/2),
    ok.
cleanup(_) -> meck:unload(elib_pg).
list_messages_after(Org, Ws, Query) -> eb_pg_message_ext:list_messages_after(Org, Ws, Query).
params(Extra) -> maps:merge(#{workspace_id => 2, conversation_id => 3, store => ?MODULE}, Extra).
page(Extra) -> eb_message_app:list_messages(1, params(Extra)).
ids(Rows) -> [maps:get(id, R) || R <- Rows].
latest_four_pages() ->
    {ok, A} = page(#{order => <<"desc">>, limit => 50}),
    {ok, B} = page(#{order => <<"desc">>, before_id => lists:last(ids(A)), limit => 50}),
    {ok, C} = page(#{order => <<"desc">>, before_id => lists:last(ids(B)), limit => 50}),
    {ok, D} = page(#{order => <<"desc">>, before_id => lists:last(ids(C)), limit => 50}),
    ?assertEqual(lists:seq(200, 1, -1), ids(A ++ B ++ C ++ D)),
    {ok, []} = page(#{order => <<"desc">>, before_id => 1, limit => 50}).
legacy_ascending() ->
    {ok, A} = page(#{limit => 50}),
    {ok, B} = page(#{after_id => 50, limit => 50}),
    ?assertEqual(lists:seq(1, 100), ids(A ++ B)).
invalid_cursors() ->
    Calls = meck:num_calls(elib_pg, query, '_'),
    ?assertMatch({error, {invalid_message_order, _}}, page(#{order => <<"other">>})),
    ?assertMatch({error, invalid_message_cursors}, page(#{before_id => 10})),
    ?assertMatch({error, invalid_message_cursors}, page(#{order => <<"desc">>, after_id => 0})),
    ?assertMatch(
        {error, invalid_message_cursors},
        page(#{order => <<"desc">>, after_id => 10, before_id => 20})
    ),
    ?assertMatch({error, {invalid_before_id, _}}, page(#{order => <<"desc">>, before_id => 0})),
    ?assertMatch({error, {invalid_limit, _}}, page(#{order => <<"desc">>, limit => 201})),
    ?assertEqual(Calls, meck:num_calls(elib_pg, query, '_')).
scope_is_bound() ->
    ?assertEqual({ok, []}, eb_message_app:list_messages(9, params(#{order => <<"desc">>}))),
    ?assertEqual(
        {ok, []}, eb_message_app:list_messages(1, params(#{workspace_id => 9, order => <<"desc">>}))
    ),
    ?assertEqual({ok, []}, page(#{conversation_id => 9, order => <<"desc">>})).
seat_error_status() ->
    lists:foreach(
        fun(E) ->
            ?assertEqual(422, cs_http:status(E)),
            ?assertEqual(422, eb_enterprise_http:status(E))
        end,
        [invalid_message_cursors, {invalid_before_id, 0}, {invalid_message_order, <<"bad">>}]
    ).

fetch_conversation_session(1, 2, 3) ->
    {ok, #{id => 4, conversation_id => 3, contact_id => 5}}.
fetch_visitor_message_reads(1, 2, #{session_id := 4}, Ids) ->
    {ok, [#{message_id => I, read_at => <<"authorized-first-read">>} || I <- Ids]}.
cs_page(Extra) ->
    customer_service_facade:list_messages(1, (params(Extra))#{eb_store => ?MODULE}).
cs_facade_history() ->
    {ok, Latest} = cs_page(#{order => <<"desc">>, limit => 50}),
    ?assertEqual(lists:seq(200, 151, -1), ids(Latest)),
    {ok, Older} = cs_page(#{order => <<"desc">>, before_id => 151, limit => 50}),
    ?assertEqual(lists:seq(150, 101, -1), ids(Older)),
    ?assert(
        lists:all(
            fun(R) -> maps:get(read_at, R) =:= <<"authorized-first-read">> end,
            Latest ++ Older
        )
    ).
cs_facade_invalid_cursors() ->
    ?assertEqual(
        {error, invalid_message_cursors},
        cs_page(#{order => <<"desc">>, after_id => 0, before_id => 151})
    ),
    ?assertEqual({error, invalid_message_cursors}, cs_page(#{before_id => 151})),
    ?assertEqual(
        {error, {invalid_message_order, <<"unknown">>}},
        cs_page(#{order => <<"unknown">>})
    ).

query(Sql, [Org, Ws, Conv, Cursor, Limit]) ->
    ?assertNotEqual(
        nomatch,
        binary:match(Sql, <<"organization_id = $1 AND workspace_id = $2 AND conversation_id = $3">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"w.organization_id = $1 AND w.id = $2">>)),
    Desc = binary:match(Sql, <<"ORDER BY id DESC">>) =/= nomatch,
    All =
        case {Org, Ws, Conv} of
            {1, 2, 3} -> lists:seq(1, 200);
            _ -> []
        end,
    Selected =
        case Desc of
            true ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"id < $4">>)),
                lists:reverse([I || I <- All, Cursor =:= null orelse I < Cursor]);
            false ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"id > $4">>)),
                [I || I <- All, I > Cursor]
        end,
    {ok, [
        #{
            <<"id">> => I,
            <<"sender_type">> => <<"business_identity">>,
            <<"visibility">> => <<"visible">>
        }
     || I <- lists:sublist(Selected, Limit)
    ]};
query(_Sql, [_Org, _Ws, _MessageIds]) ->
    {ok, []}.
