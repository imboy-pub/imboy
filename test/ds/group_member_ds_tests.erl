-module(group_member_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

list_member_all_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end}
            ]},
            {user_repo, [
                {'tablename', 0, fun() ->
                    <<"user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, [1, 50000]) ->
                    {ok, [#{<<"nickname">> => <<"user-1">>, <<"user_id">> => 100}]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, [#{<<"nickname">> => <<"user-1">>, <<"user_id">> => 100}]},
                group_member_ds:list_member(1, [])
            )
        end
    ).

list_member_filtered_uses_offset_placeholders_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end}
            ]},
            {user_repo, [
                {'tablename', 0, fun() ->
                    <<"user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [1, 100, 200]) ->
                    SqlBin = iolist_to_binary(Sql),
                    case binary:match(SqlBin, <<"gm.group_id = $1 AND gm.user_id IN ($2,$3)">>) of
                        {_, _} -> ok;
                        nomatch -> erlang:error({unexpected_sql, SqlBin})
                    end,
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, []}, group_member_ds:list_member(1, [100, 200]))
        end
    ).

join_group_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end},
                {'find', 3, fun(1, 100, <<"id">>) ->
                    #{}
                end},
                {'add', 2, fun(_Conn, Data) ->
                    ?assertEqual(1, maps:get(group_id, Data)),
                    ?assertEqual(100, maps:get(user_id, Data)),
                    {ok, 1}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() ->
                    <<"2026-03-16T00:00:00Z">>
                end}
            ]},
            {elib_str, [
                {'trunc', 2, fun(Value, 100) ->
                    Value
                end}
            ]},
            {elib_pg, [
                {'query', 3, fun(_Conn, _Sql, [1]) ->
                    {ok, [#{<<"user_id_sum">> => 100, <<"member_count">> => 1}]}
                end},
                {'update', 5, fun(_Conn, <<"group">>, Data, <<"id = $1">>, [1]) ->
                    ?assertEqual(100, maps:get(user_id_sum, Data)),
                    ?assertEqual(1, maps:get(member_count, Data)),
                    {ok, 1}
                end}
            ]},
            {group_repo, [
                {'tablename', 0, fun() ->
                    <<"group">>
                end}
            ]},
            {group_ds, [
                {'join', 2, fun(100, 1) ->
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 100}, group_member_ds:join_group(self(), <<"invite">>, 100, 1, #{role => 1})
            )
        end
    ).

join_group_already_member_returns_zero_test_() ->
    ?WITH_MECK(
        group_member_repo,
        [
            {'tablename', 0, fun() ->
                <<"group_member">>
            end},
            {'find', 3, fun(1, 100, <<"id">>) ->
                #{<<"id">> => 1}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 0}, group_member_ds:join_group(self(), <<"invite">>, 100, 1, #{}))
        end
    ).

leave_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end},
                {'find', 3, fun(1, 100, <<"*">>) ->
                    #{<<"id">> => 1, <<"group_id">> => 1, <<"user_id">> => 100, <<"role">> => 2}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() ->
                    <<"2026-03-16T00:00:00Z">>
                end}
            ]},
            {elib_pg, [
                {'execute', 3, fun(_Conn, _Sql, [1]) ->
                    {ok, 1}
                end},
                {'query', 3, fun(_Conn, _Sql, [1]) ->
                    {ok, [#{<<"user_id_sum">> => 200, <<"member_count">> => 2}]}
                end},
                {'update', 5, fun(_Conn, <<"group">>, Data, <<"id = $1">>, [1]) ->
                    ?assertEqual(200, maps:get(user_id_sum, Data)),
                    ?assertEqual(2, maps:get(member_count, Data)),
                    {ok, 1}
                end}
            ]},
            {group_repo, [
                {'tablename', 0, fun() ->
                    <<"group">>
                end}
            ]},
            {jsone_encode, [
                {'encode', 2, fun(_Data, [native_utf8]) ->
                    {ok, <<"{}">>}
                end}
            ]},
            {group_log_repo, [
                {'add', 2, fun(_Conn, LogData) ->
                    ?assertEqual(200, maps:get(type, LogData)),
                    {ok, 1, #{}}
                end}
            ]}
        ],
        fun() ->
            {ok, 200, GM} = group_member_ds:leave(self(), 100, 1, 100),
            ?assertEqual(100, maps:get(<<"user_id">>, GM))
        end
    ).

alias_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() ->
                    <<"2026-03-16T00:00:00Z">>
                end}
            ]},
            {elib_pg, [
                {'update', 4, fun(
                    <<"group_member">>, Data, <<"group_id = $1 AND user_id = $2">>, [1, 100]
                ) ->
                    ?assertEqual(<<"alias">>, maps:get(alias, Data)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_member_ds:alias(100, 1, <<"alias">>, <<"desc">>))
        end
    ).

get_member_info_not_found_test_() ->
    ?WITH_MECK(
        group_member_repo,
        [
            {'find', 3, fun(1, 100, <<"*">>) ->
                #{}
            end}
        ],
        fun() ->
            ?assertEqual({error, not_found}, group_member_ds:get_member_info(1, 100, <<"*">>))
        end
    ).

check_admin_true_test_() ->
    ?WITH_MECK(
        group_member_repo,
        [
            {'find', 3, fun(1, 100, <<"role">>) ->
                #{<<"role">> => 3}
            end}
        ],
        fun() ->
            ?assertEqual(true, group_member_ds:check_admin(100, 1))
        end
    ).
