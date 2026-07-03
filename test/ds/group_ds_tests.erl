-module(group_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_ds 的纯单元测试
%%%
%%% 当前实现主要是缓存协调层：
%%% - is_member/2 直接查 repo
%%% - member_uids/1 带缓存读写
%%% - join/2 / leave/2 / dissolve/1 只维护缓存
%%% - check_avatar/1 接收 map，返回 map
%%%===================================================================

is_member_returns_true_when_repo_finds_member_test_() ->
    ?WITH_MECK(
        group_member_repo,
        [
            {'find', 3, fun(1, 100, <<"id">>) ->
                #{<<"id">> => 1}
            end}
        ],
        fun() ->
            ?assertEqual(true, group_ds:is_member(100, 1))
        end
    ).

is_member_returns_false_when_repo_is_empty_test_() ->
    ?WITH_MECK(
        group_member_repo,
        [
            {'find', 3, fun(999999, 999999, <<"id">>) ->
                #{}
            end}
        ],
        fun() ->
            ?assertEqual(false, group_ds:is_member(999999, 999999))
        end
    ).

member_uids_reads_repo_and_populates_cache_on_miss_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun({group, 1}) -> undefined end},
                {'set', 3, fun({group, 1}, [11, 12], Ttl) ->
                    ?assert(is_integer(Ttl)),
                    ok
                end}
            ]},
            {group_member_repo, [
                {'list_by_gid', 2, fun(1, <<"user_id">>) ->
                    {ok, [
                        #{<<"user_id">> => 11},
                        #{<<"user_id">> => 12}
                    ]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual([11, 12], group_ds:member_uids(1))
        end
    ).

member_uids_uses_cache_on_hit_test_() ->
    ?WITH_MECK(
        imboy_cache,
        [
            {'get', 1, fun({group, 2}) -> {ok, [21, 22]} end}
        ],
        fun() ->
            ?assertEqual([21, 22], group_ds:member_uids(2))
        end
    ).

member_uids_returns_empty_list_on_repo_error_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun({group, 3}) -> undefined end}
            ]},
            {group_member_repo, [
                {'list_by_gid', 2, fun(3, <<"user_id">>) ->
                    {error, db_error}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual([], group_ds:member_uids(3))
        end
    ).

check_avatar_sets_default_when_avatar_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_ds:check_avatar(#{
            <<"id">> => 1,
            <<"avatar">> => <<>>
        }),
        ?assertEqual(
            <<"/static/image/group_default_avatar.jpeg">>,
            maps:get(<<"avatar">>, Result)
        )
    end).

check_avatar_preserves_existing_avatar_test_() ->
    ?TEST_SIMPLE(fun() ->
        Avatar = <<"https://example.com/avatar.jpg">>,
        Result = group_ds:check_avatar(#{
            <<"id">> => 1,
            <<"avatar">> => Avatar
        }),
        ?assertEqual(Avatar, maps:get(<<"avatar">>, Result))
    end).

check_avatar_returns_empty_map_for_non_map_input_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(#{}, group_ds:check_avatar(<<"not-a-map">>))
    end).

join_adds_uid_to_cached_member_list_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun({group, 10}) -> {ok, [2, 3]} end},
                {'set', 3, fun({group, 10}, [1, 2, 3], Ttl) ->
                    ?assert(is_integer(Ttl)),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_ds:join(1, 10))
        end
    ).

join_is_idempotent_when_uid_already_exists_test_() ->
    ?WITH_MECK(
        imboy_cache,
        [
            {'get', 1, fun({group, 11}) -> {ok, [1, 2, 3]} end}
        ],
        fun() ->
            ?assertEqual(ok, group_ds:join(1, 11))
        end
    ).

leave_removes_uid_from_cache_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun({group, 12}) -> {ok, [1, 2, 3]} end},
                {'set', 3, fun({group, 12}, [2, 3], 3600) ->
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_ds:leave(1, 12))
        end
    ).

leave_is_noop_when_group_has_no_members_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun({group, 13}) -> undefined end}
            ]},
            {group_member_repo, [
                {'list_by_gid', 2, fun(13, <<"user_id">>) ->
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_ds:leave(1, 13))
        end
    ).

dissolve_flushes_group_cache_test_() ->
    ?WITH_MECK(
        imboy_cache,
        [
            {'flush', 1, fun({group, 14}) ->
                ok
            end}
        ],
        fun() ->
            ?assertEqual(ok, group_ds:dissolve(14))
        end
    ).

%%%===================================================================
%%% load_aggregate / save_aggregate（T2.2：group_agg 聚合根 DS 接线）
%%% 经 group_repo（mock）验证字段映射与 status 码↔atom 转换，零 DB 依赖。
%%%===================================================================

load_aggregate_rehydrates_active_group_test_() ->
    ?WITH_MECK(
        group_repo,
        [
            {'find_by_id', 2, fun(1, _Col) ->
                #{
                    <<"id">> => 1,
                    <<"owner_uid">> => 100,
                    <<"member_count">> => 5,
                    <<"type">> => 1,
                    <<"status">> => 1
                }
            end}
        ],
        fun() ->
            {ok, Agg} = group_ds:load_aggregate(1),
            ?assertEqual(1, group_agg:id(Agg)),
            ?assertEqual(100, group_agg:owner(Agg)),
            ?assertEqual(5, group_agg:member_count(Agg)),
            ?assertEqual(active, group_agg:status(Agg))
        end
    ).

load_aggregate_maps_dissolved_status_test_() ->
    ?WITH_MECK(
        group_repo,
        [
            {'find_by_id', 2, fun(2, _Col) ->
                #{
                    <<"id">> => 2,
                    <<"owner_uid">> => 100,
                    <<"member_count">> => 0,
                    <<"type">> => 1,
                    <<"status">> => 0
                }
            end}
        ],
        fun() ->
            {ok, Agg} = group_ds:load_aggregate(2),
            ?assertEqual(dissolved, group_agg:status(Agg))
        end
    ).

load_aggregate_returns_error_when_not_found_test_() ->
    ?WITH_MECK(
        group_repo,
        [
            {'find_by_id', 2, fun(999, _Col) -> #{} end}
        ],
        fun() ->
            ?assertEqual({error, group_not_found}, group_ds:load_aggregate(999))
        end
    ).

save_aggregate_persists_mapped_active_row_test_() ->
    ?WITH_MECK(
        group_repo,
        [
            {'update', 1, fun(Data) ->
                ?assertEqual(3, maps:get(<<"id">>, Data)),
                ?assertEqual(100, maps:get(<<"owner_uid">>, Data)),
                ?assertEqual(7, maps:get(<<"member_count">>, Data)),
                ?assertEqual(1, maps:get(<<"status">>, Data)),
                {ok, 1}
            end}
        ],
        fun() ->
            Agg = group_agg:rehydrate(#{
                <<"group_id">> => 3,
                <<"owner">> => 100,
                <<"member_count">> => 7,
                <<"type">> => 1
            }),
            ?assertEqual({ok, 1}, group_ds:save_aggregate(Agg))
        end
    ).

save_aggregate_maps_dissolved_to_status_zero_test_() ->
    ?WITH_MECK(
        group_repo,
        [
            {'update', 1, fun(Data) ->
                ?assertEqual(0, maps:get(<<"status">>, Data)),
                {ok, 1}
            end}
        ],
        fun() ->
            Agg0 = group_agg:rehydrate(#{
                <<"group_id">> => 4,
                <<"owner">> => 100,
                <<"member_count">> => 2,
                <<"type">> => 1
            }),
            {ok, Agg1, _} = group_agg:dissolve(Agg0),
            ?assertEqual({ok, 1}, group_ds:save_aggregate(Agg1))
        end
    ).
