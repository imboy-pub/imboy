-module(group_category_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_category_ds 的纯单元测试
%%%
%%% 当前实现已经是 service 层薄封装，这里只验证：
%%% - repo 返回值映射
%%% - 默认分类补齐
%%% - 参数校验和删除前迁移逻辑
%%%===================================================================

add_returns_existing_category_id_test_() ->
    ?WITH_MECK(
        group_category_repo,
        [
            {'find_by_name', 2, fun(100, <<"工作群"/utf8>>) ->
                {ok, #{<<"id">> => 10}}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 10}, group_category_ds:add(100, <<"工作群"/utf8>>))
        end
    ).

add_inserts_when_category_missing_test_() ->
    ?WITH_MECKS(
        [
            {group_category_repo, [
                {'find_by_name', 2, fun(100, <<"新分类"/utf8>>) ->
                    {ok, #{}}
                end},
                {'add', 2, fun(100, <<"新分类"/utf8>>) ->
                    {ok, 11}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 11}, group_category_ds:add(100, <<"新分类"/utf8>>))
        end
    ).

add_propagates_lookup_error_test_() ->
    ?WITH_MECK(
        group_category_repo,
        [
            {'find_by_name', 2, fun(_Uid, _Name) ->
                {error, db_error}
            end}
        ],
        fun() ->
            ?assertEqual({error, db_error}, group_category_ds:add(100, <<"异常"/utf8>>))
        end
    ).

find_by_uid_prepends_default_category_test_() ->
    ?WITH_MECK(
        group_category_repo,
        [
            %% find_by_uid/1 先聚合各分类群数量再列分类，须一并 mock 否则打真 elib_pg
            {'count_groups_grouped_by_category', 1, fun(100) -> {ok, []} end},
            {'list_by_uid', 2, fun(100, <<"id, category_name, sort_order">>) ->
                {ok, [
                    #{
                        <<"id">> => 1,
                        <<"category_name">> => <<"工作"/utf8>>,
                        <<"sort_order">> => 10
                    }
                ]}
            end}
        ],
        fun() ->
            [Default, Custom] = group_category_ds:find_by_uid(100),
            ?assertEqual(0, maps:get(<<"id">>, Default)),
            ?assertEqual(<<"未分类"/utf8>>, maps:get(<<"category_name">>, Default)),
            ?assertEqual(1, maps:get(<<"id">>, Custom))
        end
    ).

find_by_uid_returns_default_category_on_repo_error_test_() ->
    ?WITH_MECK(
        group_category_repo,
        [
            {'count_groups_grouped_by_category', 1, fun(_Uid) -> {ok, []} end},
            {'list_by_uid', 2, fun(_Uid, _Field) ->
                {error, db_error}
            end}
        ],
        fun() ->
            [Default] = group_category_ds:find_by_uid(100),
            ?assertEqual(0, maps:get(<<"id">>, Default)),
            ?assertEqual(<<"未分类"/utf8>>, maps:get(<<"category_name">>, Default))
        end
    ).

rename_success_test_() ->
    ?WITH_MECK(
        group_category_repo,
        [
            {'update_name', 3, fun(100, 9, <<"新名称"/utf8>>) ->
                {ok, 1}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 1}, group_category_ds:rename(100, 9, <<"新名称"/utf8>>))
        end
    ).

rename_invalid_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch({error, _}, group_category_ds:rename(100, undefined, <<"新名称"/utf8>>)),
        ?assertMatch({error, _}, group_category_ds:rename(100, 1, <<>>))
    end).

delete_moves_groups_to_default_before_delete_test_() ->
    ?WITH_MECK(
        group_category_repo,
        [
            {'list_groups_by_category', 3, fun(100, 9, <<"gm.group_id">>) ->
                {ok, [
                    #{<<"group_id">> => 2001},
                    #{<<"group_id">> => 2002}
                ]}
            end},
            {'update_group_category', 3, fun(100, Gid, 0) ->
                self() ! {moved_to_default, Gid},
                {ok, 1}
            end},
            {'delete', 2, fun(100, 9) ->
                {ok, 1}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 1}, group_category_ds:delete(100, 9)),
            Moved = lists:sort(collect_messages([])),
            ?assertEqual([{moved_to_default, 2001}, {moved_to_default, 2002}], Moved)
        end
    ).

delete_invalid_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch({error, _}, group_category_ds:delete(100, undefined)),
        ?assertMatch({error, _}, group_category_ds:delete(100, <<>>))
    end).

update_sort_order_success_test_() ->
    ?WITH_MECK(
        group_category_repo,
        [
            {'update_sort_order', 3, fun(100, 9, 88) ->
                {ok, 1}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 1}, group_category_ds:update_sort_order(100, 9, 88))
        end
    ).

move_group_to_category_returns_updated_count_test_() ->
    ?WITH_MECK(
        group_category_repo,
        [
            {'update_group_category', 3, fun(100, 2001, 9) ->
                {ok, 1}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 1}, group_category_ds:move_group_to_category(100, 2001, 9))
        end
    ).

collect_messages(Acc) ->
    receive
        Msg ->
            collect_messages([Msg | Acc])
    after 0 ->
        Acc
    end.
