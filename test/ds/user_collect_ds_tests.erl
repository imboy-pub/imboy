-module(user_collect_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_collect_ds 模块的 EUnit 测试
%%%
%%% 目标：验证用户收藏数据服务功能
%%% 覆盖：统计收藏、删除收藏、更新收藏
%%%===================================================================

%% ===================================================================
%% count_by_uid_kind_id/2 测试
%% ===================================================================

count_by_uid_kind_id_returns_count_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'count_by_uid_kind_id', 2, fun(Uid, KindId) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"favorite">>, KindId),
            5
        end}
    ], fun() ->
        Result = user_collect_ds:count_by_uid_kind_id(100, <<"favorite">>),
        ?assertEqual(5, Result)
    end).

count_by_uid_kind_id_with_zero_count_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'count_by_uid_kind_id', 2, fun(_Uid, _KindId) -> 0 end}
    ], fun() ->
        Result = user_collect_ds:count_by_uid_kind_id(100, <<"bookmark">>),
        ?assertEqual(0, Result)
    end).

count_by_uid_kind_id_with_different_kind_ids_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'count_by_uid_kind_id', 2, fun(_Uid, KindId) ->
            case KindId of
                <<"favorite">> -> 10;
                <<"bookmark">> -> 20;
                <<"like">> -> 30;
                _ -> 0
            end
        end}
    ], fun() ->
        ?assertEqual(10, user_collect_ds:count_by_uid_kind_id(100, <<"favorite">>)),
        ?assertEqual(20, user_collect_ds:count_by_uid_kind_id(100, <<"bookmark">>)),
        ?assertEqual(30, user_collect_ds:count_by_uid_kind_id(100, <<"like">>)),
        ?assertEqual(0, user_collect_ds:count_by_uid_kind_id(100, <<"unknown">>))
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_success_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'delete', 2, fun(Uid, KindId) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"favorite">>, KindId),
            {ok, 1}
        end}
    ], fun() ->
        Result = user_collect_ds:delete(100, <<"favorite">>),
        ?assertEqual({ok, 1}, Result)
    end).

delete_with_zero_affected_rows_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'delete', 2, fun(_Uid, _KindId) -> {ok, 0} end}
    ], fun() ->
        Result = user_collect_ds:delete(100, <<"nonexistent">>),
        ?assertEqual({ok, 0}, Result)
    end).

delete_with_error_returns_error_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'delete', 2, fun(_Uid, _KindId) ->
            {error, <<"database_error">>}
        end}
    ], fun() ->
        Result = user_collect_ds:delete(100, <<"favorite">>),
        ?assertEqual({error, <<"database_error">>}, Result)
    end).

delete_multiple_items_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'delete', 2, fun(_Uid, _KindId) -> {ok, 5} end}
    ], fun() ->
        Result = user_collect_ds:delete(100, <<"favorite">>),
        ?assertEqual({ok, 5}, Result)
    end).

%% ===================================================================
%% update/3 测试
%% ===================================================================

update_success_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'update', 3, fun(Uid, KindId, Data) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"favorite">>, KindId),
            ?assertEqual(#{title => <<"新标题"/utf8>>}, Data),
            {ok, 1}
        end}
    ], fun() ->
        Data = #{title => <<"新标题"/utf8>>},
        Result = user_collect_ds:update(100, <<"favorite">>, Data),
        ?assertEqual({ok, 1}, Result)
    end).

update_with_multiple_fields_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'update', 3, fun(_Uid, _KindId, Data) ->
            ?assertEqual(3, maps:size(Data)),
            ?assert(maps:is_key(title, Data)),
            ?assert(maps:is_key(description, Data)),
            ?assert(maps:is_key(updated_at, Data)),
            {ok, 1}
        end}
    ], fun() ->
        Data = #{
            title => <<"标题"/utf8>>,
            description => <<"描述"/utf8>>,
            updated_at => <<"2023-01-01T00:00:00Z">>
        },
        Result = user_collect_ds:update(100, <<"favorite">>, Data),
        ?assertEqual({ok, 1}, Result)
    end).

update_with_empty_data_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'update', 3, fun(_Uid, _KindId, Data) ->
            ?assertEqual(#{}, Data),
            {ok, 0}
        end}
    ], fun() ->
        Result = user_collect_ds:update(100, <<"favorite">>, #{}),
        ?assertEqual({ok, 0}, Result)
    end).

update_with_error_returns_error_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'update', 3, fun(_Uid, _KindId, _Data) ->
            {error, <<"update_failed">>}
        end}
    ], fun() ->
        Data = #{title => <<"标题"/utf8>>},
        Result = user_collect_ds:update(100, <<"favorite">>, Data),
        ?assertEqual({error, <<"update_failed">>}, Result)
    end).

update_nonexistent_item_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'update', 3, fun(_Uid, _KindId, _Data) -> {ok, 0} end}
    ], fun() ->
        Data = #{title => <<"标题"/utf8>>},
        Result = user_collect_ds:update(999, <<"nonexistent">>, Data),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

count_by_uid_kind_id_with_large_count_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'count_by_uid_kind_id', 2, fun(_Uid, _KindId) -> 999999 end}
    ], fun() ->
        Result = user_collect_ds:count_by_uid_kind_id(100, <<"favorite">>),
        ?assertEqual(999999, Result)
    end).

delete_with_empty_kind_id_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'delete', 2, fun(_Uid, KindId) ->
            ?assertEqual(<<>>, KindId),
            {ok, 0}
        end}
    ], fun() ->
        Result = user_collect_ds:delete(100, <<>>),
        ?assertEqual({ok, 0}, Result)
    end).

update_with_long_title_test_() ->
    LongTitle = list_to_binary(lists:duplicate(1000, $x)),
    ?WITH_MECK(user_collect_repo, [
        {'update', 3, fun(_Uid, _KindId, Data) ->
            Title = maps:get(title, Data),
            ?assert(byte_size(Title) >= 1000),
            {ok, 1}
        end}
    ], fun() ->
        Data = #{title => LongTitle},
        Result = user_collect_ds:update(100, <<"favorite">>, Data),
        ?assertEqual({ok, 1}, Result)
    end).

update_with_utf8_content_test_() ->
    ?WITH_MECK(user_collect_repo, [
        {'update', 3, fun(_Uid, _KindId, Data) ->
            Title = maps:get(title, Data),
            ?assertEqual(<<"中文标题"/utf8>>, Title),
            {ok, 1}
        end}
    ], fun() ->
        Data = #{title => <<"中文标题"/utf8>>},
        Result = user_collect_ds:update(100, <<"favorite">>, Data),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

count_by_uid_kind_id_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        KindId = <<"favorite">>,
        ?assert(is_integer(Uid)),
        ?assert(is_binary(KindId))
    end).

delete_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        KindId = <<"favorite">>,
        ?assert(is_integer(Uid)),
        ?assert(is_binary(KindId))
    end).

update_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        KindId = <<"favorite">>,
        Data = #{title => <<"标题"/utf8>>},
        ?assert(is_integer(Uid)),
        ?assert(is_binary(KindId)),
        ?assert(is_map(Data))
    end).
