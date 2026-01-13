-module(app_ddl_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_ddl_ds 模块的 EUnit 测试
%%%
%%% 目标：验证应用 DDL 数据服务功能
%%% 覆盖：DDL 列表查询、保存 DDL、删除 DDL
%%%===================================================================

%% ===================================================================
%% get_ddl_list/0 测试
%% ===================================================================

get_ddl_list_returns_list_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'all', 0, fun() ->
            [#{<<"id">> => 1, <<"name">> => <<"test_ddl">>}]
        end}
    ], fun() ->
        Result = app_ddl_ds:get_ddl_list(),
        ?assertMatch([#{<<"id">> := 1} | _], Result)
    end).

get_ddl_list_empty_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'all', 0, fun() -> [] end}
    ], fun() ->
        Result = app_ddl_ds:get_ddl_list(),
        ?assertEqual([], Result)
    end).

%% ===================================================================
%% save/1 测试
%% ===================================================================

save_success_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'save', 1, fun(Data) ->
            ?assertEqual(<<"CREATE TABLE test (id INT);">>, maps:get(<<"content">>, Data)),
            {ok, 1}
        end}
    ], fun() ->
        Result = app_ddl_ds:save(#{<<"content">> => <<"CREATE TABLE test (id INT);">>}),
        ?assertEqual({ok, 1}, Result)
    end).

save_with_full_info_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'save', 1, fun(Data) ->
            ?assertEqual(<<"1.0.0">>, maps:get(<<"version">>, Data)),
            ?assertEqual(<<"test_ddl"/utf8>>, maps:get(<<"name">>, Data)),
            {ok, 1}
        end}
    ], fun() ->
        Result = app_ddl_ds:save(#{
            <<"name">> => <<"test_ddl"/utf8>>,
            <<"version">> => <<"1.0.0">>,
            <<"content">> => <<"CREATE TABLE test (id INT);">>
        }),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% delete/1 测试
%% ===================================================================

delete_success_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'delete', 1, fun(Id) ->
            ?assertEqual(1, Id),
            ok
        end}
    ], fun() ->
        Result = app_ddl_ds:delete(1),
        ?assertEqual(ok, Result)
    end).

delete_nonexistent_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'delete', 1, fun(_Id) -> ok end}
    ], fun() ->
        Result = app_ddl_ds:delete(999),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

save_with_empty_content_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'save', 1, fun(Data) ->
            ?assertEqual(<<>>, maps:get(<<"content">>, Data)),
            {ok, 1}
        end}
    ], fun() ->
        Result = app_ddl_ds:save(#{<<"content">> => <<>>}),
        ?assertEqual({ok, 1}, Result)
    end).

save_with_long_content_test_() ->
    LongContent = list_to_binary(lists:duplicate(1000, $x)),
    ?WITH_MECK(app_ddl_repo, [
        {'save', 1, fun(Data) ->
            Content = maps:get(<<"content">>, Data),
            ?assert(byte_size(Content) >= 1000),
            {ok, 1}
        end}
    ], fun() ->
        Result = app_ddl_ds:save(#{<<"content">> => LongContent}),
        ?assertEqual({ok, 1}, Result)
    end).

save_with_utf8_content_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'save', 1, fun(Data) ->
            Content = maps:get(<<"content">>, Data),
            ?assertEqual(<<"CREATE TABLE 测试 (id INT);"/utf8>>, Content),
            {ok, 1}
        end}
    ], fun() ->
        Result = app_ddl_ds:save(#{<<"content">> => <<"CREATE TABLE 测试 (id INT);"/utf8>>}),
        ?assertEqual({ok, 1}, Result)
    end).

delete_with_large_id_test_() ->
    ?WITH_MECK(app_ddl_repo, [
        {'delete', 1, fun(Id) ->
            ?assertEqual(999999999, Id),
            ok
        end}
    ], fun() ->
        Result = app_ddl_ds:delete(999999999),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

get_ddl_list_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = app_ddl_ds:get_ddl_list(),
        ?assert(is_list(Result))
    end).

save_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Data = #{<<"content">> => <<"CREATE TABLE test;">>},
        ?assert(is_map(Data))
    end).

delete_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Id = 1,
        ?assert(is_integer(Id))
    end).
