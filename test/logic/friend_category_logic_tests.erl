-module(friend_category_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc friend_category_logic 模块测试
save_success_test_() ->
    ?WITH_MECK(friend_category_ds, [
        {'add', 2, fun(_Uid, _Name) -> {ok, 1} end}
    ], fun() ->
        Result = friend_category_logic:save(#{<<"name">> => <<"同学"/utf8>>, <<"user_id">> => 100}),
        ?assertEqual({ok, 1}, Result)
    end).

delete_success_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'set_category_id', 3, fun(_Uid, _FromId, _ToId) -> {ok, 1} end}
        ]},
        {friend_category_ds, [
            {'delete', 2, fun(_Uid, _Id) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = friend_category_logic:delete(100, 1),
        ?assertEqual({ok, 1}, Result)
    end).
