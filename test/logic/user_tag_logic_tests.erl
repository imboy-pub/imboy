-module(user_tag_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_tag_logic 模块测试
save_tag_success_test_() ->
    ?WITH_MECK(
        user_tag_ds,
        [
            {'add', 3, fun(_Uid, _Scene, _Tag) -> {ok, 1} end}
        ],
        fun() ->
            Result = user_tag_logic:save(100, 1, <<"标签1"/utf8>>),
            ?assertEqual({ok, 1}, Result)
        end
    ).

delete_tag_success_test_() ->
    ?WITH_MECKS(
        [
            {user_tag_ds, [
                {'find_tag_id', 3, fun(_Uid, _Scene, _Tag) -> {ok, 1} end},
                {'delete', 3, fun(_Uid, _Scene, _Tag) -> ok end}
            ]},
            {user_tag_relation_ds, [
                {'delete_by_tag_id', 1, fun(_TagId) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = user_tag_logic:delete(100, 1, <<"标签1"/utf8>>),
            ?assertEqual(ok, Result),
            ?assertEqual(0, meck:num_calls(user_tag_relation_ds, delete_by_tag_id, 1))
        end
    ).

delete_tag_error_is_propagated_test_() ->
    ?WITH_MECK(
        user_tag_ds,
        [
            {'delete', 3, fun(_, _, _) -> {error, aborted} end}
        ],
        fun() ->
            ?assertEqual({error, aborted}, user_tag_logic:delete(100, 1, <<"tag">>))
        end
    ).
