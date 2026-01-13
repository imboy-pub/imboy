-module(user_tag_relation_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_tag_relation_ds 模块测试
remove_success_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
        {'delete', 3, fun(_Scene, _Uid, _ObjectId) -> {ok, 1} end}
    ], fun() ->
        Result = user_tag_relation_ds:remove(100, 1, 1, 1),
        ?assertEqual({ok, 1}, Result)
    end).

save_user_tag_relation_success_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
        {'save_user_tag_relation', 6, fun(_Conn, _Scene, _Uid, _TagId, _ObjectId, _CreatedAt) -> 1 end}
    ], fun() ->
        Result = user_tag_relation_ds:save_user_tag_relation(self(), 1, 100, 1, 1, <<"2023-01-01T00:00:00Z">>),
        ?assertEqual(1, Result)
    end).

flush_subtitle_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
        {'flush_subtitle', 1, fun(_TagId) -> ok end}
    ], fun() ->
        Result = user_tag_relation_ds:flush_subtitle(1),
        ?assertEqual(ok, Result)
    end).

tag_subtitle_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
            {'tag_subtitle', 3, fun(_Scene, _TagId, _Count) -> <<"副标题"/utf8>> end}
        ], fun() ->
        Result = user_tag_relation_ds:tag_subtitle(1, 1, 0),
        ?assertEqual(<<"副标题"/utf8>>, Result)
    end).
