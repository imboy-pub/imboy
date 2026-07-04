-module(moderation_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc moderation_ds 的 EUnit 测试
%%% 覆盖：复审队列 hit_words 逗号文本 -> 字符串数组的整形
%%%===================================================================

review_page_splits_hit_words_test_() ->
    ?WITH_MECKS(
        [
            {review_queue_repo, [
                {'page', 3, fun(_Filter, Page, Size) ->
                    {ok, #{
                        page => Page,
                        size => Size,
                        total => 2,
                        total_pages => 1,
                        list => [
                            #{<<"id">> => 1, <<"hit_words">> => <<"aa,bb,cc">>},
                            #{<<"id">> => 2, <<"hit_words">> => <<>>}
                        ]
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, P} = moderation_ds:review_page(1, 10, #{}),
            [R1, R2] = maps:get(list, P),
            ?assertEqual([<<"aa">>, <<"bb">>, <<"cc">>], maps:get(<<"hit_words">>, R1)),
            ?assertEqual([], maps:get(<<"hit_words">>, R2))
        end
    ).

review_page_propagates_error_test_() ->
    ?WITH_MECKS(
        [
            {review_queue_repo, [
                {'page', 3, fun(_, _, _) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, db_down}, moderation_ds:review_page(1, 10, #{}))
        end
    ).
