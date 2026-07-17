-module(moment_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

create_post_rejects_empty_content_and_media_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = moment_logic:create_post(1001, #{}),
        ?assertEqual({error, <<"动态内容不能为空"/utf8>>}, Result)
    end).

feed_filters_invisible_posts_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'list_feed_candidates', 2, fun(0, 100) ->
                    {ok, [
                        #{
                            <<"id">> => 11,
                            <<"author_uid">> => 3001,
                            <<"content">> => <<"visible">>,
                            <<"media">> => [],
                            <<"visibility">> => 1,
                            <<"allow_comment">> => true,
                            <<"like_count">> => 0,
                            <<"comment_count">> => 0,
                            <<"status">> => 1
                        },
                        #{
                            <<"id">> => 10,
                            <<"author_uid">> => 3002,
                            <<"content">> => <<"hidden">>,
                            <<"media">> => [],
                            <<"visibility">> => 1,
                            <<"allow_comment">> => true,
                            <<"like_count">> => 0,
                            <<"comment_count">> => 0,
                            <<"status">> => 1
                        }
                    ]}
                end},
                {'can_view_post', 2, fun(1001, Post) ->
                    maps:get(<<"id">>, Post) =:= 11
                end}
            ]}
        ],
        fun() ->
            {ok, Payload} = moment_logic:feed(1001, 0, 20),
            List = maps:get(list, Payload, []),
            ?assertEqual(1, length(List)),
            [First | _] = List,
            ?assertEqual(11, maps:get(<<"id">>, First))
        end
    ).

feed_includes_comments_preview_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'list_feed_candidates', 2, fun(0, 100) ->
                    {ok, [
                        #{
                            <<"id">> => 11,
                            <<"author_uid">> => 3001,
                            <<"content">> => <<"visible">>,
                            <<"media">> => [],
                            <<"visibility">> => 1,
                            <<"allow_comment">> => true,
                            <<"like_count">> => 0,
                            <<"comment_count">> => 2,
                            <<"status">> => 1
                        }
                    ]}
                end},
                {'can_view_post', 2, fun(1001, _Post) -> true end},
                {'liked_post_ids', 2, fun([11], 1001) -> {ok, []} end},
                {'recent_likers_by_posts', 2, fun([11], 5) -> {ok, []} end},
                {'recent_comments_by_posts', 2, fun([11], 3) ->
                    {ok, [
                        #{
                            <<"id">> => 501,
                            <<"post_id">> => 11,
                            <<"user_id">> => 4001,
                            <<"reply_to_uid">> => null,
                            <<"content">> => <<"first">>,
                            <<"created_at">> => <<"2026-07-17T00:00:00Z">>
                        },
                        #{
                            <<"id">> => 502,
                            <<"post_id">> => 11,
                            <<"user_id">> => 4002,
                            <<"reply_to_uid">> => 4001,
                            <<"content">> => <<"second">>,
                            <<"created_at">> => <<"2026-07-17T00:01:00Z">>
                        }
                    ]}
                end}
            ]},
            {user_ds, [
                {'list_by_ids', 2, fun(Ids, _Fields) ->
                    {ok, [
                        #{
                            <<"id">> => Id,
                            <<"nickname">> => integer_to_binary(Id),
                            <<"avatar">> => <<>>
                        }
                     || Id <- Ids
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, Payload} = moment_logic:feed(1001, 0, 20),
            [Post] = maps:get(list, Payload, []),
            Preview = maps:get(<<"comments_preview">>, Post),
            ?assertEqual(2, length(Preview)),
            [First, Second] = Preview,
            ?assertEqual(501, maps:get(<<"id">>, First)),
            ?assertEqual(<<"4001">>, maps:get(<<"user_nickname">>, First)),
            ?assertEqual(<<>>, maps:get(<<"reply_to_uid">>, First)),
            ?assertEqual(<<"second">>, maps:get(<<"content">>, Second)),
            ?assertEqual(4001, maps:get(<<"reply_to_uid">>, Second)),
            ?assertEqual(<<"4001">>, maps:get(<<"reply_to_nickname">>, Second))
        end
    ).

like_post_not_found_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'like_post', 2, fun(1001, 99) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"动态不存在"/utf8>>},
                moment_logic:like_post(1001, <<"99">>)
            )
        end
    ).

user_posts_invalid_uid_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, <<"用户不存在"/utf8>>},
            moment_logic:user_posts(1001, <<>>, 0, 20)
        )
    end).

user_posts_success_returns_payload_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'list_user_candidates', 3, fun(2002, 0, 100) ->
                    {ok, [
                        #{
                            <<"id">> => 77,
                            <<"author_uid">> => 2002,
                            <<"content">> => <<"only me">>,
                            <<"media">> => [],
                            <<"visibility">> => 1,
                            <<"allow_comment">> => true,
                            <<"like_count">> => 0,
                            <<"comment_count">> => 0,
                            <<"status">> => 1
                        }
                    ]}
                end},
                {'can_view_post', 2, fun(1001, _Post) -> true end}
            ]}
        ],
        fun() ->
            {ok, Payload} = moment_logic:user_posts(1001, <<"2002">>, 0, 20),
            List = maps:get(list, Payload, []),
            ?assertEqual(1, length(List)),
            ?assertEqual(20, maps:get(limit, Payload))
        end
    ).

unlike_post_not_found_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'unlike_post', 2, fun(1001, 99) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"动态不存在"/utf8>>},
                moment_logic:unlike_post(1001, <<"99">>)
            )
        end
    ).

delete_post_success_returns_ok_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'delete_post', 2, fun(1001, 99) -> ok end}
            ]},
            {moment_logic_notify, [
                {'notify_post_deleted', 3, fun(1001, 99, 1001) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, moment_logic:delete_post(1001, <<"99">>))
        end
    ).

add_comment_empty_content_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, <<"评论内容不能为空"/utf8>>},
            moment_logic:add_comment(1001, <<"99">>, <<>>, undefined)
        )
    end).

list_comments_forbidden_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'get_post', 1, fun(99) ->
                    #{
                        <<"id">> => 99,
                        <<"author_uid">> => 2001,
                        <<"content">> => <<"private">>,
                        <<"media">> => [],
                        <<"visibility">> => 1,
                        <<"allow_comment">> => true,
                        <<"like_count">> => 0,
                        <<"comment_count">> => 0,
                        <<"status">> => 1
                    }
                end},
                {'can_view_post', 2, fun(1001, _Post) -> false end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"无权限查看该动态"/utf8>>},
                moment_logic:list_comments(1001, <<"99">>, 0, 20)
            )
        end
    ).

delete_comment_success_returns_ok_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'delete_comment', 3, fun(1001, 99, 199) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, moment_logic:delete_comment(1001, <<"99">>, <<"199">>))
        end
    ).

report_post_not_found_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {moment_ds, [
                {'report_post', 4, fun(1001, 99, <<"spam">>, <<"bad">>) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"动态不存在"/utf8>>},
                moment_logic:report_post(1001, <<"99">>, <<"spam">>, <<"bad">>)
            )
        end
    ).

admin_resolve_report_invalid_result_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = moment_logic:admin_resolve_report(9001, <<"abc">>, 9, <<>>),
        ?assertEqual({error, <<"举报参数无效"/utf8>>}, Result)
    end).
