%% teaching_review_logic_tests — v3 N5（P1-3 补测）：
%% review_has_content/2 三态口径（与前端 canPublish 对齐）：
%%   文本 → true；纯 feedback_image（无文本/无视频旧列）→ true；全空 → false。
%% 纯函数无 DB 依赖，纳入全量 eunit。
-module(teaching_review_logic_tests).

-include_lib("eunit/include/eunit.hrl").

empty_draft() ->
    #{
        <<"positive_point">> => <<>>,
        <<"focus_problem">> => <<>>,
        <<"practice_action">> => <<>>,
        <<"comment">> => <<>>,
        <<"video_attachment_id">> => null
    }.

has_content_text_test_() ->
    [
        ?_assertEqual(
            true,
            teaching_review_logic:review_has_content(
                (empty_draft())#{<<"positive_point">> => <<"横画起笔稳"/utf8>>},
                []
            )
        )
    ].

has_content_video_legacy_test_() ->
    [
        ?_assertEqual(
            true,
            teaching_review_logic:review_has_content(
                (empty_draft())#{<<"video_attachment_id">> => 988001},
                []
            )
        )
    ].

%% P1-3 修复主断言：纯图片（四要素全空 + 旧列 null + 1 张 feedback_image）可发布
has_content_image_only_test_() ->
    [
        ?_assertEqual(
            true,
            teaching_review_logic:review_has_content(empty_draft(), [
                #{<<"kind">> => <<"feedback_image">>, <<"attachment_id">> => 988002}
            ])
        )
    ].

has_content_empty_test_() ->
    [?_assertEqual(false, teaching_review_logic:review_has_content(empty_draft(), []))].

%% PublishedReview 老师署名（reviewer_display_name）：白名单构造直通，未知为 null 不转空串
payload_reviewer_name_present_test_() ->
    Pub = #{
        <<"id">> => 42,
        <<"positive_point">> => <<"横画起笔稳"/utf8>>,
        <<"reviewer_uid">> => 7
    },
    R = teaching_review_logic:published_review_payload(Pub, [], <<"李老师"/utf8>>),
    [
        ?_assertEqual(<<"李老师"/utf8>>, maps:get(<<"reviewer_display_name">>, R)),
        ?_assertEqual(null, maps:get(<<"video_attachment_id">>, R))
    ].

payload_reviewer_name_unknown_test_() ->
    R = teaching_review_logic:published_review_payload(#{<<"id">> => 42}, [], null),
    [?_assertEqual(null, maps:get(<<"reviewer_display_name">>, R))].

payload_undefined_review_test_() ->
    [?_assertEqual(null, teaching_review_logic:published_review_payload(undefined, [], null))].

%% ---- 逐字点评字卡（char_reviews，Phase A）：parse 白名单 / payload 读侧解码 ----

parse_char_missing_key_test_() ->
    [
        ?_assertEqual(
            {ok, null},
            teaching_review_logic:parse_char_reviews(#{
                <<"positive_point">> => <<"横画起笔稳"/utf8>>
            })
        )
    ].

parse_char_null_test_() ->
    [
        ?_assertEqual(
            {ok, null},
            teaching_review_logic:parse_char_reviews(#{
                <<"char_reviews">> => null
            })
        )
    ].

parse_char_not_list_test_() ->
    [
        ?_assertEqual(
            {error, char_reviews_invalid},
            teaching_review_logic:parse_char_reviews(#{<<"char_reviews">> => <<>>})
        ),
        ?_assertEqual(
            {error, char_reviews_invalid},
            teaching_review_logic:parse_char_reviews(#{<<"char_reviews">> => 42})
        ),
        ?_assertEqual(
            {error, char_reviews_invalid},
            teaching_review_logic:parse_char_reviews(<<"not a map">>)
        )
    ].

parse_char_valid_item_strips_extras_test_() ->
    {ok, [Item]} = teaching_review_logic:parse_char_reviews(#{
        <<"char_reviews">> => [
            #{
                <<"index">> => 0,
                <<"char">> => <<"人"/utf8>>,
                <<"grade">> => <<"good">>,
                <<"comment">> => <<"起笔藏锋到位"/utf8>>,
                <<"hacked">> => <<"drop me"/utf8>>
            }
        ]
    }),
    [
        ?_assertEqual(
            #{
                <<"index">> => 0,
                <<"char">> => <<"人"/utf8>>,
                <<"grade">> => <<"good">>,
                <<"comment">> => <<"起笔藏锋到位"/utf8>>
            },
            Item
        )
    ].

parse_char_comment_optional_test_() ->
    {ok, [Item]} = teaching_review_logic:parse_char_reviews(#{
        <<"char_reviews">> => [
            #{<<"index">> => 1, <<"char">> => <<"口"/utf8>>, <<"grade">> => <<"poor">>}
        ]
    }),
    [?_assertEqual(<<>>, maps:get(<<"comment">>, Item))].

parse_char_invalid_items_dropped_test_() ->
    %% 单项越界丢弃不整体拒绝（AI 输出容错）：grade 非法 / 负 index / char 超长 /
    %% char 空串 / comment 超长 / 项非 map，只剩 1 个合法项
    {ok, Kept} = teaching_review_logic:parse_char_reviews(#{
        <<"char_reviews">> => [
            #{<<"index">> => 0, <<"char">> => <<"人"/utf8>>, <<"grade">> => <<"excellent">>},
            #{<<"index">> => -1, <<"char">> => <<"人"/utf8>>, <<"grade">> => <<"good">>},
            #{<<"index">> => 1, <<"char">> => <<"123456789">>, <<"grade">> => <<"good">>},
            #{<<"index">> => 2, <<"char">> => <<>>, <<"grade">> => <<"good">>},
            #{
                <<"index">> => 3,
                <<"char">> => <<"人"/utf8>>,
                <<"grade">> => <<"fair">>,
                <<"comment">> => binary:copy(<<"a">>, 301)
            },
            <<"not a map">>,
            #{<<"index">> => 4, <<"char">> => <<"口"/utf8>>, <<"grade">> => <<"fair">>}
        ]
    }),
    [
        ?_assertEqual(1, length(Kept)),
        ?_assertEqual(4, maps:get(<<"index">>, hd(Kept)))
    ].

parse_char_empty_all_dropped_normalizes_null_test_() ->
    [
        ?_assertEqual(
            {ok, null},
            teaching_review_logic:parse_char_reviews(#{
                <<"char_reviews">> => []
            })
        ),
        ?_assertEqual(
            {ok, null},
            teaching_review_logic:parse_char_reviews(#{
                <<"char_reviews">> => [
                    #{<<"index">> => 0, <<"char">> => <<"人"/utf8>>, <<"grade">> => <<"bad">>}
                ]
            })
        )
    ].

parse_char_max_50_test_() ->
    {ok, Kept} = teaching_review_logic:parse_char_reviews(#{
        <<"char_reviews">> => [
            #{<<"index">> => I, <<"char">> => <<"人"/utf8>>, <<"grade">> => <<"good">>}
         || I <- lists:seq(0, 51)
        ]
    }),
    [?_assertEqual(50, length(Kept))].

%% char_reviews_payload：jsonb 读侧（elib_pg 无 json codec → JSON 文本）
char_payload_decode_test_() ->
    Bin = jsone:encode([
        #{
            <<"index">> => 0,
            <<"char">> => <<"人"/utf8>>,
            <<"grade">> => <<"good">>,
            <<"comment">> => <<>>
        }
    ]),
    [
        ?_assertEqual(
            [
                #{
                    <<"index">> => 0,
                    <<"char">> => <<"人"/utf8>>,
                    <<"grade">> => <<"good">>,
                    <<"comment">> => <<>>
                }
            ],
            teaching_review_logic:char_reviews_payload(Bin)
        ),
        ?_assertEqual(null, teaching_review_logic:char_reviews_payload(null)),
        ?_assertEqual(null, teaching_review_logic:char_reviews_payload(undefined)),
        ?_assertEqual(null, teaching_review_logic:char_reviews_payload(<<"not json">>)),
        ?_assertEqual(
            [#{<<"index">> => 7}],
            teaching_review_logic:char_reviews_payload([#{<<"index">> => 7}])
        )
    ].

%% PublishedReview 集成：char_reviews 二进制快照解码透出；无数据为 null
payload_char_reviews_present_test_() ->
    Snap = jsone:encode([
        #{
            <<"index">> => 0,
            <<"char">> => <<"人"/utf8>>,
            <<"grade">> => <<"fair">>,
            <<"comment">> => <<"收笔略飘"/utf8>>
        }
    ]),
    R = teaching_review_logic:published_review_payload(
        #{<<"id">> => 42, <<"char_reviews">> => Snap}, [], <<"李老师"/utf8>>
    ),
    [
        ?_assertEqual(
            [
                #{
                    <<"index">> => 0,
                    <<"char">> => <<"人"/utf8>>,
                    <<"grade">> => <<"fair">>,
                    <<"comment">> => <<"收笔略飘"/utf8>>
                }
            ],
            maps:get(<<"char_reviews">>, R)
        )
    ].

payload_char_reviews_absent_test_() ->
    R = teaching_review_logic:published_review_payload(#{<<"id">> => 42}, [], null),
    [?_assertEqual(null, maps:get(<<"char_reviews">>, R))].
