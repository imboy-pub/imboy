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
