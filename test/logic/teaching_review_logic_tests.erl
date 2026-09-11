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
