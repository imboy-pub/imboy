-module(teaching_assignment_latest_asset_tests).
%%%
% teaching_assignment_logic:assignment_summary/1 的 latest_asset 契约单元测试
% 2026-09-12：AssignmentSummary 增 latest_asset（作品预览句柄，moya 家长首页缩略图）。
% 契约：null | #{object_key, kind}——只给句柄不给 URL（客户端按需调
% /api/v1/attachment/view_url 换签名 URL；MEDIA-03 不持久化 presigned URL）。
% SQL 侧取值口径（最新 submission 的首张 final_photo）见
% test/repo/teaching_assignment_list_integration_tests.erl。
%%%

-include_lib("eunit/include/eunit.hrl").

%% 列表行最小夹具（键名与 repo 的 SELECT 别名一一对应）
base_row() ->
    #{
        <<"assignment_id">> => 986001,
        <<"task_gid">> => 985001,
        <<"learner_id">> => 984001,
        <<"deadline">> => null,
        <<"group_id">> => 983001,
        <<"group_title">> => <<"A1-硬笔班"/utf8>>,
        <<"title">> => <<"横竖练习"/utf8>>,
        <<"latest_submission_id">> => 987002,
        <<"latest_attempt_no">> => 2,
        <<"has_published">> => false
    }.

latest_asset_from_final_photo_test() ->
    Row = (base_row())#{
        <<"latest_asset_key">> => <<"u980002/teaching/photo.jpg">>,
        <<"latest_asset_kind">> => <<"final_photo">>
    },
    Summary = teaching_assignment_logic:assignment_summary(Row),
    ?assertEqual(
        #{
            <<"object_key">> => <<"u980002/teaching/photo.jpg">>,
            <<"kind">> => <<"final_photo">>
        },
        maps:get(<<"latest_asset">>, Summary)
    ).

%% 无提交 / 最新提交无 final_photo：SQL 给 NULL → null（不回退 practice_video）
latest_asset_null_when_no_photo_test() ->
    Summary = teaching_assignment_logic:assignment_summary(
        (base_row())#{<<"latest_asset_key">> => null, <<"latest_asset_kind">> => null}
    ),
    ?assertEqual(null, maps:get(<<"latest_asset">>, Summary)).

%% 键缺失（旧行/异常输入）同样归 null——不得编造句柄
latest_asset_null_when_key_absent_test() ->
    Summary = teaching_assignment_logic:assignment_summary(base_row()),
    ?assertEqual(null, maps:get(<<"latest_asset">>, Summary)).

%% 空串归 null（防把空句柄当有效预览发给客户端）
latest_asset_null_when_key_empty_test() ->
    Row = (base_row())#{<<"latest_asset_key">> => <<>>},
    Summary = teaching_assignment_logic:assignment_summary(Row),
    ?assertEqual(null, maps:get(<<"latest_asset">>, Summary)).

%% 契约冻结：字段集合增删必须显式改本断言（防后续误删既有字段）。
%% W2-A2-F24（CM-F2）显式新增 description（moya parseAssignment 列表侧
%% 读取 r.description；AssignmentSummary.description 为可选字段）。
assignment_summary_field_set_frozen_test() ->
    Summary = teaching_assignment_logic:assignment_summary(base_row()),
    ?assertEqual(
        lists:sort([
            <<"assignment_id">>,
            <<"deadline">>,
            <<"description">>,
            <<"group_id">>,
            <<"group_name">>,
            <<"has_published_review">>,
            <<"latest_asset">>,
            <<"latest_attempt_no">>,
            <<"latest_submission">>,
            <<"learner_id">>,
            <<"rework_required">>,
            <<"status">>,
            <<"task_id">>,
            <<"title">>
        ]),
        lists:sort(maps:keys(Summary))
    ).
