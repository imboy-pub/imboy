%% teaching_review_state_race_tests — R22 状态竞态回归（A1 REVIEW-STATE）
%%
%% R22-WITHDRAW-RACE-01：draft_guard（事务外 ACL）放行后、事务内 lock 行时
%%   submission 已被监护人 withdraw —— save_draft 必须在事务内复查 status
%%   并拒绝（{error, withdrawn} → 5482），不得对 withdrawn 提交写草稿。
%%   注：homework_submission.status 枚举仅 submitted/withdrawn（迁移
%%   00000097 ck_homework_submission_status），无 published 状态。
%% R22-PUBLISHED-DRAFT-01：该 reviewer 的草稿已发布（teacher_review.status
%%   ='published'）后再 save_draft —— upsert_draft_tx 的 find_draft_tx 只认
%%   'draft' 行会让其走 INSERT 建第二条草稿；必须在事务内 lock 后、upsert 前
%%   查 find_published_tx 拒绝（{error, already_reviewed} → 5481），零写入。
%%
%% 纯 meck 单元测试（无 DB 依赖）：elib_pg:with_tx mock 为直接执行 Tx 闭包
%% （fake_conn 透传）；repo 的 _tx 函数全部 mock——「零调用」断言即证明
%% teacher_review / review_asset 零新增零修改（所有写入只经这些 _tx 函数）。
%%
%% DC-1（返工）：workbench 顶层 my_review_draft 恒 null——load_submission_bundle
%%   的 Bundle 无 draft 键，build_workbench 直读恒 null；须复用 teacher_view
%%   已算好的草稿（前端 fetchWorkbench / OpenAPI 契约读顶层字段恢复草稿）。
-module(teaching_review_state_race_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% 夹具
%%%===================================================================

-define(TEACHER, 995001).
-define(GROUP_A1, 995100).
-define(SUB_WITHDRAWN, 995701).
-define(SUB_SUBMITTED, 995702).
-define(REVIEW_ID, 995801).

teacher_review_row() ->
    #{
        <<"id">> => ?REVIEW_ID,
        <<"submission_id">> => ?SUB_SUBMITTED,
        <<"reviewer_uid">> => ?TEACHER,
        <<"positive_point">> => <<"起笔稳"/utf8>>,
        <<"focus_problem">> => <<>>,
        <<"practice_action">> => <<>>,
        <<"comment">> => <<>>,
        <<"video_attachment_id">> => null,
        <<"rework_required">> => false,
        <<"status">> => <<"draft">>,
        <<"published_at">> => null
    }.

published_review_row() ->
    (teacher_review_row())#{
        <<"status">> => <<"published">>,
        <<"published_at">> => <<"2026-09-11T04:00:00Z">>
    }.

sub_row() ->
    #{
        <<"id">> => ?SUB_SUBMITTED,
        <<"assignment_id">> => 995501,
        <<"learner_id">> => 995301,
        <<"attempt_no">> => 1,
        <<"status">> => <<"submitted">>,
        <<"submitted_at">> => <<"2026-09-10T10:00:00Z">>,
        <<"withdrawn_at">> => null
    }.

draft_asset_row() ->
    #{
        <<"review_id">> => ?REVIEW_ID,
        <<"attachment_id">> => 995601,
        <<"object_key">> => <<"u995001/t105f/a01.mp4">>,
        <<"kind">> => <<"feedback_video">>,
        <<"sort_order">> => 0
    }.

%%%===================================================================
%%% R22-WITHDRAW-RACE-01：ACL 通过后提交已撤回 → 拒绝且零写入
%%%===================================================================

withdraw_race_draft_rejected_test_() ->
    ?WITH_MECKS(
        [
            {teaching_acl, [
                {'submission_access', 2, fun(_Uid, _Sid) ->
                    {ok, staff, #{<<"group_id">> => ?GROUP_A1}}
                end},
                {'resolve_staff', 3, fun(_Uid, _Gid, _Perm) ->
                    {ok, teacher}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 2, fun(TxFun, _Opts) -> TxFun(fake_conn) end}
            ]},
            {teaching_submission_repo, [
                %% 竞态复现：ACL 已放行，事务内锁行时监护人已 withdraw
                {'lock_submission_tx', 2, fun(_Conn, _Sid) ->
                    {ok, #{
                        <<"id">> => ?SUB_WITHDRAWN,
                        <<"status">> => <<"withdrawn">>,
                        <<"attempt_no">> => 1
                    }}
                end}
            ]},
            {teaching_review_repo, [
                {'find_published_tx', 2, fun(_Conn, _Sid) -> {ok, undefined} end},
                {'upsert_draft_tx', 3, fun(_Conn, _Sid, _Fields) ->
                    {ok, teacher_review_row()}
                end},
                {'validate_assets_tx', 3, fun(_Conn, _Uid, _Assets) -> {ok, []} end},
                {'replace_assets_tx', 4, fun(_Conn, _Rid, _Uid, _Assets) -> ok end},
                {'assets_tx', 2, fun(_Conn, _Rid) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Result = teaching_review_logic:save_draft(
                ?TEACHER,
                ?SUB_WITHDRAWN,
                #{<<"positive_point">> => <<"起笔稳"/utf8>>}
            ),
            ?assertEqual({error, withdrawn}, Result),
            %% withdrawn 提交：teacher_review / review_asset 零新增零修改
            ?assertEqual(0, meck:num_calls(teaching_review_repo, upsert_draft_tx, 3)),
            ?assertEqual(0, meck:num_calls(teaching_review_repo, replace_assets_tx, 4))
        end
    ).

%%%===================================================================
%%% R22-PUBLISHED-DRAFT-01：已发布回评后再存草稿 → 拒绝且零写入
%%% （单元层等效复现：find_draft_tx 只认 'draft' 行 → 已发布后无 draft
%%%   行 → upsert 走 INSERT 建第二条草稿；此处 mock upsert 模拟该分支，
%%%   断言守卫必须在其之前拦截）
%%%===================================================================

published_rejects_new_draft_test_() ->
    ?WITH_MECKS(
        [
            {teaching_acl, [
                {'submission_access', 2, fun(_Uid, _Sid) ->
                    {ok, staff, #{<<"group_id">> => ?GROUP_A1}}
                end},
                {'resolve_staff', 3, fun(_Uid, _Gid, _Perm) ->
                    {ok, teacher}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 2, fun(TxFun, _Opts) -> TxFun(fake_conn) end}
            ]},
            {teaching_submission_repo, [
                %% 提交本身正常（submitted）；竞态/后置场景在 teacher_review 侧
                {'lock_submission_tx', 2, fun(_Conn, _Sid) ->
                    {ok, #{
                        <<"id">> => ?SUB_SUBMITTED,
                        <<"status">> => <<"submitted">>,
                        <<"attempt_no">> => 1
                    }}
                end}
            ]},
            {teaching_review_repo, [
                %% 已有发布回评（该 reviewer 草稿已 publish）
                {'find_published_tx', 2, fun(_Conn, _Sid) ->
                    {ok, published_review_row()}
                end},
                {'upsert_draft_tx', 3, fun(_Conn, _Sid, _Fields) ->
                    {ok, teacher_review_row()}
                end},
                {'validate_assets_tx', 3, fun(_Conn, _Uid, _Assets) -> {ok, []} end},
                {'replace_assets_tx', 4, fun(_Conn, _Rid, _Uid, _Assets) -> ok end},
                {'assets_tx', 2, fun(_Conn, _Rid) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Result = teaching_review_logic:save_draft(
                ?TEACHER,
                ?SUB_SUBMITTED,
                #{<<"comment">> => <<"发布后想再补一句"/utf8>>}
            ),
            ?assertEqual({error, already_reviewed}, Result),
            %% 已发布：upsert（含 INSERT 第二条 draft 的分支）零调用
            ?assertEqual(0, meck:num_calls(teaching_review_repo, upsert_draft_tx, 3)),
            ?assertEqual(0, meck:num_calls(teaching_review_repo, replace_assets_tx, 4))
        end
    ).

%%%===================================================================
%%% 回归：正常草稿路径（submitted + 无已发布回评）不受两修复影响
%%%===================================================================

normal_draft_path_regression_test_() ->
    ?WITH_MECKS(
        [
            {teaching_acl, [
                {'submission_access', 2, fun(_Uid, _Sid) ->
                    {ok, staff, #{<<"group_id">> => ?GROUP_A1}}
                end},
                {'resolve_staff', 3, fun(_Uid, _Gid, _Perm) ->
                    {ok, teacher}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 2, fun(TxFun, _Opts) -> TxFun(fake_conn) end}
            ]},
            {teaching_submission_repo, [
                {'lock_submission_tx', 2, fun(_Conn, _Sid) ->
                    {ok, #{
                        <<"id">> => ?SUB_SUBMITTED,
                        <<"status">> => <<"submitted">>,
                        <<"attempt_no">> => 1
                    }}
                end}
            ]},
            {teaching_review_repo, [
                {'find_published_tx', 2, fun(_Conn, _Sid) -> {ok, undefined} end},
                {'upsert_draft_tx', 3, fun(_Conn, _Sid, _Fields) ->
                    {ok, teacher_review_row()}
                end},
                {'validate_assets_tx', 3, fun(_Conn, _Uid, _Assets) -> {ok, []} end},
                {'replace_assets_tx', 4, fun(_Conn, _Rid, _Uid, _Assets) -> ok end},
                {'assets_tx', 2, fun(_Conn, _Rid) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Result = teaching_review_logic:save_draft(
                ?TEACHER,
                ?SUB_SUBMITTED,
                #{<<"positive_point">> => <<"横画起笔稳"/utf8>>}
            ),
            ?assertMatch({ok, #{<<"review_id">> := <<"995801">>}}, Result),
            %% 无草稿无发布（或已有草稿 UPDATE 分支）：upsert 恰好一次
            ?assertEqual(1, meck:num_calls(teaching_review_repo, upsert_draft_tx, 3))
        end
    ).

%%%===================================================================
%%% DC-1：workbench 顶层 my_review_draft 恒 null（Bundle 无 draft 键）
%%% load_submission_bundle 构造的 Bundle 无 draft 键 → build_workbench
%%% 顶层恒 null；真实草稿由 teacher_view 内 find_draft(Sid,Uid) 取出，
%%% 应复用其已算好的值（老师重开工作台恢复草稿的契约字段）。
%%%===================================================================

dc1_workbench_top_level_draft_test_() ->
    ?WITH_MECKS(
        [
            {teaching_acl, [
                {'submission_access', 2, fun(_Uid, _Sid) ->
                    {ok, staff, #{<<"group_id">> => ?GROUP_A1}}
                end}
            ]},
            {teaching_context_repo, [
                {'submission_scope', 1, fun(_Sid) ->
                    {ok, #{<<"learner_id">> => 995301, <<"task_id">> => <<"task_dc1">>}}
                end}
            ]},
            {teaching_submission_repo, [
                {'find', 1, fun(_Sid) -> {ok, sub_row()} end},
                {'assets', 1, fun(_Sid) -> {ok, []} end}
            ]},
            {teaching_review_repo, [
                {'ai_draft', 1, fun(_Sid) -> {ok, undefined} end},
                {'find_published', 1, fun(_Sid) -> {ok, undefined} end},
                %% 该 reviewer 有一份含媒体的草稿
                {'find_draft', 2, fun(_Sid, _Uid) -> {ok, teacher_review_row()} end},
                {'assets', 1, fun(_Rid) -> {ok, [draft_asset_row()]} end}
            ]},
            {elib_pg, [
                %% learner_name / task_title 池查询（空结果 → 兜底 <<>>）
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            {ok, WB} = teaching_review_logic:workbench(?TEACHER, ?SUB_SUBMITTED),
            Top = maps:get(<<"my_review_draft">>, WB, missing),
            %% 顶层必须含草稿数据（文字 + 媒体）；当前实现恒 null → RED
            ?assertMatch(
                #{
                    <<"review_id">> := <<"995801">>,
                    <<"positive_point">> := <<"起笔稳"/utf8>>,
                    <<"assets">> := [_ | _]
                },
                Top
            ),
            %% submission 内嵌 my_review_draft 保持不动（兼容窗口）：同源非空
            Embedded =
                maps:get(
                    <<"my_review_draft">>,
                    maps:get(<<"submission">>, WB, #{}),
                    missing
                ),
            ?assertMatch(#{<<"review_id">> := <<"995801">>}, Embedded)
        end
    ).
