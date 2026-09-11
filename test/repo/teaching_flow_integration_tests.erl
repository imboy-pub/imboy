%% teaching_flow_integration_tests
%% FLOW-01 / IDEMP-01 / STATE-01 — moya 教学回课闭环真库集成测试。
%%
%% 直连 scratch@127.0.0.1:4323（RUN 专属库 moya_zcode_181902，00000001→103
%% 全量态 + 00000105 review_asset），每用例 BEGIN ... ROLLBACK，不留数据。
%% 测试直接驱动 Repo 的 _tx 函数（与生产 elib_pg:with_tx 同一代码路径），
%% 验证 STEP-08-DB 配方①②③：
%%   ① 幂等 CTE（ON CONFLICT 带部分索引谓词）+ digest 判 5460
%%   ② attempt FOR UPDATE 取号
%%   ③ 撤回/发布 lock-first 互斥
%% P0-4（MN-MEDIA）追加（995xxx 独立 ID 段）：
%%   ④ 回评媒体：validate_assets_tx 归属/MIME/scope/数量全拒绝、
%%      replace_assets_tx 原子替换（只解除关联不删对象）、零部分写入
%% DB 不可达时自动 skip（本地无 scratch 库的 CI 环境）。

-module(teaching_flow_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- 夹具 ----
-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_zcode_181902">>).

-define(TEACHER, 980001).
-define(PARENT, 980002).
-define(ASSISTANT, 980003).
-define(ORG_A, 981000).
-define(WS_A, 982001).
-define(GROUP_A1, 983001).
-define(LEARNER_A1, 984001).
-define(TASK_ID, <<"task99_hash_001">>).
-define(ASSIGN_ID, 986001).
-define(ATT_VIDEO, 988001).
-define(ATT_PHOTO, 988002).
%% P0-4 独立 ID 段（995xxx，与 attach 集成测试共用同一套组织夹具）
-define(RV_TEACHER, 995001).
-define(RV_PARENT, 995002).
-define(RV_SUB1, 995701).
-define(RV_DRAFT, 995801).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        {ok, _} = application:ensure_all_started(epgsql),
        %% 直连模式未起 imboy 应用：本测试仅需 default TSID 生成器
        try
            elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
        catch
            _:_ -> ok
        end,
        {ok, C} = epgsql:connect(#{
            host => ?PG_HOST,
            port => ?PG_PORT,
            username => ?PG_USER,
            password => ?PG_PASS,
            database => ?PG_DB,
            timeout => 5000
        }),
        C
    catch
        _:_ -> skip
    end.

close_conn(skip) ->
    ok;
close_conn(C) ->
    try
        epgsql:close(C)
    catch
        _:_ -> ok
    end,
    ok.

with_tx(TestFun) ->
    {setup, fun setup_conn/0, fun close_conn/1, fun
        (skip) ->
            [];
        (C) ->
            ?_test(begin
                ok = exec(C, <<"BEGIN">>),
                try
                    TestFun(C),
                    ok
                after
                    exec(C, <<"ROLLBACK">>)
                end
            end)
    end}.

exec(C, Sql) ->
    exec(C, Sql, []).

exec(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

%% 取行查询：返回 Rows（空表为 []）
q(C, Sql) ->
    q(C, Sql, []).

q(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

%%%===================================================================
%%% Seed（与 STEP-08/behavior-acl.sql 同构，含 workspace_member 子集前置）
%%%===================================================================

seed(C) ->
    exec(C, <<
        "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES "
        "(980001, 'x', 't99_teacher', '127.0.0.1', 'x'), "
        "(980002, 'x', 't99_parent', '127.0.0.1', 'x'), "
        "(980003, 'x', 't99_assist', '127.0.0.1', 'x')"
    >>),
    exec(
        C, <<"INSERT INTO organization (id, name, owner_id) VALUES (981000, '机构A', 980001)"/utf8>>
    ),
    exec(C, <<
        "INSERT INTO workspace (id, name, owner_id, organization_id) VALUES "
        "(982001, 'A-校区', 980001, 981000)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, status) "
        "VALUES (982001, 980001, 'owner', 980001, 'active'), "
        "(982001, 980002, 'member', 980001, 'active')"
    >>),
    exec(C, <<
        "INSERT INTO \"group\" (id, owner_uid, creator_uid, scope, workspace_id, title) "
        "VALUES (983001, 980001, 980001, 'workspace', 982001, 'A1-硬笔班')"/utf8
    >>),
    exec(C, <<
        "INSERT INTO learner (id, organization_id, display_name) VALUES "
        "(984001, 981000, '大宝')"/utf8
    >>),
    exec(C, <<"INSERT INTO class_enrollment (group_id, learner_id) VALUES (983001, 984001)">>),
    exec(C, <<
        "INSERT INTO class_staff (group_id, user_id, role) VALUES "
        "(983001, 980001, 'teacher'), (983001, 980003, 'assistant')"
    >>),
    exec(C, <<
        "INSERT INTO guardian_learner (guardian_uid, learner_id, can_submit, can_view_review) "
        "VALUES (980002, 984001, true, true)"
    >>),
    exec(C, <<
        "INSERT INTO group_task (id, group_id, task_id, title, creator_id, status) VALUES "
        "(985001, 983001, '",
        (?TASK_ID)/binary,
        "', '横竖练习', 980001, 1)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) VALUES "
        "(986001, '",
        (?TASK_ID)/binary,
        "', 980002, 984001)"
    >>),
    exec(C, <<"SET CONSTRAINTS ALL IMMEDIATE">>),
    exec(C, <<
        "INSERT INTO attachment (id, file_hash256, path, mime_type, creator_user_id) VALUES "
        "(988001, 'h99video', 'p/988001', 'video/mp4', 980002), "
        "(988002, 'h99photo', 'p/988002', 'image/jpeg', 980002)"
    >>).

%%%===================================================================
%%% FLOW-01：提交 → 草稿 → 发布 → 已发布回评（无 AI 人工闭环主链）
%%%===================================================================

flow01_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 1. 幂等提交（配方①②）
        {ok, Sid} = create_submission(C, ?PARENT, <<"idem-flow-01">>, <<"digest-a">>),
        ?assert(is_integer(Sid)),
        %% 2. AI 草稿已入队（queued 占位，无 Worker 本波不推进）
        [#{<<"status">> := <<"queued">>}] = q(
            C,
            <<"SELECT status FROM calligraphy_review_draft WHERE submission_id = ",
                (integer_to_binary(Sid))/binary>>
        ),
        %% 3. 老师保存草稿
        Draft = #{
            uid => ?TEACHER,
            positive_point => <<"横画稳"/utf8>>,
            focus_problem => <<"竖画歪"/utf8>>,
            practice_action => <<"每天三行竖画"/utf8>>,
            comment => <<>>,
            video_attachment_id => undefined,
            rework_required => false
        },
        {ok, DraftRow} = teaching_review_repo:upsert_draft_tx(C, Sid, Draft),
        ?assertEqual(<<"draft">>, maps:get(<<"status">>, DraftRow)),
        %% 4. lock-first 发布（配方③）
        lock_sub(C, Sid),
        {ok, published, Pub} = teaching_review_repo:publish_tx(C, Sid, ?TEACHER),
        ?assertEqual(<<"published">>, maps:get(<<"status">>, Pub)),
        ?assert(maps:get(<<"published_at">>, Pub) =/= null),
        ?assertEqual(?TEACHER, maps:get(<<"reviewer_uid">>, Pub)),
        %% 5. 家长视角：已发布回评可读（仅一条，同连接版本）
        {ok, #{<<"id">> := PubId}} = teaching_review_repo:find_published_tx(C, Sid),
        ?assertEqual(maps:get(<<"id">>, Pub), PubId),
        %% 6. 已发布后撤回被互斥拒绝（STATE-01 交叉）
        {error, already_reviewed} = teaching_submission_repo:withdraw_tx(C, Sid, ?PARENT)
    end).

%%%===================================================================
%%% IDEMP-01：同 key 重试不新增 submission/attempt/attachment relation
%%%===================================================================

idemp01_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Sid1} = create_submission(C, ?PARENT, <<"idem-key-001">>, <<"digest-x">>),
        %% 同 key 同 digest：返回同一 submission（created=false）
        {ok, Sid1} = create_submission(C, ?PARENT, <<"idem-key-001">>, <<"digest-x">>),
        %% 同 key 不同 digest：5460（idempotency_conflict）
        ?assertEqual(
            {error, idempotency_conflict},
            create_submission(C, ?PARENT, <<"idem-key-001">>, <<"digest-Y">>)
        ),
        %% 不同 key：新 attempt=2
        {ok, _Sid2} = create_submission(C, ?PARENT, <<"idem-key-002">>, <<"digest-x">>),
        [#{<<"c">> := 2}] = q(C, <<
            "SELECT count(*) AS c FROM homework_submission "
            "WHERE assignment_id = ",
            (integer_to_binary(?ASSIGN_ID))/binary
        >>),
        %% 重试未复制附件关系：2 个真实提交 × (video+photo) = 4；
        %% 重放/冲突两次重试均未新增（若重放复制会是 8）
        [#{<<"c">> := 4}] = q(C, <<"SELECT count(*) AS c FROM submission_asset">>),
        %% attempt 序列 1,2（未因重试跳号/重复）
        Rows = q(C, <<
            "SELECT attempt_no FROM homework_submission "
            "WHERE assignment_id = ",
            (integer_to_binary(?ASSIGN_ID))/binary,
            " ORDER BY attempt_no"
        >>),
        ?assertEqual([1, 2], [maps:get(<<"attempt_no">>, R) || R <- Rows])
    end).

%%%===================================================================
%%% STATE-01：非法状态跳转 / 重复发布 / AI failed 不阻断人工发布
%%%===================================================================

state01_publish_without_draft_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, ?PARENT, <<"idem-s1a">>, <<"d">>),
        lock_sub(C, Sid),
        %% 无草稿发布 → no_draft（5480）
        ?assertEqual({error, no_draft}, teaching_review_repo:publish_tx(C, Sid, ?TEACHER))
    end).

state01_double_publish_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, ?PARENT, <<"idem-s1b">>, <<"d">>),
        Draft = #{uid => ?TEACHER, positive_point => <<"ok">>},
        {ok, _} = teaching_review_repo:upsert_draft_tx(C, Sid, Draft),
        lock_sub(C, Sid),
        {ok, published, _} = teaching_review_repo:publish_tx(C, Sid, ?TEACHER),
        %% 重复发布 → already_published（幂等返回已发布结果，不报错）
        {ok, already_published, _} = teaching_review_repo:publish_tx(C, Sid, ?TEACHER),
        %% 已发布后仍只有一条 published（uk_tr_published_per_submission）
        [#{<<"c">> := 1}] = q(
            C, <<"SELECT count(*) AS c FROM teacher_review WHERE status = 'published'">>
        )
    end).

state01_withdraw_then_publish_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, ?PARENT, <<"idem-s1c">>, <<"d">>),
        %% 家长撤回（lock-first 配方③）
        {ok, withdrawn} = teaching_submission_repo:withdraw_tx(C, Sid, ?PARENT),
        %% 撤回审计列（00000098 CHECK 强制）
        [#{<<"status">> := <<"withdrawn">>, <<"withdrawn_by">> := ?PARENT}] = q(
            C,
            <<"SELECT status, withdrawn_by FROM homework_submission WHERE id = ",
                (integer_to_binary(Sid))/binary>>
        ),
        %% 撤回后发布被拒（5482 路径）
        Draft = #{uid => ?TEACHER, positive_point => <<"ok">>},
        {ok, _} = teaching_review_repo:upsert_draft_tx(C, Sid, Draft),
        lock_sub(C, Sid),
        ?assertEqual({error, withdrawn}, teaching_review_repo:publish_tx(C, Sid, ?TEACHER)),
        %% 重复撤回 → not_submitted（409/5444 路径）
        ?assertEqual(
            {error, not_submitted},
            teaching_submission_repo:withdraw_tx(C, Sid, ?PARENT)
        )
    end).

state01_ai_failed_not_blocking_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, ?PARENT, <<"idem-s1d">>, <<"d">>),
        %% AI 草稿置为 failed（模拟 Step 11 失败路径）
        ok = exec(C, <<
            "UPDATE calligraphy_review_draft SET status = 'failed', "
            "error_code = 'timeout', completed_at = now() WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>),
        %% AI failed 不阻断人工回评（D-10：人工闭环仍通过）
        Draft = #{uid => ?TEACHER, focus_problem => <<"竖画右倾"/utf8>>},
        {ok, _} = teaching_review_repo:upsert_draft_tx(C, Sid, Draft),
        lock_sub(C, Sid),
        {ok, published, _} = teaching_review_repo:publish_tx(C, Sid, ?TEACHER)
    end).

%%%===================================================================
%%% P0-4（MN-MEDIA-02）：回评媒体校验/原子替换（995xxx 独立段）
%%% validate_assets_tx：attachment 存在+active+scope=teaching+creator=reviewer+MIME↔kind
%%% replace_assets_tx：原子替换（delete+insert 同事务，只解除关联不删对象）
%%%===================================================================

rv_seed(C) ->
    exec(C, <<
        "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES "
        "(995001, 'x', 't105f_teacher', '127.0.0.1', 'x'), "
        "(995002, 'x', 't105f_parent', '127.0.0.1', 'x')"
    >>),
    exec(
        C,
        <<"INSERT INTO organization (id, name, owner_id) VALUES (995100, 'P04F机构', 995001)"/utf8>>
    ),
    exec(C, <<
        "INSERT INTO workspace (id, name, owner_id, organization_id) VALUES "
        "(995110, 'P04F校区', 995001, 995100)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, status) "
        "VALUES (995110, 995001, 'owner', 995001, 'active')"
    >>),
    exec(C, <<
        "INSERT INTO \"group\" (id, owner_uid, creator_uid, scope, workspace_id, title) "
        "VALUES (995201, 995001, 995001, 'workspace', 995110, 'P04F硬笔班')"/utf8
    >>),
    exec(C, <<
        "INSERT INTO learner (id, organization_id, display_name) VALUES "
        "(995301, 995100, 'P04F大宝')"/utf8
    >>),
    exec(C, <<"INSERT INTO class_enrollment (group_id, learner_id) VALUES (995201, 995301)">>),
    exec(
        C,
        <<"INSERT INTO class_staff (group_id, user_id, role) VALUES (995201, 995001, 'teacher')">>
    ),
    exec(C, <<
        "INSERT INTO guardian_learner (guardian_uid, learner_id, can_submit, can_view_review) "
        "VALUES (995002, 995301, true, true)"
    >>),
    exec(C, <<
        "INSERT INTO group_task (id, group_id, task_id, title, creator_id, status) VALUES "
        "(995401, 995201, 'task105f_hash_01', 'P04F练习', 995001, 1)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) VALUES "
        "(995501, 'task105f_hash_01', 995002, 995301)"
    >>),
    exec(C, <<"SET CONSTRAINTS ALL IMMEDIATE">>),
    exec(C, <<
        "INSERT INTO homework_submission (id, assignment_id, learner_id, submitted_by, "
        "attempt_no, idempotency_key, request_digest) VALUES "
        "(995701, 995501, 995301, 995002, 1, 't105f-k1', 'd1')"
    >>),
    exec(C, <<
        "INSERT INTO teacher_review (id, submission_id, reviewer_uid, comment, status) VALUES "
        "(995801, 995701, 995001, 'P04F草稿', 'draft')"/utf8
    >>),
    exec(C, <<"SET CONSTRAINTS ALL IMMEDIATE">>).

%% 附件矩阵（默认 creator=995001 teacher、scope=teaching、status=1）：
%%   995601 video/mp4 合法视频 | 995602-995604 image/* 合法图 | 995605 第 4 图
%%   995606 video/mp4 creator=995002（owner 不符）
%%   995607 image/jpeg scope=private
%%   995608 video/mp4（配 feedback_image 时 MIME 不匹配）
%%   995609 video/mp4 status=-1（已软删）
rv_seed_attachments(C) ->
    exec(C, <<
        "INSERT INTO attachment (id, file_hash256, path, url, mime_type, creator_user_id, scope, status) VALUES "
        "(995601, 'h105f01', 'u995001/t105f/a01.mp4', 'u995001/t105f/a01.mp4', 'video/mp4', 995001, 'teaching', 1), "
        "(995602, 'h105f02', 'u995001/t105f/a02.jpg', 'u995001/t105f/a02.jpg', 'image/jpeg', 995001, 'teaching', 1), "
        "(995603, 'h105f03', 'u995001/t105f/a03.png', 'u995001/t105f/a03.png', 'image/png', 995001, 'teaching', 1), "
        "(995604, 'h105f04', 'u995001/t105f/a04.jpg', 'u995001/t105f/a04.jpg', 'image/jpeg', 995001, 'teaching', 1), "
        "(995605, 'h105f05', 'u995001/t105f/a05.jpg', 'u995001/t105f/a05.jpg', 'image/jpeg', 995001, 'teaching', 1), "
        "(995606, 'h105f06', 'u995002/t105f/a06.mp4', 'u995002/t105f/a06.mp4', 'video/mp4', 995002, 'teaching', 1), "
        "(995607, 'h105f07', 'u995001/t105f/a07.jpg', 'u995001/t105f/a07.jpg', 'image/jpeg', 995001, 'private', 1), "
        "(995608, 'h105f08', 'u995001/t105f/a08.mp4', 'u995001/t105f/a08.mp4', 'video/mp4', 995001, 'teaching', 1), "
        "(995609, 'h105f09', 'u995001/t105f/a09.mp4', 'u995001/t105f/a09.mp4', 'video/mp4', 995001, 'teaching', -1)"
    >>).

media_validate_ok_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        %% 1 video + 3 image 全合法（创建者是当前 reviewer）
        {ok, _} = teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, [
            {995601, <<"feedback_video">>, 0},
            {995602, <<"feedback_image">>, 1},
            {995603, <<"feedback_image">>, 2},
            {995604, <<"feedback_image">>, 3}
        ]),
        %% 空集合合法（纯文字回评）
        {ok, []} = teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, [])
    end).

media_validate_rejections_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        %% MIME↔kind 不匹配（video/mp4 配 feedback_image）
        ?assertEqual(
            {error, assets_invalid},
            teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, [
                {995608, <<"feedback_image">>, 0}
            ])
        ),
        %% MIME↔kind 不匹配（image 配 feedback_video）
        ?assertEqual(
            {error, assets_invalid},
            teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, [
                {995602, <<"feedback_video">>, 0}
            ])
        ),
        %% owner 不符（家长创建的附件）→ not_found（与不存在同响应，防存在性探测）
        ?assertEqual(
            {error, not_found},
            teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, [
                {995606, <<"feedback_video">>, 0}
            ])
        ),
        %% scope 不符（private）
        ?assertEqual(
            {error, assets_invalid},
            teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, [
                {995607, <<"feedback_image">>, 0}
            ])
        ),
        %% 已软删（status=-1）
        ?assertEqual(
            {error, assets_invalid},
            teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, [
                {995609, <<"feedback_video">>, 0}
            ])
        ),
        %% 不存在
        ?assertEqual(
            {error, not_found},
            teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, [
                {999999, <<"feedback_video">>, 0}
            ])
        )
    end).

%% 原子替换：第二次 replace 完全替换第一组；被替换 attachment 不物理删（只解除关联）
media_replace_atomic_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        %% 第一次：1 video + 1 image
        ok = teaching_review_repo:replace_assets_tx(C, ?RV_DRAFT, ?RV_TEACHER, [
            {995601, <<"feedback_video">>, 0},
            {995602, <<"feedback_image">>, 1}
        ]),
        [#{<<"c">> := 2}] = q(C, <<"SELECT count(*) AS c FROM review_asset">>),
        %% 第二次：3 图（完全替换，995601/995602 关联解除）
        ok = teaching_review_repo:replace_assets_tx(C, ?RV_DRAFT, ?RV_TEACHER, [
            {995603, <<"feedback_image">>, 0},
            {995604, <<"feedback_image">>, 1},
            {995605, <<"feedback_image">>, 2}
        ]),
        Rows = q(C, <<"SELECT attachment_id FROM review_asset ORDER BY attachment_id">>),
        ?assertEqual([995603, 995604, 995605], [maps:get(<<"attachment_id">>, R) || R <- Rows]),
        %% 被替换附件未物理删/未软删（对象保留，只解除 review_asset 关联）
        [#{<<"status">> := 1}] = q(C, <<"SELECT status FROM attachment WHERE id = 995601">>),
        %% 解除后 995601 可再绑定其他 review（全表唯一按"现存关联"生效）
        ok = exec(C, <<
            "INSERT INTO teacher_review (id, submission_id, reviewer_uid, comment, status) "
            "VALUES (995802, 995701, 995001, 'P04F二稿', 'draft')"/utf8
        >>),
        ok = teaching_review_repo:replace_assets_tx(C, 995802, ?RV_TEACHER, [
            {995601, <<"feedback_video">>, 0}
        ])
    end).

%% 校验先行配方（save_draft 同序）：混合集合任一非法 → error → 不执行写入（零部分写入）
media_validate_before_write_zero_partial_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        Mixed = [
            {995601, <<"feedback_video">>, 0},
            {995602, <<"feedback_image">>, 1},
            {995606, <<"feedback_video">>, 2}
        ],
        %% 第三个附件 owner 不符：整集合校验失败（save_draft 按此序在 replace 前调用）
        ?assertEqual(
            {error, not_found}, teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, Mixed)
        ),
        %% 未执行任何写入：review_asset 零行
        [#{<<"c">> := 0}] = q(C, <<"SELECT count(*) AS c FROM review_asset">>)
    end).

%% 旧列冗余写（兼容读窗口）：logic 的 draft_fields 从 assets 派生
%% video_attachment_id（第一条 feedback_video）传入 upsert —— repo 层断言列值落库
media_draft_legacy_column_derived_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        Assets = [
            {995602, <<"feedback_image">>, 0},
            {995601, <<"feedback_video">>, 1}
        ],
        %% 模拟 logic 侧：video_attachment_id = derive_video_id(Assets) = 995601
        Fields = #{
            uid => ?RV_TEACHER,
            comment => <<"媒体回评"/utf8>>,
            video_attachment_id => 995601,
            rework_required => false
        },
        {ok, #{<<"id">> := Rid}} = teaching_review_repo:upsert_draft_tx(C, ?RV_SUB1, Fields),
        {ok, Assets} = teaching_review_repo:validate_assets_tx(C, ?RV_TEACHER, Assets),
        ok = teaching_review_repo:replace_assets_tx(C, Rid, ?RV_TEACHER, Assets),
        %% 旧列 = 派生视频；review_asset 集合完整（旧列与新表一致，两读窗口同源）
        [#{<<"video_attachment_id">> := 995601}] =
            q(C, <<"SELECT video_attachment_id FROM teacher_review WHERE id = 995801">>),
        [#{<<"c">> := 2}] = q(
            C, <<"SELECT count(*) AS c FROM review_asset WHERE review_id = 995801">>
        )
    end).

%%%===================================================================
%%% Helpers：走生产同款配方（FOR UPDATE → 取号 → CTE 幂等插入 → 附件 → 入队）
%%%===================================================================

lock_sub(C, Sid) ->
    {ok, Row} = teaching_submission_repo:lock_submission_tx(C, Sid),
    ?assert(is_map(Row)),
    Row.

-spec create_submission(pid(), integer(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
create_submission(C, Uid, IdemKey, Digest) ->
    {ok, ?ASSIGN_ID} = teaching_submission_repo:lock_assignment_tx(C, ?ASSIGN_ID),
    {ok, Next} = teaching_submission_repo:next_attempt_tx(C, ?ASSIGN_ID),
    case
        teaching_submission_repo:create_idempotent_tx(C, #{
            id => 987000 + Next,
            assignment_id => ?ASSIGN_ID,
            learner_id => ?LEARNER_A1,
            uid => Uid,
            attempt_no => Next,
            idempotency_key => IdemKey,
            request_digest => Digest
        })
    of
        {ok, #{<<"id">> := Sid} = Row} ->
            case maps:get(created, Row, false) of
                true ->
                    ok = teaching_submission_repo:insert_assets_tx(C, Sid, Uid, [
                        {?ATT_VIDEO, <<"practice_video">>, 0},
                        {?ATT_PHOTO, <<"final_photo">>, 0}
                    ]),
                    ok = teaching_submission_repo:mark_submitted_by_tx(C, ?ASSIGN_ID, Uid),
                    ok = teaching_submission_repo:enqueue_ai_draft_tx(C, Sid);
                false ->
                    ok
            end,
            {ok, Sid};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% R22（A1 REVIEW-STATE）：状态竞态真库回归（995xxx 段）
%%% logic 全链嫁接测试连接：meck elib_pg:with_tx → Tx(C)（repo 层真 SQL、
%%% 真约束、真行数断言）；teaching_acl 放行——ACL 查询走池连接，看不到
%%% 本事务（C 连接 BEGIN 后未提交）的 seed 数据，故事务外守卫须 mock。
%%%===================================================================

%% R22-WITHDRAW-RACE-01：ACL 通过后 submission 已被监护人撤回
%% → save_draft 事务内锁行复查 status 必须拒绝（{error, withdrawn}），
%%   teacher_review 零新增零修改、review_asset 零行。
r22_withdraw_race_save_draft_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        %% 家长撤回（真实 withdraw_tx，同连接同事务）
        {ok, withdrawn} = teaching_submission_repo:withdraw_tx(C, ?RV_SUB1, ?RV_PARENT),
        ok = meck:new(teaching_acl, [no_link, passthrough]),
        meck:expect(teaching_acl, submission_access, 2, fun(_Uid, _Sid) ->
            {ok, staff, #{<<"group_id">> => 995201}}
        end),
        meck:expect(teaching_acl, resolve_staff, 3, fun(_Uid, _Gid, _Perm) ->
            {ok, teacher}
        end),
        ok = meck:new(elib_pg, [no_link, passthrough]),
        meck:expect(elib_pg, with_tx, 2, fun(Tx, _Opts) -> Tx(C) end),
        try
            ?assertEqual(
                {error, withdrawn},
                teaching_review_logic:save_draft(?RV_TEACHER, ?RV_SUB1, #{
                    <<"comment">> => <<"撤回后不该写入"/utf8>>
                })
            ),
            %% teacher_review 仍只有 rv_seed 的 1 条 draft（未新增、原 comment 未被改写）
            [#{<<"c">> := 1}] = q(C, <<"SELECT count(*) AS c FROM teacher_review">>),
            [#{<<"comment">> := <<"P04F草稿"/utf8>>}] =
                q(C, <<"SELECT comment FROM teacher_review WHERE id = 995801">>),
            [#{<<"c">> := 0}] = q(C, <<"SELECT count(*) AS c FROM review_asset">>)
        after
            meck:unload(teaching_acl),
            meck:unload(elib_pg)
        end
    end).

%% R22-PUBLISHED-DRAFT-01：该 reviewer 草稿已发布后再 save_draft
%% → 事务内 lock 后 upsert 前必须拒绝（DC-2：{error, review_published} → 5486，
%%   5481 保留给撤回场景），保持一条 published、零新增 draft、review_asset 零行。
r22_published_rejects_save_draft_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        %% 真实链路：rv_seed 的 draft 995801 → lock-first → publish
        lock_sub(C, ?RV_SUB1),
        {ok, published, _} = teaching_review_repo:publish_tx(C, ?RV_SUB1, ?RV_TEACHER),
        ok = meck:new(teaching_acl, [no_link, passthrough]),
        meck:expect(teaching_acl, submission_access, 2, fun(_Uid, _Sid) ->
            {ok, staff, #{<<"group_id">> => 995201}}
        end),
        meck:expect(teaching_acl, resolve_staff, 3, fun(_Uid, _Gid, _Perm) ->
            {ok, teacher}
        end),
        ok = meck:new(elib_pg, [no_link, passthrough]),
        meck:expect(elib_pg, with_tx, 2, fun(Tx, _Opts) -> Tx(C) end),
        try
            ?assertEqual(
                {error, review_published},
                teaching_review_logic:save_draft(?RV_TEACHER, ?RV_SUB1, #{
                    <<"comment">> => <<"发布后想补写一句"/utf8>>
                })
            ),
            %% 一条 published（uk_tr_published_per_submission）、零新增 draft
            [#{<<"c">> := 1}] = q(
                C, <<"SELECT count(*) AS c FROM teacher_review WHERE status = 'published'">>
            ),
            [#{<<"c">> := 0}] = q(
                C, <<"SELECT count(*) AS c FROM teacher_review WHERE status = 'draft'">>
            ),
            [#{<<"c">> := 0}] = q(C, <<"SELECT count(*) AS c FROM review_asset">>)
        after
            meck:unload(teaching_acl),
            meck:unload(elib_pg)
        end
    end).
