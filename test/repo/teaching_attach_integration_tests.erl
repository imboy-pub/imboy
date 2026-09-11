%% teaching_attach_integration_tests
%% Step 10 真库集成（scratch@4323，BEGIN/ROLLBACK 不留数据）：
%%   ① submission_for_asset_path：附件路径 → submission 绑定解析（MEDIA-01 数据面）
%%   ② unbound_teaching_attachments：只列"超龄+未绑定"教学附件——
%%      已绑定（含撤回证据）/新近未绑定/其他 scope 一律不列（MEDIA-02 不误删）
%%   ③ attachment 行 path==url==object_key（MEDIA-03 行级断言：落库无 presigned URL）
%%   ④ P0-4（MN-MEDIA）：review_for_asset_path 数据面（draft/published/withdrawn）、
%%      孤儿清理双排除（submission_asset ∪ review_asset 含草稿引用）、
%%      review_asset 约束（attachment 全表唯一 / 单 review 单视频 / 3 图上限触发器）
%%
%% 库名：RUN 专属 scratch 库 moya_zcode_181902（1→103 全量态 + 00000105）。
%% DB 不可达自动 skip。

-module(teaching_attach_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_zcode_181902">>).

-define(PARENT, 980002).
-define(TEACHER, 980001).
-define(ORG_A, 981000).
-define(WS_A, 982001).
-define(GROUP_A1, 983001).
-define(LEARNER_A1, 984001).
-define(TASK_ID, <<"task10a_hash_01">>).
-define(ASSIGN_ID, 986001).
%% P0-4 独立 ID 段（995xxx，不与既有用例/其他卡夹具冲突）
-define(RV_TEACHER, 995001).
-define(RV_PARENT, 995002).
-define(RV_ORG, 995100).
-define(RV_WS, 995110).
-define(RV_GROUP, 995201).
-define(RV_LEARNER, 995301).
-define(RV_TASK_ID, <<"task105_hash_01">>).
-define(RV_ASSIGN_ID, 995501).
-define(RV_SUB1, 995701).
-define(RV_SUB2, 995702).
-define(RV_DRAFT, 995801).

%%%===================================================================
%%% Fixture（同 teaching_flow_integration_tests 模式）
%%%===================================================================

setup_conn() ->
    try
        {ok, _} = application:ensure_all_started(epgsql),
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
    end.

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

exec(C, Sql) -> exec(C, Sql, []).
exec(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

q(C, Sql) -> q(C, Sql, []).
q(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

%%%===================================================================
%%% Seed
%%%===================================================================

seed(C) ->
    exec(C, <<
        "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES "
        "(980001, 'x', 't10a_teacher', '127.0.0.1', 'x'), "
        "(980002, 'x', 't10a_parent', '127.0.0.1', 'x')"
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
    exec(
        C,
        <<"INSERT INTO class_staff (group_id, user_id, role) VALUES (983001, 980001, 'teacher')">>
    ),
    exec(C, <<"INSERT INTO guardian_learner (guardian_uid, learner_id) VALUES (980002, 984001)">>),
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
    %% 两个 submission：S1 submitted、S2 withdrawn（withdrawn_by 审计列满足 00000098 CHECK）
    exec(C, <<
        "INSERT INTO homework_submission (id, assignment_id, learner_id, submitted_by, "
        "attempt_no, idempotency_key, request_digest) VALUES "
        "(987001, 986001, 984001, 980002, 1, 't10a-k1', 'd1'), "
        "(987002, 986001, 984001, 980002, 2, 't10a-k2', 'd2')"
    >>),
    exec(C, <<
        "UPDATE homework_submission SET status = 'withdrawn', withdrawn_at = now(), "
        "withdrawn_by = 980002 WHERE id = 987002"
    >>),
    exec(C, <<"SET CONSTRAINTS ALL IMMEDIATE">>).

%% 教学附件四件套：
%%   989001 绑定 S1（submitted）           —— 正常可读路径
%%   989002 未绑定、created_at 25h 前      —— 超龄孤儿（应被列出）
%%   989003 绑定 S2（withdrawn 证据）      —— 不列出（不误删）
%%   989004 未绑定、created_at now         —— 新近（不列出）
%%   989005 scope=private 未绑定、25h 前   —— 非教学 scope（不列出）
seed_attachments(C) ->
    Rows = [
        {989001, <<"h10a1">>, <<"u980002/t10a/att1.mp4">>, <<"teaching">>, 0},
        {989002, <<"h10a2">>, <<"u980002/t10a/att2.mp4">>, <<"teaching">>, 25},
        {989003, <<"h10a3">>, <<"u980002/t10a/att3.mp4">>, <<"teaching">>, 0},
        {989004, <<"h10a4">>, <<"u980002/t10a/att4.jpg">>, <<"teaching">>, 0},
        {989005, <<"h10a5">>, <<"u980002/t10a/att5.jpg">>, <<"private">>, 25}
    ],
    lists:foreach(
        fun({Id, Hash, Path, Scope, AgeHours}) ->
            %% MEDIA-03 落库形态：path == url == object_key（与 attach_logic do_save_1 一致）
            exec(C, <<
                "INSERT INTO attachment (id, file_hash256, path, url, mime_type, "
                "creator_user_id, scope) VALUES (",
                (integer_to_binary(Id))/binary,
                ", '",
                Hash/binary,
                "', '",
                Path/binary,
                "', '",
                Path/binary,
                "', 'video/mp4', 980002, '",
                Scope/binary,
                "')"
            >>),
            case AgeHours of
                0 ->
                    ok;
                H ->
                    exec(C, <<
                        "UPDATE attachment SET created_at = "
                        "now() - interval '",
                        (integer_to_binary(H))/binary,
                        " hours' "
                        "WHERE id = ",
                        (integer_to_binary(Id))/binary
                    >>)
            end
        end,
        Rows
    ),
    exec(C, <<
        "INSERT INTO submission_asset (id, submission_id, attachment_id, kind, created_by) "
        "VALUES (989101, 987001, 989001, 'practice_video', 980002), "
        "(989102, 987002, 989003, 'practice_video', 980002)"
    >>).

%%%===================================================================
%%% ① submission_for_asset_path（MEDIA-01 数据面）
%%%===================================================================

asset_path_resolution_test_() ->
    with_tx(fun(C) ->
        seed(C),
        seed_attachments(C),
        %% 绑定附件 → submission + 状态
        {ok, #{<<"submission_id">> := 987001, <<"submission_status">> := <<"submitted">>}} =
            teaching_submission_repo:submission_for_asset_path_tx(C, <<"u980002/t10a/att1.mp4">>),
        %% 撤回 submission 的绑定附件 → 状态 withdrawn（授权层据此拒绝日常读，T17）
        {ok, #{<<"submission_id">> := 987002, <<"submission_status">> := <<"withdrawn">>}} =
            teaching_submission_repo:submission_for_asset_path_tx(C, <<"u980002/t10a/att3.mp4">>),
        %% 未绑定附件 → undefined（任何身份都拿不到读 URL）
        {ok, undefined} =
            teaching_submission_repo:submission_for_asset_path_tx(C, <<"u980002/t10a/att2.mp4">>),
        %% 不存在的路径 → undefined
        {ok, undefined} =
            teaching_submission_repo:submission_for_asset_path_tx(C, <<"u980002/t10a/nope.mp4">>)
    end).

%%%===================================================================
%%% ② unbound_teaching_attachments（MEDIA-02：只列超龄未绑定，不误删）
%%%===================================================================

unbound_listing_test_() ->
    with_tx(fun(C) ->
        seed(C),
        seed_attachments(C),
        {ok, Rows} = teaching_submission_repo:unbound_teaching_attachments_tx(C, 24),
        Ids = lists:sort([maps:get(<<"id">>, R) || R <- Rows]),
        %% 恰好只有 989002（超龄+未绑定+teaching）：
        %% 989001/989003 已绑定不列（含撤回证据）；989004 新近不列；989005 非 teaching 不列
        ?assertEqual([989002], Ids)
    end).

%%%===================================================================
%%% ③ MEDIA-03 行级断言：教学附件落库 path==url==object_key，无 presigned URL
%%%===================================================================

media03_no_url_persisted_in_rows_test_() ->
    with_tx(fun(C) ->
        seed(C),
        seed_attachments(C),
        Rows = q(C, <<"SELECT id, path, url FROM attachment WHERE id >= 989001 AND id <= 989005">>),
        ?assertEqual(5, length(Rows)),
        lists:foreach(
            fun(#{<<"path">> := Path, <<"url">> := Url}) ->
                ?assertEqual(Path, Url),
                %% object_key 形如 u<Uid>/...；presigned URL 必含签名查询串
                ?assertEqual(nomatch, binary:match(Url, <<"X-Amz-">>)),
                ?assertEqual(nomatch, binary:match(Url, <<"?Expires=">>)),
                ?assertEqual(nomatch, binary:match(Url, <<"Signature">>))
            end,
            Rows
        ),
        %% 业务表（submission_asset）只存 attachment_id，无 URL 列泄漏
        Cols = q(C, <<
            "SELECT column_name FROM information_schema.columns "
            "WHERE table_name = 'submission_asset' AND column_name LIKE '%url%'"
        >>),
        ?assertEqual([], Cols)
    end).

%%%===================================================================
%%% ④ P0-4（MN-MEDIA）：review_asset 数据面 + 孤儿双排除 + 约束
%%%===================================================================

%% P0-4 seed（995xxx 独立段；撤回审计列满足 00000098 CHECK）
rv_seed(C) ->
    exec(C, <<
        "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES "
        "(995001, 'x', 't105_teacher', '127.0.0.1', 'x'), "
        "(995002, 'x', 't105_parent', '127.0.0.1', 'x')"
    >>),
    exec(C, <<
        "INSERT INTO organization (id, name, owner_id) VALUES "
        "(995100, 'P04机构', 995001)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO workspace (id, name, owner_id, organization_id) VALUES "
        "(995110, 'P04校区', 995001, 995100)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, status) "
        "VALUES (995110, 995001, 'owner', 995001, 'active')"
    >>),
    exec(C, <<
        "INSERT INTO \"group\" (id, owner_uid, creator_uid, scope, workspace_id, title) "
        "VALUES (995201, 995001, 995001, 'workspace', 995110, 'P04硬笔班')"/utf8
    >>),
    exec(C, <<
        "INSERT INTO learner (id, organization_id, display_name) VALUES "
        "(995301, 995100, 'P04大宝')"/utf8
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
        "(995401, 995201, '",
        (?RV_TASK_ID)/binary,
        "', 'P04横竖练习', 995001, 1)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) VALUES "
        "(995501, '",
        (?RV_TASK_ID)/binary,
        "', 995002, 995301)"
    >>),
    exec(C, <<"SET CONSTRAINTS ALL IMMEDIATE">>),
    %% SUB1 submitted / SUB2 withdrawn
    exec(C, <<
        "INSERT INTO homework_submission (id, assignment_id, learner_id, submitted_by, "
        "attempt_no, idempotency_key, request_digest) VALUES "
        "(995701, 995501, 995301, 995002, 1, 't105-k1', 'd1'), "
        "(995702, 995501, 995301, 995002, 2, 't105-k2', 'd2')"
    >>),
    exec(C, <<
        "UPDATE homework_submission SET status = 'withdrawn', withdrawn_at = now(), "
        "withdrawn_by = 995002 WHERE id = 995702"
    >>),
    %% DRAFT on SUB1（在播草稿）/ DRAFT_WD on SUB2（撤回后遗留草稿——撤回守卫只挡 published）
    exec(C, <<
        "INSERT INTO teacher_review (id, submission_id, reviewer_uid, comment, status) VALUES "
        "(995801, 995701, 995001, 'P04草稿', 'draft'), "
        "(995803, 995702, 995001, 'P04撤回草稿', 'draft')"/utf8
    >>),
    %% PUBLISHED on SUB1（直接 INSERT published：publish_guard 校验 SUB1=submitted 通过）
    exec(C, <<
        "INSERT INTO teacher_review (id, submission_id, reviewer_uid, comment, status, published_at) "
        "VALUES (995802, 995701, 995001, 'P04已发布', 'published', now())"/utf8
    >>),
    exec(C, <<"SET CONSTRAINTS ALL IMMEDIATE">>).

%% P0-4 教学附件（老师 995001 创建，除特殊标注外 scope=teaching、status=1）：
%%   995611 draft video     —— 绑 DRAFT（995801）
%%   995612 draft image     —— 绑 DRAFT（草稿引用不误删）
%%   995613 published video —— 绑 PUBLISHED（995802）
%%   995614 withdrawn image —— 绑 DRAFT_WD（995803，撤回 submission 草稿）
%%   995615 未绑定超龄 25h  —— 超龄孤儿（应被列出）
%%   995616 未绑定新近      —— 不列出
rv_seed_attachments(C) ->
    Rows = [
        {995611, <<"h105a11">>, <<"u995001/t105/att11.mp4">>, <<"teaching">>, 0},
        {995612, <<"h105a12">>, <<"u995001/t105/att12.jpg">>, <<"teaching">>, 0},
        {995613, <<"h105a13">>, <<"u995001/t105/att13.mp4">>, <<"teaching">>, 0},
        {995614, <<"h105a14">>, <<"u995001/t105/att14.jpg">>, <<"teaching">>, 0},
        {995615, <<"h105a15">>, <<"u995001/t105/att15.jpg">>, <<"teaching">>, 25},
        {995616, <<"h105a16">>, <<"u995001/t105/att16.jpg">>, <<"teaching">>, 0}
    ],
    lists:foreach(
        fun({Id, Hash, Path, Scope, AgeHours}) ->
            exec(C, <<
                "INSERT INTO attachment (id, file_hash256, path, url, mime_type, "
                "creator_user_id, scope) VALUES (",
                (integer_to_binary(Id))/binary,
                ", '",
                Hash/binary,
                "', '",
                Path/binary,
                "', '",
                Path/binary,
                "', 'video/mp4', 995001, '",
                Scope/binary,
                "')"
            >>),
            case AgeHours of
                0 ->
                    ok;
                H ->
                    exec(C, <<
                        "UPDATE attachment SET created_at = now() - interval '",
                        (integer_to_binary(H))/binary,
                        " hours' WHERE id = ",
                        (integer_to_binary(Id))/binary
                    >>)
            end
        end,
        Rows
    ),
    exec(C, <<
        "INSERT INTO review_asset (id, review_id, attachment_id, kind, created_by) VALUES "
        "(995911, 995801, 995611, 'feedback_video', 995001), "
        "(995912, 995801, 995612, 'feedback_image', 995001), "
        "(995913, 995802, 995613, 'feedback_video', 995001), "
        "(995914, 995803, 995614, 'feedback_image', 995001)"
    >>).

%% review_for_asset_path 数据面：draft/published/withdrawn/unbound/missing
review_asset_path_resolution_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        %% draft review 的附件：review_status=draft + reviewer_uid + submission submitted
        {ok, #{
            <<"review_id">> := 995801,
            <<"review_status">> := <<"draft">>,
            <<"reviewer_uid">> := ?RV_TEACHER,
            <<"submission_id">> := ?RV_SUB1,
            <<"submission_status">> := <<"submitted">>
        }} = teaching_review_repo:review_for_asset_path_tx(C, <<"u995001/t105/att11.mp4">>),
        %% published review 的附件：review_status=published
        {ok, #{<<"review_status">> := <<"published">>, <<"submission_id">> := ?RV_SUB1}} =
            teaching_review_repo:review_for_asset_path_tx(C, <<"u995001/t105/att13.mp4">>),
        %% 撤回 submission 上的 draft 草稿附件：submission_status=withdrawn（授权层 fail closed）
        {ok, #{<<"submission_status">> := <<"withdrawn">>}} =
            teaching_review_repo:review_for_asset_path_tx(C, <<"u995001/t105/att14.jpg">>),
        %% 未绑定附件 → undefined
        {ok, undefined} =
            teaching_review_repo:review_for_asset_path_tx(C, <<"u995001/t105/att15.jpg">>),
        %% 不存在路径 → undefined
        {ok, undefined} =
            teaching_review_repo:review_for_asset_path_tx(C, <<"u995001/t105/nope.jpg">>)
    end).

%% 孤儿清理双排除：被 submission_asset 或 review_asset（含草稿引用）引用一律不列
unbound_double_exclusion_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        %% 额外：submission_asset 绑定的超龄附件（对照：submission 维度也不误删）
        exec(C, <<
            "INSERT INTO attachment (id, file_hash256, path, url, mime_type, "
            "creator_user_id, scope) VALUES "
            "(995617, 'h105a17', 'u995001/t105/att17.jpg', 'u995001/t105/att17.jpg', "
            "'image/jpeg', 995001, 'teaching')"
        >>),
        exec(C, <<
            "UPDATE attachment SET created_at = now() - interval '25 hours' WHERE id = 995617"
        >>),
        exec(C, <<
            "INSERT INTO submission_asset (id, submission_id, attachment_id, kind, created_by) "
            "VALUES (995917, 995701, 995617, 'final_photo', 995002)"
        >>),
        {ok, Rows} = teaching_submission_repo:unbound_teaching_attachments_tx(C, 24),
        Ids = lists:sort([maps:get(<<"id">>, R) || R <- Rows]),
        %% 恰好只有 995615（超龄+未绑定）：
        %% 995611/995612 draft review 引用不列；995613 published 引用不列；
        %% 995614 撤回 submission 草稿引用不列；995616 新近不列；995617 submission 绑定不列
        ?assertEqual([995615], Ids)
    end).

%% review_asset 读取：按 kind/sort_order 排序返回
review_assets_read_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        {ok, Rows} = teaching_review_repo:assets_tx(C, 995801),
        ?assertEqual(
            [
                {995612, <<"feedback_image">>, 0},
                {995611, <<"feedback_video">>, 0}
            ],
            [
                {
                    maps:get(<<"attachment_id">>, R),
                    maps:get(<<"kind">>, R),
                    maps:get(<<"sort_order">>, R)
                }
             || R <- Rows
            ]
        )
    end).

%% attachment 全表唯一：同一附件绑第二个 review → unique_violation（uk_review_asset_attachment）
review_asset_attachment_globally_unique_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        Err =
            try
                exec(C, <<
                    "INSERT INTO review_asset (id, review_id, attachment_id, kind, created_by) "
                    "VALUES (995918, 995802, 995611, 'feedback_video', 995001)"
                >>),
                no_error
            catch
                _:{sql_error, Reason} -> Reason
            end,
        ?assertEqual(<<"23505">>, sql_error_code(Err))
    end).

%% 单 review 单视频：第二个 feedback_video → unique_violation（uk_review_asset_one_video_per_review）
review_asset_one_video_per_review_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        Err =
            try
                exec(C, <<
                    "INSERT INTO attachment (id, file_hash256, path, url, mime_type, "
                    "creator_user_id, scope) VALUES "
                    "(995619, 'h105a19', 'u995001/t105/att19.mp4', 'u995001/t105/att19.mp4', "
                    "'video/mp4', 995001, 'teaching')"
                >>),
                exec(C, <<
                    "INSERT INTO review_asset (id, review_id, attachment_id, kind, created_by) "
                    "VALUES (995919, 995801, 995619, 'feedback_video', 995001)"
                >>),
                no_error
            catch
                _:{sql_error, Reason} -> Reason
            end,
        ?assertEqual(<<"23505">>, sql_error_code(Err))
    end).

%% 3 图上限：单语句插入第 4 张 feedback_image → trg_review_asset_image_cap RAISE（23514）
review_asset_image_cap_test_() ->
    with_tx(fun(C) ->
        rv_seed(C),
        rv_seed_attachments(C),
        %% 造 3 张新图（995621-995623）+ 第 4 张（995624），单语句插 4 行
        lists:foreach(
            fun(I) ->
                Bin = integer_to_binary(I),
                exec(C, <<
                    "INSERT INTO attachment (id, file_hash256, path, url, mime_type, "
                    "creator_user_id, scope) VALUES (9956",
                    Bin/binary,
                    ", 'h105a",
                    Bin/binary,
                    "', 'u995001/t105/att",
                    Bin/binary,
                    ".jpg', 'u995001/t105/att",
                    Bin/binary,
                    ".jpg', 'image/jpeg', 995001, 'teaching')"
                >>)
            end,
            [21, 22, 23, 24]
        ),
        Err =
            try
                %% SAVEPOINT 隔离预期失败的语句（否则事务 aborted 无法复查）
                exec(C, <<"SAVEPOINT sp_image_cap">>),
                exec(C, <<
                    "INSERT INTO review_asset (id, review_id, attachment_id, kind, created_by) VALUES "
                    "(995921, 995802, 995621, 'feedback_image', 995001), "
                    "(995922, 995802, 995622, 'feedback_image', 995001), "
                    "(995923, 995802, 995623, 'feedback_image', 995001), "
                    "(995924, 995802, 995624, 'feedback_image', 995001)"
                >>),
                no_error
            catch
                _:{sql_error, Reason} -> Reason
            end,
        ?assertEqual(<<"23514">>, sql_error_code(Err)),
        exec(C, <<"ROLLBACK TO SAVEPOINT sp_image_cap">>),
        %% 语句被触发器整体拒绝（语句级原子）：4 行零部分写入
        [#{<<"c">> := 0}] = q(C, <<
            "SELECT count(*) AS c FROM review_asset WHERE review_id = 995802 "
            "AND kind = 'feedback_image'"
        >>)
    end).

%% epgsql 错误码提取（覆盖三种形态）：
%%   {error, error, Code, Name, Msg, Extra}（equery 经 elib_pg 透传，exec 抛出后即此形态）
%%   {error, {error, error, Code, Name, Msg, Extra}}（外层再包一层 error 的形态）
%%   {pgsql_error, #{<<"code">> := Code}}
sql_error_code({error, error, Code, _Name, _Msg, _Extra}) ->
    Code;
sql_error_code({error, {error, error, Code, _Name, _Msg, _Extra}}) ->
    Code;
sql_error_code({pgsql_error, #{<<"code">> := Code}}) ->
    Code;
sql_error_code(_) ->
    undefined.
