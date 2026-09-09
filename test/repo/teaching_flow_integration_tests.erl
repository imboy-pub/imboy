%% teaching_flow_integration_tests
%% FLOW-01 / IDEMP-01 / STATE-01 — moya 教学回课闭环真库集成测试。
%%
%% 直连 moya_mig_test@127.0.0.1:4323（scratch，00000001→00000098 全量态），
%% 每用例 BEGIN ... ROLLBACK，不留数据。测试直接驱动 Repo 的 _tx 函数
%% （与生产 elib_pg:with_tx 同一代码路径），验证 STEP-08-DB 配方①②③：
%%   ① 幂等 CTE（ON CONFLICT 带部分索引谓词）+ digest 判 5460
%%   ② attempt FOR UPDATE 取号
%%   ③ 撤回/发布 lock-first 互斥
%% DB 不可达时自动 skip（本地无 scratch 库的 CI 环境）。

-module(teaching_flow_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- 夹具 ----
-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_mig_test">>).

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
