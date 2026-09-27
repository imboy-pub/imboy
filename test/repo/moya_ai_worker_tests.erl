%% moya_ai_worker_tests
%% AI-01 / AI-03（Worker 侧落库路径）— 墨芽书法 AI 回课 Worker 真库集成测试（Step 11）。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 MOYA_INTTEST，全链迁移
%% 至当前 head），每用例 BEGIN ... ROLLBACK，不留数据（模式照
%% moya_flow_integration_tests）。供给失败显式 FAIL，无静默 skip。
%%
%% 外部依赖全部 meck（零真实网络、零真实模型密钥）：
%%   imboy_llm_registry:lookup/1、provider（imboy_llm_qianfan 的 chat/3 与
%%   capabilities/0，全量替换）、config_ds:env/2；
%%   elib_pg:query/2（全局池版）转发到测试连接 C —— Worker 的
%%   load_context/load_attachment 走全局池，直连模式下经此路由到同一事务。
%%   带 Conn 的 _tx 函数（claim/finish/requeue/publish 等）不经 mock，走真实实现。
%%
%% 覆盖（验收 AI-01 五路径 + 重试上限 + AI-03 降级闭环）：
%%   成功        —— queued→running→succeeded，result_json 白名单 + model_profile 落库
%%   超时        —— requeued（run:2 回队、error_code 清空）→ 再超时达上限=failed(timeout)
%%   provider 错误 —— 显式例外对照：provider_unavailable 不重试直接 failed
%%   附件删除    —— submission_asset 解绑 → failed(attachment_missing)，chat 0 次
%%   撤回        —— submission withdrawn → failed(submission_withdrawn)
%%   媒体超限    —— size>100MB → failed(media_too_large)，chat 0 次
%%   AI-03       —— registry 未命中 → failed(provider_unavailable) 后老师人工
%%                  发布回评在同一事务仍通过（核心回课闭环不破）
%%   队列 FIFO   —— 两 queued 只处理最老一条；空队列 → {ok, done}

-module(moya_ai_worker_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- 夹具（marker 库；ID 段 97 前缀与 flow-98/bind-99 错开） ----
-define(TEACHER, 970001).
-define(PARENT, 970002).
-define(ORG_A, 971000).
-define(WS_A, 972001).
-define(GROUP_A1, 973001).
-define(LEARNER_A1, 974001).
-define(TASK_ID, <<"task11_hash_001">>).
-define(ASSIGN_ID, 976001).
-define(ATT_VIDEO, 978001).
-define(ATT_PHOTO, 978002).
-define(SUB_ID_BASE, 977000).

-define(PROVIDER_NAME, <<"moya-fake">>).
-define(MODEL_NAME, <<"glm-test-model">>).
-define(FAKE_MOD, imboy_llm_qianfan).

%%%===================================================================
%%% Fixture：连接 + meck 安装（每用例独立）
%%%===================================================================

setup_all() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    {ok, _} = application:ensure_all_started(meck),
    %% 一次性 marker 库（inttest_marker_db 配方）：env 覆盖（<= imboy.pg_conf
    %% 回退）→ 建库 → 12 扩展 → erlang_migrate:up 全链；任一失败显式 error。
    %% A1c：reuse => true —— 全套件 18 个 ai_tx 用例共享同一 marker 库/连接
    %% （VM 级引用计数，末位 release 才 DROP）。本套件用例全部 BEGIN/ROLLBACK
    %% 自持事务且串行，共享连接安全；全量长跑下 Docker 端口转发会对高连接数
    %% 的 VM 按进程 econnrefused（evidence/CP-TD-A02 full_run1/2 实证），
    %% 把 18 次 provision 压到 1 次，避开 refused 窗口。
    State = inttest_marker_db:provision(#{env_prefix => <<"MOYA_INTTEST">>,
                                          reuse => true}),
    ok = install_mocks(),
    State.

cleanup_all(State) ->
    uninstall_mocks(),
    inttest_marker_db:release(State),
    ok.

%% 真库事务 + mock 生效域。chat 返回值经进程字典 fake_chat 注入
%% （undefined → 合法结构化 JSON；{error, ...} / {ok, #{content...}} 直传）。
ai_tx(TestFun) ->
    {timeout, 900,
        {setup, fun setup_all/0, fun cleanup_all/1, fun(State) ->
            C = maps:get(conn, State),
            ?_test(begin
                ok = exec(C, <<"BEGIN">>),
                put(moya_test_conn, C),
                try
                    TestFun(C),
                    ok
                after
                    erase(moya_test_conn),
                    erase(fake_chat),
                    erase(fake_lookup),
                    erase(fake_provider_name),
                    erase(fake_with_tx),
                    erase(fake_with_tx_calls),
                    exec(C, <<"ROLLBACK">>)
                end
            end)
        end}}.

install_mocks() ->
    Mocks = [
        {config_ds, [
            {'env', 2, fun
                (teaching_ai_llm_provider, _) ->
                    case get(fake_provider_name) of
                        unconfigured -> undefined;
                        undefined -> ?PROVIDER_NAME;
                        V -> V
                    end;
                (teaching_ai_max_retries, _) ->
                    2;
                (_, Default) ->
                    Default
            end}
        ]},
        {imboy_llm_registry, [
            {'lookup', 1, fun(_) ->
                case get(fake_lookup) of
                    miss ->
                        undefined;
                    undefined ->
                        {ok, #{
                            module => ?FAKE_MOD,
                            opts => #{api_key => <<"test-key-placeholder">>, model => ?MODEL_NAME}
                        }};
                    V ->
                        V
                end
            end}
        ]},
        {?FAKE_MOD, [
            {'capabilities', 0, fun() -> #{vision => true} end},
            {'chat', 3, fun(_Uid, _Messages, _Opts) ->
                case get(fake_chat) of
                    undefined -> {ok, #{<<"content">> => jsone:encode(valid_result())}};
                    R -> R
                end
            end}
        ]},
        %% Worker 的 load_context/load_attachment 走全局池 query/2：
        %% 直连模式下转发到测试连接（同事务可见种子与 ROLLBACK 隔离）。
        %% 带 Conn 的 query/3 未 mock（passthrough），_tx 函数走真实实现。
        {elib_pg, [
            {'query', 2, fun(Sql, Params) ->
                C = get(moya_test_conn),
                elib_pg:query(C, Sql, Params)
            end},
            %% run_once/reclaim_stuck 池入口：with_tx 转发为直接执行 Tx(C)
            %% （外层已有 BEGIN..ROLLBACK，不开嵌套事务；R7 池入口测试用。
            %%   fake_with_tx 哨兵可注入 {error, _} 测 fail-closed 路径）
            {'with_tx', 1, fun(Tx) -> pool_tx(Tx) end},
            {'with_tx', 2, fun(Tx, _Opts) -> pool_tx(Tx) end}
        ]}
    ],
    lists:foreach(
        fun({Mod, Exp}) ->
            case meck_helper:setup_mock(Mod, Exp) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({mock_setup_failed, Mod, Reason})
            end
        end,
        Mocks
    ),
    ok.

uninstall_mocks() ->
    lists:foreach(
        fun(Mod) ->
            try
                meck_helper:cleanup_mock(Mod)
            catch
                _:_ -> ok
            end
        end,
        [config_ds, imboy_llm_registry, ?FAKE_MOD, elib_pg]
    ),
    ok.

%% with_tx 转发体：正常时直接执行 Tx(测试连接)（外层事务兜底）；
%% fake_with_tx 哨兵注入失败形态测 fail-closed：
%%   {fail, Reason}     —— 每次都失败（claim 失败 → claim_error）
%%   {fail_second, Reason} —— 首次成功、第二次失败（claim 成功后 process 失败 → worker_error）
pool_tx(Tx) ->
    case get(fake_with_tx) of
        {fail, Reason} ->
            {error, Reason};
        {fail_second, Reason} ->
            case get(fake_with_tx_calls) of
                undefined ->
                    put(fake_with_tx_calls, 1),
                    pool_tx_run(Tx);
                _ ->
                    {error, Reason}
            end;
        _ ->
            pool_tx_run(Tx)
    end.

pool_tx_run(Tx) ->
    C = get(moya_test_conn),
    Tx(C).

%%%===================================================================
%%% Seed（与 moya_flow_integration_tests 同构；机构/班/学员/作业/附件）
%%%===================================================================

seed(C) ->
    exec(C, <<
        "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES "
        "(970001, 'x', 't11_teacher', '127.0.0.1', 'x'), "
        "(970002, 'x', 't11_parent', '127.0.0.1', 'x')"
    >>),
    exec(C, <<"INSERT INTO organization (id, name, owner_id) VALUES (971000, 'orgA11', 970001)">>),
    exec(C, <<
        "INSERT INTO workspace (id, name, owner_id, organization_id) VALUES "
        "(972001, 'A-hq11', 970001, 971000)"
    >>),
    exec(C, <<
        "INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, status) "
        "VALUES (972001, 970001, 'owner', 970001, 'active'), "
        "(972001, 970002, 'member', 970001, 'active')"
    >>),
    exec(C, <<
        "INSERT INTO \"group\" (id, owner_uid, creator_uid, scope, workspace_id, title) "
        "VALUES (973001, 970001, 970001, 'workspace', 972001, 'A1-class11')"
    >>),
    exec(C, <<
        "INSERT INTO learner (id, organization_id, display_name) VALUES "
        "(974001, 971000, 'L-11')"
    >>),
    exec(C, <<"INSERT INTO class_enrollment (group_id, learner_id) VALUES (973001, 974001)">>),
    exec(C, <<
        "INSERT INTO class_staff (group_id, user_id, role) VALUES "
        "(973001, 970001, 'teacher')"
    >>),
    exec(C, <<
        "INSERT INTO guardian_learner (guardian_uid, learner_id, can_submit, can_view_review) "
        "VALUES (970002, 974001, true, true)"
    >>),
    exec(C, <<
        "INSERT INTO group_task (id, group_id, task_id, title, creator_id, status) VALUES "
        "(975001, 973001, '",
        (?TASK_ID)/binary,
        "', 'task-11', 970001, 1)"
    >>),
    exec(C, <<
        "INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) VALUES "
        "(976001, '",
        (?TASK_ID)/binary,
        "', 970002, 974001)"
    >>),
    exec(C, <<"SET CONSTRAINTS ALL IMMEDIATE">>),
    exec(C, <<
        "INSERT INTO attachment (id, file_hash256, path, mime_type, creator_user_id) VALUES "
        "(978001, 'h11video', 'p/978001', 'video/mp4', 970002), "
        "(978002, 'h11photo', 'p/978002', 'image/jpeg', 970002)"
    >>).

%%%===================================================================
%%% AI-01 路径 1：成功 —— succeeded + result_json 白名单 + model_profile
%%%===================================================================

ai01_success_path_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, <<"idem-ai11-ok">>),
        {ok, #{outcome := succeeded, result := Result}} = moya_ai_worker:run_once_tx(C),
        %% chat 恰好调用一次（经过 resolve→capabilities→chat 全链）
        ?assertEqual(1, meck:num_calls(?FAKE_MOD, chat, 3)),
        %% DB 终态：succeeded + 白名单 jsonb + model_profile + completed_at
        [Draft] = q(C, <<
            "SELECT status, result_json, model_profile, completed_at, error_code "
            "FROM calligraphy_review_draft WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>),
        ?assertEqual(<<"succeeded">>, maps:get(<<"status">>, Draft)),
        ?assertEqual(?MODEL_NAME, maps:get(<<"model_profile">>, Draft)),
        ?assert(maps:get(<<"completed_at">>, Draft) =/= null),
        ?assertEqual(null, maps:get(<<"error_code">>, Draft)),
        Decoded = jsone:decode(maps:get(<<"result_json">>, Draft)),
        ?assertEqual(lists:sort(maps:keys(Result)), lists:sort(maps:keys(Decoded))),
        %% 白名单契约：无思维链键（AI-02 交叉）
        ?assertEqual(false, maps:is_key(<<"reasoning">>, Decoded))
    end).

%%%===================================================================
%%% AI-01 路径 2：超时 —— requeued(run:2) → 再超时达上限 failed(timeout)
%%%===================================================================

ai01_timeout_retry_then_cap_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, _Sid} = create_submission(C, <<"idem-ai11-timeout">>),
        _ = put(fake_chat, {error, timeout}),
        %% 第 1 次：ai_task_id NULL → attempt=1 < max(2) → 回队 run:2
        %%（返回的 attempt 字段 = 下一次尝试号 Attempt+1，经调试脚本对照源码确认）
        {ok, #{outcome := requeued, attempt := 2, reason := timeout}} =
            moya_ai_worker:run_once_tx(C),
        [Requeued] = q(C, <<
            "SELECT status, ai_task_id, error_code, completed_at "
            "FROM calligraphy_review_draft WHERE status = 'queued'"
        >>),
        ?assertEqual(<<"run:2">>, maps:get(<<"ai_task_id">>, Requeued)),
        ?assertEqual(null, maps:get(<<"error_code">>, Requeued)),
        ?assertEqual(null, maps:get(<<"completed_at">>, Requeued)),
        %% 第 2 次：attempt=2 = max → 终态 failed(timeout)
        {ok, #{outcome := failed, error_code := <<"timeout">>}} =
            moya_ai_worker:run_once_tx(C),
        [Failed] = q(C, <<
            "SELECT status, error_code, completed_at FROM calligraphy_review_draft "
            "WHERE status = 'failed'"
        >>),
        ?assertEqual(<<"timeout">>, maps:get(<<"error_code">>, Failed)),
        ?assert(maps:get(<<"completed_at">>, Failed) =/= null),
        %% 全程两次 provider 调用（重试上限=2 的显式体现）
        ?assertEqual(2, meck:num_calls(?FAKE_MOD, chat, 3))
    end).

%%%===================================================================
%%% AI-01 路径 3：provider 错误类 —— provider_unavailable 显式不重试直接终态
%%%（timeout/provider_error 可重试已在上一用例覆盖；此处断言例外分支）
%%%===================================================================

ai01_provider_unavailable_no_retry_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, _Sid} = create_submission(C, <<"idem-ai11-pu">>),
        _ = put(fake_lookup, miss),
        _ = put(fake_provider_name, unconfigured),
        %% provider 名未配置 → provider_unavailable（AI-03 主降级路径）
        {ok, #{outcome := failed, error_code := <<"provider_unavailable">>}} =
            moya_ai_worker:run_once_tx(C),
        [Failed] = q(C, <<
            "SELECT status, error_code FROM calligraphy_review_draft "
            "WHERE status = 'failed'"
        >>),
        ?assertEqual(<<"provider_unavailable">>, maps:get(<<"error_code">>, Failed)),
        %% 显式例外：不重试（queue 无 run:2 残留、chat 零调用 = 零外呼）
        ?assertEqual(0, meck:num_calls(?FAKE_MOD, chat, 3)),
        [] = q(C, <<"SELECT id FROM calligraphy_review_draft WHERE ai_task_id LIKE 'run:%'">>)
    end).

%%%===================================================================
%%% AI-01 路径 4：非法 JSON —— bad_output 不重试（bad_schema 终态）
%%%===================================================================

ai01_bad_json_failed_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, _Sid} = create_submission(C, <<"idem-ai11-badjson">>),
        _ = put(fake_chat, {ok, #{<<"content">> => <<"这不是 JSON 而是自由文本"/utf8>>}}),
        %% bad_output 不在 TRANSIENT_ERRORS：直接终态 failed(bad_schema)
        {ok, #{outcome := failed, error_code := <<"bad_schema">>}} =
            moya_ai_worker:run_once_tx(C),
        [Failed] = q(C, <<
            "SELECT status, error_code FROM calligraphy_review_draft "
            "WHERE status = 'failed'"
        >>),
        ?assertEqual(<<"bad_schema">>, maps:get(<<"error_code">>, Failed)),
        ?assertEqual(1, meck:num_calls(?FAKE_MOD, chat, 3))
    end).

%%%===================================================================
%%% AI-01 路径 5：附件删除 —— 绑定关系不存在 → failed(attachment_missing)，零外呼
%%%===================================================================

ai01_attachment_deleted_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, <<"idem-ai11-att-del">>),
        %% 附件删除形态：practice_video 绑定关系移除（系统侧 authorize 等价物）
        ok = exec(
            C,
            <<"DELETE FROM submission_asset WHERE submission_id = ",
                (integer_to_binary(Sid))/binary, " AND kind = 'practice_video'">>
        ),
        {ok, #{outcome := failed, error_code := <<"attachment_missing">>}} =
            moya_ai_worker:run_once_tx(C),
        [Failed] = q(C, <<
            "SELECT status, error_code FROM calligraphy_review_draft "
            "WHERE status = 'failed'"
        >>),
        ?assertEqual(<<"attachment_missing">>, maps:get(<<"error_code">>, Failed)),
        %% 资源门在 provider 之前：chat 零调用
        ?assertEqual(0, meck:num_calls(?FAKE_MOD, chat, 3))
    end).

%%%===================================================================
%%% 资源门：撤回 → failed(submission_withdrawn)，零外呼
%%%===================================================================

ai01_submission_withdrawn_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, <<"idem-ai11-wd">>),
        %% 真实撤回路径（审计列齐全）
        {ok, withdrawn} = moya_submission_repo:withdraw_tx(C, Sid, ?PARENT),
        {ok, #{outcome := failed, error_code := <<"submission_withdrawn">>}} =
            moya_ai_worker:run_once_tx(C),
        ?assertEqual(0, meck:num_calls(?FAKE_MOD, chat, 3))
    end).

%%%===================================================================
%%% 媒体复核：size 超 100MB → failed(media_too_large)，零外呼
%%%===================================================================

ai01_media_too_large_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, _Sid} = create_submission(C, <<"idem-ai11-big">>),
        ok = exec(
            C,
            <<"UPDATE attachment SET size = 157286400 WHERE id = ",
                (integer_to_binary(?ATT_VIDEO))/binary>>
        ),
        {ok, #{outcome := failed, error_code := <<"media_too_large">>}} =
            moya_ai_worker:run_once_tx(C),
        ?assertEqual(0, meck:num_calls(?FAKE_MOD, chat, 3))
    end).

%%%===================================================================
%%% AI-03：降级到人工队列后，核心回课闭环同一事务仍通过
%%%===================================================================

ai03_degrade_human_loop_still_works_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, <<"idem-ai11-human">>),
        %% 模拟现实主路径：无可用 vision provider（registry 未命中）
        _ = put(fake_lookup, miss),
        {ok, #{outcome := failed, error_code := <<"provider_unavailable">>}} =
            moya_ai_worker:run_once_tx(C),
        %% AI failed 不阻断人工：老师直接发布人工回评（不经 AI）
        {ok, _} = moya_review_repo:upsert_draft_tx(C, Sid, #{
            uid => ?TEACHER,
            positive_point => <<"横画稳，人工点评"/utf8>>,
            focus_problem => <<"竖画歪"/utf8>>,
            practice_action => <<"每天三行竖画"/utf8>>,
            comment => <<>>,
            video_attachment_id => undefined,
            rework_required => false
        }),
        {ok, _Row} = moya_submission_repo:lock_submission_tx(C, Sid),
        {ok, published, Pub} = moya_review_repo:publish_tx(C, Sid, ?TEACHER),
        ?assertEqual(<<"published">>, maps:get(<<"status">>, Pub)),
        ?assert(maps:get(<<"published_at">>, Pub) =/= null),
        %% 家长可读已发布回评（闭环末端）
        {ok, #{<<"id">> := _PubId}} = moya_review_repo:find_published_tx(C, Sid)
    end).

%%%===================================================================
%%% 队列语义：FIFO（最老先处理）；空队列 {ok, done}
%%%===================================================================

claim_fifo_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, SidOld} = create_submission(C, <<"idem-ai11-fifo-1">>),
        {ok, SidNew} = create_submission(C, <<"idem-ai11-fifo-2">>),
        %% 同事务 CURRENT_TIMESTAMP 恒定：显式错开 created_at 保证 FIFO 确定性
        ok = exec(C, <<
            "UPDATE calligraphy_review_draft SET created_at = now() - interval '1 hour' "
            "WHERE submission_id = ",
            (integer_to_binary(SidOld))/binary
        >>),
        {ok, #{outcome := _}} = moya_ai_worker:run_once_tx(C),
        %% 只处理了最老的一条（succeeded），另一条仍 queued
        [#{<<"submission_id">> := SidOld}] = q(C, <<
            "SELECT submission_id FROM calligraphy_review_draft "
            "WHERE status = 'succeeded'"
        >>),
        [#{<<"submission_id">> := SidNew}] = q(C, <<
            "SELECT submission_id FROM calligraphy_review_draft "
            "WHERE status = 'queued'"
        >>)
    end).

queue_empty_done_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        ?assertEqual({ok, done}, moya_ai_worker:run_once_tx(C))
    end).

%%%===================================================================
%%% Helpers：走生产同款配方（与 moya_flow_integration_tests 一致）
%%%===================================================================

exec(C, Sql) ->
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

q(C, Sql) ->
    case elib_pg:query(C, Sql, []) of
        {ok, Rows} -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

-spec create_submission(pid(), binary()) -> {ok, integer()}.
create_submission(C, IdemKey) ->
    {ok, ?ASSIGN_ID} = moya_submission_repo:lock_assignment_tx(C, ?ASSIGN_ID),
    {ok, Next} = moya_submission_repo:next_attempt_tx(C, ?ASSIGN_ID),
    {ok, #{<<"id">> := Sid} = Row} =
        moya_submission_repo:create_idempotent_tx(C, #{
            id => ?SUB_ID_BASE + Next,
            assignment_id => ?ASSIGN_ID,
            learner_id => ?LEARNER_A1,
            uid => ?PARENT,
            attempt_no => Next,
            idempotency_key => IdemKey,
            request_digest => <<"digest-ai11">>
        }),
    case maps:get(created, Row, false) of
        true ->
            ok = moya_submission_repo:insert_assets_tx(C, Sid, ?PARENT, [
                {?ATT_VIDEO, <<"practice_video">>, 0},
                {?ATT_PHOTO, <<"final_photo">>, 0}
            ]),
            ok = moya_submission_repo:mark_submitted_by_tx(C, ?ASSIGN_ID, ?PARENT),
            ok = moya_submission_repo:enqueue_ai_draft_tx(C, Sid);
        false ->
            ok
    end,
    {ok, Sid}.

valid_result() ->
    #{
        <<"positive_point">> => <<"执笔姿势稳定，横画起收笔干净"/utf8>>,
        <<"focus_problem">> => <<"竖画整体右倾，重心不稳"/utf8>>,
        <<"practice_action">> => <<"每天三行悬针竖，对格线书写"/utf8>>,
        <<"evidence_moments">> => [1.5, 8.2],
        <<"script_outline">> => [<<"先肯定坐姿"/utf8>>, <<"再演示竖画回正"/utf8>>],
        <<"needs_human_check">> => false,
        <<"confidence">> => 0.9,
        <<"reasoning">> => <<"思维链片段不落库"/utf8>>
    }.

%%%===================================================================
%%% R7 任务1：run_once/0 生产池入口（claim→处理→finish 全链 + fail-closed）
%%%（with_tx mock 转发 Tx(测试连接)；正常路径不开嵌套事务，外层 ROLLBACK 兜底）
%%%===================================================================

%% 池版成功：{ok, {processed, Outcome}}；DB 终态与 tx 版一致
run_once_pool_success_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, <<"idem-r7-pool-ok">>),
        {ok, {processed, #{outcome := succeeded, result := _}}} = moya_ai_worker:run_once(),
        [#{<<"status">> := <<"succeeded">>}] = q(C, <<
            "SELECT status FROM calligraphy_review_draft "
            "WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>)
    end).

%% 池版空队列：{ok, done}
run_once_pool_empty_test_() ->
    ai_tx(fun(_C) ->
        ?assertEqual({ok, done}, moya_ai_worker:run_once())
    end).

%% 连接/事务失败（claim 阶段）→ {error, claim_error}，行仍 queued（外层回滚语义）
run_once_pool_claim_fail_closed_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, <<"idem-r7-pool-claimfail">>),
        _ = put(fake_with_tx, {fail, conn_lost}),
        try
            ?assertEqual({error, claim_error}, moya_ai_worker:run_once()),
            %% fail-closed：行未被认领仍 queued
            [#{<<"status">> := <<"queued">>}] = q(C, <<
                "SELECT status FROM calligraphy_review_draft "
                "WHERE submission_id = ",
                (integer_to_binary(Sid))/binary
            >>)
        after
            erase(fake_with_tx)
        end
    end).

%% 处理事务失败（claim 成功后 process 事务异常）→ {error, worker_error}
run_once_pool_process_fail_closed_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, _Sid} = create_submission(C, <<"idem-r7-pool-procfail">>),
        _ = put(fake_with_tx, {fail_second, process_crash}),
        try
            ?assertEqual({error, worker_error}, moya_ai_worker:run_once())
        after
            erase(fake_with_tx),
            erase(fake_with_tx_calls)
        end
    end).

%%%===================================================================
%%% R7 任务3：stuck-row 回收（阈值下限强制 + 手动/池入口 + 计数延续）
%%%===================================================================

%% running 行超龄回收回 queued；阈值内的 running 行不动；其他状态不动
reclaim_stuck_basic_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, SidOld} = create_submission(C, <<"idem-r7-stuck-old">>),
        {ok, SidFresh} = create_submission(C, <<"idem-r7-stuck-fresh">>),
        %% 两个 running：一个卡死 1 小时，一个刚认领
        ok = exec(C, <<
            "UPDATE calligraphy_review_draft SET status = 'running', "
            "created_at = now() - interval '1 hour' WHERE submission_id = ",
            (integer_to_binary(SidOld))/binary
        >>),
        ok = exec(C, <<
            "UPDATE calligraphy_review_draft SET status = 'running' "
            "WHERE submission_id = ",
            (integer_to_binary(SidFresh))/binary
        >>),
        {ok, 1} = moya_ai_worker:reclaim_stuck_tx(C, 600),
        %% 超龄行回 queued；新行仍 running
        [#{<<"status">> := <<"queued">>}] = q(C, <<
            "SELECT status FROM calligraphy_review_draft "
            "WHERE submission_id = ",
            (integer_to_binary(SidOld))/binary
        >>),
        [#{<<"status">> := <<"running">>}] = q(C, <<
            "SELECT status FROM calligraphy_review_draft "
            "WHERE submission_id = ",
            (integer_to_binary(SidFresh))/binary
        >>)
    end).

%% 阈值下限钳制：传 1 秒（<300 下限）按 300s 执行——刚认领 10 秒的行不被误回收
reclaim_stuck_min_age_clamp_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, <<"idem-r7-stuck-clamp">>),
        ok = exec(C, <<
            "UPDATE calligraphy_review_draft SET status = 'running', "
            "created_at = now() - interval '10 seconds' WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>),
        %% 若未钳制，1 秒阈值会回收 10 秒的行；钳制后不回收
        {ok, 0} = moya_ai_worker:reclaim_stuck_tx(C, 1),
        [#{<<"status">> := <<"running">>}] = q(C, <<
            "SELECT status FROM calligraphy_review_draft "
            "WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>),
        %% 回拨超 300s 后即回收
        ok = exec(C, <<
            "UPDATE calligraphy_review_draft SET created_at = "
            "now() - interval '10 minutes' WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>),
        {ok, 1} = moya_ai_worker:reclaim_stuck_tx(C, 1),
        [#{<<"status">> := <<"queued">>}] = q(C, <<
            "SELECT status FROM calligraphy_review_draft "
            "WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>)
    end).

%% 回收后重跑 worker：坏行（timeout 毒行）计数延续快速达上限 failed（防永动）
reclaim_stuck_retry_budget_preserved_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, _Sid} = create_submission(C, <<"idem-r7-stuck-poison">>),
        _ = put(fake_chat, {error, timeout}),
        %% 首次：attempt 1 → requeue run:2
        {ok, #{outcome := requeued}} = moya_ai_worker:run_once_tx(C),
        %% 模拟 worker 崩溃残留：claim 后置 running（不 finish），回拨超龄
        {ok, _} = moya_review_repo:claim_next_queued_tx(C),
        ok = exec(C, <<
            "UPDATE calligraphy_review_draft SET created_at = "
            "now() - interval '1 hour' WHERE status = 'running'"
        >>),
        %% 回收（ai_task_id run:2 保留）
        {ok, 1} = moya_ai_worker:reclaim_stuck_tx(C, 600),
        [#{<<"ai_task_id">> := <<"run:2">>}] = q(C, <<
            "SELECT ai_task_id FROM calligraphy_review_draft "
            "WHERE status = 'queued'"
        >>),
        %% 重跑：attempt=2 = max → 直接 failed，不再 requeue（防毒行永动）
        {ok, #{outcome := failed, error_code := <<"timeout">>}} =
            moya_ai_worker:run_once_tx(C),
        %% 全程 chat 共 2 次：首次 + 回收后重跑（第 2 次达上限不再重试）
        ?assertEqual(2, meck:num_calls(?FAKE_MOD, chat, 3))
    end).

%% 池入口 reclaim_stuck/0：with_tx 转发 + env 阈值读取链
reclaim_stuck_pool_entry_test_() ->
    ai_tx(fun(C) ->
        seed(C),
        {ok, Sid} = create_submission(C, <<"idem-r7-stuck-pool">>),
        ok = exec(C, <<
            "UPDATE calligraphy_review_draft SET status = 'running', "
            "created_at = now() - interval '1 hour' WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>),
        {ok, 1} = moya_ai_worker:reclaim_stuck(),
        [#{<<"status">> := <<"queued">>}] = q(C, <<
            "SELECT status FROM calligraphy_review_draft "
            "WHERE submission_id = ",
            (integer_to_binary(Sid))/binary
        >>)
    end).

%%%===================================================================
%%% R7 任务2：SKIP LOCKED 双连接竞态（独立夹具：种子先提交，仅 claim 留在
%%% 未提交事务；无 meck）。关键点：A 连接事务内种子对 B 不可见（READ
%%% COMMITTED），故种子/建 submission 用 autocommit 先落库，两个 BEGIN 只
%%% 包 claim 与后续断言；cleanup 负责删除已提交种子（不留数据）。
%%%===================================================================

%% 两连接并发 claim：无双重认领（同一行不被两连接拿到）、无丢行（两 queued
%% 各被一连接认领）。A 认领后不提交，B 的 SKIP LOCKED 必须跳过 A 锁定行。
claim_skip_locked_no_double_claim_test_() ->
    {timeout, 900,
        {setup, fun setup_pair/0, fun cleanup_pair/1, fun({State, CB}) ->
            CA = maps:get(conn, State),
            ?_test(begin
                try
                    %% 种子先提交（autocommit）：队列行对两连接均可见
                    seed(CA),
                    {ok, SidOld} = create_submission(CA, <<"idem-r7-race-1">>),
                    {ok, SidNew} = create_submission(CA, <<"idem-r7-race-2">>),
                    %% FIFO 确定性：显式错开 created_at
                    ok = exec(CA, <<
                        "UPDATE calligraphy_review_draft SET created_at = "
                        "now() - interval '1 hour' WHERE submission_id = ",
                        (integer_to_binary(SidOld))/binary
                    >>),
                    %% 两连接各开事务，仅装 claim 竞态
                    ok = exec(CA, <<"BEGIN">>),
                    ok = exec(CB, <<"BEGIN">>),
                    {ok, A1} = moya_review_repo:claim_next_queued_tx(CA),
                    {ok, B1} = moya_review_repo:claim_next_queued_tx(CB),
                    %% 无双重认领/无丢行：两行各被一连接认领（FIFO：A 得老、B 得新）
                    ?assertMatch(#{<<"submission_id">> := SidOld}, A1),
                    ?assertMatch(#{<<"submission_id">> := SidNew}, B1),
                    %% 双视角认领态：各自事务内自己的行 running、对方未提交行仍 queued
                    [#{<<"status">> := <<"running">>}] =
                        q(
                            CA,
                            <<"SELECT status FROM calligraphy_review_draft WHERE submission_id = ",
                                (integer_to_binary(SidOld))/binary>>
                        ),
                    [#{<<"status">> := <<"queued">>}] =
                        q(
                            CA,
                            <<"SELECT status FROM calligraphy_review_draft WHERE submission_id = ",
                                (integer_to_binary(SidNew))/binary>>
                        ),
                    [#{<<"status">> := <<"running">>}] =
                        q(
                            CB,
                            <<"SELECT status FROM calligraphy_review_draft WHERE submission_id = ",
                                (integer_to_binary(SidNew))/binary>>
                        ),
                    [#{<<"status">> := <<"queued">>}] =
                        q(
                            CB,
                            <<"SELECT status FROM calligraphy_review_draft WHERE submission_id = ",
                                (integer_to_binary(SidOld))/binary>>
                        ),
                    ok
                after
                    rollback_soft(CA),
                    rollback_soft(CB)
                end
            end)
        end}}.

%% 单行竞态：A 认领唯一行后，B claim 得 undefined（不阻塞、不重复）；
%% A 回滚释放锁后 B 可再次认领同一行（回收语义基础）
claim_skip_locked_single_row_test_() ->
    {timeout, 900,
        {setup, fun setup_pair/0, fun cleanup_pair/1, fun({State, CB}) ->
            CA = maps:get(conn, State),
            ?_test(begin
                try
                    seed(CA),
                    {ok, _Sid} = create_submission(CA, <<"idem-r7-race-single">>),
                    ok = exec(CA, <<"BEGIN">>),
                    ok = exec(CB, <<"BEGIN">>),
                    {ok, #{<<"id">> := IdA}} = moya_review_repo:claim_next_queued_tx(CA),
                    %% B 视角该行被 A 锁定：SKIP → undefined
                    {ok, undefined} = moya_review_repo:claim_next_queued_tx(CB),
                    ok = exec(CA, <<"ROLLBACK">>),
                    %% A 回滚释放锁后：B 新语句（READ COMMITTED 新快照）可认领同一行
                    {ok, #{<<"id">> := IdA}} = moya_review_repo:claim_next_queued_tx(CB),
                    ok
                after
                    rollback_soft(CA),
                    rollback_soft(CB)
                end
            end)
        end}}.

%% ---- 双连接夹具（无 meck；种子已提交，cleanup 物理清理） ----

setup_pair() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    %% 一次性 marker 库（inttest_marker_db 配方）：CA = 夹具主连接；
    %% CB 从同一 marker 库再开一条（SKIP LOCKED 双连接竞态用）。
    State = inttest_marker_db:provision(#{env_prefix => <<"MOYA_INTTEST">>}),
    CA = maps:get(conn, State),
    #{
        db := Db,
        server := #{
            host := Host,
            port := Port,
            username := User,
            password := Pass
        }
    } = State,
    {ok, CB} = inttest_marker_db:safe_connect(#{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => Db,
        timeout => 5000
    }),
    %% 预清理：上一轮若中断残留已提交种子（best-effort；marker 库全新为 no-op）
    cleanup_seed(CA),
    {State, CB}.

cleanup_pair({State, CB}) ->
    CA = maps:get(conn, State),
    cleanup_seed(CA),
    try
        epgsql:close(CB)
    catch
        _:_ -> ok
    end,
    %% release 关闭 CA/maint 并 DROP 整库（已提交种子随库消失，不留数据）
    inttest_marker_db:release(State),
    ok.

%% 物理清理已提交种子（best-effort，逆 FK 序；本夹具专用——ai_tx 用例靠 ROLLBACK）
cleanup_seed(C) ->
    SubQ =
        <<"SELECT id FROM homework_submission WHERE assignment_id = ",
            (integer_to_binary(?ASSIGN_ID))/binary>>,
    Lists = [
        <<"DELETE FROM calligraphy_review_draft WHERE submission_id IN (", SubQ/binary, ")">>,
        <<"DELETE FROM teacher_review WHERE submission_id IN (", SubQ/binary, ")">>,
        <<"DELETE FROM submission_asset WHERE submission_id IN (", SubQ/binary, ")">>,
        <<"DELETE FROM homework_submission WHERE assignment_id = ",
            (integer_to_binary(?ASSIGN_ID))/binary>>,
        <<"DELETE FROM group_task_assignment WHERE id = ", (integer_to_binary(?ASSIGN_ID))/binary>>,
        <<"DELETE FROM group_task WHERE id = 975001">>,
        <<"DELETE FROM guardian_learner WHERE guardian_uid = 970002">>,
        <<"DELETE FROM class_staff WHERE group_id = 973001">>,
        <<"DELETE FROM class_enrollment WHERE group_id = 973001">>,
        <<"DELETE FROM learner WHERE id = 974001">>,
        <<"DELETE FROM \"group\" WHERE id = 973001">>,
        <<"DELETE FROM workspace_member WHERE workspace_id = 972001">>,
        <<"DELETE FROM workspace WHERE id = 972001">>,
        <<"DELETE FROM organization WHERE id = 971000">>,
        <<"DELETE FROM attachment WHERE id IN (978001, 978002)">>,
        <<"DELETE FROM \"user\" WHERE id IN (970001, 970002)">>
    ],
    lists:foreach(
        fun(Sql) ->
            try
                elib_pg:query(C, Sql, [])
            catch
                _:_ -> ok
            end
        end,
        Lists
    ).

rollback_soft(C) ->
    %% 事务可能已在上一步被显式回滚（无活动事务时 PG 仅 WARNING，仍返回 ok），
    %% 连接已断等异常场景一律吞掉——ROLLBACK 是清理动作，失败不掩盖断言错误。
    try
        elib_pg:query(C, <<"ROLLBACK">>, [])
    catch
        _:_ -> ok
    end,
    ok.
