%% teaching_assignment_list_integration_tests
%% 家长作业列表 latest_asset（作品预览句柄）真库集成——scratch@127.0.0.1:4323
%% （RUN 专属库 moya_zcode_181902，00000001→103 全量态 + 00000105/110），
%% 每用例 BEGIN ... ROLLBACK 不留数据；DB 不可达自动 skip。
%%
%% 验收口径（2026-09-12 拍板：单对象 / 只透 final_photo / sort_order 升序取首条）：
%%   ① 取「最新 submission」（attempt_no 最大）的 final_photo——旧 attempt 的照片不入选
%%   ② 同一 submission 多张 final_photo → ORDER BY (sort_order, id) 首条
%%      （夹具故意让 id 顺序与 sort_order 顺序相反，区分两种口径）
%%   ③ 最新 submission 只有 practice_video → null（不回退视频）
%%   ④ 无 submission → null
%%   ⑤ 行经 assignment_summary/1 组装后 DTO 形状 = {object_key, kind}（无 URL 字段）
%% 素材只给 object_key 句柄：客户端调 GET /api/v1/attachment/view_url 换签名 URL。

-module(teaching_assignment_list_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- 夹具 ----
-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_zcode_181902">>).

-define(TEACHER, 980001).
-define(PARENT, 980002).
-define(ORG, 981000).
-define(WS, 982001).
-define(GROUP, 983001).
-define(LEARNER, 984001).

%% assignment / task 段（la = latest_asset）
-define(ASSIGN_A, 986101).
-define(ASSIGN_B, 986102).
-define(ASSIGN_C, 986103).
-define(ASSIGN_D, 986104).
-define(TASK_A, <<"taskla_hash_a">>).
-define(TASK_B, <<"taskla_hash_b">>).
-define(TASK_C, <<"taskla_hash_c">>).
-define(TASK_D, <<"taskla_hash_d">>).

%% attachment 段
-define(ATT_A1, 990101).
-define(ATT_A2_FIRST, 990102).
-define(ATT_A2_SECOND, 990103).
-define(ATT_A2_VIDEO, 990104).
-define(ATT_B_VIDEO, 990105).
-define(ATT_D1, 990106).
-define(ATT_D2_VIDEO, 990107).

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

%%%===================================================================
%%% Seed（与 teaching_flow_integration_tests 同构；assignment 段独立 9861xx）
%%%===================================================================

seed(C) ->
    exec(C, <<
        "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES "
        "(980001, 'x', 'tla_teacher', '127.0.0.1', 'x'), "
        "(980002, 'x', 'tla_parent', '127.0.0.1', 'x')"
    >>),
    exec(
        C, <<"INSERT INTO organization (id, name, owner_id) VALUES (981000, '机构LA', 980001)"/utf8>>
    ),
    exec(C, <<
        "INSERT INTO workspace (id, name, owner_id, organization_id) VALUES "
        "(982001, 'LA-校区', 980001, 981000)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO \"group\" (id, owner_uid, creator_uid, scope, workspace_id, title) "
        "VALUES (983001, 980001, 980001, 'workspace', 982001, 'LA-硬笔班')"/utf8
    >>),
    exec(
        C,
        <<"INSERT INTO learner (id, organization_id, display_name) VALUES (984001, 981000, 'LA宝')"/utf8>>
    ),
    %% 四个 assignment（同一 learner）：A 两次提交、B 仅视频、C 无提交、
    %% D 老提交有照片但最新提交只有视频
    exec(C, <<
        "INSERT INTO group_task (id, group_id, task_id, title, creator_id, status) VALUES "
        "(985101, 983001, '",
        (?TASK_A)/binary,
        "', 'A-横竖', 980001, 1), "/utf8,
        "(985102, 983001, '",
        (?TASK_B)/binary,
        "', 'B-撇捺', 980001, 1), "/utf8,
        "(985103, 983001, '",
        (?TASK_C)/binary,
        "', 'C-结构', 980001, 1), "/utf8,
        "(985104, 983001, '",
        (?TASK_D)/binary,
        "', 'D-临帖', 980001, 1)"/utf8
    >>),
    exec(C, <<
        "INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) VALUES "
        "(986101, '",
        (?TASK_A)/binary,
        "', 980002, 984001), "
        "(986102, '",
        (?TASK_B)/binary,
        "', 980002, 984001), "
        "(986103, '",
        (?TASK_C)/binary,
        "', 980002, 984001), "
        "(986104, '",
        (?TASK_D)/binary,
        "', 980002, 984001)"
    >>),
    exec(C, <<
        "INSERT INTO attachment (id, file_hash256, path, mime_type, creator_user_id) VALUES "
        "(990101, 'h_la_a1', 'u980002/la/a1.jpg', 'image/jpeg', 980002), "
        "(990102, 'h_la_a2f', 'u980002/la/a2-first.jpg', 'image/jpeg', 980002), "
        "(990103, 'h_la_a2s', 'u980002/la/a2-second.jpg', 'image/jpeg', 980002), "
        "(990104, 'h_la_a2v', 'u980002/la/a2.mp4', 'video/mp4', 980002), "
        "(990105, 'h_la_bv', 'u980002/la/b.mp4', 'video/mp4', 980002), "
        "(990106, 'h_la_d1', 'u980002/la/d1.jpg', 'image/jpeg', 980002), "
        "(990107, 'h_la_d2v', 'u980002/la/d2.mp4', 'video/mp4', 980002)"
    >>),
    exec(C, <<"SET CONSTRAINTS ALL IMMEDIATE">>).

%%%===================================================================
%%% 用例
%%%===================================================================

latest_asset_selection_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% A：attempt1 = 一张照片；attempt2 = 两张照片（sort_order 1/0，id 顺序相反）+ 视频
        SubA1 = submit(C, ?ASSIGN_A, 987111, [{?ATT_A1, <<"final_photo">>, 0}]),
        SubA2 = submit(C, ?ASSIGN_A, 987112, [
            {?ATT_A2_SECOND, <<"final_photo">>, 1},
            {?ATT_A2_FIRST, <<"final_photo">>, 0},
            {?ATT_A2_VIDEO, <<"practice_video">>, 0}
        ]),
        %% B：仅视频
        _SubB1 = submit(C, ?ASSIGN_B, 987113, [{?ATT_B_VIDEO, <<"practice_video">>, 0}]),
        %% D：老提交有照片，最新提交只有视频（不得回退到老照片）
        _SubD1 = submit(C, ?ASSIGN_D, 987114, [{?ATT_D1, <<"final_photo">>, 0}]),
        _SubD2 = submit(C, ?ASSIGN_D, 987115, [{?ATT_D2_VIDEO, <<"practice_video">>, 0}]),

        {ok, Rows, Total} = teaching_submission_repo:assignments_for_learner_tx(C, ?LEARNER, 1, 10),
        ?assertEqual(4, Total),
        ById = maps:from_list([{maps:get(<<"assignment_id">>, R), R} || R <- Rows]),

        %% ① 最新 submission（attempt_no=2）+ ② 同 submission 内 sort_order 首条
        A = maps:get(?ASSIGN_A, ById),
        ?assertEqual(SubA2, maps:get(<<"latest_submission_id">>, A)),
        ?assertEqual(2, maps:get(<<"latest_attempt_no">>, A)),
        ?assertEqual(<<"u980002/la/a2-first.jpg">>, maps:get(<<"latest_asset_key">>, A)),
        ?assertEqual(<<"final_photo">>, maps:get(<<"latest_asset_kind">>, A)),
        %% 旧 attempt 的照片未被选中（1 号附件确实绑在 attempt1 上）
        ?assert(SubA1 =/= SubA2),

        %% ③ 仅视频 → NULL；④ 无提交 → NULL；D 最新提交仅视频 → NULL
        ?assertEqual(null, maps:get(<<"latest_asset_key">>, maps:get(?ASSIGN_B, ById))),
        ?assertEqual(null, maps:get(<<"latest_asset_key">>, maps:get(?ASSIGN_C, ById))),
        ?assertEqual(null, maps:get(<<"latest_asset_key">>, maps:get(?ASSIGN_D, ById))),
        %% 无提交的 C 其余字段仍完整（既有契约未破）
        CRow = maps:get(?ASSIGN_C, ById),
        ?assertEqual(null, maps:get(<<"latest_submission_id">>, CRow)),
        ?assertEqual(0, maps:get(<<"submission_count">>, CRow)),
        ?assertEqual(false, maps:get(<<"has_published">>, CRow)),

        %% ⑤ DTO 形状：只给句柄，无任何 URL 字段（MEDIA-03）
        DtoA = teaching_assignment_logic:assignment_summary(A),
        ?assertEqual(
            #{
                <<"object_key">> => <<"u980002/la/a2-first.jpg">>,
                <<"kind">> => <<"final_photo">>
            },
            maps:get(<<"latest_asset">>, DtoA)
        ),
        ?assertEqual(
            null,
            maps:get(
                <<"latest_asset">>,
                teaching_assignment_logic:assignment_summary(maps:get(?ASSIGN_B, ById))
            )
        )
    end).

%% 软删附件（status = -1）不进预览——与读授权路径 asset_path_run/2
%% 的 att.status >= 0 同口径（SQL 放行的对象 view_url 才会放行）
deleted_attachment_excluded_test_() ->
    with_tx(fun(C) ->
        seed(C),
        _ = submit(C, ?ASSIGN_A, 987121, [{?ATT_A1, <<"final_photo">>, 0}]),
        exec(C, <<"UPDATE attachment SET status = -1 WHERE id = 990101">>),
        {ok, Rows, _} = teaching_submission_repo:assignments_for_learner_tx(C, ?LEARNER, 1, 10),
        A = hd([R || R <- Rows, maps:get(<<"assignment_id">>, R) =:= ?ASSIGN_A]),
        ?assertEqual(null, maps:get(<<"latest_asset_key">>, A))
    end).

%%%===================================================================
%%% Helpers：走生产同款配方（FOR UPDATE → 取号 → CTE 幂等插入 → 附件绑定）
%%%===================================================================

submit(C, AssignId, SubId, Assets) ->
    {ok, AssignId} = teaching_submission_repo:lock_assignment_tx(C, AssignId),
    {ok, Next} = teaching_submission_repo:next_attempt_tx(C, AssignId),
    Suffix = integer_to_binary(SubId),
    {ok, #{<<"id">> := Sid} = Row} =
        teaching_submission_repo:create_idempotent_tx(C, #{
            id => SubId,
            assignment_id => AssignId,
            learner_id => ?LEARNER,
            uid => ?PARENT,
            attempt_no => Next,
            idempotency_key => <<"idem-", Suffix/binary>>,
            request_digest => <<"digest-", Suffix/binary>>
        }),
    true = maps:get(created, Row, false),
    ok = teaching_submission_repo:insert_assets_tx(C, Sid, ?PARENT, Assets),
    ok = teaching_submission_repo:mark_submitted_by_tx(C, AssignId, ?PARENT),
    Sid.
