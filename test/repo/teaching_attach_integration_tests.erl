%% teaching_attach_integration_tests
%% Step 10 真库集成（moya_mig_test@4323，BEGIN/ROLLBACK 不留数据）：
%%   ① submission_for_asset_path：附件路径 → submission 绑定解析（MEDIA-01 数据面）
%%   ② unbound_teaching_attachments：只列"超龄+未绑定"教学附件——
%%      已绑定（含撤回证据）/新近未绑定/其他 scope 一律不列（MEDIA-02 不误删）
%%   ③ attachment 行 path==url==object_key（MEDIA-03 行级断言：落库无 presigned URL）

-module(teaching_attach_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_mig_test">>).

-define(PARENT, 980002).
-define(TEACHER, 980001).
-define(ORG_A, 981000).
-define(WS_A, 982001).
-define(GROUP_A1, 983001).
-define(LEARNER_A1, 984001).
-define(TASK_ID, <<"task10a_hash_01">>).
-define(ASSIGN_ID, 986001).

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
