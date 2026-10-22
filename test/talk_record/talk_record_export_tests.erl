%%% @doc talk_record T19 导出/责任移交域 logic 级测试（真实 DB 55433）。
%%%
%%% 覆盖（T19 卡交付 7）：
%%%   * 导出权限矩阵：owner / grantee+allow_export / grantee 无 allow
%%%     （viewer+editor）/ 陌生人 404 / 实例与企业策略禁（owner 不能绕过）；
%%%   * 202 queued 后真实 worker 生成：content 快照（segments+speakers+summary）、
%%%     export 行 ready、job succeeded、审计、Idempotent-Replayed 语义
%%%     （同键重放/同键异 body 409）；
%%%   * include_audio=true 422、format 非法 422、缺幂等键 422；
%%%   * 下载重验：requester 开票 200、非 requester 403、撤权 404、
%%%     软删 404、到期 410 + 惰性到期（正文附件引用由后台 sweep 释放）、sweep 批量清理；
%%%   * exportState 轮询：requester 200 形状、到期 state=expired、
%%%     非 requester 403；
%%%   * 移交两分支（keep_access 保留/不保留）、目标目录校验（跨企业/
%%%     自己）、personal 拒绝、非 owner 403、If-Match 428/412、
%%%     acl_revision 推进 + revision/content_revision 不动、
%%%     created_by/scope 不变、审计；
%%%   * 移交幂等重放（原 owner 失权后同键回放首次结果）；
%%%   * 移交后旧创建请求不重复建记录（created_by+client_recording_id
%%%     幂等键不随 owner 变化）；
%%%   * custody 投影：owner 失去归属访问 → handover_required（读路径），
%%%     移交成功后 custody=active。
-module(talk_record_export_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    State = talk_record_t19_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t19_support:seed(State),
            State
        end,
        fun talk_record_t19_support:teardown_all/1
    ).

cleanup(State) ->
    talk_record_t19_support:teardown_all(State).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%%%===================================================================
%%% 夹具（编号段 8x；personal + org 两形态，转写产物齐全）
%%%===================================================================

-spec fixture(map(), integer()) -> map().
fixture(State, N) ->
    Conn = maps:get(conn, State),
    Ids = talk_record_t19_support:ids(),
    U1 = maps:get(u1, Ids),
    {ok, Rid} = talk_record_t15_support:create_record(U1, N, <<"t19-export"/utf8>>),
    transcribed(Conn, Rid),
    #{
        record => Rid,
        conn => Conn,
        u1 => U1,
        u2 => maps:get(u2, Ids),
        u3 => maps:get(u3, Ids),
        u4 => maps:get(u4, Ids),
        ids => Ids
    }.

-spec org_fixture(map(), integer()) -> map().
org_fixture(State, N) ->
    Conn = maps:get(conn, State),
    Ids = talk_record_t19_support:ids(),
    U1 = maps:get(u1, Ids),
    {ok, Rid} = talk_record_t15_support:create_org_record(U1, maps:get(org_a, Ids), 0, N),
    transcribed(Conn, Rid),
    #{
        record => Rid,
        conn => Conn,
        u1 => U1,
        u2 => maps:get(u2, Ids),
        u3 => maps:get(u3, Ids),
        u4 => maps:get(u4, Ids),
        ids => Ids
    }.

-spec transcribed(pid() | map(), integer()) -> ok.
transcribed(Conn, Rid) ->
    _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
    S1 = talk_record_t15_support:seed_speaker(Conn, Rid, <<"S1"/utf8>>, #{
        display_name => <<"张三"/utf8>>
    }),
    _ = talk_record_t15_support:seed_segment(Conn, Rid, 1, 0, S1, 0, <<"第一句正文"/utf8>>),
    _ = talk_record_t15_support:seed_segment(Conn, Rid, 1, 1, S1, 5000, <<"第二句正文"/utf8>>),
    _ = talk_record_t15_support:seed_summary(Conn, Rid, 1, #{overview => <<"会议纪要概述"/utf8>>}),
    talk_record_t19_support:sql(Conn, [
        "UPDATE talk_record SET duration_ms = 10000 WHERE id = ", integer_to_binary(Rid)
    ]),
    ok.

-spec body(binary()) -> map().
body(Format) ->
    #{<<"format">> => Format}.

%%%===================================================================
%%% 导出权限矩阵 + 创建链路
%%%===================================================================

export_owner_happy_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 81),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        {ok, created, P} = create_ready(U1, Rid, <<"t19-key-81">>, body(<<"txt">>)),
        ?assertEqual(<<"ready">>, maps:get(<<"state">>, P)),
        ?assert(is_integer(maps:get(<<"export_id">>, P))),
        ?assert(is_integer(maps:get(<<"job_id">>, P))),
        ?assert(is_binary(maps:get(<<"expires_at">>, P))),
        %% DB：行 ready + 内容快照 + job succeeded + 审计
        Row = talk_record_t19_support:export_row(Conn, maps:get(<<"export_id">>, P)),
        ?assertEqual(<<"ready">>, maps:get(<<"state">>, Row)),
        ?assertEqual(false, maps:get(<<"include_audio">>, Row)),
        Content = maps:get(<<"content">>, Row),
        ?assert(binary:match(Content, <<"第一句正文"/utf8>>) =/= nomatch),
        ?assert(binary:match(Content, <<"张三"/utf8>>) =/= nomatch),
        ?assert(binary:match(Content, <<"会议纪要概述"/utf8>>) =/= nomatch),
        ?assertEqual(1, talk_record_t15_support:audit_count(Conn, Rid, <<"export_create">>)),
        Job = talk_record_t19_support:export_job(Conn, Rid),
        ?assertEqual(<<"succeeded">>, maps:get(<<"state">>, Job)),
        ?assert(maps:get(<<"finished_at">>, Job) =/= null)
    end).

export_md_render_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 82),
        Conn = maps:get(conn, F),
        {ok, created, P} = create_ready(
            maps:get(u1, F),
            maps:get(record, F),
            <<"t19-key-82">>,
            body(<<"md">>)
        ),
        Content = talk_record_t19_support:export_content(Conn, maps:get(<<"export_id">>, P)),
        ?assert(binary:match(Content, <<"# t19-export"/utf8>>) =/= nomatch),
        ?assert(binary:match(Content, <<"## 正文"/utf8>>) =/= nomatch),
        ?assert(binary:match(Content, <<"## 摘要"/utf8>>) =/= nomatch),
        ?assertEqual(
            <<"md">>,
            maps:get(
                <<"format">>,
                talk_record_t19_support:export_row(Conn, maps:get(<<"export_id">>, P))
            )
        )
    end).

export_record_only_empty_body_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 83),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        %% 抹掉转写投影（record_only 形态）
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record SET transcript_status='not_requested',",
            " current_transcript_id=NULL WHERE id=",
            integer_to_binary(Rid)
        ]),
        {ok, created, P} = create_ready(
            maps:get(u1, F),
            Rid,
            <<"t19-key-83">>,
            body(<<"txt">>)
        ),
        Content = talk_record_t19_support:export_content(Conn, maps:get(<<"export_id">>, P)),
        ?assert(binary:match(Content, <<"正文为空"/utf8>>) =/= nomatch),
        ?assert(binary:match(Content, <<"第一句正文"/utf8>>) =:= nomatch)
    end).

export_grantee_matrix_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 84),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U2 = maps:get(u2, F),
        U3 = maps:get(u3, F),
        %% U2=viewer+allow_export=true；U3=editor 无 allow
        talk_record_t15_support:add_grant(Conn, Rid, U2, maps:get(u1, F), <<"viewer">>),
        talk_record_t15_support:sql(Conn, [
            "UPDATE talk_record_grant SET allow_export=true WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U2)
        ]),
        talk_record_t15_support:add_grant(Conn, Rid, U3, maps:get(u1, F), <<"editor">>),
        %% grantee+allow → ok
        ?assertMatch(
            {ok, created, _},
            create_ready(U2, Rid, <<"t19-key-84a">>, body(<<"txt">>))
        ),
        %% grantee 无 allow（viewer 无 allow / editor）→ forbidden
        ?assertMatch(
            {error, forbidden},
            create_ready(U3, Rid, <<"t19-key-84b">>, body(<<"txt">>))
        ),
        %% 陌生人 → not_found
        ?assertMatch(
            {error, not_found},
            create_ready(maps:get(u4, F), Rid, <<"t19-key-84c">>, body(<<"txt">>))
        )
    end).

export_policy_disabled_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 85),
        Rid = maps:get(record, F),
        Ids = maps:get(ids, F),
        OrgA = maps:get(org_a, Ids),
        %% 实例级禁：owner 也被拒（§8 owner 不能绕过）
        talk_record_t19_support:with_env(
            talk_record_export_policy,
            #{enabled => false},
            fun() ->
                ?assertMatch(
                    {error, export_disabled_by_policy},
                    create_ready(maps:get(u1, F), Rid, <<"t19-key-85a">>, body(<<"txt">>))
                )
            end
        ),
        %% 企业级禁：仅 org 记录拒
        talk_record_t19_support:with_env(
            talk_record_export_policy,
            #{org_overrides => #{OrgA => false}},
            fun() ->
                ?assertMatch(
                    {error, export_disabled_by_policy},
                    create_ready(maps:get(u1, F), Rid, <<"t19-key-85b">>, body(<<"txt">>))
                )
            end
        ),
        %% 未配置默认允许（策略检查点注释语义）
        ?assertMatch(
            {ok, created, _},
            create_ready(maps:get(u1, F), Rid, <<"t19-key-85c">>, body(<<"txt">>))
        )
    end).

export_caps_projection_policy_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 86),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        Ids = maps:get(ids, F),
        OrgA = maps:get(org_a, Ids),
        U2 = maps:get(u2, F),
        talk_record_t15_support:add_grant(Conn, Rid, U2, maps:get(u1, F), <<"viewer">>),
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record_grant SET allow_export=true WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U2)
        ]),
        talk_record_t19_support:with_env(
            talk_record_export_policy,
            #{org_overrides => #{OrgA => false}},
            fun() ->
                {ok, #{payload := OwnerP}} = talk_record_ds:detail(maps:get(u1, F), Rid),
                ?assertEqual(
                    false,
                    maps:get(<<"export">>, maps:get(<<"capabilities_for_actor">>, OwnerP))
                ),
                {ok, #{payload := ViewerP}} = talk_record_ds:detail(U2, Rid),
                ?assertEqual(
                    false,
                    maps:get(<<"export">>, maps:get(<<"capabilities_for_actor">>, ViewerP))
                )
            end
        )
    end).

export_input_gates_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 87),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        ?assertMatch(
            {error, export_audio_unsupported},
            create_ready(
                U1,
                Rid,
                <<"t19-key-87a">>,
                #{<<"format">> => <<"txt">>, <<"include_audio">> => true}
            )
        ),
        ?assertMatch(
            {error, invalid_export_format},
            create_ready(
                U1,
                Rid,
                <<"t19-key-87b">>,
                #{<<"format">> => <<"pdf">>}
            )
        ),
        ?assertMatch(
            {error, idempotency_key_required},
            create_ready(U1, Rid, undefined, body(<<"txt">>))
        )
    end).

export_idempotency_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 88),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        {ok, created, P1} = create_ready(U1, Rid, <<"t19-key-88">>, body(<<"txt">>)),
        %% 同键同 body 重放 → replay 同 export_id（不重复建行）
        {ok, replay, P2} = create_ready(U1, Rid, <<"t19-key-88">>, body(<<"txt">>)),
        ?assertEqual(maps:get(<<"export_id">>, P1), maps:get(<<"export_id">>, P2)),
        ?assertEqual(1, talk_record_t19_support:export_count(Conn, Rid)),
        %% 同键异 body → 409
        ?assertMatch(
            {error, idempotency_conflict},
            create_ready(U1, Rid, <<"t19-key-88">>, body(<<"md">>))
        )
    end).

%%%===================================================================
%%% 下载重验 / 到期 410 / exportState 轮询
%%%===================================================================

download_ticket_and_stream_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 89),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        {ok, created, P} = create_ready(U1, Rid, <<"t19-key-89">>, body(<<"txt">>)),
        ExportId = maps:get(<<"export_id">>, P),
        %% 开票：url 内嵌 ticket + expiry
        {ok, D} = talk_record_export_logic:download(U1, Rid, ExportId),
        ?assert(is_binary(maps:get(<<"url">>, D))),
        ?assert(is_binary(maps:get(<<"ticket">>, D))),
        ?assert(is_integer(maps:get(<<"expiry">>, D))),
        %% 验票（内容流校验点）→ 行含 content
        {ok, VRow} = talk_record_export_download:verify_ticket(maps:get(<<"ticket">>, D)),
        ?assert(binary:match(maps:get(<<"content">>, VRow), <<"第一句正文"/utf8>>) =/= nomatch)
    end).

download_reverify_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 90),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        talk_record_t15_support:add_grant(Conn, Rid, U2, U1, <<"viewer">>),
        {ok, created, P} = create_ready(U1, Rid, <<"t19-key-90">>, body(<<"txt">>)),
        ExportId = maps:get(<<"export_id">>, P),
        %% 非 requester（有 ACL 的 U2）→ forbidden
        ?assertMatch({error, forbidden}, talk_record_export_logic:download(U2, Rid, ExportId)),
        %% 陌生人 → not_found
        ?assertMatch(
            {error, not_found},
            talk_record_export_logic:download(maps:get(u4, F), Rid, ExportId)
        ),
        %% 撤权后（requester 已非 owner 场景：移交走 grant 用户）——
        %% 撤销 U2 grant 后 U2 轮询 → not_found（visible 失败）
        G = talk_record_t17_support:one(Conn, [
            "SELECT id FROM talk_record_grant WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U2),
            " AND revoked_at IS NULL LIMIT 1"
        ]),
        {ok, _} = talk_record_grant_logic:revoke(
            U1,
            Rid,
            maps:get(<<"id">>, G),
            <<"acl:1">>
        ),
        ?assertMatch(
            {error, not_found},
            talk_record_export_logic:state(U2, Rid, ExportId)
        ),
        %% 软删记录 → 下载 404（visible 谓词排除）
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record SET deleted_at=now() WHERE id=", integer_to_binary(Rid)
        ]),
        ?assertMatch({error, not_found}, talk_record_export_logic:download(U1, Rid, ExportId))
    end).

download_expired_410_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 91),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        {ok, created, P} = create_ready(U1, Rid, <<"t19-key-91">>, body(<<"txt">>)),
        ExportId = maps:get(<<"export_id">>, P),
        %% 到期：expires_at 置过去 → 下载 410 + 惰性到期（正文附件引用由后台 sweep 释放）
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record_export SET expires_at = now() - interval '1 hour' WHERE id=",
            integer_to_binary(ExportId)
        ]),
        ?assertMatch({error, export_expired}, talk_record_export_logic:download(U1, Rid, ExportId)),
        Row = talk_record_t19_support:export_row(Conn, ExportId),
        ?assertEqual(<<"expired">>, maps:get(<<"state">>, Row)),
        ?assertEqual(null, maps:get(<<"content">>, Row)),
        %% 轮询端点：200 + state=expired（410 语义在下载端点）
        {ok, S} = talk_record_export_logic:state(U1, Rid, ExportId),
        ?assertEqual(<<"expired">>, maps:get(<<"state">>, S))
    end).

export_state_shape_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 92),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        {ok, created, P} = create_ready(U1, Rid, <<"t19-key-92">>, body(<<"txt">>)),
        ExportId = maps:get(<<"export_id">>, P),
        {ok, S} = talk_record_export_logic:state(U1, Rid, ExportId),
        ?assertEqual(ExportId, maps:get(<<"export_id">>, S)),
        ?assertEqual(<<"ready">>, maps:get(<<"state">>, S)),
        ?assertEqual(<<"txt">>, maps:get(<<"format">>, S)),
        ?assert(is_binary(maps:get(<<"expires_at">>, S))),
        %% 非 requester 且无 ACL（u3 在本夹具无 grant）→ not_found；
        %% 跨记录 export_id 复合键不匹配 → not_found（不泄露存在性）
        ?assertMatch(
            {error, not_found},
            talk_record_export_logic:state(maps:get(u3, F), Rid, ExportId)
        ),
        ?assertMatch(
            {error, not_found},
            talk_record_export_logic:state(U1, Rid + 1, ExportId)
        )
    end).

sweep_expired_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 93),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        {ok, created, P} = create_ready(
            maps:get(u1, F),
            Rid,
            <<"t19-key-93">>,
            body(<<"txt">>)
        ),
        ExportId = maps:get(<<"export_id">>, P),
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record_export SET expires_at = now() - interval '1 hour' WHERE id=",
            integer_to_binary(ExportId)
        ]),
        N = elib_pg:with_conn(fun(C) ->
            {ok, Count} = talk_record_export_repo:sweep_expired_tx(C),
            Count
        end),
        ?assert(N >= 1),
        ?assertEqual(
            <<"expired">>,
            maps:get(<<"state">>, talk_record_t19_support:export_row(Conn, ExportId))
        )
    end).

%%%===================================================================
%% 责任移交（§9）
%%%===================================================================

transfer_keep_no_access_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 94),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        Before = talk_record_t19_support:owner_and_custody(Conn, Rid),
        %% 预置一条 U1 的遗留 grant（keep_access=false 必须清掉）
        talk_record_t15_support:add_grant(Conn, Rid, U1, U1, <<"viewer">>),
        {ok, created, P} = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"record:1">>,
            <<"t19-tr-94">>,
            #{<<"target_user_id">> => U2, <<"keep_access">> => false}
        ),
        ?assertEqual(U2, maps:get(<<"owner_user_id">>, P)),
        ?assertEqual(false, maps:get(<<"keep_access">>, P)),
        After = talk_record_t19_support:owner_and_custody(Conn, Rid),
        ?assertEqual(U2, maps:get(<<"owner_user_id">>, After)),
        %% §11.2：移交推 acl_revision；revision/content_revision 不动
        ?assertEqual(maps:get(<<"revision">>, Before) + 0, maps:get(<<"revision">>, After)),
        ?assertEqual(
            maps:get(<<"content_revision">>, Before), maps:get(<<"content_revision">>, After)
        ),
        ?assertEqual(maps:get(<<"acl_revision">>, Before) + 1, maps:get(<<"acl_revision">>, After)),
        %% created_by/scope/custody 不变式
        ?assertEqual(maps:get(<<"created_by">>, Before), maps:get(<<"created_by">>, After)),
        ?assertEqual(maps:get(<<"scope">>, Before), maps:get(<<"scope">>, After)),
        ?assertEqual(<<"active">>, maps:get(<<"custody_status">>, After)),
        %% 原 owner 无遗留 grant（活跃行 0）且失权（detail 404）
        ?assertEqual(0, talk_record_t17_support:active_grant_count(Conn, Rid, U1)),
        ?assertMatch({error, not_found}, talk_record_ds:detail(U1, Rid)),
        %% 新 owner 可见
        ?assertMatch({ok, _}, talk_record_ds:detail(U2, Rid)),
        %% 审计
        ?assertEqual(1, talk_record_t15_support:audit_count(Conn, Rid, <<"ownership_transfer">>))
    end).

transfer_keep_access_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 95),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        {ok, created, _P} = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"1">>,
            <<"t19-tr-95">>,
            #{<<"target_user_id">> => U2, <<"keep_access">> => true}
        ),
        %% 原 owner 保留 viewer grant 且仍可见（纯数字 If-Match 形态兼容）
        ?assertEqual(1, talk_record_t17_support:active_grant_count(Conn, Rid, U1)),
        ?assertMatch({ok, _}, talk_record_ds:detail(U1, Rid))
    end).

transfer_target_gates_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 96),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        %% 目标=自己 → 422
        ?assertMatch(
            {error, transfer_target_self},
            talk_record_transfer_logic:transfer(
                U1,
                Rid,
                <<"record:1">>,
                <<"t19-tr-96a">>,
                #{<<"target_user_id">> => U1, <<"keep_access">> => false}
            )
        ),
        %% 跨企业目标（U4=ORG_B owner）→ 422 target_not_in_directory
        ?assertMatch(
            {error, target_not_in_directory},
            talk_record_transfer_logic:transfer(
                U1,
                Rid,
                <<"record:1">>,
                <<"t19-tr-96b">>,
                #{<<"target_user_id">> => maps:get(u4, F), <<"keep_access">> => false}
            )
        ),
        %% personal 记录 → 422
        PF = fixture(State, 97),
        ?assertMatch(
            {error, personal_not_transferable},
            talk_record_transfer_logic:transfer(
                maps:get(u1, PF),
                maps:get(record, PF),
                <<"record:1">>,
                <<"t19-tr-96c">>,
                #{<<"target_user_id">> => maps:get(u2, PF), <<"keep_access">> => false}
            )
        ),
        %% 不存在用户目标 → 422 target_not_in_directory（目录校验拒绝）
        ?assertMatch(
            {error, target_not_in_directory},
            talk_record_transfer_logic:transfer(
                U1,
                Rid,
                <<"record:1">>,
                <<"t19-tr-96d">>,
                #{<<"target_user_id">> => 12345, <<"keep_access">> => false}
            )
        )
    end).

transfer_editor_forbidden_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 98),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U2 = maps:get(u2, F),
        talk_record_t15_support:add_grant(Conn, Rid, U2, maps:get(u1, F), <<"editor">>),
        ?assertMatch(
            {error, forbidden},
            talk_record_transfer_logic:transfer(
                U2,
                Rid,
                <<"record:1">>,
                <<"t19-tr-98">>,
                #{<<"target_user_id">> => maps:get(u1, F), <<"keep_access">> => false}
            )
        )
    end).

transfer_if_match_gates_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 99),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        Req = #{<<"target_user_id">> => maps:get(u2, F), <<"keep_access">> => false},
        %% 缺 If-Match → 428；旧 revision → 412
        ?assertMatch(
            {error, precondition_required},
            talk_record_transfer_logic:transfer(U1, Rid, undefined, <<"t19-tr-99a">>, Req)
        ),
        ?assertMatch(
            {error, precondition_failed},
            talk_record_transfer_logic:transfer(U1, Rid, <<"record:99">>, <<"t19-tr-99b">>, Req)
        ),
        ?assertMatch(
            {error, idempotency_key_required},
            talk_record_transfer_logic:transfer(U1, Rid, <<"record:1">>, undefined, Req)
        ),
        ?assertMatch(
            {error, invalid_target_user_id},
            talk_record_transfer_logic:transfer(
                U1,
                Rid,
                <<"record:1">>,
                <<"t19-tr-99c">>,
                #{<<"target_user_id">> => 0, <<"keep_access">> => false}
            )
        )
    end).

transfer_idempotent_replay_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 100),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        {ok, created, P1} = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"record:1">>,
            <<"t19-tr-100">>,
            #{<<"target_user_id">> => U2, <<"keep_access">> => false}
        ),
        %% 原 owner 已失权：同键重放命中幂等快照 → replay 首次结果（不 404）
        {ok, replay, P2} = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"record:1">>,
            <<"t19-tr-100">>,
            #{<<"target_user_id">> => U2, <<"keep_access">> => false}
        ),
        ?assertEqual(maps:get(<<"owner_user_id">>, P1), maps:get(<<"owner_user_id">>, P2)),
        ?assertEqual(1, talk_record_t15_support:audit_count(Conn, Rid, <<"ownership_transfer">>))
    end).

%%%===================================================================
%%% 移交后旧创建请求不重复建记录（created_by 幂等键语义，§11）
%%%===================================================================

create_replay_after_transfer_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 101),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        {ok, created, _} = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"record:1">>,
            <<"t19-tr-101">>,
            #{<<"target_user_id">> => U2, <<"keep_access">> => false}
        ),
        Before = talk_record_t19_support:owner_and_custody(Conn, Rid),
        %% 移交且不保留访问后，旧创建请求不得恢复 owner 专属身份。
        %% 幂等键仍保留，拒绝访问且不得重复建记录。
        Body = #{
            <<"client_recording_id">> => talk_record_t19_support:rec_uuid(101),
            <<"notice_ack">> => true,
            <<"policy_version">> => <<"talk-record-notice-v1">>,
            <<"scope">> => <<"organization">>,
            <<"organization_id">> => maps:get(org_a, maps:get(ids, F)),
            <<"processing_mode">> => <<"transcribe">>,
            <<"title">> => <<"t15-org"/utf8>>
        },
        ?assertMatch(
            {error, not_found}, talk_record_logic:create(U1, Body, <<"t15-org-key-101">>)
        ),
        Dup = talk_record_t19_support:one(
            Conn,
            [
                "SELECT count(*) AS n FROM talk_record WHERE created_by=",
                integer_to_binary(U1),
                " AND client_recording_id='",
                talk_record_t19_support:rec_uuid(101),
                "'"
            ]
        ),
        ?assertEqual(1, maps:get(<<"n">>, Dup)),
        %% owner 不因重放回退
        ?assertEqual(
            maps:get(<<"owner_user_id">>, Before),
            maps:get(<<"owner_user_id">>, talk_record_t19_support:owner_and_custody(Conn, Rid))
        )
    end).

%%%===================================================================
%%% custody 投影（§9 T19 交付 6：判定与展示字段就绪）
%%%===================================================================

custody_projection_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 102),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        Ids = maps:get(ids, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        talk_record_t15_support:add_grant(Conn, Rid, U2, U1, <<"viewer">>),
        %% owner 失去归属访问（org 成员停用）→ 有 grant 的同 org 用户
        %% 读路径投影 handover_required（owner 自己 visible 失败 404）
        Org = maps:get(org_a, Ids),
        ?assertMatch(
            {ok, _}, organization_owner_transfer:transfer(U1, Org, maps:get(u3, Ids))
        ),
        ?assertMatch({ok, _}, talk_record_ds:detail(U1, Rid)),
        talk_record_t19_support:set_org_member_status(Conn, Org, U1, <<"suspended">>),
        {ok, #{payload := P}} = talk_record_ds:detail(U2, Rid),
        ?assertEqual(<<"handover_required">>, maps:get(<<"custody_status">>, P)),
        ?assertMatch({error, not_found}, talk_record_ds:detail(U1, Rid)),
        %% owner 恢复 → active（判定实时，不落库）
        talk_record_t19_support:set_org_member_status(Conn, maps:get(org_a, Ids), U1, <<"active">>),
        {ok, #{payload := P2}} = talk_record_ds:detail(U2, Rid),
        ?assertEqual(<<"active">>, maps:get(<<"custody_status">>, P2))
    end).

custody_restored_by_transfer_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 103),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        Ids = maps:get(ids, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        %% 存储侧 handover_required（治理流程写入形态）→ 移交成功后回 active
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record SET custody_status='handover_required' WHERE id=",
            integer_to_binary(Rid)
        ]),
        %% owner 失权时移交走不了（visible 404——metadata-only recovery 属
        %% 后续治理）；恢复成员资格后由当前 owner 移交
        talk_record_t19_support:set_org_member_status(Conn, maps:get(org_a, Ids), U1, <<"active">>),
        {ok, created, P} = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"record:1">>,
            <<"t19-tr-103">>,
            #{<<"target_user_id">> => U2, <<"keep_access">> => false}
        ),
        ?assertEqual(<<"active">>, maps:get(<<"custody_status">>, P))
    end).

%%%===================================================================
%%% Q-review 修复回归（P1/P2/P3/P4）
%%%===================================================================

%% P1：票据携带 acl claim；验票动态重验 记录 ACL + 撤销版本——
%% 撤权/移交/软删后 TTL 窗口内带 ticket 不再放行。
ticket_revoke_stale_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 104),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        %% U2 viewer+allow_export 建导出并开票
        talk_record_t15_support:add_grant(Conn, Rid, U2, U1, <<"viewer">>),
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record_grant SET allow_export=true WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U2)
        ]),
        {ok, created, P} = create_ready(U2, Rid, <<"t19-q-104">>, body(<<"txt">>)),
        {ok, D} = talk_record_export_logic:download(U2, Rid, maps:get(<<"export_id">>, P)),
        Ticket = maps:get(<<"ticket">>, D),
        %% 基线：验票通过
        ?assertMatch({ok, _}, talk_record_export_download:verify_ticket(Ticket)),
        %% 撤权（撤销 U2 的 grant，acl_revision 推进）→ 验票 not_found
        G = talk_record_t17_support:one(Conn, [
            "SELECT id FROM talk_record_grant WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U2),
            " AND revoked_at IS NULL LIMIT 1"
        ]),
        {ok, _} = talk_record_grant_logic:revoke(U1, Rid, maps:get(<<"id">>, G), <<"acl:1">>),
        ?assertMatch({error, not_found}, talk_record_export_download:verify_ticket(Ticket))
    end).

ticket_transfer_stale_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 105),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        %% U1（org owner）建导出并开票 → keep=true 移交（acl_revision 推进）
        {ok, created, P} = create_ready(U1, Rid, <<"t19-q-105">>, body(<<"txt">>)),
        {ok, D} = talk_record_export_logic:download(U1, Rid, maps:get(<<"export_id">>, P)),
        Ticket = maps:get(<<"ticket">>, D),
        ?assertMatch({ok, _}, talk_record_export_download:verify_ticket(Ticket)),
        {ok, created, _} = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"record:1">>,
            <<"t19-q-105t">>,
            #{<<"target_user_id">> => U2, <<"keep_access">> => true}
        ),
        %% U1 仍可见，但移交保留的 viewer grant 未授予导出能力。
        ?assertMatch({error, forbidden}, talk_record_export_download:verify_ticket(Ticket)),
        %% 重新开票也必须拒绝；可见权限不能替代 allow_export。
        ?assertEqual(
            {error, forbidden},
            talk_record_export_logic:download(U1, Rid, maps:get(<<"export_id">>, P))
        )
    end).

ticket_soft_delete_stale_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 106),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        {ok, created, P} = create_ready(U1, Rid, <<"t19-q-106">>, body(<<"txt">>)),
        {ok, D} = talk_record_export_logic:download(U1, Rid, maps:get(<<"export_id">>, P)),
        Ticket = maps:get(<<"ticket">>, D),
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record SET deleted_at=now() WHERE id=", integer_to_binary(Rid)
        ]),
        ?assertMatch({error, not_found}, talk_record_export_download:verify_ticket(Ticket))
    end).

%% P2：并发双移交双花防护——锁后复核当前 owner == 发起人（行锁 CAS
%% revision 不拦截移交：§11.2 移交推 acl_revision 不推 revision）。
%% 用 meck 冻结 find_visible 为移交前视图（模拟并发第二事务在第一事务
%% 提交前通过事务外门禁），锁后 owner 已易主 → 412，owner 不被双花。
transfer_double_spend_guard_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 107),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        U3 = maps:get(u3, F),
        %% 抓取移交前 owner=U1 的可见行（并发第二事务的事务外视图）
        {ok, OldView} = talk_record_repo:find_visible(U1, Rid),
        %% 模拟并发第一移交已提交：owner→U2、acl_revision+1（revision 不动）
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record SET owner_user_id=",
            integer_to_binary(U2),
            ", acl_revision=acl_revision+1 WHERE id=",
            integer_to_binary(Rid)
        ]),
        ok = meck:new(talk_record_repo, [passthrough, unstick, no_link]),
        ok = meck:expect(
            talk_record_repo,
            find_visible,
            fun(_Uid, _Rid2) -> {ok, OldView} end
        ),
        Result = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"record:1">>,
            <<"t19-q-107">>,
            #{<<"target_user_id">> => U3, <<"keep_access">> => false}
        ),
        catch meck:unload(talk_record_repo),
        ?assertMatch({error, precondition_failed}, Result),
        %% owner 未被双花（仍为第一移交目标 U2）
        ?assertEqual(
            U2,
            maps:get(
                <<"owner_user_id">>,
                talk_record_t19_support:owner_and_custody(Conn, Rid)
            )
        )
    end).

%% P3：editor+allow_export=true 的 capabilities_for_actor.export 投影
%% 与 POST /exports 门禁同口径（不再恒 false）。
editor_allow_export_caps_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 108),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U3 = maps:get(u3, F),
        talk_record_t15_support:add_grant(Conn, Rid, U3, maps:get(u1, F), <<"editor">>),
        {ok, #{payload := P0}} = talk_record_ds:detail(U3, Rid),
        ?assertEqual(false, maps:get(<<"export">>, maps:get(<<"capabilities_for_actor">>, P0))),
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record_grant SET allow_export=true WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U3)
        ]),
        {ok, #{payload := P1}} = talk_record_ds:detail(U3, Rid),
        ?assertEqual(true, maps:get(<<"export">>, maps:get(<<"capabilities_for_actor">>, P1)))
    end).

%% P4：keep_access=true 时预置"活跃但已过期"的 grant 行——存在性检查
%% 必须按有效判定，过期行被重置为未过期 viewer（原 owner 实际获得访问）。
transfer_keep_with_expired_grant_test_() ->
    with_db(fun(State) ->
        F = org_fixture(State, 109),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        %% 预置 U1 的过期 grant（revoked NULL、expires_at 已过——活跃谓词
        %% 视为无访问）
        talk_record_t15_support:add_grant(Conn, Rid, U1, U1, <<"viewer">>),
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record_grant SET expires_at = now() - interval '1 hour'",
            " WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U1)
        ]),
        {ok, created, _} = talk_record_transfer_logic:transfer(
            U1,
            Rid,
            <<"record:1">>,
            <<"t19-q-109">>,
            #{<<"target_user_id">> => U2, <<"keep_access">> => true}
        ),
        %% 移交后 U1 有有效 viewer grant（expires_at 已重置 NULL）且可见
        Grant = talk_record_t17_support:one(Conn, [
            "SELECT role, expires_at FROM talk_record_grant WHERE record_id=",
            integer_to_binary(Rid),
            " AND grantee_user_id=",
            integer_to_binary(U1),
            " AND revoked_at IS NULL LIMIT 1"
        ]),
        ?assertEqual(<<"viewer">>, maps:get(<<"role">>, Grant)),
        ?assertEqual(null, maps:get(<<"expires_at">>, Grant)),
        ?assertMatch({ok, _}, talk_record_ds:detail(U1, Rid))
    end).

%% Exercise real asynchronous production flow; initial response must be queued.
create_ready(Uid, Rid, Key, Body) ->
    case talk_record_export_logic:create(Uid, Rid, Key, Body) of
        {ok, created, P} ->
            ?assertEqual(<<"queued">>, maps:get(<<"state">>, P)),
            {ok, J} = elib_pg:with_tx(fun(C) ->
                talk_record_job_repo:claim_next_tx(
                    C,
                    <<"t19-test">>,
                    <<"11111111-2222-4333-8444-555555555555">>,
                    300
                )
            end),
            {ok, _} = talk_record_export_job:run(J),
            {ok, S} = talk_record_export_logic:state(Uid, Rid, maps:get(<<"export_id">>, P)),
            {ok, created, S};
        Other ->
            Other
    end.
