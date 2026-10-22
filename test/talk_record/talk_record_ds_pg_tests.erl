%%% @doc talk_record DS 层真库测试（127.0.0.1:55433 隔离库）。
%%%
%%% 覆盖（T05 卡）：
%%%   * 创建幂等：同键同 payload 重放返回首次 id；同键异 payload 409 冲突；
%%%     过期后幂等键可重用；业务键 (created_by, client_recording_id) 撞库转重放。
%%%   * 归属：跨主体/跨企业/无 workspace 越权读统一 404（not_found）；
%%%     personal 带 org 字段 422（logic 层，见 logic 测试）；org 组合失效
%%%     409（context_changed）；企业记录 owner 失去成员资格即失读权。
%%%   * ETag/CAS：GET 出 ETag；PATCH 旧 If-Match 412、正确值 revision+1；
%%%     owner/scope 不可改；editor 仅 notes；处理已开始锁 language。
%%%   * 列表：filter=owned/shared 与 cursor 翻页；无权记录不出现；
%%%     游标绑定调用者（换人用同 cursor → cursor_invalid）。
%%%   * creation-contexts：personal + 有权 org/ws；org keyset 翻页。
-module(talk_record_ds_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEY1, <<"t05-idem-key-0001">>).
-define(KEY2, <<"t05-idem-key-0002">>).
-define(UUID1, <<"00000000-0000-0000-0000-000000010001">>).
-define(UUID2, <<"00000000-0000-0000-0000-000000010002">>).

personal_body() ->
    #{
        <<"client_recording_id">> => ?UUID1,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"title">> => <<"个人记录"/utf8>>
    }.

org_body(OrgId, WsId) ->
    ((personal_body())#{
        <<"scope">> => <<"organization">>,
        <<"organization_id">> => OrgId
    })#{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000010005">>,
        <<"workspace_id">> => WsId
    }.

setup() ->
    State = talk_record_t05_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t05_support:seed_fixtures(maps:get(conn, State)),
            State
        end,
        fun talk_record_t05_support:teardown_all/1
    ).

cleanup(State) ->
    talk_record_t05_support:teardown_all(State).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%%%===================================================================
%%% 创建 + 幂等
%%%===================================================================

create_and_idempotent_replay_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        Body = personal_body(),
        {ok, created, P1, ETag1} = talk_record_logic:create(U1, Body, ?KEY1),
        Id1 = maps:get(<<"id">>, P1),
        ?assertEqual(1, maps:get(<<"revision">>, P1)),
        ?assertEqual(<<"\"1\"">>, ETag1),
        ?assertEqual(<<"personal">>, maps:get(<<"scope">>, P1)),

        %% 同键同 payload：重放返回首次结果（同 id、同快照 revision=1）
        {ok, replay, P2, _} = talk_record_logic:create(U1, Body, ?KEY1),
        ?assertEqual(Id1, maps:get(<<"id">>, P2)),
        ?assertEqual(1, maps:get(<<"revision">>, P2)),

        %% 首次结果后 PATCH 改 title，再重放仍返回首次快照（revision 不变）
        {ok, #{payload := P3}} =
            talk_record_ds:update(U1, Id1, 1, #{<<"title">> => <<"改名后"/utf8>>}),
        ?assertEqual(2, maps:get(<<"revision">>, P3)),
        {ok, replay, P4, _} = talk_record_logic:create(U1, Body, ?KEY1),
        ?assertEqual(1, maps:get(<<"revision">>, P4)),
        ?assertEqual(<<"个人记录"/utf8>>, maps:get(<<"title">>, P4)),

        %% 幂等行已回填 result_ref
        #{<<"result_ref">> := Ref} = talk_record_t05_support:one(
            C,
            <<"SELECT result_ref FROM talk_record_idempotency", " WHERE actor_id = ",
                (integer_to_binary(U1))/binary, " AND idempotency_key = '", ?KEY1/binary, "'">>
        ),
        ?assert(is_binary(Ref) andalso byte_size(Ref) > 2)
    end).

idempotent_conflict_and_reuse_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, created, _, _} = talk_record_logic:create(U1, personal_body(), ?KEY1),
        %% 同键异 payload：409 冲突（digest 不一致）
        Bad = (personal_body())#{<<"title">> => <<"不同 payload"/utf8>>},
        ?assertEqual(
            {error, idempotency_conflict}, talk_record_logic:create(U1, Bad, ?KEY1)
        ),
        %% 跨主体同键：不冲突（scope = actor+method+resource+key）
        U2 = maps:get(u2, Ids),
        U2Body = (personal_body())#{<<"client_recording_id">> => ?UUID2},
        ?assertMatch(
            {ok, created, _, _}, talk_record_logic:create(U2, U2Body, ?KEY1)
        ),
        %% TTL 过期后同键可重用（新 client_recording_id → 新记录）
        talk_record_t05_support:sql(
            C,
            <<"UPDATE talk_record_idempotency SET expires_at = now() - interval '1 second'">>
        ),
        Fresh = (personal_body())#{<<"client_recording_id">> => ?UUID2},
        ?assertMatch(
            {ok, created, _, _}, talk_record_logic:create(U1, Fresh, ?KEY1)
        )
    end).

business_key_replay_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, created, P1, _} = talk_record_logic:create(U1, personal_body(), ?KEY1),
        Id1 = maps:get(<<"id">>, P1),
        %% 不同幂等键 + 相同 client_recording_id：业务键去重转重放（§11.1）
        {ok, replay, P2, _} = talk_record_logic:create(U1, personal_body(), ?KEY2),
        ?assertEqual(Id1, maps:get(<<"id">>, P2))
    end).

idempotency_key_required_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t05_support:ids(),
        ?assertEqual(
            {error, idempotency_key_required},
            talk_record_logic:create(maps:get(u1, Ids), personal_body(), undefined)
        )
    end).

%%%===================================================================
%%% 归属组合（org 创建核验 + 越权读 404）
%%%===================================================================

org_create_and_context_changed_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        U3 = maps:get(u3, Ids),
        OrgA = maps:get(org_a, Ids),
        OrgB = maps:get(org_b, Ids),
        WsA1 = maps:get(ws_a1, Ids),
        WsA2 = maps:get(ws_a2, Ids),
        WsB1 = maps:get(ws_b1, Ids),

        %% org 记录正常创建（U1 为 org owner 且 ws member）
        Body = org_body(OrgA, WsA1),
        {ok, created, P, _} = talk_record_logic:create(U1, Body, ?KEY1),
        ?assertEqual(<<"organization">>, maps:get(<<"scope">>, P)),
        ?assertEqual(OrgA, maps:get(<<"organization_id">>, P)),

        %% 非成员组织 → context_changed
        ?assertEqual(
            {error, context_changed},
            talk_record_logic:create(U3, org_body(OrgB, 0), <<"k-org-b">>)
        ),
        %% 组织被归档 → context_changed
        talk_record_t05_support:sql(
            C, <<"UPDATE organization SET status='archived' WHERE id = ", (i(OrgA))/binary>>
        ),
        ?assertEqual(
            {error, context_changed},
            talk_record_logic:create(U1, org_body(OrgA, 0), <<"k-arch">>)
        ),
        talk_record_t05_support:sql(
            C, <<"UPDATE organization SET status='active' WHERE id = ", (i(OrgA))/binary>>
        ),
        %% U3 是 org 成员但不是 WS_A1 成员（也非 owner/admin）→ 组合失效
        ?assertEqual(
            {error, context_changed},
            talk_record_logic:create(U3, org_body(OrgA, WsA1), <<"k-ws-a1">>)
        ),
        %% workspace 不属于该组织 → 组合失效
        ?assertEqual(
            {error, context_changed},
            talk_record_logic:create(U1, org_body(OrgA, WsB1), <<"k-cross-ws">>)
        ),
        %% U1 是 org owner：owner/admin 对未加入的 WS_A2 仍有创建能力
        ?assertMatch(
            {ok, created, _, _},
            talk_record_logic:create(
                U1,
                (org_body(OrgA, WsA2))#{
                    <<"client_recording_id">> =>
                        <<"00000000-0000-0000-0000-000000010003">>
                },
                <<"k-ws-a2-owner">>
            )
        )
    end).

visibility_and_404_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        U4 = maps:get(u4, Ids),
        OrgA = maps:get(org_a, Ids),
        OrgB = maps:get(org_b, Ids),
        WsA1 = maps:get(ws_a1, Ids),

        %% U1 的个人记录：U4 不可见（404 统一口径）
        {ok, created, P1, _} =
            talk_record_logic:create(U1, personal_body(), ?KEY1),
        Id1 = maps:get(<<"id">>, P1),
        ?assertMatch({ok, _}, talk_record_ds:detail(U1, Id1)),
        ?assertEqual({error, not_found}, talk_record_ds:detail(U4, Id1)),
        ?assertEqual({error, not_found}, talk_record_ds:detail(U2, Id1)),

        %% U1 的 org 记录（绑定 WS_A1）+ viewer grant 给 U2：U2 需同时具备
        %% ws 访问权（ws 成员）才可读（§1 企业记录 ACL + workspace 资源授权）
        talk_record_t05_support:sql(
            C,
            <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES (",
                (i(WsA1))/binary, ", ", (i(U2))/binary, ", 'member', 'active')">>
        ),
        {ok, created, PO, _} =
            talk_record_logic:create(U1, org_body(OrgA, WsA1), <<"k-vis-org">>),
        IdO = maps:get(<<"id">>, PO),
        talk_record_t05_support:sql(
            C,
            <<"INSERT INTO talk_record_grant(id, record_id, grantee_user_id, role, allow_export, granted_by) ",
                "VALUES (910001, ", (i(IdO))/binary, ", ", (i(U2))/binary, ", 'viewer', true, ",
                (i(U1))/binary, ")">>
        ),
        ?assertMatch({ok, _}, talk_record_ds:detail(U2, IdO)),
        ?assertEqual({error, not_found}, talk_record_ds:detail(U4, IdO)),

        %% The current organization owner cannot be suspended. Transfer ownership
        %% through the domain command first, then test the record owner's revocation.
        U3 = maps:get(u3, Ids),
        ?assertMatch(
            {ok, #{owner_id := U3}}, organization_owner_transfer:transfer(U1, OrgA, U3)
        ),
        ?assertMatch({ok, _}, talk_record_ds:detail(U1, IdO)),
        %% U1（record owner）失去 org 成员资格 → 立即失读权（§8 离开企业）
        talk_record_t05_support:sql(
            C,
            <<"UPDATE organization_member SET status='suspended' WHERE organization_id = ",
                (i(OrgA))/binary, " AND user_id = ", (i(U1))/binary>>
        ),
        ?assertEqual({error, not_found}, talk_record_ds:detail(U1, IdO)),
        %% viewer U2 仍是有效成员 → 仍可读
        ?assertMatch({ok, _}, talk_record_ds:detail(U2, IdO)),
        %% ORG_B 的记录对 ORG_A 成员不可见（跨企业 404）
        ?assertEqual({error, not_found}, talk_record_ds:detail(U1, 990001))
    end).

%%%===================================================================
%%% ETag / CAS 编辑
%%%===================================================================

etag_and_patch_cas_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        U3 = maps:get(u3, Ids),
        U2 = maps:get(u2, Ids),
        {ok, created, P, _} = talk_record_logic:create(U1, personal_body(), ?KEY1),
        Id = maps:get(<<"id">>, P),

        %% 缺 If-Match（logic 428）
        ?assertEqual(
            {error, precondition_required}, talk_record_logic:update(U1, Id, undefined, #{})
        ),
        %% 旧版本 → 412
        ?assertMatch(
            {error, precondition_failed},
            talk_record_ds:update(U1, Id, 99, #{<<"notes">> => <<"x">>})
        ),
        %% 不存在/无权 → 404
        ?assertEqual(
            {error, not_found},
            talk_record_ds:update(U2, Id, 1, #{<<"notes">> => <<"x">>})
        ),
        %% 正确版本 → 200 + revision+1
        {ok, #{payload := P2, etag := ETag2}} =
            talk_record_ds:update(U1, Id, 1, #{
                <<"title">> => <<"改"/utf8>>,
                <<"notes">> => <<"备注"/utf8>>
            }),
        ?assertEqual(2, maps:get(<<"revision">>, P2)),
        ?assertEqual(<<"\"2\"">>, ETag2),

        %% editor grant：仅 notes 可改；改 title → forbidden_field
        talk_record_t05_support:sql(
            maps:get(conn, State),
            <<"INSERT INTO talk_record_grant(id, record_id, grantee_user_id, role, allow_export, granted_by) ",
                "VALUES (910002, ", (i(Id))/binary, ", ", (i(U3))/binary, ", 'editor', false, ",
                (i(U1))/binary, ")">>
        ),
        ?assertMatch(
            {ok, #{payload := #{<<"revision">> := 3}}},
            talk_record_ds:update(U3, Id, 2, #{<<"notes">> => <<"编辑备注"/utf8>>})
        ),
        ?assertEqual(
            {error, forbidden_field},
            talk_record_ds:update(U3, Id, 3, #{<<"title">> => <<"越权"/utf8>>})
        ),
        %% viewer（U2 无 grant）→ 不可见 → 404
        ?assertEqual(
            {error, not_found}, talk_record_ds:update(U2, Id, 3, #{<<"notes">> => <<"v">>})
        )
    end).

language_lock_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, created, P, _} = talk_record_logic:create(U1, personal_body(), ?KEY1),
        Id = maps:get(<<"id">>, P),
        %% 转写排队后 language 锁定，notes 仍可改
        talk_record_t05_support:sql(
            C,
            <<"UPDATE talk_record SET transcript_status='queued' WHERE id = ", (i(Id))/binary>>
        ),
        ?assertEqual(
            {error, language_locked},
            talk_record_ds:update(U1, Id, 1, #{<<"language">> => <<"en-US">>})
        ),
        ?assertMatch(
            {ok, #{payload := #{<<"revision">> := 2}}},
            talk_record_ds:update(U1, Id, 1, #{<<"notes">> => <<"仍可编辑"/utf8>>})
        )
    end).

%%%===================================================================
%%% 列表（filter/owned/shared + cursor + 越权不可见）
%%%===================================================================

list_filter_and_cursor_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        OrgA = maps:get(org_a, Ids),
        OrgB = maps:get(org_b, Ids),
        WsA1 = maps:get(ws_a1, Ids),

        %% U1 建 3 条 personal + 1 条 org(A) ；U4 建 1 条 org(B)（U1 不可见）
        {ok, created, _, _} =
            talk_record_logic:create(U1, personal_body(), <<"k-l1">>),
        {ok, created, _, _} = talk_record_logic:create(
            U1,
            (personal_body())#{<<"client_recording_id">> => ?UUID2},
            <<"k-l2">>
        ),
        {ok, created, P3, _} = talk_record_logic:create(
            U1,
            (personal_body())#{
                <<"client_recording_id">> =>
                    <<"00000000-0000-0000-0000-000000010004">>
            },
            <<"k-l3">>
        ),
        {ok, created, PO, _} =
            talk_record_logic:create(U1, org_body(OrgA, WsA1), <<"k-l4">>),
        Id3 = maps:get(<<"id">>, P3),
        IdO = maps:get(<<"id">>, PO),
        talk_record_t05_support:sql(
            C,
            <<"INSERT INTO talk_record(id, owner_user_id, created_by, client_recording_id, scope, organization_id, title)",
                " VALUES (980001, ", (i(maps:get(u4, Ids)))/binary, ", ",
                (i(maps:get(u4, Ids)))/binary,
                ", '00000000-0000-0000-0000-000000010005', 'organization', ", (i(OrgB))/binary,
                ", 't05-org-b-record')">>
        ),

        %% all：U1 见 4（3 personal + 1 org A），不见 org B
        Filters = #{
            scope => <<>>,
            organization_id => 0,
            workspace_id => 0,
            filter => <<"all">>,
            q => <<>>
        },
        {ok, #{<<"list">> := L1}} = talk_record_ds:list(U1, Filters, undefined, 20),
        ?assertEqual(4, length(L1)),
        ?assertNot(lists:member(980001, [maps:get(<<"id">>, X) || X <- L1])),
        %% 默认倒序：最新在前（org 记录最后创建）
        ?assertEqual(IdO, maps:get(<<"id">>, hd(L1))),
        ?assertEqual(<<"owned">>, maps:get(<<"visibility">>, hd(L1))),

        %% owned/shared 对 U2：U2 对 Id3 有 viewer grant → shared=1
        talk_record_t05_support:sql(
            C,
            <<"INSERT INTO talk_record_grant(id, record_id, grantee_user_id, role, allow_export, granted_by) ",
                "VALUES (910003, ", (i(Id3))/binary, ", ", (i(U2))/binary, ", 'viewer', false, ",
                (i(U1))/binary, ")">>
        ),
        {ok, #{<<"list">> := LShared}} = talk_record_ds:list(
            U2, Filters#{filter => <<"shared">>}, undefined, 20
        ),
        ?assertEqual(1, length(LShared)),
        ?assertEqual(Id3, maps:get(<<"id">>, hd(LShared))),
        ?assertEqual(<<"shared">>, maps:get(<<"visibility">>, hd(LShared))),
        {ok, #{<<"list">> := LOwnedU2}} = talk_record_ds:list(
            U2, Filters#{filter => <<"owned">>}, undefined, 20
        ),
        ?assertEqual(0, length(LOwnedU2)),
        %% U2 是 org A 成员：org 过滤下 org 记录仍不可见（无 grant、非 owner）
        {ok, #{<<"list">> := LOrgU2}} = talk_record_ds:list(
            U2,
            Filters#{scope => <<"organization">>, organization_id => OrgA},
            undefined,
            20
        ),
        ?assertEqual(0, length(LOrgU2)),
        %% org 过滤对 U1：仅返回 org A 记录
        {ok, #{<<"list">> := LOrgU1}} = talk_record_ds:list(
            U1,
            Filters#{scope => <<"organization">>, organization_id => OrgA},
            undefined,
            20
        ),
        ?assertEqual(1, length(LOrgU1)),

        %% cursor 翻页（limit=2）：3 页取回 4 条，无重复
        {ok, #{<<"list">> := Pa, <<"next_cursor">> := Ca}} = talk_record_ds:list(
            U1, Filters, undefined, 2
        ),
        ?assertEqual(2, length(Pa)),
        ?assert(is_binary(Ca)),
        {ok, #{<<"list">> := Pb, <<"next_cursor">> := Cb}} = talk_record_ds:list(
            U1, Filters, Ca, 2
        ),
        ?assertEqual(2, length(Pb)),
        {ok, #{<<"list">> := Pc, <<"next_cursor">> := null}} = talk_record_ds:list(
            U1, Filters, Cb, 2
        ),
        ?assertEqual(0, length(Pc)),
        All = [maps:get(<<"id">>, X) || X <- Pa] ++ [maps:get(<<"id">>, X) || X <- Pb],
        ?assertEqual(4, length(lists:usort(All))),

        %% 游标绑定调用者：U2 拿 U1 的 cursor → cursor_invalid
        ?assertEqual(
            {error, cursor_invalid}, talk_record_ds:list(U2, Filters, Ca, 20)
        ),
        %% 游标绑定筛选：换 filter 沿用 → cursor_invalid
        ?assertEqual(
            {error, cursor_invalid},
            talk_record_ds:list(U1, Filters#{filter => <<"owned">>}, Ca, 20)
        ),
        %% q 标题过滤
        {ok, #{<<"list">> := Lq}} = talk_record_ds:list(
            U1, Filters#{q => <<"org-b-record">>}, undefined, 20
        ),
        ?assertEqual(0, length(Lq)),
        %% q=个人记录：3 条 personal + 1 条 org（org_body 沿用同名 title）
        {ok, #{<<"list">> := Lq2}} = talk_record_ds:list(
            U1, Filters#{q => <<"个人记录"/utf8>>}, undefined, 20
        ),
        ?assertEqual(4, length(Lq2))
    end).

%%%===================================================================
%%% creation-contexts
%%%===================================================================

creation_contexts_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t05_support:ids(),
        U1 = maps:get(u1, Ids),
        U7 = maps:get(u7, Ids),
        U6 = maps:get(u6, Ids),
        OrgA = maps:get(org_a, Ids),

        %% U1：personal + ORG_A（ws 成员见 WS_A1；org owner/admin 补充见 WS_A2）
        {ok, #{<<"personal">> := Personal, <<"organizations">> := Orgs1}} =
            talk_record_ds:creation_contexts(U1, undefined, 20),
        ?assertEqual(<<"personal">>, maps:get(<<"scope">>, Personal)),
        ?assertEqual(1, length(Orgs1)),
        Ctx = hd(Orgs1),
        ?assertEqual(OrgA, maps:get(<<"organization_id">>, Ctx)),
        WsIds = [maps:get(<<"id">>, W) || W <- maps:get(<<"workspaces">>, Ctx)],
        ?assertEqual(lists:sort([maps:get(ws_a1, Ids), maps:get(ws_a2, Ids)]), lists:sort(WsIds)),

        %% U7（无任何组织关系）：仅 personal
        {ok, #{<<"organizations">> := Orgs7}} =
            talk_record_ds:creation_contexts(U7, undefined, 20),
        ?assertEqual(0, length(Orgs7)),

        %% U6 加入 21 个 org：limit 20 翻页 → 1+1 页
        {ok, #{<<"organizations">> := P1, <<"next_cursor">> := Cur}} =
            talk_record_ds:creation_contexts(U6, undefined, 20),
        ?assertEqual(20, length(P1)),
        ?assert(is_binary(Cur)),
        {ok, #{<<"organizations">> := P2, <<"next_cursor">> := null}} =
            talk_record_ds:creation_contexts(U6, Cur, 20),
        ?assertEqual(1, length(P2)),
        %% 游标绑定调用者：U1 拿 U6 的 cursor → invalid
        ?assertEqual(
            {error, cursor_invalid}, talk_record_ds:creation_contexts(U1, Cur, 20)
        )
    end).

%%%===================================================================
%%% capabilities
%%%===================================================================

capabilities_honest_test_() ->
    with_db(fun(_State) ->
        Cap = talk_record_ds:capabilities(),
        %% ASR/summary/voice 未接入处理器：如实 false（不虚称）
        ?assertEqual(false, maps:get(<<"asr_enabled">>, Cap)),
        ?assertEqual(false, maps:get(<<"summary_enabled">>, Cap)),
        ?assertEqual(false, maps:get(<<"processing_available">>, Cap)),
        ?assertEqual(false, maps:get(<<"voice_id_enabled">>, Cap)),
        ?assertEqual([<<"live_recording">>], maps:get(<<"source_types">>, Cap)),
        ?assert(lists:member(<<"personal">>, maps:get(<<"scopes">>, Cap))),
        ?assert(is_integer(maps:get(<<"max_duration_ms">>, Cap)))
    end).

%%%===================================================================
%%% Internal
%%%===================================================================

i(N) when is_integer(N) ->
    integer_to_binary(N).
