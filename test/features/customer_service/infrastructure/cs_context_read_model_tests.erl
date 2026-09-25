%%% @doc CS-BE-03（客户上下文读模型）真库 focused 套件。
%%%
%%% 冻结决策 CS-DEC-01（evidence/CS-DEC-01/decision.md）：客户上下文字段白名单
%%% **仅限**——掩码名 / 来源 / 首次与最近出现时间 / 同组织历史客服会话 / 明确
%%% 授权的备注；电话、邮箱、原始外部身份、跨组织资料、密文与凭证材料一律
%%% 禁止出站。越界字段需求一律 BLOCKED_SCOPE_EXPANSION。
%%%
%%% 读模型由 session/org/workspace/contact **事实派生**（零宽表、零迁移）：
%%% GET /api/v1/cs/organizations/:org_id/sessions/:id/context
%%%   * 授权：Seat `conversation.read`（cs_auth 门）+ **session ownership**
%%%     （application 复核：请求坐席必须是该会话当前经办 identity——
%%%     转接后新 Seat 可读、原 Seat 失去读权；queued 会话无经办 ⇒ 拒绝）；
%%%   * 撤权立即拒绝：seat suspend 后同一坐席的读被 `seat_disabled` 拒绝；
%%%   * 读操作零写副作用：无审计行、无版本推进、无 contact 行改动。
%%%
%%% 覆盖（CS-CONTEXT-01 验收）：
%%%   * 白名单逐字：顶层/嵌套**响应键集相等**；红线键（电话/邮箱/原始身份/
%%%     密文/凭证/close_reason/visit_token_id）深度扫描零出现；
%%%   * 事实派生：masked_name 取 subject_mask（原始 display_name 不出站）、
%%%     first_seen = contact.created_at、last_seen = max(会话活动)、
%%%     source 由会话事实推导（visit_token→widget）；
%%%   * 转接后新 Seat 可读 / 原 Seat 拒绝；撤权（suspend）立即拒绝；
%%%   * 同组织历史：仅本 contact 的会话、DESC 键集分页；
%%%   * 授权备注：active 备注的事实 stub（无密文无明文——EB 无 note 读面，
%%%     密文材料禁出站），软删行排除；
%%%   * 跨 Org：store 同语句裁决 not_found。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID scope；无真实数据；
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）。与 CS-INT-01 共享 scratch DB：
%%% 只做随机 scope 的 INSERT/UPDATE/DELETE，无 DROP/TRUNCATE。
-module(cs_context_read_model_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_context_read_model_pg_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun whitelist_exact_key_sets_and_no_redline_material/0},
        {timeout, 60, fun facts_derive_masked_name_seen_and_source/0},
        {timeout, 60, fun transfer_moves_ownership_to_new_seat/0},
        {timeout, 60, fun revoked_seat_is_rejected_immediately/0},
        {timeout, 60, fun read_has_zero_write_side_effects/0},
        {timeout, 60, fun queued_session_has_no_owner_to_read_context/0},
        {timeout, 60, fun history_is_same_contact_only_and_keyset_paginated/0},
        {timeout, 60, fun notes_stub_projection_excludes_deleted_and_cipher/0},
        {timeout, 60, fun cross_org_read_is_not_found/0}
    ];
cases({error, Reason}) ->
    erlang:error({csbe03_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% CS-DEC-01 白名单：响应键集逐字相等；红线材料深度零出现
%% ===================================================================

whitelist_exact_key_sets_and_no_redline_material() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        _NoteId = seed_note(Scope, 1700000400, active),
        {ok, View} = read_context(Scope, Seed),
        %% 顶层键集（逐字）：锚定会话 + 掩码名 + 来源 + 历史 + 备注。
        ?assertEqual(
            [contact, history, notes, session_id, source, workspace_id],
            lists:sort(maps:keys(View))
        ),
        ?assertEqual(
            [first_seen, last_seen, masked_name],
            lists:sort(maps:keys(maps:get(contact, View)))
        ),
        ?assertEqual([next_after_id, sessions], lists:sort(maps:keys(maps:get(history, View)))),
        [HistRow | _] = maps:get(sessions, maps:get(history, View)),
        ?assertEqual(
            [
                claimed_at,
                closed_at,
                conversation_id,
                id,
                queued_at,
                rating,
                status,
                version,
                workspace_id
            ],
            lists:sort(maps:keys(HistRow))
        ),
        [Note | _] = maps:get(notes, View),
        ?assertEqual(
            [created_at, created_by_identity_id, id],
            lists:sort(maps:keys(Note))
        ),
        %% 红线键深度扫描（任意层级）：电话/邮箱/原始身份/密文/凭证/内部审计列。
        assert_no_redline_keys_deep(View)
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 事实派生：掩码名（subject_mask 优先；原始 display_name 不出站）/
%% first_seen = contact.created_at / last_seen = max(会话活动) /
%% source = 会话事实（visit_token ⇒ widget）
%% ===================================================================

facts_derive_masked_name_seen_and_source() ->
    Scope = ?FIX:new_scope(),
    try
        %% contact.created_at 钉到固定过去值（其余时间取会话活动，确定性断言）。
        ok = ?FIX:exec(
            <<"UPDATE enterprise_contact SET created_at = to_timestamp($1),"
                " updated_at = NULL WHERE organization_id = $2 AND id = $3">>,
            [1690000000, org(Scope), maps:get(contact_id, Scope)]
        ),
        %% subject_mask 是既有掩码（wx***1 形态）：命中行优先于 display_name。
        IdentityId = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO enterprise_contact_identity"
                " (id, organization_id, contact_id, channel, subject_hmac, subject_mask)"
                " VALUES ($1, $2, $3, 'wechat', $4, 'wx***1')"
            >>,
            [IdentityId, org(Scope), maps:get(contact_id, Scope), hex64(IdentityId)]
        ),
        TokenId = seed_visit_token(Scope),
        SessionId = open_session(Scope, #{
            visit_token_id => TokenId, at => 1700000000
        }),
        ok = claim(Scope, SessionId, seat_identity(Scope), 1700000100),
        {ok, View} = read_context(Scope, #{
            session_id => SessionId, business_identity_id => seat_identity(Scope)
        }),
        Contact = maps:get(contact, View),
        ?assertEqual(<<"wx***1">>, maps:get(masked_name, Contact)),
        ?assertEqual(1690000000, maps:get(first_seen, Contact)),
        %% last_seen = max(contact.created_at, 会话活动) = claimed_at。
        ?assertEqual(1700000100, maps:get(last_seen, Contact)),
        %% source：visit_token_id 非空 ⇒ widget。
        ?assertEqual(<<"widget">>, maps:get(source, View)),
        ok
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 转接：新 Seat 可读（ownership 跟随会话），原 Seat 失去读权
%% ===================================================================

transfer_moves_ownership_to_new_seat() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        SessionId = maps:get(session_id, Seed),
        SeatB = make_seat(Scope),
        %% 原 Seat（A）读：OK。
        {ok, Before} = read_context(Scope, Seed),
        ?assertEqual(SessionId, maps:get(session_id, Before)),
        %% A → B 转接（version 2 = claim 后的版本）。
        {ok, _} = cs_session_app:transfer(org(Scope), #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            to_identity_id => SeatB,
            expected_version => 2,
            at => 1700000200
        }),
        %% 新 Seat B 可读（转接后 ownership 落到 B）。
        {ok, After} = read_context(Scope, #{
            session_id => SessionId, business_identity_id => SeatB
        }),
        ?assertEqual(SessionId, maps:get(session_id, After)),
        %% 原 Seat A 拒绝：ownership 已迁走。
        ?assertEqual(
            {error, {not_session_owner, seat_identity(Scope), SeatB}},
            read_context(Scope, Seed)
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 撤权立即拒绝：suspend 后同一坐席的读被 seat_disabled 拒绝
%% ===================================================================

revoked_seat_is_rejected_immediately() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        SessionId = maps:get(session_id, Seed),
        {ok, _} = read_context(Scope, Seed),
        {ok, _} = cs_seat_app:suspend_seat(org(Scope), #{
            workspace_id => ws(Scope),
            business_identity_id => seat_identity(Scope),
            at => 1700000300
        }),
        ?assertEqual(
            {error, seat_disabled},
            read_context(Scope, Seed)
        ),
        %% 恢复后可读（撤权是状态门，不是数据删除）。
        {ok, _} = cs_seat_app:resume_seat(org(Scope), #{
            workspace_id => ws(Scope),
            business_identity_id => seat_identity(Scope),
            at => 1700000301
        }),
        ?assertMatch({ok, _}, read_context(Scope, Seed)),
        %% 会话事实未被撤权动作改动（session 版本仍 = claim 后的 2）。
        {ok, Session} = cs_session_app:fetch_session(org(Scope), #{
            workspace_id => ws(Scope), session_id => SessionId
        }),
        ?assertEqual(2, maps:get(version, Session))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 读操作零写副作用：无审计行 / 无会话版本推进 / contact 行不变
%% ===================================================================

read_has_zero_write_side_effects() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        SessionId = maps:get(session_id, Seed),
        EventsBefore = ?FIX:count(org(Scope), events),
        ContactBefore = ?FIX:scalar(
            <<"SELECT (version, updated_at) FROM enterprise_contact"
                " WHERE organization_id = $1 AND id = $2">>,
            [org(Scope), maps:get(contact_id, Scope)]
        ),
        SessionsBefore = ?FIX:count(org(Scope), sessions),
        {ok, V1} = read_context(Scope, Seed),
        {ok, V2} = read_context(Scope, Seed),
        ?assertEqual(V1, V2),
        ?assertEqual(EventsBefore, ?FIX:count(org(Scope), events)),
        ?assertEqual(SessionsBefore, ?FIX:count(org(Scope), sessions)),
        ?assertEqual(
            ContactBefore,
            ?FIX:scalar(
                <<"SELECT (version, updated_at) FROM enterprise_contact"
                    " WHERE organization_id = $1 AND id = $2">>,
                [org(Scope), maps:get(contact_id, Scope)]
            )
        ),
        %% 幂等读不推进会话 CAS 版本（仍是 claim 后的 2）。
        {ok, Session} = cs_session_app:fetch_session(org(Scope), #{
            workspace_id => ws(Scope), session_id => SessionId
        }),
        ?assertEqual(2, maps:get(version, Session))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% queued 会话无经办坐席：无 ownership ⇒ 上下文不读（claim 后再读）
%% ===================================================================

queued_session_has_no_owner_to_read_context() ->
    Scope = ?FIX:new_scope(),
    try
        SessionId = open_session(Scope, #{at => 1700000000}),
        ?assertEqual(
            {error, {not_session_owner, seat_identity(Scope), undefined}},
            read_context(Scope, #{
                session_id => SessionId, business_identity_id => seat_identity(Scope)
            })
        ),
        %% claim 之后 ownership 成立，可读。
        ok = claim(Scope, SessionId, seat_identity(Scope), 1700000050),
        ?assertMatch(
            {ok, _},
            read_context(Scope, #{
                session_id => SessionId, business_identity_id => seat_identity(Scope)
            })
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 同组织历史：仅本 contact 的会话（同 Org 他 contact 不混入）；DESC 键集分页
%% ===================================================================

history_is_same_contact_only_and_keyset_paginated() ->
    %% 同一坐席要 claim 两条会话：seat 上限放宽到 2（否则第二条 claim 被
    %% seat_at_capacity 拒——读模型历史页的真实库约束夹具前提）。
    Scope = ?FIX:new_scope(#{max_concurrent => 2}),
    try
        %% 本 contact：两条会话（先旧后新，各自 claim 出 ownership）。
        OldId = open_session(Scope, #{at => 1700000000}),
        NewId = open_session(Scope, #{at => 1700000050, conv => fresh_conversation(Scope)}),
        ok = claim(Scope, OldId, seat_identity(Scope), 1700000060),
        ok = claim(Scope, NewId, seat_identity(Scope), 1700000070),
        %% 同 Org 另一 contact 的会话：不得进入本 contact 的历史。
        {OtherContact, OtherSession} = make_contact_session(Scope),
        %% 键集分页（C1~C4 冻结口径）：DESC 页满页 = **本页尾行 id** 为游标
        %% （下一页取 `id < 游标`）；不足一页 = undefined。Page1(limit=1)
        %% 只含 NewId，游标即 NewId；Page2(游标=NewId) 取更早的 OldId。
        {ok, Page1} = read_context(Scope, #{
            session_id => NewId, business_identity_id => seat_identity(Scope), limit => 1
        }),
        History1 = maps:get(sessions, maps:get(history, Page1)),
        ?assertEqual([NewId], [maps:get(id, R) || R <- History1]),
        ?assertEqual(NewId, maps:get(next_after_id, maps:get(history, Page1))),
        %% Page2：游标 = NewId，缺省 limit（50 > 1 行 ⇒ 不满页）→ 返回更早
        %% 的 OldId 且游标 undefined（分页结束）。
        {ok, Page2} = read_context(Scope, #{
            session_id => NewId,
            business_identity_id => seat_identity(Scope),
            after_id => NewId
        }),
        History2 = maps:get(sessions, maps:get(history, Page2)),
        ?assertEqual([OldId], [maps:get(id, R) || R <- History2]),
        ?assertEqual(undefined, maps:get(next_after_id, maps:get(history, Page2))),
        %% 他 contact 的会话绝不在任何一页。
        AllIds =
            [maps:get(id, R) || R <- History1] ++ [maps:get(id, R) || R <- History2],
        ?assertNot(lists:member(OtherSession, AllIds)),
        _ = OtherContact,
        ok
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 授权备注：active 事实 stub（id/created_by/created_at）按 id DESC；
%% 软删行排除；密文/密钥材料零出站（EB 无 note 读面 ⇒ 无正文投影）
%% ===================================================================

notes_stub_projection_excludes_deleted_and_cipher() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        N1 = seed_note(Scope, 1700000100, active),
        N2 = seed_note(Scope, 1700000200, active),
        N3 = seed_note(Scope, 1700000300, deleted),
        {ok, View} = read_context(Scope, Seed),
        Notes = maps:get(notes, View),
        ?assertEqual([N2, N1], [maps:get(id, N) || N <- Notes]),
        ?assertNot(lists:member(N3, [maps:get(id, N) || N <- Notes])),
        lists:foreach(
            fun(N) ->
                ?assertEqual(seat_identity(Scope), maps:get(created_by_identity_id, N)),
                ?assert(is_integer(maps:get(created_at, N)))
            end,
            Notes
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 跨 Org：store 同语句裁决 not_found（不区分不存在与跨租户）
%% ===================================================================

cross_org_read_is_not_found() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        %% 该坐席在另一 Org 也有 seat（隔离变量：不是 seat 门在拒绝）。
        OtherIdentity = make_seat_in(Scope, maps:get(other_org_id, Scope)),
        ?assertEqual(
            {error, not_found},
            cs_seat_app:session_context(
                maps:get(other_org_id, Scope),
                #{
                    workspace_id => maps:get(other_workspace_id, Scope),
                    session_id => maps:get(session_id, Seed),
                    business_identity_id => OtherIdentity
                }
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 夹具辅助（随机 TSID scope 内自建事实；无 DROP/TRUNCATE）
%% ===================================================================

org(Scope) ->
    maps:get(org_id, Scope).

ws(Scope) ->
    maps:get(workspace_id, Scope).

seat_identity(Scope) ->
    maps:get(service_identity_id, Scope).

%% 建一条「已 claim、有 ownership」的锚定会话；返回直驱读参数。
owned_session(Scope) ->
    SessionId = open_session(Scope, #{at => 1700000000}),
    ok = claim(Scope, SessionId, seat_identity(Scope), 1700000100),
    #{
        session_id => SessionId,
        business_identity_id => seat_identity(Scope),
        workspace_id => ws(Scope)
    }.

%% 开一条 queued 会话（每条会话独立 conversation——(org, conv) 未关闭唯一索引）。
open_session(Scope, Opts) ->
    ConversationId =
        case maps:get(conv, Opts, undefined) of
            undefined -> maps:get(conversation_id, Scope);
            Fresh -> Fresh
        end,
    {ok, Session} = cs_session_app:open_session(org(Scope), #{
        workspace_id => ws(Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => ConversationId,
        visit_token_id => maps:get(visit_token_id, Opts, undefined),
        at => maps:get(at, Opts, 1700000000)
    }),
    maps:get(id, Session).

claim(Scope, SessionId, IdentityId, At) ->
    {ok, _} = cs_session_app:claim(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => SessionId,
        business_identity_id => IdentityId,
        expected_version => 1,
        at => At
    }),
    ok.

%% 直驱 application 用例（HTTP 面的认证派生键在此显式给出）。
read_context(Scope, Seed) ->
    cs_seat_app:session_context(
        org(Scope),
        maps:merge(
            #{
                workspace_id => ws(Scope),
                session_id => maps:get(session_id, Seed),
                business_identity_id => maps:get(business_identity_id, Seed)
            },
            maps:with([after_id, limit], Seed)
        )
    ).

%% 新开一个 customer_service identity + enabled seat；返回 identity id。
make_seat(Scope) ->
    make_seat_in(Scope, org(Scope)).

make_seat_in(Scope, TargetOrg) ->
    Identity = ?FIX:id(),
    Owner = maps:get(owner_user_id, Scope),
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id, organization_id, function_key, display_name, status, version, created_by_user_id)"
            " VALUES ($1, $2, 'customer_service', $3, 'active', 1, $4)"
        >>,
        [Identity, TargetOrg, integer_to_binary(?FIX:id()), Owner]
    ),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_seat"
            " (organization_id, business_identity_id, function_key, enabled, max_concurrent,"
            "  created_by_user_id)"
            " VALUES ($1, $2, 'customer_service', true, 1, $3)"
        >>,
        [TargetOrg, Identity, Owner]
    ),
    Identity.

%% 同 Org 另一 contact + conversation + 会话（历史隔离的对照组）。
make_contact_session(Scope) ->
    Contact = ?FIX:id(),
    Service = seat_identity(Scope),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_contact"
            " (id, organization_id, imboy_user_id, status, display_name,"
            "  created_by_business_identity_id, version)"
            " VALUES ($1, $2, NULL, 'active', $3, $4, 1)"
        >>,
        [Contact, org(Scope), integer_to_binary(?FIX:id()), Service]
    ),
    SessionId = open_session_for(Scope, Contact, fresh_conversation_for(Scope, Contact)),
    {Contact, SessionId}.

open_session_for(Scope, ContactId, ConversationId) ->
    {ok, Session} = cs_session_app:open_session(org(Scope), #{
        workspace_id => ws(Scope),
        contact_id => ContactId,
        conversation_id => ConversationId,
        at => 1700000055
    }),
    maps:get(id, Session).

%% 新 conversation（(org, contact) 一致性由复合 FK 保证——必须带 contact）。
fresh_conversation_for(Scope, ContactId) ->
    fresh_conversation_in(Scope, ContactId).

fresh_conversation(Scope) ->
    fresh_conversation_in(Scope, maps:get(contact_id, Scope)).

fresh_conversation_in(Scope, ContactId) ->
    Conversation = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_conversation"
            " (id, organization_id, workspace_id, contact_id, business_identity_id, status, version,"
            "  notice_version, consent_at, consent_subject, consent_evidence_kind)"
            " VALUES ($1, $2, $3, $4, $5, 'active', 1, 'csbe03-notice-v1',"
            "  to_timestamp($6), $7, 'synthetic')"
        >>,
        [
            Conversation,
            org(Scope),
            ws(Scope),
            ContactId,
            seat_identity(Scope),
            1700000000,
            integer_to_binary(?FIX:id())
        ]
    ),
    Conversation.

%% visit token 行（source=widget 的事实源；digest 仅测试合成值）。
seed_visit_token(Scope) ->
    TokenId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_visit_token"
            " (id, organization_id, contact_id, token_digest, expires_at,"
            "  created_by_business_identity_id, created_by_user_id)"
            " VALUES ($1, $2, $3, $4, to_timestamp($5), $6, $7)"
        >>,
        [
            TokenId,
            org(Scope),
            maps:get(contact_id, Scope),
            hex64(TokenId),
            1900000000,
            seat_identity(Scope),
            maps:get(owner_user_id, Scope)
        ]
    ),
    TokenId.

%% note 行：密文可空（无 EB note 读面 ⇒ 读模型只消费行事实，不碰密文）。
%% 占位符 $6（created_at）被 deleted_at 与 created_at 两处引用——参数列表
%% 只给一次（epgsql 参数数量必须与占位符集合一致，多传即错）。
seed_note(Scope, CreatedAt, Status) ->
    NoteId = ?FIX:id(),
    DeletedAtSql =
        case Status of
            deleted -> <<"to_timestamp($6)">>;
            active -> <<"NULL">>
        end,
    Sql = <<
        "INSERT INTO enterprise_note"
        " (id, organization_id, contact_id, business_identity_id, body_cipher,"
        "  body_key_version, status, deleted_at, created_at)"
        " VALUES ($1, $2, $3, $4, NULL, NULL, $5, ", DeletedAtSql/binary,
        ", to_timestamp($6))"
    >>,
    Params = [NoteId, org(Scope), maps:get(contact_id, Scope), seat_identity(Scope), Status, CreatedAt],
    ok = ?FIX:exec(Sql, Params),
    NoteId.

%% 红线键深度扫描：CS-DEC-01 禁项 + 内部审计/凭证/密文材料。
assert_no_redline_keys_deep(Value) when is_map(Value) ->
    RedLine = redline_keys(),
    lists:foreach(
        fun(K) ->
            case lists:member(K, RedLine) of
                true -> erlang:error({redline_key_leaked, K});
                false -> ok
            end
        end,
        maps:keys(Value)
    ),
    lists:foreach(fun assert_no_redline_keys_deep/1, maps:values(Value));
assert_no_redline_keys_deep(Value) when is_list(Value) ->
    lists:foreach(fun assert_no_redline_keys_deep/1, Value);
assert_no_redline_keys_deep(_Scalar) ->
    ok.

redline_keys() ->
    [
        %% CS-DEC-01 禁项：联系方式 / 原始外部身份 / 跨组织资料。
        phone,
        email,
        subject,
        subject_hmac,
        subject_id,
        imboy_user_id,
        %% 密文 / 密钥 / 凭证材料（任何形态）。
        profile_cipher,
        profile,
        cipher,
        body_cipher,
        body,
        key_version,
        body_key_version,
        key_ref,
        digest,
        token_digest,
        key_digest,
        secret,
        %% 内部审计列（page 投影同款红线）。
        visit_token_id,
        close_reason,
        created_by_user_id
    ].

hex64(Seed) ->
    binary:encode_hex(crypto:hash(sha256, integer_to_binary(Seed)), lowercase).
