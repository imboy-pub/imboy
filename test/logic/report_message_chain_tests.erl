-module(report_message_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% G-01 真库配方入口（erl -eval run/1；隔离库 imboy_plan_g01_*@55427）
-export([run/1]).

%%% R-01 First-class Report Targets and Evidence —— 真 PostgreSQL 链路
%%% 运行：make eunit-local t=report_message_chain_tests
%%% 前置：本地 imboy_v1 schema 已应用 00000087。
%%%
%%% 覆盖：
%%%   * 迁移 00000087 列/CHECK 生效（message 形态、坏 sub_type、缺 scope 拒绝）
%%%   * repo create/9 evidence jsonb 落库 + (target_type,target_id,reporter) 幂等
%%%   * fetch_message_target 三表面统一形态
%%%   * create_message 全链：c2c/c2g/channel 合法举报人成功；
%%%     跨会话/跨群/跨频道 IDOR 拒绝；撤回=已删除拒绝；E2EE 同意门
%%%   * find_by_id / page_admin 带新列；admin_detail 出参
%%%   * G-01:LOCAL:01 证据白名单真库落库边界 + 管理端可见范围=落库范围
%%%   * G-01:LOCAL:02 resolve 真实状态落库/审计行/未处置不假填主体时限；
%%%     举报工单→处置→申诉→独立复审全链真实状态；申诉窗口配置驱动

%% ===================================================================
%% fixtures（真库唯一键惯例：毫秒时间戳基址 + 自清理）
%% ===================================================================

base_id() ->
    erlang:system_time(millisecond) * 1000 + rand:uniform(899999).

create_user(Uid) ->
    Account = <<"ru", (integer_to_binary(Uid))/binary>>,
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
                " VALUES ($1, $2, 'x', '127.0.0.1', '')"
            >>,
            [Uid, Account]
        ),
    ok.

cleanup_user(Uid) ->
    _ = elib_pg:query(<<"DELETE FROM public.report_ticket WHERE reporter_uid = $1">>, [Uid]),
    _ = elib_pg:query(<<"DELETE FROM public.\"user\" WHERE id = $1">>, [Uid]),
    ok.

insert_c2c(Id, FromId, ToId, E2EE) ->
    {ok, _} =
        personal_message_order_repo:execute_formal(
            FromId,
            ToId,
            c2c_msg_key(Id),
            <<
                "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, e2ee, payload, conv_seq)"
                " VALUES ($1, $2, $3, $4, 'text', $5::jsonb, $6, $7)"
            >>,
            [
                Id,
                FromId,
                ToId,
                <<"cm-", (integer_to_binary(Id))/binary>>,
                E2EE,
                <<"{\"t\":\"hi\"}">>
            ]
        ),
    Id.

insert_c2g(Id, FromId, Gid, E2EE) ->
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.msg_c2g (id, topic_id, from_id, to_id, msg_id, msg_type, e2ee, payload)"
                " VALUES ($1, 0, $2, $3, $4, 'text', $5::jsonb, $6::jsonb)"
            >>,
            [Id, FromId, Gid, <<"gm-", (integer_to_binary(Id))/binary>>, E2EE, <<"{\"t\":\"gh\"}">>]
        ),
    Id.

insert_group(Gid, OwnerId) ->
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.\"group\" (id, owner_uid, creator_uid, title)"
                " VALUES ($1, $2, $2, $3)"
            >>,
            [Gid, OwnerId, <<"rg-", (integer_to_binary(Gid))/binary>>]
        ),
    ok.

insert_group_member(Gid, Uid) ->
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.group_member (id, group_id, user_id, role, is_join, status)"
                " VALUES ($1, $2, $3, 0, true, 1)"
            >>,
            [base_id(), Gid, Uid]
        ),
    ok.

cleanup_group(Gid) ->
    _ = elib_pg:query(<<"DELETE FROM public.group_member WHERE group_id = $1">>, [Gid]),
    _ = elib_pg:query(<<"DELETE FROM public.\"group\" WHERE id = $1">>, [Gid]),
    ok.

insert_channel(ChannelId, OwnerId, Visibility) ->
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.channel (id, name, creator_uid, custom_id, visibility, access_type, join_policy, status)"
                " VALUES ($1, $2, $3, $4, $5, 0, 0, 1)"
            >>,
            [
                ChannelId,
                <<"rc-", (integer_to_binary(ChannelId))/binary>>,
                OwnerId,
                <<"c", (integer_to_binary(ChannelId))/binary>>,
                Visibility
            ]
        ),
    ok.

insert_channel_sub(ChannelId, Uid) ->
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.channel_subscription (id, channel_id, user_id, status)"
                " VALUES ($1, $2, $3, 1)"
            >>,
            [base_id(), ChannelId, Uid]
        ),
    ok.

insert_channel_message(Id, ChannelId, AuthorId, Revoked) ->
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.channel_message (id, channel_id, author_id, content, msg_type, status, revoked)"
                " VALUES ($1, $2, $3, $4, 'channel_text', 1, $5)"
            >>,
            [Id, ChannelId, AuthorId, <<"channel content">>, Revoked]
        ),
    Id.

cleanup_channel(ChannelId) ->
    _ = elib_pg:query(<<"DELETE FROM public.channel_subscription WHERE channel_id = $1">>, [
        ChannelId
    ]),
    _ = elib_pg:query(<<"DELETE FROM public.channel_message WHERE channel_id = $1">>, [ChannelId]),
    _ = elib_pg:query(<<"DELETE FROM public.channel WHERE id = $1">>, [ChannelId]),
    ok.

%% c2c/c2g 消息的客户端定位键 = msg_id（insert 时构造）
c2c_msg_key(Id) -> <<"cm-", (integer_to_binary(Id))/binary>>.
c2g_msg_key(Id) -> <<"gm-", (integer_to_binary(Id))/binary>>.

cleanup_msg(MsgId) ->
    _ = elib_pg:query(<<"DELETE FROM public.msg_c2c WHERE id = $1">>, [MsgId]),
    _ = elib_pg:query(<<"DELETE FROM public.msg_c2g WHERE id = $1">>, [MsgId]),
    ok.

seed_world() ->
    %% 用户：作者 2001 系 / 收件举报人 3001 系 / 群成员 4001 系 / 外部者 9999 系
    Author = base_id() + 1,
    Recipient = base_id() + 2,
    Member = base_id() + 3,
    Outsider = base_id() + 4,
    [create_user(U) || U <- [Author, Recipient, Member, Outsider]],
    C2CId = base_id() + 10,
    insert_c2c(C2CId, Author, Recipient, <<"null">>),
    C2CE2EEId = base_id() + 11,
    insert_c2c(C2CE2EEId, Author, Recipient, <<"{\"v\":\"OLM.V1\"}">>),
    Gid = base_id() + 20,
    insert_group(Gid, Author),
    insert_group_member(Gid, Author),
    insert_group_member(Gid, Member),
    C2GId = base_id() + 12,
    insert_c2g(C2GId, Author, Gid, <<"null">>),
    ChannelId = base_id() + 21,
    insert_channel(ChannelId, Author, 1),
    insert_channel_sub(ChannelId, Member),
    ChMsgId = base_id() + 13,
    insert_channel_message(ChMsgId, ChannelId, Author, false),
    ChRevokedId = base_id() + 14,
    insert_channel_message(ChRevokedId, ChannelId, Author, true),
    #{
        author => Author,
        recipient => Recipient,
        member => Member,
        outsider => Outsider,
        c2c_id => C2CId,
        c2c_e2ee_id => C2CE2EEId,
        c2g_id => C2GId,
        gid => Gid,
        channel_id => ChannelId,
        ch_msg_id => ChMsgId,
        ch_revoked_id => ChRevokedId
    }.

cleanup_world(W) ->
    cleanup_msg(maps:get(c2c_id, W)),
    cleanup_msg(maps:get(c2c_e2ee_id, W)),
    cleanup_msg(maps:get(c2g_id, W)),
    {ok, _} = elib_pg:execute(
        <<"DELETE FROM public.msg_store_seq WHERE conv_key=$1">>,
        [msg_archive_repo:conv_key(<<"c2c">>, maps:get(author, W), maps:get(recipient, W))]
    ),
    cleanup_channel(maps:get(channel_id, W)),
    cleanup_group(maps:get(gid, W)),
    [
        cleanup_user(U)
     || U <- [
            maps:get(author, W), maps:get(recipient, W), maps:get(member, W), maps:get(outsider, W)
        ]
    ],
    ok.

%% 宏参数不能包含 try/after 表达式，seed/cleanup/执行移入普通函数。
with_world(TestFun) ->
    ?TEST_WITH_DB(fun() -> run_with_world(TestFun) end).

run_with_world(TestFun) ->
    W = seed_world(),
    try
        TestFun(W)
    after
        cleanup_world(W)
    end.

%% repo 行的 evidence 列按 epgsql 默认是 JSON 文本 binary，断言前解码。
decode_row_evidence(Row) ->
    Raw = maps:get(<<"evidence">>, Row, #{}),
    case Raw of
        Map when is_map(Map) -> Map;
        Bin when is_binary(Bin) -> jsone:decode(Bin);
        _ -> #{}
    end.

%% ===================================================================
%% 迁移 00000087：列与 CHECK
%% ===================================================================

migration_columns_exist_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, Cols} = elib_pg:query(
            <<
                "SELECT column_name FROM information_schema.columns"
                " WHERE table_name = 'report_ticket'"
            >>,
            []
        ),
        Names = [maps:get(<<"column_name">>, C) || C <- Cols],
        ?assert(lists:member(<<"target_sub_type">>, Names)),
        ?assert(lists:member(<<"target_scope_id">>, Names)),
        ?assert(lists:member(<<"target_author_id">>, Names)),
        ?assert(lists:member(<<"evidence">>, Names))
    end).

check_constraints_reject_bad_shapes_test_() ->
    ?TEST_WITH_DB(fun check_constraints_body/0).

check_constraints_body() ->
    Uid = base_id(),
    create_user(Uid),
    try
        %% message + 坏 sub_type
        ?assertMatch(
            {error, _},
            elib_pg:query(
                <<
                    "INSERT INTO public.report_ticket"
                    " (id, target_type, target_id, target_sub_type, target_scope_id, target_author_id, reporter_uid, reason)"
                    " VALUES ($1, 'message', $2, 'p2p', 1, 1, $3, 'spam')"
                >>,
                [base_id(), base_id(), Uid]
            )
        ),
        %% message 缺 scope/author（0 不满足 > 0）
        ?assertMatch(
            {error, _},
            elib_pg:query(
                <<
                    "INSERT INTO public.report_ticket"
                    " (id, target_type, target_id, target_sub_type, reporter_uid, reason)"
                    " VALUES ($1, 'message', $2, 'c2c', $3, 'spam')"
                >>,
                [base_id(), base_id(), Uid]
            )
        ),
        %% message 合法形态可入（INSERT 无 RETURNING → {ok, Count}）
        ?assertMatch(
            {ok, _},
            elib_pg:query(
                <<
                    "INSERT INTO public.report_ticket"
                    " (id, target_type, target_id, target_sub_type, target_scope_id, target_author_id, reporter_uid, reason, evidence)"
                    " VALUES ($1, 'message', $2, 'c2c', $3, $4, $5, 'spam', $6::jsonb)"
                >>,
                [base_id(), base_id(), base_id(), base_id(), Uid, <<"{\"e2ee\":false}">>]
            )
        ),
        %% 旧 user 形态兼容（空 sub_type / 0 scope）
        ?assertMatch(
            {ok, _},
            elib_pg:query(
                <<
                    "INSERT INTO public.report_ticket"
                    " (id, target_type, target_id, reporter_uid, reason)"
                    " VALUES ($1, 'user', $2, $3, 'spam')"
                >>,
                [base_id(), base_id(), Uid]
            )
        )
    after
        cleanup_user(Uid)
    end.

%% ===================================================================
%% repo：evidence 落库 + 幂等
%% ===================================================================

repo_create_message_and_duplicate_test_() ->
    ?TEST_WITH_DB(fun repo_create_message_body/0).

repo_create_message_body() ->
    Uid = base_id(),
    create_user(Uid),
    TargetId = base_id(),
    try
        Evidence = #{
            <<"e2ee">> => false,
            <<"content_state">> => <<"present">>,
            <<"content_excerpt">> => <<"你好垃圾消息"/utf8>>,
            <<"server_content_hash">> => <<"ab12">>
        },
        {ok, ReportId} = report_ticket_repo:create(
            <<"message">>, TargetId, <<"c2c">>, 11, 22, Uid, <<"spam">>, <<>>, Evidence
        ),
        ?assert(ReportId > 0),
        %% 同 (target_type, target_id, reporter_uid) 幂等
        ?assertEqual(
            {error, already_reported},
            report_ticket_repo:create(
                <<"message">>, TargetId, <<"c2c">>, 11, 22, Uid, <<"spam">>, <<>>, #{}
            )
        ),
        %% 行回读：evidence 往返一致
        Row = report_ticket_repo:find_by_id(ReportId),
        ?assertEqual(<<"message">>, maps:get(<<"target_type">>, Row)),
        ?assertEqual(<<"c2c">>, maps:get(<<"target_sub_type">>, Row)),
        ?assertEqual(11, maps:get(<<"target_scope_id">>, Row)),
        ?assertEqual(22, maps:get(<<"target_author_id">>, Row)),
        StoredEvidence = decode_row_evidence(Row),
        ?assertEqual(<<"你好垃圾消息"/utf8>>, maps:get(<<"content_excerpt">>, StoredEvidence)),
        ?assertEqual(false, maps:get(<<"e2ee">>, StoredEvidence))
    after
        cleanup_user(Uid)
    end.

%% ===================================================================
%% fetch_message_target：三表面统一形态
%% ===================================================================

fetch_target_shapes_test_() ->
    with_world(fun(W) ->
        {ok, C2C} = report_ticket_ds:fetch_message_target(c2c, c2c_msg_key(maps:get(c2c_id, W))),
        ?assertEqual(maps:get(author, W), maps:get(from_id, C2C)),
        ?assertEqual(maps:get(recipient, W), maps:get(to_id, C2C)),
        ?assertEqual(false, maps:get(e2ee, C2C)),

        {ok, E2EE} = report_ticket_ds:fetch_message_target(
            c2c, c2c_msg_key(maps:get(c2c_e2ee_id, W))
        ),
        ?assertEqual(true, maps:get(e2ee, E2EE)),

        {ok, C2G} = report_ticket_ds:fetch_message_target(c2g, c2g_msg_key(maps:get(c2g_id, W))),
        ?assertEqual(maps:get(gid, W), maps:get(scope_id, C2G)),
        ?assertEqual(false, maps:get(e2ee, C2G)),

        {ok, Ch} = report_ticket_ds:fetch_message_target(channel, maps:get(ch_msg_id, W)),
        ?assertEqual(maps:get(channel_id, W), maps:get(scope_id, Ch)),
        ?assertEqual(false, maps:get(channel_public, Ch)),

        ?assertEqual(
            {error, not_found},
            report_ticket_ds:fetch_message_target(c2c, <<"no-such-msg-id">>)
        )
    end).

%% ===================================================================
%% create_message 全链（logic → ds → 真 PG）
%% ===================================================================

full_chain_c2c_success_and_idor_test_() ->
    with_world(fun(W) ->
        ?assertMatch(
            {ok, #{<<"target_type">> := <<"message">>, <<"target_sub_type">> := <<"c2c">>}},
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"spam">>,
                #{<<"content_excerpt">> => <<"你好垃圾消息"/utf8>>}
            )
        ),
        %% 跨会话 IDOR：无关用户举报
        ?assertEqual(
            {error, <<"无权举报该消息"/utf8>>},
            report_logic:create_message(
                maps:get(outsider, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"spam">>,
                #{}
            )
        ),
        %% 不存在
        ?assertEqual(
            {error, <<"举报对象不存在或已被删除"/utf8>>},
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                <<"no-such-msg">>,
                integer_to_binary(maps:get(author, W)),
                <<"spam">>,
                #{}
            )
        )
    end).

full_chain_c2g_member_gate_test_() ->
    with_world(fun(W) ->
        ?assertMatch(
            {ok, _},
            report_logic:create_message(
                maps:get(member, W),
                <<"c2g">>,
                c2g_msg_key(maps:get(c2g_id, W)),
                integer_to_binary(maps:get(gid, W)),
                <<"spam">>,
                #{}
            )
        ),
        %% 非群成员（跨群 IDOR）
        ?assertEqual(
            {error, <<"无权举报该消息"/utf8>>},
            report_logic:create_message(
                maps:get(outsider, W),
                <<"c2g">>,
                c2g_msg_key(maps:get(c2g_id, W)),
                integer_to_binary(maps:get(gid, W)),
                <<"spam">>,
                #{}
            )
        ),
        %% scope 指向别的群
        ?assertEqual(
            {error, <<"举报参数无效"/utf8>>},
            report_logic:create_message(
                maps:get(member, W),
                <<"c2g">>,
                c2g_msg_key(maps:get(c2g_id, W)),
                integer_to_binary(base_id()),
                <<"spam">>,
                #{}
            )
        )
    end).

full_chain_channel_access_matrix_test_() ->
    with_world(fun(W) ->
        %% 订阅者成功
        ?assertMatch(
            {ok, _},
            report_logic:create_message(
                maps:get(member, W),
                <<"channel">>,
                integer_to_binary(maps:get(ch_msg_id, W)),
                integer_to_binary(maps:get(channel_id, W)),
                <<"spam">>,
                #{}
            )
        ),
        %% 未订阅（私密频道 visibility=1）拒绝
        ?assertEqual(
            {error, <<"无权举报该消息"/utf8>>},
            report_logic:create_message(
                maps:get(outsider, W),
                <<"channel">>,
                integer_to_binary(maps:get(ch_msg_id, W)),
                integer_to_binary(maps:get(channel_id, W)),
                <<"spam">>,
                #{}
            )
        ),
        %% 撤回消息 = 已删除语义
        ?assertEqual(
            {error, <<"举报对象不存在或已被删除"/utf8>>},
            report_logic:create_message(
                maps:get(member, W),
                <<"channel">>,
                integer_to_binary(maps:get(ch_revoked_id, W)),
                integer_to_binary(maps:get(channel_id, W)),
                <<"spam">>,
                #{}
            )
        )
    end).

full_chain_e2ee_consent_gate_test_() ->
    with_world(fun(W) ->
        %% E2EE：无同意带摘录 → 拒绝
        ?assertEqual(
            {error, <<"提交加密消息内容需要您的明确同意"/utf8>>},
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_e2ee_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"spam">>,
                #{<<"content_excerpt">> => <<"秘密"/utf8>>}
            )
        ),
        %% E2EE：明确同意带摘录 → 成功且服务端未触碰密文
        {ok, #{<<"report_id">> := ReportId}} =
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_e2ee_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"spam">>,
                #{
                    <<"content_excerpt">> => <<"秘密"/utf8>>,
                    <<"e2ee_consent">> => true,
                    <<"content_hash">> => <<"ab12">>
                }
            ),
        Row = report_ticket_repo:find_by_id(ReportId),
        Evidence = decode_row_evidence(Row),
        ?assertEqual(true, maps:get(<<"e2ee">>, Evidence)),
        ?assertEqual(<<"秘密"/utf8>>, maps:get(<<"content_excerpt">>, Evidence)),
        ?assertEqual(<<"ab12">>, maps:get(<<"content_hash">>, Evidence)),
        ?assertNot(maps:is_key(<<"server_content_hash">>, Evidence)),
        %% E2EE：拒绝提交内容 → 仅元数据也可举报
        ?assertMatch(
            {ok, _},
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"other">>,
                #{<<"content_hash">> => <<"cd34">>}
            )
        )
    end).

admin_detail_returns_bound_evidence_only_test_() ->
    with_world(fun(W) ->
        {ok, #{<<"report_id">> := ReportId}} =
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"spam">>,
                #{<<"content_excerpt">> => <<"被举报内容"/utf8>>}
            ),
        {ok, Detail} = report_logic:admin_detail(1, integer_to_binary(ReportId)),
        ?assertEqual(ReportId, maps:get(<<"id">>, Detail)),
        ?assertEqual(<<"message">>, maps:get(<<"target_type">>, Detail)),
        ?assertEqual(<<"c2c">>, maps:get(<<"target_sub_type">>, Detail)),
        ?assertEqual(maps:get(author, W), maps:get(<<"target_author_id">>, Detail)),
        ?assertEqual(
            <<"被举报内容"/utf8>>,
            maps:get(<<"content_excerpt">>, maps:get(<<"evidence">>, Detail))
        ),
        %% 无效/不存在工单
        ?assertEqual(
            {error, <<"举报记录不存在"/utf8>>},
            report_logic:admin_detail(1, integer_to_binary(base_id()))
        )
    end).

page_admin_includes_new_columns_test_() ->
    with_world(fun(W) ->
        {ok, _} = report_logic:create_message(
            maps:get(recipient, W),
            <<"c2c">>,
            c2c_msg_key(maps:get(c2c_id, W)),
            integer_to_binary(maps:get(author, W)),
            <<"spam">>,
            #{}
        ),
        {ok, Payload} = report_logic:admin_list(<<"message">>, -1, 1, 10, #{}),
        List = maps:get(list, Payload),
        ?assert(length(List) >= 1),
        [First | _] = List,
        ?assertEqual(<<"message">>, maps:get(<<"target_type">>, First)),
        ?assertEqual(<<"c2c">>, maps:get(<<"target_sub_type">>, First)),
        ?assert(maps:is_key(<<"evidence">>, First))
    end).

%% ===================================================================
%% G-01：最小证据范围对用户可见且越权拒绝；申诉/结果有真实后端状态
%% ===================================================================

%% 工单级审计残留清理（appeal → action → action_log，票体由 cleanup_user 收）
cleanup_case_audit(ReportId) ->
    _ = elib_pg:query(<<"DELETE FROM public.moderation_appeal WHERE case_id = $1">>, [
        ReportId
    ]),
    _ = elib_pg:query(<<"DELETE FROM public.moderation_action WHERE case_id = $1">>, [
        ReportId
    ]),
    _ = elib_pg:query(<<"DELETE FROM public.report_action_log WHERE report_id = $1">>, [
        ReportId
    ]),
    ok.

action_log_count(ReportId) ->
    {ok, #{<<"count">> := N}} = elib_pg:one(
        <<"SELECT COUNT(*) AS count FROM public.report_action_log WHERE report_id = $1">>,
        [ReportId]
    ),
    ec_cnv:to_integer(N).

%% G-01:LOCAL:01 —— 最小证据范围：白名单外字段（会话转储/密钥材料/自由
%% 文本）一律不落库；服务端只补充有限核验元数据（明文才有内容哈希）；
%% 管理端可见证据 = 落库证据 = 用户提交白名单，不扩大也不缩小。
evidence_scope_whitelist_enforced_and_visible_test_() ->
    with_world(fun(W) ->
        Evidence = #{
            <<"content_excerpt">> => <<"涉赌引流文案"/utf8>>,
            <<"content_hash">> => <<"AB12CD34">>,
            <<"client_msg_id">> => <<"cmid-g01">>,
            <<"msg_type">> => <<"text">>,
            <<"sent_at">> => 1732190000123,
            <<"e2ee_consent">> => false,
            <<"conversation_dump">> => <<"被诱导上传的完整聊天记录"/utf8>>,
            <<"session_keys">> => <<"e2ee-session-key-material">>,
            <<"raw_payload">> => <<"非声明字段">>,
            <<"free_note">> => <<"自由文本">>
        },
        {ok, #{<<"report_id">> := ReportId}} =
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"fraud">>,
                Evidence
            ),
        Stored = decode_row_evidence(report_ticket_repo:find_by_id(ReportId)),
        %% 白名单外字段不落库（服务端不收集未声明证据）
        [
            ?assertNot(maps:is_key(K, Stored))
         || K <- [
                <<"conversation_dump">>,
                <<"session_keys">>,
                <<"raw_payload">>,
                <<"free_note">>
            ]
        ],
        %% 落库键 ⊆ 白名单 ∪ 服务端核验元数据
        Allowed = [
            <<"e2ee">>,
            <<"content_state">>,
            <<"server_msg_id">>,
            <<"server_content_hash">>,
            <<"content_excerpt">>,
            <<"content_hash">>,
            <<"client_msg_id">>,
            <<"msg_type">>,
            <<"sent_at">>,
            <<"e2ee_consent">>
        ],
        [?assert(lists:member(K, Allowed)) || K <- maps:keys(Stored)],
        %% hash 归一小写；摘录/元数据按用户提交原样往返；未同意不落同意标记
        ?assertEqual(<<"ab12cd34">>, maps:get(<<"content_hash">>, Stored)),
        ?assertEqual(<<"涉赌引流文案"/utf8>>, maps:get(<<"content_excerpt">>, Stored)),
        ?assertEqual(<<"cmid-g01">>, maps:get(<<"client_msg_id">>, Stored)),
        ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Stored)),
        ?assertEqual(1732190000123, maps:get(<<"sent_at">>, Stored)),
        ?assertNot(maps:is_key(<<"e2ee_consent">>, Stored)),
        %% 明文消息：服务端仅补 e2ee/content_state/server_msg_id/内容哈希
        ?assertEqual(false, maps:get(<<"e2ee">>, Stored)),
        ?assertEqual(<<"present">>, maps:get(<<"content_state">>, Stored)),
        ?assertEqual(
            integer_to_binary(maps:get(c2c_id, W)),
            maps:get(<<"server_msg_id">>, Stored)
        ),
        {ok, Target} = report_ticket_ds:fetch_message_target(c2c, c2c_msg_key(maps:get(c2c_id, W))),
        ExpectedHash = binary:encode_hex(crypto:hash(sha256, maps:get(payload, Target))),
        ?assertEqual(ExpectedHash, maps:get(<<"server_content_hash">>, Stored)),
        %% 管理端详情可见证据 = 落库证据（可见范围不扩大）
        {ok, Detail} = report_logic:admin_detail(1, integer_to_binary(ReportId)),
        ?assertEqual(Stored, maps:get(<<"evidence">>, Detail)),
        %% E2EE 对照：同意摘录时白名单同样生效，服务端不触碰密文（无内容哈希）
        {ok, #{<<"report_id">> := E2EEReportId}} =
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_e2ee_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"fraud">>,
                Evidence#{<<"e2ee_consent">> => true}
            ),
        E2EEStored = decode_row_evidence(report_ticket_repo:find_by_id(E2EEReportId)),
        [
            ?assertNot(maps:is_key(K, E2EEStored))
         || K <- [
                <<"conversation_dump">>,
                <<"session_keys">>,
                <<"raw_payload">>,
                <<"free_note">>,
                <<"server_content_hash">>
            ]
        ],
        ?assertEqual(true, maps:get(<<"e2ee">>, E2EEStored)),
        ?assertEqual(true, maps:get(<<"e2ee_consent">>, E2EEStored)),
        ?assertEqual(<<"涉赌引流文案"/utf8>>, maps:get(<<"content_excerpt">>, E2EEStored))
    end).

%% G-01:LOCAL:02 —— 处置结果有真实后端状态：resolve 真实落
%% status/handled_by/handled_at + report_action_log 审计行，detail 与
%% 按状态过滤的 list 均可按真实状态检索；未处置工单主体未知不假填
%% （handled_by=0、无任何承诺时限字段），失败处置不产生假审计行。
resolve_result_real_state_and_no_fabricated_fields_test_() ->
    with_world(fun(W) ->
        {ok, #{<<"report_id">> := ReportId}} =
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"spam">>,
                #{<<"content_excerpt">> => <<"处置链证据"/utf8>>}
            ),
        try
            %% 受理态：主体未知不假填（status=0 / handled_by=null / 无时限承诺）
            Pending = report_ticket_repo:find_by_id(ReportId),
            ?assertEqual(0, maps:get(<<"status">>, Pending)),
            ?assertEqual(null, maps:get(<<"handled_by">>, Pending)),
            ?assertEqual(null, maps:get(<<"handled_at">>, Pending)),
            {ok, PendingDetail} = report_logic:admin_detail(1, integer_to_binary(ReportId)),
            ?assertEqual(0, maps:get(<<"status">>, PendingDetail)),
            ?assertEqual(0, maps:get(<<"handled_by">>, PendingDetail)),
            %% 出参键 ⊆ 真实列集：不假造 operator_name/deadline/sla 之类字段
            RealColumns = [
                <<"id">>,
                <<"target_type">>,
                <<"target_id">>,
                <<"target_sub_type">>,
                <<"target_scope_id">>,
                <<"target_author_id">>,
                <<"reporter_uid">>,
                <<"reason">>,
                <<"description">>,
                <<"evidence">>,
                <<"status">>,
                <<"handled_by">>,
                <<"handled_at">>,
                <<"created_at">>,
                <<"updated_at">>
            ],
            [?assert(lists:member(K, RealColumns)) || K <- maps:keys(PendingDetail)],
            %% 类型不匹配 resolve：拒绝、状态不动、零审计行
            ?assertEqual(
                {error, <<"举报类型不匹配"/utf8>>},
                report_logic:admin_resolve(4290101, <<"group">>, ReportId, 2, <<"x">>)
            ),
            ?assertEqual(0, action_log_count(ReportId)),
            %% 合法 resolve：结果真实落库（status/handled_by/handled_at + 审计行）
            AdmUid = 4290102,
            ?assertEqual(
                ok,
                report_logic:admin_resolve(
                    AdmUid, <<"message">>, ReportId, 2, <<"确认违规，维持处置"/utf8>>
                )
            ),
            Resolved = report_ticket_repo:find_by_id(ReportId),
            ?assertEqual(2, maps:get(<<"status">>, Resolved)),
            ?assertEqual(AdmUid, maps:get(<<"handled_by">>, Resolved)),
            ?assertNotEqual(null, maps:get(<<"handled_at">>, Resolved)),
            ?assertEqual(1, action_log_count(ReportId)),
            {ok, [LogRow]} = elib_pg:query(
                <<
                    "SELECT operator_uid, result, note FROM public.report_action_log"
                    " WHERE report_id = $1"
                >>,
                [ReportId]
            ),
            ?assertEqual(AdmUid, maps:get(<<"operator_uid">>, LogRow)),
            ?assertEqual(2, maps:get(<<"result">>, LogRow)),
            ?assertEqual(<<"确认违规，维持处置"/utf8>>, maps:get(<<"note">>, LogRow)),
            %% 结果对管理面可见：detail 反映真实状态；list 按状态可检索
            {ok, Detail} = report_logic:admin_detail(1, integer_to_binary(ReportId)),
            ?assertEqual(2, maps:get(<<"status">>, Detail)),
            ?assertEqual(AdmUid, maps:get(<<"handled_by">>, Detail)),
            {ok, Payload2} = report_logic:admin_list(<<"message">>, 2, 1, 100, #{}),
            ?assert(
                lists:member(ReportId, [maps:get(<<"id">>, R) || R <- maps:get(list, Payload2)])
            ),
            {ok, Payload0} = report_logic:admin_list(<<"message">>, 0, 1, 100, #{}),
            ?assertNot(
                lists:member(ReportId, [maps:get(<<"id">>, R) || R <- maps:get(list, Payload0)])
            )
        after
            cleanup_case_audit(ReportId)
        end
    end).

%% G-01:LOCAL:02 —— 申诉有真实后端状态：真实举报工单 → 处置动作
%% （executed）→ 申诉 pending（reviewer_id=0/reviewed_at=null，主体未知
%% 不假填）→ 独立复审 accepted → 处置动作真实 reversed，用户面 my_list
%% 反映真实状态；非被处置人申诉被拒（越权拒绝）。
appeal_chain_from_report_case_real_states_test_() ->
    with_world(fun(W) ->
        {ok, #{<<"report_id">> := ReportId}} =
            report_logic:create_message(
                maps:get(recipient, W),
                <<"c2c">>,
                c2c_msg_key(maps:get(c2c_id, W)),
                integer_to_binary(maps:get(author, W)),
                <<"spam">>,
                #{<<"content_excerpt">> => <<"申诉链证据"/utf8>>}
            ),
        try
            Moderator = 4290201,
            {ok, Action} = moderation_action_logic:execute(
                Moderator,
                ReportId,
                <<"warning">>,
                maps:get(author, W),
                #{reason => <<"发送违规引流消息"/utf8>>}
            ),
            ?assertEqual(<<"executed">>, maps:get(<<"status">>, Action)),
            ?assertEqual(ReportId, maps:get(<<"case_id">>, Action)),
            ?assertEqual(maps:get(author, W), maps:get(<<"target_uid">>, Action)),
            ActionId = maps:get(<<"id">>, Action),
            %% 越权拒绝：非被处置人不能对该处置申诉
            ?assertEqual(
                {error, <<"只能对针对自己的处置提出申诉"/utf8>>},
                moderation_appeal_logic:submit(
                    maps:get(recipient, W), ActionId, <<"我不是被处置人"/utf8>>
                )
            ),
            %% 被处置人申诉：pending 真实落库；复审主体未知不假填
            {ok, AppealView} = moderation_appeal_logic:submit(
                maps:get(author, W), ActionId, <<"处置过重，请求复核"/utf8>>
            ),
            ?assertEqual(<<"pending">>, maps:get(<<"status">>, AppealView)),
            AppealId = maps:get(<<"id">>, AppealView),
            {ok, AppealRow} = moderation_appeal_repo:find_by_id(AppealId),
            ?assertEqual(<<"pending">>, maps:get(<<"status">>, AppealRow)),
            ?assertEqual(0, maps:get(<<"reviewer_id">>, AppealRow)),
            ?assertEqual(null, maps:get(<<"reviewed_at">>, AppealRow)),
            %% 用户面可自助查询真实申诉状态；举报人/case 关联不外泄
            {ok, [MyAppeal | _]} = moderation_appeal_logic:my_list(maps:get(author, W)),
            ?assertEqual(<<"pending">>, maps:get(<<"status">>, MyAppeal)),
            ?assertNot(maps:is_key(<<"case_id">>, MyAppeal)),
            ?assertNot(maps:is_key(<<"reporter">>, MyAppeal)),
            ?assertNot(maps:is_key(<<"reviewer_id">>, MyAppeal)),
            %% 原处置执行者复审被拒（复审主体独立性）
            ?assertEqual(
                {error, <<"原处置执行者不能复审该申诉"/utf8>>},
                moderation_appeal_logic:review(Moderator, AppealId, <<"accept">>, <<"自审"/utf8>>)
            ),
            %% 独立复审 accepted：申诉与处置状态真实翻转并可见
            Reviewer = 4290202,
            {ok, Reviewed} =
                moderation_appeal_logic:review(Reviewer, AppealId, <<"accept">>, <<"证据不足"/utf8>>),
            ?assertEqual(<<"accepted">>, maps:get(<<"status">>, Reviewed)),
            {ok, AppealRow2} = moderation_appeal_repo:find_by_id(AppealId),
            ?assertEqual(<<"accepted">>, maps:get(<<"status">>, AppealRow2)),
            ?assertEqual(Reviewer, maps:get(<<"reviewer_id">>, AppealRow2)),
            ?assertNotEqual(null, maps:get(<<"reviewed_at">>, AppealRow2)),
            {ok, ActionAfter} = moderation_action_repo:find_by_id(ActionId),
            ?assertEqual(<<"reversed">>, maps:get(<<"status">>, ActionAfter)),
            {ok, [MyAppeal2 | _]} = moderation_appeal_logic:my_list(maps:get(author, W)),
            ?assertEqual(<<"accepted">>, maps:get(<<"status">>, MyAppeal2))
        after
            cleanup_case_audit(ReportId)
        end
    end).

%% G-01:LOCAL:02 —— 申诉时限来自真实配置（appeal_window_days），不假填
%% 固定时限：正值按天数、0=不设限、负值回退默认 30 天。
appeal_window_config_driven_test_() ->
    [
        ?WITH_MECKS(
            [
                {config_ds, [
                    {'get', 2, fun(<<"appeal_window_days">>, _Default) -> 7 end}
                ]}
            ],
            fun() ->
                ?assertEqual(7 * 24 * 3600 * 1000, moderation_appeal_logic:appeal_window_ms())
            end
        ),
        ?WITH_MECKS(
            [
                {config_ds, [
                    {'get', 2, fun(<<"appeal_window_days">>, _Default) -> 0 end}
                ]}
            ],
            fun() ->
                ?assertEqual(
                    16 * 365 * 24 * 3600 * 1000,
                    moderation_appeal_logic:appeal_window_ms()
                )
            end
        ),
        ?WITH_MECKS(
            [
                {config_ds, [
                    {'get', 2, fun(<<"appeal_window_days">>, _Default) -> -5 end}
                ]}
            ],
            fun() ->
                ?assertEqual(
                    30 * 24 * 3600 * 1000,
                    moderation_appeal_logic:appeal_window_ms()
                )
            end
        )
    ].

%% ===================================================================
%% G-01 真库配方（erl -eval run/1；隔离库 imboy_plan_g01_*@55427，
%% 参考 s02/s03 erl -eval run/1 惯例）。池面重定向到脚本自有连接，
%% report/moderation/appeal 全链真跑；warning 通知腿（best-effort by
%% design）与 appeal 可用性门在配方内短路，状态断言全部走真表 SQL。
%% ===================================================================

run(ConnectOpts) when is_map(ConnectOpts) ->
    Required = [database, host, password, port, username],
    ?assertEqual(Required, lists:sort(maps:keys(maps:with(Required, ConnectOpts)))),
    ExpectedDb = unicode:characters_to_binary(maps:get(database, ConnectOpts)),
    ?assertEqual(
        match, re:run(ExpectedDb, <<"\\Aimboy_plan_g01_[a-z0-9_]+\\z">>, [{capture, none}])
    ),
    {ok, _} = application:ensure_all_started(epgsql),
    application:set_env(imboy, sql_driver, pgsql),
    %% gen_tcp 本栈不接受 binary host：host 用 string，其余值统一 binary；
    %% codecs 与 imboy 池配置同集（timestamptz → rfc3339 binary）
    Host = unicode:characters_to_list(maps:get(host, ConnectOpts)),
    ConnOpts = #{
        host => Host,
        port => maps:get(port, ConnectOpts),
        username => unicode:characters_to_binary(maps:get(username, ConnectOpts)),
        password => unicode:characters_to_binary(maps:get(password, ConnectOpts)),
        database => ExpectedDb,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    {ok, Conn} = epgsql:connect(ConnOpts),
    try
        g01_run_conn(Conn, ExpectedDb)
    after
        epgsql:close(Conn)
    end.

g01_run_conn(Conn, ExpectedDb) ->
    ?assertEqual(
        {ok, [#{<<"db">> => ExpectedDb}]},
        elib_pg:query(Conn, <<"SELECT current_database() AS db">>, [])
    ),
    {ok, [#{<<"v">> := V}]} =
        elib_pg:query(Conn, <<"SELECT max(version) AS v FROM schema_migrations">>, []),
    ?assert(V >= 287),
    Modules = [
        elib_pg,
        elib_tsid,
        agent_rate_limiter,
        elib_log,
        imboy_feature,
        message_ds,
        config_ds
    ],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(elib_pg, query, fun(Sql, Params) ->
            elib_pg:query(Conn, Sql, Params)
        end),
        meck:expect(elib_pg, one, fun(Sql, Params) ->
            case elib_pg:query(Conn, Sql, Params) of
                {ok, [Row | _]} -> {ok, Row};
                {ok, []} -> {ok, #{}};
                Error -> Error
            end
        end),
        meck:expect(elib_pg, execute, fun(Sql, Params) ->
            elib_pg:execute(Conn, Sql, Params)
        end),
        %% with_tx 在外层 BEGIN/ROLLBACK 内直跑（无嵌套事务语义损失：
        %% 回收由外层 ROLLBACK 兜底）
        meck:expect(elib_pg, with_tx, fun(Fun) -> Fun(Conn) end),
        meck:expect(elib_tsid, generate, 1, fun(_) ->
            erlang:unique_integer([positive, monotonic])
        end),
        meck:expect(agent_rate_limiter, allow, 2, fun(_, _) -> allow end),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        %% config 面（depcache ETS 需 app boot）：默认值口径（30 天窗口），
        %% 真实 config_ds 链路已由 eunit 轨道覆盖
        meck:expect(config_ds, get, 2, fun(_Key, Default) -> Default end),
        meck:expect(imboy_feature, enabled, 1, fun
            (appeal) -> true;
            (_) -> false
        end),
        %% warning 通知腿 best-effort（logic 侧本就丢弃结果）；eunit 轨道
        %% 已真跑 message_ds 全路径，配方内短路以聚焦真表状态断言
        meck:expect(message_ds, send_next, 6, fun(_, _, _, _, _, _) -> ok end),
        {ok, []} = elib_pg:query(Conn, <<"BEGIN">>, []),
        try
            g01_real_db_scenarios(Conn)
        after
            {ok, []} = elib_pg:query(Conn, <<"ROLLBACK">>, [])
        end
    after
        meck:unload(Modules)
    end.

g01_seed_world(Conn) ->
    Author = base_id() + 1,
    Recipient = base_id() + 2,
    Member = base_id() + 3,
    Outsider = base_id() + 4,
    [
        begin
            {ok, _} =
                elib_pg:query(
                    Conn,
                    <<
                        "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
                        " VALUES ($1, $2, 'x', '127.0.0.1', '')"
                    >>,
                    [U, <<"g01u-", (integer_to_binary(U))/binary>>]
                )
        end
     || U <- [Author, Recipient, Member, Outsider]
    ],
    C2CId = base_id() + 10,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, e2ee, payload, conv_seq)"
                " VALUES ($1, $2, $3, $4, 'text', 'null'::jsonb, '{\"t\":\"hi\"}'::jsonb, 1)"
            >>,
            [C2CId, Author, Recipient, c2c_msg_key(C2CId)]
        ),
    C2CE2EEId = base_id() + 11,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, e2ee, payload, conv_seq)"
                " VALUES ($1, $2, $3, $4, 'text', '{\"v\":\"OLM.V1\"}'::jsonb, '{\"ct\":\"...\"}'::jsonb, 2)"
            >>,
            [C2CE2EEId, Author, Recipient, c2c_msg_key(C2CE2EEId)]
        ),
    Gid = base_id() + 20,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.\"group\" (id, owner_uid, creator_uid, title)"
                " VALUES ($1, $2, $2, $3)"
            >>,
            [Gid, Author, <<"g01g-", (integer_to_binary(Gid))/binary>>]
        ),
    [
        begin
            {ok, _} =
                elib_pg:query(
                    Conn,
                    <<
                        "INSERT INTO public.group_member (id, group_id, user_id, role, is_join, status)"
                        " VALUES ($1, $2, $3, 0, true, 1)"
                    >>,
                    [base_id(), Gid, U]
                )
        end
     || U <- [Author, Member]
    ],
    C2GId = base_id() + 12,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.msg_c2g (id, topic_id, from_id, to_id, msg_id, msg_type, e2ee, payload)"
                " VALUES ($1, 0, $2, $3, $4, 'text', 'null'::jsonb, '{\"t\":\"gh\"}'::jsonb)"
            >>,
            [C2GId, Author, Gid, c2g_msg_key(C2GId)]
        ),
    ChannelId = base_id() + 21,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.channel (id, name, creator_uid, custom_id, visibility, access_type, join_policy, status)"
                " VALUES ($1, $2, $3, $4, 1, 0, 0, 1)"
            >>,
            [
                ChannelId,
                <<"g01c-", (integer_to_binary(ChannelId))/binary>>,
                Author,
                <<"c", (integer_to_binary(ChannelId))/binary>>
            ]
        ),
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.channel_subscription (id, channel_id, user_id, status)"
                " VALUES ($1, $2, $3, 1)"
            >>,
            [base_id(), ChannelId, Member]
        ),
    ChMsgId = base_id() + 13,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.channel_message (id, channel_id, author_id, content, msg_type, status, revoked)"
                " VALUES ($1, $2, $3, $4, 'channel_text', 1, false)"
            >>,
            [ChMsgId, ChannelId, Author, <<"channel content">>]
        ),
    #{
        author => Author,
        recipient => Recipient,
        member => Member,
        outsider => Outsider,
        c2c_id => C2CId,
        c2c_e2ee_id => C2CE2EEId,
        c2g_id => C2GId,
        gid => Gid,
        channel_id => ChannelId,
        ch_msg_id => ChMsgId
    }.

g01_real_db_scenarios(Conn) ->
    W = g01_seed_world(Conn),
    R = maps:get(recipient, W),
    A = maps:get(author, W),
    Outsider = maps:get(outsider, W),
    Member = maps:get(member, W),
    %% ---- LOCAL:01 最小证据范围：白名单外字段真表零落库 ----
    {ok, #{<<"report_id">> := PlainId}} =
        report_logic:create_message(
            R,
            <<"c2c">>,
            c2c_msg_key(maps:get(c2c_id, W)),
            integer_to_binary(A),
            <<"fraud">>,
            #{
                <<"content_excerpt">> => <<"涉赌引流文案"/utf8>>,
                <<"content_hash">> => <<"AB12CD34">>,
                <<"conversation_dump">> => <<"整段聊天记录"/utf8>>,
                <<"session_keys">> => <<"key-material">>
            }
        ),
    {ok, [PlainRow]} =
        elib_pg:query(
            Conn, <<"SELECT evidence FROM public.report_ticket WHERE id = $1">>, [PlainId]
        ),
    PlainEvidence = jsone:decode(maps:get(<<"evidence">>, PlainRow)),
    ?assertNot(maps:is_key(<<"conversation_dump">>, PlainEvidence)),
    ?assertNot(maps:is_key(<<"session_keys">>, PlainEvidence)),
    ?assertEqual(<<"ab12cd34">>, maps:get(<<"content_hash">>, PlainEvidence)),
    ?assert(maps:is_key(<<"server_content_hash">>, PlainEvidence)),
    ?assertEqual(false, maps:get(<<"e2ee">>, PlainEvidence)),
    %% 管理端可见证据 = 落库证据
    {ok, Detail} = report_logic:admin_detail(1, integer_to_binary(PlainId)),
    ?assertEqual(PlainEvidence, maps:get(<<"evidence">>, Detail)),
    %% ---- LOCAL:01 E2EE：自愿同意门 + 服务端不触碰密文 ----
    ?assertEqual(
        {error, <<"提交加密消息内容需要您的明确同意"/utf8>>},
        report_logic:create_message(
            R,
            <<"c2c">>,
            c2c_msg_key(maps:get(c2c_e2ee_id, W)),
            integer_to_binary(A),
            <<"fraud">>,
            #{<<"content_excerpt">> => <<"密文摘录"/utf8>>}
        )
    ),
    {ok, #{<<"report_id">> := E2EEId}} =
        report_logic:create_message(
            R,
            <<"c2c">>,
            c2c_msg_key(maps:get(c2c_e2ee_id, W)),
            integer_to_binary(A),
            <<"fraud">>,
            #{
                <<"content_excerpt">> => <<"密文摘录"/utf8>>,
                <<"e2ee_consent">> => true,
                <<"conversation_dump">> => <<"x">>
            }
        ),
    {ok, [E2EERow]} =
        elib_pg:query(
            Conn, <<"SELECT evidence FROM public.report_ticket WHERE id = $1">>, [E2EEId]
        ),
    E2EEEvidence = jsone:decode(maps:get(<<"evidence">>, E2EERow)),
    ?assertEqual(true, maps:get(<<"e2ee">>, E2EEEvidence)),
    ?assertEqual(true, maps:get(<<"e2ee_consent">>, E2EEEvidence)),
    ?assertNot(maps:is_key(<<"server_content_hash">>, E2EEEvidence)),
    ?assertNot(maps:is_key(<<"conversation_dump">>, E2EEEvidence)),
    %% ---- LOCAL:01 越权拒绝（真表）：三路 IDOR 零写入 ----
    ?assertEqual(
        {error, <<"无权举报该消息"/utf8>>},
        report_logic:create_message(
            Outsider,
            <<"c2c">>,
            c2c_msg_key(maps:get(c2c_id, W)),
            integer_to_binary(A),
            <<"spam">>,
            #{}
        )
    ),
    ?assertEqual(
        {error, <<"无权举报该消息"/utf8>>},
        report_logic:create_message(
            Outsider,
            <<"c2g">>,
            c2g_msg_key(maps:get(c2g_id, W)),
            integer_to_binary(maps:get(gid, W)),
            <<"spam">>,
            #{}
        )
    ),
    ?assertEqual(
        {error, <<"无权举报该消息"/utf8>>},
        report_logic:create_message(
            Outsider,
            <<"channel">>,
            integer_to_binary(maps:get(ch_msg_id, W)),
            integer_to_binary(maps:get(channel_id, W)),
            <<"spam">>,
            #{}
        )
    ),
    {ok, [#{<<"mine">> := 0}]} =
        elib_pg:query(
            Conn,
            <<"SELECT count(*) AS mine FROM public.report_ticket WHERE reporter_uid = $1">>,
            [Outsider]
        ),
    %% ---- LOCAL:02 结果真实后端状态 ----
    ?assertEqual(0, action_log_count(PlainId)),
    {ok, [PendingRow]} =
        elib_pg:query(
            Conn,
            <<"SELECT status, handled_by, handled_at FROM public.report_ticket WHERE id = $1">>,
            [PlainId]
        ),
    ?assertEqual(0, maps:get(<<"status">>, PendingRow)),
    ?assertEqual(null, maps:get(<<"handled_by">>, PendingRow)),
    ?assertEqual(null, maps:get(<<"handled_at">>, PendingRow)),
    ?assertEqual(
        {error, <<"举报类型不匹配"/utf8>>},
        report_logic:admin_resolve(4290301, <<"group">>, PlainId, 2, <<"x">>)
    ),
    ?assertEqual(0, action_log_count(PlainId)),
    AdmUid = 4290302,
    ?assertEqual(
        ok,
        report_logic:admin_resolve(AdmUid, <<"message">>, PlainId, 2, <<"确认违规"/utf8>>)
    ),
    {ok, [ResolvedRow]} =
        elib_pg:query(
            Conn,
            <<"SELECT status, handled_by, handled_at FROM public.report_ticket WHERE id = $1">>,
            [PlainId]
        ),
    ?assertEqual(2, maps:get(<<"status">>, ResolvedRow)),
    ?assertEqual(AdmUid, maps:get(<<"handled_by">>, ResolvedRow)),
    ?assertNotEqual(null, maps:get(<<"handled_at">>, ResolvedRow)),
    ?assertEqual(1, action_log_count(PlainId)),
    {ok, [LogRow]} =
        elib_pg:query(
            Conn,
            <<
                "SELECT operator_uid, result, note FROM public.report_action_log"
                " WHERE report_id = $1"
            >>,
            [PlainId]
        ),
    ?assertEqual(AdmUid, maps:get(<<"operator_uid">>, LogRow)),
    ?assertEqual(2, maps:get(<<"result">>, LogRow)),
    ?assertEqual(<<"确认违规"/utf8>>, maps:get(<<"note">>, LogRow)),
    %% ---- LOCAL:02 申诉真实后端状态（工单→处置→申诉→复审全真表）----
    Moderator = 4290303,
    Reviewer = 4290304,
    {ok, Action} = moderation_action_logic:execute(
        Moderator,
        E2EEId,
        <<"warning">>,
        A,
        #{reason => <<"违规引流警告"/utf8>>}
    ),
    ActionId = maps:get(<<"id">>, Action),
    ?assertEqual(<<"executed">>, maps:get(<<"status">>, Action)),
    ?assertEqual(
        {error, <<"只能对针对自己的处置提出申诉"/utf8>>},
        moderation_appeal_logic:submit(Member, ActionId, <<"我不是被处置人"/utf8>>)
    ),
    {ok, Appeal} = moderation_appeal_logic:submit(A, ActionId, <<"处置过重"/utf8>>),
    AppealId = maps:get(<<"id">>, Appeal),
    {ok, [AppealPending]} =
        elib_pg:query(
            Conn,
            <<
                "SELECT status, reviewer_id, reviewed_at FROM public.moderation_appeal"
                " WHERE id = $1"
            >>,
            [AppealId]
        ),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, AppealPending)),
    ?assertEqual(0, maps:get(<<"reviewer_id">>, AppealPending)),
    ?assertEqual(null, maps:get(<<"reviewed_at">>, AppealPending)),
    ?assertEqual(
        {error, <<"原处置执行者不能复审该申诉"/utf8>>},
        moderation_appeal_logic:review(Moderator, AppealId, <<"accept">>, <<"自审"/utf8>>)
    ),
    {ok, _} = moderation_appeal_logic:review(Reviewer, AppealId, <<"accept">>, <<"证据不足"/utf8>>),
    {ok, [AppealDone]} =
        elib_pg:query(
            Conn,
            <<
                "SELECT status, reviewer_id, reviewed_at FROM public.moderation_appeal"
                " WHERE id = $1"
            >>,
            [AppealId]
        ),
    ?assertEqual(<<"accepted">>, maps:get(<<"status">>, AppealDone)),
    ?assertEqual(Reviewer, maps:get(<<"reviewer_id">>, AppealDone)),
    ?assertNotEqual(null, maps:get(<<"reviewed_at">>, AppealDone)),
    {ok, [ActionDone]} =
        elib_pg:query(
            Conn,
            <<"SELECT status FROM public.moderation_action WHERE id = $1">>,
            [ActionId]
        ),
    ?assertEqual(<<"reversed">>, maps:get(<<"status">>, ActionDone)),
    ok.
