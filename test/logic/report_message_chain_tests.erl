-module(report_message_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

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
        elib_pg:query(
            <<
                "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, e2ee, payload)"
                " VALUES ($1, $2, $3, $4, 'text', $5::jsonb, $6)"
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
        Bin when is_binary(Bin) -> jsx:decode(Bin, [return_maps]);
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
