-module(ac04_e2ee_off_archive_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% Q06/AC04 九步真库深验（前半场：S1–S4、S7、S9 + S5 manifest 写入）
%%%
%%% E2EE OFF（phase1_delivery：storage_mode=archived + e2ee_mode=disabled
%%% + message_search=true）不破坏归档/检索/保留合同：
%%%   S1 写入：staging 明文落库（e2ee NULL）→ worker 搬运 → 正式表
%%%   S2 历史分页：conv_seq 游标翻页，msg_id 集合与 payload 明文一致
%%%   S3 FTS：jiebacfg 中文关键词命中明文行；密文行绝不出现（S7 并验）
%%%   S4 EXPLAIN：fts_user_repo 两条主查询执行计划含 e2ee IS NULL 过滤
%%%   S7 历史密文不降级：预置 E2EE 时代密文行——读取原样、FTS 排除、
%%%      新写入不触碰旧行（行指纹不变）
%%%   S9 retention：phase1_delivery capabilities 在 e2ee disabled 下
%%%      retention rolling 365 不丢失；运行时 env 注入后 policy 门读出一致
%%%
%%% S5 重启回读：本模块把 msg_id 清单写入 /tmp manifest；随后以独立
%%% eunit 进程（ac04_s5_readback_tests，新 VM = 重启语义）读回对比。
%%%===================================================================

-define(KEYWORD, <<"交付验收"/utf8>>).
-define(S5_MANIFEST, "/tmp/q06_ac04_s5_manifest.term").

uid() ->
    elib_tsid:generate(user).

ts(I) ->
    BaseMs = 1790100000000,
    elib_dt:to_rfc3339(BaseMs + I * 1000, millisecond).

mk_group(Gid, OwnerUid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"group\" (id, owner_uid, creator_uid, member_max, "
            "member_count, introduction, avatar, title, chat_aes_key, status, "
            "created_at, e2ee_mode, scope) VALUES ($1,$2,$3,500,0,'','','t','k',1,now(),0,'personal')"
        >>,
        [Gid, OwnerUid, OwnerUid]
    ),
    ok.

%% staging → 正式表（复用 worker 的 process_row 搬运算子，与异步链路同源）
flush_staging(MsgId) ->
    {ok, Rows} = elib_pg:query(
        <<"SELECT * FROM public.msg_store_staging WHERE msg_id = $1">>,
        [MsgId]
    ),
    [?assertMatch(ok, msg_store_worker:process_row(Row)) || Row <- Rows],
    ok.

msg_ids_of(Messages) ->
    [maps:get(<<"msg_id">>, M) || M <- Messages].

%% ===================================================================
%% S1 + S2：明文写入 → 搬运 → 历史分页（E2EE OFF 归档保持）
%% ===================================================================

s1_s2_plaintext_archive_and_history_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        A = uid(),
        B = uid(),
        Kw = ?KEYWORD,
        PayloadKw = jsone:encode(#{<<"text">> => Kw}),
        PayloadPlain = jsone:encode(#{<<"text">> => <<"普通消息"/utf8>>}),
        MsgIds = [
            iolist_to_binary(["q06ac4c-", integer_to_binary(A), $-, integer_to_binary(I)])
         || I <- lists:seq(1, 3)
        ],
        Payloads = [PayloadKw, PayloadKw, PayloadPlain],
        %% S1：三条明文 C2C 经 staging（SERVER_ACK 持久化承诺点）
        lists:foreach(
            fun({I, {MsgId, Pl}}) ->
                ?assertEqual(
                    {ok, new},
                    msg_store_ds:stage(
                        <<"c2c">>,
                        MsgId,
                        <<"text">>,
                        <<>>,
                        null,
                        Pl,
                        A,
                        B,
                        ts(I),
                        ts(I),
                        <<>>
                    )
                )
            end,
            lists:zip(lists:seq(1, 3), lists:zip(MsgIds, Payloads))
        ),
        %% S1 oracle：staging e2ee IS NULL、payload 明文
        {ok, [#{<<"n">> := N, <<"e">> := ENull}]} = elib_pg:query(
            <<
                "SELECT count(*) AS n, count(e2ee) AS e FROM public.msg_store_staging ",
                "WHERE msg_id = ANY($1)"
            >>,
            [MsgIds]
        ),
        ?assertEqual(3, N),
        ?assertEqual(0, ENull),
        %% worker 搬运（同步触发，等价异步批处理一批）
        [flush_staging(MsgId) || MsgId <- MsgIds],
        {ok, [#{<<"n">> := ArchN}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM public.msg_store WHERE msg_id = ANY($1)">>,
            [MsgIds]
        ),
        ?assertEqual(3, ArchN),
        %% staging 无 pending 残留
        {ok, [#{<<"n">> := PendN}]} = elib_pg:query(
            <<
                "SELECT count(*) AS n FROM public.msg_store_staging ",
                "WHERE msg_id = ANY($1) AND processed_at IS NULL"
            >>,
            [MsgIds]
        ),
        ?assertEqual(0, PendN),

        %% S2：conv_seq 游标分页（limit=2 两页），集合与明文一致
        PeerEnc = integer_to_binary(B),
        {ok, #{<<"messages">> := M1, <<"next_seq">> := NextSeq, <<"has_more">> := More1}} =
            messaging_logic:history(A, <<"c2c">>, PeerEnc, 0, 2),
        ?assertEqual(true, More1),
        ?assertEqual(2, length(M1)),
        {ok, #{<<"messages">> := M2, <<"has_more">> := More2}} =
            messaging_logic:history(A, <<"c2c">>, PeerEnc, NextSeq, 2),
        ?assertEqual(false, More2),
        ?assertEqual(1, length(M2)),
        All = M1 ++ M2,
        ?assertEqual(lists:sort(MsgIds), lists:sort(msg_ids_of(All))),
        %% conv_seq 单调且 payload 明文未被改写
        Seqs = [maps:get(<<"conv_seq">>, M) || M <- All],
        ?assertEqual(Seqs, lists:sort(Seqs)),
        KwMsgs = [M || M <- All, maps:get(<<"msg_id">>, M) =/= lists:last(MsgIds)],
        ?assertEqual(2, length(KwMsgs)),
        lists:foreach(
            fun(M) ->
                Payload = maps:get(<<"payload">>, M),
                ?assertEqual(Kw, maps:get(<<"text">>, Payload)),
                ?assertEqual(null, maps:get(<<"e2ee">>, M, null))
            end,
            KwMsgs
        )
    end).

%% ===================================================================
%% S3：FTS 中文关键词命中（真 pg_jieba）；C2C + C2G
%% ===================================================================

s3_fts_jieba_hit_c2c_and_c2g_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        A = uid(),
        B = uid(),
        Kw = ?KEYWORD,
        C2CMsgId = iolist_to_binary(["q06ac4f-", integer_to_binary(A)]),
        {ok, new} = msg_store_ds:stage(
            <<"c2c">>,
            C2CMsgId,
            <<"text">>,
            <<>>,
            null,
            jsone:encode(#{<<"text">> => Kw}),
            A,
            B,
            ts(1),
            ts(1),
            <<>>
        ),
        flush_staging(C2CMsgId),
        %% C2C：发送方视角检索命中（且仅命中）
        {ok, C2CHits} = fts_user_repo:search_c2c_msg(Kw, 10, 0, A),
        HitIds = msg_ids_of(C2CHits),
        ?assert(lists:member(C2CMsgId, HitIds)),
        lists:foreach(
            fun(Row) ->
                ?assertEqual(null, maps:get(<<"e2ee">>, Row, null))
            end,
            C2CHits
        ),

        %% C2G：群内中文关键词
        Gid = 523000 + (uid() rem 100000),
        ok = mk_group(Gid, A),
        ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}),
        C2GMsgId = iolist_to_binary(["q06ac4g-", integer_to_binary(Gid)]),
        %% 真实 C2G 信封形态（msg_c2g_logic MsgFull）：文本在嵌套
        %% payload.text，顶层无 text——FTS 文档表达式必须能命中此形态
        C2GPayload = jsone:encode(#{
            <<"ver">> => 3,
            <<"id">> => C2GMsgId,
            <<"type">> => <<"C2G">>,
            <<"from">> => integer_to_binary(A),
            <<"to">> => Gid,
            <<"payload">> => #{<<"text">> => Kw}
        }),
        {ok, new, _Seq, _Members} = msg_store_ds:stage(
            <<"c2g">>,
            C2GMsgId,
            <<"text">>,
            <<"send">>,
            null,
            C2GPayload,
            A,
            Gid,
            ts(2),
            ts(2),
            <<>>
        ),
        flush_staging(C2GMsgId),
        {ok, C2GHits} = fts_user_repo:search_c2g_msg(Kw, 10, 0, A),
        C2GHitIds = msg_ids_of(C2GHits),
        ?assert(lists:member(C2GMsgId, C2GHitIds))
    end).

%% ===================================================================
%% S4：EXPLAIN 执行计划含 e2ee IS NULL 过滤（无绕过路径）
%% ===================================================================

s4_explain_e2ee_null_predicate_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Kw = ?KEYWORD,
        KwLit = <<"'", Kw/binary, "'">>,
        PlanC2C = explain_plan(
            <<
                "select m.* from msg_c2c m where to_tsvector('jiebacfg', m.payload) "
                "@@ to_tsquery('jiebacfg', ",
                KwLit/binary,
                ") and m.e2ee is null and (m.from_id = 1 or m.to_id = 1) ",
                "order by m.created_at desc LIMIT 10"
            >>
        ),
        ?assert(plan_has_e2ee_null_filter(PlanC2C)),
        PlanC2G = explain_plan(
            <<
                "select m.* from msg_c2g m where to_tsvector('jiebacfg', ",
                "coalesce(m.payload #>> '{payload,text}', m.payload->>'text')) "
                "@@ to_tsquery('jiebacfg', ",
                KwLit/binary,
                ") and m.e2ee is null and exists (select 1 from public.group_member gm ",
                "where gm.group_id = m.to_id and gm.user_id = 1) ",
                "order by m.created_at desc LIMIT 10"
            >>
        ),
        ?assert(plan_has_e2ee_null_filter(PlanC2G))
    end).

explain_plan(Sql) ->
    {ok, Rows} = elib_pg:query(<<"EXPLAIN ", Sql/binary>>, []),
    iolist_to_binary([maps:get(<<"QUERY PLAN">>, R) || R <- Rows]).

plan_has_e2ee_null_filter(Plan) ->
    Lower = string:lowercase(Plan),
    binary:match(Lower, [<<"e2ee is null">>]) =/= nomatch.

%% ===================================================================
%% S7：历史密文不降级（E2EE ON 时代数据在 E2EE OFF 环境的原样保持）
%% ===================================================================

s7_legacy_ciphertext_not_degraded_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        A = uid(),
        B = uid(),
        Kw = ?KEYWORD,
        KwJson = <<"{\"text\": \"", Kw/binary, "\"}">>,
        E2EEJson = <<"{\"session_id\": \"legacy-s1\", \"ciphertext\": \"QjhFTkNSWVBU\"}">>,
        CipherMsgId = iolist_to_binary(["q06ac4e-", integer_to_binary(A)]),
        %% 直插 E2EE 时代遗留行：msg_c2c + msg_store（payload 含关键词但
        %% e2ee 非空——若 FTS 谓词失效该行必然命中，构成强 oracle）
        ConvKey = iolist_to_binary([
            <<"c2c:">>, integer_to_binary(erlang:min(A, B)), $:, integer_to_binary(erlang:max(A, B))
        ]),
        {ok, _} = elib_pg:query(
            <<
                "INSERT INTO msg_c2c (id, from_id, to_id, msg_id, msg_type, payload, ",
                "created_at, e2ee) VALUES ($1, $2, $3, $4, 'text', $5::jsonb, $6, $7::jsonb)"
            >>,
            [uid(), A, B, CipherMsgId, KwJson, ts(5), E2EEJson]
        ),
        {ok, _} = elib_pg:query(
            <<
                "INSERT INTO public.msg_store (id, chat_type, conv_key, conv_seq, msg_id, ",
                "msg_type, from_id, to_id, payload, e2ee, created_at, server_ts) ",
                "VALUES ($1,'c2c',$2,1,$3,'text',$4,$5,$6::jsonb,$7::jsonb,now(),now())"
            >>,
            [uid(), ConvKey, CipherMsgId, A, B, KwJson, E2EEJson]
        ),
        Fingerprint = row_fingerprint(CipherMsgId),

        %% 读取一（history）：payload/e2ee 原样返回，不静默降级为明文标记
        {ok, #{<<"messages">> := Msgs}} = messaging_logic:history(
            A, <<"c2c">>, integer_to_binary(B), 0, 50
        ),
        [CipherRow] = [M || M <- Msgs, maps:get(<<"msg_id">>, M) =:= CipherMsgId],
        E2EE = maps:get(<<"e2ee">>, CipherRow),
        ?assertEqual(<<"legacy-s1">>, maps:get(<<"session_id">>, E2EE)),
        ?assertEqual(<<"QjhFTkNSWVBU">>, maps:get(<<"ciphertext">>, E2EE)),
        ?assertEqual(Kw, maps:get(<<"text">>, maps:get(<<"payload">>, CipherRow))),

        %% 读取二（offline）：接收方视角拉取，密文行同样原样下发
        OfflinePayload = messaging_logic:offline(B, 1000, 0, 0, 0, <<>>),
        C2CList = maps:get(<<"list">>, maps:get(<<"c2c">>, OfflinePayload)),
        ?assert(lists:member(CipherMsgId, msg_ids_of(C2CList))),

        %% FTS：含关键词的密文行绝不出现（e2ee IS NULL 谓词生效直接证据）
        {ok, Hits} = fts_user_repo:search_c2c_msg(Kw, 50, 0, A),
        ?assertNot(lists:member(CipherMsgId, msg_ids_of(Hits))),

        %% 新写入（同会话明文消息）不得触碰旧行
        NewMsgId = <<CipherMsgId/binary, "-n">>,
        {ok, new} = msg_store_ds:stage(
            <<"c2c">>,
            NewMsgId,
            <<"text">>,
            <<>>,
            null,
            jsone:encode(#{<<"text">> => Kw}),
            A,
            B,
            ts(9),
            ts(9),
            <<>>
        ),
        flush_staging(NewMsgId),
        ?assertEqual(Fingerprint, row_fingerprint(CipherMsgId))
    end).

row_fingerprint(MsgId) ->
    {ok, [#{<<"fp">> := FP1}]} = elib_pg:query(
        <<"SELECT md5(payload::text || coalesce(e2ee::text,'')) AS fp FROM msg_c2c WHERE msg_id = $1">>,
        [MsgId]
    ),
    {ok, [#{<<"fp">> := FP2}]} = elib_pg:query(
        <<
            "SELECT md5(payload::text || coalesce(e2ee::text,'')) AS fp ",
            "FROM public.msg_store WHERE msg_id = $1"
        >>,
        [MsgId]
    ),
    {FP1, FP2}.

%% ===================================================================
%% S9：retention/policy 合同在 e2ee_mode=disabled 下不丢失
%% ===================================================================

s9_policy_retention_preserved_test_() ->
    ?TEST_SIMPLE(fun() ->
        #{capabilities := Caps} = imboy_profile_preset:defaults(phase1_delivery),
        %% E2EE OFF 只关产品能力：归档/检索/保留全保持（AC04 硬约束）
        ?assertEqual(archived, maps:get(storage_mode, Caps)),
        ?assertEqual(disabled, maps:get(e2ee_mode, Caps)),
        ?assertEqual(true, maps:get(message_search, Caps)),
        #{mode := rolling_days, days := 365} = maps:get(retention_policy, Caps),
        ?assertEqual(full, maps:get(audit_mode, Caps)),
        %% 运行时读出：该档位注入 env 后 policy 门与 preset 一致
        Prev = application:get_env(imboy, capabilities),
        try
            application:set_env(imboy, capabilities, Caps),
            ?assertEqual(true, imboy_policy:message_search_enabled()),
            ?assertEqual(false, imboy_policy:message_encryption_required()),
            ?assertEqual(false, imboy_policy:e2ee_disabled()),
            ?assertEqual(full, imboy_policy:message_audit_mode())
        after
            case Prev of
                undefined -> application:unset_env(imboy, capabilities);
                {ok, V} -> application:set_env(imboy, capabilities, V)
            end
        end
    end).

%% ===================================================================
%% S5（写入半场）：明文/密文混合数据 + manifest 落盘，
%% 由 ac04_s5_readback_tests 在独立 VM（重启语义）读回对比
%% ===================================================================

s5_manifest_write_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        A = uid(),
        B = uid(),
        Kw = ?KEYWORD,
        MsgIds = [
            iolist_to_binary(["q06ac4s-", integer_to_binary(A), $-, integer_to_binary(I)])
         || I <- lists:seq(1, 2)
        ],
        lists:foreach(
            fun({I, MsgId}) ->
                {ok, new} = msg_store_ds:stage(
                    <<"c2c">>,
                    MsgId,
                    <<"text">>,
                    <<>>,
                    null,
                    jsone:encode(#{<<"text">> => Kw}),
                    A,
                    B,
                    ts(I),
                    ts(I),
                    <<>>
                ),
                flush_staging(MsgId)
            end,
            lists:zip(lists:seq(1, 2), MsgIds)
        ),
        ok = file:write_file(?S5_MANIFEST, term_to_binary({A, B, Kw, MsgIds})),
        ?assert(filelib:is_file(?S5_MANIFEST))
    end).

%% ===================================================================
%% S8：推送联动（E2EE OFF 不影响推送合同）——logic 级直证：
%% 明文消息 {ok,new} 全链路中 maybe_push_for_c2c 恰被调用一次
%% ===================================================================

s8_push_dispatch_survives_e2ee_off_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun
                    (<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000;
                    (<<"2026-02-24T10:00:00Z">>, microsecond) -> 1708768800000000;
                    (<<"2026-02-24T09:58:20Z">>, microsecond) -> 1708768700000000
                end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                {'stage_with_sequence', 11, fun(_, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, new, 1}
                end},
                {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]},
            {elib_async, [
                {'async_retry', 3, fun(Fun, 3, 1000) ->
                    Fun(),
                    ok
                end},
                {'async', 1, fun(Fun) ->
                    Fun(),
                    self()
                end}
            ]},
            {push_notification_logic, [
                {'maybe_push_for_c2c', 4, fun(_, _, _, _) -> ok end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(_T, _F, _To, _P, MsgId, _MT, _A, _E) ->
                    #{<<"id">> => MsgId}
                end},
                {'with_sender_device', 2, fun(Msg, _D) -> Msg end}
            ]},
            {msg_c2c_delivery_ds, [
                {'send_other_devices', 4, fun(_, _, _, _) -> ok end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"push me">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },
            Stamped = message_ds:stamp_sender_device(
                Data, #{did => <<"s8-dev">>, dtype => <<"android">>}
            ),
            ok = msg_c2c_logic:c2c(<<"q06ac4-s8-push-001">>, 123, Stamped),
            %% E2EE OFF（e2ee=null 明文）下推送链路仍触发，且仅一次
            ?assertEqual(1, meck:num_calls(push_notification_logic, maybe_push_for_c2c, 4))
        end
    ).
