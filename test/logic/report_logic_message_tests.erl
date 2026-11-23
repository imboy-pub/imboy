-module(report_logic_message_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% S-03 真库配方入口（erl -eval 报表测试：run/1）
-export([run/1]).

%%% R-01 First-class Report Targets and Evidence —— report_logic:create_message/6
%%% 单元层（meck 数据边界）。真库链路见 report_message_chain_tests。
%%% 覆盖：白名单（chat_type/reason/evidence 字段）、限流、目标存在性、
%%% 删除/撤回/编辑语义、跨会话/跨群/跨频道 IDOR、E2EE 同意门、重复举报。

%% ---------- 公共 mock 基座 ----------
%% 注意：meck 对同一模块重复 setup 会整表重置，report_ticket_ds 的
%% fetch/create 期望必须合并在同一条目里。

rate_ok() ->
    {agent_rate_limiter, [
        {'allow', 2, fun(<<"report_create">>, _Uid) -> allow end}
    ]}.

log_ok() ->
    {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]}.

%% fetch 指定结果 + create 恒定结果
ds(ChatType, FetchResult, CreateResult) ->
    {report_ticket_ds, [
        {'fetch_message_target', 2, fun(CT, _Id) when CT =:= ChatType -> FetchResult end},
        {'create_message', 9, fun(_T, _ST, _S, _A, _R, _Re, _D, _E, _X) -> CreateResult end}
    ]}.

%% fetch 指定结果 + create 断言 Evidence
ds_check(ChatType, FetchResult, CheckFun) ->
    {report_ticket_ds, [
        {'fetch_message_target', 2, fun(CT, _Id) when CT =:= ChatType -> FetchResult end},
        {'create_message', 9, fun(_T, _ST, _S, _A, _R, _Re, _D, Evidence, _X) ->
            CheckFun(Evidence),
            {ok, 424242}
        end}
    ]}.

c2c_target() ->
    #{
        server_id => 55001,
        msg_id => <<"m-1">>,
        msg_type => <<"text">>,
        e2ee => false,
        from_id => 2001,
        to_id => 3001,
        scope_id => 3001,
        author_id => 2001,
        revoked => false,
        status => 1,
        edited_at => null,
        payload => <<"hello">>,
        created_at => null
    }.

c2g_target() ->
    #{
        server_id => 55002,
        msg_id => <<"m-2">>,
        msg_type => <<"text">>,
        e2ee => false,
        from_id => 2001,
        to_id => 7001,
        scope_id => 7001,
        author_id => 2001,
        revoked => false,
        status => 1,
        edited_at => null,
        payload => <<"group hello">>,
        created_at => null
    }.

channel_target() ->
    #{
        server_id => 55003,
        msg_id => <<"9001">>,
        msg_type => <<"channel_text">>,
        e2ee => false,
        from_id => 2001,
        to_id => 8001,
        scope_id => 8001,
        author_id => 2001,
        revoked => false,
        status => 1,
        edited_at => null,
        payload => <<"channel hello">>,
        created_at => null,
        channel_public => false
    }.

%% ---------- 参数与白名单 ----------

rejects_invalid_chat_type_test_() ->
    ?WITH_MECKS([], fun() ->
        ?assertEqual(
            {error, <<"举报类型无效"/utf8>>},
            report_logic:create_message(1001, <<"p2p">>, <<"55">>, <<"66">>, <<"spam">>, #{})
        )
    end).

rejects_zero_target_id_test_() ->
    ?WITH_MECKS([rate_ok()], fun() ->
        %% channel：行 ID 必须为正整数
        ?assertEqual(
            {error, <<"举报对象无效"/utf8>>},
            report_logic:create_message(1001, <<"channel">>, <<"0">>, <<"66">>, <<"spam">>, #{})
        ),
        %% c2c/c2g：msg_id 不能为空（纯空白归一后为空）
        ?assertEqual(
            {error, <<"举报对象无效"/utf8>>},
            report_logic:create_message(1001, <<"c2c">>, <<"  ">>, <<"66">>, <<"spam">>, #{})
        )
    end).

rejects_zero_scope_id_test_() ->
    ?WITH_MECKS([rate_ok()], fun() ->
        ?assertEqual(
            {error, <<"举报参数无效"/utf8>>},
            report_logic:create_message(1001, <<"c2c">>, <<"55">>, <<>>, <<"spam">>, #{})
        )
    end).

rejects_non_whitelisted_reason_test_() ->
    ?WITH_MECKS([rate_ok()], fun() ->
        ?assertEqual(
            {error, <<"举报原因无效"/utf8>>},
            report_logic:create_message(1001, <<"c2c">>, <<"55">>, <<"66">>, <<"随便写"/utf8>>, #{})
        ),
        ?assertEqual(
            {error, <<"举报原因无效"/utf8>>},
            report_logic:create_message(
                1001, <<"c2g">>, <<"55">>, <<"66">>, <<"SPAM; DROP TABLE">>, #{}
            )
        )
    end).

accepts_whitelisted_reason_test_() ->
    ?WITH_MECKS([rate_ok(), ds(c2c, {ok, c2c_target()}, {ok, 424242}), log_ok()], fun() ->
        lists:foreach(
            fun(Reason) ->
                ?assertMatch(
                    {ok, _},
                    report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, Reason, #{})
                )
            end,
            [
                <<"spam">>,
                <<"harassment">>,
                <<"inappropriate">>,
                <<"pornography">>,
                <<"fraud">>,
                <<"violence">>,
                <<"illegal">>,
                <<"other">>
            ]
        )
    end).

%% ---------- 限流 ----------

rate_limit_denies_before_db_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [
                {'allow', 2, fun(<<"report_create">>, 1001) -> {deny, requester_rate} end}
            ]},
            {report_ticket_ds, [
                {'fetch_message_target', 2, fun(_, _) -> erlang:error(should_not_be_called) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"举报过于频繁，请稍后再试"/utf8>>},
                report_logic:create_message(1001, <<"c2c">>, <<"55">>, <<"66">>, <<"spam">>, #{})
            )
        end
    ).

%% ---------- 存在性 / 删除语义 ----------

rejects_missing_target_test_() ->
    ?WITH_MECKS([rate_ok(), ds(c2c, {error, not_found}, {ok, 0})], fun() ->
        ?assertEqual(
            {error, <<"举报对象不存在或已被删除"/utf8>>},
            report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"66">>, <<"spam">>, #{})
        )
    end).

rejects_db_error_test_() ->
    ?WITH_MECKS([rate_ok(), ds(c2c, {error, db_down}, {ok, 0})], fun() ->
        ?assertEqual(
            {error, <<"举报失败"/utf8>>},
            report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"66">>, <<"spam">>, #{})
        )
    end).

rejects_revoked_channel_message_test_() ->
    Target = (channel_target())#{revoked => true},
    ?WITH_MECKS([rate_ok(), ds(channel, {ok, Target}, {ok, 0})], fun() ->
        ?assertEqual(
            {error, <<"举报对象不存在或已被删除"/utf8>>},
            report_logic:create_message(1001, <<"channel">>, <<"55">>, <<"8001">>, <<"spam">>, #{})
        )
    end).

rejects_unpublished_channel_message_test_() ->
    Target = (channel_target())#{status => 0},
    ?WITH_MECKS([rate_ok(), ds(channel, {ok, Target}, {ok, 0})], fun() ->
        ?assertEqual(
            {error, <<"举报对象不存在或已被删除"/utf8>>},
            report_logic:create_message(1001, <<"channel">>, <<"55">>, <<"8001">>, <<"spam">>, #{})
        )
    end).

allows_edited_channel_message_with_state_test_() ->
    Target = (channel_target())#{edited_at => <<"2026-09-05T00:00:00Z">>},
    ?WITH_MECKS(
        [
            rate_ok(),
            ds_check(channel, {ok, Target}, fun(Evidence) ->
                ?assertEqual(<<"edited">>, maps:get(<<"content_state">>, Evidence))
            end),
            {channel_subscription_ds, [{'is_subscribed', 2, fun(_C, _U) -> true end}]},
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(
                    1001, <<"channel">>, <<"55">>, <<"8001">>, <<"spam">>, #{}
                )
            )
        end
    ).

%% ---------- IDOR / 可见性 ----------

rejects_c2c_outside_conversation_test_() ->
    %% 举报人既非 from 也非 to（跨会话 IDOR）
    ?WITH_MECKS([rate_ok(), ds(c2c, {ok, c2c_target()}, {ok, 0})], fun() ->
        ?assertEqual(
            {error, <<"无权举报该消息"/utf8>>},
            report_logic:create_message(9999, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{})
        )
    end).

allows_c2c_recipient_test_() ->
    ?WITH_MECKS([rate_ok(), ds(c2c, {ok, c2c_target()}, {ok, 424242}), log_ok()], fun() ->
        ?assertMatch(
            {ok, _},
            report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{})
        )
    end).

rejects_c2g_non_member_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            ds(c2g, {ok, c2g_target()}, {ok, 0}),
            {group_member_ds, [{'is_member', 2, fun(_G, _U) -> false end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"无权举报该消息"/utf8>>},
                report_logic:create_message(9999, <<"c2g">>, <<"55">>, <<"7001">>, <<"spam">>, #{})
            )
        end
    ).

allows_c2g_member_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            ds(c2g, {ok, c2g_target()}, {ok, 424242}),
            {group_member_ds, [
                {'is_member', 2, fun
                    (7001, 4001) -> true;
                    (_G, _U) -> false
                end}
            ]},
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(4001, <<"c2g">>, <<"55">>, <<"7001">>, <<"spam">>, #{})
            )
        end
    ).

rejects_channel_unsubscribed_private_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            ds(channel, {ok, channel_target()}, {ok, 0}),
            {channel_subscription_ds, [{'is_subscribed', 2, fun(_C, _U) -> false end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"无权举报该消息"/utf8>>},
                report_logic:create_message(
                    9999, <<"channel">>, <<"55">>, <<"8001">>, <<"spam">>, #{}
                )
            )
        end
    ).

allows_channel_public_visitor_test_() ->
    Target = (channel_target())#{channel_public => true},
    ?WITH_MECKS(
        [
            rate_ok(),
            ds(channel, {ok, Target}, {ok, 424242}),
            {channel_subscription_ds, [
                {'is_subscribed', 2, fun(_C, _U) -> erlang:error(should_not_be_called) end}
            ]},
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(
                    9999, <<"channel">>, <<"55">>, <<"8001">>, <<"spam">>, #{}
                )
            )
        end
    ).

%% ---------- scope 一致性（防伪造 scope） ----------

rejects_c2c_scope_not_in_conversation_test_() ->
    ?WITH_MECKS([rate_ok(), ds(c2c, {ok, c2c_target()}, {ok, 0})], fun() ->
        ?assertEqual(
            {error, <<"举报参数无效"/utf8>>},
            report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"8888">>, <<"spam">>, #{})
        )
    end).

rejects_c2g_scope_mismatch_test_() ->
    %% 申报 scope 指向别的群
    ?WITH_MECKS(
        [
            rate_ok(),
            ds(c2g, {ok, c2g_target()}, {ok, 0}),
            {group_member_ds, [{'is_member', 2, fun(_G, _U) -> true end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"举报参数无效"/utf8>>},
                report_logic:create_message(4001, <<"c2g">>, <<"55">>, <<"7999">>, <<"spam">>, #{})
            )
        end
    ).

rejects_channel_scope_mismatch_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            ds(channel, {ok, channel_target()}, {ok, 0}),
            {channel_subscription_ds, [{'is_subscribed', 2, fun(_C, _U) -> true end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"举报参数无效"/utf8>>},
                report_logic:create_message(
                    1001, <<"channel">>, <<"55">>, <<"8999">>, <<"spam">>, #{}
                )
            )
        end
    ).

%% ---------- E2EE 证据门 ----------

rejects_e2ee_excerpt_without_consent_test_() ->
    Target = (c2c_target())#{e2ee => true},
    ?WITH_MECKS([rate_ok(), ds(c2c, {ok, Target}, {ok, 0})], fun() ->
        ?assertEqual(
            {error, <<"提交加密消息内容需要您的明确同意"/utf8>>},
            report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{
                <<"content_excerpt">> => <<"敏感明文"/utf8>>
            })
        )
    end).

allows_e2ee_excerpt_with_consent_test_() ->
    Target = (c2c_target())#{e2ee => true},
    ?WITH_MECKS(
        [
            rate_ok(),
            ds_check(c2c, {ok, Target}, fun(Evidence) ->
                ?assertEqual(<<"敏感明文"/utf8>>, maps:get(<<"content_excerpt">>, Evidence)),
                %% E2EE：服务端不得触碰密文 → 无 server_content_hash
                ?assertNot(maps:is_key(<<"server_content_hash">>, Evidence)),
                ?assertEqual(true, maps:get(<<"e2ee">>, Evidence))
            end),
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{
                    <<"content_excerpt">> => <<"敏感明文"/utf8>>,
                    <<"e2ee_consent">> => true
                })
            )
        end
    ).

allows_e2ee_metadata_only_test_() ->
    %% 拒绝提交明文 → 仅哈希/元数据，举报仍可成立
    Target = (c2c_target())#{e2ee => true},
    ?WITH_MECKS(
        [
            rate_ok(),
            ds_check(c2c, {ok, Target}, fun(Evidence) ->
                ?assertNot(maps:is_key(<<"content_excerpt">>, Evidence)),
                ?assertNot(maps:is_key(<<"server_content_hash">>, Evidence)),
                ?assertEqual(<<"ab12">>, maps:get(<<"content_hash">>, Evidence))
            end),
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{
                    <<"content_hash">> => <<"AB12">>
                })
            )
        end
    ).

plaintext_gets_server_hash_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            ds_check(c2c, {ok, c2c_target()}, fun(Evidence) ->
                Expected = binary:encode_hex(crypto:hash(sha256, <<"hello">>)),
                ?assertEqual(Expected, maps:get(<<"server_content_hash">>, Evidence)),
                ?assertEqual(false, maps:get(<<"e2ee">>, Evidence))
            end),
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{})
            )
        end
    ).

%% ---------- evidence schema ----------

drops_unknown_evidence_fields_and_bad_hash_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            ds_check(c2c, {ok, c2c_target()}, fun(Evidence) ->
                ?assertNot(maps:is_key(<<"evil_field">>, Evidence)),
                ?assertNot(maps:is_key(<<"content_hash">>, Evidence)),
                ?assertEqual(<<"m-1">>, maps:get(<<"client_msg_id">>, Evidence)),
                ?assertEqual(1770000000000, maps:get(<<"sent_at">>, Evidence))
            end),
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{
                    <<"evil_field">> => <<"x">>,
                    <<"content_hash">> => <<"not-hex!">>,
                    <<"client_msg_id">> => <<"m-1">>,
                    <<"sent_at">> => 1770000000000
                })
            )
        end
    ).

accepts_string_json_evidence_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            ds_check(c2c, {ok, c2c_target()}, fun(Evidence) ->
                ?assertEqual(<<"摘录"/utf8>>, maps:get(<<"content_excerpt">>, Evidence)),
                ?assertNot(maps:is_key(<<"junk">>, Evidence))
            end),
            log_ok()
        ],
        fun() ->
            Json = <<"{\"content_excerpt\": \"摘录\", \"junk\": 1}"/utf8>>,
            ?assertMatch(
                {ok, _},
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, Json)
            )
        end
    ).

%% ---------- 重复举报 / 成功形态 ----------

duplicate_report_maps_to_friendly_error_test_() ->
    ?WITH_MECKS(
        [rate_ok(), ds(c2c, {ok, c2c_target()}, {error, already_reported}), log_ok()], fun() ->
            ?assertEqual(
                {error, <<"您已举报过该对象"/utf8>>},
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{})
            )
        end
    ).

success_payload_shape_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            {report_ticket_ds, [
                {'fetch_message_target', 2, fun(c2g, _Id) -> {ok, c2g_target()} end},
                {'create_message', 9, fun(
                    TargetId, SubType, ScopeId, AuthorId, ReporterUid, Reason, _D, _E, _X
                ) ->
                    ?assertEqual(55002, TargetId),
                    ?assertEqual(<<"c2g">>, SubType),
                    ?assertEqual(7001, ScopeId),
                    ?assertEqual(2001, AuthorId),
                    ?assertEqual(4001, ReporterUid),
                    ?assertEqual(<<"spam">>, Reason),
                    {ok, 424248}
                end}
            ]},
            {group_member_ds, [{'is_member', 2, fun(_G, _U) -> true end}]},
            log_ok()
        ],
        fun() ->
            ?assertEqual(
                {ok, #{
                    <<"report_id">> => 424248,
                    <<"target_type">> => <<"message">>,
                    <<"target_sub_type">> => <<"c2g">>
                }},
                report_logic:create_message(4001, <<"c2g">>, <<"55">>, <<"7001">>, <<"spam">>, #{})
            )
        end
    ).

legacy_create_maps_duplicate_test_() ->
    %% 旧入口 create/5 的重复举报也应有明确文案（原来笼统“举报失败”）
    ?WITH_MECKS(
        [
            {report_ticket_ds, [
                {'create', 5, fun(_T, _I, _U, _R, _D) -> {error, already_reported} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"您已举报过该对象"/utf8>>},
                report_logic:create(1001, <<"user">>, 88, <<"spam">>, <<>>)
            )
        end
    ).

%% ===================================================================
%% S-03:LOCAL:01 / S-03:LOCAL:02 补充断言（2026-11-21 S-03 worker）
%%
%% LOCAL:01 越权/注入/过大 payload 明确失败：
%%   * 过大 evidence 字段按白名单边界截断/丢弃（excerpt 500、
%%     client_msg_id/msg_type 50、hash 64-hex、sent_at 非数字丢弃）
%%   * 过大 target/scope ID（>2^63-1）以业务错误明确失败——不落
%%     epgsql int8 编码崩溃路径（连接进程被打死的负例已实证）
%%   * 注入串（SQL/XSS/引号）只作为惰性数据透传参数，reason 白名单拒绝
%%   * 安全拒绝（IDOR/伪造 scope/E2EE 未同意）永不触达持久层
%% LOCAL:02 安全日志可追溯且不泄漏：
%%   * 处置审计写必要字段（ReportId/AdmUid/Result/Note）
%%   * 审计写失败 → 错误日志四元钉形 [事件,ReportId,AdmUid,Reason]，
%%     不吞处置结果也不回退越权；备注正文（含 PII/JWT canary）不进日志
%%   * 类型不匹配/不存在/非法参数 fail-closed，零 UPDATE 零审计行
%% ===================================================================

%% ---------- LOCAL:01 过大 payload ----------

oversized_evidence_fields_are_bounded_test_() ->
    Hex64 = binary:copy(<<"a">>, 64),
    ?WITH_MECKS(
        [
            rate_ok(),
            ds_check(c2c, {ok, c2c_target()}, fun(Evidence) ->
                ?assertEqual(500, byte_size(maps:get(<<"content_excerpt">>, Evidence))),
                ?assertEqual(binary:copy(<<"B">>, 500), maps:get(<<"content_excerpt">>, Evidence)),
                ?assertEqual(50, byte_size(maps:get(<<"client_msg_id">>, Evidence))),
                ?assertEqual(50, byte_size(maps:get(<<"msg_type">>, Evidence))),
                ?assertEqual(Hex64, maps:get(<<"content_hash">>, Evidence)),
                %% 非数字 sent_at → 0 → 白名单过滤丢弃
                ?assertNot(maps:is_key(<<"sent_at">>, Evidence))
            end),
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{
                    <<"content_excerpt">> => binary:copy(<<"B">>, 501),
                    <<"client_msg_id">> => binary:copy(<<"A">>, 51),
                    <<"msg_type">> => binary:copy(<<"C">>, 51),
                    <<"content_hash">> => string:uppercase(Hex64),
                    <<"sent_at">> => <<"garbage">>
                })
            )
        end
    ).

oversized_hash_dropped_and_legacy_desc_bounded_test_() ->
    ?WITH_MECKS(
        [
            rate_ok(),
            log_ok(),
            %% 同一模块的期望必须合并为一条（重复 setup 会整表重置）
            {report_ticket_ds, [
                {'fetch_message_target', 2, fun(c2c, _Id) -> {ok, c2c_target()} end},
                {'create_message', 9, fun(_T, _ST, _S, _A, _R, _Re, _D, Evidence, _X) ->
                    %% 65 位 hex 先钳到 64 字节边界再校验（bound 成立：≤64 hex）
                    ?assertEqual(64, byte_size(maps:get(<<"content_hash">>, Evidence))),
                    {ok, 424302}
                end},
                {'create', 5, fun(_T, _I, _U, _R, Desc) ->
                    ?assertEqual(500, byte_size(Desc)),
                    {ok, 424301}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(
                    3001,
                    <<"c2c">>,
                    <<"55">>,
                    <<"2001">>,
                    <<"spam">>,
                    #{<<"content_hash">> => binary:copy(<<"a">>, 65)}
                )
            ),
            %% 旧入口 description 4096 → 500
            ?assertMatch(
                {ok, _},
                report_logic:create(1001, <<"user">>, 88, <<"spam">>, binary:copy(<<"D">>, 4096))
            )
        end
    ).

oversized_target_keys_fail_cleanly_test_() ->
    HugeNum = <<"99999999999999999999999999">>,
    TenKb = binary:copy(<<"x">>, 10240),
    ?WITH_MECKS(
        [
            rate_ok(),
            {report_ticket_ds, [
                {'fetch_message_target', 2, fun
                    (c2c, Key) ->
                        %% 超长 msg_id 先钳到 50 字节边界再进查询
                        ?assert(byte_size(Key) =< 50),
                        {error, not_found};
                    (_CT, _Key) ->
                        {error, not_found}
                end}
            ]}
        ],
        fun() ->
            %% channel 巨大数字行 ID（>int8）→ 明确业务失败，不触发 epgsql 编码崩溃
            ?assertEqual(
                {error, <<"举报对象无效"/utf8>>},
                report_logic:create_message(
                    1001, <<"channel">>, HugeNum, <<"8001">>, <<"spam">>, #{}
                )
            ),
            %% c2c 超长 msg_id（10KB）→ 50 字节边界内按数据查询（失败=不存在）
            ?assertEqual(
                {error, <<"举报对象不存在或已被删除"/utf8>>},
                report_logic:create_message(1001, <<"c2c">>, TenKb, <<"2001">>, <<"spam">>, #{})
            ),
            %% 巨大数字 scope → 明确失败
            ?assertEqual(
                {error, <<"举报参数无效"/utf8>>},
                report_logic:create_message(1001, <<"c2c">>, <<"55">>, HugeNum, <<"spam">>, #{})
            )
        end
    ).

%% ---------- LOCAL:01 注入 ----------

injection_payloads_stay_inert_data_test_() ->
    SqlProbe = <<"); DELETE FROM report_ticket; --">>,
    XssProbe = <<"<script>alert('xss')</script>">>,
    QuoteProbe = <<"' OR '1'='1">>,
    ?WITH_MECKS(
        [
            rate_ok(),
            ds_check(c2c, {ok, c2c_target()}, fun(Evidence) ->
                %% 注入串原样作为数据进参数位（logic 层不做任何拼接/解释）
                ?assertEqual(SqlProbe, maps:get(<<"content_excerpt">>, Evidence)),
                ?assertEqual(QuoteProbe, maps:get(<<"client_msg_id">>, Evidence)),
                ?assertEqual(XssProbe, maps:get(<<"msg_type">>, Evidence))
            end),
            log_ok()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{
                    <<"content_excerpt">> => SqlProbe,
                    <<"client_msg_id">> => QuoteProbe,
                    <<"msg_type">> => XssProbe
                })
            ),
            %% reason 注入串不在白名单 → 明确失败
            ?assertEqual(
                {error, <<"举报原因无效"/utf8>>},
                report_logic:create_message(
                    3001, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam' OR 1=1 --">>, #{}
                )
            )
        end
    ).

%% ---------- LOCAL:01/02 安全拒绝不被吞、不回退越权 ----------

security_denial_never_reaches_persistence_test_() ->
    C2C = c2c_target(),
    C2G = c2g_target(),
    CH = channel_target(),
    Cases = [
        {"c2c_outside_conversation_idor", [rate_ok(), ds(c2c, {ok, C2C}, {ok, 0})],
            fun() ->
                report_logic:create_message(9999, <<"c2c">>, <<"55">>, <<"2001">>, <<"spam">>, #{})
            end,
            {error, <<"无权举报该消息"/utf8>>}},
        {"c2g_non_member_idor",
            [
                rate_ok(),
                ds(c2g, {ok, C2G}, {ok, 0}),
                {group_member_ds, [{'is_member', 2, fun(_, _) -> false end}]}
            ],
            fun() ->
                report_logic:create_message(9999, <<"c2g">>, <<"55">>, <<"7001">>, <<"spam">>, #{})
            end,
            {error, <<"无权举报该消息"/utf8>>}},
        {"channel_unsubscribed_idor",
            [
                rate_ok(),
                ds(channel, {ok, CH}, {ok, 0}),
                {channel_subscription_ds, [{'is_subscribed', 2, fun(_, _) -> false end}]}
            ],
            fun() ->
                report_logic:create_message(
                    9999, <<"channel">>, <<"55">>, <<"8001">>, <<"spam">>, #{}
                )
            end,
            {error, <<"无权举报该消息"/utf8>>}},
        {"forged_scope_rejected", [rate_ok(), ds(c2c, {ok, C2C}, {ok, 0})],
            fun() ->
                report_logic:create_message(3001, <<"c2c">>, <<"55">>, <<"8888">>, <<"spam">>, #{})
            end,
            {error, <<"举报参数无效"/utf8>>}},
        {"e2ee_excerpt_without_consent", [rate_ok(), ds(c2c, {ok, C2C#{e2ee => true}}, {ok, 0})],
            fun() ->
                report_logic:create_message(
                    3001,
                    <<"c2c">>,
                    <<"55">>,
                    <<"2001">>,
                    <<"spam">>,
                    #{<<"content_excerpt">> => <<"密文摘录"/utf8>>}
                )
            end,
            {error, <<"提交加密消息内容需要您的明确同意"/utf8>>}}
    ],
    [
        begin
            {Name, Mecks, Call, Expected} = Case,
            {Name,
                ?WITH_MECKS(Mecks, fun() ->
                    ?assertEqual(Expected, Call()),
                    %% 安全拒绝路径：持久化零调用（失败不能被后续阶段吞掉）
                    ?assertEqual(0, meck:num_calls(report_ticket_ds, create_message, 9))
                end)}
        end
     || Case <- Cases
    ].

%% ---------- LOCAL:02 审计可追溯 + 不泄漏 ----------

resolve_writes_audit_log_with_required_fields_test_() ->
    Row = #{<<"id">> => 77001, <<"target_type">> => <<"message">>},
    ?WITH_MECKS(
        [
            {report_ticket_ds, [
                {'find_by_id', 1, fun(77001) -> Row end},
                {'resolve', 4, fun(_Id, _R, _N, _U) -> {ok, 1} end}
            ]},
            {report_action_log_ds, [
                {'create', 4, fun(ReportId, AdmUid, Result, Note) ->
                    %% 审计必要字段：工单 ID + 操作管理员 + 处置结果 + 备注
                    ?assertEqual(77001, ReportId),
                    ?assertEqual(42, AdmUid),
                    ?assertEqual(1, Result),
                    ?assertEqual(<<"处置备注"/utf8>>, Note),
                    {ok, 88001}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                report_logic:admin_resolve(42, <<"message">>, 77001, 1, <<"处置备注"/utf8>>)
            ),
            ?assertEqual(1, meck:num_calls(report_action_log_ds, create, 4))
        end
    ).

resolve_audit_failure_is_logged_traceable_and_leak_free_test_() ->
    %% 备注 canary：真实用户资料（手机号）+ 秘密（JWT）——只允许进审计表，永不进日志
    Phone = <<"13800138000">>,
    %% 合成 JWT canary：拆段拼装，避免被秘密扫描器误判为真实字面量
    Jwt =
        <<"eyJhbGciOiJIUzI1NiJ9", ".", "eyJzdWIiOiIxMjM0NTY3ODkwIn0", ".",
            "dozjgNryP4J3jVmNHl0w5N0">>,
    Note = <<"举报人手机号"/utf8, Phone/binary, " ", Jwt/binary>>,
    Row = #{<<"id">> => 77002, <<"target_type">> => <<"message">>},
    ?WITH_MECKS(
        [
            {report_ticket_ds, [
                {'find_by_id', 1, fun(77002) -> Row end},
                {'resolve', 4, fun(_Id, _R, _N, _U) -> {ok, 1} end}
            ]},
            {report_action_log_ds, [
                {'create', 4, fun(_Id, _U, _R, _N) -> {error, db_down} end}
            ]},
            {elib_log, [{'internal_log', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            %% 审计写失败：处置结果不被吞（工单已 resolve → ok），但必须留痕
            ?assertEqual(ok, report_logic:admin_resolve(42, <<"message">>, 77002, 1, Note)),
            Logs = [Args || {_, {elib_log, internal_log, Args}, _} <- meck:history(elib_log)],
            ?assertEqual(1, length(Logs)),
            %% internal_log 实参为 list：[Level, Msg, Module, Line]
            [[error, Msg, report_logic, _Line]] = Logs,
            %% 可追溯四元钉形：事件 + 工单 ID + 操作管理员 + DB 原因
            ?assertEqual(["report_action_log_failed", 77002, 42, db_down], Msg),
            %% 不泄漏：备注正文/手机号/JWT/任何证据字段不进日志
            [
                ?assertEqual(false, term_contains(Msg, Canary))
             || Canary <- [Note, Phone, Jwt]
            ]
        end
    ).

resolve_type_mismatch_and_missing_fail_closed_test_() ->
    UserRow = #{<<"id">> => 77003, <<"target_type">> => <<"user">>},
    ?WITH_MECKS(
        [
            {report_ticket_ds, [
                {'find_by_id', 1, fun
                    (77003) -> UserRow;
                    (_) -> {error, not_found}
                end},
                {'resolve', 4, fun(_, _, _, _) -> erlang:error(should_not_be_called) end}
            ]},
            {report_action_log_ds, [
                {'create', 4, fun(_, _, _, _) -> erlang:error(should_not_be_called) end}
            ]}
        ],
        fun() ->
            %% 类型不匹配（跨对象越权处置）→ 拒绝，零 UPDATE 零审计
            ?assertEqual(
                {error, <<"举报类型不匹配"/utf8>>},
                report_logic:admin_resolve(42, <<"message">>, 77003, 1, <<>>)
            ),
            %% 不存在 → 拒绝
            ?assertEqual(
                {error, <<"举报记录不存在"/utf8>>},
                report_logic:admin_resolve(42, <<"message">>, 77004, 1, <<>>)
            ),
            %% 非法 result → 参数无效
            ?assertEqual(
                {error, <<"举报参数无效"/utf8>>},
                report_logic:admin_resolve(42, <<"message">>, 77003, 3, <<>>)
            ),
            %% 非法管理员 uid → 参数无效
            ?assertEqual(
                {error, <<"举报参数无效"/utf8>>},
                report_logic:admin_resolve(0, <<"message">>, 77003, 1, <<>>)
            ),
            ?assertEqual(0, meck:num_calls(report_ticket_ds, resolve, 4)),
            ?assertEqual(0, meck:num_calls(report_action_log_ds, create, 4))
        end
    ).

batch_resolve_partial_failure_no_audit_for_failed_test_() ->
    Rows = #{
        77010 => #{<<"id">> => 77010, <<"target_type">> => <<"message">>},
        77011 => #{<<"id">> => 77011, <<"target_type">> => <<"user">>}
    },
    ?WITH_MECKS(
        [
            {report_ticket_ds, [
                {'find_by_id', 1, fun(Id) -> maps:get(Id, Rows, {error, not_found}) end},
                {'resolve', 4, fun(_Id, _R, _N, _U) -> {ok, 1} end}
            ]},
            {report_action_log_ds, [
                {'create', 4, fun(_Id, _U, _R, _N) -> {ok, 88002} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{
                    <<"success_count">> => 1,
                    <<"failed_count">> => 2,
                    <<"failed_ids">> => [<<"77011">>, <<"77012">>]
                }},
                report_logic:admin_batch_resolve(42, <<"message">>, [77010, 77011, 77012], 1, <<>>)
            ),
            %% 只有成功工单产生审计行；失败项不产生假审计
            ?assertEqual(1, meck:num_calls(report_action_log_ds, create, 4))
        end
    ).

%% @private 深度扫描：Term 内任意层级是否出现与 Needle 相等的值。
term_contains(Term, Needle) when is_map(Term) ->
    term_contains(maps:to_list(Term), Needle);
term_contains(Term, Needle) when is_tuple(Term) ->
    term_contains(tuple_to_list(Term), Needle);
term_contains(Term, Needle) when is_list(Term) ->
    lists:any(fun(Element) -> term_contains(Element, Needle) end, Term);
term_contains(Term, Needle) ->
    Term =:= Needle.

%% ===================================================================
%% S-03 真库配方（隔离库 imboy_plan_s03_*@55427；参考 s02 erl -eval run/1 惯例）
%% 真实 ds/repo/SQL 链路（elib_pg 重定向到脚本自有连接）：
%%   LOCAL:01 注入（真实参数化 SQL 不执行注入串）+ 过大 payload（evidence
%%   500 字节边界；>int8 行 ID 明确失败且连接存活）+ IDOR 真表拒绝
%%   LOCAL:02 审计真实落库（report_action_log 必要字段）+ 类型不匹配零审计
%%   BEGIN/ROLLBACK 自清理，终态断言零残留
%% ===================================================================

-spec run(map()) -> ok.
run(ConnectOpts) when is_map(ConnectOpts) ->
    Required = [database, host, password, port, username],
    ?assertEqual(Required, lists:sort(maps:keys(maps:with(Required, ConnectOpts)))),
    ExpectedDb = unicode:characters_to_binary(maps:get(database, ConnectOpts)),
    ?assertEqual(
        match, re:run(ExpectedDb, <<"\\Aimboy_plan_s03_[a-z0-9_]+\\z">>, [{capture, none}])
    ),
    {ok, _} = application:ensure_all_started(epgsql),
    application:set_env(imboy, sql_driver, pgsql),
    {ok, Conn} = epgsql:connect(ConnectOpts),
    try
        run_conn(Conn, ExpectedDb)
    after
        epgsql:close(Conn)
    end.

run_conn(Conn, ExpectedDb) ->
    ?assertEqual(
        {ok, [#{<<"db">> => ExpectedDb}]},
        elib_pg:query(Conn, <<"SELECT current_database() AS db">>, [])
    ),
    {ok, [#{<<"v">> := 287}]} =
        elib_pg:query(Conn, <<"SELECT max(version) AS v FROM schema_migrations">>, []),
    Modules = [elib_pg, elib_tsid, agent_rate_limiter, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        %% 池面重定向到脚本自有连接：repo 层 SQL 全部真跑
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
        meck:expect(elib_tsid, generate, 1, fun(_) ->
            erlang:unique_integer([positive, monotonic])
        end),
        meck:expect(agent_rate_limiter, allow, 2, fun(_, _) -> allow end),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        {ok, Base} = report_counts(Conn),
        {ok, []} = elib_pg:query(Conn, <<"BEGIN">>, []),
        try
            s03_real_db_scenarios(Conn)
        after
            {ok, []} = elib_pg:query(Conn, <<"ROLLBACK">>, [])
        end,
        %% 终态零残留（与 BEGIN 前同基线）
        ?assertEqual({ok, Base}, report_counts(Conn))
    after
        meck:unload(Modules)
    end.

report_counts(Conn) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT (SELECT count(*) FROM public.report_ticket) AS tickets,"
            " (SELECT count(*) FROM public.report_action_log) AS action_logs"
        >>,
        []
    ).

s03_real_db_scenarios(Conn) ->
    W = s03_seed_world(Conn),
    R = maps:get(recipient, W),
    A = maps:get(author, W),
    Outsider = maps:get(outsider, W),
    Member = maps:get(member, W),
    Gid = maps:get(gid, W),
    ChannelId = maps:get(channel_id, W),
    ChMsgId = maps:get(ch_msg_id, W),
    C2CKey = <<"cm-", (integer_to_binary(maps:get(c2c_id, W)))/binary>>,
    C2GKey = <<"gm-", (integer_to_binary(maps:get(c2g_id, W)))/binary>>,
    %% ---- LOCAL:01 注入：真实参数化 SQL ----
    ?assertEqual(
        {error, <<"举报对象不存在或已被删除"/utf8>>},
        report_logic:create_message(
            R,
            <<"c2c">>,
            <<"cm-1'; DROP TABLE report_ticket; --">>,
            integer_to_binary(A),
            <<"spam">>,
            #{}
        )
    ),
    {ok, [#{<<"present">> := true}]} =
        elib_pg:query(
            Conn, <<"SELECT to_regclass('public.report_ticket') IS NOT NULL AS present">>, []
        ),
    SqlProbe = <<"); DELETE FROM report_ticket; --">>,
    QuoteProbe = <<"' OR '1'='1">>,
    {ok, #{<<"report_id">> := InjectReportId}} =
        report_logic:create_message(
            R,
            <<"c2c">>,
            C2CKey,
            integer_to_binary(A),
            <<"spam">>,
            #{<<"content_excerpt">> => SqlProbe, <<"client_msg_id">> => QuoteProbe}
        ),
    {ok, [InjectRow]} =
        elib_pg:query(
            Conn,
            <<"SELECT evidence FROM public.report_ticket WHERE id = $1">>,
            [InjectReportId]
        ),
    InjectEvidence = jsone:decode(maps:get(<<"evidence">>, InjectRow)),
    ?assertEqual(SqlProbe, maps:get(<<"content_excerpt">>, InjectEvidence)),
    ?assertEqual(QuoteProbe, maps:get(<<"client_msg_id">>, InjectEvidence)),
    {ok, [#{<<"mine">> := 1}]} =
        elib_pg:query(
            Conn,
            <<"SELECT count(*) AS mine FROM public.report_ticket WHERE reporter_uid = $1">>,
            [R]
        ),
    %% 管理端 keyword 注入串：ILIKE 参数化，正常返回不崩
    ?assertMatch(
        {ok, _},
        report_logic:admin_list(<<"message">>, -1, 1, 10, #{keyword => <<"%'; --">>})
    ),
    %% ---- LOCAL:01 过大 payload：evidence 500 字节边界 + >int8 明确失败 ----
    %% （作者 A 也是会话当事方，与 R 的举报不撞唯一键）
    {ok, #{<<"report_id">> := BigReportId}} =
        report_logic:create_message(
            A,
            <<"c2c">>,
            C2CKey,
            integer_to_binary(R),
            <<"other">>,
            #{<<"content_excerpt">> => binary:copy(<<"E">>, 10240)}
        ),
    {ok, [BigRow]} =
        elib_pg:query(
            Conn, <<"SELECT evidence FROM public.report_ticket WHERE id = $1">>, [BigReportId]
        ),
    BigEvidence = jsone:decode(maps:get(<<"evidence">>, BigRow)),
    ?assertEqual(500, byte_size(maps:get(<<"content_excerpt">>, BigEvidence))),
    HugeNum = <<"99999999999999999999999999">>,
    ?assertEqual(
        {error, <<"举报对象无效"/utf8>>},
        report_logic:create_message(
            R, <<"channel">>, HugeNum, integer_to_binary(ChannelId), <<"spam">>, #{}
        )
    ),
    ?assertEqual(
        {error, <<"举报对象无效"/utf8>>},
        report_logic:create(R, <<"user">>, HugeNum, <<"spam">>, <<>>)
    ),
    %% 明确失败后连接必须存活（未走 epgsql int8 编码崩溃路径）
    {ok, [#{<<"alive">> := 1}]} =
        elib_pg:query(Conn, <<"SELECT 1 AS alive">>, []),
    %% ---- LOCAL:01 越权：真表 IDOR ----
    ?assertEqual(
        {error, <<"无权举报该消息"/utf8>>},
        report_logic:create_message(
            Outsider, <<"c2c">>, C2CKey, integer_to_binary(A), <<"spam">>, #{}
        )
    ),
    ?assertEqual(
        {error, <<"无权举报该消息"/utf8>>},
        report_logic:create_message(
            Outsider, <<"c2g">>, C2GKey, integer_to_binary(Gid), <<"spam">>, #{}
        )
    ),
    ?assertEqual(
        {error, <<"无权举报该消息"/utf8>>},
        report_logic:create_message(
            Outsider,
            <<"channel">>,
            integer_to_binary(ChMsgId),
            integer_to_binary(ChannelId),
            <<"spam">>,
            #{}
        )
    ),
    %% ---- 重复举报幂等 ----
    ?assertEqual(
        {error, <<"您已举报过该对象"/utf8>>},
        report_logic:create_message(
            R, <<"c2c">>, C2CKey, integer_to_binary(A), <<"spam">>, #{}
        )
    ),
    %% ---- LOCAL:02 审计真实落库（必要字段可追溯）----
    {ok, #{<<"report_id">> := AuditReportId}} =
        report_logic:create_message(
            Member, <<"c2g">>, C2GKey, integer_to_binary(Gid), <<"fraud">>, #{}
        ),
    AdmUid = 429001,
    AuditNote = <<"real-db 处置备注"/utf8>>,
    ?assertEqual(
        ok, report_logic:admin_resolve(AdmUid, <<"message">>, AuditReportId, 2, AuditNote)
    ),
    {ok, [#{<<"n">> := 1, <<"op">> := AdmUid, <<"res">> := 2, <<"note">> := AuditNote}]} =
        elib_pg:query(
            Conn,
            <<
                "SELECT count(*) AS n, max(operator_uid) AS op, max(result) AS res,"
                " max(note) AS note FROM public.report_action_log WHERE report_id = $1"
            >>,
            [AuditReportId]
        ),
    %% 工单状态已被处置（handled_by 可追溯）
    {ok, [#{<<"handled_by">> := AdmUid, <<"status">> := 2}]} =
        elib_pg:query(
            Conn,
            <<"SELECT handled_by, status FROM public.report_ticket WHERE id = $1">>,
            [AuditReportId]
        ),
    %% ---- LOCAL:02 类型不匹配 fail-closed：零 UPDATE 零审计行 ----
    ?assertEqual(
        {error, <<"举报类型不匹配"/utf8>>},
        report_logic:admin_resolve(AdmUid, <<"user">>, AuditReportId, 1, <<>>)
    ),
    {ok, [#{<<"n2">> := 1}]} =
        elib_pg:query(
            Conn,
            <<"SELECT count(*) AS n2 FROM public.report_action_log WHERE report_id = $1">>,
            [AuditReportId]
        ),
    ok.

%% 真库 fixture：唯一键惯例 = 毫秒时间戳基址（同 report_message_chain_tests）
s03_base_id() ->
    erlang:system_time(millisecond) * 1000 + rand:uniform(899999).

s03_seed_world(Conn) ->
    Author = s03_base_id() + 1,
    Recipient = s03_base_id() + 2,
    Member = s03_base_id() + 3,
    Outsider = s03_base_id() + 4,
    [
        begin
            {ok, _} =
                elib_pg:query(
                    Conn,
                    <<
                        "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
                        " VALUES ($1, $2, 'x', '127.0.0.1', '')"
                    >>,
                    [U, <<"s03u-", (integer_to_binary(U))/binary>>]
                )
        end
     || U <- [Author, Recipient, Member, Outsider]
    ],
    C2CId = s03_base_id() + 10,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, e2ee, payload, conv_seq)"
                " VALUES ($1, $2, $3, $4, 'text', 'null'::jsonb, '{\"t\":\"hi\"}'::jsonb, 1)"
            >>,
            [C2CId, Author, Recipient, <<"cm-", (integer_to_binary(C2CId))/binary>>]
        ),
    Gid = s03_base_id() + 20,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.\"group\" (id, owner_uid, creator_uid, title)"
                " VALUES ($1, $2, $2, $3)"
            >>,
            [Gid, Author, <<"s03g-", (integer_to_binary(Gid))/binary>>]
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
                    [s03_base_id(), Gid, U]
                )
        end
     || U <- [Author, Member]
    ],
    C2GId = s03_base_id() + 12,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.msg_c2g (id, topic_id, from_id, to_id, msg_id, msg_type, e2ee, payload)"
                " VALUES ($1, 0, $2, $3, $4, 'text', 'null'::jsonb, '{\"t\":\"gh\"}'::jsonb)"
            >>,
            [C2GId, Author, Gid, <<"gm-", (integer_to_binary(C2GId))/binary>>]
        ),
    ChannelId = s03_base_id() + 21,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.channel (id, name, creator_uid, custom_id, visibility, access_type, join_policy, status)"
                " VALUES ($1, $2, $3, $4, 1, 0, 0, 1)"
            >>,
            [
                ChannelId,
                <<"s03c-", (integer_to_binary(ChannelId))/binary>>,
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
            [s03_base_id(), ChannelId, Member]
        ),
    ChMsgId = s03_base_id() + 13,
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO public.channel_message (id, channel_id, author_id, content, msg_type, status, revoked)"
                " VALUES ($1, $2, $3, $4, 'channel_text', 1, false)"
            >>,
            [ChMsgId, ChannelId, Author, <<"s03 channel content">>]
        ),
    #{
        author => Author,
        recipient => Recipient,
        member => Member,
        outsider => Outsider,
        c2c_id => C2CId,
        c2g_id => C2GId,
        gid => Gid,
        channel_id => ChannelId,
        ch_msg_id => ChMsgId
    }.
