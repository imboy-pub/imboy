-module(report_logic_message_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

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
