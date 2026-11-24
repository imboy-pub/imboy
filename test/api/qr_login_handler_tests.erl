-module(qr_login_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% qr_login_handler 模块的 EUnit 测试（LEE-5：PG 唯一权威版）
%%%
%%% 【测试分类】本文件全部为 **MOCK 测试**（meck qr_login_session_repo /
%%% token_ds / user_device_repo / cowboy_req / elib_response；imboy_cache
%%% 以 throw 断言"绝不触碰"）。仅证明 handler 接线、响应合同、门禁/故障
%%% fail-closed 语义；**跨节点原子性与并发领取证明见真实 PG 套件**
%%%（qr_login_session_repo_tests / qr_login_claim_acceptance_tests /
%%% scripts/cache_v221_qr_entry_dcheck.erl）。
%%%
%%% 目标：覆盖 create/status/scan/confirm/cancel 五个接口及关键边界场景：
%%% - 默认 authority=pg；显式 cache 被覆盖并告警（绝不触碰 cache）；
%%% - PG 不可用/PG18 门禁不过/迁移缺失 → create/claim 一律 503 fail closed；
%%% - 领取成功响应丢失（客户端重试）→ 5203，绝不重发 token；
%%% - confirm 广播 confirmed_ready（无 token）；
%%% - REST 成功响应及业务拒绝保持；存储故障单独映射 503。
%%%===================================================================

%% ===================================================================
%% create
%% ===================================================================

create_stores_session_and_returns_tokens_test_() ->
    ?WITH_MECKS(
        common_mocks() ++ repo_ok_mocks(),
        fun() ->
            Body = jsone:encode(
                #{
                    <<"device_id">> => <<"web-device-1">>,
                    <<"device_name">> => <<"Chrome">>,
                    <<"platform">> => <<"web">>
                },
                [native_utf8]
            ),
            Req0 = req(<<"POST">>, Body, []),

            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, #{action => create}),
            {success, Payload} = response_result(Req1),
            ?assertEqual(60, maps:get(<<"expires_in">>, Payload)),
            SessTok = maps:get(<<"session_token">>, Payload),
            QRTok = maps:get(<<"qr_token">>, Payload),
            ?assertMatch(<<_/binary>>, SessTok),
            ?assertMatch(<<_/binary>>, QRTok),
            %% 反转断言（Rule 7）：qr_token 必须不可反解 session_token
            %% （截屏二维码不得推出 session_token 窃取 login_token）。
            ?assertEqual(nomatch, binary:match(QRTok, SessTok)),
            ?assertEqual(nomatch, binary:match(base64:decode(QRTok), base64:decode(SessTok))),
            %% 会话行入 PG（waiting/human；device 字段原样落库）
            ?assert(meck:called(qr_login_session_repo, create, ['_'])),
            [[CreateParams]] = call_args(qr_login_session_repo, create, 1),
            ?assertEqual(SessTok, maps:get(session_token, CreateParams)),
            ?assertEqual(QRTok, maps:get(qr_token, CreateParams)),
            ?assertEqual(<<"web-device-1">>, maps:get(device_id, CreateParams)),
            ?assertEqual(<<"human">>, maps:get(purpose, CreateParams))
        end
    ).

create_seat_purpose_stored_in_session_test_() ->
    ?WITH_MECKS(
        common_mocks() ++ repo_ok_mocks(),
        fun() ->
            Body = jsone:encode(#{<<"device_id">> => <<"seat-device">>, <<"purpose">> => <<"seat">>}),
            {stop, Req1, _State} = qr_login_handler:handle_request(req(<<"POST">>, Body, []), #{
                action => create
            }),
            ?assertMatch({success, _}, response_result(Req1)),
            [[CreateParams]] = call_args(qr_login_session_repo, create, 1),
            ?assertEqual(<<"seat">>, maps:get(purpose, CreateParams))
        end
    ).

create_rejects_invalid_identity_or_purpose_before_repo_write_test_() ->
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'create', 1, fun(_) -> error(unexpected_repo_write) end},
                    {'ensure_claim_compatible', 0, fun() -> error(unexpected_gate_call) end}
                ]}
            ],
        fun() ->
            lists:foreach(
                fun({Did, Purpose}) ->
                    Body = jsone:encode(#{
                        <<"device_id">> => Did, <<"purpose">> => Purpose
                    }),
                    {stop, Req1, _State} = qr_login_handler:handle_request(
                        req(<<"POST">>, Body, []), #{
                            action => create
                        }
                    ),
                    ?assertMatch({error, _, ?ERR_BAD_REQUEST}, response_result(Req1))
                end,
                [{<<"seat-device">>, P} || P <- [<<"admin">>, <<>>, null, 1]] ++
                    [{D, <<"seat">>} || D <- [<<>>, null, 1]]
            )
        end
    ).

%% LEE-5：PG18 门禁不过（如 PG17）→ create 明确拒绝 503，不崩溃不降级
create_rejected_when_pg_version_gate_fails_test_() ->
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'ensure_claim_compatible', 0, fun() ->
                        {error, {pg_version_unsupported, 170004}}
                    end},
                    {'create', 1, fun(_) -> error(unexpected_repo_write) end}
                ]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"device_id">> => <<"web-device-1">>}),
            {stop, Req1, _State} = qr_login_handler:handle_request(req(<<"POST">>, Body, []), #{
                action => create
            }),
            ?assertMatch({error, _, ?ERR_SERVICE_UNAVAILABLE}, response_result(Req1))
        end
    ).

%% LEE-5：PG 不可用/迁移缺失（repo create 报错）→ create 明确拒绝 503
create_rejected_when_pg_unavailable_or_migration_missing_test_() ->
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'ensure_claim_compatible', 0, fun() -> ok end},
                    {'create', 1, fun(_) -> {error, #{<<"severity">> => <<"ERROR">>}} end}
                ]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"device_id">> => <<"web-device-1">>}),
            {stop, Req1, _State} = qr_login_handler:handle_request(req(<<"POST">>, Body, []), #{
                action => create
            }),
            ?assertMatch({error, _, ?ERR_SERVICE_UNAVAILABLE}, response_result(Req1))
        end
    ).

%% ===================================================================
%% status（轮询领取入口）
%% ===================================================================

status_returns_confirmed_with_login_token_test_() ->
    SessionToken = <<"session-status-1">>,
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++ confirmed_session_mocks(SessionToken, Uid, <<"jwt-token-1">>),
        fun() ->
            Req0 = req(<<"GET">>, <<>>, [{<<"session_token">>, SessionToken}]),
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, #{action => status}),
            ?assertEqual(
                {success, #{
                    <<"status">> => <<"confirmed">>,
                    <<"token">> => <<"jwt-token-1">>,
                    <<"uid">> => Uid,
                    <<"uid_str">> => <<"111174215241304064">>
                }},
                response_result(Req1)
            ),
            %% token/uid 均来自同一次原子领取（claim_login_token），
            %% 不读会话、不信任任何信号
            ?assert(meck:called(qr_login_session_repo, claim_login_token, [SessionToken]))
        end
    ).

%% LEE-5：领取成功但响应丢失（客户端重试）→ 5203，绝不重发 token
status_claim_success_then_response_lost_retry_gets_5203_test_() ->
    SessionToken = <<"session-status-retry">>,
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                        #{<<"status">> => <<"confirmed">>, <<"scanned_by">> => Uid}
                    end},
                    {'claim_login_token', 1, fun(ST) when ST =:= SessionToken ->
                        %% 第一次领取成功，此后一律 not_claimable
                        Claimed = length(call_args(qr_login_session_repo, claim_login_token, 1)),
                        case Claimed >= 1 of
                            true -> {error, not_claimable};
                            false -> {ok, #{login_token => <<"jwt-once">>, scanned_by => Uid}}
                        end
                    end}
                ]}
            ],
        fun() ->
            Req0 = req(<<"GET">>, <<>>, [{<<"session_token">>, SessionToken}]),
            {stop, Req1, _} = qr_login_handler:handle_request(Req0, #{action => status}),
            ?assertMatch({success, #{<<"token">> := <<"jwt-once">>}}, response_result(Req1)),
            {stop, Req2, _} = qr_login_handler:handle_request(Req0, #{action => status}),
            ?assertEqual(
                {error, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED},
                response_result(Req2)
            )
        end
    ).

%% 非 confirmed 状态（scanned）不触发领取
status_does_not_claim_when_not_confirmed_test_() ->
    SessionToken = <<"session-status-scanned">>,
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                        #{<<"status">> => <<"scanned">>}
                    end},
                    {'claim_login_token', 1, fun(_) -> error(unexpected_claim) end}
                ]}
            ],
        fun() ->
            Req0 = req(<<"GET">>, <<>>, [{<<"session_token">>, SessionToken}]),
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, #{action => status}),
            ?assertEqual({success, #{<<"status">> => <<"scanned">>}}, response_result(Req1))
        end
    ).

%% 会话确实不存在返回 404；PG 查询失败返回 503，两者均不领取/返回凭据。
%% A database failure must not masquerade as a missing session.
status_not_found_fail_closed_test_() ->
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_session_token', 1, fun
                        (<<"session-gone">>) -> undefined;
                        (<<"session-pg-err">>) -> {error, closed}
                    end},
                    {'claim_login_token', 1, fun(_) -> error(unexpected_claim) end}
                ]}
            ],
        fun() ->
            lists:foreach(
                fun({ST, ExpectedCode}) ->
                    Req0 = req(<<"GET">>, <<>>, [{<<"session_token">>, ST}]),
                    {stop, Req1, _} = qr_login_handler:handle_request(Req0, #{action => status}),
                    ?assertMatch({error, _, ExpectedCode}, response_result(Req1))
                end,
                [
                    {<<"session-gone">>, ?ERR_NOT_FOUND},
                    {<<"session-pg-err">>, ?ERR_SERVICE_UNAVAILABLE}
                ]
            )
        end
    ).
%% LEE-5：claim 门禁失败（PG18 以下/PG 故障）→ 503 明确拒绝，绝不降级
status_claim_gate_rejected_503_test_() ->
    SessionToken = <<"session-gate">>,
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                        #{<<"status">> => <<"confirmed">>}
                    end},
                    {'claim_login_token', 1, fun(_) ->
                        {error, {claim_gate, {pg_version_unsupported, 170004}}}
                    end}
                ]}
            ],
        fun() ->
            Req0 = req(<<"GET">>, <<>>, [{<<"session_token">>, SessionToken}]),
            {stop, Req1, _} = qr_login_handler:handle_request(Req0, #{action => status}),
            ?assertMatch({error, _, ?ERR_SERVICE_UNAVAILABLE}, response_result(Req1))
        end
    ).

%% ===================================================================
%% scan
%% ===================================================================

scan_updates_status_to_scanned_test_() ->
    SessionToken = <<"session-scan-1">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++ scan_ok_mocks(SessionToken, QRToken, Uid),
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => scan, current_uid => Uid},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            %% 响应携带设备信息（手机端 UI 展示"哪台设备要登录"）
            ?assertEqual(
                {success, #{
                    <<"status">> => <<"scanned">>,
                    <<"device_name">> => <<"Chrome 120"/utf8>>,
                    <<"platform">> => <<"web">>
                }},
                response_result(Req1)
            ),
            ?assert(meck:called(qr_login_session_repo, find_by_qr_token, [QRToken])),
            ?assert(meck:called(qr_login_session_repo, transition_scan, [SessionToken, Uid]))
        end
    ).

scan_returns_expired_when_session_outdated_test_() ->
    SessionToken = <<"session-expired-1">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_qr_token', 1, fun(Token) when Token =:= QRToken ->
                        #{<<"session_token">> => SessionToken}
                    end},
                    {'transition_scan', 2, fun(_, _) -> {error, not_transitioned} end},
                    {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken -> undefined end}
                ]},
                {qr_login_event_ds, [{'notify', 2, fun(_, _) -> {ok, 0} end}]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => scan, current_uid => Uid},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            ?assertEqual(
                {error, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED},
                response_result(Req1)
            )
        end
    ).

scan_returns_invalid_qr_token_for_illegal_token_test_() ->
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_qr_token', 1, fun(_) -> undefined end}
                ]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => <<"not_a_valid_base64_token">>}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => scan, current_uid => 1001},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            ?assertEqual(
                {error, <<"无效的二维码"/utf8>>, ?ERR_INVALID_QR_TOKEN},
                response_result(Req1)
            )
        end
    ).

scan_requires_login_test_() ->
    ?WITH_MECKS(
        common_mocks(),
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => <<"any-token">>}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, #{
                action => scan, current_uid => 0
            }),
            ?assertMatch({error, _, ?ERR_UNAUTHORIZED}, response_result(Req1))
        end
    ).

%% ===================================================================
%% PR-2β: handle_scan/handle_confirm 成功路径必须广播事件
%% ===================================================================

%% scan 成功后必须以 scanned 事件 broadcast 到 SessionToken 对应 group
scan_broadcasts_scanned_event_on_success_test_() ->
    SessionToken = <<"session-broadcast-scan">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++ scan_ok_mocks(SessionToken, QRToken, Uid),
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => scan, current_uid => Uid},
            {stop, _Req1, _State} = qr_login_handler:handle_request(Req0, State),
            %% meck:history 跨进程验证 notify 事件形状（scanned，无凭据）
            ?assert(
                meck:called(qr_login_event_ds, notify, [SessionToken, '_']),
                "qr_login_event_ds:notify/2 未被 scan 调用"
            ),
            Notifies = [
                Args
             || {_Pid, {qr_login_event_ds, notify, Args}, _Ret} <-
                    meck:history(qr_login_event_ds),
                length(Args) =:= 2
            ],
            ?assertMatch([[SessionToken, #{<<"status">> := <<"scanned">>}]], Notifies)
        end
    ).

%% scan 失败路径（会话已过期）禁止 broadcast
scan_does_not_broadcast_when_expired_test_() ->
    SessionToken = <<"session-no-broadcast-expired">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_qr_token', 1, fun(Token) when Token =:= QRToken ->
                        #{<<"session_token">> => SessionToken}
                    end},
                    {'transition_scan', 2, fun(_, _) -> {error, not_transitioned} end},
                    {'find_by_session_token', 1, fun(_) -> undefined end}
                ]},
                {qr_login_event_ds, [{'notify', 2, fun(_, _) -> {ok, 0} end}]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => scan, current_uid => Uid},
            {stop, _Req1, _State} = qr_login_handler:handle_request(Req0, State),
            ?assertNot(
                meck:called(qr_login_event_ds, notify, ['_', '_']),
                "expired 路径不应调 notify/2"
            )
        end
    ).

%% LEE-5：confirm 成功广播 confirmed_ready——**无 token 信号**（token 一律
%% 经 SSE 服务端代领或 status 原子领取）
confirm_broadcasts_confirmed_ready_event_without_token_test_() ->
    SessionToken = <<"session-broadcast-confirm">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++ confirm_ok_mocks(SessionToken, QRToken, Uid, <<"login-token-x">>),
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => confirm, current_uid => Uid},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            ?assertEqual({success, #{<<"status">> => <<"confirmed">>}}, response_result(Req1)),
            ?assert(
                meck:called(qr_login_event_ds, notify, [SessionToken, '_']),
                "qr_login_event_ds:notify/2 未被 confirm 调用"
            ),
            Notifies = [
                Args
             || {_Pid, {qr_login_event_ds, notify, Args}, _Ret} <-
                    meck:history(qr_login_event_ds),
                length(Args) =:= 2
            ],
            [[K, E] | _] = Notifies,
            ?assertEqual(SessionToken, K),
            %% confirmed_ready：仅通知"有凭据可领取"，不含 token/uid 任何凭据
            ?assertEqual(<<"confirmed_ready">>, maps:get(<<"status">>, E)),
            ?assertEqual(undefined, maps:get(<<"token">>, E, undefined)),
            ?assertEqual(undefined, maps:get(<<"uid">>, E, undefined)),
            ?assertEqual(1, maps:size(E))
        end
    ).

%% confirm 失败（跨用户）禁止 broadcast
confirm_does_not_broadcast_when_forbidden_test_() ->
    SessionToken = <<"session-no-broadcast-forbidden">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_qr_token', 1, fun(Token) when Token =:= QRToken ->
                        #{<<"session_token">> => SessionToken}
                    end},
                    {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                        #{<<"status">> => <<"scanned">>, <<"scanned_by">> => 2002}
                    end}
                ]},
                {qr_login_event_ds, [{'notify', 2, fun(_, _) -> {ok, 0} end}]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => confirm, current_uid => 1001},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            ?assertEqual({error, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN}, response_result(Req1)),
            ?assertNot(
                meck:called(qr_login_event_ds, notify, ['_', '_']),
                "跨用户 forbidden 路径不应调 notify/2"
            )
        end
    ).

%% ===================================================================
%% confirm
%% ===================================================================

confirm_marks_session_confirmed_and_records_device_test_() ->
    confirm_purpose_test(<<"human">>, success).

confirm_uses_stored_seat_purpose_ignoring_request_override_test_() ->
    confirm_purpose_test(<<"seat">>, success).

confirm_web_seat_requires_binding_before_confirm_test_() ->
    [confirm_purpose_test(<<"seat">>, Outcome) || Outcome <- [invalid_row, bad_epoch, mint_failed]].

confirm_purpose_test(Purpose, Outcome) ->
    SessionToken = <<"session-confirm-1">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    EncryptFun =
        case Purpose of
            <<"seat">> -> encrypt_web_seat_token;
            <<"human">> -> encrypt_web_token
        end,
    ?WITH_MOCKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_qr_token', 1, fun(Token) when Token =:= QRToken ->
                        #{<<"session_token">> => SessionToken}
                    end},
                    {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                        #{
                            <<"status">> => <<"scanned">>,
                            <<"scanned_by">> => Uid,
                            <<"device_id">> => <<"web-device-1">>,
                            <<"device_name">> => <<"Safari">>,
                            <<"platform">> => <<"web">>,
                            <<"purpose">> => Purpose
                        }
                    end},
                    {'transition_confirm', 3, fun(ST, U, <<"login-token-confirm">>) when
                        ST =:= SessionToken, U =:= Uid, Outcome =:= success
                    ->
                        ?assert(
                            meck:called(token_ds, EncryptFun, [Uid, <<"web-device-1">>, 7, 9001])
                        ),
                        {ok, #{<<"status">> => <<"confirmed">>}}
                    end}
                ]},
                {user_ds, [
                    {'find_by_id', 2, fun(U, _Fields) when U =:= Uid ->
                        #{<<"status">> => 1, <<"account_type">> => 0}
                    end}
                ]},
                {user_device_ds, [
                    {'register_web', 4, fun(_Now, U, <<"web-device-1">>, PostVals) when U =:= Uid ->
                        ?assertEqual(<<"Safari">>, maps:get(<<"dname">>, PostVals)),
                        ?assertEqual(<<"web">>, maps:get(<<"cos">>, PostVals)),
                        ?assertEqual(<<"qr_scan">>, maps:get(<<"login_type">>, PostVals)),
                        case Outcome of
                            invalid_row -> {ok, 0};
                            _ -> {ok, 9001}
                        end
                    end}
                ]},
                {token_ds, [
                    {EncryptFun, 4, fun(U, <<"web-device-1">>, 7, 9001) when U =:= Uid ->
                        ?assert(
                            meck:called(user_device_ds, register_web, [
                                '_', Uid, <<"web-device-1">>, '_'
                            ])
                        ),
                        case Outcome of
                            mint_failed -> error(mint_failed);
                            _ -> <<"login-token-confirm">>
                        end
                    end}
                ]},
                {qr_login_event_ds, [
                    {'notify', 2, fun(ST, Event) when ST =:= SessionToken ->
                        ?assert(
                            meck:called(
                                qr_login_session_repo,
                                transition_confirm,
                                [SessionToken, Uid, <<"login-token-confirm">>]
                            )
                        ),
                        ?assertEqual(qr_login_event_ds:event(confirmed_ready, undefined), Event),
                        {ok, 1}
                    end}
                ]}
            ],
        fun() ->
            meck:expect(elib_response, error_with_status, fun(Req, Http, _Message, Code) ->
                Req#{response_result => {http_error, Http, Code}}
            end),
            case Outcome of
                bad_epoch ->
                    meck:expect(auth_session_ds, current_epoch, fun(U) when U =:= Uid -> {ok, 0} end);
                _ ->
                    ok
            end,
            Body = jsone:encode(
                #{
                    <<"qr_token">> => QRToken,
                    %% 请求体的 purpose 覆盖必须被忽略（以存储值为准）
                    <<"purpose">> =>
                        case Purpose of
                            <<"seat">> -> <<"human">>;
                            _ -> <<"seat">>
                        end
                },
                [native_utf8]
            ),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => confirm, current_uid => Uid},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            case Outcome of
                success ->
                    ?assertEqual(
                        {success, #{<<"status">> => <<"confirmed">>}}, response_result(Req1)
                    ),
                    ?assertEqual(
                        1,
                        meck:num_calls(qr_login_session_repo, transition_confirm, ['_', '_', '_'])
                    ),
                    ?assertEqual(1, meck:num_calls(qr_login_event_ds, notify, ['_', '_']));
                _ ->
                    ?assertEqual({http_error, 503, 503}, response_result(Req1)),
                    ?assertNot(
                        meck:called(qr_login_session_repo, transition_confirm, ['_', '_', '_'])
                    ),
                    ?assertNot(meck:called(qr_login_event_ds, notify, ['_', '_']))
            end,
            ?assertNot(meck:called(qr_login_session_repo, claim_login_token, ['_']))
        end
    ).

confirm_returns_already_used_for_duplicate_confirm_test_() ->
    SessionToken = <<"session-dup-confirm-1">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_qr_token', 1, fun(Token) when Token =:= QRToken ->
                        #{<<"session_token">> => SessionToken}
                    end},
                    {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                        #{<<"status">> => <<"confirmed">>, <<"scanned_by">> => 1001}
                    end},
                    {'transition_confirm', 3, fun(_, _, _) -> error(unexpected_confirm) end}
                ]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => confirm, current_uid => 1001},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            ?assertEqual(
                {error, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED},
                response_result(Req1)
            )
        end
    ).

confirm_returns_not_scanned_when_waiting_test_() ->
    SessionToken = <<"session-confirm-waiting">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'find_by_qr_token', 1, fun(Token) when Token =:= QRToken ->
                        #{<<"session_token">> => SessionToken}
                    end},
                    {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                        #{<<"status">> => <<"waiting">>}
                    end}
                ]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => confirm, current_uid => 1001},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            ?assertEqual(
                {error, <<"二维码尚未扫码"/utf8>>, ?ERR_QR_LOGIN_NOT_SCANNED},
                response_result(Req1)
            )
        end
    ).

confirm_concurrent_cas_rejected_test_() ->
    SessionToken = <<"session-confirm-race">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++ confirm_ok_mocks(SessionToken, QRToken, Uid, <<"tok">>),
        fun() ->
            %% 并发 confirm：transition_confirm CAS 失败 → 兼容 5203
            meck:expect(
                qr_login_session_repo,
                transition_confirm,
                3,
                fun(_, _, _) -> {error, not_transitioned} end
            ),
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            State = #{action => confirm, current_uid => Uid},
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
            ?assertEqual(
                {error, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED},
                response_result(Req1)
            )
        end
    ).

%% ===================================================================
%% cancel
%% ===================================================================

cancel_updates_status_to_cancelled_test_() ->
    SessionToken = <<"session-cancel-1">>,
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'transition_cancel', 1, fun(ST) when ST =:= SessionToken -> ok end}
                ]}
            ],
        fun() ->
            Body = jsone:encode(#{<<"session_token">> => SessionToken}, [native_utf8]),
            Req0 = req(<<"POST">>, Body, []),
            {stop, Req1, _State} = qr_login_handler:handle_request(Req0, #{action => cancel}),
            ?assertEqual({success, #{<<"status">> => <<"cancelled">>}}, response_result(Req1))
        end
    ).

%% 已确认后不可取消（G-QR：结果唯一）；不存在/过期幂等成功
cancel_after_confirm_rejected_but_missing_idempotent_test_() ->
    ?WITH_MECKS(
        common_mocks() ++
            [
                {qr_login_session_repo, [
                    {'transition_cancel', 1, fun
                        (<<"session-cancel-confirmed">>) -> {error, not_transitioned};
                        (<<"session-cancel-gone">>) -> {error, not_transitioned}
                    end},
                    {'find_by_session_token', 1, fun
                        (<<"session-cancel-confirmed">>) ->
                            #{<<"status">> => <<"confirmed">>};
                        (<<"session-cancel-gone">>) ->
                            undefined
                    end}
                ]}
            ],
        fun() ->
            Body1 = jsone:encode(#{<<"session_token">> => <<"session-cancel-confirmed">>}, [
                native_utf8
            ]),
            {stop, Req1, _} = qr_login_handler:handle_request(
                req(<<"POST">>, Body1, []), #{action => cancel}
            ),
            ?assertEqual(
                {error, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED},
                response_result(Req1)
            ),
            Body2 = jsone:encode(#{<<"session_token">> => <<"session-cancel-gone">>}, [native_utf8]),
            {stop, Req2, _} = qr_login_handler:handle_request(
                req(<<"POST">>, Body2, []), #{action => cancel}
            ),
            ?assertEqual({success, #{<<"status">> => <<"cancelled">>}}, response_result(Req2))
        end
    ).

%% ===================================================================
%% LEE-5 权威语义：默认 pg；显式 cache 被覆盖并告警；绝不触碰 imboy_cache
%% ===================================================================

%% 未设置 qr_login_authority（默认）→ 直接走 PG（repo 被调，cache 零调用）
default_authority_is_pg_test_() ->
    SessionToken = <<"session-default-pg">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++ no_cache_mocks() ++ scan_ok_mocks(SessionToken, QRToken, Uid),
        fun() ->
            application:unset_env(imboy, qr_login_authority),
            Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
            {stop, Req1, _} = qr_login_handler:handle_request(
                req(<<"POST">>, Body, []), #{action => scan, current_uid => Uid}
            ),
            ?assertMatch({success, _}, response_result(Req1)),
            ?assert(meck:called(qr_login_session_repo, transition_scan, [SessionToken, Uid]))
        end
    ).

%% 显式 qr_login_authority=cache → 告警并按 pg 处理（cache 路径已删除；
%% imboy_cache 任何调用都会触发 throw）
explicit_cache_overridden_with_warning_test_() ->
    SessionToken = <<"session-explicit-cache">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++ no_cache_mocks() ++ scan_ok_mocks(SessionToken, QRToken, Uid),
        fun() ->
            persistent_term:erase({imboy, qr_login_authority_warned}),
            application:set_env(imboy, qr_login_authority, cache),
            try
                Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
                {stop, Req1, _} = qr_login_handler:handle_request(
                    req(<<"POST">>, Body, []), #{action => scan, current_uid => Uid}
                ),
                %% 仍走 PG 权威（服务不中断），绝不触碰 cache
                ?assertMatch({success, _}, response_result(Req1)),
                ?assert(meck:called(qr_login_session_repo, transition_scan, [SessionToken, Uid])),
                %% 节点级一次告警已置位
                ?assertEqual(true, persistent_term:get({imboy, qr_login_authority_warned}, false))
            after
                application:unset_env(imboy, qr_login_authority),
                persistent_term:erase({imboy, qr_login_authority_warned})
            end
        end
    ).

%% postgres 别名同样合法
postgres_alias_accepted_test_() ->
    SessionToken = <<"session-postgres-alias">>,
    QRToken = make_qr_token(SessionToken),
    Uid = 111174215241304064,
    ?WITH_MECKS(
        common_mocks() ++ no_cache_mocks() ++ scan_ok_mocks(SessionToken, QRToken, Uid),
        fun() ->
            persistent_term:erase({imboy, qr_login_authority_warned}),
            application:set_env(imboy, qr_login_authority, postgres),
            try
                Body = jsone:encode(#{<<"qr_token">> => QRToken}, [native_utf8]),
                {stop, Req1, _} = qr_login_handler:handle_request(
                    req(<<"POST">>, Body, []), #{action => scan, current_uid => Uid}
                ),
                ?assertMatch({success, _}, response_result(Req1)),
                %% postgres 别名不触发告警
                ?assertEqual(false, persistent_term:get({imboy, qr_login_authority_warned}, false))
            after
                application:unset_env(imboy, qr_login_authority),
                persistent_term:erase({imboy, qr_login_authority_warned})
            end
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

common_mocks() ->
    [
        {cowboy_req, [
            {'method', 1, fun(Req) -> maps:get(method, Req, <<"GET">>) end},
            {'read_body', 1, fun(Req) -> {ok, maps:get(body, Req, <<>>), Req} end},
            {'parse_qs', 1, fun(Req) -> maps:get(qs, Req, []) end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) -> Req#{response_result => {success, Payload}} end},
            {'error', 3, fun(Req, Msg, Code) -> Req#{response_result => {error, Msg, Code}} end}
        ]},
        %% LEE-5+epoch 门：pg web confirm 签发前读 auth_session epoch——
        %% mock 固定 {ok, 7}，勿依赖宿主 PG 的 user_auth_epoch 行。
        {auth_session_ds, [{'current_epoch', 1, fun(_) -> {ok, 7} end}]}
    ].

%% LEE-5 反证：QR 全链路绝不触碰 imboy_cache（任何调用即 throw 测试失败）
no_cache_mocks() ->
    [
        {imboy_cache, [
            {'set', 3, fun(_, _, _) -> error(unexpected_cache_set) end},
            {'set', 4, fun(_, _, _, _) -> error(unexpected_cache_set) end},
            {'get', 1, fun(_) -> error(unexpected_cache_get) end},
            {'get_wait', 1, fun(_) -> error(unexpected_cache_get) end},
            {'delete', 1, fun(_) -> error(unexpected_cache_delete) end}
        ]}
    ].

%% create 成功路径：门禁通过 + 会话行入库
repo_ok_mocks() ->
    [
        {qr_login_session_repo, [
            {'ensure_claim_compatible', 0, fun() -> ok end},
            {'create', 1, fun(_) -> ok end}
        ]}
    ].

%% status confirmed：claim 一次成功
confirmed_session_mocks(SessionToken, Uid, Token) ->
    [
        {qr_login_session_repo, [
            {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                #{<<"status">> => <<"confirmed">>, <<"scanned_by">> => Uid}
            end},
            {'claim_login_token', 1, fun(ST) when ST =:= SessionToken ->
                {ok, #{login_token => Token, scanned_by => Uid}}
            end}
        ]}
    ].

%% scan 成功：qr 反查 + waiting→scanned CAS + scanned 广播
scan_ok_mocks(SessionToken, QRToken, Uid) ->
    [
        {qr_login_session_repo, [
            {'find_by_qr_token', 1, fun(Token) when Token =:= QRToken ->
                #{<<"session_token">> => SessionToken}
            end},
            {'transition_scan', 2, fun(ST, U) when ST =:= SessionToken, U =:= Uid ->
                {ok, #{
                    <<"session_token">> => ST,
                    <<"status">> => <<"scanned">>,
                    <<"device_name">> => <<"Chrome 120"/utf8>>,
                    <<"platform">> => <<"web">>
                }}
            end}
        ]},
        {qr_login_event_ds, [{'notify', 2, fun(_, _) -> {ok, 1} end}]}
    ].

%% confirm 成功：scanned 会话 + token 签发 + confirmed_ready 广播
confirm_ok_mocks(SessionToken, QRToken, Uid, Token) ->
    [
        {qr_login_session_repo, [
            {'find_by_qr_token', 1, fun(T) when T =:= QRToken ->
                #{<<"session_token">> => SessionToken}
            end},
            {'find_by_session_token', 1, fun(ST) when ST =:= SessionToken ->
                #{
                    <<"status">> => <<"scanned">>,
                    <<"scanned_by">> => Uid,
                    <<"device_id">> => <<"web-device-1">>,
                    <<"device_name">> => <<"Safari">>,
                    <<"platform">> => <<"web">>,
                    <<"purpose">> => <<"human">>
                }
            end},
            {'transition_confirm', 3, fun(_, _, _) -> {ok, #{<<"status">> => <<"confirmed">>}} end}
        ]},
        {token_ds, [
            {'encrypt_token', 2, fun(_, _) -> Token end},
            {'encrypt_web_token', 4, fun(_, _, _, _) -> Token end},
            {'encrypt_seat_token', 2, fun(_, _) -> Token end}
        ]},
        %% LEE-5+设备行 fail-closed：pg confirm 校验 log_device_login 返回值——
        %% 链首关 user_ds:find_by_id（真查 PG 无行→invalid_account→503）与
        %% 链内 register_web 均须确定性 mock。
        {user_ds, [
            {'find_by_id', 2, fun(U, _Fields) when U =:= Uid ->
                #{<<"status">> => 1, <<"account_type">> => 0}
            end}
        ]},
        {user_device_ds, [
            {'register_web', 4, fun(_, _, _, _) -> {ok, 9001} end}
        ]},
        {user_device_repo, [
            {'save', 4, fun(_, _, _, _) -> ok end}
        ]},
        {qr_login_event_ds, [{'notify', 2, fun(_, _) -> {ok, 1} end}]}
    ].

req(Method, Body, Qs) ->
    #{
        method => Method,
        body => Body,
        qs => Qs
    }.

response_result(Req) ->
    maps:get(response_result, Req).

%% meck 历史中提取某函数的参数列表（跨进程）
call_args(Mod, Fun, Arity) ->
    [
        Args
     || {_Pid, {M, F, Args}, _Ret} <- meck:history(Mod),
        M =:= Mod,
        F =:= Fun,
        length(Args) =:= Arity
    ].

%% 测试用 qr_token：不透明串（真实实现为随机值入 PG UNIQUE 反查，
%% 不可由 session_token 推导）。
make_qr_token(SessionToken) ->
    <<"qr-opaque-", SessionToken/binary>>.
