-module(qr_login_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% qr_login_handler 模块的 EUnit 测试
%%%
%%% 目标：覆盖 create/status/scan/confirm/cancel 五个接口及关键边界场景
%%%===================================================================

create_stores_session_and_returns_tokens_test_() ->
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'set', 3, fun({qr_login, SessionToken}, SessionData, 60) ->
                ?assertEqual(SessionToken, maps:get(<<"session_token">>, SessionData)),
                ?assertEqual(<<"waiting">>, maps:get(<<"status">>, SessionData)),
                ?assertMatch(<<_/binary>>, maps:get(<<"qr_token">>, SessionData)),
                ok
            end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{
            <<"device_id">> => <<"web-device-1">>,
            <<"device_name">> => <<"Chrome">>,
            <<"platform">> => <<"web">>
        }),
        Req0 = req(<<"POST">>, Body, []),

        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, #{action => create}),
        {success, Payload} = response_result(Req1),
        ?assertEqual(60, maps:get(<<"expires_in">>, Payload)),
        ?assertMatch(<<_/binary>>, maps:get(<<"session_token">>, Payload)),
        ?assertMatch(<<_/binary>>, maps:get(<<"qr_token">>, Payload))
    end).

status_returns_confirmed_with_login_token_test_() ->
    SessionToken = <<"session-status-1">>,
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"confirmed">>,
                    <<"login_token">> => <<"jwt-token-1">>
                }}
            end}
        ]}
    ], fun() ->
        Req0 = req(<<"GET">>, <<>>, [{<<"session_token">>, SessionToken}]),
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, #{action => status}),
        ?assertEqual(
            {success, #{<<"status">> => <<"confirmed">>, <<"token">> => <<"jwt-token-1">>}},
            response_result(Req1))
    end).

scan_updates_status_to_scanned_test_() ->
    SessionToken = <<"session-scan-1">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"waiting">>,
                    <<"expires_at">> => future_ms(),
                    <<"device_name">> => <<"Chrome 120"/utf8>>,
                    <<"platform">> => <<"web">>
                }}
            end},
            {'set', 3, fun({qr_login, KeyToken}, SessionData, 60) when KeyToken =:= SessionToken ->
                ?assertEqual(<<"scanned">>, maps:get(<<"status">>, SessionData)),
                ?assertEqual(1001, maps:get(<<"scanned_by">>, SessionData)),
                ok
            end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => scan, current_uid => 1001},
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
        %% 响应携带设备信息（slice-5b：手机端 UI 展示"哪台设备要登录"）。
        ?assertEqual(
            {success, #{<<"status">> => <<"scanned">>,
                        <<"device_name">> => <<"Chrome 120"/utf8>>,
                        <<"platform">> => <<"web">>}},
            response_result(Req1))
    end).

scan_returns_empty_device_fields_when_session_lacks_them_test_() ->
    %% 老 session（在补丁前生成）可能缺 device_name / platform；
    %% 此时响应字段以空 binary 兜底，前端解析层视为 deviceInfo=null。
    SessionToken = <<"session-scan-legacy">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"waiting">>,
                    <<"expires_at">> => future_ms()
                }}
            end},
            {'set', 3, fun(_Key, _Data, 60) -> ok end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => scan, current_uid => 1001},
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assertEqual(
            {success, #{<<"status">> => <<"scanned">>,
                        <<"device_name">> => <<>>,
                        <<"platform">> => <<>>}},
            response_result(Req1))
    end).

confirm_marks_session_confirmed_and_records_device_test_() ->
    SessionToken = <<"session-confirm-1">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"scanned">>,
                    <<"scanned_by">> => 1001,
                    <<"expires_at">> => future_ms(),
                    <<"device_id">> => <<"web-device-1">>,
                    <<"device_name">> => <<"Safari">>,
                    <<"platform">> => <<"web">>
                }}
            end},
            {'set', 3, fun({qr_login, KeyToken}, SessionData, 60) when KeyToken =:= SessionToken ->
                ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, SessionData)),
                ?assertEqual(<<"login-token-1001">>, maps:get(<<"login_token">>, SessionData)),
                ok
            end}
        ]},
        {token_ds, [
            {'encrypt_token', 1, fun(1001) -> <<"login-token-1001">> end}
        ]},
        {user_device_repo, [
            {'save', 4, fun(_Now, 1001, <<"web-device-1">>, PostVals) ->
                ?assertEqual(<<"Safari">>, maps:get(<<"device_name">>, PostVals)),
                ?assertEqual(<<"web">>, maps:get(<<"platform">>, PostVals)),
                ?assertEqual(<<"qr_scan">>, maps:get(<<"login_type">>, PostVals)),
                ok
            end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => confirm, current_uid => 1001},
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assertEqual({success, #{<<"status">> => <<"confirmed">>}}, response_result(Req1))
    end).

cancel_updates_status_to_cancelled_test_() ->
    SessionToken = <<"session-cancel-1">>,
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"waiting">>,
                    <<"expires_at">> => future_ms()
                }}
            end},
            {'set', 3, fun({qr_login, KeyToken}, SessionData, 60) when KeyToken =:= SessionToken ->
                ?assertEqual(<<"cancelled">>, maps:get(<<"status">>, SessionData)),
                ok
            end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"session_token">> => SessionToken}),
        Req0 = req(<<"POST">>, Body, []),
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, #{action => cancel}),
        ?assertEqual({success, #{<<"status">> => <<"cancelled">>}}, response_result(Req1))
    end).

scan_returns_expired_when_session_outdated_test_() ->
    SessionToken = <<"session-expired-1">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"waiting">>,
                    <<"expires_at">> => past_ms()
                }}
            end},
            {'delete', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken -> ok end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => scan, current_uid => 1001},
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assertEqual(
            {error, <<"二维码已过期，请刷新"/utf8>>, ?ERR_QR_LOGIN_EXPIRED},
            response_result(Req1))
    end).

confirm_returns_already_used_for_duplicate_confirm_test_() ->
    SessionToken = <<"session-dup-confirm-1">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"confirmed">>,
                    <<"scanned_by">> => 1001,
                    <<"expires_at">> => future_ms()
                }}
            end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => confirm, current_uid => 1001},
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assertEqual(
            {error, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED},
            response_result(Req1))
    end).

confirm_returns_forbidden_for_cross_user_test_() ->
    SessionToken = <<"session-cross-user-1">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"scanned">>,
                    <<"scanned_by">> => 2002,
                    <<"expires_at">> => future_ms()
                }}
            end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => confirm, current_uid => 1001},
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assertEqual({error, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN}, response_result(Req1))
    end).

scan_returns_invalid_qr_token_for_illegal_token_test_() ->
    ?WITH_MECKS(common_mocks(), fun() ->
        Body = jsx:encode(#{<<"qr_token">> => <<"not_a_valid_base64_token">>}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => scan, current_uid => 1001},
        {stop, Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assertEqual(
            {error, <<"无效的二维码"/utf8>>, ?ERR_INVALID_QR_TOKEN},
            response_result(Req1))
    end).

%% ===================================================================
%% PR-2β: handle_scan/handle_confirm 成功路径必须广播事件
%% ===================================================================

%% scan 成功后必须以 scanned 事件 broadcast 到 SessionToken 对应 group
scan_broadcasts_scanned_event_on_success_test_() ->
    SessionToken = <<"session-broadcast-scan">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"waiting">>,
                    <<"expires_at">> => future_ms(),
                    <<"device_name">> => <<"Chrome">>,
                    <<"platform">> => <<"web">>
                }}
            end},
            {'set', 3, fun(_Key, _Data, 60) -> ok end}
        ]},
        {qr_login_event_ds, [
            {'event', 2, fun(scanned, undefined) ->
                #{<<"status">> => <<"scanned">>}
            end},
            {'notify', 2, fun(_KeyToken, _Event) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => scan, current_uid => 1001},
        {stop, _Req1, _State} = qr_login_handler:handle_request(Req0, State),
        %% 使用 meck:history 跨进程验证 notify 调用（消息发送在 mock 闭包中
        %% 跨进程不可达，必须走 meck 内置的 ETS 历史记录）。
        ?assert(meck:called(qr_login_event_ds, notify, [SessionToken, '_']),
                "qr_login_event_ds:notify/2 未被 scan 调用"),
        Notifies = [Args || {_Pid, {qr_login_event_ds, notify, Args}, _Ret}
                            <- meck:history(qr_login_event_ds),
                            length(Args) =:= 2],
        ?assertMatch([[SessionToken, #{<<"status">> := <<"scanned">>}]], Notifies)
    end).

%% scan 失败路径（如已过期）禁止 broadcast
scan_does_not_broadcast_when_expired_test_() ->
    SessionToken = <<"session-no-broadcast-expired">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{<<"status">> => <<"waiting">>, <<"expires_at">> => past_ms()}}
            end},
            {'delete', 1, fun(_) -> ok end}
        ]},
        {qr_login_event_ds, [
            {'notify', 2, fun(_, _) -> {ok, 1} end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => scan, current_uid => 1001},
        {stop, _Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assertNot(meck:called(qr_login_event_ds, notify, ['_', '_']),
                   "expired 路径不应调 notify/2")
    end).

%% confirm 成功后必须以 confirmed + login_token 事件 broadcast
confirm_broadcasts_confirmed_event_with_token_on_success_test_() ->
    SessionToken = <<"session-broadcast-confirm">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"scanned">>,
                    <<"scanned_by">> => 1001,
                    <<"expires_at">> => future_ms(),
                    <<"device_id">> => <<"web-d1">>,
                    <<"device_name">> => <<"Safari">>,
                    <<"platform">> => <<"web">>
                }}
            end},
            {'set', 3, fun(_Key, _Data, 60) -> ok end}
        ]},
        {token_ds, [
            {'encrypt_token', 1, fun(1001) -> <<"login-token-1001">> end}
        ]},
        {user_device_repo, [
            {'save', 4, fun(_, _, _, _) -> ok end}
        ]},
        {qr_login_event_ds, [
            {'event', 2, fun(confirmed, <<"login-token-1001">>) ->
                #{<<"status">> => <<"confirmed">>, <<"token">> => <<"login-token-1001">>}
            end},
            {'notify', 2, fun(_KeyToken, _Event) -> {ok, 1} end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => confirm, current_uid => 1001},
        {stop, _Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assert(meck:called(qr_login_event_ds, notify, [SessionToken, '_']),
                "qr_login_event_ds:notify/2 未被 confirm 调用"),
        Notifies = [Args || {_Pid, {qr_login_event_ds, notify, Args}, _Ret}
                            <- meck:history(qr_login_event_ds),
                            length(Args) =:= 2],
        [[K, E] | _] = Notifies,
        ?assertEqual(SessionToken, K),
        ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, E)),
        ?assertEqual(<<"login-token-1001">>, maps:get(<<"token">>, E))
    end).

%% confirm 失败（跨用户）禁止 broadcast
confirm_does_not_broadcast_when_forbidden_test_() ->
    SessionToken = <<"session-no-broadcast-forbidden">>,
    QRToken = make_qr_token(SessionToken),
    ?WITH_MECKS(common_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun({qr_login, KeyToken}) when KeyToken =:= SessionToken ->
                {ok, #{
                    <<"status">> => <<"scanned">>,
                    <<"scanned_by">> => 2002,
                    <<"expires_at">> => future_ms()
                }}
            end}
        ]},
        {qr_login_event_ds, [
            {'notify', 2, fun(_, _) -> {ok, 1} end}
        ]}
    ], fun() ->
        Body = jsx:encode(#{<<"qr_token">> => QRToken}),
        Req0 = req(<<"POST">>, Body, []),
        State = #{action => confirm, current_uid => 1001},
        {stop, _Req1, _State} = qr_login_handler:handle_request(Req0, State),
        ?assertNot(meck:called(qr_login_event_ds, notify, ['_', '_']),
                   "跨用户 forbidden 路径不应调 notify/2")
    end).

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
        ]}
    ].

req(Method, Body, Qs) ->
    #{
        method => Method,
        body => Body,
        qs => Qs
    }.

response_result(Req) ->
    maps:get(response_result, Req).

future_ms() ->
    erlang:system_time(millisecond) + 60000.

past_ms() ->
    erlang:system_time(millisecond) - 1000.

make_qr_token(SessionToken) ->
    base64:encode(<<SessionToken/binary, ":1700000000000">>).
