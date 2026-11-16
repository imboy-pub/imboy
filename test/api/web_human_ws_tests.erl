-module(web_human_ws_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_frame.hrl").

with_mocks(Fun) ->
    Modules = [cowboy_req, cors_middleware, elib_response, auth_ds, config_ds, throttle,
        token_ds, user_ds, user_device_ds, auth_session_ds, elib_log],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        meck:expect(cowboy_req, method, fun(_) -> <<"GET">> end),
        meck:expect(cowboy_req, binding, fun(_, _, Default) -> Default end),
        meck:expect(cowboy_req, parse_qs, fun(R) -> maps:get(qs, R, []) end),
        meck:expect(cowboy_req, header, fun(K, R) -> maps:get(K, maps:get(headers, R, #{}), undefined) end),
        meck:expect(cowboy_req, header, fun(K, R, D) -> maps:get(K, maps:get(headers, R, #{}), D) end),
        meck:expect(cowboy_req, set_resp_header, fun(K, V, R) -> R#{resp_headers => (maps:get(resp_headers, R, #{}))#{K => V}} end),
        meck:expect(cowboy_req, reply, fun(S, R) -> R#{status => S} end),
        meck:expect(elib_response, error_with_status, fun(R, S, _, _) -> R#{status => S} end),
        meck:expect(cors_middleware, web_origin_allowed, fun(O) -> O =:= <<"https://web.example.test">> end),
        meck:expect(token_ds, decrypt_web_token, fun
            (<<"synthetic-web">>, <<"tk">>) -> {ok, 123, 9999999999, <<"tk">>, <<"trusted-device">>, 2, 456};
            (_, _) -> {error, 401, <<"invalid">>, #{}}
        end),
        meck:expect(user_ds, find_by_id, fun(123, _) -> #{<<"status">> => 1, <<"account_type">> => 0} end),
        meck:expect(user_device_ds, is_active_web, fun(123, <<"trusted-device">>, 456) -> true end),
        meck:expect(auth_session_ds, current_epoch, fun(123) -> {ok, 2} end),
        meck:expect(auth_ds, web_session_valid, fun(123, <<"trusted-device">>, 2, 456) -> true; (_, _, _, _) -> false end),
        meck:expect(config_ds, env, fun(_, Default) -> Default end),
        meck:expect(throttle, check, fun(throttle_ws, <<"trusted-device">>) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),
        Fun()
    after
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.

opts() -> #{surface => web_human, jwt_purpose => web_human, web_auth => ws, jwt_methods => [<<"GET">>]}.
request(Protocol) -> #{headers => #{<<"origin">> => <<"https://web.example.test">>, <<"sec-websocket-protocol">> => Protocol}}.
valid_protocol() -> <<"imboy.v2, imboy.auth.c3ludGhldGljLXdlYg">>.

malformed_duplicate_and_oversized_protocols_reject_test() ->
    with_mocks(fun() ->
        Protocols = [undefined, <<>>, <<"imboy.v2">>, <<"imboy.auth.c3ludGhldGljLXdlYg">>,
            <<"imboy.v2, imboy.auth.">>, <<"imboy.v2, imboy.auth.YQ==">>,
            <<"imboy.v2, imboy.auth.YR">>, <<"imboy.v2, imboy.auth.a">>,
            <<"text, imboy.auth.c3ludGhldGljLXdlYg">>,
            <<"imboy.v2, imboy.v2, imboy.auth.c3ludGhldGljLXdlYg">>,
            <<"imboy.v2, imboy.auth.YQ, imboy.auth.YQ">>,
            <<"imboy.v2, imboy.auth.", (binary:copy(<<"A">>, 8192))/binary>>],
        lists:foreach(fun(P) ->
            ?assertMatch({stop, #{status := 400}}, auth_middleware_api_v1:execute(request(P), #{handler_opts => opts()}))
        end, Protocols)
    end).

origin_and_multiple_credential_sources_reject_test() ->
    with_mocks(fun() ->
        R = request(valid_protocol()),
        H = maps:get(headers, R),
        lists:foreach(fun(Origin) ->
            ?assertMatch({stop, #{status := 403}}, auth_middleware_api_v1:execute(R#{headers => H#{<<"origin">> => Origin}}, #{handler_opts => opts()}))
        end, [undefined, <<>>, <<"null">>, <<"https://evil.example.test">>]),
        lists:foreach(fun(K) ->
            ?assertMatch({stop, #{status := 400}}, auth_middleware_api_v1:execute(R#{qs => [{K, <<"same-synthetic-token">>}]}, #{handler_opts => opts()}))
        end, [<<"token">>, <<"auth">>, <<"authorization">>, <<"did">>, <<"cos">>]),
        lists:foreach(fun(K) ->
            ?assertMatch({stop, #{status := 400}}, auth_middleware_api_v1:execute(R#{headers => H#{K => <<"same-synthetic-token">>}}, #{handler_opts => opts()}))
        end, [<<"authorization">>, <<"token">>])
    end).

trusted_state_only_and_no_auth_subprotocol_echo_test() ->
    with_mocks(fun() ->
        {ok, R, #{handler_opts := State}} = auth_middleware_api_v1:execute(request(valid_protocol()), #{handler_opts => opts()}),
        ?assertEqual(<<"imboy.v2">>, maps:get(<<"sec-websocket-protocol">>, maps:get(headers, R))),
        ?assertNot(maps:is_key(<<"authorization">>, maps:get(headers, R))),
        {cowboy_websocket, Upgraded, WS, _} = websocket_handler:init(R, State),
        ?assertEqual(<<"imboy.v2">>, maps:get(<<"sec-websocket-protocol">>, maps:get(resp_headers, Upgraded))),
        ?assertEqual(123, maps:get(current_uid, WS)),
        ?assertEqual(<<"trusted-device">>, maps:get(did, WS)),
        ?assertEqual(<<"web">>, maps:get(dtype, WS)),
        ?assertEqual(v2, maps:get(framing, WS)),
        ?assert(websocket_ds:session_still_valid(WS)),
        ?assertMatch({ok, _, hibernate}, websocket_handler:websocket_handle(ping, WS)),
        meck:expect(auth_ds, web_session_valid, fun(_, _, _, _) -> false end),
        ?assertNot(websocket_ds:session_still_valid(WS)),
        ?assertMatch({reply, {close, 4001, _}, _}, websocket_handler:websocket_handle({ping, <<>>}, WS)),
        ?assertMatch({ok, #{status := 401}, _}, websocket_handler:init(R, State))
    end).

missing_or_expired_web_state_never_uses_legacy_test() ->
    with_mocks(fun() ->
        R = request(<<"imboy.v2">>),
        lists:foreach(fun(S) ->
            ?assertNot(websocket_ds:session_still_valid(S)),
            ?assertMatch({ok, #{status := 401}, _}, websocket_handler:init(R, S))
        end, [opts(),
            (opts())#{current_uid => 123, current_did => <<"trusted-device">>, session_epoch => 2, device_session_id => 456, token_expire_at => 1},
            (opts())#{current_uid => 123, current_did => <<>>, session_epoch => 2, device_session_id => 456, token_expire_at => 9999999999},
            (opts())#{current_uid => 123, current_did => <<"trusted-device">>, session_epoch => 0, token_expire_at => 9999999999}])
    end).

%% 每种入站分支都必须先过公共会话门，不能以业务流量持续刷新 idle timeout。
expired_or_revoked_web_frames_close_before_dispatch_test() ->
    with_mocks(fun() ->
        meck:new(imboy_codec, [passthrough, no_link]),
        try
            S = active_state(),
            lists:foreach(fun({State, SessionValid}) ->
                meck:expect(auth_ds, web_session_valid, fun(_, _, _, _) -> SessionValid end),
                lists:foreach(fun(Frame) ->
                    ?assertEqual({reply, {close, 4001, <<"session_revoked">>}, State},
                        websocket_handler:websocket_handle(Frame, State))
                end, inbound_frames())
            end, [{S, false}, {S#{token_expire_at => 1}, true}, {maps:remove(session_epoch, S), true}]),
            ?assertEqual(0, meck:num_calls(throttle, check, '_')),
            ?assertEqual(0, meck:num_calls(imboy_codec, unwrap_v2_frame, '_')),
            ?assertEqual(0, meck:num_calls(imboy_codec, decode_payload, '_')),
            ?assertEqual(0, meck:num_calls(elib_log, internal_log, '_'))
        after meck:unload(imboy_codec) end
    end).

active_web_delegates_and_native_dispatch_stays_unchanged_test() ->
    with_mocks(fun() ->
        S = active_state(),
        ?assertEqual({reply, {text, <<"PONG">>}, S, hibernate},
            websocket_handler:websocket_handle({text, <<"PING">>}, S)),
        ?assertEqual({reply, {text, <<"pong">>}, S, hibernate},
            websocket_handler:websocket_handle({text, <<"ping">>}, S)),
        ?assertEqual({ok, S, hibernate}, websocket_handler:websocket_handle(ping, S)),
        ?assertEqual({ok, S, hibernate}, websocket_handler:websocket_handle(pong, S)),
        ?assertEqual({ok, S, hibernate}, websocket_handler:websocket_handle(unknown_frame, S)),
        ?assertEqual({reply, {binary, imboy_frame:heartbeat_pong(7)}, S, hibernate},
            websocket_handler:websocket_handle({binary, imboy_frame:heartbeat_ping(7)}, S)),
        meck:expect(auth_ds, web_session_valid, fun(_, _, _, _) -> error(unexpected_native_web_gate) end),
        Native = maps:without([surface, jwt_purpose, web_auth], S#{token_expire_at => 1}),
        ?assertEqual({reply, {text, <<"PONG">>}, Native, hibernate},
            websocket_handler:websocket_handle({text, <<"PING">>}, Native)),
        ?assertEqual({ok, Native, hibernate}, websocket_handler:websocket_handle(ping, Native))
    end).

active_state() ->
    (opts())#{current_uid => 123, current_did => <<"trusted-device">>,
        did => <<"trusted-device">>, dtype => <<"web">>, session_epoch => 2, device_session_id => 456,
        token_expire_at => 9999999999, framing => v2, protocol => protobuf}.

inbound_frames() ->
    [ping, {ping, <<"synthetic">>}, pong, {pong, <<"synthetic">>},
        {text, <<"ping">>}, {text, <<"PING">>},
        {text, <<"{\"type\":\"C2C\",\"payload\":\"synthetic\"}">>},
        {text, <<"CLIENT_ACK,C2C,123,trusted-device">>},
        {binary, imboy_frame:encode(?FRAME_TYPE_MSG_C2C, 0, <<"synthetic">>)},
        {binary, imboy_frame:ack(123)}, {binary, imboy_frame:heartbeat_ping(7)},
        {binary, imboy_frame:encode(?FRAME_TYPE_HEARTBEAT_PING, 0, <<0>>)},
        {binary, <<"malformed-frame">>}, unknown_frame].

with_outbound_mocks(Fun) ->
    with_mocks(fun() ->
        Modules = [user_logic, app_version_logic, websocket_delivery_guard, imboy_codec,
            websocket_logic, micro_community_access_logic, ack_retry_cache],
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        try
            meck:expect(user_logic, online, fun(_, _, _, _) -> ok end),
            meck:expect(app_version_logic, check, fun(_, _, _) -> #{<<"upgrade_type">> => <<"none">>} end),
            meck:expect(imboy_codec, encode_ws_msg, fun(_, _, _, _) -> {binary, <<"synthetic-wire">>} end),
            meck:expect(imboy_codec, wrap_v2_frame, fun imboy_frame:encode/3),
            meck:expect(micro_community_access_logic, delivery_allowed, fun(_, _) -> true end),
            meck:expect(ack_retry_cache, delete_if_value, fun(_, _) -> true end),
            meck:expect(ack_retry_cache, get, fun(_) -> undefined end),
            meck:expect(websocket_logic, send_next, fun(_, _, _, _, _, _) -> ok end),
            Fun()
        after lists:foreach(fun meck:unload/1, lists:reverse(Modules)) end
    end).

invalid_web_init_and_outbound_info_have_no_side_effects_test() ->
    with_outbound_mocks(fun() ->
        S = (active_state())#{vsn => <<"synthetic-version">>},
        Invalid = [{S, false}, {S#{token_expire_at => 1}, true}, {S, exception}] ++
            [{maps:remove(K, S), true} || K <- [current_uid, current_did, session_epoch, token_expire_at, device_session_id]],
        lists:foreach(fun({State, SessionValid}) ->
            meck:expect(auth_ds, web_session_valid, fun(_, _, _, _) ->
                case SessionValid of exception -> error(synthetic_session_failure); _ -> SessionValid end
            end),
            Closed = {reply, {close, 4001, <<"session_revoked">>}, State},
            ?assertEqual(Closed, websocket_handler:websocket_init(State)),
            lists:foreach(fun(Info) ->
                ?assertEqual(Closed, websocket_handler:websocket_info(Info, State))
            end, outbound_infos())
        end, Invalid),
        ?assertEqual(0, meck:num_calls(user_logic, online, '_')),
        ?assertEqual(0, meck:num_calls(app_version_logic, check, '_')),
        ?assertEqual(0, meck:num_calls(websocket_delivery_guard, run, '_')),
        ?assertEqual(0, meck:num_calls(websocket_delivery_guard, retry, '_')),
        ?assertEqual(0, meck:num_calls(imboy_codec, encode_ws_msg, '_')),
        ?assertEqual(0, meck:num_calls(imboy_codec, wrap_v2_frame, '_')),
        ?assertEqual(0, meck:num_calls(ack_retry_cache, delete_if_value, '_')),
        ?assertEqual(0, meck:num_calls(websocket_logic, send_next, '_'))
    end).

active_web_and_native_init_outbound_preserve_dispatch_and_acl_test() ->
    with_outbound_mocks(fun() ->
        S = (active_state())#{vsn => <<"synthetic-version">>},
        ?assertEqual({ok, S, hibernate}, websocket_handler:websocket_init(S)),
        ?assert(meck:called(user_logic, online, [123, <<"web">>, self(), <<"trusted-device">>])),
        ?assertEqual(1, meck:num_calls(app_version_logic, check, '_')),
        Msg = #{<<"id">> => <<"synthetic-message">>, <<"type">> => <<"C2C">>},
        Expected = {reply, {binary, <<"synthetic-wire">>}, S, hibernate},
        ?assertEqual(Expected, websocket_handler:websocket_info({reply, Msg}, S)),
        meck:expect(micro_community_access_logic, delivery_allowed, fun(_, _) -> false end),
        ?assertEqual({ok, S, hibernate}, websocket_handler:websocket_info({reply, Msg}, S)),
        ?assertEqual(1, meck:num_calls(imboy_codec, encode_ws_msg, '_')),
        meck:expect(micro_community_access_logic, delivery_allowed, fun(_, _) -> true end),
        Bin = jsone:encode(Msg),
        Info = {timeout, make_ref(), {[100], {123, <<"trusted-device">>, <<"synthetic-message">>}, Bin}},
        ?assertMatch({reply, {binary, _}, S, hibernate}, websocket_handler:websocket_info(Info, S)),
        ?assertEqual(1, meck:num_calls(websocket_logic, send_next, '_')),
        meck:expect(auth_ds, web_session_valid, fun(_, _, _, _) -> error(unexpected_native_web_gate) end),
        Native = maps:without([surface, jwt_purpose, web_auth], S#{dtype => <<"ios">>, token_expire_at => 1}),
        ?assertEqual({ok, Native, hibernate}, websocket_handler:websocket_init(Native)),
        ?assertEqual({reply, {binary, <<"synthetic-wire">>}, Native, hibernate},
            websocket_handler:websocket_info({reply, Msg}, Native))
    end).

outbound_infos() ->
    Ref = make_ref(),
    Msg = #{<<"id">> => <<"synthetic-message">>, <<"type">> => <<"C2C">>},
    Attachment = Msg#{<<"payload">> => #{<<"attachment_id">> => <<"synthetic-attachment">>}},
    Bin = jsone:encode(Attachment),
    TimerKey = {123, <<"trusted-device">>, <<"synthetic-message">>},
    [{reply, Msg}, {reply, Attachment}, {reply, [{<<"id">>, <<"synthetic-message">>}]},
        {reply, Bin}, {timeout, Ref, Bin}, {timeout, Ref, Attachment},
        {timeout, Ref, {[100], TimerKey, Bin}}, {timeout, Ref, {[100], TimerKey, Attachment}},
        {kick_device, #{<<"reason">> => <<"synthetic-revocation">>}},
        {ack_cancel, 123, <<"trusted-device">>, <<"synthetic-message">>, 1},
        {close, 4000, <<"synthetic-close">>}, stop, unknown_info].
