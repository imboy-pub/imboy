-module(web_human_bootstrap_login_tests).
-include_lib("eunit/include/eunit.hrl").

%% Direct boundary mocks: no application, database or service boot.
-define(WEB, #{jwt_purpose => web_human}).
-define(UID, 111174215241304065).

bootstrap_public_allowlist_test() ->
    with_mocks([
        {cowboy_req, [{header, fun(_, _, Default) -> Default end}]},
        {config_ds, [
            {env, fun(login_rsa_pub_key) -> <<"public-key">> end},
            {env, fun(ws_url, _) -> <<"wss://trusted.test/old?credential=redacted#fragment">>;
                (login_pwd_rsa_encrypt, _) -> <<"on">>;
                (_, Default) -> Default end}]},
        {elib_oss, [{public_base_url, fun() -> <<"https://assets.test">> end}]},
        {imboy_policy, [{effective_features, fun() -> #{core => true} end}]},
        {product_experience, [{effective_binary, fun() -> <<"community">> end},
            {config_version, fun() -> <<"test-version">> end}]},
        {product_terminology, [{public, fun(_) -> #{} end}]},
        {elib_response, response_mocks()}
    ], fun() ->
        {ok, Req, _} = index_handler:init(#{}, #{action => web_bootstrap}),
        Payload = maps:get(payload, Req),
        ?assertEqual(lists:sort([<<"ws_url">>, <<"login_pwd_rsa_encrypt">>,
            <<"login_rsa_pub_key">>, <<"public_base_url">>, <<"attachment_download_targets">>, <<"features">>,
            <<"effective_product_experience">>, <<"config_version">>, <<"terminology">>]),
            lists:sort(maps:keys(Payload))),
        ?assertEqual(<<"wss://trusted.test/api/v1/web/ws">>, maps:get(<<"ws_url">>, Payload)),
        ?assertEqual(<<"1">>, maps:get(<<"login_pwd_rsa_encrypt">>, Payload))
    end).

web_login_registration_failure_hides_credentials_test() ->
    login_case({error, service_unavailable}, fun(Req) ->
        ?assertEqual(503, maps:get(status, Req)),
        ?assertNot(maps:is_key(payload, Req)),
        ?assertEqual(0, login_cast_count())
    end).

web_login_registers_fixed_metadata_test() ->
    login_case(ok, fun(Req) ->
        ?assertEqual(200, maps:get(status, Req)),
        ?assertEqual(<<"synthetic-tk">>, maps:get(<<"token">>, maps:get(payload, Req))),
        ?assertEqual(1, login_cast_count())
    end).

web_credential_settings_failure_precedes_session_side_effects_test() ->
    credential_case(ok, fun() ->
        meck:expect(user_setting_ds, find_web_by_uid, fun(_) -> {error, unavailable} end),
        ?assertEqual({error, service_unavailable}, passport_logic:do_web_login(
            <<"email">>, <<"synthetic@example.test">>, <<"pwd">>, web_values())),
        ?assertEqual(0, meck:num_calls(user_device_ds, register_web, '_')),
        ?assertEqual(0, meck:num_calls(user_device_logic, kick_device, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, issue_web_session, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
        ?assertEqual(0, meck:num_calls(elib_async, async, '_'))
    end).

web_credential_login_registration_failure_does_not_kick_or_sign_test() ->
    credential_case({error, db_down}, fun() ->
        ?assertEqual({error, service_unavailable}, passport_logic:do_web_login(
            <<"email">>, <<"synthetic@example.test">>, <<"pwd">>, web_values())),
        ?assertEqual({error, service_unavailable}, passport_logic:do_web_login_by_code(
            <<"email">>, <<"synthetic@example.test">>, <<"123456">>, web_values())),
        ?assertEqual(2, meck:num_calls(user_device_ds, register_web, '_')),
        ?assertEqual(0, meck:num_calls(user_device_logic, kick_device, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_refreshtoken, '_'))
    end).

web_credential_login_rejects_nonhuman_and_disabled_before_registration_test() ->
    lists:foreach(fun({Type, Status}) ->
        credential_case(ok, fun() ->
            meck:expect(user_ds, find_by_id, fun(?UID, <<"status,account_type">>) ->
                #{<<"status">> => Status, <<"account_type">> => Type} end),
            ?assertMatch({error, _}, passport_logic:do_web_login(
                <<"email">>, <<"synthetic@example.test">>, <<"pwd">>, web_values())),
            ?assertEqual(0, meck:num_calls(user_device_ds, register_web, '_')),
            ?assertEqual(0, meck:num_calls(user_device_logic, kick_device, '_')),
            ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_'))
        end)
    end, [{1, 1}, {2, 1}, {3, 1}, {0, 0}, {0, -1}]).

web_credential_login_registers_then_kicks_then_signs_test() ->
    credential_case(ok, fun() ->
        %% Exercise real token_ds epoch selection, mock only its signing/config boundaries.
        meck:unload(token_ds),
        with_mocks([
            {auth_session_ds, [{current_epoch, fun(?UID) ->
                ?assert(meck:called(user_device_logic, kick_device,
                    [?UID, <<"web">>, <<"old-web-did">>])),
                {ok, 7} end}]},
            {config_ds, [{env, fun(jwt_key, _) -> <<"synthetic-key">> end}]},
            {imboy_jwt, [{sign, fun(Claims, <<"synthetic-key">>) ->
                ?assertEqual(7, maps:get(<<"ep">>, Claims)),
                ?assertEqual(456, maps:get(<<"dsid">>, Claims)),
                ?assertEqual(<<"web-did">>, maps:get(<<"did">>, Claims)),
                case maps:get(<<"sub">>, Claims) of
                    <<"tk">> -> <<"after-kick-tk">>;
                    <<"rtk">> -> <<"after-kick-rtk">>
                end end}]}
        ], fun() ->
            {ok, Data} = passport_logic:do_web_login(
                <<"email">>, <<"synthetic@example.test">>, <<"pwd">>, web_values()),
            ?assertEqual(<<"after-kick-tk">>, maps:get(<<"token">>, Data)),
            ?assertEqual(<<"after-kick-rtk">>, maps:get(<<"refreshtoken">>, Data))
        end)
    end).

web_initial_login_epoch_unknown_never_signs_or_reports_success_test() ->
    lists:foreach(fun(EpochResult) ->
        lists:foreach(fun(Method) ->
            credential_case(ok, fun() ->
                with_mocks([{auth_session_ds, [{current_epoch, fun(?UID) -> EpochResult end}]}],
                    fun() ->
                        Result = case Method of
                            password -> passport_logic:do_web_login(<<"email">>,
                                <<"synthetic@example.test">>, <<"pwd">>, web_values());
                            code -> passport_logic:do_web_login_by_code(<<"email">>,
                                <<"synthetic@example.test">>, <<"123456">>, web_values())
                        end,
                        ?assertEqual({error, service_unavailable}, Result),
                        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
                        ?assertEqual(0, meck:num_calls(token_ds, encrypt_refreshtoken, '_')),
                        ?assertEqual(0, meck:num_calls(token_ds, issue_web_session, '_')),
                        ?assertEqual(0, meck:num_calls(elib_async, async, '_'))
                    end)
            end)
        end, [password, code])
    end, [{error, db_down}, undefined, {ok, 0}, {ok, <<"7">>}, {ok, 1.0}]).

web_login_invalid_did_rejected_before_authentication_test() ->
    lists:foreach(fun(Did) ->
        with_mocks([
            {cowboy_req, [{header, fun(<<"did">>, _, _) -> Did;
                (_, _, Default) -> Default end}]},
            {passport_logic, []}, {elib_param, []}, {elib_response, response_mocks()}
        ], fun() ->
            {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => login}),
            ?assertEqual(400, maps:get(code, Req)),
            ?assertEqual(0, meck:num_calls(passport_logic, do_login, '_')),
            ?assertEqual(0, meck:num_calls(elib_param, post, '_'))
        end)
    end, [<<>>, 42, <<255>>, binary:copy(<<"x">>, 129)]).

web_refresh_uses_only_verified_refresh_identity_test() ->
    with_mocks([
        {cowboy_req, [{header, fun(<<"imboy-refreshtoken">>, _) -> <<"synthetic-rtk">> end}]},
        {throttle, [{check, fun(refreshtoken, <<"synthetic-rtk">>) -> ok end}]},
        {auth_ds, [{verify_web_refresh_token, fun(<<"synthetic-rtk">>) ->
            {ok, ?UID, <<"verified-web-did">>, 7, 9999999999, 456} end}]},
        {token_ds, [{encrypt_web_token, fun(?UID, <<"verified-web-did">>, 7, 456) -> <<"new-tk">> end}]},
        {elib_response, response_mocks()}
    ], fun() ->
        {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => refreshtoken}),
        ?assertEqual(#{<<"token">> => <<"new-tk">>}, maps:get(payload, Req)),
        ?assertEqual(0, meck:num_calls(token_ds, decrypt_token, '_'))
    end).

web_refresh_rejection_never_signs_test() ->
    with_mocks([
        {cowboy_req, [{header, fun(<<"imboy-refreshtoken">>, _) -> <<"wrong-purpose">> end}]},
        {throttle, [{check, fun(_, _) -> ok end}]},
        {auth_ds, [{verify_web_refresh_token, fun(_) -> {error, 706, <<"invalid">>} end}]},
        {token_ds, []}, {elib_response, response_mocks()}
    ], fun() ->
        {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => refreshtoken}),
        ?assertEqual(706, maps:get(code, Req)),
        ?assertEqual(401, maps:get(status, Req)),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_web_token, '_'))
    end).

web_refresh_does_not_upgrade_verified_epoch_after_revocation_test() ->
    with_mocks([
        {cowboy_req, [{header, fun(<<"imboy-refreshtoken">>, _) -> <<"synthetic-old-rtk">> end}]},
        {throttle, [{check, fun(_, _) -> ok end}]},
        {auth_ds, [{verify_web_refresh_token, fun(_) ->
            {ok, ?UID, <<"web-did">>, 2, 9999999999, 456} end}]},
        {auth_session_ds, [{current_epoch, fun(?UID) -> {ok, 3} end}]},
        {config_ds, [{env, fun(jwt_key, _) -> <<"synthetic-key">> end}]},
        {imboy_jwt, [{sign, fun(Claims, _) ->
            ?assertEqual(2, maps:get(<<"ep">>, Claims)),
            ?assertEqual(456, maps:get(<<"dsid">>, Claims)),
            ?assertEqual(<<"web-did">>, maps:get(<<"did">>, Claims)),
            <<"still-epoch-two-tk">> end}]},
        {elib_response, response_mocks()}
    ], fun() ->
        {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => refreshtoken}),
        ?assertEqual(#{<<"token">> => <<"still-epoch-two-tk">>}, maps:get(payload, Req)),
        ?assertEqual(0, meck:num_calls(auth_session_ds, current_epoch, '_'))
    end).

web_qr_create_rejects_seat_before_storage_test() ->
    with_qr(cache, #{<<"device_id">> => <<"web-did">>, <<"purpose">> => <<"seat">>},
        fun() ->
            {stop, Req, _} = qr_login_handler:handle_request(#{method => <<"POST">>},
                ?WEB#{action => create}),
            ?assertEqual(400, maps:get(code, Req)),
            ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
        end).

web_qr_create_fixes_platform_and_bounds_device_fields_test() ->
    with_qr(cache, #{<<"device_id">> => binary:copy(<<"中"/utf8>>, 128),
        <<"device_name">> => binary:copy(<<"中"/utf8>>, 80), <<"platform">> => <<"ios">>},
        fun() ->
            meck:expect(imboy_cache, set, fun
                ({qr_login, _}, Session, 60) ->
                    ?assertEqual(<<"web">>, maps:get(<<"platform">>, Session)),
                    ?assertEqual(<<"human">>, maps:get(<<"purpose">>, Session)), ok;
                (_, _, 60) -> ok
            end),
            {stop, Req, _} = qr_login_handler:handle_request(#{method => <<"POST">>},
                ?WEB#{action => create}),
            ?assertEqual(200, maps:get(status, Req))
        end),
    lists:foreach(fun(Body) ->
        with_qr(cache, Body, fun() ->
            {stop, Req, _} = qr_login_handler:handle_request(#{method => <<"POST">>},
                ?WEB#{action => create}),
            ?assertEqual(400, maps:get(code, Req)),
            ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
        end)
    end, [#{<<"device_id">> => binary:copy(<<"x">>, 129)},
        #{<<"device_id">> => <<"web-did">>, <<"device_name">> => 42},
        #{<<"device_id">> => <<"web-did">>, <<"device_name">> => binary:copy(<<"x">>, 81)},
        {raw_json, <<"{\"device_id\":\"web-did\",\"device_name\":\"", 255, "\"}">>}]).

web_qr_consumers_reject_mixed_session_test() ->
    lists:foreach(fun(Authority) ->
        lists:foreach(fun({Purpose, Platform}) ->
            with_qr(Authority, #{<<"session_token">> => <<"session">>}, fun() ->
                mock_session(Authority, qr_session(Purpose, Platform)),
                {stop, Status, _} = qr_login_handler:handle_request(#{method => <<"GET">>},
                    ?WEB#{action => status}),
                ?assertEqual(403, maps:get(code, Status)),
                {stop, Cancel, _} = qr_login_handler:handle_request(#{method => <<"POST">>},
                    ?WEB#{action => cancel}),
                ?assertEqual(403, maps:get(code, Cancel)),
                ?assertMatch({ok, _, _}, qr_login_sse_handler:init(#{}, ?WEB)),
                ?assertEqual(0, meck:num_calls(qr_login_session_ds, claim_login_token, '_')),
                ?assertEqual(0, meck:num_calls(qr_login_session_ds, transition_cancel, '_')),
                ?assertEqual(0, meck:num_calls(cowboy_req, stream_reply, '_'))
            end)
        end, [{<<"seat">>, <<"web">>}, {<<"human">>, <<"ios">>}])
    end, [cache, pg]).

qr_registration_failure_precedes_signing_for_both_authorities_test() ->
    lists:foreach(fun(Authority) ->
        with_qr(Authority, #{<<"qr_token">> => <<"qr">>}, fun() ->
            mock_session(Authority, qr_session(<<"human">>, <<"web">>)),
            meck:expect(qr_login_logic, log_device_login, fun(_, _, _, _) -> {error, db_down} end),
            {stop, Req, _} = qr_login_handler:handle_request(#{method => <<"POST">>},
                #{action => confirm, current_uid => ?UID}),
            ?assertEqual(503, maps:get(status, Req)),
            ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
            ?assertEqual(0, meck:num_calls(qr_login_session_ds, transition_confirm, '_')),
            ?assertEqual(0, meck:num_calls(qr_login_event_ds, notify, '_')),
            ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
        end)
    end, [cache, pg]).

web_qr_epoch_unknown_never_signs_confirms_or_notifies_test() ->
    lists:foreach(fun(Authority) ->
        lists:foreach(fun(EpochResult) ->
            with_qr(Authority, #{<<"qr_token">> => <<"qr">>}, fun() ->
                mock_session(Authority, qr_session(<<"human">>, <<"web">>)),
                meck:expect(qr_login_logic, log_device_login, fun(_, _, _, _) -> {ok, 456} end),
                with_mocks([{auth_session_ds, [{current_epoch, fun(?UID) -> EpochResult end}]}],
                    fun() ->
                        {stop, Req, _} = qr_login_handler:handle_request(#{method => <<"POST">>},
                            #{action => confirm, current_uid => ?UID}),
                        ?assertEqual(503, maps:get(status, Req)),
                        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
                        ?assertEqual(0, meck:num_calls(token_ds, encrypt_web_token, '_')),
                        ?assertEqual(0, meck:num_calls(qr_login_session_ds, transition_confirm, '_')),
                        ?assertEqual(0, meck:num_calls(qr_login_event_ds, notify, '_')),
                        ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
                    end)
            end)
        end, [{error, db_down}, undefined, {ok, 0}, {ok, <<"7">>}])
    end, [cache, pg]).

web_cache_sse_consumes_authoritative_session_once_test() ->
    with_qr(cache, #{}, fun() ->
        Session = (qr_session(<<"human">>, <<"web">>))#{
            <<"status">> => <<"confirmed">>, <<"login_token">> => <<"authoritative-tk">>},
        put(web_qr_session, Session),
        try
            meck:expect(imboy_cache, get, fun({qr_login, <<"session">>}) ->
                case get(web_qr_session) of undefined -> undefined; Value -> {ok, Value} end end),
            %% DEF-F6：claim 消费 = 写无 token 墓碑（TTL 120s），不再删除会话。
            meck:expect(imboy_cache, set, fun({qr_login, <<"session">>}, Tombstone, 120) ->
                ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, Tombstone)),
                ?assertEqual(null, maps:get(<<"login_token">>, Tombstone)),
                put(web_qr_session, Tombstone), ok end),
            meck:expect(qr_login_event_ds, confirmed, fun(<<"authoritative-tk">>, ?UID) ->
                {ok, #{<<"status">> => <<"confirmed">>, <<"uid">> => ?UID}} end),
            meck:expect(cowboy_req, stream_body, fun(_, nofin, _) -> ok end),
            Event = #{<<"status">> => <<"confirmed">>, <<"token">> => <<"untrusted-event">>},
            State = #{session_token => <<"session">>, web_human => true},
            ?assertMatch({stop, _, _}, qr_login_sse_handler:info(Event, #{}, State)),
            ?assertMatch({stop, _, _}, qr_login_sse_handler:info(Event, #{}, State)),
            %% 墓碑无 token：第二次 info 走 not_claimable，不再发放凭据。
            ?assertEqual(1, meck:num_calls(qr_login_event_ds, confirmed, '_'))
        after erase(web_qr_session) end
    end).

web_cache_sse_cancelled_session_does_not_emit_old_credentials_test() ->
    with_qr(cache, #{}, fun() ->
        Session = (qr_session(<<"human">>, <<"web">>))#{
            <<"status">> => <<"cancelled">>, <<"login_token">> => <<"old-tk">>},
        mock_session(cache, Session),
        ?assertMatch({stop, _, _}, qr_login_sse_handler:info(
            #{<<"status">> => <<"confirmed">>}, #{},
            #{session_token => <<"session">>, web_human => true})),
        ?assertEqual(0, meck:num_calls(cowboy_req, stream_body, '_'))
    end).

web_cache_concurrent_consumers_have_one_credential_winner_test() ->
    lists:foreach(fun(Kinds) ->
        with_qr(cache, #{}, fun() ->
            Table = ets:new(qr_web_claim_race, [set, public]),
            Ref = make_ref(),
            Parent = self(),
            Session = (qr_session(<<"human">>, <<"web">>))#{
                <<"status">> => <<"confirmed">>, <<"login_token">> => <<"authoritative-tk">>},
            true = ets:insert(Table, {session, Session}),
            try
                meck:expect(imboy_cache, flush_process_dict, fun() ->
                    erase(qr_claim_cached_value), ok end),
                meck:expect(imboy_cache, get, fun({qr_login, <<"session">>}) ->
                    Value = case get(qr_claim_cached_value) of
                        {cached, Cached} -> Cached;
                        undefined -> case ets:lookup(Table, session) of
                            [{_, S}] -> {ok, S}; [] -> undefined end
                    end,
                    put(qr_claim_cached_value, {cached, Value}),
                    case get(qr_claim_stale_read) of
                        undefined ->
                            put(qr_claim_stale_read, true),
                            %% Both callers hold the same stale pre-lock copy before claiming.
                            Parent ! {Ref, ready, self()},
                            receive {Ref, release} -> ok after 2000 -> error(claim_barrier_timeout) end;
                        _ -> ok
                    end,
                    Value
                end),
                meck:expect(imboy_cache, set, fun({qr_login, <<"session">>}, Tombstone, 120) ->
                    ?assertEqual(null, maps:get(<<"login_token">>, Tombstone)),
                    true = ets:insert(Table, {session, Tombstone}), ok end),
                meck:expect(qr_login_event_ds, confirmed, fun(<<"authoritative-tk">>, ?UID) ->
                    {ok, #{<<"status">> => <<"confirmed">>, <<"token">> => <<"authoritative-tk">>,
                        <<"uid">> => ?UID}} end),
                meck:expect(cowboy_req, stream_body, fun(Body, nofin, _) ->
                    true = ets:insert(Table, {{frame, make_ref()}, Body}), ok end),
                Pids = [spawn_link(fun() ->
                    Result = case Kind of
                        sse -> qr_login_sse_handler:info(#{<<"status">> => <<"confirmed">>}, #{},
                            #{session_token => <<"session">>, web_human => true});
                        legacy_sse -> qr_login_sse_handler:info(
                            #{<<"status">> => <<"confirmed">>, web_human => true}, #{},
                            #{session_token => <<"session">>});
                        legacy_status -> qr_login_handler:handle_request(#{method => <<"GET">>},
                            #{action => status})
                    end,
                    Parent ! {Ref, done, self(), Result}
                end) || Kind <- Kinds],
                [receive {Ref, ready, Pid} -> ok after 2000 -> error(claim_not_ready) end || Pid <- Pids],
                [Pid ! {Ref, release} || Pid <- Pids],
                Results = [receive {Ref, done, Pid, Result} -> Result
                    after 2000 -> error(claim_not_done) end || Pid <- Pids],
                HttpWinners = [P || {stop, #{payload := P}, _} <- Results,
                    maps:is_key(<<"token">>, P)],
                SseWinners = [B || {{frame, _}, B} <- ets:tab2list(Table)],
                ?assertEqual(1, length(HttpWinners) + length(SseWinners)),
                %% DEF-F6：唯一赢家写一次墓碑（无 token），不再有 delete。
                ?assertEqual(1, meck:num_calls(imboy_cache, set, '_')),
                ?assertEqual(2, meck:num_calls(imboy_cache, flush_process_dict, '_'))
            after ets:delete(Table) end
        end)
    end, [[sse, sse], [sse, legacy_status], [legacy_sse, legacy_status]]).

web_cache_claim_tombstone_write_failure_does_not_emit_credentials_test() ->
    with_qr(cache, #{}, fun() ->
        Session = (qr_session(<<"human">>, <<"web">>))#{
            <<"status">> => <<"confirmed">>, <<"login_token">> => <<"old-tk">>},
        mock_session(cache, Session),
        %% DEF-F6：墓碑写失败必须整体失败——若照常发放凭据而墓碑未落，
        %% 原 token 会话仍在缓存内可重复领取（一次性交付被击穿）。
        meck:expect(imboy_cache, set, fun(_, _, _) -> {error, unavailable} end),
        ?assertEqual({error, unavailable}, qr_login_logic:claim_web_cache_token(<<"session">>)),
        ?assertEqual(0, meck:num_calls(cowboy_req, stream_body, '_'))
    end).

web_cache_real_depcache_atomic_claim_test_() ->
    {setup, fun real_cache_setup/0, fun real_cache_cleanup/1,
        fun(Context) -> {timeout, 15, ?_test(real_cache_consumers(Context))} end}.

real_cache_setup() ->
    ?assertNot(lists:keymember(imboy, 1, application:which_applications())),
    ?assertEqual(undefined, whereis(imboy_cache)),
    %% claim 的 global:trans 互斥依赖 global_name_server（eunit VM 极简引导
    %% 不随内核启动；生产 vm.args 分布式启动时在线）——并发领取轮次前置。
    _ = (catch global:start()),
    lists:foreach(fun(Name) -> ?assertEqual(undefined, ets:info(Name)) end,
        ['m:imboy_cache', 'p:imboy_cache', 'd:imboy_cache']),
    Values = [{dsync_enabled, false}, {cache_fenced_namespaces, []},
        {cache_registry_overrides, #{}}, {qr_login_authority, cache}],
    Saved = [{K, application:get_env(imboy, K)} || {K, _} <- Values],
    lists:foreach(fun({K, V}) -> application:set_env(imboy, K, V) end, Values),
    try depcache:start_link(imboy_cache, #{memory_max => 1}) of
        {ok, Pid} ->
            unlink(Pid),
            {links, Linked} = process_info(Pid, links),
            Children = lists:delete(self(), Linked),
            #{cache_pid => Pid, children => Children, saved_env => Saved}
    catch Class:Reason:Stack ->
        restore_real_cache_env(Saved),
        erlang:raise(Class, Reason, Stack)
    end.

real_cache_cleanup(#{cache_pid := Pid, children := Children, saved_env := Saved}) ->
    %% Stop only the exact process returned by this fixture, never a process found by name.
    Refs = [{P, erlang:monitor(process, P)} || P <- [Pid | Children]],
    try
        ok = gen_server:stop(Pid, shutdown, 5000),
        lists:foreach(fun({P, Ref}) ->
            receive {'DOWN', Ref, process, P, shutdown} -> ok
            after 5000 -> error({owned_cache_process_not_stopped, P}) end
        end, Refs),
        ?assertEqual(undefined, whereis(imboy_cache)),
        lists:foreach(fun(Name) -> ?assertEqual(undefined, ets:info(Name)) end,
            ['m:imboy_cache', 'p:imboy_cache', 'd:imboy_cache'])
    after restore_real_cache_env(Saved) end.

restore_real_cache_env(Saved) ->
    lists:foreach(fun
        ({K, undefined}) -> application:unset_env(imboy, K);
        ({K, {ok, V}}) -> application:set_env(imboy, K, V)
    end, Saved).

real_cache_consumers(#{cache_pid := Pid, children := Children}) ->
    ?assertEqual(Pid, whereis(imboy_cache)),
    ?assertEqual(1, length(Children)),
    lists:foreach(fun(Child) ->
        ?assertEqual({initial_call, {depcache, cleanup, 1}}, process_info(Child, initial_call))
    end, Children),
    lists:foreach(fun(Name) -> ?assertEqual(Pid, ets:info(Name, owner)) end,
        ['m:imboy_cache', 'p:imboy_cache', 'd:imboy_cache']),
    %% Mock transport only: cache, wrapper, authority helper and global lock all run unchanged.
    with_mocks([
        {cowboy_req, [{method, fun(Req) -> maps:get(method, Req) end},
            {parse_qs, fun(Req) -> maps:get(qs, Req) end},
            {stream_body, fun(Body, nofin, #{test_parent := Parent, test_ref := Ref}) ->
                Parent ! {Ref, real_cache_frame, Body}, ok end}]},
        {elib_response, response_mocks()}
    ], fun() ->
        lists:foreach(fun real_cache_round/1, [
            [old_status, new_status], [old_status, sse],
            [new_status, old_sse], [sse, old_sse]
        ])
    end).

real_cache_round(Kinds) ->
    Ref = make_ref(),
    Parent = self(),
    ST = <<"real-cache-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Session = (qr_session(<<"human">>, <<"web">>))#{
        <<"status">> => <<"confirmed">>, <<"login_token">> => <<"real-cache-synthetic-tk">>},
    ok = imboy_cache:set({qr_login, ST}, Session, 60),
    Workers = [spawn_monitor(fun() -> real_cache_caller(Parent, Ref, ST, Session, Kind) end)
        || Kind <- Kinds],
    try
        [receive {Ref, ready, P} -> ok after 2000 -> error(real_cache_not_ready) end
            || {P, _} <- Workers],
        [P ! {Ref, claim} || {P, _} <- Workers],
        Results = [receive {Ref, done, P, First, Replay} -> {First, Replay}
            after 3000 -> error(real_cache_claim_timeout) end || {P, _} <- Workers],
        Frames = real_cache_frames(Ref),
        HttpWins = [Payload || {{stop, #{payload := Payload}, _}, _} <- Results,
            maps:is_key(<<"token">>, Payload)],
        ?assertEqual(1, length(HttpWins) + length(Frames)),
        lists:foreach(fun(Payload) ->
            ?assertEqual(<<"real-cache-synthetic-tk">>, maps:get(<<"token">>, Payload)),
            ?assertEqual(?UID, maps:get(<<"uid">>, Payload)),
            ?assertNot(maps:is_key(<<"web_human">>, Payload))
        end, HttpWins ++ Frames),
        ?assertEqual([], [Payload || {_, {stop, #{payload := Payload}, _}} <- Results,
            maps:is_key(<<"token">>, Payload)]),
        %% DEF-F6：claim 后保留无 token 墓碑（宽限窗内轮询幂等 confirmed）。
        ?assertMatch({ok, #{<<"status">> := <<"confirmed">>, <<"login_token">> := null}},
            imboy_cache:get({qr_login, ST}))
    after
        lists:foreach(fun({P, Monitor}) ->
            case is_process_alive(P) of true -> exit(P, kill); false -> ok end,
            erlang:demonitor(Monitor, [flush])
        end, Workers)
    end.

real_cache_caller(Parent, Ref, ST, Session, Kind) ->
    _ = depcache:in_process(true),
    ?assertEqual({ok, Session}, imboy_cache:get({qr_login, ST})),
    %% This is the real library's memo entry, not a simulated stale-cache flag.
    ?assertEqual({memo, {ok, Session}}, get({depcache, {qr_login, ST}})),
    Parent ! {Ref, ready, self()},
    receive {Ref, claim} -> ok after 3000 -> error(real_cache_not_released) end,
    Req = #{method => <<"GET">>, qs => [{<<"session_token">>, ST}],
        test_parent => Parent, test_ref => Ref},
    First = real_cache_request(Kind, Req, ST),
    %% DEF-F6：claim 后是墓碑而非删除；Replay 在宽限窗内幂等、不得重发 token。
    ?assertMatch({ok, #{<<"status">> := <<"confirmed">>, <<"login_token">> := null}},
        imboy_cache:get({qr_login, ST})),
    Replay = real_cache_request(Kind, Req, ST),
    Parent ! {Ref, done, self(), First, Replay}.

real_cache_request(old_status, Req, _) ->
    qr_login_handler:handle_request(Req, #{action => status});
real_cache_request(new_status, Req, _) ->
    qr_login_handler:handle_request(Req, ?WEB#{action => status});
real_cache_request(sse, Req, ST) ->
    qr_login_sse_handler:info(#{<<"status">> => <<"confirmed">>}, Req,
        #{session_token => ST, web_human => true});
real_cache_request(old_sse, Req, ST) ->
    qr_login_sse_handler:info(#{<<"status">> => <<"confirmed">>, web_human => true}, Req,
        #{session_token => ST}).

real_cache_frames(Ref) ->
    receive {Ref, real_cache_frame, <<"data: ", Json/binary>>} ->
        [jsone:decode(binary:part(Json, 0, byte_size(Json) - 2)) | real_cache_frames(Ref)]
    after 0 -> [] end.

qr_logic_maps_device_fields_and_propagates_errors_test() ->
    with_mocks([
        {elib_dt, [{now, fun() -> <<"synthetic-now">> end}]},
        {user_ds, [{find_by_id, fun(?UID, <<"status,account_type">>) ->
            #{<<"status">> => 1, <<"account_type">> => 0} end}]},
        {user_device_ds, [
            {register_web, fun(<<"synthetic-now">>, ?UID, <<"web-did">>, Values) ->
                ?assertEqual(<<"web">>, maps:get(<<"cos">>, Values)),
                ?assertEqual(<<"Browser">>, maps:get(<<"dname">>, Values)),
                {error, db_down} end},
            {save, fun(_, _, _, Values) ->
                ?assertEqual(<<"ios">>, maps:get(<<"cos">>, Values)),
                ?assertEqual(<<"Phone">>, maps:get(<<"dname">>, Values)),
                {error, db_down} end}]}
    ], fun() ->
        ?assertEqual({error, db_down}, qr_login_logic:log_device_login(
            ?UID, <<"web-did">>, <<"Browser">>, <<"web">>)),
        ?assertEqual({error, db_down}, qr_login_logic:log_device_login(
            ?UID, <<"native-did">>, <<"Phone">>, <<"ios">>))
    end).

login_case(Registration, Assert) ->
    with_mocks([
        {cowboy_req, [{header, fun(<<"did">>, _, _) -> <<"web-did">>;
            (<<"cos">>, _, _) -> <<"ios">>;
            (<<"dname">>, _, _) -> <<"Browser">>;
            (_, _, Default) -> Default end}]},
        {elib_param, [{post, fun(_) -> #{<<"type">> => <<"email">>,
            <<"account">> => <<"synthetic@example.test">>, <<"pwd">> => <<"pwd">>} end}]},
        {elib_req, [{get_client_ip, fun(_) -> <<"127.0.0.1">> end}]},
        {elib_cipher, [{safe_rsa_decrypt, fun(Pwd, _) -> Pwd end}]},
        {passport_logic, [
            {do_web_login, fun(<<"email">>, _, <<"pwd">>, Values) ->
                ?assertEqual(<<"web-did">>, maps:get(<<"did">>, Values)),
                ?assertEqual(<<"web">>, maps:get(<<"cos">>, Values)),
                ?assertEqual(<<"web">>, maps:get(<<"dtype">>, Values)),
                case Registration of
                    ok -> {ok, #{<<"uid">> => ?UID, <<"token">> => <<"synthetic-tk">>,
                        <<"refreshtoken">> => <<"synthetic-rtk">>, <<"setting">> => #{}}};
                    Error -> Error
                end end},
            {find_user_setting, fun(?UID) -> #{} end}]},
        {elib_response, response_mocks()}
    ], fun() ->
        %% This suite is intentionally run without the application or its real user_server.
        ?assertEqual(undefined, whereis(user_server)),
        true = register(user_server, self()),
        try
            {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => login}),
            ?assertEqual(0, meck:num_calls(passport_logic, record_login_device, '_')),
            Assert(Req)
        after
            unregister(user_server),
            _ = login_cast_count()
        end
    end).

login_cast_count() ->
    receive {'$gen_cast', {login_success, ?UID, _}} -> 1 + login_cast_count()
    after 0 -> 0 end.

web_values() ->
    #{<<"did">> => <<"web-did">>, <<"cos">> => <<"web">>,
        <<"dtype">> => <<"web">>, <<"dname">> => <<"Browser">>}.

credential_case(Registration, Test) ->
    User = #{<<"id">> => ?UID, <<"password">> => <<"synthetic-hash">>,
        <<"email">> => <<"synthetic@example.test">>, <<"nickname">> => <<"test">>,
        <<"avatar">> => <<>>, <<"account">> => <<"synthetic">>, <<"gender">> => 0,
        <<"region">> => <<>>, <<"sign">> => <<>>, <<"status">> => 1},
    with_mocks([
        {elib_type, [{is_email, fun(_) -> true end}]},
        {elib_async, [{async, fun(_) -> ok end}]},
        {user_ds, [{find_by_email, fun(_, _) -> User end},
            {find_by_id, fun(?UID, <<"status,account_type">>) ->
                #{<<"status">> => 1, <<"account_type">> => 0} end}]},
        {login_attempt_ds, [{begin_attempt, fun(_, _, default) -> {ok, synthetic_ref} end},
            {end_attempt, fun(synthetic_ref, _) -> ok end}]},
        {login_security_logic, [{check_login_allowed, fun(_) -> {ok, true} end}]},
        {verification_code_ds, [{consume, fun(_, <<"123456">>) -> {ok, <<"123456">>} end}]},
        {elib_password, [{verify, fun(<<"pwd">>, <<"synthetic-hash">>) -> {ok, true} end},
            {verify_and_plan, fun(_, _) -> {ok, #{version => v2}} end}]},
        {elib_dt, [{now, fun() -> <<"synthetic-now">> end}]},
        {user_setting_ds, [{find_web_by_uid, fun(?UID) -> {ok, #{<<"nested">> => #{<<"keep">> => true}}} end}]},
        {user_device_ds, [{register_web, fun(_, ?UID, <<"web-did">>, Values) ->
            ?assertEqual(<<"web">>, maps:get(<<"cos">>, Values)),
            case Registration of ok -> {ok, 456}; Other -> Other end end}]},
        {user_device_logic, [
            {check_login_conflict, fun(?UID, <<"web">>) ->
                ?assertEqual(0, meck:num_calls(user_device_logic, kick_device, '_')),
                {ok, conflict, #{<<"device_id">> => <<"old-web-did">>}} end},
            {kick_device, fun(?UID, <<"web">>, <<"old-web-did">>) ->
                ?assert(meck:called(user_device_ds, register_web, ['_', ?UID, <<"web-did">>, '_'])),
                ok end}]},
        {token_ds, [
            {encrypt_token, fun(?UID, <<"web-did">>) ->
                ?assert(meck:called(user_device_logic, kick_device, [?UID, <<"web">>, <<"old-web-did">>])),
                <<"after-kick-tk">> end},
            {encrypt_refreshtoken, fun(?UID, <<"web-did">>) ->
                ?assert(meck:called(user_device_logic, kick_device, [?UID, <<"web">>, <<"old-web-did">>])),
                <<"after-kick-rtk">> end}]}
    ], Test).

with_qr(Authority, Body, Test) ->
    Old = application:get_env(imboy, qr_login_authority),
    application:set_env(imboy, qr_login_authority, Authority),
    %% claim 的 global:trans 互斥依赖 global_name_server；eunit VM 极简引导
    %% （no_dot_erlang）不随内核启动 global，而生产 vm.args 分布式启动时在线。
    %% 并发领取类用例（唯一赢家）需该契约在线。幂等：已启动则直通。
    _ = (catch global:start()),
    try with_mocks([
        {cowboy_req, [{method, fun(Req) -> maps:get(method, Req) end},
            {read_body, fun(Req) ->
                Raw = case Body of {raw_json, Bytes} -> Bytes; _ -> jsone:encode(Body) end,
                {ok, Raw, Req} end},
            {parse_qs, fun(_) -> [{<<"session_token">>, <<"session">>}] end},
            {reply, fun(Status, _, _, Req) -> Req#{status => Status} end}]},
        {imboy_cache, [{get, fun({qr_login_qr, <<"qr">>}) -> {ok, <<"session">>} end},
            {flush_process_dict, fun() -> ok end}]},
        {qr_login_session_ds, [{find_by_qr_token, fun(<<"qr">>) ->
            #{<<"session_token">> => <<"session">>} end}]},
        {qr_login_logic, []}, {qr_login_event_ds, []}, {token_ds, []},
        {elib_response, response_mocks()}
    ], Test)
    after
        case Old of undefined -> application:unset_env(imboy, qr_login_authority);
            {ok, V} -> application:set_env(imboy, qr_login_authority, V) end
    end.

mock_session(cache, Session) ->
    meck:expect(imboy_cache, get, fun({qr_login_qr, <<"qr">>}) -> {ok, <<"session">>};
        ({qr_login, <<"session">>}) -> {ok, Session} end);
mock_session(pg, Session) ->
    meck:expect(qr_login_session_ds, find_by_session_token, fun(<<"session">>) -> Session end).

qr_session(Purpose, Platform) ->
    #{<<"status">> => <<"scanned">>, <<"scanned_by">> => ?UID,
        <<"purpose">> => Purpose, <<"platform">> => Platform,
        <<"device_id">> => <<"web-did">>, <<"device_name">> => <<"Browser">>,
        <<"expires_at">> => erlang:system_time(millisecond) + 60000}.

response_mocks() ->
    [{success, fun(Req, Payload) -> Req#{status => 200, payload => Payload} end},
        {success, fun(Req, Payload, _) -> Req#{status => 200, payload => Payload} end},
        {error, fun(Req, _, Code) -> Req#{status => 200, code => Code} end},
        {error_with_status, fun(Req, Status, _, Code) -> Req#{status => Status, code => Code} end}].

with_mocks(Mocks, Test) ->
    try
        lists:foreach(fun({Module, Expectations}) ->
            Options = case Module of
                qr_login_logic -> [passthrough, no_link, non_strict, unstick];
                _ -> [no_link, non_strict, unstick]
            end,
            meck:new(Module, Options),
            lists:foreach(fun({Name, Fun}) -> meck:expect(Module, Name, Fun) end, Expectations)
        end, Mocks),
        Test()
    after
        lists:foreach(fun({Module, _}) -> catch meck:unload(Module) end, lists:reverse(Mocks))
    end.
