-module(imboy_plugin_lifecycle_broadcast_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% P5-B3: broadcast_manifest_change/0 test
%% Verifies the function constructs a valid S2C message and
%% iterates over syn groups without crashing.
%% ===================================================================

setup() ->
    application:set_env(imboy, env, test),
    ok.

cleanup(_) ->
    ok.

broadcast_manifest_change_does_not_crash_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            %% Mock syn:group_names to return empty list (no online users)
            ok = meck:new(syn, [unstick, passthrough]),
            meck:expect(syn, group_names, fun(imboy_chat) -> [] end),

            %% Mock app_manifest_handler:build_manifest
            ok = meck:new(app_manifest_handler, [passthrough]),
            meck:expect(app_manifest_handler, build_manifest, fun() ->
                #{<<"features">> => #{}, <<"generated_at">> => 0}
            end),

            %% Should not crash even with empty online users
            Result = imboy_plugin_lifecycle:broadcast_manifest_change(),
            ?assertEqual(ok, Result),

            %% Wait for spawned process to complete
            timer:sleep(50),

            meck:unload(syn),
            meck:unload(app_manifest_handler)
        end
     end}.

broadcast_manifest_change_publishes_to_online_users_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            ok = meck:new(syn, [unstick, passthrough]),
            meck:expect(syn, group_names, fun(imboy_chat) -> [1001, 1002] end),
            meck:expect(syn, publish, fun(_Scope, _Uid, _Msg) -> {ok, 1} end),

            ok = meck:new(app_manifest_handler, [passthrough]),
            meck:expect(app_manifest_handler, build_manifest, fun() ->
                #{<<"features">> => #{<<"chat">> => true}, <<"generated_at">> => 1234}
            end),

            %% Mock message_ds:assemble_msg/8 and elib_id:gen/1
            %% (called inside elib_async:async — must be mocked or async process crashes)
            ok = meck:new(message_ds, [passthrough]),
            meck:expect(message_ds, assemble_msg, 8,
                fun(_Type, _From, _To, Payload, _Id, _Tpl, Action, _Extra) ->
                    #{<<"type">> => _Type, <<"action">> => Action,
                      <<"payload">> => Payload}
                end),
            ok = meck:new(elib_id, [passthrough]),
            meck:expect(elib_id, gen, fun(_Prefix) -> <<"test_id_123">> end),

            Result = imboy_plugin_lifecycle:broadcast_manifest_change(),
            ?assertEqual(ok, Result),

            %% Wait for async process to complete
            timer:sleep(200),

            %% syn:publish should be called twice (once per user)
            ?assertEqual(2, meck:num_calls(syn, publish, 3)),

            %% Verify the message is valid JSON containing manifest_updated action
            History = meck:history(syn),
            PublishCalls = [{P, MFA, R} || {P, {syn, publish, _} = MFA, R} <- History],
            [{_Pid1, {syn, publish, [_Scope, _Uid1, Msg1]}, {ok, 1}} | _] = PublishCalls,
            Decoded = jsone:decode(Msg1),
            ?assertEqual(<<"manifest_updated">>, maps:get(<<"action">>, Decoded)),
            ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Decoded)),

            meck:unload(syn),
            meck:unload(app_manifest_handler),
            meck:unload(message_ds),
            meck:unload(elib_id)
        end
     end}.
