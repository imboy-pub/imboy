-module(adm_admin_feature_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

init_config_features_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {imboy_feature, [
            {'all', 0, fun() ->
                #{<<"core">> => true, <<"channel">> => false}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_features, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{<<"core">> => true, <<"channel">> => false}, maps:get(payload, RespReq)),
        ?assertEqual(#{adm_user_id => 1001}, State)
    end).

init_config_policy_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {imboy_policy, [
            {'effective_view', 0, fun() ->
                #{
                    <<"profile">> => <<"enterprise">>,
                    <<"capabilities">> => #{<<"audit_mode">> => <<"full">>},
                    <<"features">> => #{<<"channel">> => true},
                    <<"plugins">> => #{<<"channel">> => #{<<"enabled">> => true}}
                }
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_policy, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"enterprise">>, maps:get(<<"profile">>, maps:get(payload, RespReq))),
        ?assertEqual(#{adm_user_id => 1001}, State)
    end).

init_config_policy_bootstrap_success_test_() ->
    BootstrapPayload = #{
        <<"meta">> => #{<<"profiles">> => #{<<"supported">> => [<<"community">>, <<"enterprise">>]}},
        <<"saved">> => #{<<"plugins">> => #{<<"channel">> => true}},
        <<"effective">> => #{<<"profile">> => <<"enterprise">>}
    },
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {imboy_policy, [
            {'admin_config_view', 0, fun() -> BootstrapPayload end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_policy_bootstrap, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(BootstrapPayload, maps:get(payload, RespReq)),
        ?assertEqual(#{adm_user_id => 1001}, State)
    end).

init_config_policy_meta_success_test_() ->
    MetaPayload = #{
        <<"profiles">> => #{<<"supported">> => [<<"community">>, <<"enterprise">>]},
        <<"capabilities">> => #{
            <<"storage_mode">> => #{
                <<"type">> => <<"enum">>,
                <<"options">> => [<<"archived">>, <<"secure_e2ee">>]
            }
        },
        <<"features">> => #{<<"all">> => [<<"core">>, <<"channel">>]}
    },
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {imboy_policy, [
            {'meta_view', 0, fun() -> MetaPayload end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_policy_meta, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(MetaPayload, maps:get(payload, RespReq)),
        ?assertEqual(#{adm_user_id => 1001}, State)
    end).

init_config_policy_saved_success_test_() ->
    SavedPayload = #{
        <<"profile">> => <<"enterprise">>,
        <<"capabilities">> => #{<<"message_export">> => false},
        <<"plugins">> => #{<<"channel">> => true},
        <<"features">> => #{<<"channel_order">> => false}
    },
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {imboy_policy, [
            {'saved_view', 0, fun() -> SavedPayload end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_policy_saved, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(SavedPayload, maps:get(payload, RespReq)),
        ?assertEqual(#{adm_user_id => 1001}, State)
    end).

preview_config_policy_post_success_test_() ->
    SavePayload = policy_save_payload(),
    PreviewPayload = #{
        <<"saved">> => #{
            <<"features">> => #{<<"channel">> => true}
        },
        <<"effective">> => #{
            <<"features">> => #{<<"channel">> => true},
            <<"plugins">> => #{<<"channel">> => #{<<"enabled">> => true}}
        },
        <<"adjustments">> => #{
            <<"features">> => #{
                <<"channel_order">> => #{
                    <<"saved">> => true,
                    <<"effective">> => false,
                    <<"reason">> => <<"dependency">>,
                    <<"depends_on">> => [<<"channel">>]
                }
            }
        }
    },
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> SavePayload end}
        ]},
        {imboy_policy, [
            {'preview_admin_config', 1, fun(Payload) ->
                ?assertEqual(SavePayload, Payload),
                {ok, PreviewPayload}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_policy_preview, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(PreviewPayload, maps:get(payload, RespReq)),
        ?assertEqual(#{adm_user_id => 1001}, State),
        ?assertEqual(1, meck:num_calls(elib_param, post, 1)),
        ?assertEqual(1, meck:num_calls(imboy_policy, preview_admin_config, 1))
    end).

preview_config_policy_bad_request_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"capabilities">> => #{<<"storage_mode">> => <<"invalid_mode">>}}
            end}
        ]},
        {imboy_policy, [
            {'preview_admin_config', 1, fun(_Payload) ->
                {error,
                    <<"invalid storage_mode value">>,
                    #{
                        <<"section">> => <<"capabilities">>,
                        <<"field">> => <<"storage_mode">>,
                        <<"reason">> => <<"invalid_enum">>
                    }}
            end}
        ]},
        {elib_response, [
            {'error', 4, fun(Req, Msg, Code, Options) ->
                Req#{
                    response_status => 400,
                    error_msg => Msg,
                    error_code => Code,
                    error_details => maps:get(<<"details">>, Options)
                }
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy_preview, adm_user_id => 1001}),
        ?assertEqual(400, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_BAD_REQUEST, maps:get(error_code, RespReq)),
        ?assertEqual(<<"invalid storage_mode value">>, maps:get(error_msg, RespReq)),
        ?assertEqual(
            #{
                <<"section">> => <<"capabilities">>,
                <<"field">> => <<"storage_mode">>,
                <<"reason">> => <<"invalid_enum">>
            },
            maps:get(error_details, RespReq)
        )
    end).

save_config_policy_put_success_test_() ->
    SavePayload = policy_save_payload(),
    PolicyPayload = policy_response_payload(),
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> SavePayload end}
        ]},
        {imboy_policy, [
            {'save_admin_config', 1, fun(Payload) ->
                ?assertEqual(SavePayload, Payload),
                {ok, PolicyPayload}
            end},
            {'effective_view', 0, fun() ->
                erlang:error(should_not_be_called)
            end},
            {'saved_view', 0, fun() ->
                erlang:error(should_not_be_called)
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_policy, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(PolicyPayload, maps:get(payload, RespReq)),
        ?assertEqual(true, maps:is_key(<<"saved">>, maps:get(payload, RespReq))),
        ?assertEqual(#{adm_user_id => 1001}, State),
        ?assertEqual(1, meck:num_calls(elib_param, post, 1)),
        ?assertEqual(1, meck:num_calls(imboy_policy, save_admin_config, 1)),
        ?assertEqual(0, meck:num_calls(imboy_policy, effective_view, 0)),
        ?assertEqual(0, meck:num_calls(imboy_policy, saved_view, 0))
    end).

save_config_policy_post_success_test_() ->
    SavePayload = policy_save_payload(),
    PolicyPayload = policy_response_payload(),
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> SavePayload end}
        ]},
        {imboy_policy, [
            {'save_admin_config', 1, fun(Payload) ->
                ?assertEqual(SavePayload, Payload),
                {ok, PolicyPayload}
            end},
            {'effective_view', 0, fun() ->
                erlang:error(should_not_be_called)
            end},
            {'saved_view', 0, fun() ->
                erlang:error(should_not_be_called)
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_policy, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(PolicyPayload, maps:get(payload, RespReq)),
        ?assertEqual(true, maps:is_key(<<"saved">>, maps:get(payload, RespReq))),
        ?assertEqual(#{adm_user_id => 1001}, State),
        ?assertEqual(1, meck:num_calls(elib_param, post, 1)),
        ?assertEqual(1, meck:num_calls(imboy_policy, save_admin_config, 1)),
        ?assertEqual(0, meck:num_calls(imboy_policy, effective_view, 0)),
        ?assertEqual(0, meck:num_calls(imboy_policy, saved_view, 0))
    end).

init_config_features_forbidden_without_settings_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {imboy_feature, [
            {'all', 0, fun() -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_features, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(imboy_feature, all, 0))
    end).

init_config_policy_forbidden_without_settings_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {imboy_policy, [
            {'effective_view', 0, fun() -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(imboy_policy, effective_view, 0))
    end).

init_config_policy_bootstrap_forbidden_without_settings_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {imboy_policy, [
            {'admin_config_view', 0, fun() -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy_bootstrap, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(imboy_policy, admin_config_view, 0))
    end).

init_config_policy_meta_forbidden_without_settings_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {imboy_policy, [
            {'meta_view', 0, fun() -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy_meta, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(imboy_policy, meta_view, 0))
    end).

init_config_policy_saved_forbidden_without_settings_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {imboy_policy, [
            {'saved_view', 0, fun() -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy_saved, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(imboy_policy, saved_view, 0))
    end).

preview_config_policy_forbidden_without_settings_update_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> erlang:error(should_not_be_called) end}
        ]},
        {imboy_policy, [
            {'preview_admin_config', 1, fun(_Payload) -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy_preview, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(elib_param, post, 1)),
        ?assertEqual(0, meck:num_calls(imboy_policy, preview_admin_config, 1))
    end).

save_config_policy_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"profile">> => <<"enterprise">>,
                    <<"capabilities">> => #{<<"message_export">> => false},
                    <<"features">> => #{<<"channel">> => true}
                }
            end}
        ]},
        {imboy_policy, [
            {'save_admin_config', 1, fun(Payload) ->
                ?assertEqual(<<"enterprise">>, maps:get(<<"profile">>, Payload)),
                {ok,
                    #{
                        <<"profile">> => <<"enterprise">>,
                        <<"capabilities">> => #{<<"message_export">> => false},
                        <<"features">> => #{<<"channel">> => true},
                        <<"plugins">> => #{<<"channel">> => #{<<"enabled">> => true}}
                    }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_policy, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"enterprise">>, maps:get(<<"profile">>, maps:get(payload, RespReq))),
        ?assertEqual(#{adm_user_id => 1001}, State)
    end).

save_config_policy_bad_request_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"profile">> => <<"invalid">>}
            end}
        ]},
        {imboy_policy, [
            {'save_admin_config', 1, fun(_Payload) ->
                {error,
                    <<"invalid profile value">>,
                    #{
                        <<"section">> => <<"profile">>,
                        <<"field">> => <<"profile">>,
                        <<"reason">> => <<"invalid_profile">>
                    }}
            end}
        ]},
        {elib_response, [
            {'error', 4, fun(Req, Msg, Code, Options) ->
                Req#{
                    response_status => 400,
                    error_msg => Msg,
                    error_code => Code,
                    error_details => maps:get(<<"details">>, Options)
                }
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy, adm_user_id => 1001}),
        ?assertEqual(400, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_BAD_REQUEST, maps:get(error_code, RespReq)),
        ?assertEqual(<<"invalid profile value">>, maps:get(error_msg, RespReq)),
        ?assertEqual(
            #{
                <<"section">> => <<"profile">>,
                <<"field">> => <<"profile">>,
                <<"reason">> => <<"invalid_profile">>
            },
            maps:get(error_details, RespReq)
        )
    end).

save_config_policy_forbidden_without_settings_update_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> erlang:error(should_not_be_called) end}
        ]},
        {imboy_policy, [
            {'save_admin_config', 1, fun(_Payload) -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(imboy_policy, save_admin_config, 1))
    end).

save_config_policy_forbidden_without_settings_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> erlang:error(should_not_be_called) end}
        ]},
        {imboy_policy, [
            {'save_admin_config', 1, fun(_Payload) -> erlang:error(should_not_be_called) end},
            {'effective_view', 0, fun() -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_policy, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(elib_param, post, 1)),
        ?assertEqual(0, meck:num_calls(imboy_policy, save_admin_config, 1)),
        ?assertEqual(0, meck:num_calls(imboy_policy, effective_view, 0))
    end).

policy_save_payload() ->
    #{
        <<"profile">> => <<"enterprise">>,
        <<"capabilities">> => #{
            <<"storage_mode">> => <<"archived">>,
            <<"message_search">> => true,
            <<"message_export">> => false,
            <<"audit_mode">> => <<"metadata">>
        },
        <<"features">> => #{
            <<"moment">> => false
        },
        <<"plugins">> => #{
            <<"channel">> => #{<<"enabled">> => true},
            <<"group_collab">> => false
        }
    }.

policy_response_payload() ->
    #{
        <<"profile">> => <<"enterprise">>,
        <<"capabilities">> => #{
            <<"storage_mode">> => <<"archived">>,
            <<"message_search">> => true,
            <<"message_export">> => false,
            <<"audit_mode">> => <<"metadata">>
        },
        <<"features">> => #{
            <<"channel">> => true,
            <<"channel_discover">> => true,
            <<"channel_invitation">> => true,
            <<"channel_order">> => true,
            <<"group_vote">> => false,
            <<"group_schedule">> => false,
            <<"group_task">> => false,
            <<"moment">> => false
        },
        <<"plugins">> => #{
            <<"channel">> => #{<<"enabled">> => true},
            <<"group_collab">> => #{<<"enabled">> => false},
            <<"moment">> => #{<<"enabled">> => false}
        },
        <<"saved">> => #{
            <<"profile">> => <<"enterprise">>,
            <<"capabilities">> => #{
                <<"storage_mode">> => <<"archived">>,
                <<"message_search">> => true,
                <<"message_export">> => false,
                <<"audit_mode">> => <<"metadata">>
            },
            <<"plugins">> => #{
                <<"channel">> => true,
                <<"group_collab">> => false,
                <<"moment">> => false
            }
        },
        <<"adjustments">> => #{
            <<"features">> => #{
                <<"channel_order">> => #{
                    <<"saved">> => true,
                    <<"effective">> => false,
                    <<"reason">> => <<"dependency">>,
                    <<"depends_on">> => [<<"channel">>]
                }
            }
        }
    }.
