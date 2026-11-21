-module(web_human_routes_tests).
-include_lib("eunit/include/eunit.hrl").

%% 普通 EUnit/meck：无应用启动、真实配置或数据库。
with_mocks(Fun) ->
    Modules = [
        cowboy_req,
        config_ds,
        elib_response,
        auth_ds,
        token_ds,
        user_ds,
        user_device_ds,
        auth_session_ds
    ],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        meck:expect(config_ds, env, fun(_, Default) -> Default end),
        meck:expect(cowboy_req, method, fun(R) -> maps:get(method, R, <<"GET">>) end),
        meck:expect(cowboy_req, path, fun(R) -> maps:get(path, R, <<"/api/v1/web/msg">>) end),
        meck:expect(cowboy_req, binding, fun(K, R, D) ->
            maps:get(K, maps:get(bindings, R, #{}), D)
        end),
        meck:expect(cowboy_req, parse_qs, fun(R) -> maps:get(qs, R, []) end),
        meck:expect(cowboy_req, header, fun(K, R) ->
            maps:get(K, maps:get(headers, R, #{}), undefined)
        end),
        meck:expect(cowboy_req, set_resp_header, fun(K, V, R) ->
            R#{resp_headers => (maps:get(resp_headers, R, #{}))#{K => V}}
        end),
        meck:expect(cowboy_req, reply, fun(S, R) -> R#{status => S} end),
        meck:expect(elib_response, error_with_status, fun(R, S, _, _) -> R#{status => S} end),
        meck:expect(auth_ds, verify_sign, fun(R, _) -> {stop, R#{status => 902}} end),
        meck:expect(token_ds, decrypt_web_token, fun
            (<<"synthetic-web">>, <<"tk">>) ->
                {ok, 123, 9999999999, <<"tk">>, <<"web-device">>, 2, 456};
            (_, _) ->
                {error, 401, <<"invalid">>, #{}}
        end),
        meck:expect(user_ds, find_by_id, fun(123, _) ->
            #{<<"status">> => 1, <<"account_type">> => 0}
        end),
        meck:expect(user_device_ds, is_active_web, fun(123, <<"web-device">>, 456) -> true end),
        meck:expect(auth_session_ds, current_epoch, fun(123) -> {ok, 2} end),
        Fun()
    after
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.

opts(Mode, Methods) ->
    #{surface => web_human, jwt_purpose => web_human, web_auth => Mode, jwt_methods => Methods}.

method_action_and_missing_metadata_reject_test() ->
    with_mocks(fun() ->
        O = opts(access, [<<"GET">>]),
        ?assertMatch(
            {stop, #{status := 405}},
            auth_middleware_api_v1:execute(#{method => <<"POST">>}, #{handler_opts => O})
        ),
        ?assertMatch(
            {stop, #{status := 401}}, auth_middleware_api_v1:execute(#{}, #{handler_opts => O})
        ),
        ?assertMatch(
            {stop, #{status := 401}},
            auth_middleware_api_v1:execute(#{}, #{handler_opts => maps:remove(jwt_purpose, O)})
        ),
        ?assertMatch(
            {stop, #{status := 401}},
            auth_middleware_api_v1:execute(#{}, #{handler_opts => O#{web_auth => unknown}})
        ),
        ?assertMatch(
            {stop, #{status := 400}},
            auth_middleware_api_v1:execute(#{qs => [{<<"action">>, <<"delete">>}]}, #{
                handler_opts => O#{action => history_backfill}
            })
        ),
        ?assertMatch(
            {stop, #{status := 902}},
            auth_middleware_api_v1:execute(#{headers => #{<<"cos">> => <<"web">>}}, #{
                handler_opts => #{}
            })
        )
    end).

direct_binding_is_method_specific_test() ->
    with_mocks(fun() ->
        O = (opts(access, [<<"GET">>, <<"POST">>]))#{
            web_actions => #{<<"status">> => [<<"GET">>], <<"register">> => [<<"POST">>]}
        },
        lists:foreach(
            fun({Action, Method, Status}) ->
                R = #{method => Method, bindings => #{action => Action}},
                ?assertMatch(
                    {stop, #{status := Status}},
                    auth_middleware_api_v1:execute(R, #{handler_opts => O})
                )
            end,
            [
                {<<"status">>, <<"POST">>, 405},
                {<<"register">>, <<"GET">>, 405},
                {<<"presign">>, <<"POST">>, 404}
            ]
        ),
        R = #{
            bindings => #{action => <<"status">>},
            headers => #{<<"authorization">> => <<"Bearer synthetic-web">>}
        },
        ?assertMatch(
            {ok, _, #{
                handler_opts := #{
                    current_uid := 123, current_did := <<"web-device">>, session_epoch := 2
                }
            }},
            auth_middleware_api_v1:execute(R, #{handler_opts => O})
        )
    end).

native_signature_and_web_token_boundaries_test() ->
    with_mocks(fun() ->
        meck:expect(auth_ds, verify_sign, fun(R, E) ->
            case maps:get(<<"sign">>, maps:get(headers, R, #{}), undefined) of
                <<"synthetic-valid">> -> {ok, R, E};
                _ -> {stop, R#{status => 902}}
            end
        end),
        meck:expect(token_ds, decrypt_token, fun(<<"synthetic-native">>) ->
            {ok, 123, 9999999999, <<"tk">>, <<"native-device">>, 2}
        end),
        meck:expect(user_device_ds, is_active_native, fun(123, <<"native-device">>) -> true end),
        meck:expect(auth_session_ds, revoked, fun(123, 2) -> false end),
        H = #{
            <<"authorization">> => <<"Bearer synthetic-native">>,
            <<"cos">> => <<"web">>,
            <<"did">> => <<"web-device">>
        },
        R = #{path => <<"/api/v1/msg/offline">>, headers => H},
        ?assertMatch(
            {stop, #{status := 902}}, auth_middleware_api_v1:execute(R, #{handler_opts => #{}})
        ),
        ?assertMatch(
            {ok, _, #{handler_opts := #{current_uid := 123, current_did := <<"native-device">>}}},
            auth_middleware_api_v1:execute(
                R#{headers => H#{<<"sign">> => <<"synthetic-valid">>}}, #{handler_opts => #{}}
            )
        ),
        ?assertMatch(
            {stop, #{status := 401}},
            auth_middleware_api_v1:execute(
                R#{path => <<"/api/v1/web/user/show">>}, #{
                    handler_opts => opts(access, [<<"GET">>])
                }
            )
        )
    end).

public_modes_only_pass_explicit_metadata_test() ->
    with_mocks(fun() ->
        lists:foreach(
            fun(Mode) ->
                ?assertMatch(
                    {ok, _, _},
                    auth_middleware_api_v1:execute(#{method => <<"POST">>}, #{
                        handler_opts => opts(Mode, [<<"POST">>])
                    })
                )
            end,
            [bootstrap, login, qr, refresh]
        )
    end).

cors_origin_headers_and_per_action_method_test() ->
    with_mocks(fun() ->
        meck:expect(config_ds, env, fun
            (cors_allowed_origins, _) -> [<<"https://web.example.test">>];
            (cors_allow_localhost, _) -> true;
            (_, Default) -> Default
        end),
        lists:foreach(
            fun(O) -> ?assertNot(cors_middleware:web_origin_allowed(O)) end,
            [
                undefined,
                <<>>,
                <<"null">>,
                <<"http://localhost:9191">>,
                <<"https://web.example.test.evil">>
            ]
        ),
        ?assert(cors_middleware:web_origin_allowed(<<"https://web.example.test">>)),
        O = (opts(access, [<<"GET">>, <<"POST">>]))#{web_actions => #{<<"status">> => [<<"GET">>]}},
        H = #{
            <<"origin">> => <<"https://web.example.test">>,
            <<"access-control-request-method">> => <<"GET">>,
            <<"access-control-request-headers">> => <<"Authorization, Content-Type, dNaMe">>
        },
        R = #{method => <<"OPTIONS">>, headers => H, bindings => #{action => <<"status">>}},
        {stop, #{status := 204, resp_headers := RH}} = cors_middleware:execute(R, #{
            handler_opts => O
        }),
        ?assertEqual(
            <<"https://web.example.test">>, maps:get(<<"access-control-allow-origin">>, RH)
        ),
        ?assertEqual(<<"Origin">>, maps:get(<<"vary">>, RH)),
        ?assertNot(maps:is_key(<<"access-control-allow-credentials">>, RH)),
        lists:foreach(
            fun(Headers) ->
                ?assertMatch(
                    {stop, #{status := 403}},
                    cors_middleware:execute(R#{headers => Headers}, #{handler_opts => O})
                )
            end,
            [
                H#{<<"access-control-request-method">> => <<"POST">>},
                H#{<<"access-control-request-headers">> => <<"cos">>},
                H#{<<"origin">> => <<"https://evil.example.test">>}
            ]
        ),
        ?assertMatch(
            {stop, #{status := 403}},
            cors_middleware:execute(
                R#{
                    method => <<"GET">>,
                    headers => #{<<"origin">> => <<"https://evil.example.test">>}
                },
                #{handler_opts => O}
            )
        ),
        ?assertMatch(
            {ok, _, _},
            cors_middleware:execute(R#{method => <<"GET">>, headers => #{}}, #{handler_opts => O})
        )
    end).

web_offline_and_ack_bind_trusted_device_test() ->
    with_mocks(fun() ->
        Modules = [messaging_logic, elib_param, msg_offline_cursor],
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        try
            meck:expect(elib_param, post, fun(R) -> maps:get(body, R, #{}) end),
            meck:expect(elib_param, int, fun(_, _, Default) -> {ok, Default} end),
            meck:expect(msg_offline_cursor, decode, fun(_) -> {ok, undefined} end),
            meck:expect(elib_response, success, fun(R, P) -> R#{payload => P} end),
            meck:expect(messaging_logic, offline_ack, fun(_, _, _, D) -> {ok, D} end),
            meck:expect(messaging_logic, offline, fun(_, _, _, _, _, D, _) -> D end),
            S = #{
                jwt_purpose => web_human, current_uid => 123, current_did => <<"trusted-device">>
            },
            ?assertMatch(#{payload := <<"trusted-device">>}, msg_handler:offline_ack(#{}, S)),
            ?assertMatch(#{payload := <<"trusted-device">>}, msg_handler:offline(#{}, S)),
            ?assertMatch(
                #{payload := <<"trusted-device">>},
                msg_handler:offline_ack(#{body => #{<<"did">> => <<"trusted-device">>}}, S)
            ),
            ?assertMatch(
                #{status := 400},
                msg_handler:offline_ack(#{body => #{<<"did">> => <<"other-device">>}}, S)
            ),
            ?assertMatch(
                #{status := 400}, msg_handler:offline(#{qs => [{<<"did">>, <<"other-device">>}]}, S)
            ),
            ?assertMatch(
                #{status := 400}, msg_handler:offline_ack(#{}, maps:remove(current_did, S))
            ),
            ?assertMatch(#{payload := <<>>}, msg_handler:offline_ack(#{}, #{current_uid => 123})),
            ?assertMatch(
                #{payload := <<"native-device">>},
                msg_handler:offline(#{qs => [{<<"did">>, <<"native-device">>}]}, #{
                    current_uid => 123
                })
            ),
            ?assertEqual(3, meck:num_calls(messaging_logic, offline_ack, '_')),
            ?assertEqual(2, meck:num_calls(messaging_logic, offline, '_'))
        after
            lists:foreach(fun meck:unload/1, lists:reverse(Modules))
        end
    end).

organizations_mine_is_private_get_only_web_alias_test() ->
    with_mocks(fun() ->
        [{_, Routes}] = imboy_router:get_routes(),
        Path = "/api/v1/web/organizations/mine",
        {Path, organization_handler, O} = lists:keyfind(Path, 1, Routes),
        ?assertMatch(
            #{
                action := mine,
                surface := web_human,
                jwt_purpose := web_human,
                web_auth := access,
                jwt_methods := [<<"GET">>]
            },
            O
        ),
        ?assertNot(lists:member(list_to_binary(Path), imboy_router:open())),
        ?assertNot(lists:member(list_to_binary(Path), imboy_router:option())),
        ?assertNot(lists:keymember("/api/v1/web/organizations", 1, Routes)),
        ?assertNot(lists:keymember("/api/v1/web/organizations/:organization_id", 1, Routes)),
        ?assertMatch(
            {stop, #{status := 401}},
            auth_middleware_api_v1:execute(#{path => list_to_binary(Path)}, #{handler_opts => O})
        ),
        H = #{<<"authorization">> => <<"Bearer synthetic-web">>},
        R = #{path => list_to_binary(Path), headers => H},
        ?assertMatch(
            {ok, _, #{handler_opts := #{current_uid := 123, current_did := <<"web-device">>}}},
            auth_middleware_api_v1:execute(R, #{handler_opts => O})
        ),
        lists:foreach(
            fun(Method) ->
                ?assertMatch(
                    {stop, #{status := 405}},
                    auth_middleware_api_v1:execute(
                        R#{method => Method}, #{handler_opts => O}
                    )
                )
            end,
            [<<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"HEAD">>]
        ),
        ?assertEqual(0, meck:num_calls(auth_ds, verify_sign, '_'))
    end).

route_aliases_keep_exact_existing_handlers_and_features_test() ->
    with_mocks(fun() ->
        [{_, All}] = imboy_router:get_routes(),
        Web = [
            {P, H, O}
         || {P, H, O} <- All, is_map(O), maps:get(surface, O, undefined) =:= web_human
        ],
        Expected = expected_routes(),
        Paths = [
            "/api/v1/web/" ++ W
         || {S, W, _, _, _, _} <- Expected, lists:keymember("/api/v1/" ++ S, 1, All)
        ],
        ?assertEqual(
            lists:sort(["/api/v1/web/user/me" | Paths]), lists:sort([P || {P, _, _} <- Web])
        ),
        lists:foreach(
            fun({Source, WebPath, Handler, Action, Methods, Mode}) ->
                case lists:keyfind("/api/v1/" ++ Source, 1, All) of
                    false ->
                        ?assertNot(lists:keymember("/api/v1/web/" ++ WebPath, 1, Web));
                    {_, Handler, SourceOpts} ->
                        {_, Handler, Opts} = lists:keyfind("/api/v1/web/" ++ WebPath, 1, Web),
                        ?assertEqual(Action, maps:get(action, Opts, undefined)),
                        ?assertEqual(web_human, maps:get(jwt_purpose, Opts)),
                        ?assertEqual(Methods, maps:get(jwt_methods, Opts)),
                        ?assertEqual(Mode, maps:get(web_auth, Opts)),
                        ?assertEqual(
                            maps:get(required_feature, SourceOpts, undefined),
                            maps:get(required_feature, Opts, undefined)
                        )
                end
            end,
            Expected
        ),
        {_, _, Direct} = lists:keyfind("/api/v1/web/attachment/direct/:action", 1, Web),
        Actions = maps:get(web_actions, Direct),
        ?assertEqual(
            lists:sort([
                <<"register">>,
                <<"declare">>,
                <<"grant">>,
                <<"renew">>,
                <<"finalize">>,
                <<"status">>
            ]),
            lists:sort(maps:keys(Actions))
        ),
        lists:foreach(
            fun(A) -> ?assertEqual([<<"POST">>], maps:get(A, Actions)) end,
            [<<"register">>, <<"declare">>, <<"grant">>, <<"renew">>, <<"finalize">>]
        ),
        ?assertEqual([<<"GET">>], maps:get(<<"status">>, Actions)),
        ?assertEqual([], [
            P
         || P <- imboy_router:open() ++ imboy_router:option(),
            binary:match(P, <<"/web/">>) =/= nomatch
        ])
    end).

%% JNY-06：群创建/加入族 web 别名——私有 JWT 面、逐方法门、不入 open/option、
%% 不触发 native 902 签名门（auth_ds:verify_sign 零调用）。
group_creation_family_web_aliases_test() ->
    with_mocks(fun() ->
        [{_, All}] = imboy_router:get_routes(),
        Family = [
            {"/api/v1/web/group/add", group_handler, add, [<<"POST">>]},
            {"/api/v1/web/group/face2face", group_handler, face2face, [<<"GET">>]},
            {"/api/v1/web/group/face2face_save", group_handler, face2face_save, [<<"POST">>]},
            {"/api/v1/web/group_member/join", group_member_handler, join, [<<"POST">>]}
        ],
        lists:foreach(
            fun({Path, Handler, Action, Methods}) ->
                {Path, Handler, Opts} = lists:keyfind(Path, 1, All),
                ?assertEqual(Action, maps:get(action, Opts)),
                ?assertEqual(web_human, maps:get(surface, Opts)),
                ?assertEqual(web_human, maps:get(jwt_purpose, Opts)),
                ?assertEqual(access, maps:get(web_auth, Opts)),
                ?assertEqual(Methods, maps:get(jwt_methods, Opts)),
                ?assertNot(lists:member(list_to_binary(Path), imboy_router:open())),
                ?assertNot(lists:member(list_to_binary(Path), imboy_router:option())),
                Allowed = hd(Methods),
                %% 未带 web token 一律 401（绝不落入 native 902 签名分支）
                ?assertMatch(
                    {stop, #{status := 401}},
                    auth_middleware_api_v1:execute(
                        #{method => Allowed, path => list_to_binary(Path)},
                        #{handler_opts => Opts}
                    )
                ),
                %% 逐方法门：GET-only 别名拒 POST，POST-only 别名拒 GET
                lists:foreach(
                    fun(M) ->
                        ?assertMatch(
                            {stop, #{status := 405}},
                            auth_middleware_api_v1:execute(
                                #{method => M, path => list_to_binary(Path)},
                                #{handler_opts => Opts}
                            )
                        )
                    end,
                    [M || M <- [<<"GET">>, <<"POST">>], not lists:member(M, Methods)]
                ),
                %% 带 web token 放行且零签名校验
                H = #{<<"authorization">> => <<"Bearer synthetic-web">>},
                ?assertMatch(
                    {ok, _, #{handler_opts := #{current_uid := 123}}},
                    auth_middleware_api_v1:execute(
                        #{method => Allowed, path => list_to_binary(Path), headers => H},
                        #{handler_opts => Opts}
                    )
                )
            end,
            Family
        ),
        ?assertEqual(0, meck:num_calls(auth_ds, verify_sign, '_'))
    end).

%% 从冻结契约显式抄录；不从实现函数生成期望集合。
expected_routes() ->
    [
        {"init", "bootstrap", index_handler, web_bootstrap, [<<"GET">>], bootstrap},
        {"passport/login", "passport/login", passport_handler, login, [<<"POST">>], login},
        {"refreshtoken", "refreshtoken", passport_handler, refreshtoken, [<<"POST">>], refresh},
        {"user_device/delete", "user_device/delete", user_device_handler, delete, [<<"POST">>],
            access},
        {"passport/qr_login/create", "passport/qr_login/create", qr_login_handler, create,
            [<<"POST">>], qr},
        {"passport/qr_login/status", "passport/qr_login/status", qr_login_handler, status,
            [<<"GET">>], qr},
        {"passport/qr_login/cancel", "passport/qr_login/cancel", qr_login_handler, cancel,
            [<<"POST">>], qr},
        {"passport/qr_login/subscribe", "passport/qr_login/subscribe", qr_login_sse_handler,
            undefined, [<<"GET">>], qr},
        {"msg/offline", "msg/offline", msg_handler, offline, [<<"GET">>], access},
        {"msg/offline_ack", "msg/offline_ack", msg_handler, offline_ack, [<<"POST">>], access},
        {"msg/history", "msg/history", msg_handler, history, [<<"GET">>], access},
        {"msg", "msg", msg_handler, history_backfill, [<<"GET">>], access},
        {"msg/result", "msg/result", msg_handler, message_result, [<<"GET">>], access},
        %% 个人消息历史域 Web N1 面（MH-01 §10.1 合同 + MH-02 路由落地；
        %% key-package 读端点 GET 依 mh04 addendum/FL-04 裁决，写面 Web 惯例 POST）
        {"personal-history/status", "personal-history/status", personal_history_handler, status,
            [<<"GET">>], access},
        {"personal-history/opt-in", "personal-history/opt-in", personal_history_handler, opt_in,
            [<<"POST">>], access},
        {"personal-history/upload", "personal-history/upload", personal_history_handler, upload,
            [<<"POST">>], access},
        {"personal-history/changes", "personal-history/changes", personal_history_handler, changes,
            [<<"GET">>], access},
        {"personal-history/ack-cursor", "personal-history/ack-cursor", personal_history_handler,
            ack_cursor, [<<"POST">>], access},
        {"personal-history/page", "personal-history/page", personal_history_handler, page,
            [<<"GET">>], access},
        {"personal-history/key-package", "personal-history/key-package", personal_history_handler,
            key_package, [<<"GET">>, <<"POST">>], access},
        {"personal-history/disable", "personal-history/disable", personal_history_handler, disable,
            [<<"POST">>], access},
        {"personal-history/delete-status", "personal-history/delete-status",
            personal_history_handler, delete_status, [<<"GET">>], access},
        {"conversation/mine", "conversation/mine", conversation_handler, mine, [<<"GET">>], access},
        {"conversation/pin", "conversation/pin", conversation_handler, pin_conversation,
            [<<"POST">>], access},
        {"conversation/unpin", "conversation/unpin", conversation_handler, unpin_conversation,
            [<<"POST">>], access},
        {"conversation/delete", "conversation/delete", conversation_handler, delete_conversation,
            [<<"POST">>], access},
        {"conversation/restore", "conversation/restore", conversation_handler, restore_conversation,
            [<<"POST">>], access},
        {"conversation/pinned", "conversation/pinned", conversation_handler, pinned_list,
            [<<"GET">>], access},
        {"conversation/manual_pending", "conversation/manual_pending", conversation_handler,
            set_manual_pending, [<<"POST">>], access},
        {"conversation/manual_pending/list", "conversation/manual_pending/list",
            conversation_handler, manual_pending_list, [<<"GET">>], access},
        {"friend/list", "friend/list", friend_handler, list, [<<"GET">>], access},
        {"friend/add", "friend/add", friend_handler, add_friend, [<<"POST">>], access},
        {"friend/confirm", "friend/confirm", friend_handler, confirm, [<<"POST">>], access},
        {"fts/recently_user", "fts/recently_user", fts_handler, recently_user, [<<"GET">>], access},
        {"user/show", "user/show", user_handler, show, [<<"GET">>], access},
        {"user/setting", "user/setting", user_handler, setting, [<<"POST">>], access},
        {"group/page", "group/page", group_handler, page, [<<"GET">>], access},
        {"group/detail", "group/detail", group_handler, detail, [<<"GET">>], access},
        {"group/add", "group/add", group_handler, add, [<<"POST">>], access},
        {"group/face2face", "group/face2face", group_handler, face2face, [<<"GET">>], access},
        {"group/face2face_save", "group/face2face_save", group_handler, face2face_save,
            [<<"POST">>], access},
        {"group_member/join", "group_member/join", group_member_handler, join, [<<"POST">>],
            access},
        {"group_member/page", "group_member/page", group_member_handler, page, [<<"GET">>], access},
        {"group_notice/page", "group_notice/page", group_notice_handler, page, [<<"GET">>], access},
        {"attachment/view_url", "attachment/view_url", attach_handler, view_url, [<<"GET">>],
            access},
        {"organizations/mine", "organizations/mine", organization_handler, mine, [<<"GET">>],
            access},
        {"workspaces/mine", "workspaces/mine", workspace_handler, mine, [<<"GET">>], access},
        {"workspaces/:workspace_id", "workspaces/:workspace_id", workspace_handler, show,
            [<<"GET">>], access},
        {"workspaces/:workspace_id/branding", "workspaces/:workspace_id/branding",
            workspace_handler, branding, [<<"GET">>], access},
        {"workspaces/:workspace_id/channels", "workspaces/:workspace_id/channels",
            workspace_handler, channel_list, [<<"GET">>], access},
        {"workspaces/:workspace_id/groups", "workspaces/:workspace_id/groups", workspace_handler,
            group_list, [<<"GET">>], access},
        {"channels/subscribed", "channels/subscribed", channel_handler, subscribed, [<<"GET">>],
            access},
        {"channels/unread/summary", "channels/unread/summary", channel_handler, unread_summary,
            [<<"GET">>], access},
        {"channel/:channel_id", "channel/:channel_id", channel_handler, show, [<<"GET">>], access},
        {"channel/:channel_id/messages", "channel/:channel_id/messages", channel_handler, messages,
            [<<"GET">>], access},
        {"ws", "ws", websocket_handler, undefined, [<<"GET">>], ws},
        {"attachment/direct/:action", "attachment/direct/:action", attach_direct_handler, undefined,
            [<<"GET">>, <<"POST">>], access}
    ].

web_self_profile_route_is_private_get_only_test() ->
    with_mocks(fun() ->
        [{_, All}] = imboy_router:get_routes(),
        Path = "/api/v1/web/user/me",
        {_, user_handler, Opts} = lists:keyfind(Path, 1, All),
        ?assertMatch(
            #{
                action := web_me,
                surface := web_human,
                jwt_purpose := web_human,
                web_auth := access,
                jwt_methods := [<<"GET">>]
            },
            Opts
        ),
        ?assertNot(lists:keymember("/api/v1/user/me", 1, All)),
        ?assertNot(lists:member(list_to_binary(Path), imboy_router:open())),
        ?assertMatch(
            {stop, #{status := 401}},
            auth_middleware_api_v1:execute(
                #{path => list_to_binary(Path)}, #{handler_opts => Opts}
            )
        ),
        lists:foreach(
            fun(Method) ->
                ?assertMatch(
                    {stop, #{status := 405}},
                    auth_middleware_api_v1:execute(
                        #{path => list_to_binary(Path), method => Method}, #{handler_opts => Opts}
                    )
                )
            end,
            [<<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>]
        )
    end).
