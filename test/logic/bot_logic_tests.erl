-module(bot_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% Guard undeclared DB/entity calls in a fresh peer without application startup.
-define(PURE_MOCKS(Configs, Test),
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() -> pure_mock_fixture:with_mocks(Configs, Test) end)
    end}
).


%% ===================================================================
%% register
%% ===================================================================

register_creates_bot_with_tokens_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [
                {'find_by_id', 2, fun(100, <<"id, status">>) ->
                    #{<<"id">> => 100}
                end}
            ]},
            {bot_ds, [
                {'create', 1, fun(Data) ->
                    ?assert(maps:is_key(api_token, Data)),
                    ?assert(maps:is_key(verify_token, Data)),
                    ?assertEqual(<<"TestBot">>, maps:get(name, Data)),
                    {ok, #{<<"user_id">> => 1}}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:register(#{
                name => <<"TestBot">>,
                username => <<"testbot">>,
                owner_uid => 100
            }),
            ?assertEqual(1, maps:get(<<"user_id">>, Result)),
            ?assert(maps:is_key(<<"api_token">>, Result)),
            ?assert(maps:is_key(<<"verify_token">>, Result))
        end
    ).

register_returns_error_when_owner_not_found_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [
                {'find_by_id', 2, fun(999, <<"id, status">>) ->
                    #{}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"用户不存在或已停用"/utf8>>},
                bot_logic:register(#{
                    name => <<"TestBot">>,
                    username => <<"testbot">>,
                    owner_uid => 999
                })
            )
        end
    ).

%% ===================================================================
%% get
%% ===================================================================

get_returns_bot_without_sensitive_fields_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{
                        <<"user_id">> => 1,
                        <<"name">> => <<"TestBot">>,
                        <<"api_token">> => <<"secret">>,
                        <<"verify_token">> => <<"secret2">>,
                        <<"status">> => 1
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, Bot} = bot_logic:get(1),
            ?assertEqual(1, maps:get(<<"user_id">>, Bot)),
            ?assertEqual(<<"TestBot">>, maps:get(<<"name">>, Bot)),
            ?assertNot(maps:is_key(<<"api_token">>, Bot)),
            ?assertNot(maps:is_key(<<"verify_token">>, Bot))
        end
    ).

get_returns_error_on_notfound_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(999) -> {error, notfound} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"Bot 不存在"/utf8>>},
                bot_logic:get(999)
            )
        end
    ).

%% ===================================================================
%% update（仅属主）
%% ===================================================================

update_modifies_bot_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"owner_uid">> => 100, <<"name">> => <<"OldName">>}}
                end},
                {'update', 2, fun(1, #{name := <<"NewName">>}) ->
                    {ok, [#{<<"user_id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:update(1, #{name => <<"NewName">>}, 100),
            ?assertEqual(1, maps:get(<<"user_id">>, Result))
        end
    ).

update_rejects_non_owner_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"owner_uid">> => 100}}
                end},
                {'update', 2, fun(_, _) -> exit(update_should_not_be_called) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"无权操作此 Bot"/utf8>>},
                bot_logic:update(1, #{name => <<"Hijack">>}, 999)
            )
        end
    ).

update_returns_error_on_notfound_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(999) -> {error, notfound} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"Bot 不存在"/utf8>>},
                bot_logic:update(999, #{name => <<"NewName">>}, 100)
            )
        end
    ).

%% ===================================================================
%% set_status（仅属主）
%% ===================================================================

set_status_enables_bot_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"owner_uid">> => 100, <<"status">> => 0}}
                end},
                {'set_status', 2, fun(1, 1) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:set_status(1, 1, 100),
            ?assertEqual(1, maps:get(<<"user_id">>, Result)),
            ?assertEqual(1, maps:get(<<"status">>, Result))
        end
    ).

set_status_disables_bot_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"owner_uid">> => 100, <<"status">> => 1}}
                end},
                {'set_status', 2, fun(1, 0) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:set_status(1, 0, 100),
            ?assertEqual(1, maps:get(<<"user_id">>, Result)),
            ?assertEqual(0, maps:get(<<"status">>, Result))
        end
    ).

set_status_rejects_non_owner_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"owner_uid">> => 100, <<"status">> => 1}}
                end},
                {'set_status', 2, fun(_, _) -> exit(set_status_should_not_be_called) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"无权操作此 Bot"/utf8>>},
                bot_logic:set_status(1, 0, 999)
            )
        end
    ).

set_status_returns_error_on_notfound_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(999) -> {error, notfound} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"Bot 不存在"/utf8>>},
                bot_logic:set_status(999, 1, 100)
            )
        end
    ).

%% ===================================================================
%% list_mine
%% ===================================================================

list_mine_returns_owner_bots_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'page_by_owner', 3, fun(1, 20, 100) ->
                    {ok, #{
                        total => 2,
                        page => 1,
                        size => 20,
                        list => [
                            #{<<"user_id">> => 1, <<"name">> => <<"Bot1">>},
                            #{<<"user_id">> => 2, <<"name">> => <<"Bot2">>}
                        ]
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:list_mine(100, 1),
            ?assertEqual(2, maps:get(total, Result))
        end
    ).

%% ===================================================================
%% search
%% ===================================================================

search_returns_public_bots_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'search', 3, fun(<<"test">>, 1, 20) ->
                    {ok, #{
                        total => 1,
                        page => 1,
                        size => 20,
                        list => [
                            #{<<"user_id">> => 1, <<"name">> => <<"TestBot">>}
                        ]
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:search(<<"test">>, 1, 20),
            ?assertEqual(1, maps:get(total, Result))
        end
    ).

%% ===================================================================
%% send_message
%% ===================================================================

send_message_sends_c2c_message_test_() ->
    ?WITH_MECKS(
        [
            {elib_uuid, [{'gen_v7', 0, fun() -> <<"018f0000-0000-7000-8000-000000012345">> end}]},
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 1}}
                end}
            ]},
            {msg_c2c_logic, [
                {'c2c', 3, fun(_MsgId, 1, Data) ->
                    ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Data)),
                    ok
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:send_message(1, 2, #{
                <<"msg_type">> => <<"text">>,
                <<"payload">> => #{<<"text">> => <<"Hello">>}
            }),
            ?assert(maps:is_key(<<"msg_id">>, Result))
        end
    ).

send_message_returns_error_when_bot_disabled_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 0}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"Bot 已停用"/utf8>>},
                bot_logic:send_message(1, 2, #{<<"msg_type">> => <<"text">>})
            )
        end
    ).

send_message_returns_error_when_bot_not_found_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(999) -> {error, notfound} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"Bot 不存在"/utf8>>},
                bot_logic:send_message(999, 2, #{<<"msg_type">> => <<"text">>})
            )
        end
    ).

send_group_message_requires_real_c2g_ack_test_() ->
    ?WITH_MECKS(
        [
            {elib_uuid, [{'gen_v7', 0, fun() -> <<"018f0000-0000-7000-8000-000000067890">> end}]},
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 1}}
                end}
            ]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(<<"018f0000-0000-7000-8000-000000067890">> = MsgId, 1, Data) ->
                    ?assertEqual(<<"2">>, maps:get(<<"to">>, Data)),
                    ?assertEqual(
                        <<"hello">>, maps:get(<<"text">>, maps:get(<<"payload">>, Data))
                    ),
                    self() !
                        {reply, #{
                            <<"id">> => MsgId,
                            <<"type">> => <<"C2G_SERVER_ACK">>
                        }},
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{<<"msg_id">> => <<"018f0000-0000-7000-8000-000000067890">>}},
                bot_logic:send_group_message(1, 2, <<"hello">>)
            )
        end
    ).

send_group_message_rejects_c2g_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_uuid, [{'gen_v7', 0, fun() -> <<"018f0000-0000-7000-8000-000000067891">> end}]},
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 1}}
                end}
            ]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(<<"018f0000-0000-7000-8000-000000067891">> = MsgId, 1, _Data) ->
                    self() !
                        {reply, #{
                            <<"id">> => MsgId,
                            <<"type">> => <<"C2G_ERROR">>
                        }},
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"消息发送失败"/utf8>>},
                bot_logic:send_group_message(1, 2, <<"hello">>)
            )
        end
    ).

%% 普通 IM 业务 ID 不借用实体 TSID 游标，C2C/C2G 共用同一唯一性空间。
%% Business message IDs remain distinct even when entity cursors return the same value.
server_generated_message_ids_are_uuid_v7_across_types_test_() ->
    ?PURE_MOCKS(
        [
            {elib_tsid, [{'generate', 1, fun(_) -> 42 end}]},
            {bot_repo, [{'find', 1, fun(_) -> {ok, #{<<"status">> => 1}} end}]},
            {msg_c2c_logic, [{'c2c', 3, fun(_, _, _) -> ok end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(MsgId, _, _) ->
                self() ! {reply, #{<<"id">> => MsgId, <<"type">> => <<"C2G_SERVER_ACK">>}},
                ok
            end}]}
        ],
        fun() ->
            Ids = lists:flatmap(fun(_) ->
                {ok, #{<<"msg_id">> := C2C}} = bot_logic:send_message(1, 2, #{}),
                {ok, #{<<"msg_id">> := C2G}} = bot_logic:send_group_message(1, 3, <<"hello">>),
                [C2C, C2G]
            end, lists:seq(1, 32)),
            ?assertEqual(64, length(lists:usort(Ids))),
            lists:foreach(fun(Id) ->
                ?assertEqual(36, byte_size(Id)),
                ?assertMatch({match, _}, re:run(Id,
                    <<"^[0-9a-f]{8}-[0-9a-f]{4}-7[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>))
            end, Ids),
            ?assertEqual(0, meck:num_calls(elib_tsid, generate, 1))
        end
    ).

specified_message_ids_and_e2ee_bindings_are_preserved_test_() ->
    ?PURE_MOCKS(
        [
            {elib_uuid, [{'gen_v7', 0, fun() -> error(unexpected_message_id_replacement) end}]},
            {bot_repo, [{'find', 1, fun(_) -> {ok, #{<<"status">> => 1}} end}]},
            {msg_c2c_logic, [{'c2c', 3, fun(_, _, _) -> ok end}]}
        ],
        fun() ->
            E2EE = #{<<"sender_did">> => <<"test-device">>, <<"e2ee">> => true},
            Data = #{<<"msg_type">> => <<"e2ee">>,
                <<"payload">> => <<"opaque-ciphertext">>, <<"e2ee">> => E2EE},
            Ids = [<<"63825930240000000">>, <<"d2q9i6rvp1dc73i5hhvg">>,
                <<"018f0000-0000-7000-8000-000000000001">>],
            lists:foreach(fun(Id) ->
                %% Repeating the same request must preserve its original identity.
                lists:foreach(fun(_) ->
                    ?assertEqual({ok, #{<<"msg_id">> => Id}},
                        bot_logic:send_message(Id, 1, 2, Data))
                end, [first, retry]),
                ?assertEqual(2, meck:num_calls(msg_c2c_logic, c2c, [Id, 1, '_']))
            end, Ids),
            lists:foreach(fun({_, {_, c2c, [_, 1, Sent]}, _}) ->
                ?assertEqual(E2EE, maps:get(<<"e2ee">>, Sent)),
                ?assertEqual(<<"opaque-ciphertext">>, maps:get(<<"payload">>, Sent))
            end, meck:history(msg_c2c_logic)),
            ?assertEqual(0, meck:num_calls(elib_uuid, gen_v7, 0))
        end
    ).
