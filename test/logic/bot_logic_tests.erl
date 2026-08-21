-module(bot_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

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
%% update
%% ===================================================================

update_modifies_bot_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"name">> => <<"OldName">>}}
                end},
                {'update', 2, fun(1, #{name := <<"NewName">>}) ->
                    {ok, [#{<<"user_id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:update(1, #{name => <<"NewName">>}),
            ?assertEqual(1, maps:get(<<"user_id">>, Result))
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
                bot_logic:update(999, #{name => <<"NewName">>})
            )
        end
    ).

%% ===================================================================
%% set_status
%% ===================================================================

set_status_enables_bot_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 0}}
                end},
                {'set_status', 2, fun(1, 1) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:set_status(1, 1),
            ?assertEqual(1, maps:get(<<"user_id">>, Result)),
            ?assertEqual(1, maps:get(<<"status">>, Result))
        end
    ).

set_status_disables_bot_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'find', 1, fun(1) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 1}}
                end},
                {'set_status', 2, fun(1, 0) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            {ok, Result} = bot_logic:set_status(1, 0),
            ?assertEqual(1, maps:get(<<"user_id">>, Result)),
            ?assertEqual(0, maps:get(<<"status">>, Result))
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
                bot_logic:set_status(999, 1)
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
            {elib_tsid, [
                {'generate', 0, fun() -> 12345 end}
            ]},
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
