-module(friend_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_logic 模块的 EUnit 测试
%%%
%%% 目标：验证好友业务逻辑功能
%%% 覆盖：添加好友、确认好友、删除好友、移动分组、获取信息
%%%===================================================================

%% ===================================================================
%% add_friend/4 测试
%% ===================================================================

add_friend_with_undefined_to_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        To = undefined,
        Payload = #{<<"msg">> => <<"test">>},
        CreatedAt = 1234567890,
        Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
        ?assertEqual({error, <<"Parameter error">>, <<"to">>}, Result)
    end).

add_friend_with_undefined_payload_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        To = <<"test_to_2">>,
        Payload = undefined,
        CreatedAt = 1234567890,
        Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
        ?assertEqual({error, <<"Parameter error">>, <<"payload">>}, Result)
    end).

add_friend_with_undefined_created_at_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        To = <<"test_to_2">>,
        Payload = #{<<"msg">> => <<"test">>},
        CreatedAt = undefined,
        Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
        ?assertEqual({error, <<"Parameter error">>, <<"created_at">>}, Result)
    end).

add_friend_success_test_() ->
    ?WITH_MECK(
        elib_dt,
        [
            {'to_rfc3339', 1, fun(_Timestamp) -> <<"2023-01-01T00:00:00Z">> end},
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ],
        fun() ->
            ?WITH_MECK(
                msg_s2c_ds,
                [
                    {'write_msg', 8, fun(
                        _CreatedAt, _MsgId, _Payload, _FromId, _ToId, _NowTs, _Action, _E2EE
                    ) ->
                        ok
                    end}
                ],
                fun() ->
                    ?WITH_MECK(
                        message_ds,
                        [
                            {'assemble_msg', 8, fun(
                                _Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext
                            ) ->
                                #{<<"type">> => <<"S2C">>, <<"msg_id">> => <<"test_msg_123">>}
                            end}
                        ],
                        fun() ->
                            ?WITH_MECK(
                                message_ds,
                                [
                                    {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                                ],
                                fun() ->
                                    ?WITH_MECK(
                                        elib_retry_config,
                                        [
                                            {'intervals', 1, fun(<<"s2c">>) ->
                                                [2000, 5000, 7000, 11000]
                                            end}
                                        ],
                                        fun() ->
                                            ?WITH_MECK(
                                                friend_ds,
                                                [
                                                    {'pending_status', 2, fun(_From, _To) ->
                                                        none
                                                    end},
                                                    {'insert_pending', 4, fun(
                                                        _From, _To, _Setting, _NowTs
                                                    ) ->
                                                        ok
                                                    end}
                                                ],
                                                fun() ->
                                                    CurrentUid = 1,
                                                    To = <<"test_to_2">>,
                                                    Payload = #{<<"msg">> => <<"请加我好友"/utf8>>},
                                                    CreatedAt = 1640995200,

                                                    Result = friend_logic:add_friend(
                                                        CurrentUid, To, Payload, CreatedAt
                                                    ),
                                                    ?assertEqual(ok, Result)
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

add_friend_with_map_payload_test_() ->
    ?WITH_MECK(
        elib_dt,
        [
            {'to_rfc3339', 1, fun(_Timestamp) -> <<"2023-01-01T00:00:00Z">> end},
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ],
        fun() ->
            ?WITH_MECK(
                msg_s2c_ds,
                [
                    {'write_msg', 8, fun(
                        _CreatedAt, _MsgId, _Payload, _FromId, _ToId, _NowTs, _Action, _E2EE
                    ) ->
                        ok
                    end}
                ],
                fun() ->
                    ?WITH_MECK(
                        message_ds,
                        [
                            {'assemble_msg', 8, fun(
                                _Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext
                            ) ->
                                #{<<"type">> => <<"S2C">>}
                            end}
                        ],
                        fun() ->
                            ?WITH_MECK(
                                message_ds,
                                [
                                    {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                                ],
                                fun() ->
                                    ?WITH_MECK(
                                        elib_retry_config,
                                        [
                                            {'intervals', 1, fun(_) -> [2000] end}
                                        ],
                                        fun() ->
                                            ?WITH_MECK(
                                                friend_ds,
                                                [
                                                    {'pending_status', 2, fun(_From, _To) ->
                                                        none
                                                    end},
                                                    {'insert_pending', 4, fun(
                                                        _From, _To, _Setting, _NowTs
                                                    ) ->
                                                        ok
                                                    end}
                                                ],
                                                fun() ->
                                                    CurrentUid = 1,
                                                    To = <<"test_to_2">>,
                                                    Payload = #{
                                                        <<"msg">> => <<"你好"/utf8>>,
                                                        <<"source">> => <<"search">>
                                                    },
                                                    CreatedAt = <<"2023-01-01T00:00:00Z">>,

                                                    Result = friend_logic:add_friend(
                                                        CurrentUid, To, Payload, CreatedAt
                                                    ),
                                                    ?assertEqual(ok, Result)
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

%% ===================================================================
%% add_friend/4 state-gating 测试（T3.4：委托 friend_agg 守护申请不变量）
%% ===================================================================

%% 对方已是好友 → friend_agg:request 拒绝 already_friends，不发送申请消息
add_friend_already_friends_returns_error_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'pending_status', 2, fun(_From, _To) -> friends end}
        ],
        fun() ->
            CurrentUid = 1,
            To = <<"2">>,
            Payload = #{<<"msg">> => <<"请加我好友"/utf8>>},
            CreatedAt = 1640995200,
            Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
            ?assertMatch({error, <<"already_friends">>, _}, Result)
        end
    ).

%% 已拉黑对方 → friend_agg:request 拒绝 blocked，不发送申请消息
add_friend_when_blocked_returns_error_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'pending_status', 2, fun(_From, _To) -> blocked end}
        ],
        fun() ->
            CurrentUid = 1,
            To = <<"2">>,
            Payload = #{<<"msg">> => <<"请加我好友"/utf8>>},
            CreatedAt = 1640995200,
            Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
            ?assertMatch({error, <<"blocked">>, _}, Result)
        end
    ).

%% 已发送过申请（pending）→ friend_agg:request 拒绝 already_requested（T3.4 余项）
add_friend_already_requested_returns_error_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'pending_status', 2, fun(_From, _To) -> pending end}
        ],
        fun() ->
            CurrentUid = 1,
            To = <<"2">>,
            Payload = #{<<"msg">> => <<"请加我好友"/utf8>>},
            CreatedAt = 1640995200,
            Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
            ?assertMatch({error, <<"already_requested">>, _}, Result)
        end
    ).

%% ===================================================================
%% confirm_friend/4 测试
%% ===================================================================

confirm_friend_with_undefined_from_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        From = undefined,
        To = <<"test_to_2">>,
        Payload = <<"{}">>,
        Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
        ?assertEqual({error, <<"Parameter error">>, <<"from">>}, Result)
    end).

confirm_friend_with_undefined_to_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        From = <<"test_from_2">>,
        To = undefined,
        Payload = <<"{}">>,
        Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
        ?assertEqual({error, <<"Parameter error">>, <<"to">>}, Result)
    end).

confirm_friend_with_undefined_payload_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        From = <<"test_from_2">>,
        To = <<"test_to_2">>,
        Payload = undefined,
        Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
        ?assertEqual({error, <<"Parameter error">>, <<"payload">>}, Result)
    end).

confirm_friend_success_test_() ->
    ?WITH_MECK(
        elib_dt,
        [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ],
        fun() ->
            ?WITH_MECK(
                jsone,
                [
                    {'decode', 2, fun(_Payload, _Opts) ->
                        {ok, #{
                            <<"from">> => #{<<"remark">> => <<"好友A"/utf8>>, <<"tag">> => <<>>},
                            <<"to">> => #{<<"remark">> => <<"好友B"/utf8>>, <<"tag">> => <<>>},
                            <<"source">> => <<"search">>
                        }}
                    end}
                ],
                fun() ->
                    ?WITH_MECK(
                        friend_ds,
                        [
                            {'pending_status', 2, fun(_From, _To) -> pending end},
                            {'is_friend', 2, fun(_FromId, _ToId) -> false end},
                            {'confirm_friend', 7, fun(
                                _IsFriend, _FromId, _ToId, _Remark, _Setting, _Tag, _NowTs
                            ) ->
                                ok
                            end}
                        ],
                        fun() ->
                            ?WITH_MECK(
                                msg_s2c_ds,
                                [
                                    {'write_msg', 8, fun(
                                        _CreatedAt,
                                        _MsgId,
                                        _Payload,
                                        _FromId,
                                        _ToId,
                                        _NowTs,
                                        _Action,
                                        _E2EE
                                    ) ->
                                        ok
                                    end}
                                ],
                                fun() ->
                                    ?WITH_MECK(
                                        message_ds,
                                        [
                                            {'assemble_msg', 8, fun(
                                                _Type,
                                                _From,
                                                _To,
                                                _Payload,
                                                _MsgId,
                                                _Body,
                                                _Action,
                                                _Ext
                                            ) ->
                                                #{<<"type">> => <<"S2C">>}
                                            end}
                                        ],
                                        fun() ->
                                            ?WITH_MECK(
                                                message_ds,
                                                [
                                                    {'send_next', 3, fun(
                                                        _ToId, _MsgId, _Message, _MsLi
                                                    ) ->
                                                        ok
                                                    end}
                                                ],
                                                fun() ->
                                                    ?WITH_MECK(
                                                        elib_retry_config,
                                                        [
                                                            {'intervals', 1, fun(_) -> [2000] end}
                                                        ],
                                                        fun() ->
                                                            ?WITH_MECK(
                                                                imboy_cache,
                                                                [
                                                                    {'flush', 1, fun(_Key) ->
                                                                        ok
                                                                    end}
                                                                ],
                                                                fun() ->
                                                                    CurrentUid = 200,
                                                                    From = <<"test_from_2">>,
                                                                    To = <<"test_to_2">>,
                                                                    Payload = <<"{}">>,

                                                                    Result = friend_logic:confirm_friend(
                                                                        CurrentUid,
                                                                        From,
                                                                        To,
                                                                        Payload
                                                                    ),
                                                                    ?assertMatch(
                                                                        {ok, _FromID, _Remark,
                                                                            _Source},
                                                                        Result
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

confirm_friend_with_tags_test_() ->
    ?WITH_MECK(
        elib_dt,
        [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ],
        fun() ->
            ?WITH_MECK(
                jsone,
                [
                    {'decode', 2, fun(_Payload, _Opts) ->
                        {ok, #{
                            <<"from">> => #{
                                <<"remark">> => <<"朋友"/utf8>>, <<"tag">> => <<"tag1,tag2">>
                            },
                            <<"to">> => #{<<"remark">> => <<"同事"/utf8>>, <<"tag">> => <<"tag3">>},
                            <<"source">> => <<"qrcode">>
                        }}
                    end}
                ],
                fun() ->
                    ?WITH_MECK(
                        friend_ds,
                        [
                            {'pending_status', 2, fun(_From, _To) -> pending end},
                            {'is_friend', 2, fun(_FromId, _ToId) -> false end},
                            {'confirm_friend', 7, fun(
                                _IsFriend, _FromId, _ToId, _Remark, _Setting, _Tag, _NowTs
                            ) ->
                                ok
                            end}
                        ],
                        fun() ->
                            ?WITH_MECK(
                                msg_s2c_ds,
                                [
                                    {'write_msg', 8, fun(
                                        _CreatedAt,
                                        _MsgId,
                                        _Payload,
                                        _FromId,
                                        _ToId,
                                        _NowTs,
                                        _Action,
                                        _E2EE
                                    ) ->
                                        ok
                                    end}
                                ],
                                fun() ->
                                    ?WITH_MECK(
                                        message_ds,
                                        [
                                            {'assemble_msg', 8, fun(
                                                _Type,
                                                _From,
                                                _To,
                                                _Payload,
                                                _MsgId,
                                                _Body,
                                                _Action,
                                                _Ext
                                            ) ->
                                                #{<<"type">> => <<"S2C">>}
                                            end}
                                        ],
                                        fun() ->
                                            ?WITH_MECK(
                                                message_ds,
                                                [
                                                    {'send_next', 3, fun(
                                                        _ToId, _MsgId, _Message, _MsLi
                                                    ) ->
                                                        ok
                                                    end}
                                                ],
                                                fun() ->
                                                    ?WITH_MECK(
                                                        elib_retry_config,
                                                        [
                                                            {'intervals', 1, fun(_) -> [2000] end}
                                                        ],
                                                        fun() ->
                                                            ?WITH_MECK(
                                                                user_tag_relation_logic,
                                                                [
                                                                    {'add', 4, fun(
                                                                        _Uid,
                                                                        _Scene,
                                                                        _TargetUid,
                                                                        _Tags
                                                                    ) ->
                                                                        ok
                                                                    end}
                                                                ],
                                                                fun() ->
                                                                    ?WITH_MECK(
                                                                        imboy_cache,
                                                                        [
                                                                            {'flush', 1, fun(_Key) ->
                                                                                ok
                                                                            end}
                                                                        ],
                                                                        fun() ->
                                                                            CurrentUid = 200,
                                                                            From =
                                                                                <<"test_from_2">>,
                                                                            To = <<"test_to_2">>,
                                                                            Payload = <<"{}">>,

                                                                            Result = friend_logic:confirm_friend(
                                                                                CurrentUid,
                                                                                From,
                                                                                To,
                                                                                Payload
                                                                            ),
                                                                            ?assertMatch(
                                                                                {ok, _FromID,
                                                                                    _Remark,
                                                                                    _Source},
                                                                                Result
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

%% ===================================================================
%% confirm_friend_resp/2 测试
%% ===================================================================

confirm_friend_resp_test_() ->
    ?WITH_MECK(
        user_logic,
        [
            {'find_by_id', 2, fun(_Uid, _Column) ->
                #{
                    <<"id">> => 123,
                    <<"account">> => <<"test_account">>,
                    <<"nickname">> => <<"测试用户"/utf8>>,
                    <<"avatar">> => <<"https://example.com/avatar.jpg">>,
                    <<"gender">> => 1,
                    <<"sign">> => <<"个性签名"/utf8>>,
                    <<"region">> => <<"北京"/utf8>>,
                    <<"last_seen_at">> => 1700000000000
                }
            end}
        ],
        fun() ->
            ?WITH_MECK(
                user_ds,
                [
                    %% batch_online_state 注入实时 status + 透传 last_seen_at
                    {'batch_online_state', 1, fun([User]) ->
                        [
                            User#{
                                <<"status">> => online,
                                <<"last_seen_at">> => maps:get(<<"last_seen_at">>, User, <<>>)
                            }
                        ]
                    end}
                ],
                fun() ->
                    Uid = 123,
                    Remark = <<"备注名"/utf8>>,

                    Result = friend_logic:confirm_friend_resp(Uid, Remark),
                    ?assertMatch(
                        #{
                            <<"id">> := 123,
                            <<"peerId">> := 123,
                            <<"remark">> := <<"备注名"/utf8>>,
                            <<"account">> := <<"test_account">>,
                            <<"nickname">> := <<"测试用户"/utf8>>,
                            <<"is_friend">> := 1,
                            <<"status">> := online,
                            <<"last_seen_at">> := 1700000000000
                        },
                        Result
                    )
                end
            )
        end
    ).

%% ===================================================================
%% delete_friend/2 测试
%% ===================================================================

delete_friend_with_binary_uid_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'delete', 2, fun(_CurrentUid, _TargetUid) -> ok end}
        ],
        fun() ->
            ?WITH_MECK(
                imboy_cache,
                [
                    {'flush', 1, fun(_Key) -> ok end}
                ],
                fun() ->
                    CurrentUid = 1,
                    Uid = <<"2">>,

                    Result = friend_logic:delete_friend(CurrentUid, Uid),
                    ?assertEqual(ok, Result)
                end
            )
        end
    ).

delete_friend_with_integer_uid_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'delete', 2, fun(_CurrentUid, _TargetUid) -> ok end}
        ],
        fun() ->
            ?WITH_MECK(
                imboy_cache,
                [
                    {'flush', 1, fun(_Key) -> ok end}
                ],
                fun() ->
                    CurrentUid = 1,
                    Uid = 2,

                    Result = friend_logic:delete_friend(CurrentUid, Uid),
                    ?assertEqual(ok, Result)
                end
            )
        end
    ).

%% ===================================================================
%% move_to_category/3 测试
%% ===================================================================

move_to_category_success_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'move_to_category', 3, fun(_CurrentUid, _TargetUid, _CategoryId) -> ok end}
        ],
        fun() ->
            CurrentUid = 1,
            Uid = 2,
            CategoryId = 5,

            Result = friend_logic:move_to_category(CurrentUid, Uid, CategoryId),
            ?assertEqual(ok, Result)
        end
    ).

move_to_category_with_binary_uid_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'move_to_category', 3, fun(_CurrentUid, _TargetUid, _CategoryId) -> ok end}
        ],
        fun() ->
            CurrentUid = 1,
            Uid = <<"2">>,
            CategoryId = 3,

            Result = friend_logic:move_to_category(CurrentUid, Uid, CategoryId),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% information/2 测试
%% ===================================================================

information_with_valid_friend_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'is_friend_fields', 3, fun(_CurrentUid, _Uid, _Column) -> {true, #{<<"id">> => 1}} end}
        ],
        fun() ->
            ?WITH_MECK(
                user_logic,
                [
                    {'find_by_id', 2, fun(_Uid, _Column) ->
                        #{
                            <<"id">> => 2,
                            <<"account">> => <<"test_account">>,
                            <<"nickname">> => <<"测试用户"/utf8>>,
                            <<"avatar">> => <<"https://example.com/avatar.jpg">>
                        }
                    end}
                ],
                fun() ->
                    CurrentUid = 1,
                    Uid = 2,

                    Result = friend_logic:information(CurrentUid, Uid),
                    ?assertMatch(#{<<"is_friend">> := true}, Result)
                end
            )
        end
    ).

information_with_non_friend_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'is_friend_fields', 3, fun(_CurrentUid, _Uid, _Column) -> {false, #{}} end}
        ],
        fun() ->
            CurrentUid = 1,
            Uid = 2,

            Result = friend_logic:information(CurrentUid, Uid),
            ?assertEqual(#{}, Result)
        end
    ).

information_with_nonexistent_user_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'is_friend_fields', 3, fun(_CurrentUid, _Uid, _Column) -> {true, #{<<"id">> => 1}} end}
        ],
        fun() ->
            ?WITH_MECK(
                user_logic,
                [
                    {'find_by_id', 2, fun(_Uid, _Column) -> #{} end}
                ],
                fun() ->
                    CurrentUid = 1,
                    Uid = 999,

                    Result = friend_logic:information(CurrentUid, Uid),
                    ?assertEqual(#{}, Result)
                end
            )
        end
    ).

%% ===================================================================
%% T3.4 余项 pending-store：accept/reject gating 测试
%% ===================================================================

%% confirm_friend：无 pending 申请（none）→ friend_agg:accept 拒 no_pending_request
confirm_friend_without_pending_returns_error_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'pending_status', 2, fun(_From, _To) -> none end}
        ],
        fun() ->
            CurrentUid = 2,
            From = <<"1">>,
            To = <<"2">>,
            Payload = <<"{}">>,
            Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
            ?assertMatch({error, <<"no_pending_request">>, _}, Result)
        end
    ).

%% reject_friend：存在 pending → friend_agg:reject ok，删 pending 行
reject_friend_with_pending_returns_ok_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'pending_status', 2, fun(_From, _To) -> pending end},
            {'delete_pending', 2, fun(_From, _To) -> ok end}
        ],
        fun() ->
            CurrentUid = 2,
            From = <<"1">>,
            Result = friend_logic:reject_friend(CurrentUid, From),
            ?assertEqual(ok, Result)
        end
    ).

%% reject_friend：无 pending（none）→ friend_agg:reject 拒 no_pending_request
reject_friend_without_pending_returns_error_test_() ->
    ?WITH_MECK(
        friend_ds,
        [
            {'pending_status', 2, fun(_From, _To) -> none end}
        ],
        fun() ->
            CurrentUid = 2,
            From = <<"1">>,
            Result = friend_logic:reject_friend(CurrentUid, From),
            ?assertMatch({error, <<"no_pending_request">>, _}, Result)
        end
    ).

%% ===================================================================
%% 加友方式隐私开关读侧强制（QA #19）
%% ===================================================================

add_friend_denied_by_phone_switch_test_() ->
    ?WITH_MECK(
        user_setting_ds,
        [{'find_by_uid', 1, fun(_Uid) -> #{<<"allow_add_by_phone">> => false} end}],
        fun() ->
            Payload = #{
                <<"from">> => #{<<"source">> => <<"mobile">>},
                <<"msg">> => <<"hi">>
            },
            Result = friend_logic:add_friend(1, <<"2">>, Payload, 1640995200),
            ?assertMatch({error, <<"add_way_disabled">>, _}, Result)
        end
    ).

add_friend_denied_by_qr_switch_test_() ->
    ?WITH_MECK(
        user_setting_ds,
        [{'find_by_uid', 1, fun(_Uid) -> #{<<"allow_add_by_qr">> => false} end}],
        fun() ->
            Payload = #{
                <<"from">> => #{<<"source">> => <<"qrcode">>},
                <<"msg">> => <<"hi">>
            },
            Result = friend_logic:add_friend(1, <<"2">>, Payload, 1640995200),
            ?assertMatch({error, <<"add_way_disabled">>, _}, Result)
        end
    ).

add_friend_allowed_when_switch_absent_test_() ->
    ?WITH_MECKS(
        [
            {user_setting_ds, [{'find_by_uid', 1, fun(_Uid) -> #{} end}]},
            {friend_ds, [
                {'pending_status', 2, fun(_From, _To) -> pending end}
            ]}
        ],
        fun() ->
            %% 开关缺省=允许：source=mobile 但无设置时须走到 already_requested
            %% （pending 状态拦截），而非 add_way_disabled
            Payload = #{
                <<"from">> => #{<<"source">> => <<"mobile">>},
                <<"msg">> => <<"hi">>
            },
            Result = friend_logic:add_friend(1, <<"2">>, Payload, 1640995200),
            ?assertMatch({error, <<"already_requested">>, _}, Result)
        end
    ).
