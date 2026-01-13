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
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"test_to_2">>) -> 123 end},
        {'encode', 1, fun(1) -> <<"encoded_1">> end}
    ], fun() ->
        ?WITH_MECK(elib_dt, [
            {'to_rfc3339', 1, fun(_Timestamp) -> <<"2023-01-01T00:00:00Z">> end},
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ], fun() ->
            ?WITH_MECK(msg_s2c_ds, [
                {'write_msg', 8, fun(_CreatedAt, _MsgId, _Payload, _FromId, _ToId, _NowTs, _Action, _E2EE) -> ok end}
            ], fun() ->
                ?WITH_MECK(message_ds, [
                    {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                        #{<<"type">> => <<"S2C">>, <<"msg_id">> => <<"test_msg_123">>}
                    end}
                ], fun() ->
                    ?WITH_MECK(message_ds, [
                        {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                    ], fun() ->
                        ?WITH_MECK(elib_retry_config, [
                            {'intervals', 1, fun(<<"s2c">>) -> [2000, 5000, 7000, 11000] end}
                        ], fun() ->
                            CurrentUid = 1,
                            To = <<"test_to_2">>,
                            Payload = #{<<"msg">> => <<"请加我好友"/utf8>>},
                            CreatedAt = 1640995200,

                            Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
                            ?assertEqual(ok, Result)
                        end)
                    end)
                end)
            end)
        end)
    end).

add_friend_with_map_payload_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"test_to_2">>) -> 123 end},
        {'encode', 1, fun(1) -> <<"encoded_1">> end}
    ], fun() ->
        ?WITH_MECK(elib_dt, [
            {'to_rfc3339', 1, fun(_Timestamp) -> <<"2023-01-01T00:00:00Z">> end},
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ], fun() ->
            ?WITH_MECK(msg_s2c_ds, [
                {'write_msg', 8, fun(_CreatedAt, _MsgId, _Payload, _FromId, _ToId, _NowTs, _Action, _E2EE) -> ok end}
            ], fun() ->
                ?WITH_MECK(message_ds, [
                    {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                        #{<<"type">> => <<"S2C">>}
                    end}
                ], fun() ->
                    ?WITH_MECK(message_ds, [
                        {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                    ], fun() ->
                        ?WITH_MECK(elib_retry_config, [
                            {'intervals', 1, fun(_) -> [2000] end}
                        ], fun() ->
                            CurrentUid = 1,
                            To = <<"test_to_2">>,
                            Payload = #{<<"msg">> => <<"你好"/utf8>>, <<"source">> => <<"search">>},
                            CreatedAt = <<"2023-01-01T00:00:00Z">>,

                            Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
                            ?assertEqual(ok, Result)
                        end)
                    end)
                end)
            end)
        end)
    end).

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
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"test_from_2">>) -> 100; (<<"test_to_2">>) -> 200 end}
    ], fun() ->
        ?WITH_MECK(elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ], fun() ->
            ?WITH_MECK(jsone, [
                {'decode', 2, fun(_Payload, _Opts) ->
                    {ok, #{<<"from">> => #{<<"remark">> => <<"好友A"/utf8>>, <<"tag">> => <<>>},
                           <<"to">> => #{<<"remark">> => <<"好友B"/utf8>>, <<"tag">> => <<>>},
                           <<"source">> => <<"search">>}}
                end}
            ], fun() ->
                ?WITH_MECK(friend_ds, [
                    {'is_friend', 2, fun(_FromId, _ToId) -> false end},
                    {'confirm_friend', 7, fun(_IsFriend, _FromId, _ToId, _Remark, _Setting, _Tag, _NowTs) -> ok end}
                ], fun() ->
                    ?WITH_MECK(msg_s2c_ds, [
                        {'write_msg', 8, fun(_CreatedAt, _MsgId, _Payload, _FromId, _ToId, _NowTs, _Action, _E2EE) -> ok end}
                    ], fun() ->
                        ?WITH_MECK(message_ds, [
                            {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                                #{<<"type">> => <<"S2C">>}
                            end}
                        ], fun() ->
                            ?WITH_MECK(message_ds, [
                                {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                            ], fun() ->
                                ?WITH_MECK(elib_retry_config, [
                                    {'intervals', 1, fun(_) -> [2000] end}
                                ], fun() ->
                                    ?WITH_MECK(imboy_cache, [
                                        {'flush', 1, fun(_Key) -> ok end}
                                    ], fun() ->
                                        CurrentUid = 200,
                                        From = <<"test_from_2">>,
                                        To = <<"test_to_2">>,
                                        Payload = <<"{}">>,

                                        Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
                                        ?assertMatch({ok, _FromID, _Remark, _Source}, Result)
                                    end)
                                end)
                            end)
                        end)
                    end)
                end)
            end)
        end)
    end).

confirm_friend_with_tags_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"test_from_2">>) -> 100; (<<"test_to_2">>) -> 200 end}
    ], fun() ->
        ?WITH_MECK(elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ], fun() ->
            ?WITH_MECK(jsone, [
                {'decode', 2, fun(_Payload, _Opts) ->
                    {ok, #{<<"from">> => #{<<"remark">> => <<"朋友"/utf8>>, <<"tag">> => <<"tag1,tag2">>},
                           <<"to">> => #{<<"remark">> => <<"同事"/utf8>>, <<"tag">> => <<"tag3">>},
                           <<"source">> => <<"qrcode">>}}
                end}
            ], fun() ->
                ?WITH_MECK(friend_ds, [
                    {'is_friend', 2, fun(_FromId, _ToId) -> false end},
                    {'confirm_friend', 7, fun(_IsFriend, _FromId, _ToId, _Remark, _Setting, _Tag, _NowTs) -> ok end}
                ], fun() ->
                    ?WITH_MECK(msg_s2c_ds, [
                        {'write_msg', 8, fun(_CreatedAt, _MsgId, _Payload, _FromId, _ToId, _NowTs, _Action, _E2EE) -> ok end}
                    ], fun() ->
                        ?WITH_MECK(message_ds, [
                            {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                                #{<<"type">> => <<"S2C">>}
                            end}
                        ], fun() ->
                            ?WITH_MECK(message_ds, [
                                {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                            ], fun() ->
                                ?WITH_MECK(elib_retry_config, [
                                    {'intervals', 1, fun(_) -> [2000] end}
                                ], fun() ->
                                    ?WITH_MECK(user_tag_relation_logic, [
                                        {'add', 4, fun(_Uid, _Scene, _TargetUid, _Tags) -> ok end}
                                    ], fun() ->
                                        ?WITH_MECK(imboy_cache, [
                                            {'flush', 1, fun(_Key) -> ok end}
                                        ], fun() ->
                                            CurrentUid = 200,
                                            From = <<"test_from_2">>,
                                            To = <<"test_to_2">>,
                                            Payload = <<"{}">>,

                                            Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
                                            ?assertMatch({ok, _FromID, _Remark, _Source}, Result)
                                        end)
                                    end)
                                end)
                            end)
                        end)
                    end)
                end)
            end)
        end)
    end).

%% ===================================================================
%% confirm_friend_resp/2 测试
%% ===================================================================

confirm_friend_resp_test_() ->
    ?WITH_MECK(user_logic, [
        {'find_by_id', 2, fun(_Uid, _Column) ->
            #{
                <<"id">> => 123,
                <<"account">> => <<"test_account">>,
                <<"nickname">> => <<"测试用户"/utf8>>,
                <<"avatar">> => <<"https://example.com/avatar.jpg">>,
                <<"gender">> => 1,
                <<"sign">> => <<"个性签名"/utf8>>,
                <<"region">> => <<"北京"/utf8>>,
                <<"status">> => 1
            }
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_123">> end}
        ], fun() ->
            Uid = 123,
            Remark = <<"备注名"/utf8>>,

            Result = friend_logic:confirm_friend_resp(Uid, Remark),
            ?assertMatch(#{
                <<"id">> := <<"encoded_123">>,
                <<"remark">> := <<"备注名"/utf8>>,
                <<"account">> := <<"test_account">>,
                <<"nickname">> := <<"测试用户"/utf8>>
            }, Result)
        end)
    end).

%% ===================================================================
%% delete_friend/2 测试
%% ===================================================================

delete_friend_with_binary_uid_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"encoded_uid_2">>) -> 2 end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'delete', 2, fun(_CurrentUid, _TargetUid) -> ok end}
        ], fun() ->
            ?WITH_MECK(imboy_cache, [
                {'flush', 1, fun(_Key) -> ok end}
            ], fun() ->
                CurrentUid = 1,
                Uid = <<"encoded_uid_2">>,

                Result = friend_logic:delete_friend(CurrentUid, Uid),
                ?assertEqual(ok, Result)
            end)
        end)
    end).

delete_friend_with_integer_uid_test_() ->
    ?WITH_MECK(friend_ds, [
        {'delete', 2, fun(_CurrentUid, _TargetUid) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ], fun() ->
            CurrentUid = 1,
            Uid = 2,

            Result = friend_logic:delete_friend(CurrentUid, Uid),
            ?assertEqual(ok, Result)
        end)
    end).

%% ===================================================================
%% move_to_category/3 测试
%% ===================================================================

move_to_category_success_test_() ->
    ?WITH_MECK(friend_ds, [
        {'move_to_category', 3, fun(_CurrentUid, _TargetUid, _CategoryId) -> ok end}
    ], fun() ->
        CurrentUid = 1,
        Uid = 2,
        CategoryId = 5,

        Result = friend_logic:move_to_category(CurrentUid, Uid, CategoryId),
        ?assertEqual(ok, Result)
    end).

move_to_category_with_binary_uid_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"encoded_uid_2">>) -> 2 end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'move_to_category', 3, fun(_CurrentUid, _TargetUid, _CategoryId) -> ok end}
        ], fun() ->
            CurrentUid = 1,
            Uid = <<"encoded_uid_2">>,
            CategoryId = 3,

            Result = friend_logic:move_to_category(CurrentUid, Uid, CategoryId),
            ?assertEqual(ok, Result)
        end)
    end).

%% ===================================================================
%% information/2 测试
%% ===================================================================

information_with_valid_friend_test_() ->
    ?WITH_MECK(friend_ds, [
        {'is_friend_fields', 3, fun(_CurrentUid, _Uid, _Column) -> {true, #{<<"id">> => 1}} end}
    ], fun() ->
        ?WITH_MECK(user_logic, [
            {'find_by_id', 2, fun(_Uid, _Column) ->
                #{
                    <<"id">> => 2,
                    <<"account">> => <<"test_account">>,
                    <<"nickname">> => <<"测试用户"/utf8>>,
                    <<"avatar">> => <<"https://example.com/avatar.jpg">>
                }
            end}
        ], fun() ->
            CurrentUid = 1,
            Uid = 2,

            Result = friend_logic:information(CurrentUid, Uid),
            ?assertMatch(#{<<"is_friend">> := true}, Result)
        end)
    end).

information_with_non_friend_test_() ->
    ?WITH_MECK(friend_ds, [
        {'is_friend_fields', 3, fun(_CurrentUid, _Uid, _Column) -> {false, #{}} end}
    ], fun() ->
        CurrentUid = 1,
        Uid = 2,

        Result = friend_logic:information(CurrentUid, Uid),
        ?assertEqual(#{}, Result)
    end).

information_with_nonexistent_user_test_() ->
    ?WITH_MECK(friend_ds, [
        {'is_friend_fields', 3, fun(_CurrentUid, _Uid, _Column) -> {true, #{<<"id">> => 1}} end}
    ], fun() ->
        ?WITH_MECK(user_logic, [
            {'find_by_id', 2, fun(_Uid, _Column) -> #{} end}
        ], fun() ->
            CurrentUid = 1,
            Uid = 999,

            Result = friend_logic:information(CurrentUid, Uid),
            ?assertEqual(#{}, Result)
        end)
    end).
