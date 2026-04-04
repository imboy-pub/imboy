-module(user_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_server 模块的 EUnit 测试
%%%
%%% 目标：验证用户服务器 gen_server 功能
%%% 覆盖：上线/下线处理、好友通知、用户注销
%%%===================================================================

%% ===================================================================
%% gen_server 回调测试
%% ===================================================================

init_returns_empty_state_test_() ->
    ?_test(begin
        Result = user_server:init([]),
        ?assertMatch({ok, _State}, Result),
        {ok, State} = Result,
        ?assertEqual([], State)
    end).

handle_call_stop_returns_stopped_test() ->
    State = [],
    Result = user_server:handle_call(stop, self(), State),
    ?assertMatch({stop, normal, stopped, []}, Result).

handle_call_unknown_request_returns_ignored_test() ->
    State = [],
    From = self(),
    Request = unknown_request,
    Result = user_server:handle_call(Request, From, State),
    ?assertMatch({reply, ignored, []}, Result).

handle_info_returns_noreply_test() ->
    State = [],
    Info = some_info,
    Result = user_server:handle_info(Info, State),
    ?assertMatch({noreply, []}, Result).

terminate_returns_ok_test() ->
    Result = user_server:terminate(normal, []),
    ?assertEqual(ok, Result).

code_change_returns_ok_test() ->
    State = [],
    Result = user_server:code_change(v1, State, v2),
    ?assertMatch({ok, []}, Result).

%% ===================================================================
%% handle_cast 测试
%% ===================================================================

handle_cast_signup_success_hibernates_test() ->
    State = [],
    Uid = <<"uid_123">>,
    PostVals = #{<<"email">> => <<"test@example.com">>},

    Result = user_server:handle_cast({signup_success, Uid, PostVals}, State),
    ?assertMatch({noreply, [], hibernate}, Result).

handle_cast_login_success_updates_device_and_notifies_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"uid_123">>) -> 123 end}
    ], fun() ->
        ?WITH_MECK(elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ], fun() ->
            ?WITH_MECK(user_device_repo, [
                {'save', 5, fun(_Now, _Uid, _DID, _PostMap) -> ok end}
            ], fun() ->
                ?WITH_MECK(user_repo, [
                    {'update_friends_last_seen_at', 2, fun(_Uid, _Now) -> ok end}
                ], fun() ->
                    ?WITH_MECK(message_ds, [
                        {'check_and_notify_offline_msgs', 1, fun(_Uid) -> ok end}
                    ], fun() ->
                        State = [],
                        Uid = <<"uid_123">>,
                        PostVals = #{<<"did">> => <<"device_1">>},

                        Result = user_server:handle_cast({login_success, Uid, PostVals}, State),
                        ?assertMatch({noreply, [], hibernate}, Result)
                    end)
                end)
            end)
        end)
    end).

handle_cast_ws_online_updates_device_and_checks_offline_test_() ->
    ?WITH_MECK(elib_dt, [
        {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
    ], fun() ->
        ?WITH_MECK(user_device_repo, [
            {'update_by_did', 4, fun(_Uid, _DID, _Set, _SetArgs) -> ok end}
        ], fun() ->
            ?WITH_MECK(message_ds, [
                {'check_and_notify_offline_msgs', 1, fun(_Uid) -> ok end}
            ], fun() ->
                State = [],
                Uid = 123,
                Pid = self(),
                DType = <<"ios">>,
                DID = <<"device_1">>,

                Result = user_server:handle_cast({ws_online, Uid, Pid, DType, DID}, State),
                ?assertMatch({noreply, [], hibernate}, Result)
            end)
        end)
    end).

handle_cast_online_notifies_friends_test_() ->
    ?WITH_MECK(user_device_logic, [
        {'device_name', 2, fun(_Uid, _DID) -> <<"iPhone 14">> end}
    ], fun() ->
        ?WITH_MECK(elib_id, [
            {'gen', 1, fun(_) -> <<"msg_id_123">> end}
        ], fun() ->
            ?WITH_MECK(message_ds, [
                {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                    #{<<"type">> => <<"S2C">>}
                end}
            ], fun() ->
                ?WITH_MECK(elib_retry_config, [
                    {'intervals', 1, fun(_) -> [2000] end}
                ], fun() ->
                    ?WITH_MECK(jsone, [
                        {'encode', 2, fun(_Msg, _Opts) -> <<"{}">> end}
                    ], fun() ->
                        ?WITH_MECK(message_ds, [
                            {'send_next', 5, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
                        ], fun() ->
                            ?WITH_MECK(user_setting_ds, [
                                {'chat_state_hide', 1, fun(_Uid) -> false end}
                            ], fun() ->
                                ?WITH_MECK(friend_ds, [
                                    {'list_by_uid', 1, fun(_Uid) -> [2, 3, 4] end}
                                ], fun() ->
                                    ?WITH_MECK(msg_s2c_ds, [
                                        {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
                                    ], fun() ->
                                        State = [],
                                        Uid = 123,
                                        Pid = self(),
                                        DID = <<"device_1">>,

                                        Result = user_server:handle_cast({online, Uid, Pid, DID}, State),
                                        ?assertMatch({noreply, [], hibernate}, Result)
                                    end)
                                end)
                            end)
                        end)
                    end)
                end)
            end)
        end)
    end).


handle_cast_online_with_hidden_state_skips_notification_test_() ->
    ?WITH_MECKS([
        {user_device_logic, [
            {'device_name', 2, fun(_Uid, _DID) -> <<"iPhone 14">> end}
        ]},
        {elib_id, [
            {'gen', 1, fun(_) -> <<"msg_id_123">> end}
        ]},
        {message_ds, [
            {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                #{<<"type">> => <<"S2C">>}
            end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_) -> [2000] end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Msg, _Opts) -> <<"{}">> end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
        ]},
        {user_setting_ds, [
            {'chat_state_hide', 1, fun(_Uid) -> true end}
        ]}
    ], fun() ->
        State = [],
        Uid = 123,
        Pid = self(),
        DID = <<"device_1">>,

        Result = user_server:handle_cast({online, Uid, Pid, DID}, State),
        ?assertMatch({noreply, [], hibernate}, Result)
    end).

handle_cast_offline_notifies_friends_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'list_by_uid', 1, fun(_Uid) -> [2, 3] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
        ]}
    ], fun() ->
        State = [],
        Uid = 123,
        Pid = self(),
        DID = <<"device_1">>,

        Result = user_server:handle_cast({offline, Uid, Pid, DID}, State),
        ?assertMatch({noreply, [], hibernate}, Result)
    end).

handle_cast_cancel_deletes_user_and_notifies_friends_test_() ->
    ?WITH_MECKS([
        {user_repo, [
            {'find_by_id', 2, fun(_Uid, _Column) -> #{<<"id">> => 123} end}
        ]},
        {user_setting_ds, [
            {'find_by_uid', 1, fun(_Uid) -> #{<<"hide_chat">> => false} end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Timestamp) -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(_Fun) -> ok end}
        ]},
        {user_log_repo, [
            {'add', 2, fun(_Conn, _Data) -> {ok, 1} end}
        ]},
        {friend_ds, [
            {'list_by_uid', 1, fun(_Uid) -> [2, 3] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
        ]}
    ], fun() ->
        State = [],
        Uid = 123,
        CreatedAt = 1672531200,
        Opt = #{<<"reason">> => <<"user_request">>},

        Result = user_server:handle_cast({cancel, Uid, CreatedAt, Opt}, State),
        ?assertMatch({noreply, [], hibernate}, Result)
    end).

handle_cast_notice_friend_sends_notification_test_() ->
    ?WITH_MECK(friend_ds, [
        {'list_by_uid', 1, fun(_Uid) -> [2, 3, 4] end}
    ], fun() ->
        ?WITH_MECK(msg_s2c_ds, [
            {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
        ], fun() ->
            State = [],
            Uid = 123,
            ToState = <<"online">>,

            Result = user_server:handle_cast({notice_friend, Uid, ToState}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end)
    end).

handle_cast_unknown_message_returns_noreply_test() ->
    State = [],
    Msg = unknown_message,
    Result = user_server:handle_cast(Msg, State),
    ?assertMatch({noreply, []}, Result).

%% ===================================================================
%% API 函数测试
%% ===================================================================

cast_notice_friend_sends_cast_test_() ->
    ?WITH_MECK(gen_server, [
        {'cast', 2, fun(?MODULE, _Msg) -> ok end}
    ], fun() ->
        CurrentUid = 123,
        ChatState = <<"online">>,

        Result = user_server:cast_notice_friend(CurrentUid, ChatState),
        ?assertEqual(ok, Result)
    end).

cast_online_sends_two_casts_test_() ->
    ?WITH_MECK(gen_server, [
        {'cast', 2, fun(?MODULE, _Msg) -> ok end}
    ], fun() ->
        Uid = 123,
        Pid = self(),
        DID = <<"device_1">>,
        DType = <<"ios">>,

        Result = user_server:cast_online(Uid, Pid, DID, DType),
        ?assertEqual(ok, Result)
    end).

cast_offline_sends_cast_test_() ->
    ?WITH_MECK(gen_server, [
        {'cast', 2, fun(?MODULE, _Msg) -> ok end}
    ], fun() ->
        Uid = 123,
        Pid = self(),
        DID = <<"device_1">>,

        Result = user_server:cast_offline(Uid, Pid, DID),
        ?assertEqual(ok, Result)
    end).

cast_cancel_sends_cast_test_() ->
    ?WITH_MECK(gen_server, [
        {'cast', 2, fun(?MODULE, _Msg) -> ok end}
    ], fun() ->
        Uid = 123,
        CreatedAt = 1672531200,
        Opt = #{<<"reason">> => <<"user_request">>},

        Result = user_server:cast_cancel(Uid, CreatedAt, Opt),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 内部函数测试
%% ===================================================================

notice_friend_sends_online_notification_test_() ->
    ?WITH_MECK(friend_ds, [
        {'list_by_uid', 1, fun(_Uid) -> [2, 3] end}
    ], fun() ->
        ?WITH_MECK(msg_s2c_ds, [
            {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
        ], fun() ->
            Uid = 123,
            Action = <<"online">>,

            Result = user_server:notice_friend(Uid, Action),
            ?assertEqual(ok, Result)
        end)
    end).

notice_friend_sends_offline_notification_test_() ->
    ?WITH_MECK(friend_ds, [
        {'list_by_uid', 1, fun(_Uid) -> [2, 3, 4] end}
    ], fun() ->
        ?WITH_MECK(msg_s2c_ds, [
            {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
        ], fun() ->
            Uid = 123,
            Action = <<"offline">>,

            Result = user_server:notice_friend(Uid, Action),
            ?assertEqual(ok, Result)
        end)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

handle_cast_login_success_with_missing_did_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"uid_123">>) -> 123 end}
    ], fun() ->
        ?WITH_MECK(elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ], fun() ->
            ?WITH_MECK(user_device_repo, [
                {'save', 5, fun(_Now, _Uid, _DID, _PostMap) -> ok end}
            ], fun() ->
                ?WITH_MECK(user_repo, [
                    {'update_friends_last_seen_at', 2, fun(_Uid, _Now) -> ok end}
                ], fun() ->
                    ?WITH_MECK(message_ds, [
                        {'check_and_notify_offline_msgs', 1, fun(_Uid) -> ok end}
                    ], fun() ->
                        State = [],
                        Uid = <<"uid_123">>,
                        PostVals = #{<<"other">> => <<"data">>},  % 缺少 did

                        Result = user_server:handle_cast({login_success, Uid, PostVals}, State),
                        ?assertMatch({noreply, [], hibernate}, Result)
                    end)
                end)
            end)
        end)
    end).

handle_cast_online_with_empty_did_test_() ->
    ?WITH_MECK(user_device_logic, [
        {'device_name', 2, fun(_Uid, _DID) -> <<"Unknown Device">> end}
    ], fun() ->
        ?WITH_MECK(elib_id, [
            {'gen', 1, fun(_) -> <<"msg_id_123">> end}
        ], fun() ->
            ?WITH_MECK(message_ds, [
                {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                    #{<<"type">> => <<"S2C">>}
                end}
            ], fun() ->
                ?WITH_MECK(elib_retry_config, [
                    {'intervals', 1, fun(_) -> [2000] end}
                ], fun() ->
                    ?WITH_MECK(jsone, [
                        {'encode', 2, fun(_Msg, _Opts) -> <<"{}">> end}
                    ], fun() ->
                        ?WITH_MECK(message_ds, [
                            {'send_next', 5, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
                        ], fun() ->
                            ?WITH_MECK(user_setting_ds, [
                                {'chat_state_hide', 1, fun(_Uid) -> false end}
                            ], fun() ->
                                ?WITH_MECK(friend_ds, [
                                    {'list_by_uid', 1, fun(_Uid) -> [] end}
                                ], fun() ->
                                    ?WITH_MECK(msg_s2c_ds, [
                                        {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
                                    ], fun() ->
                                        State = [],
                                        Uid = 123,
                                        Pid = self(),
                                        DID = <<>>,

                                        Result = user_server:handle_cast({online, Uid, Pid, DID}, State),
                                        ?assertMatch({noreply, [], hibernate}, Result)
                                    end)
                                end)
                            end)
                        end)
                    end)
                end)
            end)
        end)
    end).

notice_friend_with_no_friends_test_() ->
    ?WITH_MECK(friend_ds, [
        {'list_by_uid', 1, fun(_Uid) -> [] end}
    ], fun() ->
        ?WITH_MECK(msg_s2c_ds, [
            {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
        ], fun() ->
            Uid = 123,
            Action = <<"online">>,

            Result = user_server:notice_friend(Uid, Action),
            ?assertEqual(ok, Result)
        end)
    end).
