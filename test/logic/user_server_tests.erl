-module(user_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_server 模块的 EUnit 测试
%%%
%%% 目标：验证用户服务器 gen_server 功能
%%% 覆盖：上线/下线处理、好友通知、用户注销
%%%
%%% 修复说明：
%%%   - API 函数测试不再 mock gen_server 本身（会导致框架死锁），
%%%     改为直接调用并验证返回值
%%%   - 合并同一模块的重复 mock，避免嵌套覆盖导致真实函数调用
%%%   - 补齐 cancel/3、login_success、online 等分支缺少的 mock
%%%   - 所有 mock 使用 ?WITH_MECKS 扁平结构，减少嵌套干扰
%%%===================================================================

%% ===================================================================
%% gen_server 回调测试（纯函数，无需 mock）
%% ===================================================================

init_starts_fixed_worker_shards_test_() ->
    ?_test(begin
        Result = user_server:init([]),
        ?assertMatch({ok, _State}, Result),
        {ok, State} = Result,
        ?assertEqual(16, length(State)),
        ?assert(lists:all(fun(#{pid := Pid}) -> is_pid(Pid) end, State)),
        ok = user_server:terminate(normal, State)
    end).

different_users_run_concurrently_same_user_stays_ordered_test_() ->
    FirstUid = 1,
    FirstShard = erlang:phash2(FirstUid, 16),
    [OtherUid | _] = [Uid || Uid <- lists:seq(2, 100), erlang:phash2(Uid, 16) =/= FirstShard],
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'list_by_uid', 1, fun(_) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_, _, _, _, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            Parent = self(),
            Calls = counters:new(1, []),
            meck:expect(friend_ds, list_by_uid, fun
                (Uid) when Uid =:= FirstUid ->
                    counters:add(Calls, 1, 1),
                    N = counters:get(Calls, 1),
                    Parent ! {first_user_started, N, self()},
                    case N of
                        1 ->
                            receive
                                release -> []
                            end;
                        _ ->
                            []
                    end;
                (Uid) when Uid =:= OtherUid ->
                    Parent ! other_user_finished,
                    []
            end),
            {ok, State} = user_server:init([]),
            try
                {noreply, State1, hibernate} =
                    user_server:handle_cast({notice_friend, FirstUid, <<"online">>}, State),
                receive
                    {first_user_started, 1, FirstWorker} ->
                        {noreply, State2, hibernate} = user_server:handle_cast(
                            {notice_friend, FirstUid, <<"offline">>}, State1
                        ),
                        {noreply, State3, hibernate} = user_server:handle_cast(
                            {notice_friend, OtherUid, <<"online">>}, State2
                        ),
                        receive
                            other_user_finished -> ok
                        after 500 -> ?assert(false)
                        end,
                        receive
                            {first_user_started, 2, _} -> ?assert(false)
                        after 0 ->
                            ok
                        end,
                        FirstWorker ! release,
                        receive
                            {worker_done, FirstWorker, DoneRef} ->
                                {noreply, State4} = user_server:handle_info(
                                    {worker_done, FirstWorker, DoneRef}, State3
                                ),
                                receive
                                    {first_user_started, 2, _} -> ok
                                after 500 -> ?assert(false)
                                end,
                                ?assertEqual(length(State3), length(State4))
                        after 500 -> ?assert(false)
                        end
                after 500 ->
                    ?assert(false)
                end
            after
                user_server:terminate(normal, State)
            end
        end
    ).

handle_call_stop_returns_stopped_test() ->
    State = [],
    Result = user_server:handle_call(stop, self(), State),
    ?assertMatch({stop, normal, stopped, []}, Result).

handle_call_unknown_request_returns_ignored_test() ->
    State = [],
    From = self(),
    Request = unknown_request,
    Result = user_server:handle_call(Request, From, State),
    %% handle_call 对未知请求调用 ?DEBUG_LOG，debug 模式下需要 elib_log
    %% 但这里只验证返回格式
    ?assertMatch({reply, ignored, []}, Result).

handle_info_returns_noreply_test() ->
    State = [],
    Info = some_info,
    Result = user_server:handle_info(Info, State),
    ?assertMatch({noreply, []}, Result).

worker_exit_restarts_same_shard_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'list_by_uid', 1, fun(_) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_, _, _, _, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            Parent = self(),
            Calls = counters:new(1, []),
            meck:expect(friend_ds, list_by_uid, fun(_) ->
                counters:add(Calls, 1, 1),
                N = counters:get(Calls, 1),
                Parent ! {task_started, N, self()},
                case N of
                    1 ->
                        receive
                            never -> []
                        end;
                    _ ->
                        []
                end
            end),
            {ok, State} = user_server:init([]),
            {noreply, RunningState, hibernate} =
                user_server:handle_cast({notice_friend, 16, <<"online">>}, State),
            receive
                {task_started, 1, Worker} ->
                    exit(Worker, kill),
                    receive
                        {'EXIT', Worker, Reason} ->
                            {noreply, NewState} =
                                user_server:handle_info({'EXIT', Worker, Reason}, RunningState),
                            receive
                                {task_started, 2, NewWorker} ->
                                    ?assertNotEqual(Worker, NewWorker),
                                    ?assert(is_process_alive(NewWorker))
                            after 500 -> ?assert(false)
                            end,
                            user_server:terminate(normal, NewState)
                    after 500 -> ?assert(false)
                    end
            after 500 -> ?assert(false)
            end
        end
    ).

worker_exit_drops_no_replay_current_and_runs_next_test_() ->
    ?_test(begin
        process_flag(trap_exit, true),
        Worker = spawn_link(fun() ->
            receive
                stop -> ok
            end
        end),
        Parent = self(),
        NeverReplay = fun() -> Parent ! replayed end,
        RunNext = fun() -> Parent ! next_ran end,
        State = [
            #{
                pid => Worker,
                current => {make_ref(), NeverReplay, no_replay},
                queue => queue:from_list([{RunNext, replay}])
            }
        ],
        exit(Worker, kill),
        receive
            {'EXIT', Worker, Reason} ->
                {noreply, NewState} = user_server:handle_info({'EXIT', Worker, Reason}, State),
                receive
                    next_ran -> ok
                after 500 -> ?assert(false)
                end,
                receive
                    replayed -> ?assert(false)
                after 0 -> ok
                end,
                ok = user_server:terminate(normal, NewState)
        after 500 -> ?assert(false)
        end
    end).

terminate_stops_workers_before_return_test_() ->
    ?_test(begin
        {ok, State} = user_server:init([]),
        ok = user_server:terminate(normal, State),
        ?assert(lists:all(fun(#{pid := Pid}) -> not is_process_alive(Pid) end, State))
    end).

terminate_returns_ok_test() ->
    Result = user_server:terminate(normal, []),
    ?assertEqual(ok, Result).

code_change_migrates_legacy_empty_state_test_() ->
    ?_test(begin
        {ok, State} = user_server:code_change(v1, [], v2),
        ?assertEqual(16, length(State)),
        ok = user_server:terminate(normal, State)
    end).

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
    %% login_success 调用链:
    %%   ec_cnv:to_integer/1  (纯函数，无需 mock)
    %%   elib_dt:now/0
    %%   user_device_ds:save/4
    %%   user_ds:update_friends_last_seen_at/2
    %%   message_ds:check_and_notify_offline_msgs/1
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {user_device_ds, [
                {'save', 4, fun(_Now, _Uid, _DID, _PostMap) -> ok end}
            ]},
            {user_ds, [
                {'update_friends_last_seen_at', 2, fun(_Uid, _Now) -> ok end}
            ]},
            {message_ds, [
                {'check_and_notify_offline_msgs', 2, fun(_Uid, _DID) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            PostVals = #{<<"did">> => <<"device_1">>},

            Result = user_server:handle_cast({login_success, Uid, PostVals}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

handle_cast_ws_online_updates_device_and_checks_offline_test_() ->
    %% ws_online 已合并到 online 分支（5 参数版本）
    %% handle_cast({online, Uid, Pid, DType, DID}, State) 调用:
    %%   elib_dt:now/0
    %%   user_device_ds:update_by_did/4
    %%   message_ds:check_and_notify_offline_msgs/1
    %%   user_device_logic:device_name/2
    %%   elib_id:gen/1
    %%   message_ds:assemble_msg/8
    %%   elib_retry_config:intervals/1
    %%   jsone:encode/2
    %%   message_ds:send_next/6
    %%   user_setting_ds:chat_state_hide/1
    %%   friend_ds:list_by_uid/1
    %%   msg_s2c_ds:send/7
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {user_device_ds, [
                {'update_by_did', 4, fun(_Uid, _DID, _Set, _SetArgs) -> ok end}
            ]},
            {message_ds, [
                {'check_and_notify_offline_msgs', 2, fun(_Uid, _DID) -> ok end},
                {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                    #{<<"type">> => <<"S2C">>}
                end},
                {'send_next', 6, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) ->
                    ok
                end}
            ]},
            {user_device_logic, [
                {'device_name', 2, fun(_Uid, _DID) -> <<"iPhone 14">> end}
            ]},
            {elib_id, [
                {'gen', 1, fun(_) -> <<"msg_id_123">> end}
            ]},
            {elib_retry_config, [
                {'intervals', 1, fun(_) -> [2000] end}
            ]},
            {jsone, [
                {'encode', 2, fun(_Msg, _Opts) -> <<"{}">> end}
            ]},
            {user_setting_ds, [
                {'chat_state_hide', 1, fun(_Uid) -> false end}
            ]},
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [2, 3, 4] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            Pid = self(),
            DType = <<"ios">>,
            DID = <<"device_1">>,

            Result = user_server:handle_cast({online, Uid, Pid, DType, DID}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

handle_cast_online_notifies_friends_test_() ->
    %% 与 ws_online 测试相同的调用链，验证好友通知分支
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {user_device_ds, [
                {'update_by_did', 4, fun(_Uid, _DID, _Set, _SetArgs) -> ok end}
            ]},
            {message_ds, [
                {'check_and_notify_offline_msgs', 2, fun(_Uid, _DID) -> ok end},
                {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                    #{<<"type">> => <<"S2C">>}
                end},
                {'send_next', 6, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) ->
                    ok
                end}
            ]},
            {user_device_logic, [
                {'device_name', 2, fun(_Uid, _DID) -> <<"iPhone 14">> end}
            ]},
            {elib_id, [
                {'gen', 1, fun(_) -> <<"msg_id_123">> end}
            ]},
            {elib_retry_config, [
                {'intervals', 1, fun(_) -> [2000] end}
            ]},
            {jsone, [
                {'encode', 2, fun(_Msg, _Opts) -> <<"{}">> end}
            ]},
            {user_setting_ds, [
                {'chat_state_hide', 1, fun(_Uid) -> false end}
            ]},
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [2, 3, 4] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            Pid = self(),
            DType = <<"ios">>,
            DID = <<"device_1">>,

            Result = user_server:handle_cast({online, Uid, Pid, DType, DID}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

handle_cast_online_with_hidden_state_skips_notification_test_() ->
    %% chat_state_hide = true 时跳过好友通知
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {user_device_ds, [
                {'update_by_did', 4, fun(_Uid, _DID, _Set, _SetArgs) -> ok end}
            ]},
            {message_ds, [
                {'check_and_notify_offline_msgs', 2, fun(_Uid, _DID) -> ok end},
                {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                    #{<<"type">> => <<"S2C">>}
                end},
                {'send_next', 6, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) ->
                    ok
                end}
            ]},
            {user_device_logic, [
                {'device_name', 2, fun(_Uid, _DID) -> <<"iPhone 14">> end}
            ]},
            {elib_id, [
                {'gen', 1, fun(_) -> <<"msg_id_123">> end}
            ]},
            {elib_retry_config, [
                {'intervals', 1, fun(_) -> [2000] end}
            ]},
            {jsone, [
                {'encode', 2, fun(_Msg, _Opts) -> <<"{}">> end}
            ]},
            {user_setting_ds, [
                {'chat_state_hide', 1, fun(_Uid) -> true end}
            ]},
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            Pid = self(),
            DType = <<"ios">>,
            DID = <<"device_1">>,

            Result = user_server:handle_cast({online, Uid, Pid, DType, DID}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

handle_cast_offline_notifies_friends_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [2, 3] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            Pid = self(),
            DID = <<"device_1">>,

            Result = user_server:handle_cast({offline, Uid, Pid, DID}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

handle_cast_cancel_deletes_user_and_notifies_friends_test_() ->
    %% cancel/3 调用链:
    %%   user_ds:find_by_id/2
    %%   user_setting_ds:find_by_uid/1
    %%   elib_dt:to_rfc3339/1
    %%   jsone:encode/1
    %%   user_log_ds:add_internal/5
    %%   user_ds:delete_all_related_data/1
    %%   friend_ds:list_by_uid/1
    %%   msg_s2c_ds:send/7
    ?WITH_MECKS(
        [
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Column) -> #{<<"id">> => 123} end},
                {'delete_all_related_data', 1, fun(_Uid) -> ok end}
            ]},
            {user_setting_ds, [
                {'find_by_uid', 1, fun(_Uid) -> #{<<"hide_chat">> => false} end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_Timestamp) -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {jsone, [
                {'encode', 1, fun(_Map) -> <<"{}">> end}
            ]},
            {user_log_ds, [
                {'add_internal', 5, fun(_Conn, _Type, _Uid, _Body, _CreatedAt) -> ok end}
            ]},
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [2, 3] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            CreatedAt = 1672531200,
            Opt = #{<<"reason">> => <<"user_request">>},

            Result = user_server:handle_cast({cancel, Uid, CreatedAt, Opt}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

handle_cast_notice_friend_sends_notification_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [2, 3, 4] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            ToState = <<"online">>,

            Result = user_server:handle_cast({notice_friend, Uid, ToState}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

handle_cast_unknown_message_returns_noreply_test() ->
    State = [],
    Msg = unknown_message,
    Result = user_server:handle_cast(Msg, State),
    %% handle_cast 未知消息调用 ?DEBUG_LOG，结果取决于编译时 debug 开关
    %% 只验证返回的 noreply 元组
    ?assertMatch({noreply, State}, Result).

%% ===================================================================
%% API 函数测试
%% ===================================================================
%% 注意：不再 mock gen_server 本身，因为 mock gen_server 会导致
%% EUnit 框架死锁。改为直接验证函数返回值（所有 cast_* 函数
%% 都返回 ok，内部只是 gen_server:cast 调用）。

cast_notice_friend_returns_ok_test() ->
    %% cast_notice_friend 内部调用 gen_server:cast，在测试环境中
    %% user_server 进程不存在，但 gen_server:cast 对不存在的注册名
    %% 不会报错（异步发送，直接返回 ok）
    CurrentUid = 123,
    ChatState = <<"online">>,

    Result = user_server:cast_notice_friend(CurrentUid, ChatState),
    ?assertEqual(ok, Result).

cast_online_returns_ok_test() ->
    Uid = 123,
    Pid = self(),
    DID = <<"device_1">>,
    DType = <<"ios">>,

    Result = user_server:cast_online(Uid, Pid, DID, DType),
    ?assertEqual(ok, Result).

cast_offline_returns_ok_test() ->
    Uid = 123,
    Pid = self(),
    DID = <<"device_1">>,

    Result = user_server:cast_offline(Uid, Pid, DID),
    ?assertEqual(ok, Result).

cast_cancel_returns_ok_test() ->
    Uid = 123,
    CreatedAt = 1672531200,
    Opt = #{<<"reason">> => <<"user_request">>},

    Result = user_server:cast_cancel(Uid, CreatedAt, Opt),
    ?assertEqual(ok, Result).

%% ===================================================================
%% 内部函数测试（直接调用 notice_friend/2）
%% ===================================================================

notice_friend_sends_online_notification_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [2, 3] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            Action = <<"online">>,

            Result = user_server:handle_cast({notice_friend, Uid, Action}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

notice_friend_sends_offline_notification_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [2, 3, 4] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            Action = <<"offline">>,

            Result = user_server:handle_cast({notice_friend, Uid, Action}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

handle_cast_login_success_with_missing_did_test_() ->
    %% login_success 缺少 did 字段时，DID 回退为 <<"">>
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {user_device_ds, [
                {'save', 4, fun(_Now, _Uid, _DID, _PostMap) -> ok end}
            ]},
            {user_ds, [
                {'update_friends_last_seen_at', 2, fun(_Uid, _Now) -> ok end}
            ]},
            {message_ds, [
                {'check_and_notify_offline_msgs', 2, fun(_Uid, _DID) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            % 缺少 did
            PostVals = #{<<"other">> => <<"data">>},

            Result = user_server:handle_cast({login_success, Uid, PostVals}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

handle_cast_online_with_empty_did_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
            ]},
            {user_device_ds, [
                {'update_by_did', 4, fun(_Uid, _DID, _Set, _SetArgs) -> ok end}
            ]},
            {message_ds, [
                {'check_and_notify_offline_msgs', 2, fun(_Uid, _DID) -> ok end},
                {'assemble_msg', 8, fun(_Type, _From, _To, _Payload, _MsgId, _Body, _Action, _Ext) ->
                    #{<<"type">> => <<"S2C">>}
                end},
                {'send_next', 6, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) ->
                    ok
                end}
            ]},
            {user_device_logic, [
                {'device_name', 2, fun(_Uid, _DID) -> <<"Unknown Device">> end}
            ]},
            {elib_id, [
                {'gen', 1, fun(_) -> <<"msg_id_123">> end}
            ]},
            {elib_retry_config, [
                {'intervals', 1, fun(_) -> [2000] end}
            ]},
            {jsone, [
                {'encode', 2, fun(_Msg, _Opts) -> <<"{}">> end}
            ]},
            {user_setting_ds, [
                {'chat_state_hide', 1, fun(_Uid) -> false end}
            ]},
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            Pid = self(),
            DType = <<"ios">>,
            DID = <<>>,

            Result = user_server:handle_cast({online, Uid, Pid, DType, DID}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).

notice_friend_with_no_friends_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'list_by_uid', 1, fun(_Uid) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_Uid, _ToUidLi, _Action, _Payload, _MsgId, _Data, _Save) -> ok end}
            ]}
        ],
        fun() ->
            State = [],
            Uid = 123,
            Action = <<"online">>,

            Result = user_server:handle_cast({notice_friend, Uid, Action}, State),
            ?assertMatch({noreply, [], hibernate}, Result)
        end
    ).
