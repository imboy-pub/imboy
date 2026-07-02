-module(e2ee_transfer_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc E2EE 设备传输 DS 层测试
%%%
%%% 测试目标：
%%% - 验证传输会话管理功能
%%% - 验证缓存逻辑
%%% - 验证权限检查
%%%===================================================================

%% ===================================================================
%% 创建传输会话测试
%% ===================================================================

create_session_test_() ->
    ?WITH_MECKS(
        [
            %% 实现已改用 elib_uuid:gen_v7 生成会话 ID
            {elib_uuid, [
                {gen_v7, 0, fun() -> <<"test-session-123">> end}
            ]},
            {e2ee_transfer_repo, [
                {create, 1, fun(_SessionMap) ->
                    {ok, 999}
                end}
            ]}
        ],
        fun() ->
            FromUid = 10001,
            FromDeviceId = <<"device-001">>,
            ToUid = 10002,
            EncryptedBundle = <<"encrypted-key-bundle">>,

            Result = e2ee_transfer_ds:create_session(
                FromUid, FromDeviceId, ToUid, EncryptedBundle
            ),

            ?assertMatch(
                {ok, #{
                    <<"session_id">> := <<"test-session-123">>,
                    <<"from_uid">> := 10001,
                    <<"to_uid">> := 10002,
                    <<"from_device_id">> := <<"device-001">>
                }},
                Result
            )
        end
    ).

get_session_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_repo, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"id">> => 999,
                        <<"session_id">> => <<"test-session-456">>,
                        <<"from_uid">> => 10001,
                        <<"to_uid">> => 10002,
                        <<"status">> => <<"pending">>
                    }}
                end}
            ]},
            {imboy_cache, [
                {get, 1, fun(_CacheKey) -> undefined end},
                {set, 3, fun(_CacheKey, _Session, _TTL) -> ok end}
            ]}
        ],
        fun() ->
            SessionId = <<"test-session-456">>,

            Result = e2ee_transfer_ds:get_session(SessionId),

            ?assertMatch(
                {ok, #{
                    <<"session_id">> := <<"test-session-456">>,
                    <<"status">> := <<"pending">>
                }},
                Result
            )
        end
    ).

accept_session_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_repo, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"id">> => 999,
                        <<"session_id">> => <<"test-session-789">>,
                        <<"from_uid">> => 10001,
                        <<"to_uid">> => 10002,
                        <<"status">> => <<"pending">>
                    }}
                end},
                {update_status_and_device, 4, fun(_SessionId, _Status, _ToDeviceId, Grace) when
                    is_integer(Grace), Grace > 0
                ->
                    ok
                end}
            ]},
            {imboy_cache, [
                {get, 1, fun(_CacheKey) -> undefined end},
                {set, 3, fun(_CacheKey, _Session, _TTL) -> ok end},
                {delete, 1, fun(_CacheKey) -> ok end}
            ]}
        ],
        fun() ->
            SessionId = <<"test-session-789">>,
            ToUid = 10002,
            ToDeviceId = <<"device-002">>,

            Result = e2ee_transfer_ds:accept_session(SessionId, ToUid, ToDeviceId),

            ?assertEqual(ok, Result)
        end
    ).

accept_session_wrong_user_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_repo, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"id">> => 999,
                        <<"session_id">> => <<"test-session-abc">>,
                        <<"from_uid">> => 10001,
                        % 不同的用户
                        <<"to_uid">> => 10003,
                        <<"status">> => <<"pending">>
                    }}
                end}
            ]},
            {imboy_cache, [
                {get, 1, fun(_CacheKey) -> undefined end},
                {set, 3, fun(_CacheKey, _Session, _TTL) -> ok end}
            ]}
        ],
        fun() ->
            SessionId = <<"test-session-abc">>,
            ToUid = 10002,
            ToDeviceId = <<"device-002">>,

            Result = e2ee_transfer_ds:accept_session(SessionId, ToUid, ToDeviceId),

            ?assertMatch({error, <<"会话不属于该用户"/utf8>>}, Result)
        end
    ).

confirm_session_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_repo, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"id">> => 999,
                        <<"session_id">> => <<"test-session-def">>,
                        <<"from_uid">> => 10001,
                        <<"to_uid">> => 10002,
                        <<"status">> => <<"accepted">>
                    }}
                end},
                {update_status, 2, fun(_SessionId, _Status) ->
                    ok
                end}
            ]},
            {imboy_cache, [
                {get, 1, fun(_CacheKey) -> undefined end},
                {set, 3, fun(_CacheKey, _Session, _TTL) -> ok end},
                {delete, 1, fun(_CacheKey) -> ok end}
            ]}
        ],
        fun() ->
            SessionId = <<"test-session-def">>,
            FromUid = 10001,

            Result = e2ee_transfer_ds:confirm_session(SessionId, FromUid),

            ?assertEqual(ok, Result)
        end
    ).

confirm_session_unauthorized_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_repo, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"id">> => 999,
                        <<"session_id">> => <<"test-session-unauth">>,
                        % 不同的用户
                        <<"from_uid">> => 10002,
                        <<"to_uid">> => 10002,
                        <<"status">> => <<"accepted">>
                    }}
                end}
            ]},
            {imboy_cache, [
                {get, 1, fun(_CacheKey) -> undefined end},
                {set, 3, fun(_CacheKey, _Session, _TTL) -> ok end}
            ]}
        ],
        fun() ->
            SessionId = <<"test-session-unauth">>,
            FromUid = 10001,

            Result = e2ee_transfer_ds:confirm_session(SessionId, FromUid),

            ?assertMatch({error, <<"无权限确认此会话"/utf8>>}, Result)
        end
    ).

get_pending_sessions_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_repo, [
                {get_pending_sessions, 1, fun(_ToUid) ->
                    {ok, [
                        #{
                            <<"id">> => 1,
                            <<"session_id">> => <<"session-1">>,
                            <<"from_uid">> => 10001,
                            <<"to_uid">> => 10002
                        },
                        #{
                            <<"id">> => 2,
                            <<"session_id">> => <<"session-2">>,
                            <<"from_uid">> => 10003,
                            <<"to_uid">> => 10002
                        }
                    ]}
                end}
            ]},
            {imboy_cache, [
                {get, 1, fun(_CacheKey) -> undefined end},
                {set, 3, fun(_CacheKey, _Sessions, _TTL) -> ok end}
            ]}
        ],
        fun() ->
            ToUid = 10002,

            Result = e2ee_transfer_ds:get_pending_sessions(ToUid),

            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

cancel_session_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_repo, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"id">> => 999,
                        <<"session_id">> => <<"test-session-cancel">>,
                        <<"from_uid">> => 10001,
                        <<"to_uid">> => 10002,
                        <<"status">> => <<"pending">>
                    }}
                end},
                {update_status, 2, fun(_SessionId, _Status) ->
                    ok
                end}
            ]},
            {imboy_cache, [
                {get, 1, fun(_CacheKey) -> undefined end},
                {set, 3, fun(_CacheKey, _Session, _TTL) -> ok end},
                {delete, 1, fun(_CacheKey) -> ok end}
            ]}
        ],
        fun() ->
            SessionId = <<"test-session-cancel">>,
            FromUid = 10001,

            Result = e2ee_transfer_ds:cancel_session(SessionId, FromUid),

            ?assertEqual(ok, Result)
        end
    ).

is_valid_session_test_() ->
    ?WITH_MECK(
        e2ee_transfer_repo,
        [
            {is_valid_session, 1, fun(_SessionId) ->
                true
            end}
        ],
        fun() ->
            SessionId = <<"valid-session">>,

            Result = e2ee_transfer_ds:is_valid_session(SessionId),

            ?assertEqual(true, Result)
        end
    ).

%% 【E2EE-P2-17】cleanup_expired_sessions 测试已随 DS 双实现删除；
%% 生产清理路径 = e2ee_cleanup_worker → e2ee_transfer_repo:cleanup_expired_sessions/0
