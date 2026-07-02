-module(msg_c2c_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2c_ds 模块的 EUnit 测试
%%%
%%% 目标：验证C2C消息服务功能
%%% 覆盖：消息写入、撤回、已读、删除
%%%
%%% Mock 策略：mock repo 层和 lib 层，不 mock elib_pg_sql
%%%===================================================================

%% Common elib_dt mock expectations (shared across write_msg tests)
%% msg_c2c_ds:write_msg/6,8 calls elib_dt:to_rfc3339/1 to convert timestamps
-define(ELIB_DT_MOCK,
    {elib_dt, [
        {'to_rfc3339', 1, fun
            (Val) when is_integer(Val) ->
                <<"2026-01-01T00:00:00Z">>;
            (Val) when is_binary(Val) -> Val
        end}
    ]}
).

%% Common msg_c2c_repo mock for write_msg/6 tests
%% msg_c2c_repo:write_msg/8 returns ok | {error, Reason} (not {ok, Count})
-define(WRITE_MSG_REPO_MOCK,
    {msg_c2c_repo, [
        {'count_by_to_id', 1, fun(_ToUid) -> 0 end},
        {'write_msg', 8, fun(_CreatedAt, _Id, _Payload, _From, _To, _ServerTS, _MsgType, _E2EE) ->
            ok
        end}
    ]}
).

%% ===================================================================
%% 模块加载测试
%% ===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(msg_c2c_ds),
        ?assertMatch({file, _}, code:is_loaded(msg_c2c_ds))
    end).

%% ===================================================================
%% write_msg/6 测试
%% ===================================================================

write_msg_creates_message_test_() ->
    CreatedAt = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    Body = <<"Test message"/utf8>>,
    MsgId = <<"msg_test_123">>,
    ?WITH_MECKS(
        [
            ?ELIB_DT_MOCK,
            ?WRITE_MSG_REPO_MOCK
        ],
        fun() ->
            Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt),
            ?assertEqual(ok, Result)
        end
    ).

write_msg_with_utf8_content_test_() ->
    CreatedAt = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    Body = <<"测试消息"/utf8>>,
    MsgId = <<"msg_test_utf8_123">>,
    ?WITH_MECKS(
        [
            ?ELIB_DT_MOCK,
            ?WRITE_MSG_REPO_MOCK
        ],
        fun() ->
            Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt),
            ?assertEqual(ok, Result)
        end
    ).

write_msg_with_emoji_content_test_() ->
    CreatedAt = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    Body = <<"消息 😊👍"/utf8>>,
    MsgId = <<"msg_test_emoji_123">>,
    ?WITH_MECKS(
        [
            ?ELIB_DT_MOCK,
            ?WRITE_MSG_REPO_MOCK
        ],
        fun() ->
            Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt),
            ?assertEqual(ok, Result)
        end
    ).

write_msg_with_empty_body_test_() ->
    CreatedAt = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    Body = <<>>,
    MsgId = <<"msg_empty_123">>,
    ?WITH_MECKS(
        [
            ?ELIB_DT_MOCK,
            ?WRITE_MSG_REPO_MOCK
        ],
        fun() ->
            Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt),
            ?assertEqual(ok, Result)
        end
    ).

write_msg_with_large_body_test_() ->
    CreatedAt = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    LargeBody = list_to_binary(lists:duplicate(10000, $x)),
    MsgId = <<"msg_large_123">>,
    ?WITH_MECKS(
        [
            ?ELIB_DT_MOCK,
            ?WRITE_MSG_REPO_MOCK
        ],
        fun() ->
            Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, LargeBody, FromUid, ToUid, CreatedAt),
            ?assertEqual(ok, Result)
        end
    ).

%% write_msg/8 测试

write_msg_v2_creates_message_test_() ->
    CreatedAt = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    Body = <<"Test v2 message">>,
    MsgId = <<"msg_test_v2_123">>,
    MsgType = <<"text">>,
    E2EE = null,
    ?WITH_MECKS(
        [
            ?ELIB_DT_MOCK,
            ?WRITE_MSG_REPO_MOCK
        ],
        fun() ->
            Result = msg_c2c_ds:write_msg(
                CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt, MsgType, E2EE
            ),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% read_msg/2 测试
%% ===================================================================

read_msg_returns_list_test_() ->
    Uid = 1,
    Limit = 10,
    Row = #{
        <<"id">> => 100,
        <<"payload">> => <<"{\"text\":\"hello\"}">>,
        <<"from_id">> => 2,
        <<"to_id">> => 1,
        <<"created_at">> => <<"2026-01-01T00:00:00Z">>,
        <<"server_ts">> => <<"2026-01-01T00:00:01Z">>,
        <<"msg_id">> => <<"msg_123">>,
        <<"msg_type">> => <<"text">>,
        <<"e2ee">> => null
    },
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end},
                {'read_msg', 4, fun(_Where, _Column, _Limit, _Params) ->
                    {ok, [Row]}
                end}
            ]},
            {elib_response, [
                %% 实现已对 payload 与 e2ee 两列做 JSON 解码
                {'json_decode_field', 2, fun
                    (R, <<"payload">>) ->
                        R#{<<"payload">> => #{<<"text">> => <<"hello">>}};
                    (R, <<"e2ee">>) ->
                        R
                end}
            ]}
        ],
        fun() ->
            Result = msg_c2c_ds:read_msg(Uid, Limit),
            ?assert(is_list(Result)),
            ?assertEqual(1, length(Result))
        end
    ).

read_msg_with_non_existent_msg_test_() ->
    Uid = 999999,
    Limit = 10,
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end},
                {'read_msg', 4, fun(_Where, _Column, _Limit, _Params) ->
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            Result = msg_c2c_ds:read_msg(Uid, Limit),
            ?assertEqual([], Result)
        end
    ).

read_msg_with_zero_uid_test_() ->
    Uid = 0,
    Limit = 10,
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end},
                {'read_msg', 4, fun(_Where, _Column, _Limit, _Params) ->
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            Result = msg_c2c_ds:read_msg(Uid, Limit),
            ?assertEqual([], Result)
        end
    ).

%% ===================================================================
%% delete_msg/1 测试
%% ===================================================================

delete_msg_returns_ok_test_() ->
    Id = 1,
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 1, fun(_Id) -> {ok, 1} end}
        ],
        fun() ->
            Result = msg_c2c_ds:delete_msg(Id),
            ?assertEqual(ok, Result)
        end
    ).

delete_msg_with_non_existent_msg_test_() ->
    Id = 999999,
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 1, fun(_Id) -> {error, not_found} end}
        ],
        fun() ->
            Result = msg_c2c_ds:delete_msg(Id),
            ?assertEqual(ok, Result)
        end
    ).

delete_msg_with_zero_id_test_() ->
    Id = 0,
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 1, fun(_Id) -> {ok, 0} end}
        ],
        fun() ->
            Result = msg_c2c_ds:delete_msg(Id),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% find_msg_by_id/1 测试
%% ===================================================================

find_msg_by_id_existing_msg_test_() ->
    MsgId = <<"msg_123">>,
    Msg = #{
        <<"from_id">> => 1,
        <<"to_id">> => 2,
        <<"created_at">> => <<"2026-01-01">>,
        <<"payload">> => <<"{}">>
    },
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'find_msg_by_id', 1, fun(_MsgId) -> {ok, Msg} end}
        ],
        fun() ->
            Result = msg_c2c_ds:find_msg_by_id(MsgId),
            ?assertMatch({ok, _}, Result)
        end
    ).

find_msg_by_id_non_existent_msg_test_() ->
    MsgId = <<"msg_nonexist_999">>,
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'find_msg_by_id', 1, fun(_MsgId) -> {error, not_found} end}
        ],
        fun() ->
            Result = msg_c2c_ds:find_msg_by_id(MsgId),
            ?assertMatch({error, _}, Result)
        end
    ).

%% ===================================================================
%% revoke_offline_msg/5 测试
%% ===================================================================

revoke_offline_msg_success_test_() ->
    NowTs = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    MsgId = <<"msg_revoke_123">>,
    Payload = <<"test"/utf8>>,
    ?WITH_MECKS(
        [
            ?ELIB_DT_MOCK,
            {msg_c2c_repo, [
                {'count_by_to_id', 1, fun(_ToUid) -> 0 end},
                {'write_msg', 8, fun(_CreatedAt, _Id, _P, _From, _To, _ServerTS, _MsgType, _E2EE) ->
                    ok
                end},
                {'update_payload_by_msg_id', 2, fun(_MsgId, _Payload) -> ok end},
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {elib_pg, [
                {'update', 4, fun(_Tb, _Update, _Where, _Args) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_c2c_ds:revoke_offline_msg(Payload, NowTs, MsgId, FromUid, ToUid),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% edit_offline_msg/5 测试
%% ===================================================================

edit_offline_msg_success_test_() ->
    NowTs = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    MsgId = <<"msg_edit_123">>,
    NewBody = <<"Edited message"/utf8>>,
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {elib_pg, [
                {'update', 4, fun(_Tb, _Update, _Where, _Args) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_c2c_ds:edit_offline_msg(NewBody, NowTs, MsgId, FromUid, ToUid),
            ?assertEqual(ok, Result)
        end
    ).

edit_offline_msg_with_utf8_content_test_() ->
    NowTs = 1707686743435,
    FromUid = 1,
    ToUid = 2,
    MsgId = <<"msg_edit_utf8_123">>,
    NewBody = <<"编辑后的消息"/utf8>>,
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {elib_pg, [
                {'update', 4, fun(_Tb, _Update, _Where, _Args) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_c2c_ds:edit_offline_msg(NewBody, NowTs, MsgId, FromUid, ToUid),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% update_pinned/3 测试
%% ===================================================================

update_pinned_delegates_to_repo_test_() ->
    MsgId = <<"msg_pin_123">>,
    ToUid = 1,
    Pinned = true,
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'update_pinned', 3, fun(_MsgId, _ToUid, _Pinned) -> {ok, 1} end}
        ],
        fun() ->
            Result = msg_c2c_ds:update_pinned(MsgId, ToUid, Pinned),
            ?assertEqual({ok, 1}, Result)
        end
    ).

%% ===================================================================
%% count_unread_since/2 测试
%% ===================================================================

count_unread_since_returns_count_test_() ->
    ToId = 1,
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, [#{<<"count">> => 5}]} end}
            ]}
        ],
        fun() ->
            Result = msg_c2c_ds:count_unread_since(ToId, undefined),
            ?assertEqual(5, Result)
        end
    ).

count_unread_since_returns_zero_on_error_test_() ->
    ToId = 1,
    Since = <<"2026-01-01T00:00:00Z">>,
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {error, connection_failed} end}
            ]}
        ],
        fun() ->
            Result = msg_c2c_ds:count_unread_since(ToId, Since),
            ?assertEqual(0, Result)
        end
    ).
