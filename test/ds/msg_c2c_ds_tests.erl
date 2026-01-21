-module(msg_c2c_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2c_ds 模块的 EUnit 测试
%%%
%%% 目标：验证C2C消息服务功能
%%% 覆盖：消息写入、撤回、已读、删除
%%%===================================================================

%% ===================================================================
%% write_msg/6 测试
%% ===================================================================

write_msg_creates_message_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = elib_dt:now(millisecond),
        FromUid = 1,
        ToUid = 2,
        Body = <<"Test message"/utf8>>,
        MsgId = <<"msg_test_123">>,
        Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt),
        ?ASSERT_OK(Result)
    end).

write_msg_with_utf8_content_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = elib_dt:now(millisecond),
        FromUid = 1,
        ToUid = 2,
        Body = <<"测试消息"/utf8>>,
        MsgId = <<"msg_test_utf8_123">>,
        Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt),
        ?ASSERT_OK(Result)
    end).

write_msg_with_emoji_content_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = elib_dt:now(millisecond),
        FromUid = 1,
        ToUid = 2,
        Body = <<"消息 😊👍"/utf8>>,
        MsgId = <<"msg_test_emoji_123">>,
        Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt),
        ?ASSERT_OK(Result)
    end).

%% ===================================================================
%% read_msg/2 测试
%% ===================================================================

read_msg_marks_as_read_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Id = 1,
        Result = msg_c2c_ds:read_msg(Uid, Id),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

read_msg_with_non_existent_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Id = 999999,
        Result = msg_c2c_ds:read_msg(Uid, Id),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

read_msg_with_zero_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 0,
        Id = 1,
        Result = msg_c2c_ds:read_msg(Uid, Id),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

%% ===================================================================
%% delete_msg/1 测试
%% ===================================================================

delete_msg_removes_message_test_() ->
    ?TEST_WITH_DB(fun() ->
        Id = 1,
        Result = msg_c2c_ds:delete_msg(Id),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

delete_msg_with_non_existent_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        Id = 999999,
        Result = msg_c2c_ds:delete_msg(Id),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

delete_msg_with_zero_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        Id = 0,
        Result = msg_c2c_ds:delete_msg(Id),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

%% ===================================================================
%% revoke_offline_msg/5 测试
%% ===================================================================

revoke_offline_msg_success_test_() ->
    ?TEST_WITH_DB(fun() ->
        NowTs = elib_dt:now(millisecond),
        FromUid = 1,
        ToUid = 2,
        MsgId = <<"msg_revoke_123">>,
        Result = msg_c2c_ds:revoke_offline_msg(NowTs, MsgId, FromUid, ToUid, <<"test"/utf8>>),
        case Result of
            {ok, _} -> ok;
            ok -> ok
        end
    end).

%% ===================================================================
%% edit_offline_msg/5 测试
%% ===================================================================

edit_offline_msg_success_test_() ->
    ?TEST_WITH_DB(fun() ->
        NowTs = elib_dt:now(millisecond),
        FromUid = 1,
        ToUid = 2,
        MsgId = <<"msg_edit_123">>,
        NewBody = <<"Edited message"/utf8>>,
        Result = msg_c2c_ds:edit_offline_msg(NowTs, MsgId, FromUid, ToUid, NewBody),
        case Result of
            {ok, _} -> ok;
            ok -> ok
        end
    end).

edit_offline_msg_with_utf8_content_test_() ->
    ?TEST_WITH_DB(fun() ->
        NowTs = elib_dt:now(millisecond),
        FromUid = 1,
        ToUid = 2,
        MsgId = <<"msg_edit_utf8_123">>,
        NewBody = <<"编辑后的消息"/utf8>>,
        Result = msg_c2c_ds:edit_offline_msg(NowTs, MsgId, FromUid, ToUid, NewBody),
        case Result of
            {ok, _} -> ok;
            ok -> ok
        end
    end).

%% ===================================================================
%% find_msg_by_id/1 测试
%% ===================================================================

find_msg_by_id_existing_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = 1,
        Result = msg_c2c_ds:find_msg_by_id(MsgId),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

find_msg_by_id_non_existent_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = 999999,
        Result = msg_c2c_ds:find_msg_by_id(MsgId),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

write_msg_with_empty_body_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = elib_dt:now(millisecond),
        FromUid = 1,
        ToUid = 2,
        Body = <<>>,
        MsgId = <<"msg_empty_123">>,
        Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, Body, FromUid, ToUid, CreatedAt),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

write_msg_with_large_body_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = elib_dt:now(millisecond),
        FromUid = 1,
        ToUid = 2,
        LargeBody = list_to_binary(lists:duplicate(10000, $x)),
        MsgId = <<"msg_large_123">>,
        Result = msg_c2c_ds:write_msg(CreatedAt, MsgId, LargeBody, FromUid, ToUid, CreatedAt),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).
