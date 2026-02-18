-module(msg_forward_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_forward_logic 模块的 EUnit 测试
%%%
%%% 目标：验证消息转发业务逻辑功能
%%% 覆盖：单聊→单聊、单聊→群聊、群聊→单聊、群聊→群聊、批量转发、权限验证
%%%===================================================================

%% ===================================================================
%% 单聊 → 单聊转发测试
%% ===================================================================

forward_c2c_to_c2c_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 789 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 123, <<"to_id">> => 456, <<"msg_type">> => <<"text">>}}
            end}
        ]},
        {msg_c2c_logic, [
            {'c2c', 3, fun(_MsgId, _FromUid, _Data) -> ok end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {msg_forward_repo, [
            {'insert', 1, fun(_ForwardRecord) -> {ok, 1} end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>],
        FromUid = 123,
        ToId = 789,
        ToType = <<"c2c">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({ok, [_]}, Result)
    end).

forward_c2c_to_c2c_with_non_friend_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 789 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 123, <<"to_id">> => 456}}
            end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {false, 0} end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>],
        FromUid = 123,
        ToId = 789,
        ToType = <<"c2c">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({error, {not_friends, _}}, Result)
    end).

%% ===================================================================
%% 单聊 → 群聊转发测试
%% ===================================================================

forward_c2c_to_c2g_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_123">>) -> 1001 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 123, <<"to_id">> => 456, <<"msg_type">> => <<"text">>}}
            end}
        ]},
        {msg_c2g_logic, [
            {'c2g', 3, fun(_MsgId, _FromUid, _Data) -> ok end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(_Gid, _Uid) -> true end}
        ]},
        {msg_forward_repo, [
            {'insert', 1, fun(_ForwardRecord) -> {ok, 1} end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>],
        FromUid = 123,
        ToId = 1001,
        ToType = <<"c2g">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({ok, [_]}, Result)
    end).

forward_c2c_to_c2g_with_non_group_member_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_123">>) -> 1001 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 123, <<"to_id">> => 456}}
            end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(_Gid, _Uid) -> false end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>],
        FromUid = 123,
        ToId = 1001,
        ToType = <<"c2g">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({error, {not_group_member, _}}, Result)
    end).

%% ===================================================================
%% 群聊 → 单聊转发测试
%% ===================================================================

forward_c2g_to_c2c_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 789 end}
        ]},
        {msg_c2g_timeline_repo, [
            {'find_by_msg_id', 1, fun(_MsgId) ->
                {ok, [#{<<"from_id">> => 123, <<"to_gid">> => 1001, <<"msg_type">> => <<"text">>}]}
            end}
        ]},
        {msg_c2c_logic, [
            {'c2c', 3, fun(_MsgId, _FromUid, _Data) -> ok end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {msg_forward_repo, [
            {'insert', 1, fun(_ForwardRecord) -> {ok, 1} end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>],
        FromUid = 123,
        ToId = 789,
        ToType = <<"c2c">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({ok, [_]}, Result)
    end).

%% ===================================================================
%% 群聊 → 群聊转发测试
%% ===================================================================

forward_c2g_to_c2g_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_456">>) -> 1002 end}
        ]},
        {msg_c2g_timeline_repo, [
            {'find_by_msg_id', 1, fun(_MsgId) ->
                {ok, [#{<<"from_id">> => 123, <<"to_gid">> => 1001, <<"msg_type">> => <<"text">>}]}
            end}
        ]},
        {msg_c2g_logic, [
            {'c2g', 3, fun(_MsgId, _FromUid, _Data) -> ok end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(_Gid, _Uid) -> true end}
        ]},
        {msg_forward_repo, [
            {'insert', 1, fun(_ForwardRecord) -> {ok, 1} end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>],
        FromUid = 123,
        ToId = 1002,
        ToType = <<"c2g">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({ok, [_]}, Result)
    end).

%% ===================================================================
%% 批量转发测试
%% ===================================================================

forward_batch_messages_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 789 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 123, <<"to_id">> => 456, <<"msg_type">> => <<"text">>}}
            end}
        ]},
        {msg_c2c_logic, [
            {'c2c', 3, fun(_MsgId, _FromUid, _Data) -> ok end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {msg_forward_repo, [
            {'insert', 1, fun(_ForwardRecord) -> {ok, 1} end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>, <<"msg_124">>, <<"msg_125">>],
        FromUid = 123,
        ToId = 789,
        ToType = <<"c2c">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({ok, MsgIds}, {ok, _} = Result)
    end).

%% ===================================================================
%% 权限验证测试
%% ===================================================================

forward_with_permission_denied_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 789 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                % 消息不属于当前用户
                {ok, #{<<"from_id">> => 999, <<"to_id">> => 888}}
            end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>],
        FromUid = 123,  % 不是消息发送者或接收者
        ToId = 789,
        ToType = <<"c2c">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({error, {permission_denied, _}}, Result)
    end).

forward_msg_not_found_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 789 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {error, not_found}
            end}
        ]}
    ], fun() ->
        MsgIds = [<<"non_existent_msg">>],
        FromUid = 123,
        ToId = 789,
        ToType = <<"c2c">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({error, {msg_not_found, _}}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

forward_with_empty_msg_ids_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgIds = [],
        FromUid = 123,
        ToId = 789,
        ToType = <<"c2c">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({error, {invalid_param, _}}, Result)
    end).

forward_with_invalid_to_type_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgIds = [<<"msg_123">>],
        FromUid = 123,
        ToId = 789,
        ToType = <<"invalid_type">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        ?assertMatch({error, {invalid_param, _}}, Result)
    end).

%% ===================================================================
%% 部分失败测试
%% ===================================================================

forward_partial_success_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 789 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                case _MsgId of
                    <<"msg_123">> -> {ok, #{<<"from_id">> => 123, <<"to_id">> => 456}};
                    <<"msg_124">> -> {error, not_found};
                    <<"msg_125">> -> {ok, #{<<"from_id">> => 123, <<"to_id">> => 456}}
                end
            end}
        ]},
        {msg_c2c_logic, [
            {'c2c', 3, fun(_MsgId, _FromUid, _Data) -> ok end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {msg_forward_repo, [
            {'insert', 1, fun(_ForwardRecord) -> {ok, 1} end}
        ]}
    ], fun() ->
        MsgIds = [<<"msg_123">>, <<"msg_124">>, <<"msg_125">>],
        FromUid = 123,
        ToId = 789,
        ToType = <<"c2c">>,

        Result = msg_forward_logic:forward(MsgIds, FromUid, ToId, ToType),
        % 应该返回部分成功的结果
        ?assertMatch({ok, _}, Result)
    end).
