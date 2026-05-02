-module(mention_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% mention_ds 模块的 EUnit 测试
%%%
%%% 目标：验证@提及数据服务功能
%%% 覆盖：创建@记录、查询@消息、标记已读、删除@记录
%%%===================================================================

%% ===================================================================
%% save_mentions/5 测试
%% ===================================================================

save_mentions_with_empty_list_test_() ->
    ?WITH_MECKS([
        {mention_repo, [
            {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"test_msg_1">>,
        Gid = 100,
        Mentions = [],
        FromUid = 300,
        Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
        ?assertEqual(ok, Result)
    end).

save_mentions_with_single_user_test_() ->
    ?WITH_MECK(mention_repo, [
        {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
    ], fun() ->
        MsgId = <<"test_msg_2">>,
        Gid = 100,
        Mentions = [<<"101">>],
        FromUid = 300,
        Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
        ?assertEqual(ok, Result)
    end).

save_mentions_with_multiple_users_test_() ->
    ?WITH_MECK(mention_repo, [
        {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
    ], fun() ->
        MsgId = <<"test_msg_3">>,
        Gid = 100,
        Mentions = [<<"101">>, <<"102">>, <<"103">>],
        FromUid = 300,
        Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
        ?assertEqual(ok, Result)
    end).

save_mentions_with_all_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_Gid) -> [201, 202, 203, 204, 205] end}
        ]},
        {mention_repo, [
            {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"test_msg_4">>,
        Gid = 100,
        Mentions = [<<"all">>],
        FromUid = 300,
        Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% list_by_uid/2 测试
%% ===================================================================

list_by_uid_returns_unread_test_() ->
    ?WITH_MECK(mention_repo, [
        {'find_by_uid', 2, fun(_Uid, false) ->
            {ok, [
                #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100, <<"from_uid">> => 300},
                #{<<"msg_id">> => <<"msg2">>, <<"group_id">> => 100, <<"from_uid">> => 301}
            ]}
        end}
    ], fun() ->
        {ok, Results} = mention_ds:list_by_uid(200, false),
        ?assertEqual(2, length(Results))
    end).

list_by_uid_with_pagination_test_() ->
    ?WITH_MECK(mention_repo, [
        {'find_by_uid', 2, fun(_Uid, false) ->
            {ok, [
                #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100, <<"from_uid">> => 300},
                #{<<"msg_id">> => <<"msg2">>, <<"group_id">> => 100, <<"from_uid">> => 301}
            ]}
        end}
    ], fun() ->
        % 测试分页参数
        {ok, Results} = mention_ds:list_by_uid(200, false, #{page => 1, size => 10}),
        ?assertEqual(2, length(Results))
    end).

%% ===================================================================
%% list_by_group_and_uid/3 测试
%% ===================================================================

list_by_group_and_uid_filters_correctly_test_() ->
    ?WITH_MECK(mention_repo, [
        {'find_by_group_and_uid', 3, fun(_Gid, _Uid, false) ->
            {ok, [
                #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100}
            ]}
        end}
    ], fun() ->
        {ok, Results} = mention_ds:list_by_group_and_uid(100, 200, false),
        ?assertEqual(1, length(Results))
    end).

%% ===================================================================
%% mark_as_read/2 测试
%% ===================================================================

mark_as_read_calls_repo_test_() ->
    ?WITH_MECK(mention_repo, [
        {'mark_as_read', 2, fun(_MsgId, _Uid) -> ok end}
    ], fun() ->
        Result = mention_ds:mark_as_read(<<"msg1">>, 200),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% count_unread/1 测试
%% ===================================================================

count_unread_returns_count_test_() ->
    ?WITH_MECK(mention_repo, [
        {'count_unread', 1, fun(_Uid) -> 5 end}
    ], fun() ->
        Count = mention_ds:count_unread(200),
        ?assertEqual(5, Count)
    end).

count_unread_returns_zero_test_() ->
    ?WITH_MECK(mention_repo, [
        {'count_unread', 1, fun(_Uid) -> 0 end}
    ], fun() ->
        Count = mention_ds:count_unread(200),
        ?assertEqual(0, Count)
    end).

%% ===================================================================
%% delete_by_msg_id/1 测试
%% ===================================================================

delete_by_msg_id_calls_repo_test_() ->
    ?WITH_MECK(mention_repo, [
        {'delete_by_msg_id', 1, fun(_MsgId) -> {ok, 2} end}
    ], fun() ->
        Result = mention_ds:delete_by_msg_id(<<"msg1">>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

save_mentions_with_invalid_uid_test_() ->
    ?WITH_MECKS([
        {ec_cnv, [
            {'to_integer', 1, fun(<<"invalid">>) -> 0 end}
        ]},
        {mention_repo, [
            {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"test_msg_5">>,
        Gid = 100,
        Mentions = [<<"invalid">>],
        FromUid = 300,
        Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
        % 应该跳过无效的用户ID
        ?assertEqual(ok, Result)
    end).
