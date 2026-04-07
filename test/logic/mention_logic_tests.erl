-module(mention_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% mention_logic 模块的 EUnit 测试
%%%
%%% 目标：验证@提及业务逻辑功能
%%% 覆盖：创建@记录、查询@消息、标记已读、获取成员建议
%%%===================================================================

%% ===================================================================
%% create_mentions/5 测试
%% ===================================================================

create_mentions_with_empty_list_test_() ->
    ?WITH_MECKS([
        {mention_ds, [
            {'save_mentions', 4, fun(_MsgId, _Gid, _Mentions, _FromUid) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"test_msg_1">>,
        Gid = 100,
        Mentions = [],
        FromUid = 300,
        Result = mention_logic:create_mentions(MsgId, Gid, Mentions, FromUid),
        ?assertEqual(ok, Result)
    end).

create_mentions_with_users_test_() ->
    ?WITH_MECKS([
        {mention_ds, [
            {'save_mentions', 4, fun(_MsgId, _Gid, _Mentions, _FromUid) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"test_msg_2">>,
        Gid = 100,
        Mentions = [<<"hash1">>, <<"hash2">>],
        FromUid = 300,
        Result = mention_logic:create_mentions(MsgId, Gid, Mentions, FromUid),
        ?assertEqual(ok, Result)
    end).

create_mentions_with_all_requires_admin_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'check_admin', 2, fun(_Uid, _Gid) -> false end}
        ]}
    ], fun() ->
        MsgId = <<"test_msg_3">>,
        Gid = 100,
        Mentions = [<<"all">>],
        FromUid = 300,
        Result = mention_logic:create_mentions(MsgId, Gid, Mentions, FromUid),
        ?assertEqual({error, permission_denied}, Result)
    end).

create_mentions_with_all_by_admin_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'check_admin', 2, fun(_Uid, _Gid) -> true end}
        ]},
        {mention_ds, [
            {'save_mentions', 4, fun(_MsgId, _Gid, _Mentions, _FromUid) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"test_msg_4">>,
        Gid = 100,
        Mentions = [<<"all">>],
        FromUid = 300,
        Result = mention_logic:create_mentions(MsgId, Gid, Mentions, FromUid),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% list_mentions/3 测试
%% ===================================================================

list_mentions_returns_unread_test_() ->
    ?WITH_MECK(mention_ds, [
        {'list_by_uid', 3, fun(_Uid, false, _Options) ->
            {ok, [
                #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100},
                #{<<"msg_id">> => <<"msg2">>, <<"group_id">> => 100}
            ]}
        end}
    ], fun() ->
        {ok, Results} = mention_logic:list_mentions(200, false, #{page => 1, size => 10}),
        ?assertEqual(2, length(Results))
    end).

list_mentions_returns_all_test_() ->
    ?WITH_MECK(mention_ds, [
        {'list_by_uid', 3, fun(_Uid, undefined, _Options) ->
            {ok, [
                #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100}
            ]}
        end}
    ], fun() ->
        {ok, Results} = mention_logic:list_mentions(200, undefined, #{}),
        ?assertEqual(1, length(Results))
    end).

%% ===================================================================
%% list_group_mentions/4 测试
%% ===================================================================

list_group_mentions_filters_by_group_test_() ->
    ?WITH_MECK(mention_ds, [
        {'list_by_group_and_uid', 4, fun(_Gid, _Uid, false, _Options) ->
            {ok, [
                #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100}
            ]}
        end}
    ], fun() ->
        {ok, Results} = mention_logic:list_group_mentions(100, 200, false, #{}),
        ?assertEqual(1, length(Results))
    end).

%% ===================================================================
%% mark_as_read/2 测试
%% ===================================================================

mark_as_read_success_test_() ->
    ?WITH_MECK(mention_ds, [
        {'mark_as_read', 2, fun(_MsgId, _Uid) -> ok end}
    ], fun() ->
        Result = mention_logic:mark_as_read(<<"msg1">>, 200),
        ?assertEqual(ok, Result)
    end).

mark_as_read_by_mention_id_success_test_() ->
    ?WITH_MECK(mention_ds, [
        {'mark_as_read_by_mention_id', 2, fun(_MentionId, _Uid) -> {ok, <<"msg1">>} end}
    ], fun() ->
        Result = mention_logic:mark_as_read_by_mention_id(99, 200),
        ?assertEqual({ok, <<"msg1">>}, Result)
    end).

mark_all_as_read_success_test_() ->
    ?WITH_MECK(mention_ds, [
        {'mark_all_as_read', 1, fun(_Uid) -> ok end}
    ], fun() ->
        Result = mention_logic:mark_all_as_read(200),
        ?assertEqual(ok, Result)
    end).

mark_group_as_read_success_test_() ->
    ?WITH_MECK(mention_ds, [
        {'mark_group_as_read', 2, fun(_Gid, _Uid) -> ok end}
    ], fun() ->
        Result = mention_logic:mark_group_as_read(200, 100),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% count_unread/1 测试
%% ===================================================================

count_unread_returns_count_test_() ->
    ?WITH_MECK(mention_ds, [
        {'count_unread', 1, fun(_Uid) -> 5 end}
    ], fun() ->
        Count = mention_logic:count_unread(200),
        ?assertEqual(5, Count)
    end).

count_group_unread_returns_count_test_() ->
    ?WITH_MECK(mention_ds, [
        {'count_unread_in_group', 2, fun(_Uid, _Gid) -> 3 end}
    ], fun() ->
        Count = mention_logic:count_group_unread(200, 100),
        ?assertEqual(3, Count)
    end).

%% ===================================================================
%% get_member_suggestions/3 测试
%% ===================================================================

get_member_suggestions_returns_members_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'is_member', 2, fun(_Uid, _Gid) -> true end},
            {'member_uids', 1, fun(_Gid) -> [201, 202, 203] end}
        ]},
        {user_repo, [
            {'list_by_ids', 2, fun(_Uids, _Columns) ->
                {ok, [
                    #{<<"id">> => 201, <<"nickname">> => <<"用户A"/utf8>>},
                    #{<<"id">> => 202, <<"nickname">> => <<"用户B"/utf8>>},
                    #{<<"id">> => 203, <<"nickname">> => <<"用户C"/utf8>>}
                ]}
            end}
        ]}
    ], fun() ->
        {ok, Results} = mention_logic:get_member_suggestions(100, 200, <<"用户"/utf8>>),
        ?assertEqual(3, length(Results))
    end).

get_member_suggestions_filters_by_keyword_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'is_member', 2, fun(_Uid, _Gid) -> true end},
            {'member_uids', 1, fun(_Gid) -> [201, 202, 203] end}
        ]},
        {user_repo, [
            {'list_by_ids', 2, fun(_Uids, _Columns) ->
                {ok, [
                    #{<<"id">> => 201, <<"nickname">> => <<"张三"/utf8>>},
                    #{<<"id">> => 202, <<"nickname">> => <<"李四"/utf8>>},
                    #{<<"id">> => 203, <<"nickname">> => <<"王五"/utf8>>}
                ]}
            end}
        ]}
    ], fun() ->
        {ok, Results} = mention_logic:get_member_suggestions(100, 200, <<"张"/utf8>>),
        % 只应该返回"张三"
        ?assertEqual(1, length(Results))
    end).

get_member_suggestions_requires_membership_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'is_member', 2, fun(_Uid, _Gid) -> false end}
        ]}
    ], fun() ->
        Result = mention_logic:get_member_suggestions(100, 200, <<"用户"/utf8>>),
        ?assertEqual({error, not_group_member}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

create_mentions_with_invalid_mentions_test_() ->
    ?WITH_MECKS([
        {mention_ds, [
            {'save_mentions', 4, fun(_MsgId, _Gid, _Mentions, _FromUid) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"test_msg_5">>,
        Gid = 100,
        Mentions = <<"not_a_list">>,
        FromUid = 300,
        Result = mention_logic:create_mentions(MsgId, Gid, Mentions, FromUid),
        % 应该处理非列表输入
        ?assertMatch({error, _}, Result)
    end).

get_member_suggestions_with_empty_keyword_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'is_member', 2, fun(_Uid, _Gid) -> true end},
            {'member_uids', 1, fun(_Gid) -> [201, 202] end}
        ]},
        {user_repo, [
            {'list_by_ids', 2, fun(_Uids, _Columns) ->
                {ok, [
                    #{<<"id">> => 201, <<"nickname">> => <<"用户A"/utf8>>},
                    #{<<"id">> => 202, <<"nickname">> => <<"用户B"/utf8>>}
                ]}
            end}
        ]}
    ], fun() ->
        % 空关键字应该返回所有成员
        {ok, Results} = mention_logic:get_member_suggestions(100, 200, <<>>),
        ?assertEqual(2, length(Results))
    end).
