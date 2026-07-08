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
    ?WITH_MECKS(
        [
            {mention_repo, [
                {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"test_msg_1">>,
            Gid = 100,
            Mentions = [],
            FromUid = 300,
            Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
            ?assertEqual(ok, Result)
        end
    ).

save_mentions_with_single_user_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
        ],
        fun() ->
            MsgId = <<"test_msg_2">>,
            Gid = 100,
            Mentions = [<<"101">>],
            FromUid = 300,
            Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
            ?assertEqual(ok, Result)
        end
    ).

save_mentions_with_multiple_users_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
        ],
        fun() ->
            MsgId = <<"test_msg_3">>,
            Gid = 100,
            Mentions = [<<"101">>, <<"102">>, <<"103">>],
            FromUid = 300,
            Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
            ?assertEqual(ok, Result)
        end
    ).

save_mentions_with_all_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [201, 202, 203, 204, 205] end}
            ]},
            {mention_repo, [
                {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"test_msg_4">>,
            Gid = 100,
            Mentions = [<<"all">>],
            FromUid = 300,
            Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% list_by_uid/2 测试
%% ===================================================================

list_by_uid_returns_unread_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'find_by_uid', 2, fun(_Uid, false) ->
                {ok, [
                    #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100, <<"from_uid">> => 300},
                    #{<<"msg_id">> => <<"msg2">>, <<"group_id">> => 100, <<"from_uid">> => 301}
                ]}
            end}
        ],
        fun() ->
            {ok, Results} = mention_ds:list_by_uid(200, false),
            ?assertEqual(2, length(Results))
        end
    ).

list_by_uid_with_pagination_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'find_by_uid', 4, fun(_Uid, false, _Limit, _Offset) ->
                {ok, [
                    #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100, <<"from_uid">> => 300},
                    #{<<"msg_id">> => <<"msg2">>, <<"group_id">> => 100, <<"from_uid">> => 301}
                ]}
            end}
        ],
        fun() ->
            % 测试分页参数（PERF-02：有 page 时走 /4 分页函数，LIMIT/OFFSET 下推）
            {ok, Results} = mention_ds:list_by_uid(200, false, #{page => 1, size => 10}),
            ?assertEqual(2, length(Results))
        end
    ).

%% ===================================================================
%% list_by_group_and_uid/3 测试
%% ===================================================================

list_by_group_and_uid_filters_correctly_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'find_by_group_and_uid', 3, fun(_Gid, _Uid, false) ->
                {ok, [
                    #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100}
                ]}
            end}
        ],
        fun() ->
            {ok, Results} = mention_ds:list_by_group_and_uid(100, 200, false),
            ?assertEqual(1, length(Results))
        end
    ).

%% ===================================================================
%% mark_as_read/2 测试
%% ===================================================================

mark_as_read_calls_repo_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'mark_as_read', 2, fun(_MsgId, _Uid) -> ok end}
        ],
        fun() ->
            Result = mention_ds:mark_as_read(<<"msg1">>, 200),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% count_unread/1 测试
%% ===================================================================

count_unread_returns_count_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'count_unread', 1, fun(_Uid) -> 5 end}
        ],
        fun() ->
            Count = mention_ds:count_unread(200),
            ?assertEqual(5, Count)
        end
    ).

count_unread_returns_zero_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'count_unread', 1, fun(_Uid) -> 0 end}
        ],
        fun() ->
            Count = mention_ds:count_unread(200),
            ?assertEqual(0, Count)
        end
    ).

%% ===================================================================
%% delete_by_msg_id/1 测试
%% ===================================================================

delete_by_msg_id_calls_repo_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'delete_by_msg_id', 1, fun(_MsgId) -> {ok, 2} end}
        ],
        fun() ->
            Result = mention_ds:delete_by_msg_id(<<"msg1">>),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

save_mentions_with_invalid_uid_test_() ->
    ?WITH_MECKS(
        [
            {ec_cnv, [
                {'to_integer', 1, fun(<<"invalid">>) -> 0 end}
            ]},
            {mention_repo, [
                {'insert', 4, fun(_MsgId, _Gid, _Uid, _FromUid) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"test_msg_5">>,
            Gid = 100,
            Mentions = [<<"invalid">>],
            FromUid = 300,
            Result = mention_ds:save_mentions(MsgId, Gid, Mentions, FromUid),
            % 应该跳过无效的用户ID
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% 分页测试（PERF-02：LIMIT/OFFSET 下推到 repo，非内存截取）
%% ===================================================================

list_by_uid_with_page_calls_repo_with_limit_offset_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'find_by_uid', 4, fun(_Uid, _IsRead, Limit, Offset) ->
                {ok, [#{limit => Limit, offset => Offset}]}
            end}
        ],
        fun() ->
            %% page=3, size=10 → Offset = (3-1)*10 = 20, Limit = 10
            Result = mention_ds:list_by_uid(100, undefined, #{page => 3, size => 10}),
            ?assertMatch({ok, [_]}, Result),
            {ok, [#{limit := Limit, offset := Offset}]} = Result,
            ?assertEqual(10, Limit),
            ?assertEqual(20, Offset),
            ?assertEqual(1, meck:num_calls(mention_repo, find_by_uid, 4)),
            ?assertEqual(0, meck:num_calls(mention_repo, find_by_uid, 2))
        end
    ).

list_by_uid_without_page_calls_old_repo_fn_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'find_by_uid', 2, fun(_Uid, _IsRead) -> {ok, [#{legacy => true}]} end}
        ],
        fun() ->
            %% 无 page 参数 → 走旧 /2 全量函数（向后兼容）
            Result = mention_ds:list_by_uid(100, undefined, #{}),
            ?assertMatch({ok, [#{legacy := true}]}, Result),
            ?assertEqual(1, meck:num_calls(mention_repo, find_by_uid, 2))
        end
    ).

list_by_group_and_uid_with_page_calls_repo_with_limit_offset_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'find_by_group_and_uid', 5, fun(_Gid, _Uid, _IsRead, Limit, Offset) ->
                {ok, [#{limit => Limit, offset => Offset}]}
            end}
        ],
        fun() ->
            %% page=2, size=5 → Offset=5, Limit=5
            Result = mention_ds:list_by_group_and_uid(1000, 100, true, #{page => 2, size => 5}),
            ?assertMatch({ok, [_]}, Result),
            {ok, [#{limit := Limit, offset := Offset}]} = Result,
            ?assertEqual(5, Limit),
            ?assertEqual(5, Offset),
            ?assertEqual(1, meck:num_calls(mention_repo, find_by_group_and_uid, 5)),
            ?assertEqual(0, meck:num_calls(mention_repo, find_by_group_and_uid, 3))
        end
    ).

list_by_uid_default_size_when_missing_test_() ->
    ?WITH_MECK(
        mention_repo,
        [
            {'find_by_uid', 4, fun(_Uid, _IsRead, Limit, _Offset) ->
                {ok, [#{limit => Limit}]}
            end}
        ],
        fun() ->
            %% page=1 但无 size → 默认 size=20
            Result = mention_ds:list_by_uid(100, undefined, #{page => 1}),
            ?assertMatch({ok, [#{limit := 20}]}, Result)
        end
    ).
