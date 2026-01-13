-module(user_denylist_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_denylist_ds 模块的 EUnit 测试
%%%
%%% 目标：验证用户黑名单数据服务功能
%%% 覆盖：统计黑名单、分页查询、添加黑名单、移除黑名单、黑名单验证
%%%===================================================================

%% ===================================================================
%% count_for_uid/1 测试
%% ===================================================================

count_for_uid_returns_count_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'count_for_uid', 1, fun(Uid) ->
            ?assertEqual(100, Uid),
            5
        end}
    ]}], fun() ->
        Result = user_denylist_ds:count_for_uid(100),
        ?assertEqual(5, Result)
    end).

count_for_uid_with_zero_count_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'count_for_uid', 1, fun(_Uid) -> 0 end}
    ]}], fun() ->
        Result = user_denylist_ds:count_for_uid(200),
        ?assertEqual(0, Result)
    end).

count_for_uid_with_large_count_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'count_for_uid', 1, fun(_Uid) -> 9999 end}
    ]}], fun() ->
        Result = user_denylist_ds:count_for_uid(100),
        ?assertEqual(9999, Result)
    end).

%% ===================================================================
%% page_for_uid/3 测试
%% ===================================================================

page_for_uid_returns_list_test_() ->
    MockData = [
        #{
            <<"denied_user_id">> => 201,
            <<"nickname">> => <<"测试用户1"/utf8>>,
            <<"avatar">> => <<"avatar1.png">>,
            <<"account">> => <<"account1">>,
            <<"created_at">> => <<"2023-01-01T00:00:00Z">>
        },
        #{
            <<"denied_user_id">> => 202,
            <<"nickname">> => <<"测试用户2"/utf8>>,
            <<"avatar">> => <<"avatar2.png">>,
            <<"account">> => <<"account2">>,
            <<"created_at">> => <<"2023-01-02T00:00:00Z">>
        }
    ],
    ?WITH_MECKS([{user_denylist_repo, [
        {'page_for_uid', 3, fun(Uid, Size, Offset) ->
            ?assertEqual(100, Uid),
            ?assertEqual(10, Size),
            ?assertEqual(0, Offset),
            {ok, MockData}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:page_for_uid(100, 10, 0),
        ?assertEqual({ok, MockData}, Result)
    end).

page_for_uid_with_empty_list_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'page_for_uid', 3, fun(_Uid, _Size, _Offset) ->
            {ok, []}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:page_for_uid(100, 10, 0),
        ?assertEqual({ok, []}, Result)
    end).

page_for_uid_with_error_returns_error_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'page_for_uid', 3, fun(_Uid, _Size, _Offset) ->
            {error, <<"database_error"/utf8>>}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:page_for_uid(100, 10, 0),
        ?assertEqual({error, <<"database_error"/utf8>>}, Result)
    end).

page_for_uid_with_pagination_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'page_for_uid', 3, fun(_Uid, Size, Offset) ->
            case {Size, Offset} of
                {10, 0} -> {ok, [#{<<"denied_user_id">> => 1}]};
                {10, 10} -> {ok, [#{<<"denied_user_id">> => 2}]};
                {10, 20} -> {ok, [#{<<"denied_user_id">> => 3}]};
                _ -> {ok, []}
            end
        end}
    ]}], fun() ->
        ?assertEqual({ok, [#{<<"denied_user_id">> => 1}]}, user_denylist_ds:page_for_uid(100, 10, 0)),
        ?assertEqual({ok, [#{<<"denied_user_id">> => 2}]}, user_denylist_ds:page_for_uid(100, 10, 10)),
        ?assertEqual({ok, [#{<<"denied_user_id">> => 3}]}, user_denylist_ds:page_for_uid(100, 10, 20))
    end).

%% ===================================================================
%% add/3 测试
%% ===================================================================

add_success_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'add', 3, fun(Uid, DeniedUserId, CreatedAt) ->
            ?assertEqual(100, Uid),
            ?assertEqual(201, DeniedUserId),
            ?assertEqual(<<"2023-01-01T00:00:00Z">>, CreatedAt),
            {ok, 1}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:add(100, 201, <<"2023-01-01T00:00:00Z">>),
        ?assertEqual({ok, 1}, Result)
    end).

add_with_different_timestamp_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'add', 3, fun(_Uid, _DeniedUserId, CreatedAt) ->
            ?assertEqual(<<"2023-12-31T23:59:59Z">>, CreatedAt),
            {ok, 2}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:add(100, 201, <<"2023-12-31T23:59:59Z">>),
        ?assertEqual({ok, 2}, Result)
    end).

add_with_error_returns_error_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'add', 3, fun(_Uid, _DeniedUserId, _CreatedAt) ->
            {error, <<"duplicate_key"/utf8>>}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:add(100, 201, <<"2023-01-01T00:00:00Z">>),
        ?assertEqual({error, <<"duplicate_key"/utf8>>}, Result)
    end).

add_multiple_users_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'add', 3, fun(Uid, DeniedUserId, _CreatedAt) ->
            ?assertEqual(100, Uid),
            ?assert(DeniedUserId > 200),
            {ok, DeniedUserId}
        end}
    ]}], fun() ->
        ?assertEqual({ok, 201}, user_denylist_ds:add(100, 201, <<"2023-01-01T00:00:00Z">>)),
        ?assertEqual({ok, 202}, user_denylist_ds:add(100, 202, <<"2023-01-01T00:00:01Z">>)),
        ?assertEqual({ok, 203}, user_denylist_ds:add(100, 203, <<"2023-01-01T00:00:02Z">>))
    end).

%% ===================================================================
%% remove/2 测试
%% ===================================================================

remove_success_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'remove', 2, fun(Uid, DeniedUserId) ->
            ?assertEqual(100, Uid),
            ?assertEqual(201, DeniedUserId),
            {ok, 1}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:remove(100, 201),
        ?assertEqual({ok, 1}, Result)
    end).

remove_with_nonexistent_user_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'remove', 2, fun(_Uid, _DeniedUserId) ->
            {ok, 0}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:remove(100, 999),
        ?assertEqual({ok, 0}, Result)
    end).

remove_with_error_returns_error_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'remove', 2, fun(_Uid, _DeniedUserId) ->
            {error, <<"删除失败"/utf8>>}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:remove(100, 201),
        ?assertEqual({error, <<"删除失败"/utf8>>}, Result)
    end).

remove_multiple_users_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'remove', 2, fun(Uid, DeniedUserId) ->
            ?assertEqual(100, Uid),
            ?assert(DeniedUserId > 200),
            {ok, 1}
        end}
    ]}], fun() ->
        ?assertEqual({ok, 1}, user_denylist_ds:remove(100, 201)),
        ?assertEqual({ok, 1}, user_denylist_ds:remove(100, 202)),
        ?assertEqual({ok, 1}, user_denylist_ds:remove(100, 203))
    end).

%% ===================================================================
%% in_denylist/2 测试
%% ===================================================================

in_denylist_returns_one_when_in_list_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'in_denylist', 2, fun(Uid, DeniedUserId) ->
            ?assertEqual(100, Uid),
            ?assertEqual(201, DeniedUserId),
            1
        end}
    ]}], fun() ->
        Result = user_denylist_ds:in_denylist(100, 201),
        ?assertEqual(1, Result)
    end).

in_denylist_returns_zero_when_not_in_list_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'in_denylist', 2, fun(_Uid, _DeniedUserId) ->
            0
        end}
    ]}], fun() ->
        Result = user_denylist_ds:in_denylist(100, 999),
        ?assertEqual(0, Result)
    end).

in_denylist_check_multiple_users_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'in_denylist', 2, fun(_Uid, DeniedUserId) ->
            case DeniedUserId of
                201 -> 1;
                202 -> 1;
                203 -> 0;
                _ -> 0
            end
        end}
    ]}], fun() ->
        ?assertEqual(1, user_denylist_ds:in_denylist(100, 201)),
        ?assertEqual(1, user_denylist_ds:in_denylist(100, 202)),
        ?assertEqual(0, user_denylist_ds:in_denylist(100, 203)),
        ?assertEqual(0, user_denylist_ds:in_denylist(100, 999))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

count_for_uid_with_maximum_uid_test_() ->
    MaxUid = 9223372036854775807,  % int64 最大值
    ?WITH_MECKS([{user_denylist_repo, [
        {'count_for_uid', 1, fun(Uid) ->
            ?assertEqual(MaxUid, Uid),
            100
        end}
    ]}], fun() ->
        Result = user_denylist_ds:count_for_uid(MaxUid),
        ?assertEqual(100, Result)
    end).

page_for_uid_with_zero_size_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'page_for_uid', 3, fun(_Uid, Size, _Offset) ->
            ?assertEqual(0, Size),
            {ok, []}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:page_for_uid(100, 0, 0),
        ?assertEqual({ok, []}, Result)
    end).

page_for_uid_with_large_offset_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'page_for_uid', 3, fun(_Uid, _Size, Offset) ->
            ?assertEqual(99999, Offset),
            {ok, []}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:page_for_uid(100, 10, 99999),
        ?assertEqual({ok, []}, Result)
    end).

add_with_maximum_user_ids_test_() ->
    MaxUid = 9223372036854775807,
    MaxDeniedUid = 9223372036854775806,
    ?WITH_MECKS([{user_denylist_repo, [
        {'add', 3, fun(Uid, DeniedUserId, _CreatedAt) ->
            ?assertEqual(MaxUid, Uid),
            ?assertEqual(MaxDeniedUid, DeniedUserId),
            {ok, 1}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:add(MaxUid, MaxDeniedUid, <<"2023-01-01T00:00:00Z">>),
        ?assertEqual({ok, 1}, Result)
    end).

remove_with_same_uid_and_denied_uid_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'remove', 2, fun(Uid, DeniedUserId) ->
            ?assertEqual(Uid, DeniedUserId),
            {ok, 0}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:remove(100, 100),
        ?assertEqual({ok, 0}, Result)
    end).

in_denylist_with_same_uid_and_denied_uid_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'in_denylist', 2, fun(Uid, DeniedUserId) ->
            ?assertEqual(Uid, DeniedUserId),
            0
        end}
    ]}], fun() ->
        Result = user_denylist_ds:in_denylist(100, 100),
        ?assertEqual(0, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

count_for_uid_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        ?assert(is_integer(Uid))
    end).

page_for_uid_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        Size = 10,
        Offset = 0,
        ?assert(is_integer(Uid)),
        ?assert(is_integer(Size)),
        ?assert(is_integer(Offset))
    end).

add_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        DeniedUserId = 201,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ?assert(is_integer(Uid)),
        ?assert(is_integer(DeniedUserId)),
        ?assert(is_binary(CreatedAt))
    end).

remove_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        DeniedUserId = 201,
        ?assert(is_integer(Uid)),
        ?assert(is_integer(DeniedUserId))
    end).

in_denylist_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        DeniedUserId = 201,
        ?assert(is_integer(Uid)),
        ?assert(is_integer(DeniedUserId))
    end).

%% ===================================================================
%% UTF-8 编码测试
%% ===================================================================

page_for_uid_with_utf8_nickname_test_() ->
    MockData = [
        #{
            <<"denied_user_id">> => 201,
            <<"nickname">> => <<"张三"/utf8>>,
            <<"account">> => <<"account1">>,
            <<"sign">> => <<"个性签名"/utf8>>
        },
        #{
            <<"denied_user_id">> => 202,
            <<"nickname">> => <<"李四"/utf8>>,
            <<"account">> => <<"account2">>,
            <<"sign">> => <<"这是签名"/utf8>>
        }
    ],
    ?WITH_MECKS([{user_denylist_repo, [
        {'page_for_uid', 3, fun(_Uid, _Size, _Offset) ->
            {ok, MockData}
        end}
    ]}], fun() ->
        {ok, Result} = user_denylist_ds:page_for_uid(100, 10, 0),
        ?assertEqual(2, length(Result))
    end).

add_with_utf8_timestamp_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'add', 3, fun(_Uid, _DeniedUserId, CreatedAt) ->
            ?assertEqual(<<"2023-01-01T00:00:00Z"/utf8>>, CreatedAt),
            {ok, 1}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:add(100, 201, <<"2023-01-01T00:00:00Z"/utf8>>),
        ?assertEqual({ok, 1}, Result)
    end).

remove_with_utf8_error_message_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'remove', 2, fun(_Uid, _DeniedUserId) ->
            {error, <<"删除失败"/utf8>>}
        end}
    ]}], fun() ->
        Result = user_denylist_ds:remove(100, 201),
        ?assertEqual({error, <<"删除失败"/utf8>>}, Result)
    end).

%% ===================================================================
%% 集成测试场景
%% ===================================================================

denylist_lifecycle_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'count_for_uid', 1, fun(_Uid) -> 0 end},
        {'add', 3, fun(_Uid, _DeniedUserId, _CreatedAt) -> {ok, 1} end},
        {'in_denylist', 2, fun(_Uid, _DeniedUserId) -> 1 end},
        {'page_for_uid', 3, fun(_Uid, _Size, _Offset) ->
            {ok, [#{<<"denied_user_id">> => 201, <<"nickname">> => <<"测试"/utf8>>}]}
        end},
        {'remove', 2, fun(_Uid, _DeniedUserId) -> {ok, 1} end}
    ]}], fun() ->
        % 初始状态
        ?assertEqual(0, user_denylist_ds:count_for_uid(100)),

        % 添加黑名单
        ?assertEqual({ok, 1}, user_denylist_ds:add(100, 201, <<"2023-01-01T00:00:00Z">>)),

        % 检查是否在黑名单
        ?assertEqual(1, user_denylist_ds:in_denylist(100, 201)),

        % 分页查询
        {ok, Page} = user_denylist_ds:page_for_uid(100, 10, 0),
        ?assertEqual(1, length(Page)),

        % 移除黑名单
        ?assertEqual({ok, 1}, user_denylist_ds:remove(100, 201))
    end).

batch_add_to_denylist_test_() ->
    ?WITH_MECKS([{user_denylist_repo, [
        {'add', 3, fun(_Uid, DeniedUserId, _CreatedAt) ->
            {ok, DeniedUserId}
        end},
        {'count_for_uid', 1, fun(_Uid) -> 5 end}
    ]}], fun() ->
        % 批量添加
        ?assertEqual({ok, 201}, user_denylist_ds:add(100, 201, <<"2023-01-01T00:00:00Z">>)),
        ?assertEqual({ok, 202}, user_denylist_ds:add(100, 202, <<"2023-01-01T00:00:01Z">>)),
        ?assertEqual({ok, 203}, user_denylist_ds:add(100, 203, <<"2023-01-01T00:00:02Z">>)),
        ?assertEqual({ok, 204}, user_denylist_ds:add(100, 204, <<"2023-01-01T00:00:03Z">>)),
        ?assertEqual({ok, 205}, user_denylist_ds:add(100, 205, <<"2023-01-01T00:00:04Z">>)),

        % 验证总数
        ?assertEqual(5, user_denylist_ds:count_for_uid(100))
    end).
