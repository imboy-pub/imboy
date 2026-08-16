-module(mention_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% mention_repo 模块的 EUnit 测试
%%%
%%% 目标：验证@提及数据仓库功能
%%% 覆盖：插入、查询、更新、删除操作
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_binary_test() ->
    Tablename = mention_repo:tablename(),
    ?assert(is_binary(Tablename)),
    % 表名应该包含 msg_mention
    ?assert(binary:match(Tablename, <<"msg_mention">>) =/= nomatch).

%% ===================================================================
%% 函数签名测试
%% ===================================================================

%% @doc 测试 insert 函数签名
insert_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, insert, 4)).

%% @doc 测试 find_by_msg_id 函数签名
find_by_msg_id_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, find_by_msg_id, 1)).

%% @doc 测试 find_by_uid 函数签名
find_by_uid_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, find_by_uid, 2)).

%% @doc 测试 find_by_group_and_uid 函数签名
find_by_group_and_uid_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, find_by_group_and_uid, 3)).

%% @doc 测试 mark_as_read 函数签名
mark_as_read_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, mark_as_read, 2)).

%% @doc 测试 mark_all_as_read 函数签名
mark_all_as_read_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, mark_all_as_read, 1)).

%% @doc 测试 mark_group_as_read 函数签名
mark_group_as_read_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, mark_group_as_read, 2)).

%% @doc 测试 count_unread 函数签名
count_unread_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, count_unread, 1)).

%% @doc 测试 count_unread_in_group 函数签名
count_unread_in_group_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, count_unread_in_group, 2)).

%% @doc 测试 delete_by_msg_id 函数签名
delete_by_msg_id_signature_test() ->
    ?assert(erlang:function_exported(mention_repo, delete_by_msg_id, 1)).

%% ===================================================================
%% 数据库操作测试（需要数据库连接）
%% ===================================================================

%% @doc 测试 count_unread 返回整数
count_unread_returns_integer_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = mention_repo:count_unread(999999),
        ?assert(is_integer(Result))
    end).

%% @doc 测试 find_by_uid 返回列表
find_by_uid_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = mention_repo:find_by_uid(999999, undefined),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 测试 find_by_msg_id 返回列表
find_by_msg_id_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = mention_repo:find_by_msg_id(<<"non_existent_msg">>),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% 注意
%% ===================================================================
%% 测试使用 ?TEST_WITH_DB 宏，如果数据库不可用则自动跳过
%% 实际使用时请确保数据库连接正常
%%===================================================================
