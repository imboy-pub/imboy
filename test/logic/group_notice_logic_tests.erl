-module(group_notice_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_notice_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群公告业务逻辑功能
%%% 覆盖：查询群公告、删除群公告、边界条件
%%%===================================================================

%% ===================================================================
%% group_notice_ds:insert/1 测试（通过 DS 层验证插入功能）
%% ===================================================================

insert_success_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'insert', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Data = #{group_id => 1, user_id => 100, title => <<"公告标题"/utf8>>, body => <<"公告内容"/utf8>>},
        Result = group_notice_ds:insert(Data),
        ?assertMatch({ok, _}, Result)
    end).

insert_with_empty_body_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'insert', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Data = #{group_id => 1, user_id => 100, title => <<"标题"/utf8>>, body => <<>>},
        Result = group_notice_ds:insert(Data),
        ?assertMatch({ok, _}, Result)
    end).

insert_with_long_content_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'insert', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        LongBody = binary:copy(<<"测试"/utf8>>, 100),
        Data = #{group_id => 1, user_id => 100, title => <<"标题"/utf8>>, body => LongBody},
        Result = group_notice_ds:insert(Data),
        ?assertMatch({ok, _}, Result)
    end).

insert_with_extra_fields_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'insert', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Data = #{
            group_id => 1,
            user_id => 100,
            title => <<"公告标题"/utf8>>,
            body => <<"公告内容"/utf8>>,
            priority => 1
        },
        Result = group_notice_ds:insert(Data),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% group_notice_logic:delete/2 测试
%% ===================================================================

delete_success_test_() ->
    ?WITH_MECKS([
        {group_notice_ds, [
            {'find_by_id', 1, fun(_Id) -> {ok, #{<<"group_id">> => 1, <<"user_id">> => 100}} end},
            {'soft_delete', 1, fun(_Id) -> ok end}
        ]},
        {group_member_ds, [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) -> {ok, #{<<"role">> => 4}} end}
        ]}
    ], fun() ->
        CurrentUid = 100,
        NoticeId = 1,
        Result = group_notice_logic:delete(CurrentUid, NoticeId),
        ?assertEqual(ok, Result)
    end).

delete_with_nonexistent_id_test_() ->
    ?WITH_MECK(group_notice_ds, [
        {'find_by_id', 1, fun(_Id) -> {error, not_found} end}
    ], fun() ->
        CurrentUid = 100,
        NoticeId = 999999,
        Result = group_notice_logic:delete(CurrentUid, NoticeId),
        ?assertMatch({error, _}, Result)
    end).

delete_without_permission_test_() ->
    ?WITH_MECKS([
        {group_notice_ds, [
            {'find_by_id', 1, fun(_Id) -> {ok, #{<<"group_id">> => 1, <<"user_id">> => 100}} end}
        ]},
        {group_member_ds, [
            {'get_member_info', 3, fun(_Gid, _Uid, _Column) -> {ok, #{<<"role">> => 1}} end}
        ]}
    ], fun() ->
        CurrentUid = 200,  % 普通成员无权限
        NoticeId = 1,
        Result = group_notice_logic:delete(CurrentUid, NoticeId),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

insert_with_special_characters_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'insert', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Data = #{
            group_id => 1,
            user_id => 100,
            title => <<"标题"/utf8>>,
            body => <<"公告@#$%^&*()内容"/utf8>>
        },
        Result = group_notice_ds:insert(Data),
        ?assertMatch({ok, _}, Result)
    end).

insert_with_multiline_content_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'insert', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Data = #{
            group_id => 1,
            user_id => 100,
            title => <<"标题"/utf8>>,
            body => <<"第一行\n第二行\n第三行"/utf8>>
        },
        Result = group_notice_ds:insert(Data),
        ?assertMatch({ok, _}, Result)
    end).
