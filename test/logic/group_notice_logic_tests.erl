-module(group_notice_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_notice_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群公告业务逻辑功能
%%% 覆盖：保存群公告、删除群公告、边界条件
%%%===================================================================

%% ===================================================================
%% save/2 测试
%% ===================================================================

save_success_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'save', 2, fun(_Conn, _Data) -> {ok, 1} end}
    ], fun() ->
        Conn = self(),
        Data = #{<<"content">> => <<"公告内容"/utf8>>, <<"group_id">> => 1},
        Result = group_notice_logic:save(Conn, Data),
        ?assertEqual({ok, 1}, Result)
    end).

save_with_empty_content_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'save', 2, fun(_Conn, _Data) -> {ok, 1} end}
    ], fun() ->
        Conn = self(),
        Data = #{<<"content">> => <<>>, <<"group_id">> => 1},
        Result = group_notice_logic:save(Conn, Data),
        ?assertMatch({ok, _}, Result)
    end).

save_with_long_content_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'save', 2, fun(_Conn, _Data) -> {ok, 1} end}
    ], fun() ->
        Conn = self(),
        LongContent = binary:copy(<<"测试"/utf8>>, 100),
        Data = #{<<"content">> => LongContent, <<"group_id">> => 1},
        Result = group_notice_logic:save(Conn, Data),
        ?assertMatch({ok, _}, Result)
    end).

save_with_extra_fields_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'save', 2, fun(_Conn, _Data) -> {ok, 1} end}
    ], fun() ->
        Conn = self(),
        Data = #{
            <<"content">> => <<"公告内容"/utf8>>,
            <<"group_id">> => 1,
            <<"priority">> => 1,
            <<"expire_at">> => <<"2023-12-31T23:59:59Z">>
        },
        Result = group_notice_logic:save(Conn, Data),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_success_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'delete', 2, fun(_Conn, _Id) -> ok end}
    ], fun() ->
        Conn = self(),
        Id = 1,
        Result = group_notice_logic:delete(Conn, Id),
        ?assertEqual(ok, Result)
    end).

delete_with_nonexistent_id_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'delete', 2, fun(_Conn, _Id) -> ok end}
    ], fun() ->
        Conn = self(),
        Id = 999999,
        Result = group_notice_logic:delete(Conn, Id),
        ?assertEqual(ok, Result)
    end).

delete_with_zero_id_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'delete', 2, fun(_Conn, _Id) -> ok end}
    ], fun() ->
        Conn = self(),
        Id = 0,
        Result = group_notice_logic:delete(Conn, Id),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

save_with_special_characters_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'save', 2, fun(_Conn, _Data) -> {ok, 1} end}
    ], fun() ->
        Conn = self(),
        Data = #{
            <<"content">> => <<"公告@#$%^&*()内容"/utf8>>,
            <<"group_id">> => 1
        },
        Result = group_notice_logic:save(Conn, Data),
        ?assertMatch({ok, _}, Result)
    end).

save_with_multiline_content_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'save', 2, fun(_Conn, _Data) -> {ok, 1} end}
    ], fun() ->
        Conn = self(),
        Data = #{
            <<"content">> => <<"第一行\n第二行\n第三行"/utf8>>,
            <<"group_id">> => 1
        },
        Result = group_notice_logic:save(Conn, Data),
        ?assertMatch({ok, _}, Result)
    end).
