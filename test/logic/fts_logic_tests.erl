-module(fts_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_logic 模块的 EUnit 测试
%%%
%%% 目标：验证全文搜索功能
%%% 覆盖：用户搜索、关键词匹配、分页、边界条件
%%%===================================================================

%% ===================================================================
%% user_search/3 测试
%% ===================================================================

user_search_returns_results_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [
                #{<<"id">> => 1, <<"nickname">> => <<"用户1"/utf8>>},
                #{<<"id">> => 2, <<"nickname">> => <<"用户2"/utf8>>}
            ]}
        end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_, _]}, Result)
    end).

user_search_with_empty_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Keyword = <<>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

user_search_with_no_results_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Keyword = <<"不存在的用户"/utf8>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

user_search_with_pagination_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, Limit, Offset) ->
            % 返回第二页结果
            Start = Offset + 1,
            StartBin = integer_to_binary(Start),
            StartBin2 = integer_to_binary(Start + 1),
            {ok, [
                #{<<"id">> => Start, <<"nickname">> => StartBin},
                #{<<"id">> => Start + 1, <<"nickname">> => StartBin2}
            ]}
        end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        Limit = 10,
        Offset = 10,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_, _]}, Result)
    end).

user_search_with_custom_limit_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, Limit, _Offset) ->
            {ok, lists:duplicate(Limit, #{<<"id">> => 1, <<"nickname">> => <<"测试"/utf8>>})}
        end}
    ], fun() ->
        Keyword = <<"测试"/utf8>>,
        Limit = 20,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        {ok, Users} = Result,
        ?assertEqual(20, length(Users))
    end).

user_search_with_special_characters_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"用户@#$"/utf8>>}]}
        end}
    ], fun() ->
        Keyword = <<"@#$"/utf8>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_]}, Result)
    end).

user_search_with_english_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"Alice"/utf8>>}]}
        end}
    ], fun() ->
        Keyword = <<"Alice">>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_]}, Result)
    end).

user_search_with_mixed_language_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"测试Alice"/utf8>>}]}
        end}
    ], fun() ->
        Keyword = <<"测试Alice"/utf8>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_]}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

user_search_with_zero_limit_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        Limit = 0,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

user_search_with_large_offset_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        Limit = 10,
        Offset = 999999,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

user_search_with_long_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        % 创建一个超长的关键词
        Keyword = binary:copy(<<"测"/utf8>>, 100),
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% search_msg/5 增强功能测试 - 按日期范围搜索
%% ===================================================================

search_msg_with_start_date_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            5
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 1, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"Hello world"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"Hello">>,
        Type = <<"C2C">>,
        Options = #{<<"start_date">> => <<"2026-02-01">>},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 5, list := [_]}, Result)
    end).

search_msg_with_end_date_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            3
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 2, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"Hi there"/utf8>>,
                  <<"created_at">> => <<"2026-02-15T10:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"Hi">>,
        Type = <<"C2C">>,
        Options = #{<<"end_date">> => <<"2026-02-16">>},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 3, list := [_]}, Result)
    end).

search_msg_with_date_range_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            10
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 3, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"Test message"/utf8>>,
                  <<"created_at">> => <<"2026-02-10T14:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"Test">>,
        Type = <<"C2C">>,
        Options = #{
            <<"start_date">> => <<"2026-02-01">>,
            <<"end_date">> => <<"2026-02-15">>
        },
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 10, list := [_]}, Result)
    end).

%% ===================================================================
%% search_msg/5 增强功能测试 - 按消息类型搜索
%% ===================================================================

search_msg_with_msg_type_text_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            8
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 4, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"Text message"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"message">>,
        Type = <<"C2C">>,
        Options = #{<<"msg_type">> => <<"text">>},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 8, list := [_]}, Result)
    end).

search_msg_with_msg_type_image_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            2
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 5, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"image">>, <<"payload">> => <<"[图片]"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<>>,
        Type = <<"C2C">>,
        Options = #{<<"msg_type">> => <<"image">>},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 2, list := [_]}, Result)
    end).

%% ===================================================================
%% search_msg/5 增强功能测试 - 按发送者搜索
%% ===================================================================

search_msg_with_from_uid_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            6
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 6, <<"from_id">> => 300, <<"to_id">> => 100,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"From 300"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"From">>,
        Type = <<"C2C">>,
        Options = #{<<"from_uid">> => 300},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 6, list := [_]}, Result)
    end).

%% ===================================================================
%% search_msg/5 增强功能测试 - 按会话搜索
%% ===================================================================

search_msg_with_conversation_id_c2c_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            7
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 7, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"Conversation msg"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"Conversation">>,
        Type = <<"C2C">>,
        Options = #{<<"conversation_id">> => 200},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 7, list := [_]}, Result)
    end).

search_msg_with_conversation_id_c2g_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2g_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            15
        end},
        {'search_c2g_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 8, <<"from_id">> => 100, <<"group_id">> => 500,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"Group message"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"Group">>,
        Type = <<"C2G">>,
        Options = #{<<"conversation_id">> => 500},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 15, list := [_]}, Result)
    end).

%% ===================================================================
%% search_msg/5 增强功能测试 - 搜索结果排序
%% ===================================================================

search_msg_sort_by_relevance_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            10
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 9, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"keyword match"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0},
                #{<<"id">> => 10, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"partial match"/utf8>>,
                  <<"created_at">> => <<"2026-02-16T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"keyword">>,
        Type = <<"C2C">>,
        Options = #{<<"sort_by">> => <<"relevance">>},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 10, list := [_, _]}, Result)
    end).

search_msg_sort_by_time_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            10
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 11, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"newest"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0},
                #{<<"id">> => 12, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"older"/utf8>>,
                  <<"created_at">> => <<"2026-02-16T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"time">>,
        Type = <<"C2C">>,
        Options = #{<<"sort_by">> => <<"time">>},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 10, list := [_, _]}, Result)
    end).

%% ===================================================================
%% search_msg/5 增强功能测试 - 组合条件搜索
%% ===================================================================

search_msg_with_combined_filters_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count_with_options', 2, fun(_Keyword, _Options) ->
            3
        end},
        {'search_c2c_msg_with_options', 4, fun(_Keyword, _Limit, _Offset, _Options) ->
            {ok, [
                #{<<"id">> => 13, <<"from_id">> => 300, <<"to_id">> => 100,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"Combined test"/utf8>>,
                  <<"created_at">> => <<"2026-02-10T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"Combined">>,
        Type = <<"C2C">>,
        Options = #{
            <<"start_date">> => <<"2026-02-01">>,
            <<"end_date">> => <<"2026-02-15">>,
            <<"msg_type">> => <<"text">>,
            <<"from_uid">> => 300,
            <<"conversation_id">> => 100,
            <<"sort_by">> => <<"relevance">>
        },
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 3, list := [_]}, Result)
    end).

%% ===================================================================
%% search_msg/5 边界条件测试
%% ===================================================================

search_msg_with_empty_options_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count', 1, fun(_Keyword) ->
            5
        end},
        {'search_c2c_msg', 4, fun(_Keyword, _Limit, _Offset, _Uid) ->
            {ok, [
                #{<<"id">> => 14, <<"from_id">> => 100, <<"to_id">> => 200,
                  <<"msg_type">> => <<"text">>, <<"payload">> => <<"Empty options"/utf8>>,
                  <<"created_at">> => <<"2026-02-17T12:00:00Z">>, <<"status">> => 0}
            ]}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"Empty">>,
        Type = <<"C2C">>,
        Options = #{},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        ?assertMatch(#{total := 5, list := [_]}, Result)
    end).

search_msg_with_invalid_date_format_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'search_c2c_msg_count', 1, fun(_Keyword) ->
            0
        end},
        {'search_c2c_msg', 4, fun(_Keyword, _Limit, _Offset, _Uid) ->
            {ok, []}
        end}
    ], fun() ->
        Uid = 100,
        Page = 1,
        Size = 20,
        Keyword = <<"test">>,
        Type = <<"C2C">>,
        Options = #{<<"start_date">> => <<"invalid-date">>},
        Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
        % 无效日期应该被忽略或返回空结果
        ?assertMatch(#{total := 0, list := []}, Result)
    end).
