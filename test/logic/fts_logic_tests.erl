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
%% user_search_page/4 测试
%% ===================================================================

user_search_returns_results_test_() ->
    ?WITH_MECKS(
        [
            {fts_user_ds, [
                {'count_for_user_search_page', 1, fun(_Keyword) -> 2 end},
                {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
                    {ok, [
                        #{<<"id">> => 1, <<"nickname">> => <<"用户1"/utf8>>},
                        #{<<"id">> => 2, <<"nickname">> => <<"用户2"/utf8>>}
                    ]}
                end}
            ]},
            {friend_ds, [
                {'is_friend', 3, fun(_Uid, _Uid2, _Field) -> {false, <<>>} end}
            ]}
        ],
        fun() ->
            Keyword = <<"用户"/utf8>>,
            Limit = 10,
            Result = fts_logic:user_search_page(999, 1, Limit, Keyword),
            ?assertMatch(#{list := [_, _]}, Result)
        end
    ).

user_search_with_empty_keyword_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
        ],
        fun() ->
            Keyword = <<>>,
            Limit = 10,
            Result = fts_logic:user_search_page(1, 1, Limit, Keyword),
            ?assertMatch(#{list := [], total := 0}, Result)
        end
    ).

user_search_with_no_results_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {'count_for_user_search_page', 1, fun(_Keyword) -> 0 end},
            {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
        ],
        fun() ->
            Keyword = <<"不存在的用户"/utf8>>,
            Limit = 10,
            Result = fts_logic:user_search_page(1, 1, Limit, Keyword),
            ?assertMatch(#{list := []}, Result)
        end
    ).

user_search_with_pagination_test_() ->
    ?WITH_MECKS(
        [
            {fts_user_ds, [
                {'count_for_user_search_page', 1, fun(_Keyword) -> 2 end},
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
            ]},
            {friend_ds, [
                {'is_friend', 3, fun(_Uid, _Uid2, _Field) -> {false, <<>>} end}
            ]}
        ],
        fun() ->
            Keyword = <<"用户"/utf8>>,
            Limit = 10,
            Result = fts_logic:user_search_page(1, 2, Limit, Keyword),
            ?assertMatch(#{list := [_, _]}, Result)
        end
    ).

user_search_with_custom_limit_test_() ->
    ?WITH_MECKS(
        [
            {fts_user_ds, [
                {'count_for_user_search_page', 1, fun(_Keyword) -> 20 end},
                {'user_search_page', 3, fun(_Keyword, Limit, _Offset) ->
                    {ok, lists:duplicate(Limit, #{<<"id">> => 1, <<"nickname">> => <<"测试"/utf8>>})}
                end}
            ]},
            {friend_ds, [
                {'is_friend', 3, fun(_Uid, _Uid2, _Field) -> {false, <<>>} end}
            ]}
        ],
        fun() ->
            Keyword = <<"测试"/utf8>>,
            Limit = 20,
            Result = fts_logic:user_search_page(999, 1, Limit, Keyword),
            #{list := Users} = Result,
            ?assertEqual(20, length(Users))
        end
    ).

user_search_with_special_characters_test_() ->
    ?WITH_MECKS(
        [
            {fts_user_ds, [
                {'count_for_user_search_page', 1, fun(_Keyword) -> 1 end},
                {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
                    {ok, [#{<<"id">> => 1, <<"nickname">> => <<"用户@#$"/utf8>>}]}
                end}
            ]},
            {friend_ds, [
                {'is_friend', 3, fun(_Uid, _Uid2, _Field) -> {false, <<>>} end}
            ]}
        ],
        fun() ->
            Keyword = <<"@#$"/utf8>>,
            Limit = 10,
            Result = fts_logic:user_search_page(999, 1, Limit, Keyword),
            ?assertMatch(#{list := [_]}, Result)
        end
    ).

user_search_with_english_keyword_test_() ->
    ?WITH_MECKS(
        [
            {fts_user_ds, [
                {'count_for_user_search_page', 1, fun(_Keyword) -> 1 end},
                {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
                    {ok, [#{<<"id">> => 1, <<"nickname">> => <<"Alice"/utf8>>}]}
                end}
            ]},
            {friend_ds, [
                {'is_friend', 3, fun(_Uid, _Uid2, _Field) -> {false, <<>>} end}
            ]}
        ],
        fun() ->
            Keyword = <<"Alice">>,
            Limit = 10,
            Result = fts_logic:user_search_page(999, 1, Limit, Keyword),
            ?assertMatch(#{list := [_]}, Result)
        end
    ).

user_search_with_mixed_language_keyword_test_() ->
    ?WITH_MECKS(
        [
            {fts_user_ds, [
                {'count_for_user_search_page', 1, fun(_Keyword) -> 1 end},
                {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
                    {ok, [#{<<"id">> => 1, <<"nickname">> => <<"测试Alice"/utf8>>}]}
                end}
            ]},
            {friend_ds, [
                {'is_friend', 3, fun(_Uid, _Uid2, _Field) -> {false, <<>>} end}
            ]}
        ],
        fun() ->
            Keyword = <<"测试Alice"/utf8>>,
            Limit = 10,
            Result = fts_logic:user_search_page(999, 1, Limit, Keyword),
            ?assertMatch(#{list := [_]}, Result)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

user_search_with_zero_limit_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {'count_for_user_search_page', 1, fun(_Keyword) -> 0 end},
            {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
        ],
        fun() ->
            Keyword = <<"用户"/utf8>>,
            Limit = 0,
            Result = fts_logic:user_search_page(1, 1, Limit, Keyword),
            ?assertMatch(#{list := []}, Result)
        end
    ).

user_search_with_large_offset_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {'count_for_user_search_page', 1, fun(_Keyword) -> 0 end},
            {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
        ],
        fun() ->
            Keyword = <<"用户"/utf8>>,
            Limit = 10,
            Result = fts_logic:user_search_page(1, 100000, Limit, Keyword),
            ?assertMatch(#{list := []}, Result)
        end
    ).

user_search_with_long_keyword_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {'count_for_user_search_page', 1, fun(_Keyword) -> 0 end},
            {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
        ],
        fun() ->
            % 创建一个超长的关键词
            Keyword = binary:copy(<<"测"/utf8>>, 100),
            Limit = 10,
            Result = fts_logic:user_search_page(1, 1, Limit, Keyword),
            ?assertMatch(#{list := []}, Result)
        end
    ).

%% ===================================================================
%% search_msg/5 增强功能测试 - 按日期范围搜索
%% ===================================================================

search_msg_with_start_date_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 5, [
                    #{
                        <<"id">> => 1,
                        <<"msg_id">> => <<"message-1">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"Hello world"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"Hello">>,
            Type = <<"C2C">>,
            Options = #{<<"start_date">> => <<"2026-02-01">>},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 5, list := [#{<<"message_id">> := <<"message-1">>}]}, Result)
        end
    ).

search_msg_with_end_date_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 3, [
                    #{
                        <<"id">> => 2,
                        <<"msg_id">> => <<"message-2">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"Hi there"/utf8>>,
                        <<"created_at">> => <<"2026-02-15T10:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"Hi">>,
            Type = <<"C2C">>,
            Options = #{<<"end_date">> => <<"2026-02-16">>},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 3, list := [_]}, Result)
        end
    ).

search_msg_with_date_range_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 10, [
                    #{
                        <<"id">> => 3,
                        <<"msg_id">> => <<"message-3">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"Test message"/utf8>>,
                        <<"created_at">> => <<"2026-02-10T14:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
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
        end
    ).

%% ===================================================================
%% search_msg/5 增强功能测试 - 按消息类型搜索
%% ===================================================================

search_msg_with_msg_type_text_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 8, [
                    #{
                        <<"id">> => 4,
                        <<"msg_id">> => <<"message-4">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"Text message"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"message">>,
            Type = <<"C2C">>,
            Options = #{<<"msg_type">> => <<"text">>},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 8, list := [_]}, Result)
        end
    ).

search_msg_with_msg_type_image_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 2, [
                    #{
                        <<"id">> => 5,
                        <<"msg_id">> => <<"message-5">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"image">>,
                        <<"payload">> => <<"[图片]"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<>>,
            Type = <<"C2C">>,
            Options = #{<<"msg_type">> => <<"image">>},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 2, list := [_]}, Result)
        end
    ).

%% ===================================================================
%% search_msg/5 增强功能测试 - 按发送者搜索
%% ===================================================================

search_msg_with_from_uid_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 6, [
                    #{
                        <<"id">> => 6,
                        <<"msg_id">> => <<"message-6">>,
                        <<"from_id">> => 300,
                        <<"to_id">> => 100,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"From 300"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"From">>,
            Type = <<"C2C">>,
            Options = #{<<"from_uid">> => 300},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 6, list := [_]}, Result)
        end
    ).

%% ===================================================================
%% search_msg/5 增强功能测试 - 按会话搜索
%% ===================================================================

search_msg_with_conversation_id_c2c_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 7, [
                    #{
                        <<"id">> => 7,
                        <<"msg_id">> => <<"message-7">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"Conversation msg"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"Conversation">>,
            Type = <<"C2C">>,
            Options = #{<<"conversation_id">> => 200},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 7, list := [_]}, Result)
        end
    ).

search_msg_with_conversation_id_c2g_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 15, [
                    #{
                        <<"id">> => 8,
                        <<"msg_id">> => <<"message-8">>,
                        <<"from_id">> => 100,
                        <<"group_id">> => 500,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"Group message"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"Group">>,
            Type = <<"C2G">>,
            Options = #{<<"conversation_id">> => 500},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 15, list := [#{<<"message_id">> := <<"message-8">>}]}, Result)
        end
    ).

%% ===================================================================
%% search_msg/5 增强功能测试 - 搜索结果排序
%% ===================================================================

search_msg_sort_by_relevance_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 10, [
                    #{
                        <<"id">> => 9,
                        <<"msg_id">> => <<"message-9">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"keyword match"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    },
                    #{
                        <<"id">> => 10,
                        <<"msg_id">> => <<"message-10">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"partial match"/utf8>>,
                        <<"created_at">> => <<"2026-02-16T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"keyword">>,
            Type = <<"C2C">>,
            Options = #{<<"sort_by">> => <<"relevance">>},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 10, list := [_, _]}, Result)
        end
    ).

search_msg_sort_by_time_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 10, [
                    #{
                        <<"id">> => 11,
                        <<"msg_id">> => <<"message-11">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"newest"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    },
                    #{
                        <<"id">> => 12,
                        <<"msg_id">> => <<"message-12">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"older"/utf8>>,
                        <<"created_at">> => <<"2026-02-16T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"time">>,
            Type = <<"C2C">>,
            Options = #{<<"sort_by">> => <<"time">>},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 10, list := [_, _]}, Result)
        end
    ).

%% ===================================================================
%% search_msg/5 增强功能测试 - 组合条件搜索
%% ===================================================================

search_msg_with_combined_filters_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 3, [
                    #{
                        <<"id">> => 13,
                        <<"msg_id">> => <<"message-13">>,
                        <<"from_id">> => 300,
                        <<"to_id">> => 100,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"Combined test"/utf8>>,
                        <<"created_at">> => <<"2026-02-10T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
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
        end
    ).

%% ===================================================================
%% search_msg/5 边界条件测试
%% ===================================================================

search_msg_with_empty_options_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {ok, 5, [
                    #{
                        <<"id">> => 14,
                        <<"msg_id">> => <<"message-14">>,
                        <<"from_id">> => 100,
                        <<"to_id">> => 200,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"Empty options"/utf8>>,
                        <<"created_at">> => <<"2026-02-17T12:00:00Z">>,
                        <<"status">> => 0
                    }
                ]}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"Empty">>,
            Type = <<"C2C">>,
            Options = #{},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertMatch(#{total := 5, list := [_]}, Result)
        end
    ).

search_msg_with_invalid_date_format_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_Uid, _Keyword, _Limit, _Offset, _Type, _Options) ->
                {error, search_unavailable}
            end}
        ],
        fun() ->
            Uid = 100,
            Page = 1,
            Size = 20,
            Keyword = <<"test">>,
            Type = <<"C2C">>,
            Options = #{<<"start_date">> => <<"invalid-date">>},
            Result = fts_logic:search_msg(Uid, Page, Size, Keyword, Type, Options),
            ?assertEqual({error, search_unavailable}, Result)
        end
    ).

search_msg_returns_message_xid_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun
                (100, _, _, _, <<"c2c">>, _) ->
                    {ok, 1, [
                        #{
                            <<"id">> => 123,
                            <<"msg_id">> => <<"c2c-xid">>,
                            <<"from_id">> => 100,
                            <<"to_id">> => 200,
                            <<"msg_type">> => <<"text">>,
                            <<"payload">> => <<"hello">>,
                            <<"created_at">> => 1700000000
                        }
                    ]};
                (100, _, _, _, <<"c2g">>, _) ->
                    {ok, 1, [
                        #{
                            <<"id">> => 456,
                            <<"msg_id">> => <<"c2g-xid">>,
                            <<"from_id">> => 100,
                            <<"group_id">> => 300,
                            <<"msg_type">> => <<"text">>,
                            <<"payload">> => <<"hello">>,
                            <<"created_at">> => 1700000000
                        }
                    ]}
            end}
        ],
        fun() ->
            ?assertMatch(
                #{list := [#{<<"message_id">> := <<"c2c-xid">>}]},
                fts_logic:search_msg(100, 1, 10, <<"hello">>, <<"C2C">>)
            ),
            ?assertMatch(
                #{list := [#{<<"message_id">> := <<"c2g-xid">>}]},
                fts_logic:search_msg(100, 1, 10, <<"hello">>, <<"C2G">>)
            )
        end
    ).

search_msg_missing_xid_does_not_use_row_id_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(_, _, _, _, _, _) -> {ok, 1, [#{<<"id">> => 123}]} end}
        ],
        fun() ->
            ?assertError(
                {badkey, <<"msg_id">>},
                fts_logic:search_msg(100, 1, 10, <<"hello">>, <<"C2C">>)
            )
        end
    ).

search_msg_propagates_identity_and_failure_test_() ->
    ?WITH_MECK(
        fts_user_ds,
        [
            {search_authorized_page, 6, fun(123, <<"term">>, 7, 7, <<"c2g">>, Options) ->
                ?assertEqual(#{<<"from_uid">> => 42}, Options),
                {error, search_unavailable}
            end}
        ],
        fun() ->
            ?assertEqual(
                {error, search_unavailable},
                fts_logic:search_msg(123, 2, 7, <<"term">>, <<"C2G">>, #{<<"from_uid">> => 42})
            )
        end
    ).
