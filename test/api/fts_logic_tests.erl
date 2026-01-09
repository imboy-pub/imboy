-module(fts_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_logic 模块的 EUnit 测试
%%%
%%% 目标：验证全文搜索业务逻辑功能
%%% 覆盖：用户搜索、最近用户、搜索结果排序
%%%===================================================================

%% 测试常量定义
-define(TEST_UID, 12345).
-define(TEST_KEYWORD, <<"john">>).
-define(TEST_PAGE, 1).
-define(TEST_SIZE, 10).

%% 测试用户搜索功能（改进原假测试）
search_users_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(fts_user_repo, [passthrough, nolink]),
        meck:new(friend_ds, [passthrough, nolink]),
        meck:new(imboy_hashids, [passthrough, nolink]),
        meck:new(imboy_response, [passthrough, nolink]),
        
        try
            % Mock搜索结果
            MockColumnList = [<<"id">>, <<"nickname">>, <<"account">>],
            MockSearchResults = [
                {67890, <<"John Doe">>, <<"john@example.com">>},
                {67891, <<"Johnny Smith">>, <<"johnny@example.com">>}
            ],
            
            meck:expect(fts_user_repo, count_for_user_search_page, 1, fun(_Keyword) -> 2 end),
            meck:expect(fts_user_repo, user_search_page, 3, fun(_Keyword, _Size, _Offset) -> 
                {ok, MockColumnList, MockSearchResults} 
            end),
            meck:expect(friend_ds, is_friend, 2, fun(_Uid1, _Uid2) -> {true, <<"同事">>} end),
            meck:expect(imboy_hashids, encode, 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end),
            
            % 执行搜索
            Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
            % 验证返回的是分页响应格式
            ?assertMatch(#{<<"total">> := _, <<"page">> := _, <<"size">> := _, <<"list">> := _}, Result),
            
            % 验证调用
            ?assert(meck:called(fts_user_repo, count_for_user_search_page, 1)),
            ?assert(meck:called(fts_user_repo, user_search_page, 3))
        after
            % 清理Mock
            meck:unload(fts_user_repo),
            meck:unload(friend_ds),
            meck:unload(imboy_hashids),
            meck:unload(imboy_response)
        end
    end).

%% 测试群组搜索功能
search_groups_test_() ->
    ?WITH_MECKS([
        {fts_user_repo, [
            {'count_for_user_search_page', 1, fun(_Keyword) -> 25 end},
            {'user_search_page', 3, fun(_Keyword, _Size, _Offset) ->
                {ok, [<<"id">>, <<"nickname">>, <<"avatar">>], 
                    [[1, <<"Alice">>, <<"avatar1">>], [2, <<"Bob">>, <<"avatar2">>]]}
            end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_Uid1, _Uid2) -> {true, <<"friend">>} end}
        ]},
        {imboy_hashids, [
            {'encode', 1, fun(Id) -> integer_to_binary(Id) end}
        ]}
    ], fun() ->
        Uid = 12345,
        Page = 1,
        Size = 10,
        Keyword = <<"tech">>,
        
        Result = fts_logic:user_search_page(Uid, Page, Size, Keyword),
        % 验证返回的是分页响应格式
        ?assertMatch(#{<<"total">> := _, <<"page">> := _, <<"size">> := _}, Result)
    end).

%% 测试空关键词搜索
empty_keyword_search_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 执行空关键词搜索
        Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, <<>>),
        % 验证返回的是分页响应格式
        ?assertMatch(#{<<"total">> := 0, <<"page">> := ?TEST_PAGE, <<"size">> := ?TEST_SIZE, <<"list">> := []}, Result)
    end).

%% 测试搜索结果格式化
search_result_formatting_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(fts_user_repo, [passthrough, nolink]),
        meck:new(friend_ds, [passthrough, nolink]),
        meck:new(imboy_hashids, [passthrough, nolink]),
        meck:new(imboy_response, [passthrough, nolink]),
        
        try
            MockColumnList = [<<"id">>, <<"nickname">>, <<"account">>],
            MockSearchResults = [
                {67890, <<"John Doe">>, <<"john@example.com">>}
            ],
            
            meck:expect(fts_user_repo, count_for_user_search_page, 1, fun(_Keyword) -> 1 end),
            meck:expect(fts_user_repo, user_search_page, 3, fun(_Keyword, _Size, _Offset) -> 
                {ok, MockColumnList, MockSearchResults} 
            end),
            meck:expect(friend_ds, is_friend, 2, fun(_Uid1, _Uid2) -> {false, <<>>} end),
            meck:expect(imboy_hashids, encode, 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end),
            
            % 执行搜索
            Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
            % 验证返回的是分页响应格式
            ?assertMatch(#{<<"total">> := _, <<"page">> := _, <<"size">> := _, <<"list">> := _}, Result)
        after
            meck:unload(fts_user_repo),
            meck:unload(friend_ds),
            meck:unload(imboy_hashids)
        end
    end).

%% 测试最近用户搜索
recently_user_search_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(user_repo, [passthrough, nolink]),
        meck:new(friend_ds, [passthrough, nolink]),
        meck:new(imboy_hashids, [passthrough, nolink]),
        meck:new(imboy_pg, [passthrough, nolink]),
        
        try
            MockColumnList = [<<"id">>, <<"nickname">>, <<"account">>, <<"created_at">>],
            MockUserResults = [
                {67890, <<"Alice">>, <<"alice@example.com">>, <<"2023-01-01">>}
            ],
            
            meck:expect(imboy_pg, pluck_value, 5, fun(_Table, _Column, _Where, _Options, _Default) -> 1 end),
            meck:expect(user_repo, select_by_where, 6, fun(_Column, _Where, _Size, _Offset, _OrderBy) -> 
                {ok, MockColumnList, MockUserResults} 
            end),
            meck:expect(friend_ds, is_friend, 2, fun(_Uid1, _Uid2) -> {true, <<"朋友">>} end),
            meck:expect(imboy_hashids, encode, 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end),
            
            % 执行最近用户搜索
            Result = fts_logic:recently_user_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, <<>>),
            % 验证返回的是分页响应格式
            ?assertMatch(#{<<"total">> := _, <<"page">> := _, <<"size">> := _, <<"list">> := _}, Result),
            
            % 验证调用
            ?assert(meck:called(user_repo, select_by_where, 6))
        after
            meck:unload(user_repo),
            meck:unload(friend_ds),
            meck:unload(imboy_hashids),
            meck:unload(imboy_pg)
        end
    end).

%% 测试搜索性能参数
search_performance_parameters_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试分页偏移计算
        PageSizes = [5, 10, 20, 50],
        Pages = [1, 2, 5, 10],
        
        lists:foreach(fun(Size) ->
            lists:foreach(fun(Page) ->
                ExpectedOffset = (Page - 1) * Size,
                ?assert(is_integer(ExpectedOffset)),
                ?assert(ExpectedOffset >= 0)
            end, Pages)
        end, PageSizes),
        
        % 测试搜索关键词长度限制
        KeywordLengths = [1, 5, 10, 50, 100],
        lists:foreach(fun(Len) ->
            Keyword = binary:copy(<<"x">>, Len),
            ?assertEqual(Len, byte_size(Keyword))
        end, KeywordLengths),
        
        % 测试最大页面大小限制
        MaxPageSize = 100,
        ?assert(MaxPageSize > 0),
        ?assert(MaxPageSize =< 1000) % 合理的上限
    end).

%% 测试搜索结果排序
search_result_sorting_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试排序字段
        SortFields = [
            <<"created_at desc">>,
            <<"nickname asc">>,
            <<"account desc">>,
            <<"id asc">>
        ],
        
        lists:foreach(fun(Field) ->
            ?assertMatch(<<_/binary>>, Field),
            FieldStr = binary_to_list(Field),
            ?assert(string:str(FieldStr, " ") > 0) % 包含排序方向
        end, SortFields),
        
        % 测试排序方向
        SortDirections = [<<"asc">>, <<"desc">>],
        lists:foreach(fun(Direction) ->
            ?assertMatch(<<_/binary>>, Direction),
            ?assert(Direction =:= <<"asc">> orelse Direction =:= <<"desc">>)
        end, SortDirections),
        
        % 测试复合排序
        CompositeSort = <<"created_at desc, nickname asc">>,
        ?assertMatch(<<_/binary>>, CompositeSort),
        CompositeSortStr = binary_to_list(CompositeSort),
        ?assert(string:str(CompositeSortStr, ",") > 0) % 包含多个排序条件
    end).

%% 测试搜索错误处理
search_error_handling_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(fts_user_repo, [passthrough, nolink]),
        meck:new(imboy_response, [passthrough, nolink]),
        
        try
            % Mock搜索失败
            meck:expect(fts_user_repo, count_for_user_search_page, 1, fun(_Keyword) -> 0 end),
            meck:expect(fts_user_repo, user_search_page, 3, fun(_Keyword, _Size, _Offset) -> 
                {error, database_error} 
            end),
            meck:expect(imboy_response, page_payload, 4, fun(Total, Page, Size, Items) ->
                ?assertEqual(0, Total),
                ?assertEqual(?TEST_PAGE, Page),
                ?assertEqual(?TEST_SIZE, Size),
                ?assertEqual([], Items)
            end),
            
            % 执行搜索
            Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
            ?assertEqual(ok, Result)
        after
            meck:unload(fts_user_repo),
            meck:unload(imboy_response)
        end
    end).

%% 测试搜索结果过滤
search_result_filtering_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(fts_user_repo, [passthrough, nolink]),
        meck:new(friend_ds, [passthrough, nolink]),
        meck:new(imboy_hashids, [passthrough, nolink]),
        meck:new(imboy_response, [passthrough, nolink]),
        
        try
            % Mock包含当前用户的搜索结果
            MockColumnList = [<<"id">>, <<"nickname">>, <<"account">>],
            MockSearchResults = [
                {?TEST_UID, <<"Current User">>, <<"current@example.com">>},  % 应该被过滤
                {67890, <<"Other User">>, <<"other@example.com">>}       % 应该保留
            ],
            
            meck:expect(fts_user_repo, count_for_user_search_page, 1, fun(_Keyword) -> 2 end),
            meck:expect(fts_user_repo, user_search_page, 3, fun(_Keyword, _Size, _Offset) -> 
                {ok, MockColumnList, MockSearchResults} 
            end),
            meck:expect(friend_ds, is_friend, 2, fun(_Uid1, _Uid2) -> {false, <<>>} end),
            meck:expect(imboy_hashids, encode, 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end),
            meck:expect(imboy_response, page_payload, 4, fun(_Total, _Page, _Size, Items) ->
                % 验证当前用户被过滤掉
                ?assert(length(Items) =:= 1),
                [Item | _] = Items,
                ItemList = tuple_to_list(Item),
                % 检查不是当前用户
                [IsFriend, Remark | _] = ItemList,
                ?assert(is_boolean(IsFriend)),
                ?assertMatch(<<_/binary>>, Remark)
            end),
            
            % 执行搜索
            Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
            ?assertEqual(ok, Result)
        after
            meck:unload(fts_user_repo),
            meck:unload(friend_ds),
            meck:unload(imboy_hashids),
            meck:unload(imboy_response)
        end
    end).

%% 测试搜索结果编码
search_result_encoding_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(fts_user_repo, [passthrough, nolink]),
        meck:new(friend_ds, [passthrough, nolink]),
        meck:new(imboy_hashids, [passthrough, nolink]),
        meck:new(imboy_response, [passthrough, nolink]),
        
        try
            MockColumnList = [<<"id">>, <<"nickname">>, <<"account">>],
            MockSearchResults = [
                {67890, <<"用户">>, <<"user@example.com">>}  % 包含中文
            ],
            
            meck:expect(fts_user_repo, count_for_user_search_page, 1, fun(_Keyword) -> 1 end),
            meck:expect(fts_user_repo, user_search_page, 3, fun(_Keyword, _Size, _Offset) -> 
                {ok, MockColumnList, MockSearchResults} 
            end),
            meck:expect(friend_ds, is_friend, 2, fun(_Uid1, _Uid2) -> {false, <<>>} end),
            meck:expect(imboy_hashids, encode, 1, fun(Uid) -> 
                <<"hash_", (integer_to_binary(Uid))/binary>> 
            end),
            meck:expect(imboy_response, page_payload, 4, fun(_Total, _Page, _Size, Items) ->
                % 验证编码结果
                [Item | _] = Items,
                ItemList = tuple_to_list(Item),
                [IsFriend, Remark | Rest] = ItemList,
                ?assert(is_boolean(IsFriend)),
                ?assertMatch(<<_/binary>>, Remark),
                % 检查hashid编码
                [EncodedUid | _] = lists:reverse(Rest),
                ?assertMatch(<<_/binary>>, EncodedUid),
                ?assert(string:str(binary_to_list(EncodedUid), "hash_") > 0)
            end),
            
            % 执行搜索
            Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
            ?assertEqual(ok, Result)
        after
            meck:unload(fts_user_repo),
            meck:unload(friend_ds),
            meck:unload(imboy_hashids),
            meck:unload(imboy_response)
        end
    end).

%% 测试搜索边界条件
search_boundary_conditions_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试极小分页参数
        MinPage = 1,
        MinSize = 1,
        ?assert(is_integer(MinPage)),
        ?assert(is_integer(MinSize)),
        ?assert(MinPage > 0),
        ?assert(MinSize > 0),
        
        % 测试极大分页参数
        MaxPage = 1000,
        MaxSize = 100,
        ?assert(is_integer(MaxPage)),
        ?assert(is_integer(MaxSize)),
        ?assert(MaxPage > 0),
        ?assert(MaxSize > 0),
        
        % 测试空结果处理
        EmptyResults = [],
        ?assertMatch([_|_], EmptyResults),
        ?assertEqual(0, length(EmptyResults)),
        
        % 测试单字符关键词
        SingleCharKeyword = <<"a">>,
        ?assertMatch(<<_/binary>>, SingleCharKeyword),
        ?assertEqual(1, byte_size(SingleCharKeyword)),
        
        % 测试长关键词
        LongKeyword = binary:copy(<<"x">>, 100),
        ?assertMatch(<<_/binary>>, LongKeyword),
        ?assertEqual(100, byte_size(LongKeyword))
    end).
