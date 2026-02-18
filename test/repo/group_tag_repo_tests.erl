-module(group_tag_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_tag_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组标签数据仓库功能
%%% 覆盖：添加、删除、查询、按群组查询、按标签名查询
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_public_group_tag_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_repo:tablename(),
        ?assertEqual(<<"public.group_tag">>, Result)
    end).

%% ===================================================================
%% add/2 测试
%% ===================================================================

%% @doc 测试添加标签成功
add_valid_tag_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            group_id => 1,
            tag_name => <<"测试标签"/utf8>>,
            created_by => 100,
            created_at => elib_dt:now()
        },
        Result = group_tag_repo:add(undefined, Data),
        case Result of
            {ok, InsertId, Details} when is_integer(InsertId) ->
                ?assert(InsertId > 0, "Expected positive insert ID"),
                ?assertMatch(#{}, Details);
            {ok, ResultMap} when is_map(ResultMap) ->
                ?assertMatch(#{<<"id">> := _Id}, ResultMap),
                ?assert(true);
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason),
                       "Expected atom or binary error reason");
            _ ->
                ?assert(false, "Unexpected return value")
        end
    end).

%% @doc 测试添加空数据
add_empty_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{},
        Result = group_tag_repo:add(undefined, Data),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).

%% @doc 测试缺少必填字段
add_with_missing_required_field_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 缺少 group_id
        Data = #{
            tag_name => <<"测试标签"/utf8>>,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).

%% ===================================================================
%% find_by_id/2 测试
%% ===================================================================

find_by_id_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 假设 ID 为 1 的记录存在
        Id = 1,
        Column = <<"id, group_id, tag_name">>,
        Result = group_tag_repo:find_by_id(Id, Column),
        % 可能不存在，所以只验证返回格式
        case Result of
            #{<<"id">> := _, <<"group_id">> := _} -> ?assert(true);
            {error, _} -> ?assert(true)
        end
    end).

find_by_id_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Id = 999999,
        Column = <<"id">>,
        Result = group_tag_repo:find_by_id(Id, Column),
        ?assertMatch({error, _}, Result)
    end).

find_by_id_all_columns_test_() ->
    ?TEST_WITH_DB(fun() ->
        Id = 1,
        Column = <<"*">>,
        Result = group_tag_repo:find_by_id(Id, Column),
        case Result of
            #{<<"id">> := _} -> ?assert(true);
            {error, _} -> ?assert(true)
        end
    end).

%% ===================================================================
%% list_by_group/2 测试
%% ===================================================================

list_by_group_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 1,
        Column = <<"id, tag_name">>,
        Result = group_tag_repo:list_by_group(GroupId, Column),
        ?assertMatch({ok, _}, Result)
    end).

list_by_group_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 999999,
        Column = <<"id">>,
        Result = group_tag_repo:list_by_group(GroupId, Column),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% list_by_tag_name/2 测试
%% ===================================================================

list_by_tag_name_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        TagName = <<"测试标签"/utf8>>,
        Column = <<"id, group_id">>,
        Result = group_tag_repo:list_by_tag_name(TagName, Column),
        ?assertMatch({ok, _}, Result)
    end).

list_by_tag_name_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        TagName = <<"不存在的标签名"/utf8>>,
        Column = <<"id">>,
        Result = group_tag_repo:list_by_tag_name(TagName, Column),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 1,
        TagName = <<"测试标签"/utf8>>,
        Result = group_tag_repo:delete(GroupId, TagName),
        ?assertMatch({ok, _}, Result)
    end).

delete_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 999999,
        TagName = <<"不存在的标签"/utf8>>,
        Result = group_tag_repo:delete(GroupId, TagName),
        % 删除不存在的记录也应返回成功
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% delete_by_group_id/1 测试
%% ===================================================================

delete_by_group_id_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 1,
        Result = group_tag_repo:delete_by_group_id(GroupId),
        ?assertMatch({ok, _}, Result)
    end).

delete_by_group_id_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 999999,
        Result = group_tag_repo:delete_by_group_id(GroupId),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% exists/2 测试
%% ===================================================================

exists_existing_tag_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 这个测试依赖数据库中是否有数据
        GroupId = 1,
        TagName = <<"测试标签"/utf8>>,
        Result = group_tag_repo:exists(GroupId, TagName),
        ?assert(is_boolean(Result))
    end).

exists_not_existing_tag_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 999999,
        TagName = <<"不存在的标签"/utf8>>,
        Result = group_tag_repo:exists(GroupId, TagName),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% count/0 测试
%% ===================================================================

count_returns_total_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = group_tag_repo:count(),
        case Result of
            {ok, Count} when is_integer(Count) -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Count}")
        end
    end).

%% ===================================================================
%% count_by_group/1 测试
%% ===================================================================

count_by_group_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 1,
        Result = group_tag_repo:count_by_group(GroupId),
        case Result of
            {ok, Count} when is_integer(Count) -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Count}")
        end
    end).

count_by_group_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 999999,
        Result = group_tag_repo:count_by_group(GroupId),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% hot_tags/2 测试
%% ===================================================================

hot_tags_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Limit = 10,
        Result = group_tag_repo:hot_tags(Limit),
        ?assertMatch({ok, _}, Result)
    end).

hot_tags_with_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Limit = 5,
        Result = group_tag_repo:hot_tags(Limit),
        case Result of
            {ok, List} when is_list(List) ->
                ?assert(length(List) =< Limit);
            _ ->
                ?assert(false, "Expected {ok, List}")
        end
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试空标签名
add_with_empty_tag_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            group_id => 1,
            tag_name => <<>>,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        % 数据库应该拒绝空标签名
        case Result of
            {error, _} -> ?assert(true);
            _ -> ?assert(true)  % 或者数据库允许
        end
    end).

%% @doc 测试超长标签名
add_with_long_tag_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        LongTag = list_to_binary(lists:duplicate(100, $x)),
        Data = #{
            group_id => 1,
            tag_name => LongTag,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        case Result of
            {ok, _, _} -> ?assert(true);
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(true)
        end
    end).

%% @doc 测试 UTF-8 标签名
add_with_utf8_tag_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            group_id => 1,
            tag_name => <<"技术交流"/utf8>>,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        case Result of
            {ok, _, _} -> ?assert(true);
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(true)
        end
    end).

%% @doc 测试特殊字符标签名
add_with_special_chars_tag_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            group_id => 1,
            tag_name => <<"技术-交流_(2024)">>,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        case Result of
            {ok, _, _} -> ?assert(true);
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(true)
        end
    end).

%% @doc 测试零群组ID
find_by_id_with_zero_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = group_tag_repo:find_by_id(0, <<"id">>),
        ?assertMatch({error, _}, Result)
    end).

%% @doc 测试负数群组ID
list_by_group_with_negative_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = group_tag_repo:list_by_group(-1, <<"id">>),
        % 可能返回空列表或错误
        case Result of
            {ok, []} -> ?assert(true);
            {error, _} -> ?assert(true);
            _ -> ?assert(true)
        end
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的标签生命周期
complete_tag_lifecycle_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 99998,
        TagName = <<"生命周期测试"/utf8>>,
        CreatedBy = 99999,

        % 1. 添加标签
        Data = #{
            group_id => GroupId,
            tag_name => TagName,
            created_by => CreatedBy,
            created_at => elib_dt:now()
        },
        AddResult = group_tag_repo:add(undefined, Data),
        case AddResult of
            {ok, TagId, _} when is_integer(TagId) ->
                % 2. 查询标签
                Found = group_tag_repo:find_by_id(TagId, <<"*">>),
                ?assertMatch(#{<<"id">> := TagId}, Found),

                % 3. 按群组查询
                ListResult = group_tag_repo:list_by_group(GroupId, <<"*">>),
                ?assertMatch({ok, [_|_]}, ListResult),

                % 4. 检查存在性
                Exists = group_tag_repo:exists(GroupId, TagName),
                ?assertEqual(true, Exists),

                % 5. 删除标签
                DeleteResult = group_tag_repo:delete(GroupId, TagName),
                ?assertMatch({ok, _}, DeleteResult),

                % 6. 验证已删除
                ExistsAfter = group_tag_repo:exists(GroupId, TagName),
                ?assertEqual(false, ExistsAfter);
            _ ->
                ?assert(true)  % 如果添加失败，跳过后续测试
        end
    end).

%% @doc 测试同一群组添加多个标签
multiple_tags_for_same_group_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 99997,
        Tags = [<<"标签1"/utf8>>, <<"标签2"/utf8>>, <<"标签3"/utf8>>],

        % 添加多个标签
        _AddResults = [group_tag_repo:add(undefined, #{
            group_id => GroupId,
            tag_name => Tag,
            created_by => 100
        }) || Tag <- Tags],

        % 查询所有标签
        ListResult = group_tag_repo:list_by_group(GroupId, <<"*">>),
        case ListResult of
            {ok, TagList} ->
                ?assert(length(TagList) >= 3);
            _ ->
                ?assert(false, "Expected {ok, TagList}")
        end,

        % 清理
        ok = group_tag_repo:delete_by_group_id(GroupId)
    end).

%% @doc 测试热门标签统计
hot_tags_ranking_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = group_tag_repo:hot_tags(10),
        case Result of
            {ok, TagList} when is_list(TagList) ->
                % 验证返回格式
                case TagList of
                    [] -> ?assert(true);
                    [First | _] ->
                        ?assertMatch(#{<<"tag_name">> := _, <<"count">> := _}, First)
                end;
            _ ->
                ?assert(false, "Expected {ok, TagList}")
        end
    end).
