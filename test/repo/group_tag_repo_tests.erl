-module(group_tag_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_tag_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组标签数据仓库功能
%%% 使用 meck mock，不依赖真实数据库
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_public_group_tag_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        Result = group_tag_repo:tablename(),
        ?assertEqual(<<"public.group_tag">>, Result)
    end).

%% ===================================================================
%% add/2 测试
%% ===================================================================

%% @doc 测试添加标签成功
add_valid_tag_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 700001 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Data = #{
            group_id => 1,
            tag_name => <<"测试标签"/utf8>>,
            created_by => 100,
            created_at => 1700000000
        },
        Result = group_tag_repo:add(undefined, Data),
        ?assertEqual({ok, 700001}, Result)
    end).

%% @doc 测试添加空数据（数据库拒绝）
add_empty_map_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 700002 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {error, not_null_violation}
            end}
        ]}
    ], fun() ->
        Data = #{},
        Result = group_tag_repo:add(undefined, Data),
        ?assertMatch({error, _}, Result)
    end).

%% @doc 测试缺少必填字段
add_with_missing_required_field_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 700003 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {error, not_null_violation}
            end}
        ]}
    ], fun() ->
        % 缺少 group_id
        Data = #{
            tag_name => <<"测试标签"/utf8>>,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% find_by_id/2 测试
%% ===================================================================

find_by_id_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"id">> => 1, <<"group_id">> => 100, <<"tag_name">> => <<"test">>}}
            end}
        ]}
    ], fun() ->
        Id = 1,
        Column = <<"id, group_id, tag_name">>,
        Result = group_tag_repo:find_by_id(Id, Column),
        ?assertMatch(#{<<"id">> := _, <<"group_id">> := _}, Result)
    end).

find_by_id_not_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
        ]}
    ], fun() ->
        Id = 999999,
        Column = <<"id">>,
        Result = group_tag_repo:find_by_id(Id, Column),
        ?assertMatch({error, _}, Result)
    end).

find_by_id_all_columns_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"id">> => 1, <<"group_id">> => 100, <<"tag_name">> => <<"test">>}}
            end}
        ]}
    ], fun() ->
        Id = 1,
        Column = <<"*">>,
        Result = group_tag_repo:find_by_id(Id, Column),
        ?assertMatch(#{<<"id">> := _}, Result)
    end).

%% ===================================================================
%% list_by_group/2 测试
%% ===================================================================

list_by_group_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"id">> => 1, <<"tag_name">> => <<"test">>}]}
            end}
        ]}
    ], fun() ->
        GroupId = 1,
        Column = <<"id, tag_name">>,
        Result = group_tag_repo:list_by_group(GroupId, Column),
        ?assertMatch({ok, _}, Result)
    end).

list_by_group_not_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
        ]}
    ], fun() ->
        GroupId = 999999,
        Column = <<"id">>,
        Result = group_tag_repo:list_by_group(GroupId, Column),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% list_by_tag_name/2 测试
%% ===================================================================

list_by_tag_name_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"id">> => 1, <<"group_id">> => 100}]}
            end}
        ]}
    ], fun() ->
        TagName = <<"测试标签"/utf8>>,
        Column = <<"id, group_id">>,
        Result = group_tag_repo:list_by_tag_name(TagName, Column),
        ?assertMatch({ok, _}, Result)
    end).

list_by_tag_name_not_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
        ]}
    ], fun() ->
        TagName = <<"不存在的标签名"/utf8>>,
        Column = <<"id">>,
        Result = group_tag_repo:list_by_tag_name(TagName, Column),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        GroupId = 1,
        TagName = <<"测试标签"/utf8>>,
        Result = group_tag_repo:delete(GroupId, TagName),
        ?assertMatch({ok, _}, Result)
    end).

delete_not_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        GroupId = 999999,
        TagName = <<"不存在的标签"/utf8>>,
        Result = group_tag_repo:delete(GroupId, TagName),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% delete_by_group_id/1 测试
%% ===================================================================

delete_by_group_id_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 2} end}
        ]}
    ], fun() ->
        GroupId = 1,
        Result = group_tag_repo:delete_by_group_id(GroupId),
        ?assertMatch({ok, _}, Result)
    end).

delete_by_group_id_not_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        GroupId = 999999,
        Result = group_tag_repo:delete_by_group_id(GroupId),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% exists/2 测试
%% ===================================================================

exists_existing_tag_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'pluck_value', 4, fun(_Tb, _Col, _Where, _Default) -> 1 end}
        ]}
    ], fun() ->
        GroupId = 1,
        TagName = <<"测试标签"/utf8>>,
        Result = group_tag_repo:exists(GroupId, TagName),
        ?assertEqual(true, Result)
    end).

exists_not_existing_tag_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'pluck_value', 4, fun(_Tb, _Col, _Where, _Default) -> 0 end}
        ]}
    ], fun() ->
        GroupId = 999999,
        TagName = <<"不存在的标签"/utf8>>,
        Result = group_tag_repo:exists(GroupId, TagName),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% count/0 测试
%% ===================================================================

count_returns_total_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"count">> => 42}}
            end}
        ]}
    ], fun() ->
        Result = group_tag_repo:count(),
        ?assertEqual({ok, 42}, Result)
    end).

%% ===================================================================
%% count_by_group/1 测试
%% ===================================================================

count_by_group_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'pluck_value', 4, fun(_Tb, _Col, _Where, _Default) -> 5 end}
        ]}
    ], fun() ->
        GroupId = 1,
        Result = group_tag_repo:count_by_group(GroupId),
        ?assertEqual({ok, 5}, Result)
    end).

count_by_group_not_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'pluck_value', 4, fun(_Tb, _Col, _Where, _Default) -> 0 end}
        ]}
    ], fun() ->
        GroupId = 999999,
        Result = group_tag_repo:count_by_group(GroupId),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% hot_tags/2 测试
%% ===================================================================

hot_tags_returns_list_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"tag_name">> => <<"test">>, <<"count">> => 10}]}
            end}
        ]}
    ], fun() ->
        Limit = 10,
        Result = group_tag_repo:hot_tags(Limit),
        ?assertMatch({ok, _}, Result)
    end).

hot_tags_with_limit_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"tag_name">> => <<"a">>, <<"count">> => 5}]}
            end}
        ]}
    ], fun() ->
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
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 700010 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {error, check_violation}
            end}
        ]}
    ], fun() ->
        Data = #{
            group_id => 1,
            tag_name => <<>>,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        ?assertMatch({error, _}, Result)
    end).

%% @doc 测试超长标签名
add_with_long_tag_name_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 700011 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        LongTag = list_to_binary(lists:duplicate(100, $x)),
        Data = #{
            group_id => 1,
            tag_name => LongTag,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        ?assertEqual({ok, 700011}, Result)
    end).

%% @doc 测试 UTF-8 标签名
add_with_utf8_tag_name_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 700012 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Data = #{
            group_id => 1,
            tag_name => <<"技术交流"/utf8>>,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        ?assertEqual({ok, 700012}, Result)
    end).

%% @doc 测试特殊字符标签名
add_with_special_chars_tag_name_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 700013 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Data = #{
            group_id => 1,
            tag_name => <<"技术-交流_(2024)">>,
            created_by => 100
        },
        Result = group_tag_repo:add(undefined, Data),
        ?assertEqual({ok, 700013}, Result)
    end).

%% @doc 测试零群组ID
find_by_id_with_zero_id_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        Result = group_tag_repo:find_by_id(0, <<"id">>),
        ?assertEqual({error, invalid_id}, Result)
    end).

%% @doc 测试负数群组ID
list_by_group_with_negative_id_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        Result = group_tag_repo:list_by_group(-1, <<"id">>),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的标签生命周期
complete_tag_lifecycle_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 700020 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"id">> => 700020, <<"tag_name">> => <<"生命周期测试"/utf8>>}]}
            end},
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"id">> => 700020, <<"group_id">> => 99998,
                       <<"tag_name">> => <<"生命周期测试"/utf8>>}}
            end},
            {'pluck_value', 4, fun(_Tb, _Col, _Where, _Default) -> 1 end},
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        GroupId = 99998,
        TagName = <<"生命周期测试"/utf8>>,
        CreatedBy = 99999,

        % 1. 添加标签
        Data = #{
            group_id => GroupId,
            tag_name => TagName,
            created_by => CreatedBy,
            created_at => 1700000000
        },
        AddResult = group_tag_repo:add(undefined, Data),
        ?assertEqual({ok, 700020}, AddResult),

        % 2. 查询标签
        Found = group_tag_repo:find_by_id(700020, <<"*">>),
        ?assertMatch(#{<<"id">> := 700020}, Found),

        % 3. 按群组查询
        ListResult = group_tag_repo:list_by_group(GroupId, <<"*">>),
        ?assertMatch({ok, [_|_]}, ListResult),

        % 4. 检查存在性
        Exists = group_tag_repo:exists(GroupId, TagName),
        ?assertEqual(true, Exists),

        % 5. 删除标签
        DeleteResult = group_tag_repo:delete(GroupId, TagName),
        ?assertMatch({ok, _}, DeleteResult)
    end).

%% @doc 测试同一群组添加多个标签
multiple_tags_for_same_group_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"id">> => 1}, #{<<"id">> => 2}, #{<<"id">> => 3}]}
            end},
            {'execute', 2, fun(_Sql, _Params) -> {ok, 3} end}
        ]}
    ], fun() ->
        GroupId = 99997,

        % 查询所有标签
        ListResult = group_tag_repo:list_by_group(GroupId, <<"*">>),
        case ListResult of
            {ok, TagList} ->
                ?assert(length(TagList) >= 3);
            _ ->
                ?assert(false, "Expected {ok, TagList}")
        end,

        % 清理
        Result = group_tag_repo:delete_by_group_id(GroupId),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 测试热门标签统计
hot_tags_ranking_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{<<"tag_name">> => <<"技术"/utf8>>, <<"count">> => 10},
                    #{<<"tag_name">> => <<"交流"/utf8>>, <<"count">> => 5}
                ]}
            end}
        ]}
    ], fun() ->
        Result = group_tag_repo:hot_tags(10),
        case Result of
            {ok, TagList} when is_list(TagList) ->
                case TagList of
                    [] -> ?assertEqual([], TagList);
                    [First | _] ->
                        ?assertMatch(#{<<"tag_name">> := _, <<"count">> := _}, First)
                end;
            _ ->
                ?assert(false, "Expected {ok, TagList}")
        end
    end).
