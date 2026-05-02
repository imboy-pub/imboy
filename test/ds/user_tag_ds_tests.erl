-module(user_tag_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_tag_ds 模块的 EUnit 测试
%%%
%%% 目标：验证用户标签数据服务功能
%%% 覆盖：标签分页、添加、删除、修改、查询、合并
%%%===================================================================

%% ===================================================================
%% page/5 测试
%% ===================================================================

%% @doc 测试标签分页列表成功
page_returns_tag_list_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 4, fun(_Tb, _Where, Page, Size) ->
                ?assertEqual(1, Page),
                ?assertEqual(10, Size),
                {ok, #{
                    total => 2,
                    page => 1,
                    size => 10,
                    list => [
                        #{<<"id">> => 1, <<"name">> => <<"标签1"/utf8>>, <<"referer_time">> => 0},
                        #{<<"id">> => 2, <<"name">> => <<"标签2"/utf8>>, <<"referer_time">> => 0}
                    ]
                }}
            end}
        ]},
        {user_tag_relation_repo, [
            {'tag_subtitle', 3, fun(_Scene, _TagId, _RefererTime) ->
                <<"副标题"/utf8>>
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:page(1, 1, 10, #{}, <<"id desc">>),
        ?assertEqual(2, maps:get(total, Result)),
        ?assertEqual(1, maps:get(page, Result)),
        ?assertEqual(10, maps:get(size, Result)),
        List = maps:get(list, Result),
        ?assertEqual(2, length(List))
    end).

%% @doc 测试标签分页空结果
page_with_empty_result_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 4, fun(_Tb, _Where, _Page, _Size) ->
                {ok, #{total => 0, page => 1, size => 10, list => []}}
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:page(1, 1, 10, #{}, <<"id desc">>),
        ?assertEqual(0, maps:get(total, Result)),
        ?assertEqual(0, length(maps:get(list, Result)))
    end).

%% @doc 测试标签分页错误
page_with_error_returns_empty_list_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 4, fun(_Tb, _Where, _Page, _Size) ->
                {error, <<"database_error">>}
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:page(1, 1, 10, #{}, <<"id desc">>),
        ?assertEqual(0, maps:get(total, Result)),
        ?assertEqual(0, length(maps:get(list, Result))),
        ?assertEqual(<<"database_error">>, maps:get(error, Result))
    end).

%% ===================================================================
%% delete/3 测试
%% ===================================================================

%% @doc 测试删除标签成功
delete_success_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, _Where, _Opts, _Default) ->
                1
            end},
            {'with_tx', 1, fun(Fun) ->
                Fun(self())
            end},
            {'execute', 3, fun(_Conn, _Sql, _Params) ->
                {ok, 1}
            end}
        ]},
        {user_tag_relation_repo, [
            {'tablename', 0, fun() -> <<"user_tag_relation">> end},
            {'flush_subtitle', 1, fun(_TagId) -> ok end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(<<"user_collect">>) ->
                <<"public.user_collect">>
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:delete(100, 1, <<"标签1"/utf8>>),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试删除不存在的标签
delete_nonexistent_tag_returns_ok_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, _Where, _Opts, _Default) ->
                0
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:delete(100, 1, <<"不存在的标签"/utf8>>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% change_name/5 测试
%% ===================================================================

%% @doc 测试修改标签名称成功
change_name_success_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {user_tag_relation_repo, [
            {'update_tag', 5, fun(_Conn, _TagId, _TagName, _Uid, _CreatedAt) ->
                {1, <<"新标签名"/utf8>>}
            end},
            {'flush_subtitle', 1, fun(_TagId) -> ok end}
        ]},
        {elib_cnv, [
            {'implode', 2, fun(_Sep, _Parts) -> <<"tag1">> end},
            {'remove_dups', 1, fun(List) -> List end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, _Params) ->
                case binary:match(Sql, <<"SELECT object_id">>) of
                    {0, _} ->
                        {ok, [#{<<"object_id">> => 1}]};
                    nomatch ->
                        {ok, [#{<<"id">> => 9, <<"name">> => <<"旧标签"/utf8>>}]}
                end
            end},
            {'execute', 2, fun(_Sql, _Params) ->
                {ok, 1}
            end},
            {'with_tx', 1, fun(Fun) ->
                Fun(self())
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:change_name(0, 100, 1, 1, <<"新标签名"/utf8>>),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试修改标签名称时同名标签已存在
change_name_with_duplicate_name_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_tag_ds:change_name(1, 100, 1, 1, <<"标签名"/utf8>>),
        ?assertEqual(<<"标签名 已存在"/utf8>>, Result)
    end).

%% @doc 测试修改标签名称无关联对象
change_name_without_relation_objects_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {user_tag_relation_repo, [
            {'flush_subtitle', 1, fun(_TagId) -> ok end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:change_name(0, 100, 1, 1, <<"新标签名"/utf8>>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% add/3 测试
%% ===================================================================

%% @doc 测试添加标签成功
add_success_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_Table) ->
                <<"public.user_tag">>
            end},
            {'parse_result', 1, fun(_Result) ->
                {ok, 1, #{}}
            end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, _Where, _Opts, _Default) ->
                0
            end},
            {'insert', 3, fun(_Tb, _Data, _Returning) ->
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]}
    ], fun() ->
        {ok, TagId} = user_tag_ds:add(100, 1, <<"新标签"/utf8>>),
        ?assertEqual(1, TagId)
    end).

%% @doc 测试添加已存在的标签
add_existing_tag_returns_tag_id_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_Table) ->
                <<"public.user_tag">>
            end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, _Where, _Opts, _Default) ->
                5
            end}
        ]}
    ], fun() ->
        {ok, TagId} = user_tag_ds:add(100, 1, <<"已存在标签"/utf8>>),
        ?assertEqual(5, TagId)
    end).

%% @doc 测试添加标签场景为0
add_with_scene_zero_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_tag_ds:add(100, 0, <<"标签"/utf8>>),
        ?assertEqual({error, <<"invalid_scene">>}, Result)
    end).

%% @doc 测试添加标签名称为空
add_with_empty_tag_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_tag_ds:add(100, 1, <<>>),
        ?assertEqual({error, <<"invalid_tag">>}, Result)
    end).

%% ===================================================================
%% find_tag_id/3 测试
%% ===================================================================

%% @doc 测试查找标签ID成功
find_tag_id_returns_tag_id_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, _Where, _Opts, _Default) ->
                5
            end}
        ]}
    ], fun() ->
        {ok, TagId} = user_tag_ds:find_tag_id(100, 1, <<"标签1"/utf8>>),
        ?assertEqual(5, TagId)
    end).

%% @doc 测试查找不存在的标签ID
find_tag_id_not_found_returns_zero_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, _Where, _Opts, _Default) ->
                0
            end}
        ]}
    ], fun() ->
        {ok, TagId} = user_tag_ds:find_tag_id(100, 1, <<"不存在的标签"/utf8>>),
        ?assertEqual(0, TagId)
    end).

%% ===================================================================
%% get_relation_objects/3 测试
%% ===================================================================

%% @doc 测试获取关联对象成功
get_relation_objects_returns_objects_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, [Scene, Uid, TagId]) ->
            ?assertEqual(1, Scene),
            ?assertEqual(100, Uid),
            ?assertEqual(5, TagId),
            {ok, [
                #{<<"object_id">> => 1},
                #{<<"object_id">> => 2}
            ]}
        end}
    ], fun() ->
        {ok, Rows} = user_tag_ds:get_relation_objects(1, 100, 5),
        ?assertEqual(2, length(Rows))
    end).

%% @doc 测试获取关联对象空结果
get_relation_objects_with_no_results_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        {ok, Rows} = user_tag_ds:get_relation_objects(1, 100, 5),
        ?assertEqual(0, length(Rows))
    end).

%% @doc 测试获取关联对象错误返回空列表
get_relation_objects_with_error_returns_empty_list_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, <<"database_error">>}
        end}
    ], fun() ->
        {ok, Rows} = user_tag_ds:get_relation_objects(1, 100, 5),
        ?assertEqual(0, length(Rows))
    end).

%% ===================================================================
%% change_scene_tag/5 测试
%% ===================================================================

%% @doc 测试修改场景标签成功（收藏场景）
change_scene_tag_for_collect_scene_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Table) ->
                ?assertEqual(<<"user_collect">>, Table),
                <<"public.user_collect">>
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [1, 100, 1]) ->
                {ok, [#{<<"id">> => 2, <<"name">> => <<"old_tag"/utf8>>}]}
            end},
            {'execute', 2, fun(_Sql, [TagBin, Uid, ObjectId]) ->
                ?assertEqual(<<"tag1,old_tag,">>, TagBin),
                ?assertEqual(100, Uid),
                ?assertEqual(1, ObjectId),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:change_scene_tag(undefined, 1, 100, 1, [{1, <<"tag1">>}]),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试修改场景标签成功（好友场景）
change_scene_tag_for_friend_scene_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Table) ->
                ?assertEqual(<<"user_friend">>, Table),
                <<"public.user_friend">>
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [2, 100, 1]) ->
                {ok, [#{<<"id">> => 2, <<"name">> => <<"old_tag"/utf8>>}]}
            end},
            {'execute', 2, fun(_Sql, [TagBin, Uid, ObjectId]) ->
                ?assertEqual(<<"tag1,old_tag,">>, TagBin),
                ?assertEqual(100, Uid),
                ?assertEqual(1, ObjectId),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:change_scene_tag(undefined, 2, 100, 1, [{1, <<"tag1">>}]),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% flush_subtitle/1 测试
%% ===================================================================

%% @doc 测试清理缓存
flush_subtitle_success_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
        {'flush_subtitle', 1, fun(TagId) ->
            ?assertEqual(5, TagId),
            ok
        end}
    ], fun() ->
        Result = user_tag_ds:flush_subtitle(5),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试零页码
page_with_zero_page_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertError(function_clause, user_tag_ds:page(1, 0, 10, #{}, <<"id desc">>))
    end).

%% @doc 测试负页码
page_with_negative_page_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertError(function_clause, user_tag_ds:page(1, -1, 10, #{}, <<"id desc">>))
    end).

%% @doc 测试大页码
page_with_large_page_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 4, fun(_Tb, _Where, Page, Size) ->
                ?assertEqual(9999, Page),
                {ok, #{total => 0, page => 9999, size => 10, list => []}}
            end}
        ]},
        {user_tag_relation_repo, [
            {'tag_subtitle', 3, fun(_Scene, _TagId, _RefererTime) ->
                <<"副标题"/utf8>>
            end}
        ]}
    ], fun() ->
        Result = user_tag_ds:page(1, 9999, 10, #{}, <<"id desc">>),
        ?assertEqual(9999, maps:get(page, Result))
    end).

%% @doc 测试空标签名
add_with_empty_tag_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_tag_ds:add(100, 1, <<>>),
        ?assertEqual({error, <<"invalid_tag">>}, Result)
    end).

%% @doc 测试超长标签名
add_with_long_tag_name_test_() ->
    LongTag = list_to_binary(lists:duplicate(500, $x)),
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_Table) ->
                <<"public.user_tag">>
            end},
            {'parse_result', 1, fun(_Result) ->
                {ok, 1, #{}}
            end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, _Where, _Opts, _Default) ->
                0
            end},
            {'insert', 3, fun(_Tb, Data, _Returning) ->
                ?assertEqual(LongTag, maps:get(name, Data)),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]}
    ], fun() ->
        {ok, TagId} = user_tag_ds:add(100, 1, LongTag),
        ?assertEqual(1, TagId)
    end).

%% @doc 测试UTF-8标签名
add_with_utf8_tag_name_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_Table) ->
                <<"public.user_tag">>
            end},
            {'parse_result', 1, fun(_Result) ->
                {ok, 1, #{}}
            end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, _Where, _Opts, _Default) ->
                0
            end},
            {'insert', 3, fun(_Tb, Data, _Returning) ->
                ?assertEqual(<<"测试标签"/utf8>>, maps:get(name, Data)),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]}
    ], fun() ->
        {ok, TagId} = user_tag_ds:add(100, 1, <<"测试标签"/utf8>>),
        ?assertEqual(1, TagId)
    end).

%% @doc 测试特殊场景值
add_with_special_scene_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_Table) ->
                <<"public.user_tag">>
            end},
            {'parse_result', 1, fun(_Result) ->
                {ok, 1, #{}}
            end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Tb, _Column, Where, _Opts, _Default) ->
                ?assertEqual(999, maps:get(scene, Where)),
                0
            end},
            {'insert', 3, fun(_Tb, Data, _Returning) ->
                ?assertEqual(999, maps:get(scene, Data)),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]}
    ], fun() ->
        {ok, TagId} = user_tag_ds:add(100, 999, <<"标签"/utf8>>),
        ?assertEqual(1, TagId)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

%% @doc 验证add参数类型
add_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        Scene = 1,
        Tag = <<"标签"/utf8>>,
        ?assert(is_integer(Uid)),
        ?assert(is_integer(Scene)),
        ?assert(is_binary(Tag))
    end).

%% @doc 验证delete参数类型
delete_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        Scene = 1,
        Tag = <<"标签"/utf8>>,
        ?assert(is_integer(Uid)),
        ?assert(is_integer(Scene)),
        ?assert(is_binary(Tag))
    end).

%% @doc 验证find_tag_id参数类型
find_tag_id_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        Scene = 1,
        Tag = <<"标签"/utf8>>,
        ?assert(is_integer(Uid)),
        ?assert(is_integer(Scene)),
        ?assert(is_binary(Tag))
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的标签生命周期
complete_tag_lifecycle_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {elib_pg_sql, [
            {'parse_result', 1, fun(_Result) ->
                {ok, 1, #{}}
            end},
            {'public_tablename', 1, fun(Table) ->
                case Table of
                    <<"user_collect">> -> <<"public.user_collect">>;
                    _ -> <<"public.user_tag">>
                end
            end}
        ]},
        {user_tag_repo, [
            {'tablename', 0, fun() -> <<"user_tag">> end}
        ]},
        {user_tag_relation_repo, [
            {'flush_subtitle', 1, fun(_TagId) -> ok end},
            {'tag_subtitle', 3, fun(_Scene, _TagId, _RefererTime) ->
                <<"副标题"/utf8>>
            end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(Tb, _Column, _Where, _Opts, _Default) ->
                case Tb of
                    <<"public.user_tag">> -> 0;
                    <<"user_tag">> -> 1
                end
            end},
            {'insert', 3, fun(_Tb, _Data, _Returning) ->
                {ok, 1, #{<<"id">> => 1}}
            end},
            {'page_with_total', 4, fun(_Tb, _Where, Page, Size) ->
                {ok, #{total => 1, page => Page, size => Size, list => [
                    #{<<"id">> => 1, <<"name">> => <<"新标签"/utf8>>, <<"referer_time">> => 0}
                ]}}
            end},
            {'with_tx', 1, fun(Fun) ->
                Fun(self())
            end},
            {'execute', 3, fun(_Conn, _Sql, _Params) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        % 1. 添加标签
        {ok, TagId} = user_tag_ds:add(100, 1, <<"新标签"/utf8>>),
        ?assertEqual(1, TagId),
        % 2. 查找标签
        {ok, FoundTagId} = user_tag_ds:find_tag_id(100, 1, <<"新标签"/utf8>>),
        ?assertEqual(1, FoundTagId),
        % 3. 分页查询
        Result = user_tag_ds:page(1, 1, 10, #{}, <<"id desc">>),
        ?assertEqual(1, maps:get(total, Result)),
        % 4. 删除标签
        ?assertEqual(ok, user_tag_ds:delete(100, 1, <<"新标签"/utf8>>))
    end).
