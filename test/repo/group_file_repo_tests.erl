-module(group_file_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_file_repo 模块的 EUnit 测试
%%%
%%% 测试群文件数据仓库层的所有功能
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]}
        ],
        fun() ->
            Result = group_file_repo:tablename(),
            ?assertEqual(<<"public.group_file">>, Result)
        end
    ).

%% ===================================================================
%% insert/1 测试
%% ===================================================================

insert_valid_file_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 50001 end}
            ]},
            {elib_pg_sql, [
                {'insert', 2, fun(_Tb, _Data) -> {<<"INSERT">>, []} end}
            ]}
        ],
        fun() ->
            Data = #{
                group_id => 1,
                file_id => <<"file_test_001">>,
                file_name => <<"test.pdf"/utf8>>,
                file_size => 1024000,
                file_type => <<"application/pdf">>,
                file_category => <<"document">>,
                file_url => <<"http://example.com/files/test.pdf">>,
                file_hash => <<"abc123">>,
                uploader_id => 100,
                download_count => 0,
                status => 1,
                created_at => {{2026, 2, 16}, {12, 0, 0}}
            },
            Result = group_file_repo:insert(Data),
            ?assertEqual({ok, 50001}, Result)
        end
    ).

%% ===================================================================
%% list_by_group/4 真路径回归（不 mock elib_pg_sql）
%% ===================================================================

%% 回归：order_by 必须传 [{Field, Direction}] 而非裸 binary。
%% 此前传 <<"created_at DESC">> 令 build_select 内 lists:map case_clause
%% 崩掉整个请求进程（生产 crash.log 2026-07-12 坐实，群文件列表全挂）。
%% 本测试让 build_select 真跑，仅 mock elib_pg:query 捕获最终 SQL。
list_by_group_order_by_contract_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    _ = catch meck:unload(config_ds),
    meck:new(config_ds, [passthrough, no_link]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    Self = self(),
    meck:expect(elib_pg, query, fun(Sql, Params) ->
        Self ! {captured, iolist_to_binary(Sql), Params},
        {ok, []}
    end),
    ?assertMatch({ok, []}, group_file_repo:list_by_group(1, 1, 10, #{})),
    receive
        {captured, Sql, _Params} ->
            ?assertMatch({_, _}, binary:match(Sql, <<"ORDER BY created_at DESC">>))
    after 1000 ->
        ?assert(false)
    end,
    meck:unload(config_ds),
    meck:unload(elib_pg).

%% ===================================================================
%% find_by_id/1 测试
%% ===================================================================

find_by_id_existing_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}}
                end}
            ]},
            {elib_pg_sql, [
                {'build_select', 4, fun(_Tb, _Col, _Where, _Opts) -> {<<"SELECT">>, []} end}
            ]}
        ],
        fun() ->
            FileId = 1,
            Result = group_file_repo:find_by_id(FileId),
            ?assertMatch(#{<<"id">> := _, <<"file_name">> := _}, Result)
        end
    ).

find_by_id_not_existing_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
            ]},
            {elib_pg_sql, [
                {'build_select', 4, fun(_Tb, _Col, _Where, _Opts) -> {<<"SELECT">>, []} end}
            ]}
        ],
        fun() ->
            FileId = 999999,
            Result = group_file_repo:find_by_id(FileId),
            ?assertMatch(#{}, Result)
        end
    ).

%% ===================================================================
%% find_by_file_id/1 测试
%% ===================================================================

find_by_file_id_existing_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"id">> => 1, <<"file_id">> => <<"file_existing_001">>}}
                end}
            ]},
            {elib_pg_sql, [
                {'build_select', 4, fun(_Tb, _Col, _Where, _Opts) -> {<<"SELECT">>, []} end}
            ]}
        ],
        fun() ->
            FileId = <<"file_existing_001">>,
            Result = group_file_repo:find_by_file_id(FileId),
            ?assertMatch(#{<<"id">> := _, <<"file_id">> := _}, Result)
        end
    ).

find_by_file_id_not_existing_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
            ]},
            {elib_pg_sql, [
                {'build_select', 4, fun(_Tb, _Col, _Where, _Opts) -> {<<"SELECT">>, []} end}
            ]}
        ],
        fun() ->
            FileId = <<"file_not_existing_999">>,
            Result = group_file_repo:find_by_file_id(FileId),
            ?assertMatch(#{}, Result)
        end
    ).

%% ===================================================================
%% list_by_group/4 测试
%% ===================================================================

list_by_group_with_results_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [#{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}]}
                end}
            ]},
            {elib_pg_sql, [
                {'build_select', 4, fun(_Tb, _Col, _Where, _Opts) -> {<<"SELECT">>, []} end}
            ]}
        ],
        fun() ->
            Gid = 1,
            Result = group_file_repo:list_by_group(Gid, 1, 20, #{}),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

list_by_group_empty_group_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]},
            {elib_pg_sql, [
                {'build_select', 4, fun(_Tb, _Col, _Where, _Opts) -> {<<"SELECT">>, []} end}
            ]}
        ],
        fun() ->
            Gid = 999999,
            Result = group_file_repo:list_by_group(Gid, 1, 20, #{}),
            ?assertMatch({ok, []}, Result)
        end
    ).

%% ===================================================================
%% soft_delete/1 测试
%% ===================================================================

soft_delete_existing_file_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            FileId = 1,
            Result = group_file_repo:soft_delete(FileId),
            ?assertEqual({ok, 1}, Result)
        end
    ).

soft_delete_not_existing_file_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            FileId = 999999,
            Result = group_file_repo:soft_delete(FileId),
            ?assertEqual({ok, 0}, Result)
        end
    ).

%% ===================================================================
%% increment_download/1 测试
%% ===================================================================

increment_download_existing_file_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            FileId = 1,
            Result = group_file_repo:increment_download(FileId),
            ?assertEqual({ok, 1}, Result)
        end
    ).

increment_download_not_existing_file_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            FileId = 999999,
            Result = group_file_repo:increment_download(FileId),
            ?assertEqual({ok, 0}, Result)
        end
    ).

%% ===================================================================
%% count_by_group/1 测试
%% ===================================================================

count_by_group_with_files_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"count">> => 5}}
                end}
            ]}
        ],
        fun() ->
            Gid = 1,
            Result = group_file_repo:count_by_group(Gid),
            ?assertEqual({ok, 5}, Result)
        end
    ).

count_by_group_empty_group_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"count">> => 0}}
                end}
            ]}
        ],
        fun() ->
            Gid = 999999,
            Result = group_file_repo:count_by_group(Gid),
            ?assertEqual({ok, 0}, Result)
        end
    ).

%% ===================================================================
%% sum_size_by_group/1 测试
%% ===================================================================

sum_size_by_group_with_files_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"total_size">> => 1024000}}
                end}
            ]}
        ],
        fun() ->
            Gid = 1,
            Result = group_file_repo:sum_size_by_group(Gid),
            ?assertEqual({ok, 1024000}, Result)
        end
    ).

sum_size_by_group_empty_group_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"total_size">> => 0}}
                end}
            ]}
        ],
        fun() ->
            Gid = 999999,
            Result = group_file_repo:sum_size_by_group(Gid),
            ?assertEqual({ok, 0}, Result)
        end
    ).

%% ===================================================================
%% category_stats/1 测试
%% ===================================================================

category_stats_with_files_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [
                        #{
                            <<"file_category">> => <<"document">>,
                            <<"count">> => 10,
                            <<"total_size">> => 1024000
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            Gid = 1,
            Result = group_file_repo:category_stats(Gid),
            ?assertMatch({ok, [{<<"document">>, 10, 1024000}]}, Result)
        end
    ).

category_stats_empty_group_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Gid = 999999,
            Result = group_file_repo:category_stats(Gid),
            ?assertEqual({ok, []}, Result)
        end
    ).
