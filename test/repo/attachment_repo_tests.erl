-module(attachment_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% attachment_repo 模块的 EUnit 测试
%%%
%%% 目标：验证附件数据访问层功能
%%% 覆盖：附件查询、创建
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(
        elib_pg_sql,
        [
            {'public_tablename', 1, fun(_Table) -> <<"public.attachment">> end}
        ],
        fun() ->
            Result = attachment_repo:tablename(),
            ?assertEqual(<<"public.attachment">>, Result)
        end
    ).

%% ===================================================================
%% save/4 测试
%% ===================================================================

save_with_empty_attachments_test_() ->
    ?TEST_SIMPLE(fun() ->
        Conn = mock_conn,
        CreatedAt = 1234567890,
        Uid = 12345,
        Attachments = [],

        Result = attachment_repo:save(Conn, CreatedAt, Uid, Attachments),
        ?assertEqual(ok, Result)
    end).

save_with_single_attachment_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 123456789 end}
            ]},
            {elib_pg_sql, [
                {'insert', 2, fun(_Tb, _Data) -> {<<"INSERT INTO t VALUES($1)">>, [1]} end}
            ]}
        ],
        fun() ->
            Conn = mock_conn,
            CreatedAt = 1234567890,
            Uid = 12345,
            Attachments = [
                #{
                    <<"md5">> => <<"abc123">>,
                    <<"mime_type">> => <<"image/jpeg">>,
                    <<"name">> => <<"photo.jpg">>,
                    <<"path">> => <<"/uploads/photo.jpg">>,
                    <<"url">> => <<"https://example.com/photo.jpg">>,
                    <<"size">> => 102400
                }
            ],

            Result = attachment_repo:save(Conn, CreatedAt, Uid, Attachments),
            ?assertEqual(ok, Result)
        end
    ).

save_with_multiple_attachments_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 123456789 end}
            ]},
            {elib_pg_sql, [
                {'insert', 2, fun(_Tb, _Data) -> {<<"INSERT INTO t VALUES($1)">>, [1]} end}
            ]}
        ],
        fun() ->
            Conn = mock_conn,
            CreatedAt = 1234567890,
            Uid = 12345,
            Attachments = [
                #{
                    <<"md5">> => <<"abc123">>,
                    <<"mime_type">> => <<"image/jpeg">>,
                    <<"name">> => <<"photo1.jpg">>,
                    <<"path">> => <<"/uploads/photo1.jpg">>,
                    <<"url">> => <<"https://example.com/photo1.jpg">>,
                    <<"size">> => 102400
                },
                #{
                    <<"md5">> => <<"def456">>,
                    <<"mime_type">> => <<"image/png">>,
                    <<"name">> => <<"photo2.png">>,
                    <<"path">> => <<"/uploads/photo2.png">>,
                    <<"url">> => <<"https://example.com/photo2.png">>,
                    <<"size">> => 51200
                }
            ],

            Result = attachment_repo:save(Conn, CreatedAt, Uid, Attachments),
            ?assertEqual(ok, Result)
        end
    ).

save_with_image_mime_type_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 123456789 end}
            ]},
            {elib_pg_sql, [
                {'insert', 2, fun(_Tb, _Data) -> {<<"INSERT INTO t VALUES($1)">>, [1]} end}
            ]}
        ],
        fun() ->
            Conn = mock_conn,
            CreatedAt = 1234567890,
            Uid = 12345,
            Attachments = [
                #{
                    <<"md5">> => <<"abc123">>,
                    <<"mime_type">> => <<"image/jpeg">>,
                    <<"name">> => <<"photo.jpg">>,
                    <<"path">> => <<"/uploads/photo.jpg">>,
                    <<"url">> => <<"https://example.com/photo.jpg">>,
                    <<"size">> => 102400
                }
            ],

            Result = attachment_repo:save(Conn, CreatedAt, Uid, Attachments),
            ?assertEqual(ok, Result)
        end
    ).

save_with_non_image_mime_type_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 123456789 end}
            ]},
            {elib_pg_sql, [
                {'insert', 2, fun(_Tb, _Data) -> {<<"INSERT INTO t VALUES($1)">>, [1]} end}
            ]}
        ],
        fun() ->
            Conn = mock_conn,
            CreatedAt = 1234567890,
            Uid = 12345,
            Attachments = [
                #{
                    <<"md5">> => <<"xyz789">>,
                    <<"mime_type">> => <<"application/pdf">>,
                    <<"name">> => <<"document.pdf">>,
                    <<"path">> => <<"/uploads/document.pdf">>,
                    <<"url">> => <<"https://example.com/document.pdf">>,
                    <<"size">> => 204800
                }
            ],

            Result = attachment_repo:save(Conn, CreatedAt, Uid, Attachments),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% update_status/2
%% ===================================================================

update_status_disable_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.attachment">> end}]},
            {elib_pg, [{'update', 4, fun(_Tb, _D, _W, _P) -> {ok, 1} end}]},
            {elib_dt, [{'now', 0, fun() -> 1748430000 end}]}
        ],
        fun() ->
            ?assertEqual(ok, attachment_repo:update_status(123, 0))
        end
    ).

update_status_soft_delete_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.attachment">> end}]},
            {elib_pg, [{'update', 4, fun(_Tb, _D, _W, _P) -> {ok, 1} end}]},
            {elib_dt, [{'now', 0, fun() -> 1748430000 end}]}
        ],
        fun() ->
            ?assertEqual(ok, attachment_repo:update_status(456, -1))
        end
    ).

update_status_db_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.attachment">> end}]},
            {elib_pg, [{'update', 4, fun(_Tb, _D, _W, _P) -> {error, db_error} end}]},
            {elib_dt, [{'now', 0, fun() -> 1748430000 end}]}
        ],
        fun() ->
            ?assertEqual({error, db_error}, attachment_repo:update_status(789, 0))
        end
    ).

%% ===================================================================
%% orphan_stats/1
%% ===================================================================

orphan_stats_returns_map_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.attachment">> end}]},
            {elib_pg, [
                {'one', 2, fun(_Sql, []) ->
                    {ok, #{<<"count">> => 5, <<"total_size">> => 1024000}}
                end}
            ]}
        ],
        fun() ->
            {ok, Stats} = attachment_repo:orphan_stats(#{age_days => 30}),
            ?assertEqual(5, maps:get(<<"count">>, Stats)),
            ?assertEqual(1024000, maps:get(<<"total_size">>, Stats))
        end
    ).

%% ===================================================================
%% orphan_list_for_delete/1
%% ===================================================================

orphan_list_for_delete_returns_rows_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.attachment">> end}]},
            {elib_pg, [
                {'query', 2, fun(_Sql, []) ->
                    {ok, [
                        #{<<"id">> => 1, <<"path">> => <<"file_1/a.jpg">>},
                        #{<<"id">> => 2, <<"path">> => <<"file_2/b.png">>}
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, Rows} = attachment_repo:orphan_list_for_delete(#{age_days => 7}),
            ?assertEqual(2, length(Rows))
        end
    ).

%% ===================================================================
%% hard_delete_by_ids/1
%% ===================================================================

hard_delete_by_ids_empty_noop_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, attachment_repo:hard_delete_by_ids([]))
    end).

hard_delete_by_ids_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.attachment">> end}]},
            {elib_pg, [{'execute', 2, fun(_Sql, _Params) -> {ok, 2} end}]}
        ],
        fun() ->
            ?assertEqual(ok, attachment_repo:hard_delete_by_ids([1, 2]))
        end
    ).
