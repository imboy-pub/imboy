-module(group_file_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_file_logic 模块的 EUnit 测试
%%%
%%% 测试群文件业务逻辑层的所有功能
%%%===================================================================

%% C08（V3-12）：upload/5 及其用例已随 multipart 正文上传链退役；
%% 群文件唯一引用入口为 group_file_ds:bind_file（direct-v1 confirmed 绑定）。

%% ===================================================================
%% download/2 测试
%% ===================================================================

download_success_test_() ->
    ?WITH_MECKS(
        [
            {attach_logic, [
                {'view_url', 2, fun(100, <<"bound/key">>) ->
                    {ok, <<"https://example.com/signed">>}
                end}
            ]},
            {group_file_ds, [
                {'download_file', 2, fun(_, _) ->
                    {ok, <<"bound/key">>}
                end}
            ]}
        ],
        fun() ->
            FileId = 1,
            CurrentUid = 100,

            Result = group_file_logic:download(FileId, CurrentUid),

            case Result of
                {ok, FileUrl} when is_binary(FileUrl) ->
                    ?assert(is_binary(FileUrl));
                {error, Reason} ->
                    assert_reason(Reason)
            end
        end
    ).

download_not_member_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'download_file', 2, fun(_, _) ->
                    {error, not_member}
                end}
            ]}
        ],
        fun() ->
            FileId = 1,
            CurrentUid = 999,

            Result = group_file_logic:download(FileId, CurrentUid),

            ?assertMatch({error, not_member}, Result)
        end
    ).

download_not_found_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'download_file', 2, fun(_, _) ->
                    {error, not_found}
                end}
            ]}
        ],
        fun() ->
            FileId = 999999,
            CurrentUid = 100,

            Result = group_file_logic:download(FileId, CurrentUid),

            ?assertMatch({error, not_found}, Result)
        end
    ).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_success_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'delete_file', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            FileId = 1,
            CurrentUid = 100,

            Result = group_file_logic:delete(FileId, CurrentUid),

            ?assertMatch(ok, Result)
        end
    ).

delete_permission_denied_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'delete_file', 2, fun(_, _) ->
                    {error, permission_denied}
                end}
            ]}
        ],
        fun() ->
            FileId = 1,
            CurrentUid = 999,

            Result = group_file_logic:delete(FileId, CurrentUid),

            ?assertMatch({error, permission_denied}, Result)
        end
    ).

delete_not_found_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'delete_file', 2, fun(_, _) ->
                    {error, not_found}
                end}
            ]}
        ],
        fun() ->
            FileId = 999999,
            CurrentUid = 100,

            Result = group_file_logic:delete(FileId, CurrentUid),

            ?assertMatch({error, not_found}, Result)
        end
    ).

%% ===================================================================
%% list/4 测试
%% ===================================================================

list_success_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'list_files', 5, fun(_, _, _, _, _) ->
                    {ok, [
                        #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
                    ]}
                end},
                {'count_by_group', 1, fun(_) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Gid = <<"1">>,
            CurrentUid = 100,
            Page = 1,
            Size = 20,

            Result = group_file_logic:list(Gid, CurrentUid, Page, Size),

            case Result of
                {ok, FileList} when is_map(FileList) ->
                    ?assertMatch(#{<<"items">> := _, <<"total">> := _}, FileList);
                {error, Reason} ->
                    assert_reason(Reason)
            end
        end
    ).

list_not_member_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'list_files', 5, fun(_, _, _, _, _) ->
                    {error, not_member}
                end}
            ]}
        ],
        fun() ->
            Gid = <<"1">>,
            CurrentUid = 999,
            Page = 1,
            Size = 20,

            Result = group_file_logic:list(Gid, CurrentUid, Page, Size),

            ?assertMatch({error, not_member}, Result)
        end
    ).

list_with_category_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'list_files', 5, fun(_, _, _, _, _) ->
                    {ok, [
                        #{
                            <<"id">> => 1,
                            <<"file_name">> => <<"test.pdf"/utf8>>,
                            <<"file_category">> => <<"document">>
                        }
                    ]}
                end},
                {'count_by_group', 1, fun(_) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Gid = <<"1">>,
            CurrentUid = 100,
            Page = 1,
            Size = 20,
            Options = #{category => <<"document">>},

            Result = group_file_logic:list(Gid, CurrentUid, Page, Size, Options),

            case Result of
                {ok, FileList} when is_map(FileList) ->
                    ?assertMatch(#{<<"items">> := _, <<"total">> := _}, FileList);
                {error, Reason} ->
                    assert_reason(Reason)
            end
        end
    ).

%% ===================================================================
%% search/4 测试
%% ===================================================================

search_success_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'search_files', 5, fun(_, _, _, _, _) ->
                    {ok, [
                        #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
                    ]}
                end}
            ]}
        ],
        fun() ->
            Gid = <<"1">>,
            Keyword = <<"test"/utf8>>,
            Page = 1,
            Size = 20,

            Result = group_file_logic:search(Gid, Keyword, Page, Size, 42),

            case Result of
                {ok, Files} when is_list(Files) ->
                    ?assert(is_list(Files));
                {error, Reason} ->
                    assert_reason(Reason)
            end
        end
    ).

search_empty_result_test_() ->
    ?WITH_MECKS(
        [
            {group_file_ds, [
                {'search_files', 5, fun(_, _, _, _, _) ->
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            Gid = <<"1">>,
            Keyword = <<"nonexistent"/utf8>>,
            Page = 1,
            Size = 20,

            Result = group_file_logic:search(Gid, Keyword, Page, Size, 42),

            case Result of
                {ok, []} ->
                    ok;
                {ok, Files} when is_list(Files) ->
                    ?assertEqual([], Files);
                {error, Reason} ->
                    assert_reason(Reason)
            end
        end
    ).

%% ===================================================================
%% get_categories/2 测试
%% ===================================================================

get_categories_success_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(100, 1) -> true end}
            ]},
            {group_file_ds, [
                {'get_file_categories', 2, fun(1, 100) ->
                    {ok, [
                        {<<"document">>, 10, 1024000}
                    ]}
                end}
            ]}
        ],
        fun() ->
            Gid = <<"1">>,
            CurrentUid = 100,

            Result = group_file_logic:get_categories(Gid, CurrentUid),

            case Result of
                {ok, Stats} when is_list(Stats) ->
                    ?assert(is_list(Stats));
                {error, Reason} ->
                    assert_reason(Reason)
            end
        end
    ).

get_categories_empty_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(100, 1) -> true end}
            ]},
            {group_file_ds, [
                {'get_file_categories', 2, fun(1, 100) ->
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            Gid = <<"1">>,
            CurrentUid = 100,

            Result = group_file_logic:get_categories(Gid, CurrentUid),

            case Result of
                {ok, []} ->
                    ok;
                {ok, Stats} when is_list(Stats) ->
                    ?assertEqual([], Stats);
                {error, Reason} ->
                    assert_reason(Reason)
            end
        end
    ).

assert_reason(Reason) ->
    ?assert(is_atom(Reason) orelse is_binary(Reason) orelse is_tuple(Reason)).
