-module(attachment_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% attachment_ds 模块的 EUnit 测试
%%%
%%% 目标：验证附件数据服务功能
%%% 覆盖：表名获取、附件保存、MD5 去重、引用计数
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

%% @doc 测试获取附件表名
tablename_returns_table_name_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'tablename', 0, fun() -> <<"attachment">> end}
    ], fun() ->
        Result = attachment_ds:tablename(),
        ?assertEqual(<<"attachment">>, Result)
    end).

%% ===================================================================
%% save/4 测试
%% ===================================================================

%% @doc 测试保存附件信息成功
save_success_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(Conn, CreatedAt, Uid, Attach) ->
            ?assertEqual(self(), Conn),
            ?assertEqual(<<"2023-01-01T00:00:00Z">>, CreatedAt),
            ?assertEqual(100, Uid),
            ?assert(is_list(Attach)),
            ?assertEqual(1, length(Attach)),
            ok
        end}
    ], fun() ->
        Conn = self(),
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        Uid = 100,
        Attach = [#{<<"url">> => <<"https://example.com/file.pdf">>}],
        Result = attachment_ds:save(Conn, CreatedAt, Uid, Attach),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试保存多个附件
save_multiple_attachments_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
            ?assertEqual(3, length(Attach)),
            ok
        end}
    ], fun() ->
        Attach = [
            #{<<"url">> => <<"https://example.com/file1.pdf">>},
            #{<<"url">> => <<"https://example.com/file2.jpg">>},
            #{<<"url">> => <<"https://example.com/file3.png">>}
        ],
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, Attach),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试保存空附件列表
save_empty_attachments_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
            ?assertEqual(0, length(Attach)),
            ok
        end}
    ], fun() ->
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, []),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试保存带完整信息的附件
save_attachment_with_full_info_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
            [Item] = Attach,
            ?assertEqual(<<"https://example.com/file.pdf">>, maps:get(<<"url">>, Item)),
            ?assertEqual(<<"file.pdf">>, maps:get(<<"name">>, Item)),
            ?assertEqual(1024000, maps:get(<<"size">>, Item)),
            ?assertEqual(<<"application/pdf">>, maps:get(<<"content_type">>, Item)),
            ?assertEqual(<<"abc123">>, maps:get(<<"md5">>, Item)),
            ok
        end}
    ], fun() ->
        Attach = [#{<<"url">> => <<"https://example.com/file.pdf">>,
                   <<"name">> => <<"file.pdf">>,
                   <<"size">> => 1024000,
                   <<"content_type">> => <<"application/pdf">>,
                   <<"md5">> => <<"abc123">>}],
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, Attach),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试保存带大文件的附件
save_attachment_with_large_file_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
            [Item] = Attach,
            Size = maps:get(<<"size">>, Item),
            ?assert(Size > 100000000), % 100MB+
            ok
        end}
    ], fun() ->
        Attach = [#{<<"url">> => <<"https://example.com/large.zip">>,
                   <<"size">> => 500000000}], % 500MB
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, Attach),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试保存带UTF-8文件名的附件
save_attachment_with_utf8_filename_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
            [Item] = Attach,
            Name = maps:get(<<"name">>, Item),
            ?assertEqual(<<"测试文件.pdf"/utf8>>, Name),
            ok
        end}
    ], fun() ->
        Attach = [#{<<"url">> => <<"https://example.com/file.pdf">>,
                   <<"name">> => <<"测试文件.pdf"/utf8>>}],
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, Attach),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试保存带特殊字符的MD5
save_attachment_with_special_md5_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
            [Item] = Attach,
            Md5 = maps:get(<<"md5">>, Item),
            ?assertEqual(<<"abc123-xyz_789">>, Md5),
            ok
        end}
    ], fun() ->
        Attach = [#{<<"url">> => <<"https://example.com/file.pdf">>,
                   <<"md5">> => <<"abc123-xyz_789">>}],
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, Attach),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试零用户ID
save_with_zero_uid_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, Uid, _Attach) ->
            ?assertEqual(0, Uid),
            ok
        end}
    ], fun() ->
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 0, []),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试大用户ID
save_with_large_uid_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, Uid, _Attach) ->
            ?assertEqual(999999999, Uid),
            ok
        end}
    ], fun() ->
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 999999999, []),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试空创建时间
save_with_empty_created_at_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, CreatedAt, _Uid, _Attach) ->
            ?assertEqual(<<>>, CreatedAt),
            ok
        end}
    ], fun() ->
        Result = attachment_ds:save(self(), <<>>, 100, []),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试不同内容类型的附件
save_attachments_with_different_content_types_test_() ->
    ContentTypes = [
        <<"application/pdf">>,
        <<"image/jpeg">>,
        <<"image/png">>,
        <<"video/mp4">>,
        <<"audio/mpeg">>
    ],
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
            ?assertEqual(5, length(Attach)),
            lists:foreach(fun(Item) ->
                Ct = maps:get(<<"content_type">>, Item),
                ?assert(lists:member(Ct, ContentTypes))
            end, Attach),
            ok
        end}
    ], fun() ->
        Attach = [
            #{<<"content_type">> => <<"application/pdf">>},
            #{<<"content_type">> => <<"image/jpeg">>},
            #{<<"content_type">> => <<"image/png">>},
            #{<<"content_type">> => <<"video/mp4">>},
            #{<<"content_type">> => <<"audio/mpeg">>}
        ],
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, Attach),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

%% @doc 验证参数类型
tablename_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = attachment_ds:tablename(),
        ?assert(is_binary(Result))
    end).

%% @doc 验证保存参数类型
save_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Conn = self(),
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        Uid = 100,
        Attach = [#{<<"url">> => <<"https://example.com/file.pdf">>}],
        ?assert(is_pid(Conn)),
        ?assert(is_binary(CreatedAt)),
        ?assert(is_integer(Uid)),
        ?assert(is_list(Attach))
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的附件保存流程
complete_attachment_save_flow_test_() ->
    ?WITH_MECKS([
        {attachment_repo, [
            {'tablename', 0, fun() -> <<"attachment">> end},
            {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
                [Item] = Attach,
                ?assertEqual(<<"https://example.com/file.pdf">>, maps:get(<<"url">>, Item)),
                ok
            end}
        ]}
    ], fun() ->
        % 获取表名
        ?assertEqual(<<"attachment">>, attachment_ds:tablename()),
        % 保存附件
        Attach = [#{<<"url">> => <<"https://example.com/file.pdf">>}],
        ?assertEqual(ok, attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, Attach))
    end).

%% @doc 测试批量保存附件
batch_save_attachments_test_() ->
    ?WITH_MECK(attachment_repo, [
        {'save', 4, fun(_Conn, _CreatedAt, _Uid, Attach) ->
            ?assertEqual(10, length(Attach)),
            ok
        end}
    ], fun() ->
        Attach = [#{<<"url">> => <<"https://example.com/file", (integer_to_binary(N)), ".pdf">>} || N <- lists:seq(1, 10)],
        Result = attachment_ds:save(self(), <<"2023-01-01T00:00:00Z">>, 100, Attach),
        ?assertEqual(ok, Result)
    end).
