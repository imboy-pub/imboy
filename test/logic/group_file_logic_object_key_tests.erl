-module(group_file_logic_object_key_tests).
-include_lib("eunit/include/eunit.hrl").
%% Pure URL/row projections never boot the app or borrow a suite-wide pool.
-define(WITH_GARAGE_MOCKS(Configs, TestFun),
    {timeout, 30, fun() -> pure_mock_fixture:run(fun() ->
        with_garage(fun() -> pure_mock_fixture:with_mocks(Configs, TestFun) end)
    end) end}).
-define(GARAGE_ASSERT_EQUAL(Expected, Expression),
    {timeout, 30, fun() -> pure_mock_fixture:run(fun() ->
        with_garage(fun() -> ?assertEqual(Expected, Expression) end)
    end) end}).

%% 回归背景（EO-20261006 GROUP_FILE 段 r2）：群文件 list/search 出站行缺
%% object_key，客户端 GroupFilePage._resolveFileUrl 回退 file_url 完整 URL →
%% legacy 分支产出 JWT view_url API URL，WebView 无凭据 404。
%% 修复：logic 层出站统一补 object_key（elib_oss:object_key_from_url 从
%% file_url 剥 scheme/host/bucket；非本桶 URL 置 null——字段恒在）。
%% SQL 侧（group_file_repo select_columns attachment 子查询）已带的
%% object_key（attachment.path）原样保留，仅缺失/null/空串时才派生。

object_key_outbound_test_() ->
    S3Url = <<
        "http://127.0.0.1:3900/object-key-fixture/u116523137566509056/g995301/20261006/"
        "file_1791292334319_DC0C6CB7A45F906E/native-enterprise-file.txt"
    >>,
    ForeignUrl = <<"https://other.example.com/bucket2/some/key.txt">>,
    %% 模拟 SQL 真实行：无附件绑定时 epgsql 返回 null 原子
    Row = fun(Url) ->
        #{
            <<"id">> => 1,
            <<"file_name">> => <<"a.txt">>,
            <<"file_url">> => Url,
            <<"object_key">> => null
        }
    end,
    ?WITH_GARAGE_MOCKS(
        [
            {group_file_ds, [
                {'list_files', 5, fun(_G, _U, _P, _S, _O) ->
                    {ok, [Row(S3Url), Row(ForeignUrl), Row(<<>>)]}
                end},
                {'count_by_group', 1, fun(_G) -> {ok, 3} end},
                {'search_files', 5, fun(_G, _K, _P, _S, _U) ->
                    {ok, [Row(S3Url)]}
                end}
            ]}
        ],
        fun() ->
            {ok, #{<<"items">> := Items}} =
                group_file_logic:list(<<"995301">>, 116523137566509056, 1, 20, #{}),
            [WithKey, Foreign, Empty] = Items,
            ?assertEqual(
                <<
                    "u116523137566509056/g995301/20261006/"
                    "file_1791292334319_DC0C6CB7A45F906E/native-enterprise-file.txt"
                >>,
                maps:get(<<"object_key">>, WithKey)
            ),
            %% 非本桶 URL 与空 file_url：字段恒在、值为 null（客户端按
            %% containsKey 分流，不再回退 legacy 分支）
            ?assertEqual(null, maps:get(<<"object_key">>, Foreign)),
            ?assertEqual(null, maps:get(<<"object_key">>, Empty)),

            {ok, [Searched]} =
                group_file_logic:search(<<"995301">>, <<"a">>, 1, 20, 116523137566509056),
            ?assertMatch(
                #{<<"object_key">> := <<"u116523137566509056/", _/binary>>},
                Searched
            )
        end
    ).

%% SQL 已带非空 binary object_key（attachment.path）的行原样保留，不得被
%% file_url 派生结果覆写（覆盖=legacy 外域行上可用 attachment.path 被降级）。
object_key_preserve_sql_test_() ->
    S3Url = <<"http://127.0.0.1:3900/object-key-fixture/u1/g1/dir/derived.txt">>,
    ForeignUrl = <<"https://legacy.example.com/dfs/old/file.txt">>,
    ?WITH_GARAGE_MOCKS(
        [
            {group_file_ds, [
                {'list_files', 5, fun(_G, _U, _P, _S, _O) ->
                    {ok, [
                        %% 已有 key + 本桶 URL：保留 SQL key，不用派生值
                        #{
                            <<"id">> => 1,
                            <<"file_name">> => <<"sql-key.txt">>,
                            <<"file_url">> => S3Url,
                            <<"object_key">> => <<"u9/attachment/path.txt">>
                        },
                        %% 已有 key + 外域 URL：保留（不降级为 null）
                        #{
                            <<"id">> => 2,
                            <<"file_name">> => <<"legacy.txt">>,
                            <<"file_url">> => ForeignUrl,
                            <<"object_key">> => <<"file_123_abc/legacy.txt">>
                        },
                        %% null + 本桶 URL：派生
                        #{
                            <<"id">> => 3,
                            <<"file_name">> => <<"derive.txt">>,
                            <<"file_url">> => S3Url,
                            <<"object_key">> => null
                        },
                        %% 空串 + 本桶 URL：派生（空串不视为有效 key）
                        #{
                            <<"id">> => 4,
                            <<"file_name">> => <<"empty.txt">>,
                            <<"file_url">> => S3Url,
                            <<"object_key">> => <<>>
                        }
                    ]}
                end},
                {'count_by_group', 1, fun(_G) -> {ok, 4} end},
                {'search_files', 5, fun(_G, _K, _P, _S, _U) -> {ok, []} end}
            ]}
        ],
        fun() ->
            {ok, #{<<"items">> := Items}} =
                group_file_logic:list(<<"995301">>, 116523137566509056, 1, 20, #{}),
            [KeepDerived, KeepLegacy, Derive, DeriveEmpty] = Items,
            ?assertEqual(
                <<"u9/attachment/path.txt">>,
                maps:get(<<"object_key">>, KeepDerived)
            ),
            ?assertEqual(
                <<"file_123_abc/legacy.txt">>,
                maps:get(<<"object_key">>, KeepLegacy)
            ),
            ?assertEqual(<<"u1/g1/dir/derived.txt">>, maps:get(<<"object_key">>, Derive)),
            ?assertEqual(<<"u1/g1/dir/derived.txt">>, maps:get(<<"object_key">>, DeriveEmpty))
        end
    ).

%% object_key_from_url 直测：上传侧 file_url 为 raw 拼接（不做百分号编码），
%% 中文文件名等多字节路径必须正常派生（uri_string:parse 会直接 exit——
%% 独立审查 P0-1 实证），query/fragment 必须剥除。
object_key_from_url_direct_test_() ->
    [
        ?GARAGE_ASSERT_EQUAL(
            <<"u1/g1/20261006/季度报告.txt"/utf8>>,
            elib_oss:object_key_from_url(
                <<"http://127.0.0.1:3900/object-key-fixture/u1/g1/20261006/季度报告.txt"/utf8>>
            )
        ),
        ?GARAGE_ASSERT_EQUAL(
            <<"u1/g1/文件 名 with space.txt"/utf8>>,
            elib_oss:object_key_from_url(
                <<"http://127.0.0.1:3900/object-key-fixture/u1/g1/文件 名 with space.txt"/utf8>>
            )
        ),
        ?GARAGE_ASSERT_EQUAL(
            <<"u1/g1/a.txt">>,
            elib_oss:object_key_from_url(
                <<"http://127.0.0.1:3900/object-key-fixture/u1/g1/a.txt?x=1&y=%202#frag">>
            )
        ),
        ?GARAGE_ASSERT_EQUAL(undefined, elib_oss:object_key_from_url(<<>>)),
        ?GARAGE_ASSERT_EQUAL(
            undefined,
            elib_oss:object_key_from_url(
                <<"http://127.0.0.1:3900/object-key-fixture">>
            )
        ),
        ?GARAGE_ASSERT_EQUAL(
            undefined,
            elib_oss:object_key_from_url(
                <<"https://other.example.com/bucket2/some/key.txt">>
            )
        )
    ].

with_garage(Fun) ->
    Before = application:get_env(imboy, garage),
    ok = application:set_env(imboy, garage, #{
        endpoint => <<"http://127.0.0.1:3900">>,
        bucket => <<"object-key-fixture">>,
        public_bucket => <<"object-key-fixture-public">>
    }),
    try Fun()
    after
        case Before of
            undefined -> application:unset_env(imboy, garage);
            {ok, Value} -> application:set_env(imboy, garage, Value)
        end
    end.
