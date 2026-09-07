-module(moderation_policy_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% R-03 内容审核 policy 入口测试：
%%% hit/no-hit、Unicode 归一化、severity 决策、词表失败 fail-open、入队
%%%===================================================================

normalize_text_fullwidth_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 全角转半角后命中（字面量必须带 /utf8，否则编译期即被破坏）
        ?assertEqual(
            <<"abadword">>,
            moderation_policy:normalize_text(<<"ＡＢＡＤＷＯＲＤ"/utf8>>)
        ),
        %% 大写归一
        ?assertEqual(<<"badword">>, moderation_policy:normalize_text(<<"BadWord">>)),
        %% 零宽字符剥离（绕过手段）
        ZeroWidth = <<98, 226, 128, 139, 97, 100, 119, 111, 114, 100>>,
        ?assertEqual(<<"badword">>, moderation_policy:normalize_text(ZeroWidth))
    end).

inspect_allows_clean_text_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'memo', 3, fun(F, _Key, _TTL) -> F() end}
            ]},
            {sensitive_word_repo, [
                {'all', 0, fun() -> {ok, []} end}
            ]}
        ],
        fun() ->
            %% 空文本直接放行，不打词表
            ?assertEqual(allow, moderation_policy:inspect(moment_post, <<>>)),
            ?assertEqual(0, meck:num_calls(sensitive_word_repo, all, 0)),
            ?assertEqual(allow, moderation_policy:inspect(channel_message, <<"正常内容"/utf8>>)),
            ?assertEqual(1, meck:num_calls(sensitive_word_repo, all, 0))
        end
    ).

inspect_blocks_high_severity_hit_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'memo', 3, fun(F, _Key, _TTL) -> F() end}
            ]},
            {sensitive_word_repo, [
                {'all', 0, fun() ->
                    {ok, [
                        #{
                            <<"word">> => <<"badword">>,
                            <<"category">> => <<"abuse">>,
                            <<"severity">> => <<"high">>
                        },
                        #{
                            <<"word">> => <<"spam">>,
                            <<"category">> => <<"ad">>,
                            <<"severity">> => <<"medium">>
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            %% high 命中 → blocked（同一文本的 medium 命中也一并列出）
            {blocked, Hits} = moderation_policy:inspect(
                channel_message, <<"this is BadWord and spam">>
            ),
            ?assertEqual(2, length(Hits)),
            %% 全角/零宽变体不能绕过
            ?assertMatch(
                {blocked, _}, moderation_policy:inspect(moment_post, <<"ＢＡＤＷＯＲＤ"/utf8>>)
            )
        end
    ).

inspect_queues_low_medium_hit_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'memo', 3, fun(F, _Key, _TTL) -> F() end}
            ]},
            {sensitive_word_repo, [
                {'all', 0, fun() ->
                    {ok, [
                        #{
                            <<"word">> => <<"spam">>,
                            <<"category">> => <<"ad">>,
                            <<"severity">> => <<"medium">>
                        },
                        #{
                            <<"word">> => <<"mild">>,
                            <<"category">> => <<"custom">>,
                            <<"severity">> => <<"low">>
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            {queued, Hits} = moderation_policy:inspect(channel_message, <<"buy spam now">>),
            ?assertEqual(1, length(Hits)),
            ?assertEqual(<<"spam">>, maps:get(word, hd(Hits))),
            ?assertEqual(<<"medium">>, maps:get(severity, hd(Hits)))
        end
    ).

inspect_fail_open_when_wordlist_unavailable_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'memo', 3, fun(F, _Key, _TTL) -> F() end}
            ]},
            {sensitive_word_repo, [
                {'all', 0, fun() -> {error, pg_down} end}
            ]}
        ],
        fun() ->
            %% 词表读取失败 → fail-open 放行（检查设施故障≠违规证据）
            ?assertEqual(allow, moderation_policy:inspect(channel_message, <<"badword">>))
        end
    ).

enqueue_writes_pending_row_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 0, fun() -> 9001 end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"review_queue">>) -> <<"public.review_queue">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"'pending'">>)),
                    ?assertEqual(
                        [
                            9001,
                            501,
                            <<"channel_message">>,
                            <<"bad content">>,
                            1001,
                            <<>>,
                            11,
                            <<"channel">>,
                            <<"badword">>
                        ],
                        Params
                    ),
                    {ok, [#{<<"id">> => 9001}]}
                end}
            ]}
        ],
        fun() ->
            Hits = [#{word => <<"badword">>, severity => <<"high">>}],
            ok = moderation_policy:enqueue(
                channel_message, 501, 11, 1001, <<>>, <<"bad content">>, Hits
            )
        end
    ).

enqueue_failure_returned_fail_open_by_caller_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 0, fun() -> 9002 end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"review_queue">>) -> <<"public.review_queue">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {error, pg_down} end}
            ]}
        ],
        fun() ->
            Hits = [#{word => <<"badword">>, severity => <<"medium">>}],
            ?assertEqual(
                {error, pg_down},
                moderation_policy:enqueue(moment_post, 601, 0, 1001, <<>>, <<"c">>, Hits)
            )
        end
    ).
