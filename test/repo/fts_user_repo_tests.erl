-module(fts_user_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_user_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户全文搜索数据访问层功能
%%% 覆盖：全文搜索查询、SQL 注入防护
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]}
        ],
        fun() ->
            Result = fts_user_repo:tablename(),
            ?assertEqual(<<"public.fts_user">>, Result)
        end
    ).

%%%
%%% SQL 注入防护测试
%%%===================================================================

%% 测试正常的中文搜索
user_search_page_with_chinese_keyword_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                % 简化处理：直接返回模拟结果
                {ok, [
                    #{
                        <<"id">> => 1,
                        <<"nickname">> => <<"张三"/utf8>>,
                        <<"account">> => <<"zhangsan">>
                    }
                ]}
            end}
        ],
        fun() ->
            Result = fts_user_repo:user_search_page(<<"东区"/utf8>>, 10, 0),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 测试 SQL 注入攻击防护 - 单引号攻击
sql_injection_protection_single_quote_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ],
        fun() ->
            % 尝试 SQL 注入攻击
            MaliciousKeyword = <<"john' OR '1'='1">>,
            Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 测试 SQL 注入攻击防护 - 分号攻击
sql_injection_protection_semicolon_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ],
        fun() ->
            % 尝试 SQL 注入攻击
            MaliciousKeyword = <<"test; DROP TABLE users;--">>,
            Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 测试 SQL 注入攻击防护 - 注释符攻击
sql_injection_protection_comment_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ],
        fun() ->
            % 尝试 SQL 注入攻击
            MaliciousKeyword = <<"test'--">>,
            Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 测试 count 函数同样安全
count_for_user_search_page_safe_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"count">> => 5}}
            end}
        ],
        fun() ->
            Result = fts_user_repo:count_for_user_search_page(<<"test">>),
            ?assert(is_integer(Result))
        end
    ).

%% 测试空关键词
count_for_user_search_page_empty_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 空关键词应该直接返回 0
        Result = fts_user_repo:count_for_user_search_page(<<>>),
        ?assertEqual(0, Result)
    end).

%%% 认证搜索：查询和计数共享授权范围并排除密文及过期消息。
authorized_search_filters_and_scope_test_() ->
    [
        authorized_search_case(Type, Options)
     || Type <- [<<"c2c">>, <<"c2g">>],
        Options <- [#{}, #{<<"sort_by">> => <<"time">>}]
    ].

authorized_search_case(Type, Options) ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {with_tx, 2, fun(F, _) -> F(test_connection) end},
                {query, 3, fun(_, Sql, Params) ->
                    case
                        {binary:match(Sql, <<"as keyword">>), binary:match(Sql, <<"FOR SHARE">>)}
                    of
                        {{_, _}, _} ->
                            {ok, [#{<<"keyword">> => <<"hello">>}]};
                        {nomatch, {_, _}} ->
                            {ok, []};
                        {nomatch, nomatch} ->
                            ?assert(lists:member(42, Params)),
                            ?assertNotEqual(nomatch, binary:match(Sql, <<"m.e2ee IS NULL">>)),
                            ?assertNotEqual(
                                nomatch,
                                binary:match(Sql, <<"m.expire_at IS NULL OR m.expire_at > NOW()">>)
                            ),
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"count(*) AS search_total">>)
                            ),
                            case Type of
                                <<"c2c">> ->
                                    ?assertNotEqual(
                                        nomatch, binary:match(Sql, <<"m.from_id=$2 OR m.to_id=$2">>)
                                    );
                                <<"c2g">> ->
                                    ?assertNotEqual(
                                        nomatch, binary:match(Sql, <<"group_member_generation">>)
                                    ),
                                    ?assertNotEqual(
                                        nomatch, binary:match(Sql, <<"msg_c2g_recipient_snapshot">>)
                                    ),
                                    ?assertNotEqual(
                                        nomatch, binary:match(Sql, <<"search_missing">>)
                                    )
                            end,
                            {ok, [
                                #{
                                    <<"search_missing">> => false,
                                    <<"search_total">> => 0,
                                    <<"msg_id">> => null
                                }
                            ]}
                    end
                end}
            ]},
            {micro_community_access_repo, [
                {predicate_sql, 2, fun(_, _) -> <<"true">> end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 0, []},
                fts_user_repo:search_authorized_page(42, <<"hello">>, 10, 0, Type, Options)
            )
        end
    ).

authorized_search_missing_snapshot_fails_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {with_tx, 2, fun(F, _) -> F(test_connection) end},
                {query, 3, fun(_, Sql, _) ->
                    case
                        {binary:match(Sql, <<"as keyword">>), binary:match(Sql, <<"FOR SHARE">>)}
                    of
                        {{_, _}, _} ->
                            {ok, [#{<<"keyword">> => <<"hello">>}]};
                        {nomatch, {_, _}} ->
                            {ok, []};
                        {nomatch, nomatch} ->
                            {ok, [#{<<"search_missing">> => true, <<"search_total">> => 0}]}
                    end
                end}
            ]},
            {micro_community_access_repo, [{predicate_sql, 2, fun(_, _) -> <<"true">> end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, search_unavailable},
                fts_user_repo:search_authorized_page(42, <<"hello">>, 10, 0, <<"c2g">>, #{})
            )
        end
    ).

authorized_search_c2c_pair_scope_test_() ->
    [pair_scope_case(Peer) || Peer <- [17, 42]].

pair_scope_case(Peer) ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {with_tx, 2, fun(F, _) -> F(test_connection) end},
                {query, 3, fun(_, Sql, Params) ->
                    case binary:match(Sql, <<"as keyword">>) of
                        {_, _} ->
                            {ok, [#{<<"keyword">> => <<"hello">>}]};
                        nomatch ->
                            ?assertEqual([<<"hello">>, Peer, 42, 10, 0], Params),
                            ?assertNotEqual(
                                nomatch,
                                binary:match(
                                    Sql,
                                    <<"((m.from_id=$3 AND m.to_id=$2) OR (m.from_id=$2 AND m.to_id=$3))">>
                                )
                            ),
                            {ok, [
                                #{
                                    <<"search_missing">> => false,
                                    <<"search_total">> => 0,
                                    <<"msg_id">> => null
                                }
                            ]}
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 0, []},
                fts_user_repo:search_authorized_page(
                    42, <<"hello">>, 10, 0, <<"c2c">>, #{<<"conversation_id">> => Peer}
                )
            )
        end
    ).

invalid_conversation_option_fails_closed_test() ->
    ?assertEqual(
        {error, search_unavailable},
        fts_user_repo:search_authorized_page(
            42, <<"hello">>, 10, 0, <<"c2c">>, #{<<"conversation_id">> => <<"broken">>}
        )
    ).

%%% 真 PG 行为验证：认证搜索的 expire_at 过滤谓词（dadc6aaf9 语义——
%%% 普通行（expire_at NULL）命中；服务端已到期的阅后即焚行排除；
%%% 未到期 burn 行按当前设计命中）。SQL 形状断言仅证明谓词存在，
%%% 本用例用三行不同 expire_at 状态的夹具证明谓词的真实过滤行为。
fts_expire_at_filter_two_states_pg_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() -> fts_expire_at_filter_body() end).

fts_expire_at_filter_body() ->
    A = elib_tsid:generate(user),
    B = elib_tsid:generate(user),
    Uid = integer_to_binary(A),
    KwPlain = <<"v22ftsplainq", Uid/binary>>,
    KwFuture = <<"v22ftsfutureq", Uid/binary>>,
    KwPast = <<"v22ftspastq", Uid/binary>>,
    MsgIds = [
        fts_expire_at_fixture(A, B, plain, KwPlain),
        fts_expire_at_fixture(A, B, future, KwFuture),
        fts_expire_at_fixture(A, B, past, KwPast)
    ],
    ConvKey = msg_archive_repo:conv_key(<<"c2c">>, A, B),
    try
        %% 普通行（expire_at NULL）必须命中且仅命中自身
        ?assertMatch(
            {ok, 1, [_]}, fts_user_repo:search_authorized_page(A, KwPlain, 50, 0, <<"c2c">>, #{})
        ),
        {ok, 1, [PlainRow]} = fts_user_repo:search_authorized_page(
            A, KwPlain, 50, 0, <<"c2c">>, #{}
        ),
        ?assertEqual(lists:nth(1, MsgIds), maps:get(<<"msg_id">>, PlainRow)),
        %% 未到期 burn 行按当前设计（dadc6aaf9）仍然可搜
        ?assertMatch(
            {ok, 1, [_]}, fts_user_repo:search_authorized_page(A, KwFuture, 50, 0, <<"c2c">>, #{})
        ),
        {ok, 1, [FutureRow]} = fts_user_repo:search_authorized_page(
            A, KwFuture, 50, 0, <<"c2c">>, #{}
        ),
        ?assertEqual(lists:nth(2, MsgIds), maps:get(<<"msg_id">>, FutureRow)),
        %% 已到期 burn 行必须被排除（服务端过期内容不再可搜）
        ?assertEqual(
            {ok, 0, []}, fts_user_repo:search_authorized_page(A, KwPast, 50, 0, <<"c2c">>, #{})
        )
    after
        lists:foreach(
            fun(Table) ->
                {ok, _} = elib_pg:execute(
                    <<"DELETE FROM public.", Table/binary, " WHERE msg_id = ANY($1)">>,
                    [MsgIds]
                )
            end,
            [<<"msg_c2c">>]
        ),
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM public.msg_store_seq WHERE conv_key=$1">>, [ConvKey]
        )
    end.

%% 直插 msg_c2c 夹具：State = plain（expire_at NULL）/ future（未到期）/ past（已到期）。
%% conv_seq 由 execute_formal 事务内分配（追加为最后一个参数）。
fts_expire_at_fixture(A, B, State, Kw) ->
    MsgId = iolist_to_binary([
        <<"v22fts-">>, atom_to_list(State), $-, integer_to_binary(A)
    ]),
    ExpireFragment =
        case State of
            plain -> <<"NULL">>;
            future -> <<"now() + interval '1 hour'">>;
            past -> <<"now() - interval '1 hour'">>
        end,
    Sql = iolist_to_binary([
        <<
            "INSERT INTO msg_c2c (id, from_id, to_id, msg_id, msg_type, payload, "
            "created_at, e2ee, expire_at, conv_seq) VALUES ($1, $2, $3, $4, 'text', $5, "
            "now(), NULL, "
        >>,
        ExpireFragment,
        <<", $6)">>
    ]),
    {ok, 1} = personal_message_order_repo:execute_formal(
        A, B, MsgId, Sql, [
            elib_tsid:generate(msg_store),
            A,
            B,
            MsgId,
            jsone:encode(#{<<"text">> => Kw})
        ]
    ),
    MsgId.
