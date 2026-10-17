-module(textguard_export_tests).

%%%
%%% T16 — TextGuard 一致性导出定向测试（AC-072/074/075 导出侧切片）。
%%%
%%% 运行前置（隔离一次性 PG，非共享库、非生产；连接参数只经环境变量注入）：
%%%
%%%   TG16_PG_HOST / TG16_PG_PORT / TG16_PG_DB / TG16_PG_USER / TG16_PG_PASSWORD
%%%
%%% 铁律：环境变量缺失或连不上库 → 显式 FAIL（erlang:error），禁止静默
%%% skip/pass（AG31 同款惯例）。表 `public.sensitive_word`（迁移 00000022
%%% 同构）由套件自建自清（每用例 TRUNCATE）。
%%%
%%% 覆盖：
%%%   * fail-closed：缺 t2s_table_id 不导出；DB 不可达不导出空库；
%%%   * AC-074 黄金字节：固定数据集 + 增强文件 fixture 两次导出逐字节等于
%%%     priv/textguard/golden_ruleset_v1.json；created_at 变化不改字节；
%%%     增强文件缺失 = 空增强（文档化行为）；
%%%   * AC-072 并发一致性：并发 INSERT/UPDATE/DELETE 期间反复导出，
%%%     每个包必须来自单一数据库状态（成对行同代校验 = 写屏障），
%%%     大整数 id（4839475839201234567 / int8 max）字符串无精度损失；
%%%   * AC-075 依附失效：删 canonical → alias/gap/exception 剔除 + 警告；
%%%     改 canonical 文本（sha pin 不匹配）→ 扩展 inert；发布包绝不携带
%%%     悬空引用（rule_id ⊆ literals ids 断言）；
%%%   * 增强文件结构硬校验：未知键/坏 JSON/非法枚举/重复 id → 硬错误。
%%%
%%% 运行：TG16_PG_* 注入后 `make eunit t=textguard_export_tests`。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(POOL, pgsql).
-define(BIG_ID_A, 4839475839201234567).
-define(BIG_ID_B, 9223372036854775807).
-define(GOLDEN_FILE, "priv/textguard/golden_ruleset_v1.json").
-define(ENH_FILE, "priv/textguard/enhancement.example.json").

%% 共享导出（textguard_publisher_tests 复用本套件的 PG 夹具）
-export([pg_setup/0, pg_teardown/1, seed_fixed/0, trunc/0, insert_word/4, delete_word/1]).

%% ===================================================================
%% PG 夹具
%% ===================================================================

-spec conn_opts() -> map().
conn_opts() ->
    Missing = [
        K
     || K <- ["TG16_PG_HOST", "TG16_PG_PORT", "TG16_PG_DB", "TG16_PG_USER"],
        os:getenv(K) =:= false
    ],
    case Missing of
        [] -> ok;
        _ -> erlang:error({tg16_pg_missing_env, Missing})
    end,
    #{
        host => os:getenv("TG16_PG_HOST"),
        port => list_to_integer(os:getenv("TG16_PG_PORT")),
        username => os:getenv("TG16_PG_USER"),
        password => os:getenv("TG16_PG_PASSWORD", ""),
        database => os:getenv("TG16_PG_DB")
    }.

-spec pg_setup() -> ok.
pg_setup() ->
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    case pool_exists() of
        true ->
            ok;
        false ->
            {ok, _} = pooler:new_pool(#{
                name => ?POOL,
                init_count => 2,
                max_count => 8,
                queue_max => 100,
                start_mfa => {epgsql, connect, [conn_opts()]}
            }),
            ok
    end,
    ensure_table(),
    ok.

-spec pg_teardown(ok) -> ok.
pg_teardown(ok) ->
    case pool_exists() of
        true ->
            trunc(),
            pooler:rm_pool(?POOL);
        false ->
            ok
    end,
    application:unset_env(imboy, sql_driver),
    ok.

-spec pool_exists() -> boolean().
pool_exists() ->
    try
        pooler:pool_utilization(?POOL) =/= undefined
    catch
        _:_ -> false
    end.

-spec ensure_table() -> ok.
ensure_table() ->
    Ddl = [
        "CREATE TABLE IF NOT EXISTS public.sensitive_word ("
        " id bigint NOT NULL,"
        " word varchar(128) NOT NULL,"
        " category varchar(32) NOT NULL DEFAULT 'custom',"
        " severity varchar(16) NOT NULL DEFAULT 'medium',"
        " created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,"
        " PRIMARY KEY (id))",
        "CREATE UNIQUE INDEX IF NOT EXISTS uk_sensitive_word_word ON public.sensitive_word(word)",
        "CREATE INDEX IF NOT EXISTS idx_sensitive_word_category ON public.sensitive_word(category)"
    ],
    ok = elib_pg:with_conn(fun(C) ->
        lists:foreach(fun(Sql) -> _ = epgsql:squery(C, Sql) end, Ddl)
    end),
    ok.

-spec trunc() -> ok.
trunc() ->
    ok = elib_pg:with_conn(fun(C) ->
        _ = epgsql:squery(C, "TRUNCATE TABLE public.sensitive_word"),
        ok
    end),
    ok.

-spec insert_word(integer(), binary(), binary(), binary()) -> ok.
insert_word(Id, Word, Category, Severity) ->
    {ok, 1} = elib_pg:execute_count(
        <<
            "INSERT INTO public.sensitive_word (id, word, category, severity, created_at)"
            " VALUES ($1, $2, $3, $4, NOW())"
        >>,
        [Id, Word, Category, Severity]
    ),
    ok.

-spec delete_word(integer()) -> ok.
delete_word(Id) ->
    {ok, _} = elib_pg:execute_count(
        <<"DELETE FROM public.sensitive_word WHERE id = $1">>,
        [Id]
    ),
    ok.

%% 发布者套件同款固定种子（两词，无增强）。
-spec seed_fixed() -> ok.
seed_fixed() ->
    insert_word(99000101, <<"pub-w1">>, <<"pub">>, <<"high">>),
    insert_word(99000102, <<"pub-w2">>, <<"pub">>, <<"medium">>).

%% 黄金数据集（与 priv/textguard/golden_ruleset_v1.json 生成时一致）。
-spec seed_golden() -> ok.
seed_golden() ->
    insert_word(1001, <<"测试词"/utf8>>, <<"test">>, <<"high">>),
    insert_word(1002, <<"演示词"/utf8>>, <<"demo">>, <<"medium">>),
    insert_word(?BIG_ID_A, <<"大整数词"/utf8>>, <<"test">>, <<"low">>).

-spec export_golden_opts() -> map().
export_golden_opts() ->
    #{
        t2s_table_id => <<"test-t2s-v1">>,
        enhancement_file => unicode:characters_to_binary(?ENH_FILE)
    }.

%% ===================================================================
%% 套件组织
%% ===================================================================

textguard_export_test_() ->
    {setup, fun pg_setup/0, fun pg_teardown/1, fun(_) ->
        [
            {"fail-closed: missing t2s_table_id refuses export", fun t_fail_closed_no_t2s/0},
            {"AC-074: golden fixture bytes + shape + id string precision", fun t_golden_bytes/0},
            {"AC-074: byte stability (repeat export / created_at churn / absent enhancement)",
                fun t_byte_stability/0},
            {"AC-075: dangling dependents dropped with warnings; no dangling refs published",
                fun t_dangling_dependents/0},
            {"AC-075: canonical text change makes pinned extension inert", fun t_pin_mismatch/0},
            {"enhancement hard-fail: unknown key / bad json / bad enum / duplicate ids",
                fun t_enhancement_hard_fail/0},
            {"empty table exports explicit empty package with warning", fun t_empty_table/0},
            {"AC-072: consistent snapshot under concurrent writes", fun t_concurrent_snapshot/0},
            {"fail-closed: unreachable DB never exports empty package", fun t_db_unreachable/0}
        ]
    end}.

%% ===================================================================
%% 用例
%% ===================================================================

t_fail_closed_no_t2s() ->
    trunc(),
    ?assertEqual({error, not_ready}, textguard_export:export_package(#{})),
    ?assertEqual({error, not_ready}, textguard_export:export_package(#{t2s_table_id => <<>>})).

t_golden_bytes() ->
    trunc(),
    seed_golden(),
    {ok, Export} = textguard_export:export_package(export_golden_opts()),
    Package = maps:get(package, Export),
    {ok, Golden} = file:read_file(?GOLDEN_FILE),
    ?assertEqual(Golden, Package),
    ?assertEqual(3, maps:get(word_count, Export)),
    ?assertEqual(64, byte_size(maps:get(base_digest, Export))),
    ?assertEqual(
        <<"43e9e4d73fc778c997be01f119d540377ff9bdcb067ad87882f986e514b31553">>,
        maps:get(base_digest, Export)
    ),
    ?assertEqual(
        textguard_export:canonical_word_sha256(<<"nonexistent">>),
        textguard_export:canonical_word_sha256(<<"nonexistent">>)
    ),
    Decoded = jsone:decode(Package),
    ?assertEqual(1, maps:get(<<"schema_version">>, Decoded)),
    ?assertEqual(<<"test-t2s-v1">>, maps:get(<<"t2s_table_id">>, Decoded)),
    ?assertEqual(<<"imboy-base-v1">>, maps:get(<<"normalization_id">>, Decoded)),
    ?assertEqual(<<"mvp-default-v1">>, maps:get(<<"limit_profile_id">>, Decoded)),
    ?assertEqual(false, maps:get(<<"allow_empty">>, Decoded)),
    ?assertEqual(
        #{
            <<"channel_message">> => <<"enhanced-v1">>,
            <<"moment_post">> => <<"enhanced-v1">>,
            <<"profile_field">> => <<"compat-v1">>
        },
        maps:get(<<"semantics_by_scene">>, Decoded)
    ),
    Literals = maps:get(<<"literals">>, Decoded),
    ?assertEqual([<<"1001">>, <<"1002">>, <<"4839475839201234567">>], [
        maps:get(<<"id">>, L)
     || L <- Literals
    ]),
    %% 大整数 id 字符串无精度往返（int8 max 在并发用例中另行覆盖）
    ?assertEqual(?BIG_ID_A, binary_to_integer(<<"4839475839201234567">>)),
    %% 增强合并可见：1001 带 alias+gap；1002 例外挂 rule_id=1002
    Lit1001 = proplists:get_value(<<"1001">>, [{maps:get(<<"id">>, L), L} || L <- Literals]),
    ?assertMatch(
        #{
            <<"aliases">> := [#{<<"variant_id">> := <<"pinyin-01">>}],
            <<"gap">> := #{<<"enabled">> := true, <<"action">> := <<"review">>}
        },
        Lit1001
    ),
    ?assertMatch(
        [#{<<"rule_id">> := <<"1002">>, <<"views">> := [<<"B">>]}],
        maps:get(<<"exceptions">>, Decoded)
    ).

t_byte_stability() ->
    trunc(),
    seed_golden(),
    {ok, E1} = textguard_export:export_package(export_golden_opts()),
    {ok, E2} = textguard_export:export_package(export_golden_opts()),
    ?assertEqual(maps:get(package, E1), maps:get(package, E2)),
    ?assertEqual(maps:get(base_digest, E1), maps:get(base_digest, E2)),
    %% 易变字段排除：仅 created_at 变化不得改字节（导出只投影 4 个语义列）
    ok = elib_pg:with_conn(fun(C) ->
        _ = epgsql:squery(
            C,
            "UPDATE public.sensitive_word SET created_at = created_at + interval '7 hours'"
        ),
        ok
    end),
    {ok, E3} = textguard_export:export_package(export_golden_opts()),
    ?assertEqual(maps:get(package, E1), maps:get(package, E3)),
    ?assertEqual(maps:get(base_digest, E1), maps:get(base_digest, E3)),
    {ok, 3} = elib_pg:execute_count(<<"UPDATE public.sensitive_word SET id = id - 100">>, []),
    {ok, ChangedIds} = textguard_export:export_package(export_golden_opts()),
    ?assertEqual(maps:get(base_digest, E1), maps:get(base_digest, ChangedIds)),
    ?assertNotEqual(maps:get(package_sha256, E1), maps:get(package_sha256, ChangedIds)),
    %% 增强文件缺失 = 空增强（文档化行为）：包仍产出且确定性
    {ok, E4} = textguard_export:export_package(#{
        t2s_table_id => <<"test-t2s-v1">>,
        enhancement_file => <<"/nonexistent/tg16-enh-absent.json">>
    }),
    {ok, E5} = textguard_export:export_package(#{
        t2s_table_id => <<"test-t2s-v1">>,
        enhancement_file => <<"/nonexistent/tg16-enh-absent.json">>
    }),
    ?assertEqual(maps:get(package, E4), maps:get(package, E5)),
    Decoded = jsone:decode(maps:get(package, E4)),
    Literals = maps:get(<<"literals">>, Decoded),
    ?assertEqual(3, length(Literals)),
    lists:foreach(
        fun(L) ->
            ?assertEqual([], maps:get(<<"aliases">>, L)),
            ?assertEqual(false, maps:is_key(<<"gap">>, L))
        end,
        Literals
    ),
    ?assertEqual([], maps:get(<<"exceptions">>, Decoded)),
    ?assertEqual([], maps:get(<<"regex_rules">>, Decoded)).

t_dangling_dependents() ->
    trunc(),
    seed_golden(),
    Sha1001 = textguard_export:canonical_word_sha256(<<"测试词"/utf8>>),
    Sha1002 = textguard_export:canonical_word_sha256(<<"演示词"/utf8>>),
    Enh = #{
        <<"version">> => 1,
        <<"aliases">> => [
            #{
                <<"canonical_id">> => <<"1001">>,
                <<"canonical_word_sha256">> => Sha1001,
                <<"variants">> => [
                    #{
                        <<"variant_id">> => <<"py-01">>,
                        <<"type">> => <<"pinyin">>,
                        <<"pattern">> => <<"ceshici">>,
                        <<"action">> => <<"review">>,
                        <<"boundary">> => <<"ascii_word">>
                    }
                ]
            }
        ],
        <<"gap">> => [
            #{
                <<"canonical_id">> => <<"1001">>,
                <<"canonical_word_sha256">> => Sha1001,
                <<"enabled">> => true,
                <<"action">> => <<"review">>
            }
        ],
        <<"exceptions">> => [
            #{
                <<"id">> => <<"exc-1002">>,
                <<"canonical_id">> => <<"1002">>,
                <<"canonical_word_sha256">> => Sha1002,
                <<"phrase">> => <<"演示词报告"/utf8>>,
                <<"scenes">> => [<<"channel_message">>],
                <<"variant_ids">> => [<<"canonical">>],
                <<"views">> => [<<"B">>]
            }
        ]
    },
    {ok, Base} = textguard_export:export_package(#{
        t2s_table_id => <<"test-t2s-v1">>, enhancement => Enh
    }),
    BaseDecoded = jsone:decode(maps:get(package, Base)),
    ?assertMatch([_], maps:get(<<"exceptions">>, BaseDecoded)),
    %% 删 canonical 1001：其 alias/gap 剔除；包内无 1001 literal；
    %% 有效包绝不携带悬空引用（Rust 侧会整包拒绝）
    delete_word(1001),
    {ok, After} = textguard_export:export_package(#{
        t2s_table_id => <<"test-t2s-v1">>, enhancement => Enh
    }),
    Dropped = maps:get(dropped, After),
    ?assert(
        lists:member(
            #{kind => alias, canonical_id => <<"1001">>, reason => dangling_reference}, Dropped
        )
    ),
    ?assert(
        lists:member(
            #{kind => gap, canonical_id => <<"1001">>, reason => dangling_reference}, Dropped
        )
    ),
    Decoded = jsone:decode(maps:get(package, After)),
    Literals = maps:get(<<"literals">>, Decoded),
    Ids = [maps:get(<<"id">>, L) || L <- Literals],
    ?assertNot(lists:member(<<"1001">>, Ids)),
    ExceptionRuleIds = [maps:get(<<"rule_id">>, X) || X <- maps:get(<<"exceptions">>, Decoded)],
    ?assertEqual([], [R || R <- ExceptionRuleIds, not lists:member(R, Ids)]),
    lists:foreach(
        fun(L) ->
            ?assertEqual(false, maps:is_key(<<"gap">>, L)),
            ?assertEqual([], maps:get(<<"aliases">>, L))
        end,
        Literals
    ),
    Warnings = maps:get(warnings, After),
    ?assert(
        lists:any(
            fun(W) ->
                binary:match(
                    W, <<"dropped alias extension for canonical 1001: dangling_reference">>
                ) =/= nomatch
            end,
            Warnings
        )
    ).

t_pin_mismatch() ->
    trunc(),
    seed_golden(),
    Sha1002 = textguard_export:canonical_word_sha256(<<"演示词"/utf8>>),
    Enh = #{
        <<"version">> => 1,
        <<"exceptions">> => [
            #{
                <<"id">> => <<"exc-1002">>,
                <<"canonical_id">> => <<"1002">>,
                <<"canonical_word_sha256">> => Sha1002,
                <<"phrase">> => <<"演示词报告"/utf8>>,
                <<"scenes">> => [<<"channel_message">>],
                <<"variant_ids">> => [<<"canonical">>],
                <<"views">> => [<<"B">>]
            }
        ]
    },
    {ok, Base} = textguard_export:export_package(#{
        t2s_table_id => <<"test-t2s-v1">>, enhancement => Enh
    }),
    ?assertMatch([_], maps:get(<<"exceptions">>, jsone:decode(maps:get(package, Base)))),
    %% 改 canonical 文本：增强文件的 pin（canonical_word_sha256）不再匹配
    %% → 扩展 inert，从有效包剔除
    {ok, _} = elib_pg:execute_count(
        <<"UPDATE public.sensitive_word SET word = $1 WHERE id = 1002">>,
        [<<"演示词改"/utf8>>]
    ),
    {ok, After} = textguard_export:export_package(#{
        t2s_table_id => <<"test-t2s-v1">>, enhancement => Enh
    }),
    ?assertMatch(
        [
            #{
                kind := exception,
                canonical_id := <<"1002">>,
                reason := sha256_mismatch,
                id := <<"exc-1002">>
            }
        ],
        maps:get(dropped, After)
    ),
    ?assertMatch([], maps:get(<<"exceptions">>, jsone:decode(maps:get(package, After)))).

t_enhancement_hard_fail() ->
    trunc(),
    seed_golden(),
    UnknownKey = #{<<"version">> => 1, <<"totally_unknown">> => 1},
    ?assertMatch(
        {error, {invalid_enhancement, {unknown_keys, _}}},
        textguard_export:export_package(#{t2s_table_id => <<"t">>, enhancement => UnknownKey})
    ),
    BadAction = #{
        <<"version">> => 1,
        <<"aliases">> => [
            #{
                <<"canonical_id">> => <<"1001">>,
                <<"canonical_word_sha256">> => <<"00">>,
                <<"variants">> => [
                    #{
                        <<"variant_id">> => <<"v">>,
                        <<"type">> => <<"pinyin">>,
                        <<"pattern">> => <<"p">>,
                        <<"action">> => <<"allow">>,
                        <<"boundary">> => <<"ascii_word">>
                    }
                ]
            }
        ]
    },
    ?assertMatch(
        {error, {invalid_enhancement, {bad_alias_variant, _}}},
        textguard_export:export_package(#{t2s_table_id => <<"t">>, enhancement => BadAction})
    ),
    DupVariant = #{
        <<"version">> => 1,
        <<"aliases">> => [
            #{
                <<"canonical_id">> => <<"1001">>,
                <<"canonical_word_sha256">> => <<"00">>,
                <<"variants">> => [
                    #{
                        <<"variant_id">> => <<"v">>,
                        <<"type">> => <<"a">>,
                        <<"pattern">> => <<"p">>,
                        <<"action">> => <<"review">>,
                        <<"boundary">> => <<"ascii_word">>
                    },
                    #{
                        <<"variant_id">> => <<"v">>,
                        <<"type">> => <<"b">>,
                        <<"pattern">> => <<"q">>,
                        <<"action">> => <<"review">>,
                        <<"boundary">> => <<"ascii_word">>
                    }
                ]
            }
        ]
    },
    ?assertMatch(
        {error, {invalid_enhancement, {duplicate_variant_id, <<"1001">>, <<"v">>}}},
        textguard_export:export_package(#{t2s_table_id => <<"t">>, enhancement => DupVariant})
    ),
    %% 坏 JSON 文件：硬错误（绝不静默当空增强）
    Tmp = "/tmp/tg16-enh-corrupt.json",
    ok = file:write_file(Tmp, <<"{ not json">>),
    ?assertMatch(
        {error, {invalid_enhancement, {bad_json, _}}},
        textguard_export:export_package(#{
            t2s_table_id => <<"t">>, enhancement_file => unicode:characters_to_binary(Tmp)
        })
    ),
    %% regex id 必须 re: 前缀
    BadRegex = #{
        <<"version">> => 1,
        <<"regex_rules">> => [
            #{
                <<"id">> => <<"promo-01">>,
                <<"expression">> => <<"x">>,
                <<"display_word">> => <<"x">>,
                <<"category">> => <<"ad">>,
                <<"severity">> => <<"medium">>,
                <<"action">> => <<"review">>,
                <<"scenes">> => [<<"channel_message">>]
            }
        ]
    },
    ?assertMatch(
        {error, {invalid_enhancement, {bad_regex_entry, _}}},
        textguard_export:export_package(#{t2s_table_id => <<"t">>, enhancement => BadRegex})
    ).

t_empty_table() ->
    trunc(),
    {ok, E} = textguard_export:export_package(#{t2s_table_id => <<"test-t2s-v1">>}),
    ?assertEqual(0, maps:get(word_count, E)),
    Decoded = jsone:decode(maps:get(package, E)),
    ?assertEqual([], maps:get(<<"literals">>, Decoded)),
    ?assertEqual(false, maps:get(<<"allow_empty">>, Decoded)),
    ?assert(
        lists:any(
            fun(W) ->
                binary:match(W, <<"canonical word table is empty">>) =/= nomatch
            end,
            maps:get(warnings, E)
        )
    ).

%% -------------------------------------------------------------------
%% AC-072：并发写期间的一致快照
%% -------------------------------------------------------------------

t_concurrent_snapshot() ->
    trunc(),
    seed_golden(),
    insert_word(?BIG_ID_B, <<"int8-max">>, <<"test">>, <<"low">>),
    %% 写屏障设计：每个 writer 拥有若干「行对」(2K, 2K+1)；对内两行的一切
    %% 变更都是单语句（UPDATE ... WHERE id IN (a,b) / DELETE IN / 多值
    %% INSERT），数据库任何单语句快照看到的两行要么同代、要么同缺失。
    %% 分页/多次读拼包（混合版本）必然出现半对 —— 用本断言抓出来。
    PairIds = [{99000200 + 2 * K, 99000201 + 2 * K} || K <- lists:seq(0, 11)],
    Owners = [
        {W, [P || {K, P} <- lists:zip(lists:seq(0, 11), PairIds), K rem 3 =:= W]}
     || W <- lists:seq(0, 2)
    ],
    lists:foreach(
        fun({_W, Pairs}) ->
            lists:foreach(
                fun({A, B}) ->
                    insert_word(
                        A,
                        <<"row-", (integer_to_binary(A))/binary, "-g0">>,
                        <<"stress">>,
                        <<"medium">>
                    ),
                    insert_word(
                        B,
                        <<"row-", (integer_to_binary(B))/binary, "-g0">>,
                        <<"stress">>,
                        <<"medium">>
                    )
                end,
                Pairs
            )
        end,
        Owners
    ),
    Parent = self(),
    Writers = [
        spawn_link(fun() ->
            writer_loop(W, Pairs, conn_opts()),
            Parent ! {writer_done, W}
        end)
     || {W, Pairs} <- Owners
    ],
    %% 并发写期间反复导出：每个包做完整一致性校验
    Packages = [
        begin
            {ok, E} = textguard_export:export_package(#{t2s_table_id => <<"test-t2s-v1">>}),
            maps:get(package, E)
        end
     || _ <- lists:seq(1, 20)
    ],
    [W ! stop || W <- Writers],
    [
        receive
            {writer_done, _} -> ok
        after 5000 -> error(writer_not_done)
        end
     || _ <- Writers
    ],
    ?assertEqual(20, length(Packages)),
    lists:foreach(
        fun(Pkg) ->
            assert_pair_consistent(Pkg, PairIds),
            assert_static_rows(Pkg)
        end,
        Packages
    ).

-spec assert_pair_consistent(binary(), [{integer(), integer()}]) -> ok.
assert_pair_consistent(Pkg, PairIds) ->
    Decoded = jsone:decode(Pkg),
    ById = maps:from_list([{maps:get(<<"id">>, L), L} || L <- maps:get(<<"literals">>, Decoded)]),
    lists:foreach(
        fun({A, B}) ->
            AId = integer_to_binary(A),
            BId = integer_to_binary(B),
            case {maps:find(AId, ById), maps:find(BId, ById)} of
                {error, error} ->
                    ok;
                {{ok, LA}, {ok, LB}} ->
                    ?assertEqual(
                        pair_gen(LA),
                        pair_gen(LB),
                        {mixed_version_pair, A, B, maps:get(<<"word">>, LA),
                            maps:get(<<"word">>, LB)}
                    );
                Mixed ->
                    ?assertEqual(ok, {consistent_pair_required, Mixed, A, B})
            end
        end,
        PairIds
    ).

-spec pair_gen(map()) -> integer().
pair_gen(L) ->
    Word = maps:get(<<"word">>, L),
    case binary:split(Word, <<"-g">>, [global]) of
        Parts -> binary_to_integer(lists:last(Parts))
    end.

-spec assert_static_rows(binary()) -> ok.
assert_static_rows(Pkg) ->
    Decoded = jsone:decode(Pkg),
    ById = maps:from_list([{maps:get(<<"id">>, L), L} || L <- maps:get(<<"literals">>, Decoded)]),
    %% 大整数 id 字符串无精度存活（int8 max 同样不溢出）
    ?assertMatch(
        #{<<"word">> := <<"大整数词"/utf8>>},
        maps:get(<<"4839475839201234567">>, ById, undefined)
    ),
    ?assertMatch(
        #{<<"word">> := <<"int8-max"/utf8>>},
        maps:get(<<"9223372036854775807">>, ById, undefined)
    ).

-spec writer_loop(non_neg_integer(), [{integer(), integer()}], map()) -> ok.
writer_loop(W, Pairs, ConnOpts) ->
    {ok, Conn} = epgsql:connect(ConnOpts),
    writer_loop(Conn, W, Pairs, 0).

writer_loop(Conn, W, Pairs, Gen) ->
    receive
        stop ->
            epgsql:close(Conn),
            ok
    after 0 ->
        {A, B} = lists:nth((Gen rem length(Pairs)) + 1, Pairs),
        Prefix = <<"cw", (integer_to_binary(W))/binary, "p", (integer_to_binary(A))/binary>>,
        GenBin = integer_to_binary(Gen),
        Op = Gen rem 5,
        _ =
            case Op of
                3 ->
                    %% 整对删除 + 整对重插（两条单语句，各自原子）
                    {ok, _} = epgsql:equery(
                        Conn,
                        "DELETE FROM public.sensitive_word WHERE id IN ($1, $2)",
                        [A, B]
                    ),
                    {ok, 2} = epgsql:equery(
                        Conn,
                        "INSERT INTO public.sensitive_word (id, word, category, severity, created_at)"
                        " VALUES ($1, $3 || '-' || $1::text || '-g' || $2, 'stress', 'medium', NOW()),"
                        " ($4, $3 || '-' || $4::text || '-g' || $2, 'stress', 'medium', NOW())",
                        [A, GenBin, Prefix, B]
                    );
                _ ->
                    %% 整对同代更新（单语句原子改两行）
                    {ok, _} = epgsql:equery(
                        Conn,
                        "UPDATE public.sensitive_word SET word = $3 || '-' || id::text || '-g' || $2"
                        " WHERE id IN ($1, $4)",
                        [A, GenBin, Prefix, B]
                    )
            end,
        timer:sleep(rand:uniform(9)),
        writer_loop(Conn, W, Pairs, Gen + 1)
    end.

%% DB 完全不可达（无可用连接）：绝不导出（更不导出空包）
t_db_unreachable() ->
    %% 先移除池（本用例是该 {setup} 组最后一个；teardown 的 rm_pool 幂等）
    case pooler:pool_utilization(?POOL) of
        undefined -> ok;
        _ -> pooler:rm_pool(?POOL)
    end,
    ?assertMatch(
        {error, {db_read_failed, _}},
        textguard_export:export_package(#{t2s_table_id => <<"test-t2s-v1">>})
    ).
