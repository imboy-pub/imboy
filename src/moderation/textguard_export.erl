%%% @doc TextGuard 规则包一致性导出（T16，AC-072/074/075 的导出侧切片）。
%%%
%%% 从 IMBoy PostgreSQL `sensitive_word` 表（canonical 词表真源，迁移
%%% 00000022：bigint id / varchar word / varchar category / varchar severity）
%%% 读取全部词条，与**版本化增强文件**（别名/gap/例外/正则，JSON）合并，
%%% 产出 Rust TextGuard RuleSet v1 有效规则包（plan 附录 B.1）。
%%%
%%% == 一致性快照（AC-072）==
%%%
%%% 全表读取是**单条 SQL**（`SELECT id, word, category, severity ... ORDER BY id`）：
%%% PostgreSQL 单语句自带 statement-level 快照，不存在分页读出混合版本包的
%%% 可能；绝不用 LIMIT/OFFSET 分页拼接。64-bit bigint id 在 BEAM 内是任意
%%% 精度整数，序列化为 JSON **字符串**（`integer_to_binary/1`）后无精度损失。
%%%
%%% == 字节稳定序列化（AC-074）==
%%%
%%% * `jsone:encode(Pkg, [native_utf8, canonical_form])`：对象键排序、
%%%   UTF-8 直出，同一数据必产同一字节；
%%% * 列表定序：literals 按数值 id 升序（SQL ORDER BY id）；aliases 按
%%%   variant_id、exceptions/regex_rules 按 id 排序；
%%% * **排除易变字段**：只投影 `id/word/category/severity` 四个语义列，
%%%   `created_at`（及未来 T20 可能加的 `updated_at`/`status` 等易变列）
%%%   不进包——仅时间戳变化不产生新版本字节；
%%% * base digest = canonical 词表核（仅四列核心、按数值 id 排序、
%%%   canonical_form 编码）的 sha256 十六进制小写；package_sha256 =
%%%   完整包字节的 sha256。两者均导出给发布审计/对账。
%%%
%%% == 依附扩展失效（AC-075，plan §8 / B.3）==
%%%
%%% 增强文件引用 canonical id 时必须携带 `canonical_word_sha256`（该词
%%% 原文的 sha256 十六进制小写，可用 `canonical_word_sha256/1` 离线计算）：
%%%
%%% * 引用的 canonical id 已不存在（删词）→ 该 alias/gap/exception 从
%%%   有效包中**剔除**并记 warning（Rust 侧对悬空引用整包拒绝，本侧必须
%%%   先剔除才允许发布）；
%%% * canonical 文本已改（sha256 不匹配）→ 扩展 inert，同样剔除并记 warning；
%%% * 增强文件结构性错误（未知字段/非法枚举/重复 id/坏 JSON）→
%%%   `{error, {invalid_enhancement, ...}}` 硬失败，绝不静默降级发布。
%%%
%%% == 增强文件（版本化 JSON）==
%%%
%%% 路径来自配置 `{imboy, textguard_export}` 的 `{enhancement_file, Path}`；
%%% **文件不存在** → 以空增强导出（部署最小形态，文档化行为）；文件存在
%%% 但损坏/结构非法 → 硬错误。文件顶层结构（严格键集，未知键报错）见
%%% `priv/textguard/enhancement.example.json`：`version`、
%%% `semantics_by_scene`、`aliases`、`gap`、`regex_rules`、`exceptions`。
%%% 例外合并进包时 `rule_id` 即所依附 canonical 的 id 字符串；regex id 强制
%%% `re:` 前缀（与数据库数字 id 命名空间分离，plan 附录 B.1 尾注）。
%%%
%%% == 包形状（plan 附录 B.1）==
%%%
%%% 每条 literal：`id`（字符串）/`word`/`category`/`severity`/`scenes`
%%% （缺省三场景全开）+ `aliases`（可为空数组，恒出现）+ `gap`（默认显式
%%% enabled=false/action=review）。`schema_version=1`、`allow_empty=false`
%%% 固定；`normalization_id`（默认 imboy-base-v1）/`limit_profile_id`
%%% （默认 mvp-default-v1）/`semantics_by_scene`（默认
%%% channel_message=enhanced-v1、moment_post=enhanced-v1、
%%% profile_field=compat-v1）可由配置/增强文件覆盖（增强文件优先）；
%%% `t2s_table_id` **必须**由配置提供（生产用 T07 真实表 id，缺失即
%%% fail-closed `{error, not_ready}`，不拿 fixture 表 id 冒充）。
%%%
%%% == 配置（application env `{imboy, textguard_export}`）==
%%%
%%%   `{t2s_table_id, binary()}`          必填（fail-closed）
%%%   `{enhancement_file, binary()}`      增强文件路径，缺省无增强
%%%   `{normalization_id, binary()}`      默认 <<"imboy-base-v1">>
%%%   `{limit_profile_id, binary()}`      默认 <<"mvp-default-v1">>
%%%   `{semantics_by_scene, map()}`       三场景语义覆盖
%%%
%%% 注：按 plan T16 所有权约束「既有 repo 查询扩展由 I 串行合入」，本模块
%%% 未改动 `sensitive_word_repo`，快照查询直接经 `elib_pg`（表名复用
%%% `sensitive_word_repo:tablename/0` 单一真源）；T20 接线真实集成时如需
%%% 收敛回 repo 层再行迁移。
-module(textguard_export).

-export([export_package/0, export_package/1, canonical_word_sha256/1]).

-export_type([export_result/0, canonical_word/0, enhancement/0]).

-include_lib("kernel/include/logger.hrl").

-define(SCENES, [<<"channel_message">>, <<"moment_post">>, <<"profile_field">>]).
-define(DEFAULT_SEMANTICS, #{
    <<"channel_message">> => <<"enhanced-v1">>,
    <<"moment_post">> => <<"enhanced-v1">>,
    <<"profile_field">> => <<"compat-v1">>
}).
-define(DEFAULT_NORMALIZATION_ID, <<"imboy-base-v1">>).
-define(DEFAULT_LIMIT_PROFILE_ID, <<"mvp-default-v1">>).
-define(ALLOWED_SEMANTICS, [<<"compat-v1">>, <<"enhanced-v1">>]).
-define(ALLOWED_ACTIONS, [<<"review">>, <<"block">>]).
-define(ALLOWED_VIEWS, [<<"B">>, <<"T">>]).
-define(ENH_TOP_KEYS, [version, semantics_by_scene, aliases, gap, regex_rules, exceptions]).

%% 导出结果元数据：package = 包字节；base_digest = canonical 核 sha256；
%% package_sha256 = 完整包字节 sha256；dropped = 被剔除的依附扩展及原因。
-type export_result() :: #{
    package := binary(),
    base_digest := binary(),
    package_sha256 := binary(),
    word_count := non_neg_integer(),
    dropped := [map()],
    warnings := [binary()]
}.
-type canonical_word() :: #{
    id := integer(),
    word := binary(),
    category := binary(),
    severity := binary()
}.
-type enhancement() :: map().

%% ===================================================================
%% API
%% ===================================================================

%% @doc 按配置导出（读 `{imboy, textguard_export}` 应用环境）。
-spec export_package() -> {ok, export_result()} | {error, term()}.
export_package() ->
    Conf = config_ds:env(textguard_export, []),
    export_package(#{
        t2s_table_id => proplists:get_value(t2s_table_id, Conf),
        enhancement_file => proplists:get_value(enhancement_file, Conf),
        enhancement => proplists:get_value(enhancement, Conf),
        normalization_id => proplists:get_value(normalization_id, Conf),
        limit_profile_id => proplists:get_value(limit_profile_id, Conf),
        semantics_by_scene => proplists:get_value(semantics_by_scene, Conf)
    }).

%% @doc 带 Option 覆盖导出（测试/发布者注入用）。
%%%
%%% Opts：
%%%   `t2s_table_id` 必填二进制；
%%%   `enhancement_file` 路径二进制（文件缺失 = 空增强）；
%%%   `enhancement` 已解析/已构造的增强 map（优先于 enhancement_file）；
%%%   `normalization_id` / `limit_profile_id` / `semantics_by_scene` 覆盖默认。
-spec export_package(map()) -> {ok, export_result()} | {error, term()}.
export_package(Opts) when is_map(Opts) ->
    with_t2s_table_id(
        Opts,
        fun(T2sId) ->
            case safe_read_canonical_words() of
                {ok, Words} ->
                    merge_and_encode(Opts, T2sId, Words);
                {error, Reason} ->
                    %% DB 读取失败绝不导出空库（T16 禁止条件）
                    ?LOG_WARNING("textguard_export: canonical words read failed: ~p", [Reason]),
                    {error, {db_read_failed, Reason}}
            end
        end
    ).

%% DB 边界 fail-closed：池不可用/连接层崩溃（exit）也收敛成 {error, _}，
%%% 绝不让发布者进程被 DB 抖动带走，更不把失败误当空表导出。
-spec safe_read_canonical_words() -> {ok, [canonical_word()]} | {error, term()}.
safe_read_canonical_words() ->
    try
        read_canonical_words()
    catch
        exit:Reason -> {error, {db_exit, Reason}};
        error:Reason -> {error, {db_error, Reason}}
    end.

%% @doc canonical 词文本的依附 pin（增强文件 canonical_word_sha256 用），
%%% sha256 十六进制小写。操作员可用此函数离线计算增强文件的 pin 值。
-spec canonical_word_sha256(binary()) -> binary().
canonical_word_sha256(Word) when is_binary(Word) ->
    hex_lower(crypto:hash(sha256, Word)).

%% ===================================================================
%% canonical 词表读取（单语句一致性快照）
%% ===================================================================

%% 单条 SELECT：statement-level 快照保证包内所有行来自同一数据库状态；
%% ORDER BY id 让结果序即发布序（数值升序），无分页、无二次查询。
-spec read_canonical_words() -> {ok, [canonical_word()]} | {error, term()}.
read_canonical_words() ->
    Tb = sensitive_word_repo:tablename(),
    Sql = <<"SELECT id, word, category, severity FROM ", Tb/binary, " ORDER BY id">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            normalize_rows(Rows);
        {error, Reason} ->
            {error, Reason}
    end.

-spec normalize_rows([map()]) -> {ok, [canonical_word()]} | {error, term()}.
normalize_rows(Rows) ->
    normalize_rows(Rows, []).

normalize_rows([], Acc) ->
    {ok, lists:reverse(Acc)};
normalize_rows([Row | Rest], Acc) ->
    case normalize_row(Row) of
        {ok, Word} ->
            normalize_rows(Rest, [Word | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

%% 行校验：id 归一为整数（epgsql int8 本就解成任意精度整数；文本形态
%% 防御性归一），word/category/severity 必须非空二进制。
-spec normalize_row(map()) -> {ok, canonical_word()} | {error, term()}.
normalize_row(Row) ->
    Id0 = maps:get(<<"id">>, Row, undefined),
    Word = maps:get(<<"word">>, Row, undefined),
    Category = maps:get(<<"category">>, Row, undefined),
    Severity = maps:get(<<"severity">>, Row, undefined),
    case normalize_id(Id0) of
        {ok, Id} when
            is_binary(Word),
            Word =/= <<>>,
            is_binary(Category),
            Category =/= <<>>,
            is_binary(Severity),
            Severity =/= <<>>
        ->
            {ok, #{id => Id, word => Word, category => Category, severity => Severity}};
        {ok, _} ->
            {error, {invalid_canonical_row, Row}};
        error ->
            {error, {invalid_canonical_id, Id0}}
    end.

-spec normalize_id(integer() | binary() | term()) -> {ok, integer()} | error.
normalize_id(Id) when is_integer(Id) ->
    {ok, Id};
normalize_id(Id) when is_binary(Id) ->
    try
        {ok, binary_to_integer(Id)}
    catch
        _:_ -> error
    end;
normalize_id(_) ->
    error.

%% ===================================================================
%% 合并与编码
%% ===================================================================

-spec with_t2s_table_id(map(), fun((binary()) -> R)) -> R | {error, not_ready}.
with_t2s_table_id(Opts, Fun) ->
    case maps:get(t2s_table_id, Opts, undefined) of
        T2sId when is_binary(T2sId), T2sId =/= <<>> ->
            Fun(T2sId);
        _ ->
            %% fail-closed：绝不拿 fixture 表 id 冒充生产 t2s 表
            {error, not_ready}
    end.

-spec merge_and_encode(map(), binary(), [canonical_word()]) ->
    {ok, export_result()} | {error, term()}.
merge_and_encode(Opts, T2sId, Words) ->
    case load_enhancement(Opts) of
        {ok, Enh} ->
            merge_validated(Opts, T2sId, Words, Enh);
        {error, Reason} ->
            {error, Reason}
    end.

%% -------------------------------------------------------------------
%% 增强文件装载与结构校验
%% -------------------------------------------------------------------

%% `enhancement` 显式 map 优先（测试注入）；否则读 enhancement_file：
%% 文件不存在 = 空增强（文档化最小部署形态）；存在但坏 = 硬错误。
-spec load_enhancement(map()) -> {ok, enhancement()} | {error, term()}.
load_enhancement(Opts) ->
    case maps:get(enhancement, Opts, undefined) of
        Enh when is_map(Enh) ->
            validate_enhancement(Enh);
        _ ->
            case maps:get(enhancement_file, Opts, undefined) of
                Path when is_binary(Path), Path =/= <<>> ->
                    load_enhancement_file(Path);
                _ ->
                    validate_enhancement(#{})
            end
    end.

-spec load_enhancement_file(binary()) -> {ok, enhancement()} | {error, term()}.
load_enhancement_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case jsone:try_decode(Bin) of
                {ok, Enh, _Rest} when is_map(Enh) ->
                    validate_enhancement(Enh);
                {ok, _NotAMap, _Rest} ->
                    {error, {invalid_enhancement, not_an_object}};
                {error, Reason} ->
                    {error, {invalid_enhancement, {bad_json, Reason}}}
            end;
        {error, enoent} ->
            {ok, #{version => 1}};
        {error, Reason} ->
            {error, {enhancement_read_failed, Reason}}
    end.

%% 校验管线：顶层严格键集 → version → semantics → aliases → gap →
%% regex → exceptions；内部键统一归一为二进制（jsone 解码形态）。
-spec validate_enhancement(map()) -> {ok, enhancement()} | {error, term()}.
validate_enhancement(Enh) ->
    Norm = normalize_json_keys(Enh),
    case extra_keys(Norm, ?ENH_TOP_KEYS) of
        [] ->
            check_enh_version(Norm);
        Unknown ->
            {error, {invalid_enhancement, {unknown_keys, Unknown}}}
    end.

-spec check_enh_version(map()) -> {ok, enhancement()} | {error, term()}.
check_enh_version(Enh) ->
    case maps:get(<<"version">>, Enh, 1) of
        1 ->
            check_enh_semantics(Enh);
        V ->
            {error, {invalid_enhancement, {unsupported_version, V}}}
    end.

-spec check_enh_semantics(map()) -> {ok, enhancement()} | {error, term()}.
check_enh_semantics(Enh) ->
    case maps:get(<<"semantics_by_scene">>, Enh, #{}) of
        Override when is_map(Override) ->
            check_enh_aliases(Enh, normalize_json_keys(Override));
        Other ->
            {error, {invalid_enhancement, {bad_semantics_by_scene, Other}}}
    end.

-spec check_enh_aliases(map(), map()) -> {ok, enhancement()} | {error, term()}.
check_enh_aliases(Enh, Semantics) ->
    Aliases = list_of_maps(maps:get(<<"aliases">>, Enh, [])),
    case validate_alias_entries(Aliases, #{}, []) of
        {ok, NormAliases} ->
            check_enh_gap(Enh, Semantics, NormAliases);
        {error, Reason} ->
            {error, {invalid_enhancement, Reason}}
    end.

%% Seen 追踪 {CanonicalId, VariantId} 全局唯一（跨多条 alias 组）。
-spec validate_alias_entries([map()], map(), [map()]) -> {ok, [map()]} | {error, term()}.
validate_alias_entries([], _Seen, Acc) ->
    {ok, lists:reverse(Acc)};
validate_alias_entries([E | Rest], Seen, Acc) ->
    case exact_keys(E, [canonical_id, canonical_word_sha256, variants]) of
        ok ->
            case {pin_field(E), list_of_maps(maps:get(<<"variants">>, E, undefined))} of
                {{ok, Id, Sha}, Variants} when Variants =/= [] ->
                    case validate_alias_variants(Id, Variants, Seen) of
                        {ok, NormVariants, Seen1} ->
                            validate_alias_entries(
                                Rest,
                                Seen1,
                                [#{canonical_id => Id, sha => Sha, variants => NormVariants} | Acc]
                            );
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, {bad_alias_entry, E}}
            end;
        {error, Reason} ->
            {error, {bad_alias_entry, {E, Reason}}}
    end.

-spec validate_alias_variants(binary(), [map()], map()) ->
    {ok, [map()], map()} | {error, term()}.
validate_alias_variants(CanonicalId, Variants, Seen) ->
    lists:foldl(
        fun
            (_V, {error, R}) ->
                {error, R};
            (V, {ok, Acc, SeenAcc}) ->
                validate_alias_variant(CanonicalId, V, Acc, SeenAcc)
        end,
        {ok, [], Seen},
        Variants
    ).

-spec validate_alias_variant(binary(), map(), [map()], map()) ->
    {ok, [map()], map()} | {error, term()}.
validate_alias_variant(CanonicalId, V, Acc, Seen) ->
    case exact_keys(V, [variant_id, type, pattern, action, boundary]) of
        ok ->
            VId = maps:get(<<"variant_id">>, V, undefined),
            Type = maps:get(<<"type">>, V, undefined),
            Pattern = maps:get(<<"pattern">>, V, undefined),
            Action = maps:get(<<"action">>, V, undefined),
            Boundary = maps:get(<<"boundary">>, V, undefined),
            case
                nonempty_bin(VId) andalso nonempty_bin(Type) andalso nonempty_bin(Pattern) andalso
                    lists:member(Action, ?ALLOWED_ACTIONS) andalso nonempty_bin(Boundary)
            of
                true ->
                    Key = {CanonicalId, VId},
                    case maps:is_key(Key, Seen) of
                        true ->
                            {error, {duplicate_variant_id, CanonicalId, VId}};
                        false ->
                            {ok, [normalize_json_keys(V) | Acc], Seen#{Key => true}}
                    end;
                false ->
                    {error, {bad_alias_variant, V}}
            end;
        {error, Reason} ->
            {error, {bad_alias_variant, {V, Reason}}}
    end.

-spec check_enh_gap(map(), map(), [map()]) -> {ok, enhancement()} | {error, term()}.
check_enh_gap(Enh, Semantics, Aliases) ->
    Entries = list_of_maps(maps:get(<<"gap">>, Enh, [])),
    case validate_gap_entries(Entries, []) of
        {ok, Gaps} ->
            check_enh_regex(Enh, Semantics, Aliases, Gaps);
        {error, Reason} ->
            {error, {invalid_enhancement, Reason}}
    end.

-spec validate_gap_entries([map()], [map()]) -> {ok, [map()]} | {error, term()}.
validate_gap_entries([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_gap_entries([E | Rest], Acc) ->
    case exact_keys(E, [canonical_id, canonical_word_sha256, enabled, action]) of
        ok ->
            Enabled = maps:get(<<"enabled">>, E, undefined),
            Action = maps:get(<<"action">>, E, undefined),
            case {pin_field(E), Enabled, lists:member(Action, ?ALLOWED_ACTIONS)} of
                {{ok, Id, Sha}, B, true} when is_boolean(B) ->
                    case [G || G <- Acc, maps:get(canonical_id, G) =:= Id] of
                        [] ->
                            validate_gap_entries(Rest, [
                                #{canonical_id => Id, sha => Sha, enabled => B, action => Action}
                                | Acc
                            ]);
                        _ ->
                            {error, {duplicate_gap, Id}}
                    end;
                _ ->
                    {error, {bad_gap_entry, E}}
            end;
        {error, Reason} ->
            {error, {bad_gap_entry, {E, Reason}}}
    end.

-spec check_enh_regex(map(), map(), [map()], [map()]) -> {ok, enhancement()} | {error, term()}.
check_enh_regex(Enh, Semantics, Aliases, Gaps) ->
    Entries = list_of_maps(maps:get(<<"regex_rules">>, Enh, [])),
    case validate_regex_entries(Entries, []) of
        {ok, Regex} ->
            check_enh_exceptions(Enh, Semantics, Aliases, Gaps, Regex);
        {error, Reason} ->
            {error, {invalid_enhancement, Reason}}
    end.

%% regex id 强制 `re:` 前缀（与数据库数字 id 命名空间分离，plan B.1）。
-spec validate_regex_entries([map()], [map()]) -> {ok, [map()]} | {error, term()}.
validate_regex_entries([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_regex_entries([E | Rest], Acc) ->
    case exact_keys(E, [id, expression, display_word, category, severity, action, scenes]) of
        ok ->
            Id = maps:get(<<"id">>, E, undefined),
            Fields = [
                maps:get(to_bin(K), E, undefined)
             || K <- [expression, display_word, category, severity]
            ],
            Action = maps:get(<<"action">>, E, undefined),
            case
                is_re_id(Id) andalso lists:all(fun nonempty_bin/1, Fields) andalso
                    lists:member(Action, ?ALLOWED_ACTIONS)
            of
                true ->
                    case validate_scenes(maps:get(<<"scenes">>, E, undefined)) of
                        ok ->
                            case [R || R <- Acc, maps:get(<<"id">>, R) =:= Id] of
                                [] ->
                                    validate_regex_entries(Rest, [normalize_json_keys(E) | Acc]);
                                _ ->
                                    {error, {duplicate_regex_id, Id}}
                            end;
                        {error, Reason} ->
                            {error, {bad_regex_scenes, {Id, Reason}}}
                    end;
                false ->
                    {error, {bad_regex_entry, E}}
            end;
        {error, Reason} ->
            {error, {bad_regex_entry, {E, Reason}}}
    end.

-spec is_re_id(term()) -> boolean().
is_re_id(<<"re:", _/binary>>) -> true;
is_re_id(_) -> false.

-spec check_enh_exceptions(map(), map(), [map()], [map()], [map()]) ->
    {ok, enhancement()} | {error, term()}.
check_enh_exceptions(Enh, Semantics, Aliases, Gaps, Regex) ->
    Entries = list_of_maps(maps:get(<<"exceptions">>, Enh, [])),
    case validate_exception_entries(Entries, []) of
        {ok, Exceptions} ->
            {ok, #{
                semantics => Semantics,
                aliases => Aliases,
                gap => Gaps,
                regex_rules => Regex,
                exceptions => Exceptions
            }};
        {error, Reason} ->
            {error, {invalid_enhancement, Reason}}
    end.

%% views 仅 B/T（plan B.1）；variant_ids 非空二进制列表；scenes 合法。
-spec validate_exception_entries([map()], [map()]) -> {ok, [map()]} | {error, term()}.
validate_exception_entries([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_exception_entries([E | Rest], Acc) ->
    case
        exact_keys(E, [id, canonical_id, canonical_word_sha256, phrase, scenes, variant_ids, views])
    of
        ok ->
            Id = maps:get(<<"id">>, E, undefined),
            Phrase = maps:get(<<"phrase">>, E, undefined),
            Views = maps:get(<<"views">>, E, undefined),
            VariantIds = maps:get(<<"variant_ids">>, E, undefined),
            case
                nonempty_bin(Id) andalso nonempty_bin(Phrase) andalso valid_views(Views) andalso
                    nonempty_bin_list_b(VariantIds)
            of
                true ->
                    case validate_scenes(maps:get(<<"scenes">>, E, undefined)) of
                        ok ->
                            case {pin_field(E), [X || X <- Acc, maps:get(id, X) =:= Id]} of
                                {{ok, CanonId, Sha}, []} ->
                                    validate_exception_entries(Rest, [
                                        #{
                                            id => Id,
                                            canonical_id => CanonId,
                                            sha => Sha,
                                            phrase => Phrase,
                                            scenes => maps:get(<<"scenes">>, E),
                                            variant_ids => VariantIds,
                                            views => Views
                                        }
                                        | Acc
                                    ]);
                                {{ok, _}, _} ->
                                    {error, {duplicate_exception_id, Id}};
                                _ ->
                                    {error, {bad_exception_pin, E}}
                            end;
                        {error, Reason} ->
                            {error, {bad_exception_scenes, {Id, Reason}}}
                    end;
                false ->
                    {error, {bad_exception_entry, E}}
            end;
        {error, Reason} ->
            {error, {bad_exception_entry, {E, Reason}}}
    end.

-spec valid_views(term()) -> boolean().
valid_views(Views) when is_list(Views), Views =/= [] ->
    lists:all(fun(V) -> lists:member(V, ?ALLOWED_VIEWS) end, Views) andalso
        length(lists:usort(Views)) =:= length(Views);
valid_views(_) ->
    false.

-spec validate_scenes(term()) -> ok | {error, term()}.
validate_scenes(Scenes) when is_list(Scenes), Scenes =/= [] ->
    case lists:all(fun(S) -> lists:member(S, ?SCENES) end, Scenes) of
        true ->
            case length(lists:usort(Scenes)) =:= length(Scenes) of
                true -> ok;
                false -> {error, {duplicate_scenes, Scenes}}
            end;
        false ->
            {error, {unknown_scenes, Scenes}}
    end;
validate_scenes(Other) ->
    {error, {bad_scenes, Other}}.

%% -------------------------------------------------------------------
%% 依附合并（悬空/失效剔除）与编码
%% -------------------------------------------------------------------

-spec merge_validated(map(), binary(), [canonical_word()], enhancement()) ->
    {ok, export_result()} | {error, term()}.
merge_validated(Opts, T2sId, Words, Enh) ->
    ShaById = maps:from_list([
        {integer_to_binary(Id), canonical_word_sha256(Word)}
     || #{id := Id, word := Word} <- Words
    ]),
    Aliases = maps:get(aliases, Enh, []),
    Gaps = maps:get(gap, Enh, []),
    Exceptions = maps:get(exceptions, Enh, []),
    Regex = maps:get(regex_rules, Enh, []),
    {LiteralMaps, Dropped1} = build_literals(Words, ShaById, Aliases, Gaps),
    {ExceptionMaps, Dropped2} = build_exceptions(Exceptions, ShaById),
    %% canonical 已整词消失的 alias/gap 依附：literal 折叠遍历不到，
    %% 显式对账成 dangling（词表存在但文本改写的 mismatch 已在折叠内处理）
    DanglingExtras = [
        #{kind => Kind, canonical_id => maps:get(canonical_id, E), reason => dangling_reference}
     || {Kind, Entries} <- [{alias, Aliases}, {gap, Gaps}],
        E <- Entries,
        not maps:is_key(maps:get(canonical_id, E), ShaById)
    ],
    case resolve_semantics(Opts, Enh) of
        {ok, Semantics} ->
            NormId = override_bin(Opts, normalization_id, ?DEFAULT_NORMALIZATION_ID),
            LimitId = override_bin(Opts, limit_profile_id, ?DEFAULT_LIMIT_PROFILE_ID),
            Pkg =
                #{
                    <<"schema_version">> => 1,
                    <<"normalization_id">> => NormId,
                    <<"t2s_table_id">> => T2sId,
                    <<"limit_profile_id">> => LimitId,
                    <<"semantics_by_scene">> => Semantics,
                    <<"allow_empty">> => false,
                    <<"literals">> => LiteralMaps,
                    <<"regex_rules">> => sort_by_id(Regex),
                    <<"exceptions">> => sort_by_id(ExceptionMaps)
                },
            Package = jsone:encode(Pkg, [native_utf8, canonical_form]),
            Core = lists:sort([
                {Word, Category, Severity}
             || #{word := Word, category := Category, severity := Severity} <- Words
            ]),
            Dropped = DanglingExtras ++ Dropped1 ++ Dropped2,
            Warnings =
                [dropped_warning(D) || D <- Dropped] ++
                    case Words of
                        [] ->
                            %% 空表导出是合法但可疑的状态：明确记录（Rust 侧
                            %% allow_empty 语义由 T19 闭合）
                            [<<"textguard_export: canonical word table is empty">>];
                        _ ->
                            []
                    end,
            lists:foreach(fun(W) -> ?LOG_WARNING("~ts", [W]) end, Warnings),
            {ok, #{
                package => Package,
                base_digest => textguard_base_digest:digest(Core),
                package_sha256 => hex_lower(crypto:hash(sha256, Package)),
                word_count => length(Words),
                dropped => Dropped,
                warnings => Warnings
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% 语义优先级：增强文件 > 配置 > 默认；最终形态必须三场景齐备且值合法。
-spec resolve_semantics(map(), enhancement()) -> {ok, map()} | {error, term()}.
resolve_semantics(Opts, Enh) ->
    ConfSem =
        case maps:get(semantics_by_scene, Opts, undefined) of
            M when is_map(M) -> maps:merge(?DEFAULT_SEMANTICS, normalize_json_keys(M));
            _ -> ?DEFAULT_SEMANTICS
        end,
    EnhSem = maps:get(semantics, Enh, #{}),
    Final = maps:merge(ConfSem, EnhSem),
    case validate_final_semantics(Final) of
        ok -> {ok, Final};
        {error, Reason} -> {error, {invalid_semantics, Reason}}
    end.

-spec validate_final_semantics(map()) -> ok | {error, term()}.
validate_final_semantics(Sem) ->
    Missing = [S || S <- ?SCENES, not is_valid_semantic(maps:get(S, Sem, undefined))],
    case Missing of
        [] ->
            ok;
        _ ->
            {error, {missing_scenes, Missing}}
    end.

-spec is_valid_semantic(term()) -> boolean().
is_valid_semantic(V) ->
    is_binary(V) andalso lists:member(V, ?ALLOWED_SEMANTICS).

-spec dropped_warning(map()) -> binary().
dropped_warning(#{kind := Kind, canonical_id := CanonId, reason := Reason}) ->
    iolist_to_binary([
        <<"textguard_export: dropped ">>,
        atom_to_binary(Kind, utf8),
        <<" extension for canonical ">>,
        CanonId,
        <<": ">>,
        atom_to_binary(Reason, utf8)
    ]).

-spec build_literals([canonical_word()], map(), [map()], [map()]) -> {[map()], [map()]}.
build_literals(Words, ShaById, Aliases, Gaps) ->
    AliasesById = group_by_canonical(Aliases),
    GapsById = group_by_canonical(Gaps),
    {Out, Drops} =
        lists:foldl(
            fun(
                #{id := Id, word := Word, category := Category, severity := Severity},
                {OutAcc, DropsAcc}
            ) ->
                IdBin = integer_to_binary(Id),
                {AliasVariants, Drops1} = dependent_entries(
                    AliasesById, IdBin, ShaById, alias, DropsAcc
                ),
                {GapEntries, Drops2} = dependent_entries(GapsById, IdBin, ShaById, gap, Drops1),
                AliasesOut = sort_by_variant_id(lists:append(AliasVariants)),
                Literal0 =
                    #{
                        <<"id">> => IdBin,
                        <<"word">> => Word,
                        <<"category">> => Category,
                        <<"severity">> => Severity,
                        <<"scenes">> => ?SCENES,
                        <<"aliases">> => AliasesOut
                    },
                Literal =
                    case [G || G <- GapEntries, maps:get(enabled, G) =:= true] of
                        [G] ->
                            Literal0#{
                                <<"gap">> => #{
                                    <<"enabled">> => true, <<"action">> => maps:get(action, G)
                                }
                            };
                        [] ->
                            Literal0#{
                                <<"gap">> => #{<<"enabled">> => false, <<"action">> => <<"review">>}
                            }
                    end,
                {[Literal | OutAcc], Drops2}
            end,
            {[], []},
            Words
        ),
    %% fold 前插建表：反转恢复 SQL 的数值 id 升序
    {lists:reverse(Out), Drops}.

%% 例外构造：rule_id = 所依附 canonical 的 id 字符串；悬空/失效剔除。
-spec build_exceptions([map()], map()) -> {[map()], [map()]}.
build_exceptions(Entries, ShaById) ->
    lists:foldl(
        fun(E, {Out, Drops}) ->
            #{
                id := Id,
                canonical_id := CanonId,
                sha := Sha,
                phrase := Phrase,
                scenes := Scenes,
                variant_ids := VariantIds,
                views := Views
            } = E,
            case maps:get(CanonId, ShaById, undefined) of
                Sha ->
                    Exception = #{
                        <<"id">> => Id,
                        <<"phrase">> => Phrase,
                        <<"scenes">> => Scenes,
                        <<"rule_id">> => CanonId,
                        <<"variant_ids">> => VariantIds,
                        <<"views">> => Views
                    },
                    {[Exception | Out], Drops};
                undefined ->
                    keep_and_drop(Out, Drops, #{
                        kind => exception,
                        canonical_id => CanonId,
                        reason => dangling_reference,
                        id => Id
                    });
                _OtherSha ->
                    keep_and_drop(Out, Drops, #{
                        kind => exception,
                        canonical_id => CanonId,
                        reason => sha256_mismatch,
                        id => Id
                    })
            end
        end,
        {[], []},
        Entries
    ).

-spec keep_and_drop([map()], [map()], map()) -> {[map()], [map()]}.
keep_and_drop(Out, Drops, Dropped) ->
    {Out, [Dropped | Drops]}.

%% 按增强项依附校验：canonical 存在且 pin 匹配才保留。
-spec dependent_entries(map(), binary(), map(), alias | gap, [map()]) -> {[map()], [map()]}.
dependent_entries(ById, IdBin, ShaById, Kind, DropAcc) ->
    case maps:get(IdBin, ById, []) of
        [] ->
            {[], DropAcc};
        Entries ->
            Actual = maps:get(IdBin, ShaById, undefined),
            lists:foldl(
                fun(E, {Out, Drops}) ->
                    case maps:get(sha, E) of
                        Actual ->
                            {[maps:get(variants, E, E) | Out], Drops};
                        undefined ->
                            keep_and_drop(Out, Drops, #{
                                kind => Kind, canonical_id => IdBin, reason => dangling_reference
                            });
                        _ ->
                            keep_and_drop(Out, Drops, #{
                                kind => Kind, canonical_id => IdBin, reason => sha256_mismatch
                            })
                    end
                end,
                {[], DropAcc},
                Entries
            )
    end.

-spec group_by_canonical([map()]) -> map().
group_by_canonical(Entries) ->
    lists:foldl(
        fun(E, Acc) ->
            Id = maps:get(canonical_id, E),
            maps:update_with(Id, fun(L) -> [E | L] end, [E], Acc)
        end,
        #{},
        Entries
    ).

-spec sort_by_id([map()]) -> [map()].
sort_by_id(Maps) ->
    lists:sort(fun(A, B) -> maps:get(<<"id">>, A) =< maps:get(<<"id">>, B) end, Maps).

-spec sort_by_variant_id([map()]) -> [map()].
sort_by_variant_id(Maps) ->
    lists:sort(
        fun(A, B) -> maps:get(<<"variant_id">>, A) =< maps:get(<<"variant_id">>, B) end, Maps
    ).

%% -------------------------------------------------------------------
%% 小工具
%% -------------------------------------------------------------------

%% 精确键集校验（键统一二进制形态比较）。
-spec exact_keys(map(), [atom() | binary()]) -> ok | {error, {missing | unknown, [term()]}}.
exact_keys(M, Allowed0) ->
    Allowed = [to_bin(K) || K <- Allowed0],
    Keys = [K || {K, _} <- maps:to_list(M)],
    Missing = [K || K <- Allowed, not lists:member(K, Keys)],
    Unknown = [K || K <- Keys, not lists:member(K, Allowed)],
    case {Missing, Unknown} of
        {[], []} -> ok;
        {[], U} -> {error, {unknown, U}};
        {Mi, _} -> {error, {missing, Mi}}
    end.

-spec extra_keys(map(), [atom() | binary()]) -> [term()].
extra_keys(M, Allowed0) ->
    Allowed = [to_bin(K) || K <- Allowed0],
    [K || {K, _} <- maps:to_list(M), not lists:member(K, Allowed)].

%% 键归一：atom 键（测试注入 map）转二进制，与 jsone 解码形态一致。
-spec normalize_json_keys(map()) -> map().
normalize_json_keys(M) when is_map(M) ->
    maps:from_list([{to_bin(K), V} || {K, V} <- maps:to_list(M)]).

-spec to_bin(atom() | binary()) -> binary().
to_bin(K) when is_binary(K) -> K;
to_bin(K) when is_atom(K) -> atom_to_binary(K, utf8).

%% 依附 pin 字段对：canonical_id + canonical_word_sha256（均非空二进制，
%%% sha 归一小写）。
-spec pin_field(map()) -> {ok, binary(), binary()} | error.
pin_field(E) ->
    case
        {
            maps:get(<<"canonical_id">>, E, undefined),
            maps:get(<<"canonical_word_sha256">>, E, undefined)
        }
    of
        {Id, Sha} when is_binary(Id), Id =/= <<>>, is_binary(Sha), Sha =/= <<>> ->
            {ok, Id, string:lowercase(Sha)};
        _ ->
            error
    end.

-spec nonempty_bin(term()) -> boolean().
nonempty_bin(B) when is_binary(B), B =/= <<>> -> true;
nonempty_bin(_) -> false.

-spec nonempty_bin_list(term()) -> {ok, [binary()]} | error.
nonempty_bin_list(L) when is_list(L), L =/= [] ->
    case lists:all(fun nonempty_bin/1, L) of
        true -> {ok, L};
        false -> error
    end;
nonempty_bin_list(_) ->
    error.

-spec nonempty_bin_list_b(term()) -> boolean().
nonempty_bin_list_b(L) ->
    case nonempty_bin_list(L) of
        {ok, _} -> true;
        error -> false
    end.

-spec list_of_maps(term()) -> [map()].
list_of_maps(L) when is_list(L) ->
    [E || E <- L, is_map(E)];
list_of_maps(_) ->
    [].

-spec override_bin(map(), atom(), binary()) -> binary().
override_bin(Opts, Key, Default) ->
    case maps:get(Key, Opts, undefined) of
        V when is_binary(V), V =/= <<>> -> V;
        _ -> Default
    end.

-spec hex_lower(binary()) -> binary().
hex_lower(Bin) when is_binary(Bin) ->
    <<<<(hex_digit(N bsr 4)), (hex_digit(N band 15))>> || <<N:8>> <= Bin>>.

-spec hex_digit(0..15) -> byte().
hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.
