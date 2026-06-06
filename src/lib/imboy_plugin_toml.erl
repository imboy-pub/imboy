-module(imboy_plugin_toml).

%% @status FROZEN (roadmap-only, 2026-06)：v2 动态加载子系统暂停投入。
%% 当前生产走配置驱动模块化单体路线（见 product-profile-and-plugin-registry-design.md §3.1）。
%% 修改前请确认是否真要重启动态平台方向。冻结≠移除。
%% FROZEN: v2 dynamic plugin loading subsystem is suspended (roadmap-only).
%% Current production route: config-driven monolith. See §3.1 before resuming.

%%%-------------------------------------------------------------------
%%% @doc
%%% 插件清单 TOML 解析与 schema 校验
%%% Plugin manifest TOML parsing and schema validation
%%%
%%% 切片 1（纯函数）/ Slice 1 (pure functions):
%%%   - parse_contract_version/1: 解析契约版本字符串 / parse contract version string
%%%   - validate_manifest/1: 校验已解析的 raw map 是否符合 §3.3
%%%
%%% 切片 2（副作用层）/ Slice 2 (side-effects layer):
%%%   - load/1: 从文件读取并解析插件清单
%%%
%%% Phase 0 实施决策：manifest 文件采用 Erlang terms 格式（file:consult/1），
%%% 约定文件名 plugin.config。理由：零依赖、与 imboy 现有 sys.config 生态一致、
%%% dialyzer 友好。模块名 imboy_plugin_toml 保留为历史名（Phase 1+ 视需要重命名
%%% 为 imboy_plugin_manifest，并在引入 tomerl 后增加 plugin.toml 加载能力）。
%%%
%%% Phase 0 implementation decision: manifest files use Erlang terms format
%%% (file:consult/1), conventional filename plugin.config. Rationale: zero
%%% dependency, aligns with imboy's existing sys.config ecosystem, dialyzer
%%% friendly. Module name imboy_plugin_toml is retained as historical (will
%%% be renamed to imboy_plugin_manifest in Phase 1+ when tomerl is added
%%% for plugin.toml support).
%%%
%%% Source of truth: docs/plugin/contract.md §3.3
%%% Contract version: 1.0
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-include("imboy_plugin.hrl").

-export([
    load/1,
    parse_contract_version/1,
    validate_manifest/1
]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc 从文件读取并解析插件清单 / Load and parse plugin manifest from file.
%% Phase 0：文件格式为 Erlang terms（含单一 map term，以点号 `.` 结尾）。
%% Phase 0: file format is Erlang terms (single map term terminated with `.`).
%% 流程：file:consult/1 → 形状校验 → validate_manifest/1。
%% Pipeline: file:consult/1 -> shape check -> validate_manifest/1.
-spec load(file:filename_all()) ->
    {ok, manifest()} | {error, term()}.
load(Path) ->
    %% 子句顺序敏感：单 map 优先 → 空 → 单非 map → 多 term → IO 错误
    %% Clause order matters: single map first, then empty, single non-map, multi, IO errors
    case file:consult(Path) of
        {ok, [Manifest]} when is_map(Manifest) ->
            validate_manifest(Manifest);
        {ok, []} ->
            {error, {load, empty_file}};
        {ok, [NotMap]} ->
            {error, {load, {not_a_map, NotMap}}};
        {ok, [_ | _]} ->
            {error, {load, multiple_terms}};
        {error, enoent} ->
            {error, {load, file_not_found}};
        {error, Reason} ->
            {error, {load, Reason}}
    end.

%% @doc 解析契约版本字符串 / Parse contract version string.
%% 形如 <<"1.0">> -> {1, 0}；非法输入返回 {error, invalid_format}。
%% 见 docs/plugin/contract.md §9.1 Loader compatibility matrix.
-spec parse_contract_version(binary()) ->
    contract_version() | {error, invalid_format}.
parse_contract_version(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<".">>) of
        [MajorBin, MinorBin] ->
            try
                Major = binary_to_integer(MajorBin),
                Minor = binary_to_integer(MinorBin),
                if
                    Major > 0 andalso Minor >= 0 ->
                        {Major, Minor};
                    true ->
                        {error, invalid_format}
                end
            catch
                error:badarg -> {error, invalid_format}
            end;
        _ ->
            {error, invalid_format}
    end;
parse_contract_version(_) ->
    {error, invalid_format}.

%% @doc 校验已解析的 raw manifest map 是否符合 §3.3 字段约束。
%% Validate a parsed raw manifest map against §3.3 field constraints.
%% 失败时返回 {error, {Field, Reason}}，loader 据此记录结构化错误并拒绝加载。
%% On failure returns {error, {Field, Reason}}; loader logs structured error and rejects.
-spec validate_manifest(map()) ->
    {ok, manifest()} | {error, {atom(), term()}}.
validate_manifest(M) when is_map(M) ->
    Validators = [
        fun validate_required_keys/1,
        fun validate_name/1,
        fun validate_version/1,
        fun validate_contract_version/1,
        fun validate_kind/1,
        fun validate_migrations/1,
        fun validate_routes/1,
        fun validate_degrade/1,
        fun validate_features_audience/1
    ],
    case run_validators(Validators, M) of
        ok -> {ok, M};
        {error, _} = E -> E
    end;
validate_manifest(_) ->
    {error, {manifest, not_a_map}}.

%% ===================================================================
%% Internal: validator pipeline
%% ===================================================================

run_validators([], _M) ->
    ok;
run_validators([V | Rest], M) ->
    case V(M) of
        ok -> run_validators(Rest, M);
        {error, _} = E -> E
    end.

%% --- required keys ----------------------------------------------
%% 校验所有 manifest() type spec 中标记为 := 的字段全部存在。
%% V2 评审 HIGH-2：原校验器仅覆盖 8/21 个字段，Phase 1 真实插件加载会
%% 在 maps:get(limits, M) 之类调用处 badkey 崩溃。此 validator 提前
%% 拦截，返回结构化 {error, {Field, missing}}。
%% Source of truth: include/imboy_plugin.hrl 的 manifest() type
validate_required_keys(M) ->
    Required = [
        name,
        version,
        contract_version,
        kind,
        description,
        depends_on,
        requires_capabilities,
        min_core_version,
        children,
        routes,
        migrations,
        features,
        limits,
        budget,
        degrade,
        circuit_breaker,
        entries,
        i18n,
        subscribes,
        publishes,
        publishes_meta,
        audit,
        meta
    ],
    case [K || K <- Required, not maps:is_key(K, M)] of
        [] -> ok;
        [Missing | _] -> {error, {Missing, missing}}
    end.

%% --- name --------------------------------------------------------
%% snake_case atom，^[a-z][a-z0-9_]+$，长度 3..32
validate_name(#{name := Name}) when is_atom(Name) ->
    Bin = atom_to_binary(Name, utf8),
    Size = byte_size(Bin),
    case Size of
        N when N >= 3, N =< 32 ->
            case re:run(Bin, "^[a-z][a-z0-9_]+$") of
                {match, _} -> ok;
                nomatch -> {error, {name, invalid_format}}
            end;
        _ ->
            {error, {name, invalid_length}}
    end;
validate_name(_) ->
    {error, {name, missing_or_not_atom}}.

%% --- version -----------------------------------------------------
%% binary semver 2.0：X.Y.Z 或 X.Y.Z-prerelease
validate_version(#{version := V}) when is_binary(V) ->
    case re:run(V, "^[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9.-]+)?$") of
        {match, _} -> ok;
        nomatch -> {error, {version, invalid_semver}}
    end;
validate_version(_) ->
    {error, {version, missing}}.

%% --- contract_version --------------------------------------------
%% {Major :: pos_integer(), Minor :: non_neg_integer()}
validate_contract_version(#{contract_version := {Major, Minor}}) when
    is_integer(Major), Major > 0, is_integer(Minor), Minor >= 0
->
    ok;
validate_contract_version(_) ->
    {error, {contract_version, invalid_format}}.

%% --- kind --------------------------------------------------------
validate_kind(#{kind := plugin}) -> ok;
validate_kind(#{kind := aggregate_plugin}) -> ok;
validate_kind(_) -> {error, {kind, invalid_value}}.

%% --- migrations.table_prefix == <name>_ --------------------------
validate_migrations(#{name := Name, migrations := #{table_prefix := P}}) when
    is_atom(Name), is_binary(P)
->
    Expected = <<(atom_to_binary(Name, utf8))/binary, "_">>,
    case P of
        Expected -> ok;
        _ -> {error, {migrations_table_prefix, mismatch}}
    end;
validate_migrations(_) ->
    {error, {migrations, missing_or_invalid}}.

%% --- routes[].path 必须以 /v{n}/<name>/ 开头 ---------------------
%% routes 为空列表或缺失皆通过；非空时每条 path 必须命中正则
validate_routes(#{name := Name, routes := Routes}) when is_list(Routes) ->
    NameBin = atom_to_binary(Name, utf8),
    Pattern = <<"^/v[0-9]+/", NameBin/binary, "/">>,
    AllOk = lists:all(
        fun
            (#{path := P}) when is_binary(P) ->
                re:run(P, Pattern) =/= nomatch;
            (_) ->
                false
        end,
        Routes
    ),
    case AllOk of
        true -> ok;
        false -> {error, {routes, invalid_namespace}}
    end;
validate_routes(_) ->
    %% 缺失 routes 字段或非 list 不在本切片校验，由后续 schema 完整性检查处理
    ok.

%% --- degrade.on_unhealthy ∈ enum --------------------------------
validate_degrade(#{degrade := #{on_unhealthy := V}}) when
    V =:= isolate; V =:= route_404; V =:= pass_through
->
    ok;
validate_degrade(#{degrade := _}) ->
    {error, {degrade_on_unhealthy, invalid_enum}};
validate_degrade(_) ->
    %% degrade 段缺失允许（Phase 0 留位）
    ok.

%% --- features.<key>.audience.buckets ----------------------------
%% 长度 ≤ 100；元素去重；元素 0..99
validate_features_audience(#{features := Features}) when is_map(Features) ->
    maps:fold(
        fun
            (_K, _FeatureSpec, {error, _} = E) ->
                E;
            (_K, FeatureSpec, ok) ->
                validate_audience(FeatureSpec)
        end,
        ok,
        Features
    );
validate_features_audience(_) ->
    ok.

validate_audience(#{audience := #{kind := uid_hash, buckets := Buckets}}) when
    is_list(Buckets)
->
    Len = length(Buckets),
    Unique = lists:usort(Buckets),
    InRange = lists:all(
        fun(B) -> is_integer(B) andalso B >= 0 andalso B =< 99 end,
        Buckets
    ),
    if
        Len > 100 -> {error, {audience_buckets, too_many}};
        length(Unique) =/= Len -> {error, {audience_buckets, duplicates}};
        not InRange -> {error, {audience_buckets, out_of_range}};
        true -> ok
    end;
validate_audience(_) ->
    ok.
