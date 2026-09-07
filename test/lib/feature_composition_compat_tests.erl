-module(feature_composition_compat_tests).
%% F-08：feature composition 迁移与存量数据兼容契约。
%%
%% 验收条款：禁用代码不静默删除数据；隐私/保留义务不随功能禁用而停；
%% 操作者可从同一 manifest 复现构建（F-09 的可测面）。
%%
%% 四条契约（均为静态/语义级，不依赖真 PG 端到端矩阵——full→reduced→full
%% 的三仓产物级验证由 scripts/run_product_feature_matrix.sh +
%% verify_product_feature_artifacts.py 承担，证据归档
%% docs/compliance/feature-composition-evidence/）：
%%   1. schema 超集：priv/migrations 单一目录，无 per-profile fork——
%%      所有 profile 共享同一迁移序列，禁用 feature 只关代码路径，
%%      库表与既有数据原样保留；
%%   2. manifest 契约：tracked 的 profile manifest selected_features
%%      必须落在 imboy_feature:feature_names() 全集内（无未知键、
%%      无 Base 裁剪）——生成器 validate 同款约束的运行时镜像；
%%   3. 运行时门语义：enabled/1 = 编译期物理存在 ∧ 运行时 effective_features
%%      开关；运行时关→关，与编译态无关（禁用是"不可见"不是"删数据"）；
%%   4. 保留义务恒跑：credential_retention_worker（T-02）挂 imboy_sup，
%%      不经任何 feature 门——隐私/保留清理义务不随 profile/禁用而停。

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% app 根定位：ebin 父目录即 app 根（make eunit-local 的 -pa 布局保证，
%% code:which 对 .eunit/beam 返回 <app>/.eunit/<mod>.beam）。
app_root() ->
    Ebin = code:which(?MODULE),
    filename:dirname(filename:dirname(Ebin)).

%% ===================================================================
%% 1. schema 超集：无 per-profile 迁移 fork
%% ===================================================================

schema_is_superset_no_profile_forks_test_() ->
    MigDir = filename:join(app_root(), "priv/migrations"),
    Entries = filelib:wildcard(filename:join(MigDir, "*")),
    Dirs = [E || E <- Entries, filelib:is_dir(E)],
    Sqls = [E || E <- Entries, filename:extension(E) =:= ".sql"],
    [
        ?_assertMatch([], Dirs),
        ?_assert(length(Sqls) > 0)
    ].

%% ===================================================================
%% 2. manifest 契约：tracked manifest 的 selected_features ⊆ 全集
%% ===================================================================

tracked_manifests_within_catalog_test_() ->
    ManifestDir = filename:join(app_root(), "config/product-feature-manifests"),
    Files = filelib:wildcard(filename:join(ManifestDir, "*.json")),
    Catalog = imboy_feature:feature_names(),
    [
        {atom_to_binary(F, utf8), fun() -> check_manifest(F, File, Catalog) end}
     || File <- Files, F <- [list_to_atom(filename:basename(File, ".json"))]
    ].

check_manifest(Profile, File, Catalog) ->
    {ok, Bin} = file:read_file(File),
    M = jsone:decode(Bin),
    ?assertEqual(1, maps:get(<<"schema_version">>, M)),
    ?assertEqual(<<"imboy">>, maps:get(<<"product_id">>, M)),
    ?assertEqual(atom_to_binary(Profile, utf8), maps:get(<<"profile">>, M)),
    ?assertEqual(<<"imboy-feature-inventory-v1">>, maps:get(<<"base_ref">>, M)),
    %% Base 不可裁剪（generator validate 同款：disabled_base_features 必空）
    ?assertEqual([], maps:get(<<"disabled_base_features">>, M, [])),
    Selected = maps:get(<<"selected_features">>, M),
    ?assert(is_list(Selected) andalso Selected =/= []),
    Unknown =
        [
            K
         || K <- Selected,
            not lists:member(binary_to_atom(K, utf8), Catalog)
        ],
    ?assertEqual([], Unknown).

%% ===================================================================
%% 3. 运行时门语义：运行时关→关（与编译态正交）
%% ===================================================================

runtime_gate_disabled_means_off_test_() ->
    ?WITH_MECK(
        imboy_policy,
        [
            {'effective_features', 0, fun() ->
                #{
                    channel => false,
                    moment => false,
                    e2ee => false,
                    bot_webhook => false,
                    location => false
                }
            end}
        ],
        fun() ->
            ?assertEqual(false, imboy_feature:enabled(channel)),
            ?assertEqual(false, imboy_feature:enabled(moment)),
            %% e2ee 是 Base 固定 feature（CoreFixed）——运行时门同样可关其
            %% 可选面，但编译期恒在（compiled(e2ee) 恒 true，不随 manifest 裁剪）
            ?assert(imboy_feature:compiled(e2ee)),
            ?assert(imboy_feature:compiled(core)),
            ?assertEqual(false, imboy_feature:enabled(e2ee)),
            ?assertEqual(false, imboy_feature:enabled(bot_webhook)),
            ?assertEqual(false, imboy_feature:enabled(location))
        end
    ).

%% ===================================================================
%% 4. 保留义务恒跑：retention worker 不经 feature 门
%% ===================================================================

retention_duties_survive_feature_disable_test_() ->
    %% init([]) 的副作用（pooler 建池）mock 掉：只取 child spec 结构
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun
                    (pg_conf) ->
                        #{
                            start_mfa =>
                                {imboy_pg_connection, connect, [#{host => <<"localhost">>}]}
                        };
                    (_Key) ->
                        undefined
                end}
            ]},
            {pooler, [
                {'new_pool', 1, fun(_Conf) -> ok end}
            ]}
        ],
        fun() ->
            {ok, {_, Children}} = imboy_sup:init([]),
            %% child spec 兼容 proplist 与 OTP 新式 map 两种形态
            ChildId = fun
                (C) when is_map(C) -> maps:get(id, C, undefined);
                (C) when is_list(C) -> proplists:get_value(id, C, undefined);
                (_) -> undefined
            end,
            Ids = [ChildId(C) || C <- Children],
            [
                %% T-02 隐私清理 worker 恒挂 imboy_sup——启动不查任何 feature 门
                ?assert(lists:member(credential_retention_worker, Ids)),
                %% retention 不是 feature key：无任何 profile/manifest 可禁用它
                ?assert(
                    not lists:member(credential_retention, imboy_feature:feature_names())
                ),
                %% 进程级结构自检：children 非空（sup 装配真实执行）
                ?assert(length(Ids) > 0)
            ]
        end
    ).
