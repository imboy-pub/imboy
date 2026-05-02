-module(imboy_plugin_priv_plugins_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% P0-T4 端到端测试：从生产 priv/plugins/ 目录加载现有 4 个插件
%%% End-to-end test: load 4 existing plugins from production priv/plugins/
%%%
%%% 等价性核查 / Equivalence check:
%%%   plugin.config 加载结果与 imboy_plugin_registry:manifest/1（旧 hardcoded map）
%%%   在关键字段上一致：kind / entries.app / entries.admin / children / features keys
%%%
%%% Source of truth:
%%%   - priv/plugins/<name>/plugin.config (新)
%%%   - src/lib/imboy_plugin_registry.erl#raw_manifests/0 (旧)
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% Test helpers
%% ===================================================================

priv_plugins_dir() ->
    %% eunit cwd = imboy/，相对路径解析到 imboy/priv/plugins
    filename:absname("priv/plugins").

plugin_names() ->
    [channel, moment, location, group_collab].

setup() ->
    %% 清理可能残留的 persistent_term
    erase_all_pt(),
    {ok, Pid} = imboy_plugin_loader:start_link(priv_plugins_dir()),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end,
    erase_all_pt().

erase_all_pt() ->
    lists:foreach(
        fun(N) -> _ = persistent_term:erase({imboy_plugin_manifest, N}) end,
        plugin_names()
    ).

%% ===================================================================
%% 1. 全 4 个插件加载成功，无失败
%% ===================================================================

all_four_plugins_loaded_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [
             ?_assertEqual(
                 lists:sort(plugin_names()),
                 lists:sort(imboy_plugin_loader:list_plugins())
             ),
             ?_assertEqual([], imboy_plugin_loader:list_failed())
         ]
     end}.

%% ===================================================================
%% 2. 每个 manifest name 与 atom key 一致
%% ===================================================================

each_manifest_name_matches_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [
             ?_assertMatch(#{name := channel},
                           imboy_plugin_loader:get_manifest(channel)),
             ?_assertMatch(#{name := moment},
                           imboy_plugin_loader:get_manifest(moment)),
             ?_assertMatch(#{name := location},
                           imboy_plugin_loader:get_manifest(location)),
             ?_assertMatch(#{name := group_collab},
                           imboy_plugin_loader:get_manifest(group_collab))
         ]
     end}.

%% ===================================================================
%% 3. 等价性 — kind 字段
%% ===================================================================

equivalence_kind_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         lists:map(
             fun(Name) ->
                 OldKind = maps:get(kind, imboy_plugin_registry:manifest(Name)),
                 NewKind = maps:get(kind, imboy_plugin_loader:get_manifest(Name)),
                 ?_assertEqual(OldKind, NewKind)
             end,
             plugin_names()
         )
     end}.

%% ===================================================================
%% 4. 等价性 — entries.app vs 旧 app_entries
%% ===================================================================

equivalence_entries_app_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         lists:map(
             fun(Name) ->
                 OldApp = maps:get(app_entries, imboy_plugin_registry:manifest(Name)),
                 NewApp = maps:get(app,
                                   maps:get(entries, imboy_plugin_loader:get_manifest(Name))),
                 ?_assertEqual(lists:sort(OldApp), lists:sort(NewApp))
             end,
             plugin_names()
         )
     end}.

%% ===================================================================
%% 5. 等价性 — entries.admin vs 旧 admin_entries
%% ===================================================================

equivalence_entries_admin_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         lists:map(
             fun(Name) ->
                 OldAdmin = maps:get(admin_entries, imboy_plugin_registry:manifest(Name)),
                 NewAdmin = maps:get(admin,
                                     maps:get(entries, imboy_plugin_loader:get_manifest(Name))),
                 ?_assertEqual(lists:sort(OldAdmin), lists:sort(NewAdmin))
             end,
             plugin_names()
         )
     end}.

%% ===================================================================
%% 6. 等价性 — group_collab.children
%% ===================================================================

group_collab_children_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         OldChildren = maps:get(children, imboy_plugin_registry:manifest(group_collab)),
         NewChildren = maps:get(children,
                                imboy_plugin_loader:get_manifest(group_collab)),
         ?_assertEqual(lists:sort(OldChildren), lists:sort(NewChildren))
     end}.

%% ===================================================================
%% 7. 等价性 — features keys 集合 vs 旧 feature_keys
%% ===================================================================

features_cover_legacy_feature_keys_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         lists:map(
             fun(Name) ->
                 OldKeys = maps:get(feature_keys,
                                    imboy_plugin_registry:manifest(Name), []),
                 NewFeatures = maps:get(features,
                                        imboy_plugin_loader:get_manifest(Name)),
                 NewKeys = lists:sort(maps:keys(NewFeatures)),
                 ?_assertEqual(lists:sort(OldKeys), NewKeys)
             end,
             plugin_names()
         )
     end}.

%% ===================================================================
%% 8a. P0-T3.3 双轨注入：imboy_plugin_registry:manifest_v2/1 可读取
%%     loader 注入的 v2 manifest（与 imboy_plugin_loader:get_manifest/1 等价）
%% ===================================================================

registry_manifest_v2_returns_loaded_manifests_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         lists:map(
             fun(Name) ->
                 ViaLoader = imboy_plugin_loader:get_manifest(Name),
                 ViaRegistry = imboy_plugin_registry:manifest_v2(Name),
                 ?_assertEqual(ViaLoader, ViaRegistry)
             end,
             plugin_names()
         )
     end}.

registry_manifests_v2_returns_all_loaded_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         AllV2 = imboy_plugin_registry:manifests_v2(),
         [
             ?_assertEqual(4, map_size(AllV2)),
             ?_assert(maps:is_key(channel, AllV2)),
             ?_assert(maps:is_key(moment, AllV2)),
             ?_assert(maps:is_key(location, AllV2)),
             ?_assert(maps:is_key(group_collab, AllV2))
         ]
     end}.

registry_manifest_v2_returns_undefined_for_unknown_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_assertEqual(undefined,
                       imboy_plugin_registry:manifest_v2(nonexistent_plugin))
     end}.

%% ===================================================================
%% 8b. 旧 manifest/1 行为不受 v2 影响（向后兼容）
%% ===================================================================

registry_legacy_manifest_unchanged_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% 旧 manifest/1 仍返回 hardcoded 字段（feature_keys / app_entries 等）
         lists:flatmap(
             fun(Name) ->
                 OldM = imboy_plugin_registry:manifest(Name),
                 [
                     ?_assert(maps:is_key(feature_keys, OldM)),
                     ?_assert(maps:is_key(app_entries, OldM))
                 ]
             end,
             plugin_names()
         )
     end}.

%% ===================================================================
%% 9. 工业级扩展位字段全部存在（v1.0 契约要求）
%% ===================================================================

industrial_fields_present_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         RequiredKeys = [
             contract_version, version, limits, budget, degrade,
             circuit_breaker, audit, i18n, publishes_meta
         ],
         lists:flatmap(
             fun(Name) ->
                 M = imboy_plugin_loader:get_manifest(Name),
                 [
                     ?_test(begin
                         %% 失败时打印插件名 + 字段名以便定位 / Print Name+Key on failure
                         ?assertEqual({Name, K, true}, {Name, K, maps:is_key(K, M)})
                     end)
                  || K <- RequiredKeys
                 ]
             end,
             plugin_names()
         )
     end}.
