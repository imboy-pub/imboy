-module(imboy_plugin_registry).

-export([
    all/0,
    get/1,
    plugin_names/0,
    manifest/1,
    manifests/0,
    enabled_app_entries/1,
    enabled_admin_entries/1,
    feature_enabled/2,
    all_feature_keys/0,
    required_feature/3,
    required_feature_for_target/3,
    %% P0-T3.3 双轨过渡：v2 manifest 接口（从 persistent_term 读取，
    %% 由 imboy_plugin_loader 在启动期注入）。旧 manifest/1 与 manifests/0
    %% 保持不变以维持向后兼容。详见 .claude/plan/industrial-plugin-architecture-roadmap.md
    manifest_v2/1,
    manifests_v2/0
]).

%% Deprecated compatibility wrapper.
%% New callers should prefer manifests/0.
-spec all() -> map().
all() ->
    manifests().

%% Deprecated compatibility wrapper.
%% New callers should prefer manifest/1.
-spec get(atom()) -> map().
get(Name) ->
    manifest(Name).

-spec manifest(atom()) -> map().
manifest(Name) ->
    maps:get(Name, manifests(), #{}).

-spec manifests() -> map().
manifests() ->
    maps:map(
        fun(_PluginName, RawManifest) ->
            normalize_manifest(RawManifest)
        end,
        raw_manifests()
    ).

-spec plugin_names() -> [atom()].
plugin_names() ->
    RawManifests = raw_manifests(),
    DeclaredOrder = [channel, moment, location, group_collab],
    OrderedKnown = [
        PluginName
     || PluginName <- DeclaredOrder,
        maps:is_key(PluginName, RawManifests)
    ],
    Remaining = lists:sort([
        PluginName
     || PluginName <- maps:keys(RawManifests),
        not lists:member(PluginName, DeclaredOrder)
    ]),
    OrderedKnown ++ Remaining.

-spec enabled_app_entries(map()) -> [atom()].
enabled_app_entries(EnabledFeatures) ->
    enabled_entries(app_entries, EnabledFeatures).

-spec enabled_admin_entries(map()) -> [atom()].
enabled_admin_entries(EnabledFeatures) ->
    enabled_entries(admin_entries, EnabledFeatures).

-spec required_feature(api | admin, atom(), atom() | false) -> atom() | undefined.
required_feature(_Surface, _Handler, false) ->
    undefined;
required_feature(Surface, Handler, Action) ->
    RulesKey = surface_rules_key(Surface),
    case merged_handler_rules(maps:values(manifests()), RulesKey, Handler) of
        Rules when is_map(Rules) ->
            maps:get(Action, Rules, maps:get(default, Rules, undefined));
        undefined ->
            undefined
    end.

-spec required_feature_for_target(api | admin, atom(), binary()) -> atom() | undefined.
required_feature_for_target(_Surface, _Handler, <<>>) ->
    undefined;
required_feature_for_target(Surface, Handler, TargetType) when is_binary(TargetType) ->
    RulesKey = surface_target_rules_key(Surface),
    case merged_handler_rules(maps:values(manifests()), RulesKey, Handler) of
        Rules when is_map(Rules) ->
            maps:get(TargetType, Rules, undefined);
        undefined ->
            undefined
    end.

-spec raw_manifests() -> map().
raw_manifests() ->
    #{
        channel => #{
            kind => plugin,
            feature_keys => [channel, channel_discover, channel_invitation, channel_order],
            requires_capabilities => [],
            depends_on_plugins => [],
            api_feature_rules => #{
                channel_handler => #{
                    default => channel,
                    discover => channel_discover,
                    create_invitation => channel_invitation,
                    accept_invitation => channel_invitation,
                    reject_invitation => channel_invitation,
                    my_invitations => channel_invitation,
                    sent_invitations => channel_invitation,
                    create_order => channel_order,
                    pay_order => channel_order,
                    my_orders => channel_order,
                    get_order => channel_order
                }
            },
            api_target_feature_rules => #{
                report_handler => #{
                    <<"channel">> => channel
                }
            },
            admin_feature_rules => #{
                adm_channel_handler => #{
                    default => channel,
                    invitations => channel_invitation,
                    orders => channel_order
                }
            },
            admin_target_feature_rules => #{
                adm_report_handler => #{
                    <<"channel">> => channel
                }
            },
            app_entries => [channel_tab, channel_discover_page],
            admin_entries => [channels_page],
            api_handlers => [channel_handler, report_handler]
        },
        moment => #{
            kind => plugin,
            feature_keys => [moment],
            requires_capabilities => [],
            depends_on_plugins => [],
            api_feature_rules => #{
                moment_handler => #{
                    default => moment
                }
            },
            api_target_feature_rules => #{
                report_handler => #{
                    <<"moment">> => moment
                }
            },
            admin_feature_rules => #{
                adm_moment_handler => #{
                    default => moment
                }
            },
            admin_target_feature_rules => #{
                adm_report_handler => #{
                    <<"moment">> => moment
                }
            },
            app_entries => [moment_tab],
            admin_entries => [moments_page],
            api_handlers => [moment_handler, report_handler]
        },
        location => #{
            kind => plugin,
            feature_keys => [location],
            requires_capabilities => [],
            depends_on_plugins => [],
            api_feature_rules => #{
                location_handler => #{
                    default => location
                }
            },
            app_entries => [people_nearby_page],
            admin_entries => [],
            api_handlers => [location_handler]
        },
        group_collab => #{
            kind => aggregate_plugin,
            feature_keys => [group_vote, group_schedule, group_task],
            children => [vote, schedule, task],
            requires_capabilities => [],
            depends_on_plugins => [],
            api_feature_rules => #{
                group_vote_handler => #{
                    default => group_vote
                },
                group_schedule_handler => #{
                    default => group_schedule
                },
                group_task_handler => #{
                    default => group_task
                }
            },
            admin_feature_rules => #{
                adm_group_handler => #{
                    vote_list => group_vote,
                    vote_detail => group_vote,
                    vote_close => group_vote,
                    schedule_list => group_schedule,
                    schedule_detail => group_schedule,
                    schedule_cancel => group_schedule,
                    schedule_restore => group_schedule,
                    task_list => group_task,
                    task_detail => group_task,
                    task_pending_review => group_task,
                    task_review => group_task,
                    task_restore => group_task,
                    task_close => group_task,
                    task_delete => group_task
                }
            },
            app_entries => [group_vote_page, group_schedule_page, group_task_page],
            admin_entries => [
                group_vote_manage_page,
                group_schedule_manage_page,
                group_task_manage_page
            ],
            api_handlers => [group_vote_handler, group_schedule_handler, group_task_handler]
        }
    }.

%% ===================================================================
%% v2 manifest API (P0-T3.3 双轨过渡)
%% Reads from persistent_term written by imboy_plugin_loader at startup.
%% 旧 manifest/1 与 manifests/0 保持不变（向后兼容）；v2 接口提供 contract.md
%% v1.0 完整 schema（含 limits/budget/degrade/circuit_breaker/audit/i18n
%% 等工业级扩展位）。
%% ===================================================================

%% @doc 查询单个插件的 v2 manifest（来自 persistent_term）。
%% 未在 priv/plugins/<name>/plugin.config 中声明的插件返回 undefined。
-spec manifest_v2(atom()) -> map() | undefined.
manifest_v2(Name) when is_atom(Name) ->
    persistent_term:get({imboy_plugin_manifest, Name}, undefined).

%% @doc 列出全部已加载的 v2 manifest，键为 plugin name。
%% 仅 imboy_plugin_loader 已加载的插件出现在结果中；hardcoded 插件若无 .config
%% 文件则不会出现在 v2 结果中（这是双轨语义的预期行为）。
-spec manifests_v2() -> #{atom() => map()}.
manifests_v2() ->
    PluginNames = plugin_names(),
    lists:foldl(
        fun(Name, Acc) ->
            case manifest_v2(Name) of
                undefined -> Acc;
                M -> Acc#{Name => M}
            end
        end,
        #{},
        PluginNames
    ).

-spec normalize_manifest(map()) -> map().
normalize_manifest(RawManifest) ->
    Defaults = #{
        kind => plugin,
        feature_keys => [],
        requires_capabilities => [],
        depends_on_plugins => [],
        api_feature_rules => #{},
        admin_feature_rules => #{},
        api_target_feature_rules => #{},
        admin_target_feature_rules => #{},
        app_entries => [],
        admin_entries => [],
        api_handlers => []
    },
    maps:merge(Defaults, RawManifest).

-spec enabled_entries(app_entries | admin_entries, map()) -> [atom()].
enabled_entries(EntryKey, EnabledFeatures) ->
    lists:usort(
        lists:flatten(
            [
                maps:get(EntryKey, PluginManifest, [])
             || PluginName <- plugin_names(),
                PluginManifest <- [manifest(PluginName)],
                manifest_enabled(PluginManifest, EnabledFeatures)
            ]
        )
    ).

-spec manifest_enabled(map(), map()) -> boolean().
manifest_enabled(PluginManifest, EnabledFeatures) ->
    FeatureKeys = maps:get(feature_keys, PluginManifest, []),
    lists:any(
        fun(FeatureKey) ->
            feature_enabled(FeatureKey, EnabledFeatures)
        end,
        FeatureKeys
    ).

-spec feature_enabled(atom() | binary(), map()) -> boolean().
feature_enabled(FeatureKey, EnabledFeatures) when is_atom(FeatureKey) ->
    feature_enabled(atom_to_binary(FeatureKey, utf8), EnabledFeatures);
feature_enabled(FeatureKey, EnabledFeatures) ->
    case maps:find(FeatureKey, EnabledFeatures) of
        {ok, true} ->
            true;
        {ok, #{enabled := true}} ->
            true;
        {ok, #{<<"enabled">> := true}} ->
            true;
        _ ->
            false
    end.

%% @doc 从所有插件 manifest 的 feature_keys 派生全量特性键列表（去重，原子类型）。
%% 供 imboy_feature:feature_names/0 作为单一数据源使用（Task B1）。
-spec all_feature_keys() -> [atom()].
all_feature_keys() ->
    AllKeys = lists:flatmap(
        fun(Name) ->
            maps:get(feature_keys, manifest(Name), [])
        end,
        plugin_names()
    ),
    lists:usort(AllKeys).

-spec surface_rules_key(api | admin) -> api_feature_rules | admin_feature_rules.
surface_rules_key(api) ->
    api_feature_rules;
surface_rules_key(admin) ->
    admin_feature_rules.

-spec surface_target_rules_key(api | admin) ->
    api_target_feature_rules | admin_target_feature_rules.
surface_target_rules_key(api) ->
    api_target_feature_rules;
surface_target_rules_key(admin) ->
    admin_target_feature_rules.

-spec merged_handler_rules(
    [map()],
    api_feature_rules | admin_feature_rules | api_target_feature_rules | admin_target_feature_rules,
    atom()
) -> map() | undefined.
merged_handler_rules(Manifests, RulesKey, Handler) ->
    Rules = lists:foldl(
        fun(PluginManifest, Acc) ->
            RulesByHandler = maps:get(RulesKey, PluginManifest, #{}),
            case maps:find(Handler, RulesByHandler) of
                {ok, HandlerRules} when is_map(HandlerRules) ->
                    maps:merge(Acc, HandlerRules);
                _ ->
                    Acc
            end
        end,
        #{},
        Manifests
    ),
    case map_size(Rules) of
        0 -> undefined;
        _ -> Rules
    end.
