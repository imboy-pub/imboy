-module(imboy_feature).

-export([
    enabled/1,
    ensure_enabled/2,
    all/0,
    feature_names/0,
    compiled/1,
    compiled_features/0,
    manifest_hash/0,
    manifest_schema_version/0,
    compiled_routes/2,
    compiled_routes/3,
    route_feature/3
]).

-include("error_code.hrl").
-include("generated/imboy_product_features.hrl").

-type feature() :: atom() | binary() | string().

-spec enabled(feature()) -> boolean().
enabled(Feature) ->
    case normalize_feature_key(Feature) of
        undefined ->
            true;
        FeatureKey ->
            compiled(FeatureKey) andalso
                maps:get(FeatureKey, imboy_policy:effective_features(), true)
    end.

-spec compiled(feature()) -> boolean().
compiled(Feature) when is_atom(Feature) ->
    lists:member(Feature, ?IMBOY_COMPILED_FEATURES);
compiled(Feature) when is_binary(Feature) ->
    lists:member(Feature, [atom_to_binary(Name, utf8) || Name <- ?IMBOY_COMPILED_FEATURES]);
compiled(Feature) when is_list(Feature) ->
    compiled(unicode:characters_to_binary(Feature)).

-spec compiled_features() -> [atom()].
compiled_features() ->
    ?IMBOY_COMPILED_FEATURES.

-spec manifest_hash() -> binary().
manifest_hash() ->
    ?IMBOY_PRODUCT_FEATURE_MANIFEST_HASH.

-spec manifest_schema_version() -> pos_integer().
manifest_schema_version() ->
    ?IMBOY_PRODUCT_FEATURE_SCHEMA_VERSION.

-spec compiled_routes(api | admin, list()) -> list().
compiled_routes(Surface, Routes) ->
    compiled_routes(Surface, Routes, compiled_features()).

-spec compiled_routes(api | admin, list(), [atom()]) -> list().
compiled_routes(Surface, Routes, CompiledFeatures) ->
    [
        annotate_route(Surface, Route)
     || Route <- Routes, route_is_compiled(Surface, Route, CompiledFeatures)
    ].

route_is_compiled(Surface, {_Path, Handler, Opts}, CompiledFeatures) when is_map(Opts) ->
    Action = maps:get(action, Opts, false),
    case maps:get(required_feature, Opts, route_feature(Surface, Handler, Action)) of
        undefined -> true;
        Feature -> lists:member(Feature, CompiledFeatures)
    end;
route_is_compiled(_Surface, _Route, _CompiledFeatures) ->
    true.

annotate_route(Surface, {Path, Handler, Opts}) when is_map(Opts) ->
    Action = maps:get(action, Opts, false),
    case maps:get(required_feature, Opts, route_feature(Surface, Handler, Action)) of
        undefined -> {Path, Handler, Opts};
        Feature -> {Path, Handler, Opts#{required_feature => Feature}}
    end;
annotate_route(_Surface, Route) ->
    Route.

-spec route_feature(api | admin, atom(), atom() | false) -> atom() | undefined.
route_feature(api, Handler, _Action) when
    Handler =:= e2ee_handler;
    Handler =:= e2ee_backup_handler;
    Handler =:= e2ee_trust_handler;
    Handler =:= olm_handler
->
    e2ee;
route_feature(api, group_handler, set_e2ee_mode) ->
    e2ee;
route_feature(admin, adm_admin_handler, Action) when
    Action =:= compliance_key_list;
    Action =:= compliance_key_create;
    Action =:= compliance_key_revoke
->
    e2ee;
route_feature(api, channel_discovery_handler, _Action) ->
    channel_discover;
route_feature(api, channel_handler_order, _Action) ->
    channel_order;
route_feature(api, channel_handler_admin, Action) when
    Action =:= create_invitation;
    Action =:= accept_invitation;
    Action =:= reject_invitation;
    Action =:= my_invitations;
    Action =:= sent_invitations
->
    channel_invitation;
route_feature(api, Handler, _Action) when
    Handler =:= channel_handler_admin;
    Handler =:= channel_handler_comment;
    Handler =:= channel_handler_message;
    Handler =:= channel_webhook_handler
->
    channel;
route_feature(api, report_handler, moment_create) ->
    moment;
route_feature(admin, adm_group_vote_handler, _Action) ->
    group_vote;
route_feature(admin, adm_group_schedule_handler, Action) when Action =/= governance_log_list ->
    group_schedule;
route_feature(admin, adm_group_task_handler, _Action) ->
    group_task;
route_feature(admin, adm_channel_handler, Action) when
    Action =:= orders;
    Action =:= refund_order;
    Action =:= set_price
->
    channel_order;
%% EB-10：企业业务两张面（租户 /api/v1/enterprise/*、平台
%% /api/adm/enterprise-business/*）的**运行时** feature 门。
%% 路由段的编译期物理裁剪在 src/imboy_router.erl
%% （-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS)），本处供 compiled_routes/2
%% 过滤，两者是双保险而不是替代。
route_feature(api, eb_tenant_handler, _Action) ->
    enterprise_business;
route_feature(admin, eb_platform_handler, _Action) ->
    enterprise_business;
%% CS-02：客服两张面（租户 /api/v1/cs/*、平台 /api/adm/customer-service/*）的
%% **运行时** feature 门。路由段的编译期物理裁剪在 src/imboy_router.erl
%% （-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE)），本处供 compiled_routes/2 过滤，
%% 两者是双保险而不是替代。
route_feature(api, cs_tenant_handler, _Action) ->
    customer_service;
route_feature(admin, cs_platform_handler, _Action) ->
    customer_service;
route_feature(admin, adm_report_handler, Action) when
    Action =:= channel_list;
    Action =:= channel_resolve;
    Action =:= channel_batch_resolve
->
    channel;
route_feature(Surface, Handler, Action) ->
    imboy_plugin_registry:required_feature(Surface, Handler, Action).

-spec all() -> map().
all() ->
    maps:from_list([
        {atom_to_binary(Name, utf8), enabled(Name)}
     || Name <- feature_names()
    ]).

-spec ensure_enabled(cowboy_req:req(), feature()) -> ok | {error, cowboy_req:req()}.
ensure_enabled(Req, Feature) ->
    case enabled(Feature) of
        true ->
            ok;
        false ->
            {error,
                elib_response:error(
                    Req,
                    imboy_error:error_msg(?ERR_FEATURE_DISABLED),
                    ?ERR_FEATURE_DISABLED
                )}
    end.

%% @doc 全量特性键列表，以 registry 插件 feature_keys 为单一数据源，
%% 追加 core/e2ee（非插件内置特性）。顺序固定以保证 manifest etag 稳定。
-spec feature_names() -> [atom()].
feature_names() ->
    CoreFixed = [core, e2ee],
    PluginKeys = imboy_plugin_registry:all_feature_keys(),
    %% 保持历史顺序：channel/location/moment/channel_*/group_*
    Ordered = [
        K
     || K <- [
            channel,
            location,
            moment,
            channel_discover,
            channel_invitation,
            channel_order,
            group_vote,
            group_schedule,
            group_task
        ],
        lists:member(K, PluginKeys)
    ],
    Extra = [K || K <- PluginKeys, not lists:member(K, Ordered)],
    %% 平台内建（非插件）特性：
    %% - bot_webhook：Bot webhook 外呼（L-01 overseas_baseline 预设默认
    %%   关闭；community/enterprise 无显式覆盖时保持开放）。
    %% - appeal：处置申诉链（R-04 operational baseline，默认开放；
    %%   是否对用户开放由 profile/runtime 配置决定，法务结论后可关闭）。
    %% - enterprise_business / customer_service：企业业务与客服（plan v4.1
    %%   §5；EB-10 接入现成 feature 机制，物理裁剪见 FEATURE_BACKEND_MODULES
    %%   与 imboy_router 的 -ifdef 段）。两者**是 Feature 而非 Plugin**：
    %%   enterprise_business 是独立纵切单元 src/features/enterprise_business/，
    %%   不注册为插件 feature_keys；依赖边 customer_service ->
    %%   enterprise_business 声明在 imboy_policy_catalog:dependencies/1。
    Builtin = [bot_webhook, appeal, enterprise_business, customer_service],
    CoreFixed ++ Ordered ++ Extra ++ Builtin.

-spec normalize_feature_key(feature()) -> atom() | undefined.
normalize_feature_key(Feature) when is_atom(Feature) ->
    case lists:member(Feature, feature_names()) of
        true ->
            Feature;
        false ->
            undefined
    end;
normalize_feature_key(Feature) when is_binary(Feature) ->
    maybe_existing_feature_atom(Feature);
normalize_feature_key(Feature) when is_list(Feature) ->
    maybe_existing_feature_atom(unicode:characters_to_binary(Feature)).

-spec maybe_existing_feature_atom(binary()) -> atom() | undefined.
maybe_existing_feature_atom(Feature) ->
    try
        FeatureKey = binary_to_existing_atom(Feature, utf8),
        normalize_feature_key(FeatureKey)
    catch
        error:badarg ->
            undefined
    end.
