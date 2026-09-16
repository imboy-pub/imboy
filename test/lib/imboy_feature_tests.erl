-module(imboy_feature_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

enabled_defaults_true_when_feature_missing_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'effective_features', 0, fun() -> #{} end}
            ]}
        ],
        fun() ->
            ?assertEqual(true, imboy_feature:enabled(moment))
        end
    ).

enabled_reads_explicit_feature_flag_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'effective_features', 0, fun() -> #{moment => false} end}
            ]}
        ],
        fun() ->
            ?assertEqual(false, imboy_feature:enabled(moment))
        end
    ).

enabled_unknown_feature_defaults_true_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'effective_features', 0, fun() -> #{moment => false} end}
            ]}
        ],
        fun() ->
            ?assertEqual(true, imboy_feature:enabled(<<"unknown_feature">>))
        end
    ).

ensure_enabled_returns_ok_when_flag_on_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'effective_features', 0, fun() -> #{moment => true} end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, imboy_feature:ensure_enabled(#{req => 1}, moment))
        end
    ).

ensure_enabled_returns_uniform_error_when_flag_off_test_() ->
    Req = #{req => 1},
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'effective_features', 0, fun() -> #{moment => false} end}
            ]},
            {imboy_error, [
                {'error_msg', 1, fun(?ERR_FEATURE_DISABLED) -> <<"feature disabled">> end}
            ]},
            {elib_response, [
                {'error', 3, fun(Req0, Msg, Code) -> {error_resp, Req0, Msg, Code} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {error_resp, Req, <<"feature disabled">>, ?ERR_FEATURE_DISABLED}},
                imboy_feature:ensure_enabled(Req, moment)
            )
        end
    ).

all_returns_binary_key_view_for_known_features_test_() ->
    %% EB-10：企业业务 / 客服也是内建键。它们的 enabled 值经
    %% `compiled/1 andalso effective_features` 两级判定，其中 compiled 来自
    %% **编译期**宏集合（当前 manifest 选 enterprise_business、未选
    %% customer_service），故按同一真源取值，避免把测试绑死在某个 preset 上。
    EnterpriseCompiled = lists:member(enterprise_business, imboy_feature:compiled_features()),
    CustomerServiceCompiled =
        lists:member(customer_service, imboy_feature:compiled_features()),
    FeatureMap = #{
        core => true,
        e2ee => false,
        channel => true,
        location => false,
        moment => true,
        channel_discover => false,
        channel_invitation => true,
        channel_order => false,
        group_vote => true,
        group_schedule => false,
        group_task => true,
        %% 平台内建键（Builtin）：L-01 bot_webhook / R-04 appeal /
        %% EB-10 enterprise_business / customer_service
        bot_webhook => true,
        appeal => false,
        enterprise_business => EnterpriseCompiled,
        customer_service => CustomerServiceCompiled
    },
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'effective_features', 0, fun() -> FeatureMap end}
            ]}
        ],
        fun() ->
            Payload = imboy_feature:all(),
            lists:foreach(
                fun(Name) ->
                    BinKey = atom_to_binary(Name, utf8),
                    ?assertEqual(maps:get(Name, FeatureMap), maps:get(BinKey, Payload))
                end,
                imboy_feature:feature_names()
            )
        end
    ).

feature_names_contract_test() ->
    ?assertEqual(
        [
            core,
            e2ee,
            channel,
            location,
            moment,
            channel_discover,
            channel_invitation,
            channel_order,
            group_vote,
            group_schedule,
            group_task,
            %% 平台内建（Builtin）键排在插件键之后
            %% （EB-10 追加 enterprise_business / customer_service，与
            %%   src/lib/imboy_feature.erl 的 Builtin 字面量同序）
            bot_webhook,
            appeal,
            enterprise_business,
            customer_service
        ],
        imboy_feature:feature_names()
    ).

%% --- EB-10: 企业业务依赖边 + 裁剪门 ---

customer_service_depends_on_enterprise_business_test() ->
    ?assertEqual(
        [enterprise_business],
        imboy_policy_catalog:dependencies(customer_service)
    ).

enterprise_routes_gated_by_feature_test() ->
    %% 路由门：企业两张面的 Handler 必须映射到 enterprise_business，
    %% 否则 compiled_routes/2 会漏掉企业路由（运行时门失效）。
    ?assertEqual(
        enterprise_business,
        imboy_feature:route_feature(api, eb_tenant_handler, business_identities)
    ),
    ?assertEqual(
        enterprise_business,
        imboy_feature:route_feature(admin, eb_platform_handler, p_identities)
    ).

%% --- B1: feature_names 以 registry 为单一数据源 ---

feature_names_starts_with_core_and_e2ee_test() ->
    [First, Second | _] = imboy_feature:feature_names(),
    ?assertEqual(core, First),
    ?assertEqual(e2ee, Second).

feature_names_includes_all_registry_keys_test() ->
    RegistryKeys = imboy_plugin_registry:all_feature_keys(),
    FeatureNames = imboy_feature:feature_names(),
    lists:foreach(
        fun(K) -> ?assert(lists:member(K, FeatureNames)) end,
        RegistryKeys
    ).

feature_names_contains_no_duplicates_test() ->
    Names = imboy_feature:feature_names(),
    ?assertEqual(length(Names), length(lists:usort(Names))).
