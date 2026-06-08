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
        group_task => true
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
            group_task
        ],
        imboy_feature:feature_names()
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
