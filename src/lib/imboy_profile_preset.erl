-module(imboy_profile_preset).

-export([current/0, defaults/0, defaults/1, supported_profiles/0]).

-define(PRODUCT_PROFILE_CONFIG_KEY, <<"product_profile">>).

-spec current() -> community | enterprise.
current() ->
    normalize_profile(
        config_ds:get(?PRODUCT_PROFILE_CONFIG_KEY, config_ds:env(product_profile, community))
    ).

-spec defaults() -> map().
defaults() ->
    defaults(current()).

-spec defaults(term()) -> map().
defaults(Profile) ->
    profile_defaults(normalize_profile(Profile)).

-spec supported_profiles() -> [community | enterprise].
supported_profiles() ->
    [community, enterprise].

%% @doc 从 imboy_feature:feature_names/0 生成全量 features 默认值（全 true），
%% 两档套餐当前默认值相同，保留两函数以备未来差异化。
-spec default_features() -> map().
default_features() ->
    maps:from_list([{K, true} || K <- imboy_feature:feature_names()]).

-spec profile_defaults(community | enterprise) -> map().
profile_defaults(community) ->
    #{
        capabilities => #{
            storage_mode => archived,
            e2ee_mode => optional,
            message_search => false,
            message_export => false,
            audit_mode => metadata,
            retention_policy => #{
                mode => rolling_days,
                days => 30
            }
        },
        features => default_features()
    };
profile_defaults(enterprise) ->
    #{
        capabilities => #{
            storage_mode => archived,
            e2ee_mode => disabled,
            message_search => true,
            message_export => true,
            audit_mode => full,
            retention_policy => #{
                mode => rolling_days,
                days => 365
            }
        },
        features => default_features()
    }.

-spec normalize_profile(term()) -> community | enterprise.
normalize_profile(community) ->
    community;
normalize_profile(enterprise) ->
    enterprise;
normalize_profile(<<"community">>) ->
    community;
normalize_profile(<<"enterprise">>) ->
    enterprise;
normalize_profile("community") ->
    community;
normalize_profile("enterprise") ->
    enterprise;
normalize_profile(_) ->
    community.
