-module(imboy_profile_preset).

-export([current/0, defaults/0, defaults/1, supported_profiles/0]).
-export([normalize_profile/1, delivery_locked_features/1, fail_closed/1]).

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

-spec supported_profiles() ->
    [community | enterprise | overseas_baseline | phase1_delivery].
supported_profiles() ->
    [community, enterprise, overseas_baseline, phase1_delivery].

%% @doc 交付锁：一期交付 profile 下运行时（DB saved / 远端配置）不得重新
%% 开启的特性键。合并链最后强制压 false（与 e2ee storage_mode 硬闸同一
%% 时机），保证"旧缓存或远端全开配置不能绕过关闭面"（AC03）。二期七域
%% （RTC/live_room/Bot 产品/AI 助手/Agent Hub/MCP/E2EE）+ 一期默认不
%% 承诺的运营域；OA 共享投递设施（bot_delivery outbox / worker / guard）
%% 不在此列——enterprise_webhook 走独立门，不受 Bot 产品关闭影响（F08）。
-spec delivery_locked_features(term()) -> [atom()].
delivery_locked_features(phase1_delivery) ->
    [
        e2ee,
        rtc,
        live_room,
        bot_webhook,
        ai_assistant,
        agent_hub,
        mcp,
        wallet,
        moya,
        moment,
        location,
        channel_discover,
        channel_invitation,
        channel_order,
        group_vote,
        group_schedule,
        group_task,
        project
    ];
delivery_locked_features(_) ->
    [].

%% @doc 白名单语义 profile：未在 preset 枚举的特性键（含未来新增的
%% plugin 键）默认关闭而不是默认开启。一期交付关闭面必须 fail-closed。
-spec fail_closed(term()) -> boolean().
fail_closed(phase1_delivery) ->
    true;
fail_closed(_) ->
    false.

%% @doc 从 imboy_feature:feature_names/0 生成全量 features 默认值（全 true），
%% 两档套餐当前默认值相同，保留两函数以备未来差异化。
-spec default_features() -> map().
default_features() ->
    maps:from_list([{K, true} || K <- imboy_feature:feature_names()]).

-spec profile_defaults(
    community | enterprise | overseas_baseline | phase1_delivery
) -> map().
profile_defaults(overseas_baseline) ->
    %% L-01 海外基线预设：敏感功能默认 OFF（显式枚举；含尚未实现的
    %% live_room/ai_marketplace/bot_webhook——预设先于实现，未来 plugin
    %% 注册同名 key 时即被本表压制成 false）。核心基线（friend/c2c/
    %% group/workspace/project/channel/moment）保持 true。
    Disabled = [
        location,
        channel_discover,
        channel_order,
        live_room,
        ai_marketplace,
        bot_webhook
    ],
    Features = lists:foldl(
        fun(K, Acc) -> maps:put(K, false, Acc) end, default_features(), Disabled
    ),
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
        features => Features
    };
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
    };
%% @doc 一期交付白名单（主计划 2026-10-07 §1/§3）：Core IM、组织/工作区/
%% RBAC（enterprise_business）、人工客服（customer_service）、公告频道底座
%% （channel，组织内部 General 群依赖）、处置申诉（appeal，治理保留）。
%% 二期七域与一期默认不承诺的运营域显式关闭；未枚举键经 fail_closed/1
%% 同样压 false。E2EE OFF 只针对产品能力：TLS、企业托管加密（body_cipher/
%% keyring）、归档/检索/推送/保留合同全部保持（AC04）。
profile_defaults(phase1_delivery) ->
    Allowed = [
        core,
        channel,
        appeal,
        enterprise_business,
        customer_service
    ],
    AllKeys = lists:usort(imboy_feature:feature_names()),
    Features = maps:from_list([
        {K, lists:member(K, Allowed)}
     || K <- AllKeys
    ]),
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
        features => Features
    }.

-spec normalize_profile(term()) ->
    community | enterprise | overseas_baseline | phase1_delivery.
normalize_profile(community) ->
    community;
normalize_profile(enterprise) ->
    enterprise;
normalize_profile(overseas_baseline) ->
    overseas_baseline;
normalize_profile(phase1_delivery) ->
    phase1_delivery;
normalize_profile(<<"community">>) ->
    community;
normalize_profile(<<"enterprise">>) ->
    enterprise;
normalize_profile(<<"overseas_baseline">>) ->
    overseas_baseline;
normalize_profile(<<"phase1_delivery">>) ->
    phase1_delivery;
normalize_profile("community") ->
    community;
normalize_profile("enterprise") ->
    enterprise;
normalize_profile("overseas_baseline") ->
    overseas_baseline;
normalize_profile("phase1_delivery") ->
    phase1_delivery;
normalize_profile(_) ->
    community.
