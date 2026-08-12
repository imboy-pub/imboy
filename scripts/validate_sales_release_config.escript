#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).
-export([main/1]).

%% 销售版发布门禁：只检查功能策略，不读取或输出任何密钥。
%% 生产配置文件本身被 .gitignore 忽略，由部署环境单独提供。

main([]) ->
    main(["config/sys.pro.config"]);
main(["--self-test"]) ->
    %% 销售版（默认 SALES_RELEASE=true）：必须满足 e2ee_mode + 三 feature + 支付 live。
    Sales = [[
        {imboy, [
            {product_profile, community},
            {capabilities, #{e2ee_mode => required}},
            {payment_gateway_enabled, true},
            {payment_mode, live},
            {features, #{
                e2ee => #{enabled => true},
                channel => #{enabled => true},
                channel_order => #{enabled => true}
            }}
        ]}
    ]],
    %% 社区版（SALES_RELEASE=false）：宽松，仅 {imboy,...} 存在即可。
    Community = [[{imboy, [{product_profile, community}]}]],
    ok = expect_ok(sales_strict, validate(Sales)),
    os:putenv("IMBOY_SALES_RELEASE", "false"),
    ok = expect_ok(community_relaxed, validate(Community)),
    os:unsetenv("IMBOY_SALES_RELEASE"),
    io:format("SALES_RELEASE_CONFIG_SELF_TEST=PASS~n"),
    halt(0);
main([Path | _]) ->
    case file:consult(Path) of
        {ok, Terms} ->
            case validate(Terms) of
                {ok, Summary} ->
                    io:format("SALES_RELEASE_CONFIG=PASS ~ts~n", [Summary]),
                    halt(0);
                {error, Reason} ->
                    io:format(standard_error, "SALES_RELEASE_CONFIG=FAIL ~ts~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format(standard_error, "SALES_RELEASE_CONFIG=FAIL cannot consult ~ts: ~p~n", [Path, Reason]),
            halt(1)
    end.

validate(Terms) ->
    Config = unwrap_config(Terms),
    case lists:keyfind(imboy, 1, Config) of
        {imboy, AppConfig} when is_list(AppConfig) ->
            Profile = proplists:get_value(product_profile, AppConfig, undefined),
            case sales_release_enabled() of
                false ->
                    %% 社区/演示部署（IMBOY_SALES_RELEASE=false）：宽松，不强制 E2EE/频道/支付。
                    %% 现网以明文 C2C 为主，强制 e2ee_mode=required/compliance 会被
                    %% imboy_policy:message_encryption_required/0 拒收（明文断生产）。
                    %% 销售策略门禁仅对 SALES_RELEASE=true 生效（见 deploy/README.md）。
                    {ok, io_lib:format(
                        "profile=~p community release (SALES_RELEASE=false), policy relaxed",
                        [Profile]
                    )};
                true ->
                    Capabilities = proplists:get_value(capabilities, AppConfig, #{}),
                    Features = proplists:get_value(features, AppConfig, #{}),
                    E2eeMode = capability_value(e2ee_mode, Capabilities),
                    RequiredFeatures = [e2ee, channel, channel_order],
                    MissingFeatures = [
                        Name
                     || Name <- RequiredFeatures,
                        feature_enabled(Name, Features) =/= true
                    ],
                    Missing = payment_missing(AppConfig, []),
                    MissingSalesFlags = MissingFeatures ++ Missing,
                    case {normalize_mode(E2eeMode), MissingSalesFlags} of
                        {Mode, []} when Mode =:= required; Mode =:= compliance ->
                            {ok, io_lib:format(
                                "profile=~p e2ee_mode=~p e2ee=true channel=true channel_order=true payment=~p",
                                [Profile, Mode, payment_summary(true)]
                            )};
                        {Mode, _} ->
                            {error, io_lib:format(
                                "profile=~p e2ee_mode=~p missing_or_disabled_sales_flags=~p",
                                [Profile, Mode, MissingSalesFlags]
                            )}
                    end
            end;
        _ ->
            {error, "missing imboy application config"}
    end.

unwrap_config([Config]) when is_list(Config) ->
    Config;
unwrap_config(Config) ->
    Config.

payment_missing(AppConfig, Acc) ->
    GatewayEnabled =
        case os:getenv("IMBOY_PAYMENT_GATEWAY_ENABLED") of
            false -> normalize_bool(proplists:get_value(payment_gateway_enabled, AppConfig, false));
            EnvGatewayValue -> normalize_bool(EnvGatewayValue)
        end,
    Mode =
        case os:getenv("IMBOY_PAYMENT_MODE") of
            false -> normalize_mode(proplists:get_value(payment_mode, AppConfig, sandbox));
            EnvPaymentMode -> normalize_mode(EnvPaymentMode)
        end,
    Acc1 =
        case GatewayEnabled of
            true -> Acc;
            false -> [payment_gateway_disabled | Acc]
        end,
    case Mode of
        live -> Acc1;
        _ -> [payment_mode_not_live | Acc1]
    end.

sales_release_enabled() ->
    case os:getenv("IMBOY_SALES_RELEASE") of
        false -> true;
        Value -> normalize_bool(Value)
    end.

payment_summary(true) -> live;
payment_summary(false) -> not_required.

expect_ok(_Label, {ok, _}) ->
    ok;
expect_ok(Label, Other) ->
    io:format(standard_error, "SALES_RELEASE_CONFIG_SELF_TEST=FAIL ~p: ~p~n", [Label, Other]),
    halt(1).

capability_value(Name, Capabilities) when is_map(Capabilities) ->
    maps:get(Name, Capabilities, maps:get(atom_to_binary(Name), Capabilities, undefined));
capability_value(Name, Capabilities) when is_list(Capabilities) ->
    proplists:get_value(Name, Capabilities, undefined);
capability_value(_, _) ->
    undefined.

feature_enabled(Name, Features) when is_map(Features) ->
    Switch = maps:get(Name, Features, maps:get(atom_to_binary(Name), Features, undefined)),
    switch_enabled(Switch);
feature_enabled(Name, Features) when is_list(Features) ->
    switch_enabled(proplists:get_value(Name, Features, undefined));
feature_enabled(_, _) ->
    false.

switch_enabled(#{enabled := Value}) ->
    normalize_bool(Value);
switch_enabled(#{<<"enabled">> := Value}) ->
    normalize_bool(Value);
switch_enabled(Value) ->
    normalize_bool(Value).

normalize_bool(true) -> true;
normalize_bool(1) -> true;
normalize_bool(<<"true">>) -> true;
normalize_bool("true") -> true;
normalize_bool(_) -> false.

normalize_mode(required) -> required;
normalize_mode(compliance) -> compliance;
normalize_mode(live) -> live;
normalize_mode(sandbox) -> sandbox;
normalize_mode(<<"required">>) -> required;
normalize_mode(<<"compliance">>) -> compliance;
normalize_mode(<<"live">>) -> live;
normalize_mode(<<"sandbox">>) -> sandbox;
normalize_mode("required") -> required;
normalize_mode("compliance") -> compliance;
normalize_mode("live") -> live;
normalize_mode("sandbox") -> sandbox;
normalize_mode(Value) -> Value.
