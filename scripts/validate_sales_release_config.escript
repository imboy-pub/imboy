#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).
-export([main/1]).

%% 销售版发布门禁：只检查功能策略，不读取或输出任何密钥。
%% 生产配置文件本身被 .gitignore 忽略，由部署环境单独提供。

main([]) ->
    main(["config/sys.pro.config"]);
main(["--self-test"]) ->
    Good = [[
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
    case validate(Good) of
        {ok, _} ->
            io:format("SALES_RELEASE_CONFIG_SELF_TEST=PASS~n"),
            halt(0);
        Other ->
            io:format(standard_error, "SALES_RELEASE_CONFIG_SELF_TEST=FAIL ~p~n", [Other]),
            halt(1)
    end;
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
            Capabilities = proplists:get_value(capabilities, AppConfig, #{}),
            Features = proplists:get_value(features, AppConfig, #{}),
            Profile = proplists:get_value(product_profile, AppConfig, undefined),
            E2eeMode = capability_value(e2ee_mode, Capabilities),
            RequiredFeatures = [e2ee, channel, channel_order],
            MissingFeatures = [
                Name
             || Name <- RequiredFeatures,
                feature_enabled(Name, Features) =/= true
            ],
            SalesRelease = sales_release_enabled(),
            Missing =
                case SalesRelease of
                    true -> payment_missing(AppConfig, []);
                    false -> []
                end,
            MissingSalesFlags = MissingFeatures ++ Missing,
            case {normalize_mode(E2eeMode), MissingSalesFlags} of
                {Mode, []} when Mode =:= required; Mode =:= compliance ->
                    {ok, io_lib:format(
                        "profile=~p e2ee_mode=~p e2ee=true channel=true channel_order=true payment=~p",
                        [Profile, Mode, payment_summary(SalesRelease)]
                    )};
                {Mode, _} ->
                    {error, io_lib:format(
                        "profile=~p e2ee_mode=~p missing_or_disabled_sales_flags=~p",
                        [Profile, Mode, MissingSalesFlags]
                    )}
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
