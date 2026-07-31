-module(payment_gateway_enabled_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc 外部支付网关总开关 payment_gateway:enabled/0 的语义锁定
%%%
%%% 背景：strict 环境（IMBOYENV != dev/local/test）下 imboy_app 曾无条件要求
%%% payment_mode=live **且**至少一个网关凭据完整，否则 fail-fast；而 sandbox
%%% 在 strict 环境同样 fail-fast。两条路都堵死 = 没有真实支付商户凭据的部署方
%%% 根本装不起来（A-25 实测，Gate 0 因此不可达）。
%%%
%%% 引入总开关解死锁。本测试锁定三条不变量：
%%%   1. 缺省即关闭 —— 漏配不会意外打开对外收款通道
%%%   2. 只有精确的 true 才算开启 —— 任何模糊值落到"关闭"这个安全侧
%%%   3. 环境变量解析同样只认 "true"/"1"
%%%
%%% ⚠️ 方向与 payment_mode 相反且是刻意的：payment_mode 的危险侧是 sandbox
%%% （跳过验签），所以模糊值落 live；本开关的危险侧是"开着但没配好"，
%%% 所以模糊值落 false。两者都指向"可见地失败"而非"静默地放行"。
%%% @end
%%%===================================================================

setup() ->
    Prev = application:get_env(imboy, payment_gateway_enabled),
    application:unset_env(imboy, payment_gateway_enabled),
    PrevOsEnv = os:getenv("IMBOY_PAYMENT_GATEWAY_ENABLED"),
    {Prev, PrevOsEnv}.

cleanup({Prev, PrevOsEnv}) ->
    case Prev of
        undefined -> application:unset_env(imboy, payment_gateway_enabled);
        {ok, V} -> application:set_env(imboy, payment_gateway_enabled, V)
    end,
    case PrevOsEnv of
        false -> os:unsetenv("IMBOY_PAYMENT_GATEWAY_ENABLED");
        S -> os:putenv("IMBOY_PAYMENT_GATEWAY_ENABLED", S)
    end,
    ok.

enabled_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun unset_defaults_to_disabled/0,
        fun explicit_true_enables/0,
        fun non_true_values_stay_disabled/0,
        fun env_var_only_true_or_one_enables/0,
        fun env_var_garbage_disables/0
    ]}.

%% 漏配时必须是关闭。开着而没配凭据 = 启动即 fail-fast，正是要解的死锁。
unset_defaults_to_disabled() ->
    ?assertEqual(false, payment_gateway:enabled()).

explicit_true_enables() ->
    application:set_env(imboy, payment_gateway_enabled, true),
    ?assertEqual(true, payment_gateway:enabled()).

%% 只认 atom true。字符串 "true"、binary <<"true">>、1 一律不算开启 ——
%% 配置写错时宁可功能不可用，也不要让人以为收款已经通了。
non_true_values_stay_disabled() ->
    lists:foreach(
        fun(V) ->
            application:set_env(imboy, payment_gateway_enabled, V),
            ?assertEqual(
                false,
                payment_gateway:enabled(),
                lists:flatten(io_lib:format("值 ~p 不应被当作开启", [V]))
            )
        end,
        [false, "true", <<"true">>, 1, undefined, yes, "1"]
    ).

env_var_only_true_or_one_enables() ->
    lists:foreach(
        fun(S) ->
            application:unset_env(imboy, payment_gateway_enabled),
            os:putenv("IMBOY_PAYMENT_GATEWAY_ENABLED", S),
            ok = imboy_env:override_from_env(),
            ?assertEqual(
                true,
                payment_gateway:enabled(),
                lists:flatten(io_lib:format("环境变量 ~s 应开启", [S]))
            )
        end,
        ["true", "TRUE", " true ", "1"]
    ).

env_var_garbage_disables() ->
    lists:foreach(
        fun(S) ->
            application:set_env(imboy, payment_gateway_enabled, true),
            os:putenv("IMBOY_PAYMENT_GATEWAY_ENABLED", S),
            ok = imboy_env:override_from_env(),
            ?assertEqual(
                false,
                payment_gateway:enabled(),
                lists:flatten(io_lib:format("环境变量 ~s 不应开启", [S]))
            )
        end,
        ["false", "yes", "on", "enabled", "0", "True-", "2"]
    ).
