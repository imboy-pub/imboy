-module(payment_sign_default_mode_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc payment_sign 默认模式安全性测试
%%%
%%% 背景：payment_mode 此前默认 sandbox，而 sandbox_verify/3 无条件返回
%%% {ok, #{}}（完全跳过验签），且 /api/v1/payment/callback/:gateway 免 JWT。
%%% 二者叠加 = 任何人 POST 一条自造回调即可给自己的充值单入账。
%%%
%%% 本测试锁定：漏配 payment_mode 时必须落到"拒绝"一侧而非"放行"一侧。
%%% @end
%%%===================================================================

setup() ->
    Prev = application:get_env(imboy, payment_mode),
    %% 模拟"漏配"：确保该键完全不存在，走 get_env 的默认值分支
    application:unset_env(imboy, payment_mode),
    Prev.

cleanup(undefined) ->
    application:unset_env(imboy, payment_mode);
cleanup({ok, Mode}) ->
    application:set_env(imboy, payment_mode, Mode).

default_mode_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun unset_payment_mode_does_not_pass_through/0,
        fun explicit_sandbox_still_passes_through/0
    ]}.

%% 未配置 payment_mode 时不得直通。
%% 直通的判据是返回 {ok, #{}}（sandbox_verify/3 的唯一返回值）；
%% live 分支在无凭据时返回 {error, no_credential}，这正是我们要的失败一侧。
unset_payment_mode_does_not_pass_through() ->
    Result = payment_sign:verify(<<"alipay">>, <<"a=1&sign=x">>, #{}),
    ?assertNotEqual({ok, #{}}, Result),
    ?assertMatch({error, _}, Result).

%% 显式设置 sandbox 时仍然直通 —— 开发/测试不受影响。
%% 生产侧由 imboy_app:ensure_payment_mode_safe/0 在 strict env 下拦截。
explicit_sandbox_still_passes_through() ->
    application:set_env(imboy, payment_mode, sandbox),
    ?assertEqual(
        {ok, #{}},
        payment_sign:verify(<<"alipay">>, <<"a=1&sign=x">>, #{})
    ).
