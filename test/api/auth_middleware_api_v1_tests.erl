-module(auth_middleware_api_v1_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% @doc auth_middleware_api_v1 模块测试
%%%
%%% 回归 2026-07-08：43224c1f/4cc20e81 硬切换 /api 前缀后，本模块 execute/2
%%% 里判断是否需要 auth_ds:verify_sign/2（设备签名防篡改校验）的显式分支
%%% 一度仍匹配裸 /v1/ws、/v1/init、/v1/refreshtoken、/v1/passport/ 路径，
%%% 永远不命中真实的 /api/* 路径；这些端点虽在 open() 白名单里免 JWT，
%%% 但仍需 verify_sign，回归后被悄悄跳过。后又下架了 v0 裸 /api/* 路由，
%%% 只保留 /api/v1/* 形态，本文件只测 v1 路径。

verify_sign_called_for_ws_test_() ->
    verify_sign_called_case(<<"/api/v1/ws">>).

verify_sign_called_for_init_test_() ->
    verify_sign_called_case(<<"/api/v1/init">>).

verify_sign_called_for_refreshtoken_test_() ->
    verify_sign_called_case(<<"/api/v1/refreshtoken">>).

verify_sign_called_for_passport_test_() ->
    verify_sign_called_case(<<"/api/v1/passport/login">>).

verify_sign_called_case(Path) ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> Path end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, _Def) -> <<"on">> end}
            ]},
            {auth_ds, [
                {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end},
                {'condition', 5, fun(_InOptionLi, _InOpenLi, _Auth, Req, Env) -> {ok, Req, Env} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{},
            Result = auth_middleware_api_v1:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result),
            ?assertEqual(1, meck:num_calls(auth_ds, verify_sign, 2))
        end
    ).

%% @doc 非 open 路径（普通受保护端点）也应触发 verify_sign（既有行为，防止误改）
verify_sign_called_for_protected_path_test_() ->
    verify_sign_called_case(<<"/api/v1/user/info">>).

%% @doc api_auth_switch=off 时任何路径都不应调用 verify_sign
verify_sign_skipped_when_switch_off_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/ws">> end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, _Def) -> <<"off">> end}
            ]},
            {auth_ds, [
                {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end},
                {'condition', 5, fun(_InOptionLi, _InOpenLi, _Auth, Req, Env) -> {ok, Req, Env} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{},
            Result = auth_middleware_api_v1:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result),
            ?assertEqual(0, meck:num_calls(auth_ds, verify_sign, 2))
        end
    ).

%%%===================================================================
%%% 鉴权豁免矩阵回归（BE-01，2026-08）
%%%
%%% execute/2 在 Switch=on 时的完整判定矩阵（每象限至少一正一负）：
%%%
%%%   路径类别                           verify_sign(902门)  condition(InOpt,InOpen,Auth)
%%%   ---------------------------------  ------------------  ---------------------------
%%%   /api/v1/ws|init|refreshtoken       调用（见上方既有用例）
%%%   /api/v1/passport/* 前缀            调用（全前缀拦截）
%%%   /api/v1/payment/callback/<gateway> 不调用              (false,true,*) 免token放行
%%%   /api/v1/webhook/channel/<token>    不调用              (false,true,*) 免token放行
%%%   open() 其它成员(user/show、oidc..) 不调用              (false,true,*) 免token放行
%%%   option() 成员(feedback/add 等)     调用（不免签名门）  (true,false,Auth) 无token放行/
%%%                                                          有token则验证token
%%%   受保护端点                         调用                (false,false,Auth) 无/坏token
%%%                                                          →401 stop；好token→uid注入
%%%
%%% 下方矩阵用例走【真实 auth_ds:condition/5 + 真实 do_authorization】链路
%%% （meck passthrough：auth_ds 只打桩 verify_sign），401 语义断言到
%%% HTTP 状态码 + envelope code（?ERR_TOKEN_MISSING）两层。
%%%===================================================================

env() ->
    %% do_authorization/3 的 ok 分支强制匹配 #{handler_opts := _}
    #{handler_opts => #{}}.

%% 矩阵通用 mock：cowboy_req 打桩 path/header/reply，config_ds 开鉴权总闸，
%% auth_ds 只打桩 verify_sign（condition 经 passthrough 走真实实现）。
matrix_mocks(Path, Authorization) ->
    matrix_mocks(Path, Authorization, []).

matrix_mocks(Path, Authorization, Extra) ->
    [
        {cowboy_req, [
            {'path', 1, fun(_R) -> Path end},
            {'header', 2, fun
                (<<"authorization">>, _R) -> Authorization;
                (_Other, _R) -> undefined
            end},
            %% 真实 elib_response:error_with_status 最终走 reply/4，
            %% 记录 HTTP 状态码与 body 供 401 语义断言
            {'reply', 4, fun(Status, _Headers, Body, Req) ->
                Req#{replied => {Status, Body}}
            end}
        ]},
        {config_ds, [
            {'env', 2, fun(api_auth_switch, _Def) -> <<"on">> end}
        ]},
        {auth_ds, [
            {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end}
        ]}
    ] ++ Extra.

%% 捕获 condition/5 的真实调用参数（passthrough 调用同样进入 meck history）
condition_calls() ->
    [Args || {_Pid, {_M, condition, Args}, _Res} <- meck:history(auth_ds)].

envelope_code({stop, Req}) ->
    {_, Body} = maps:get(replied, Req),
    maps:get(<<"code">>, jsone:decode(Body, [{object_format, map}])).

%% 象限正例：免签名门 + 免 token 放行（payment callback / webhook / open 成员）
open_pass_case(Path) ->
    ?WITH_MECKS(matrix_mocks(Path, undefined), fun() ->
        ?assertEqual({ok, #{}, env()}, auth_middleware_api_v1:execute(#{}, env())),
        ?assertEqual(0, meck:num_calls(auth_ds, verify_sign, 2)),
        ?assertEqual([[false, true, undefined, #{}, env()]], condition_calls())
    end).

%% 象限负例：不命中豁免 → 先过签名门、无 token 被 401 拒（真实 condition 链路）
rejected_401_case(Path) ->
    ?WITH_MECKS(matrix_mocks(Path, undefined), fun() ->
        Result = auth_middleware_api_v1:execute(#{}, env()),
        ?assertMatch({stop, #{replied := {401, _}}}, Result),
        ?assertEqual(1, meck:num_calls(auth_ds, verify_sign, 2)),
        ?assertEqual(?ERR_TOKEN_MISSING, envelope_code(Result))
    end).

%% ===================================================================
%% 象限 1：/api/v1/payment/callback/<gateway> 前缀放行（:gateway 变量段）
%% ===================================================================

payment_callback_prefix_bypasses_sign_and_auth_test_() ->
    Paths = [
        <<"/api/v1/payment/callback/alipay">>,
        <<"/api/v1/payment/callback/wechat">>,
        <<"/api/v1/payment/callback/stripe">>,
        %% 尾部斜杠被 remove_last_forward_slash 归一后仍命中前缀
        <<"/api/v1/payment/callback/alipay/notify/1/">>
    ],
    [open_pass_case(P) || P <- Paths].

%% 反例：前缀相似但未命中（前缀匹配要求第 25 字符起是变量段）
payment_callback_lookalike_prefixes_not_open_test_() ->
    Paths = [
        %% 无尾斜杠：恰好 24 字符，sub_string(1,25) 不等于 25 字符前缀
        <<"/api/v1/payment/callback">>,
        <<"/api/v1/payment/callbackx/alipay">>,
        <<"/api/v1/payment/callbac/alipay">>
    ],
    [rejected_401_case(P) || P <- Paths].

%% ===================================================================
%% 象限 2：/api/v1/webhook/channel/<token> 前缀放行（:token 变量段，
%% token 即凭证，限流/校验在 channel_webhook_logic:incoming/2）
%% ===================================================================

webhook_channel_prefix_bypasses_sign_and_auth_test_() ->
    Paths = [
        <<"/api/v1/webhook/channel/tok_9f2c">>,
        <<"/api/v1/webhook/channel/a">>
    ],
    [open_pass_case(P) || P <- Paths].

webhook_channel_lookalike_prefixes_not_open_test_() ->
    Paths = [
        <<"/api/v1/webhook/channel">>,
        <<"/api/v1/webhook/channelx/abc">>
    ],
    [rejected_401_case(P) || P <- Paths].

%% ===================================================================
%% 象限 3：open() 非签名门成员 —— 免 902 也免 token
%% ===================================================================

open_list_members_skip_sign_gate_test_() ->
    Paths = [
        <<"/api/v1/user/show">>,
        %% OIDC 登录流：浏览器重定向无 sign/did 头，必须免 902 签名门
        <<"/api/v1/auth/oidc/authorize">>,
        <<"/api/v1/auth/oidc/callback">>,
        <<"/api/v1/auth/oidc/exchange">>,
        %% Bot 发消息：凭证是 api_token，不是用户 JWT/设备签名
        <<"/api/v1/bot/send_message">>
    ],
    [open_pass_case(P) || P <- Paths].

%% 反例：曾因泄露设备/拓扑清单被移出 open() 的端点不得回退白名单
%% （见 imboy_router:open/0 内 /api/v1/conversation/online 注释）
conversation_online_must_stay_protected_test_() ->
    rejected_401_case(<<"/api/v1/conversation/online">>).

%% ===================================================================
%% 象限 4：/api/v1/passport/ 前缀全量过签名门（含非 open 子路径）
%% （正例 login 等已在既有 verify_sign_called_for_passport_test_ 覆盖）
%% ===================================================================

passport_subpath_outside_open_still_hits_sign_gate_test_() ->
    rejected_401_case(<<"/api/v1/passport/unknown_sub">>).

%% ===================================================================
%% 象限 5：option() 可选认证 —— 无 token 放行，带 token 则验证
%% ===================================================================

%% 现状行为：option 端点不在 open() 里，仍要先过 verify_sign（902 门）。
%% 此处钉死该语义，防止有人误以为 option 豁免签名门。
option_paths_without_token_pass_but_hit_sign_gate_test_() ->
    Paths = [
        <<"/api/v1/feedback/add">>,
        <<"/api/v1/app_version/check">>,
        <<"/api/v1/app_upgrade/report">>
    ],
    [
        ?WITH_MECKS(matrix_mocks(P, undefined), fun() ->
            ?assertEqual({ok, #{}, env()}, auth_middleware_api_v1:execute(#{}, env())),
            ?assertEqual(1, meck:num_calls(auth_ds, verify_sign, 2)),
            ?assertEqual([[true, false, undefined, #{}, env()]], condition_calls())
        end)
     || P <- Paths
    ].

%% option 端点带有效 token：可选验证生效，uid 注入认证上下文
option_path_with_valid_token_verifies_token_test_() ->
    ?WITH_MECKS(
        matrix_mocks(<<"/api/v1/feedback/add">>, <<"Bearer opt_token">>, [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"opt_token">>) ->
                    {ok, 42, 9999999999999, <<"tk">>, <<"did-1">>}
                end}
            ]},
            {user_device_ds, [
                {'is_active', 2, fun(42, <<"did-1">>) -> true end}
            ]}
        ]),
        fun() ->
            ?assertMatch(
                {ok, #{}, #{handler_opts := #{current_uid := 42, current_did := <<"did-1">>}}},
                auth_middleware_api_v1:execute(#{}, env())
            )
        end
    ).

%% option 端点带无效 token：可选验证一旦提供凭证就必须通过
option_path_with_invalid_token_rejected_test_() ->
    ?WITH_MECKS(
        matrix_mocks(<<"/api/v1/feedback/add">>, <<"Bearer bad_token">>, [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"bad_token">>) ->
                    {error, ?ERR_TOKEN_INVALID, <<"invalid">>, #{}}
                end}
            ]}
        ]),
        fun() ->
            Result = auth_middleware_api_v1:execute(#{}, env()),
            ?assertMatch({stop, #{replied := {401, _}}}, Result),
            ?assertEqual(?ERR_TOKEN_INVALID, envelope_code(Result))
        end
    ).

%% option 相似路径不在列表：无 token 必须被拒
option_lookalike_path_rejected_test_() ->
    rejected_401_case(<<"/api/v1/feedback/addx">>).

%% ===================================================================
%% 象限 6：受保护端点（非 open 非 option）
%% ===================================================================

protected_path_without_token_rejected_401_test_() ->
    rejected_401_case(<<"/api/v1/user/info">>).

protected_path_with_invalid_token_rejected_test_() ->
    ?WITH_MECKS(
        matrix_mocks(<<"/api/v1/user/info">>, <<"Bearer expired_token">>, [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"expired_token">>) ->
                    {error, ?ERR_TOKEN_INVALID, <<"expired">>, #{}}
                end}
            ]}
        ]),
        fun() ->
            Result = auth_middleware_api_v1:execute(#{}, env()),
            ?assertMatch({stop, #{replied := {401, _}}}, Result),
            ?assertEqual(1, meck:num_calls(auth_ds, verify_sign, 2)),
            ?assertEqual(?ERR_TOKEN_INVALID, envelope_code(Result))
        end
    ).

%% 受保护端点带有效 JWT：先过签名门，token 解出 uid/did 注入 handler_opts
%% （E2EE-013：crypto 写端点据此校验设备所有权）
protected_path_with_valid_token_passes_test_() ->
    ?WITH_MECKS(
        matrix_mocks(<<"/api/v1/user/info">>, <<"Bearer good_token">>, [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"good_token">>) ->
                    {ok, 42, 9999999999999, <<"tk">>, <<"did-1">>}
                end}
            ]},
            {user_device_ds, [
                {'is_active', 2, fun(42, <<"did-1">>) -> true end}
            ]}
        ]),
        fun() ->
            ?assertMatch(
                {ok, #{}, #{handler_opts := #{current_uid := 42, current_did := <<"did-1">>}}},
                auth_middleware_api_v1:execute(#{}, env())
            ),
            ?assertEqual(1, meck:num_calls(auth_ds, verify_sign, 2)),
            ?assertEqual(1, meck:num_calls(token_ds, decrypt_token, 1))
        end
    ).
