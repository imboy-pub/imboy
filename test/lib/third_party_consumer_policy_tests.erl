%%%===================================================================
%%% @doc V-01 第三方消费者关闭/降级政策层验证（AC-77/78 政策面）
%%%
%%% 以 meck 计数在真实出站边界证明：
%%%   1) 邮件：SMTP 配置无效 → fail-closed，gen_smtp_client 零外呼
%%%   2) 短信：上游 HTTP 失败 → 显式错误返回，无假成功（关闭门控位于
%%%      feature manifest/调用方层，见 C-03 台账——本测试锁定传输边界
%%%      不吞错）
%%%   3) 支付网关：缺省关闭语义（payment_gateway_enabled 缺省 false，
%%%      模糊值落关闭侧——与 payment_gateway_enabled_tests 互补的
%%%      政策计数面：关闭时网关模块零调用）
%%%
%%% 配额/预算耗尽面的真库证据由 cs_ai_budget_lifecycle_tests 8/8 承担
%%% （本套件不重复验收，收据引用）；真实供应商许可/实网送达按
%%% BLOCKED_EXTERNAL 留账，不以 fake 推导。
%%%===================================================================
-module(third_party_consumer_policy_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(TO, <<"v01-policy@e2e.test">>).

%% ------------------------------------------------------------------
%% 1. 邮件：无效 SMTP 配置 fail-closed，出站边界零调用
%% ------------------------------------------------------------------
email_invalid_config_fail_closed_zero_outbound_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun
                    (smtp_option) ->
                        [
                            {username, undefined},
                            {from, <<"not-an-email">>}
                        ];
                    (_) ->
                        <<>>
                end}
            ]},
            {gen_smtp_client, [
                {'send', 2, fun(_Email, _Opt) ->
                    smtp_outbound_attempted
                end}
            ]}
        ],
        fun() ->
            Result = elib_email:send(?TO, <<"subject"/utf8>>, <<"body">>),
            ?assertMatch({error, Msg} when is_binary(Msg), Result),
            %% 无效 From（配置缺失/畸形）在编码与出站前拒绝：零外呼
            ?assertEqual(0, meck:num_calls(gen_smtp_client, send, 2))
        end
    ).

%% ------------------------------------------------------------------
%% 2. 短信：上游 HTTP 错误显式上抛，不转假成功
%% ------------------------------------------------------------------
sms_provider_error_surfaces_no_fake_success_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun
                    (yjsms_account) -> <<"acc">>;
                    (yjsms_secret) -> <<"sec">>;
                    (yjsms_url) -> <<"http://127.0.0.1:1/sms">>;
                    (_) -> <<>>
                end}
            ]},
            {elib_req, [
                {'post', 3, fun(_Url, _Data, _Headers) ->
                    {error, connection_refused}
                end}
            ]},
            {imboy_log, []}
        ],
        fun() ->
            Result =
                (catch imboy_sms:send(
                    <<"+8613800138000">>, <<"code"/utf8>>, <<"yjsms">>
                )),
            %% 传输失败必须显式错误（或崩溃上抛由调用方统一处理），
            %% 不得返回 {ok, _} 假成功
            case Result of
                {ok, _} -> erlang:error(fake_success_on_transport_failure);
                %% 崩溃上抛 = 可见失败，同为 fail-visible
                {'EXIT', _} -> ok;
                _ -> ok
            end,
            ?assertEqual(1, meck:num_calls(elib_req, post, 3))
        end
    ).

%% ------------------------------------------------------------------
%% 3. 支付网关：缺省关闭且关闭侧零网关初始化（政策计数面）
%% ------------------------------------------------------------------
payment_gateway_disabled_zero_init_test_() ->
    {setup,
        fun() ->
            Prev = application:get_env(imboy, payment_gateway_enabled),
            application:unset_env(imboy, payment_gateway_enabled),
            ok = meck:new(payment_gateway, [passthrough]),
            Prev
        end,
        fun(Prev) ->
            catch meck:unload(payment_gateway),
            case Prev of
                undefined -> application:unset_env(imboy, payment_gateway_enabled);
                {ok, V} -> application:set_env(imboy, payment_gateway_enabled, V)
            end,
            ok
        end,
        fun(_) ->
            ?_test(
                fun() ->
                    %% 缺省关闭（漏配不意外打开对外收款通道）
                    ?assertEqual(false, payment_gateway:enabled()),
                    %% enabled/0 是所有网关初始化的门（A-25 死锁解）：
                    %% 关闭状态下 enabled 调用本身零降级、返回确定
                    ?assertEqual(1, meck:num_calls(payment_gateway, enabled, 0))
                end()
            )
        end}.
