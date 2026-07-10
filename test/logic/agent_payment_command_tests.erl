-module(agent_payment_command_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_payment_command 单测：确定性支付指令解析 + 授权扣款。
% 重点验证两条安全红线：① 金额「元→分」整数解析无 float；② 只有 mandate.owner_uid
% 能命令 agent 花钱（FromUid≠owner 一律拒绝、绝不扣款）。授权路径用 meck 隔离 DS/logic。
%%%

%% ---------- parse_amount（纯函数）----------

parse_keyword_plus_amount_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 整数元 → 分
        ?assertEqual({ok, 500}, agent_payment_command:parse_amount(<<"付款 5"/utf8>>)),
        %% 两位小数
        ?assertEqual({ok, 550}, agent_payment_command:parse_amount(<<"转账 5.50"/utf8>>)),
        %% 一位小数=角
        ?assertEqual({ok, 550}, agent_payment_command:parse_amount(<<"支付 5.5"/utf8>>)),
        %% 分位
        ?assertEqual({ok, 505}, agent_payment_command:parse_amount(<<"pay 5.05">>)),
        %% 0.01 元 = 1 分（最小额）
        ?assertEqual({ok, 1}, agent_payment_command:parse_amount(<<"transfer 0.01">>))
    end).

parse_rejects_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 无关键词 → nomatch（防普通聊天误触发）
        ?assertEqual(nomatch, agent_payment_command:parse_amount(<<"给你 5 块"/utf8>>)),
        %% 有关键词无金额 → nomatch
        ?assertEqual(nomatch, agent_payment_command:parse_amount(<<"付款"/utf8>>)),
        %% 金额为 0 → nomatch
        ?assertEqual(nomatch, agent_payment_command:parse_amount(<<"付款 0"/utf8>>)),
        ?assertEqual(nomatch, agent_payment_command:parse_amount(<<"付款 0.00"/utf8>>)),
        %% 非数字 → nomatch
        ?assertEqual(nomatch, agent_payment_command:parse_amount(<<"付款 abc"/utf8>>)),
        %% 非 binary → nomatch
        ?assertEqual(nomatch, agent_payment_command:parse_amount(undefined))
    end).

%% H2 回归：复合词子串不得误触发真实转账（关键词须带右边界）
keyword_boundary_no_false_trigger_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% "支付宝"含子串"支付"，但其后是"宝"(非边界) → 不算指令
        ?assertEqual(
            nomatch, agent_payment_command:parse_amount(<<"我用支付宝转了123给你"/utf8>>)
        ),
        %% "转账记录"含"转账"，其后是"记" → 不算指令
        ?assertEqual(
            nomatch, agent_payment_command:parse_amount(<<"看下转账记录 5 条"/utf8>>)
        ),
        %% "payment"含"pay"，其后是"m" → 不算指令
        ?assertEqual(nomatch, agent_payment_command:parse_amount(<<"payment done 5">>)),
        %% "paypal"含"pay"，其后是"p" → 不算指令
        ?assertEqual(nomatch, agent_payment_command:parse_amount(<<"paypal 5">>)),
        %% 正例：关键词后紧跟空格/数字 → 仍正确识别
        ?assertEqual({ok, 500}, agent_payment_command:parse_amount(<<"支付 5"/utf8>>)),
        ?assertEqual({ok, 500}, agent_payment_command:parse_amount(<<"pay5">>))
    end).

%% ---------- authorize_and_pay（授权红线）----------

%% FromUid = owner → 授权通过，凭 mandate 扣款；RefNo 由 MsgId 派生（AGP_<MsgId>）
authorized_pays_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, #{<<"owner_uid">> => 999}} end}
            ]},
            {agent_payment_logic, [
                {pay_with_mandate, 4, fun(Agent, Payee, Amount, Ref) ->
                    put(paid, {Agent, Payee, Amount, Ref}),
                    {ok, #{ref_no => Ref}}
                end}
            ]}
        ],
        fun() ->
            erase(paid),
            R = agent_payment_command:authorize_and_pay(999, 100, 200, 5000, 7777),
            ?assertMatch({ok, _}, R),
            %% 传给 pay_with_mandate 的是 (agent100, payee200, 5000分, "AGP_7777")
            ?assertEqual({100, 200, 5000, <<"AGP_7777">>}, get(paid))
        end
    ).

%% owner_uid 以 binary 存储也能正确比对（to_int 归一）
authorized_binary_owner_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, #{<<"owner_uid">> => <<"999">>}} end}
            ]},
            {agent_payment_logic, [
                {pay_with_mandate, 4, fun(_, _, _, _) ->
                    put(paid, true),
                    {ok, #{}}
                end}
            ]}
        ],
        fun() ->
            erase(paid),
            R = agent_payment_command:authorize_and_pay(999, 100, 200, 5000, 7777),
            ?assertMatch({ok, _}, R),
            ?assertEqual(true, get(paid))
        end
    ).

%% FromUid ≠ owner → 拒绝，**绝不扣款**（核心红线：任意群成员不能掏空 owner 钱包）
unauthorized_rejected_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, #{<<"owner_uid">> => 999}} end}
            ]},
            {agent_payment_logic, [
                {pay_with_mandate, 4, fun(_, _, _, _) ->
                    put(paid, true),
                    {ok, #{}}
                end}
            ]}
        ],
        fun() ->
            erase(paid),
            %% FromUid=888 ≠ owner 999
            R = agent_payment_command:authorize_and_pay(888, 100, 200, 5000, 7777),
            ?assertEqual({error, not_authorized}, R),
            ?assertEqual(undefined, get(paid))
        end
    ).

%% 无有效 mandate → mandate_invalid，绝不扣款
no_mandate_rejected_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {error, notfound} end}
            ]},
            {agent_payment_logic, [
                {pay_with_mandate, 4, fun(_, _, _, _) ->
                    put(paid, true),
                    {ok, #{}}
                end}
            ]}
        ],
        fun() ->
            erase(paid),
            R = agent_payment_command:authorize_and_pay(999, 100, 200, 5000, 7777),
            ?assertEqual({error, mandate_invalid}, R),
            ?assertEqual(undefined, get(paid))
        end
    ).
