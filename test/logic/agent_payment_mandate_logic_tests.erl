-module(agent_payment_mandate_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_payment_mandate_logic 单测：mandate 创建/撤销/查询入口的金钱红线。
% 全部用 meck 隔离 ai_agent_ds / agent_payment_mandate_ds，纯验证：
%   ① owner_uid 恒为调用方（create 收到的 owner_uid == 传入 OwnerUid，body 无从篡改）
%   ② 归属校验（非 owner / 非 agent 一律拒）
%   ③ 参数边界（单笔=<累计、有效期上限、周期下限、agent≠owner）
%   ④ 单活：授权前先撤该 agent 现有有效 mandate
%   ⑤ 撤销鉴权（非本人拒，收敛为 not_authorized）
% mock fun 与测试同进程，用 process dictionary 打点断言副作用。
%%%

%% 标准 agent 元数据：agent=100 归属 owner=999
agent_meta(OwnerUid) ->
    #{<<"user_id">> => 100, <<"owner_uid">> => OwnerUid, <<"status">> => 1}.

clear() ->
    lists:foreach(fun erase/1, [created, revoked]),
    ok.

%% 合法授权参数（agent_uid 由用例覆写）
params(AgentUid) ->
    #{
        <<"agent_uid">> => AgentUid,
        <<"max_amount_fen">> => 10000,
        <<"max_total_fen">> => 50000,
        <<"expires_in_secs">> => 3600
    }.

%% ① owner_uid 恒为调用方 999；create 收到的 owner_uid 必须是 999（即使 body 塞他人 uid）
authorize_forces_owner_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [{is_agent, 1, fun(100) -> {true, agent_meta(999)} end}]},
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {error, notfound} end},
                {create, 1, fun(Data) ->
                    put(created, Data),
                    {ok, 777}
                end}
            ]}
        ],
        fun() ->
            clear(),
            %% body 恶意塞 owner_uid=1（他人）——authorize/2 根本不读 body owner_uid
            P = (params(100))#{<<"owner_uid">> => 1},
            R = agent_payment_mandate_logic:authorize(999, P),
            ?assertEqual({ok, #{<<"id">> => 777}}, R),
            Data = get(created),
            ?assertEqual(999, maps:get(owner_uid, Data)),
            ?assertEqual(100, maps:get(agent_uid, Data)),
            ?assertEqual(10000, maps:get(max_amount_fen, Data)),
            ?assertEqual(50000, maps:get(max_total_fen, Data)),
            ?assertEqual(3600, maps:get(expires_in_secs, Data))
        end
    ).

%% ② 非 owner 授权他人 agent（agent 归属 888，调用方 999）→ not_agent_owner，绝不建
authorize_not_owner_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [{is_agent, 1, fun(100) -> {true, agent_meta(888)} end}]},
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(_) -> {error, notfound} end},
                {create, 1, fun(_) ->
                    put(created, true),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_mandate_logic:authorize(999, params(100)),
            ?assertEqual({error, not_agent_owner}, R),
            ?assertEqual(undefined, get(created))
        end
    ).

%% ③ 目标非 agent（is_agent=false）→ not_agent_owner，绝不建
authorize_target_not_agent_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [{is_agent, 1, fun(100) -> false end}]},
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(_) -> {error, notfound} end},
                {create, 1, fun(_) ->
                    put(created, true),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_mandate_logic:authorize(999, params(100)),
            ?assertEqual({error, not_agent_owner}, R),
            ?assertEqual(undefined, get(created))
        end
    ).

%% ④ 单笔 > 累计（max_amount 60000 > max_total 50000）→ invalid_params，不触碰 DS
authorize_amount_gt_total_test_() ->
    ?TEST_SIMPLE(fun() ->
        P = (params(100))#{<<"max_amount_fen">> => 60000},
        ?assertEqual({error, invalid_params}, agent_payment_mandate_logic:authorize(999, P))
    end).

%% ⑤ 有效期越界：> 90 天 与 <= 0 均 invalid_params，不触碰 DS
authorize_invalid_expires_test_() ->
    ?TEST_SIMPLE(fun() ->
        POver = (params(100))#{<<"expires_in_secs">> => 7776001},
        ?assertEqual({error, invalid_params}, agent_payment_mandate_logic:authorize(999, POver)),
        PZero = (params(100))#{<<"expires_in_secs">> => 0},
        ?assertEqual({error, invalid_params}, agent_payment_mandate_logic:authorize(999, PZero))
    end).

%% ⑥ agent_uid == owner_uid → invalid_params（agent 不能是 owner 自己）
authorize_agent_equals_owner_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_params},
            agent_payment_mandate_logic:authorize(999, params(999))
        )
    end).

%% ⑦ 重复授权同 agent：先撤旧有效 mandate（id=555）再建新，保证单活约束不冲突
authorize_revokes_existing_active_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [{is_agent, 1, fun(100) -> {true, agent_meta(999)} end}]},
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, #{<<"id">> => 555}} end},
                {revoke, 1, fun(555) ->
                    put(revoked, 555),
                    {ok, 1}
                end},
                {create, 1, fun(_) ->
                    put(created, true),
                    {ok, 888}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_mandate_logic:authorize(999, params(100)),
            ?assertEqual({ok, #{<<"id">> => 888}}, R),
            ?assertEqual(555, get(revoked)),
            ?assertEqual(true, get(created))
        end
    ).

%% ⑧ 撤销他人 mandate（mandate.owner_uid=888，调用方 999）→ not_authorized，绝不撤
revoke_not_owner_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {get, 1, fun(555) -> {ok, #{<<"owner_uid">> => 888}} end},
                {revoke, 1, fun(_) ->
                    put(revoked, true),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_mandate_logic:revoke(999, 555),
            ?assertEqual({error, not_authorized}, R),
            ?assertEqual(undefined, get(revoked))
        end
    ).

%% ⑨ 撤销本人 mandate → ok；查不到（notfound）也收敛为 not_authorized（不泄露存在性）
revoke_owner_and_notfound_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {get, 1, fun
                    (555) -> {ok, #{<<"owner_uid">> => 999}};
                    (404) -> {error, notfound}
                end},
                {revoke, 1, fun(555) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 1}, agent_payment_mandate_logic:revoke(999, 555)),
            ?assertEqual({error, not_authorized}, agent_payment_mandate_logic:revoke(999, 404))
        end
    ).

%% get_active：归属校验通过返回 mandate；非 owner 返回 not_agent_owner
get_active_ownership_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_ds, [
                {is_agent, 1, fun
                    (100) -> {true, agent_meta(999)};
                    (200) -> {true, agent_meta(888)}
                end}
            ]},
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, #{<<"id">> => 42}} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, #{<<"id">> => 42}}, agent_payment_mandate_logic:get_active(999, 100)),
            %% agent 200 归属 888，调用方 999 无权查
            ?assertEqual({error, not_agent_owner}, agent_payment_mandate_logic:get_active(999, 200))
        end
    ).
