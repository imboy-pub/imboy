%%% @doc presence 时钟量纲合同（CS-INT-02 DEFECT 回归锁）。
%%%
%%% 缺陷事实：seat_presence_list 曾漏声明 clock_unit => second，at 以毫秒
%%% 缺省注入；而 last_heartbeat_at 是 epoch 秒——TTL 判定恒 offline（真实
%%% 集成门实证：心跳当秒 list 仍 offline）。本合同冻结：**所有消费
%%% presence TTL 的动作，其注入时钟必须是 second**。
-module(cs_presence_clock_contract_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TTL_ACTIONS, [
    seat_presence_heartbeat,
    seat_presence_status,
    seat_presence_list,
    %% DEFECT-2：stats 窗口换算消费 at（epoch 秒）——毫秒注入即窗口错千倍。
    session_stats,
    p_session_stats
]).

all_ttl_actions_use_second_clock_test() ->
    [
        ?assertEqual(
            second,
            maps:get(
                clock_unit,
                element_kase(
                    case cs_actions:tenant(Action) of
                        {ok, _} = Ok -> Ok;
                        _ -> cs_actions:platform(Action)
                    end
                ),
                undefined
            ),
            {action_missing_second_clock, Action}
        )
     || Action <- ?TTL_ACTIONS
    ].

element_kase({ok, Entry}) ->
    [Kase | _] = maps:get(cases, Entry),
    Kase.

%% DEFECT-3 回归锁：动作表参数类型必须是合法 ptype（integer/map 集合外的
%% 拼写（如 `integer`）会被 coerce 兜底拒绝——真实 HTTP 面恒 4xx）。
legal_ptype_test() ->
    Legal = [tsid, int, binary, list, map],
    Tables = [
        cs_actions:tenant_actions(), cs_actions:platform_actions(), cs_actions:widget_actions()
    ],
    [
        ?assert(lists:member(T, Legal), {illegal_ptype, P, T})
     || {ok, E} <- Tables,
        C <- maps:get(cases, E),
        {P, T, _} <- element(3, C)
    ].
