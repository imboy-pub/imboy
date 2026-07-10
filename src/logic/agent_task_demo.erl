-module(agent_task_demo).

%%%
% Agent 任务生命周期 **PoC 演示驱动** / demo driver（Phase 4 T4.2）。
%
% 背景：agent_task_observer:emit/1 是可观测前端（imboyapp 群内任务卡片 + 进度条）的
% 唯一数据源，但目前**没有任何真实调用方**——真正的驱动应是外部 A2A/agent 任务桥
% （task bridge），把 agent 侧的 working/submitted/progress/completed/... 状态翻译成
% emit 事件。该桥尚未落地。
%
% 本模块就是那个缺口的**临时替身**：手工产生一段最小任务生命周期，让前端在无真实
% agent 的情况下也有卡片/进度可渲染，用于端到端联调与演示。生产环境请由真实 task
% bridge 取代本模块，勿依赖其行为契约。
%
% ⚠️ E2EE 红线：本 demo 恒以 e2ee=>false 投递（演示只应在非 E2EE 群触发）。emit 层
%    fail-closed，缺省即漏投；这里显式置 false 让演示可见。真实桥须按触发群的 E2EE
%    状态显式置该字段，绝不对 E2EE 群投递服务端观察事件。
%%%

-export([run_demo/3]).

-include("log.hrl").

%% @doc 产生一段演示任务生命周期（fire-and-forget，恒容错返回 ok）：
%%   1) working          —— 过渡态，ephemeral 扇出给在线成员（携 member_uids）
%%   2) awaiting_approval —— durable 审批卡片，让人工看到"批准/拒绝"卡片
%% 用一个全新 task_id 串起两个事件（emit 内部据 task_id 登记待审）。
-spec run_demo(integer(), integer(), [integer()]) -> ok.
run_demo(AgentUid, GroupId, MemberUids) ->
    try
        TaskId = ec_cnv:to_binary(elib_tsid:generate()),
        Base = #{
            task_id => TaskId,
            agent_uid => AgentUid,
            group_id => GroupId,
            member_uids => MemberUids,
            %% E2EE 红线：演示恒非 E2EE 群
            e2ee => false
        },
        agent_task_observer:emit(Base#{
            status => working,
            text => <<"🔧 [演示] Agent 正在执行任务…"/utf8>>
        }),
        agent_task_observer:emit(Base#{
            status => awaiting_approval,
            text => <<"⏳ [演示] 任务需要人工审批：请群内有权成员批准或拒绝"/utf8>>
        }),
        ok
    catch
        Class:Reason ->
            ok = ?ERROR_LOG("[AGENT_TASK_DEMO] ~p:~p~n", [Class, Reason]),
            ok
    end.
