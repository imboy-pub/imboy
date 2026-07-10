-module(agent_task_observer).

%%%
% IM 作为 A2A/agent 任务协作的可观测前端 / IM as observable frontend for agent-task
% collaboration（Phase 4 T4.2 剩余部分，前瞻 PoC，不追生产化）。
%
% spike 裁定：B 路 + 轻混合——自建几十行事件契约，**仅借 barrel_mcp_tasks 的 MCP 任务
% 词汇**（working/completed/failed/cancelled），不引入完整 A2A/AGUI SDK，不新造状态机。
% awaiting_approval 是本层为「群内审批」扩展的事件类型（barrel_mcp_tasks 不含）。
%
% 事件 → 群消息映射（③ 可靠性档位内建于此映射）：
%   - 过渡态 working/submitted/progress → **ephemeral 扇出**（imboy_syn:publish 直推在线
%     成员，不落库、断线丢失——观察流本质是实时旁观）。
%   - 终态 completed/failed/cancelled  → **可靠群消息**（msg_c2g_logic:c2g，staging 落库 +
%     离线补拉）。
%   - awaiting_approval               → **可靠审批卡片**（同上可靠通路）+ 登记待审。
%
% 审批仲裁（②「群内任一有权成员抢先批准，其余幂等 no-op」）：
%   照 barrel_mcp_tasks:transition/5 的「幂等终态」范式，但用 **ets:insert_new 原子占位**
%   实现 first-writer-wins（DB 侧 mcp_governance 无 CAS 守卫，不可照抄）。
%
% ⚠️ E2EE 红线（fail-closed）：E2EE 群绝不投递服务端 AI 观察/卡片。无群级 E2EE 权威源
%    （imboy E2EE 逐消息），故 emit **仅当调用方显式传 e2ee=false 才投递**；e2ee=true 或
%    缺省(未声明=未知)一律跳过——忘记传应"漏投"而非"错投进 E2EE 群"。调用方须按触发
%    消息/群 E2EE 状态显式置该字段（复用 ai_agent_group_reply 同款红线判据）。
%
% fan-out 用 imboy_syn:publish + 事件携带的成员列表 N×单播（勿碰 ?ROOM_SCOPE 空壳）。
%%%

-export([emit/1, approve/2, reject/2]).
%% 供单测：读待审/决定状态
-export([lookup/1]).

-include("log.hrl").

-define(TAB, agent_task_approval_ets).

%% @doc 投递一个 agent 任务事件到群。Event：
%%   #{task_id := binary(), agent_uid := integer(), group_id := integer(),
%%     status := atom()|binary(), member_uids => [integer()],
%%     text => binary(), e2ee => boolean()}
%% 恒容错返回 ok（任何异常不得拖垮上游任务执行）。
-spec emit(map()) -> ok.
emit(Event) ->
    try
        do_emit(Event)
    catch
        Class:Reason ->
            ok = ?ERROR_LOG("[AGENT_TASK_OBSERVER] ~p:~p~n", [Class, Reason]),
            ok
    end.

%% @doc 审批：群内有权成员批准某待审任务。first-writer-wins，其余幂等 no-op。
%% ⚠️ 契约：ApproverUid **必须**由调用方（handler）从已认证会话(JWT/session)派生，
%%    禁止从客户端请求体直接透传——本层只做授权(是否群成员/非本人)，不做认证。
-spec approve(binary(), integer()) ->
    {ok, approved} | {error, not_authorized | already_decided | not_found | internal_error}.
approve(TaskId, ApproverUid) ->
    decide(TaskId, ApproverUid, approved).

%% @doc 拒绝：语义同 approve，落 rejected 终态。ApproverUid 认证契约同 approve/2。
-spec reject(binary(), integer()) ->
    {ok, rejected} | {error, not_authorized | already_decided | not_found | internal_error}.
reject(TaskId, ApproverUid) ->
    decide(TaskId, ApproverUid, rejected).

%% @doc 读审批记录（测试/排障用）：{pending, Gid, Agent} | {Decision, ApproverUid} | undefined
-spec lookup(binary()) -> {pending, integer(), integer()} | {atom(), integer()} | undefined.
lookup(TaskId) ->
    ok = ensure_ready(),
    case ets:lookup(?TAB, {decision, TaskId}) of
        [{_, Decision, ApproverUid}] ->
            {Decision, ApproverUid};
        [] ->
            case ets:lookup(?TAB, {pending, TaskId}) of
                [{_, Gid, Agent}] -> {pending, Gid, Agent};
                [] -> undefined
            end
    end.

%% ===================================================================
%% Internal — 事件路由
%% ===================================================================

do_emit(Event) ->
    case emittable(Event) of
        false ->
            %% E2EE 红线 fail-closed：仅当调用方**显式** e2ee=false 才投递；
            %% e2ee=true 或缺省(未声明=未知)一律跳过——忘记传 e2ee 应"漏投"而非"错投进 E2EE 群"。
            ok;
        true ->
            route(norm_status(maps:get(status, Event)), Event)
    end.

%% 过渡态 → ephemeral；终态 → 可靠群消息；awaiting_approval → 卡片 + 登记
route(working, Event) -> ephemeral(Event);
route(submitted, Event) -> ephemeral(Event);
route(progress, Event) -> ephemeral(Event);
route(completed, Event) -> durable_terminal(Event, completed);
route(failed, Event) -> durable_terminal(Event, failed);
route(cancelled, Event) -> durable_terminal(Event, cancelled);
route(awaiting_approval, Event) -> approval_card(Event);
route(_Unknown, _Event) -> ok.

%% 过渡态：ephemeral 扇出给在线成员（不落库）
ephemeral(Event) ->
    #{task_id := TaskId, agent_uid := AgentUid, group_id := Gid} = Event,
    Members = maps:get(member_uids, Event, []),
    Frame = #{
        <<"id">> => TaskId,
        <<"type">> => <<"C2G">>,
        <<"from">> => ec_cnv:to_binary(AgentUid),
        <<"to">> => ec_cnv:to_binary(Gid),
        <<"msg_type">> => <<"agent_task">>,
        <<"payload">> => #{
            <<"task_id">> => TaskId,
            <<"status">> => status_bin(Event),
            <<"text">> => status_text(Event)
        },
        <<"created_at">> => elib_dt:millisecond()
    },
    Json = jsone:encode(Frame, [native_utf8]),
    Online = [U || U <- Members, user_logic:is_online(U)],
    lists:foreach(fun(U) -> imboy_syn:publish(U, Json) end, Online),
    ok.

%% 终态：可靠群消息（落库 + 离线补拉）+ 回收待审 ETS（防"审批中被取消"永久残留）
durable_terminal(Event, Status) ->
    #{task_id := TaskId, agent_uid := AgentUid, group_id := Gid} = Event,
    _ = cleanup_pending(TaskId),
    durable_group_message(Gid, AgentUid, status_text(Event), #{
        <<"task_id">> => TaskId,
        <<"status">> => atom_to_binary(Status, utf8)
    }),
    ok.

%% 回收 {pending, TaskId}（decision 记录保留作审计）。表未建则无事可做。
cleanup_pending(TaskId) ->
    case ets:whereis(?TAB) of
        undefined ->
            ok;
        _ ->
            _ = ets:delete(?TAB, {pending, TaskId}),
            ok
    end.

%% awaiting_approval：原子登记待审（insert_new 防重复卡片）+ 可靠审批卡片
approval_card(Event) ->
    #{task_id := TaskId, agent_uid := AgentUid, group_id := Gid} = Event,
    ok = ensure_ready(),
    case ets:insert_new(?TAB, {{pending, TaskId}, Gid, AgentUid}) of
        true ->
            durable_group_message(Gid, AgentUid, status_text(Event), #{
                <<"task_id">> => TaskId,
                <<"status">> => <<"awaiting_approval">>,
                <<"actions">> => [<<"approve">>, <<"reject">>]
            }),
            ok;
        false ->
            %% 已登记（重发）→ 不重复投递卡片
            ok
    end.

%% ===================================================================
%% Internal — 审批仲裁
%% ===================================================================

decide(TaskId, ApproverUid, Decision) ->
    try
        do_decide(TaskId, ApproverUid, Decision)
    catch
        Class:Reason ->
            ok = ?ERROR_LOG("[AGENT_TASK_DECIDE] task=~p ~p:~p~n", [TaskId, Class, Reason]),
            {error, internal_error}
    end.

do_decide(TaskId, ApproverUid, Decision) ->
    ok = ensure_ready(),
    case ets:lookup(?TAB, {pending, TaskId}) of
        [{_, GroupId, AgentUid}] ->
            %% 审批人须是群成员，且**不得是任务所属 agent 本人**（防 agent 自我审批架空人工闸门）
            case ApproverUid =/= AgentUid andalso authorized(ApproverUid, GroupId) of
                false ->
                    {error, not_authorized};
                true ->
                    arbitrate(TaskId, ApproverUid, Decision, GroupId, AgentUid)
            end;
        [] ->
            {error, not_found}
    end.

%% 原子 first-writer-wins：insert_new 只有第一个决定者成功，其余 false=已决定。
arbitrate(TaskId, ApproverUid, Decision, GroupId, AgentUid) ->
    case ets:insert_new(?TAB, {{decision, TaskId}, Decision, ApproverUid}) of
        true ->
            durable_group_message(
                GroupId,
                AgentUid,
                decision_text(Decision, ApproverUid),
                #{
                    <<"task_id">> => TaskId,
                    <<"status">> => atom_to_binary(Decision, utf8),
                    <<"decided_by">> => ApproverUid
                }
            ),
            {ok, Decision};
        false ->
            {error, already_decided}
    end.

%% 有权审批 = 群成员（每次查 group_ds，不信事件携带的成员列表）
authorized(Uid, GroupId) ->
    lists:member(Uid, group_ds:member_uids(GroupId)).

%% ===================================================================
%% Internal — 投递 / 文本 / ETS
%% ===================================================================

%% 可靠群消息：msg_c2g_logic:c2g（agent 一等成员身份，复用 QoS/staging/离线补拉）。
%% 结构化 agent_task 元数据挂在 payload，前端据此渲染可点击卡片；content 为降级文本。
durable_group_message(GroupId, AgentUid, Text, TaskMeta) ->
    MsgId = ec_cnv:to_binary(elib_tsid:generate()),
    Content = elib_str:replace_single_quote(Text),
    Data = #{
        <<"to">> => ec_cnv:to_binary(GroupId),
        <<"msg_type">> => <<"text">>,
        <<"payload">> => #{
            <<"content">> => Content,
            <<"text">> => Content,
            <<"agent_task">> => TaskMeta
        },
        <<"created_at">> => elib_dt:millisecond()
    },
    _ = msg_c2g_logic:c2g(MsgId, AgentUid, Data),
    ok.

%% E2EE 红线闸门（fail-closed）：仅当调用方**显式**声明 e2ee=false 才放行。
%% 无群级 E2EE 权威源（imboy E2EE 是逐消息的），故契约要求调用方按触发消息/群
%% E2EE 状态显式传 e2ee；缺省(未声明)视为未知→不投递。
emittable(Event) ->
    maps:get(e2ee, Event, undefined) =:= false.

%% 状态归一：atom 直接用（内部可信调用）；binary 走**白名单**映射。
%% ⚠️ 绝不 binary_to_atom(外部输入)——高基数 status 会耗尽节点原子表(VM 崩溃级 DoS)。
norm_status(S) when is_atom(S) -> S;
norm_status(<<"working">>) -> working;
norm_status(<<"submitted">>) -> submitted;
norm_status(<<"progress">>) -> progress;
norm_status(<<"completed">>) -> completed;
norm_status(<<"failed">>) -> failed;
norm_status(<<"cancelled">>) -> cancelled;
norm_status(<<"awaiting_approval">>) -> awaiting_approval;
norm_status(_) -> unknown.

status_bin(Event) ->
    case norm_status(maps:get(status, Event)) of
        S when is_atom(S) -> atom_to_binary(S, utf8);
        _ -> <<"unknown">>
    end.

%% 事件文本：优先事件自带 text，否则按状态给默认文案
status_text(Event) ->
    case maps:get(text, Event, <<>>) of
        T when is_binary(T), T =/= <<>> -> T;
        _ -> default_text(norm_status(maps:get(status, Event)))
    end.

default_text(working) -> <<"🔧 正在执行任务…"/utf8>>;
default_text(submitted) -> <<"📥 任务已提交"/utf8>>;
default_text(progress) -> <<"⏳ 任务进行中…"/utf8>>;
default_text(completed) -> <<"✅ 任务完成"/utf8>>;
default_text(failed) -> <<"❌ 任务失败"/utf8>>;
default_text(cancelled) -> <<"⏹️ 任务已取消"/utf8>>;
default_text(awaiting_approval) -> <<"⏳ 待审批：请群内有权成员批准或拒绝"/utf8>>;
default_text(_) -> <<"任务状态更新"/utf8>>.

decision_text(approved, Uid) ->
    iolist_to_binary([<<"✅ 审批通过（by "/utf8>>, ec_cnv:to_binary(Uid), <<"）"/utf8>>]);
decision_text(rejected, Uid) ->
    iolist_to_binary([<<"🚫 审批拒绝（by "/utf8>>, ec_cnv:to_binary(Uid), <<"）"/utf8>>]).

%% 惰性建表（免 gen_server/sup 接线，同 agent_rate_limiter 思路）
%% ponytail: PoC 无 TTL 清扫，审批记录常驻 ETS；生产化再加 sweep 或落库。
ensure_ready() ->
    case ets:whereis(?TAB) of
        undefined ->
            try
                _ = ets:new(?TAB, [
                    set, public, named_table, {write_concurrency, true}, {read_concurrency, true}
                ]),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.
