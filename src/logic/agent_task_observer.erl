-module(agent_task_observer).

%%%
% IM 作为 A2A/agent 任务协作的可观测前端 / IM as observable frontend for agent-task
% collaboration。
%
% DATA-01 起：登记与审批仲裁的真源由 ETS 迁移到数据库（agent_task_repo/logic/ds，
% migration 00000090）。本模块保留**投递语义**与容错外壳：
%
% 事件 → 群消息映射（可靠性档位）：
%   - 过渡态 working/submitted/progress → **ephemeral 扇出**（imboy_syn:publish 直推在线
%     成员，不落库、断线丢失——观察流本质是实时旁观）。
%   - 终态 completed/failed/cancelled/expired → **可靠群消息**（msg_c2g_logic:c2g，
%     staging 落库 + 离线补拉）。
%   - awaiting_approval               → **可靠审批卡片**（同上可靠通路）+ DB 登记待审。
%
% 幂等/仲裁全部在 agent_task_logic（DB 原子：事件唯一键去重 + decision 唯一约束），
% 重复 durable 事件/重复决定在本层拿不到投递指令，天然不重复投递。
%
% ⚠️ E2EE 红线（fail-closed）：E2EE 群绝不投递服务端 AI 观察/卡片。emit **仅当调用方
%    显式传 e2ee=false 才投递**；e2ee=true 或缺省(未声明=未知)一律跳过——忘记传应
%    "漏投"而非"错投进 E2EE 群"。
%%%

-export([emit/1, approve/2, reject/2]).
%% 供单测：读待审/决定状态
-export([lookup/1]).

-include("log.hrl").

%% @doc 投递一个 agent 任务事件到群。Event：
%%   #{task_id := binary(), agent_uid := integer(), group_id := integer(),
%%     status := atom()|binary(), member_uids => [integer()],
%%     text => binary(), e2ee => boolean(), tool => binary(), params_digest => binary()}
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

%% @doc 读审批记录（测试/排障用；真源=DB，重启可恢复）：
%%   {pending, Gid, Agent} | {Decision, ApproverUid} | {live_status, StatusBin} | undefined
-spec lookup(binary()) ->
    {pending, integer(), integer()}
    | {atom(), integer()}
    | {live_status, binary()}
    | undefined.
lookup(TaskId) ->
    agent_task_logic:lookup(TaskId).

%% ===================================================================
%% Internal — 事件路由（持久化判定 + 投递指令消费）
%% ===================================================================

do_emit(Event) ->
    case emittable(Event) of
        false ->
            %% E2EE 红线 fail-closed：仅当调用方**显式** e2ee=false 才投递。
            ok;
        true ->
            case agent_task_logic:record_event(Event) of
                skip ->
                    ok;
                {deliver, StatusBin} ->
                    deliver(StatusBin, Event, false);
                {deliver_with_meta, StatusBin, Meta} ->
                    deliver(StatusBin, Event, Meta)
            end
    end.

%% 过渡态 → ephemeral；终态 → 可靠群消息；awaiting_approval → 卡片
deliver(<<"working">>, Event, _Meta) -> ephemeral(Event);
deliver(<<"submitted">>, Event, _Meta) -> ephemeral(Event);
deliver(<<"completed">>, Event, Meta) -> durable_terminal(Event, <<"completed">>, Meta);
deliver(<<"failed">>, Event, Meta) -> durable_terminal(Event, <<"failed">>, Meta);
deliver(<<"cancelled">>, Event, Meta) -> durable_terminal(Event, <<"cancelled">>, Meta);
deliver(<<"expired">>, Event, Meta) -> durable_terminal(Event, <<"expired">>, Meta);
deliver(<<"awaiting_approval">>, Event, Meta) -> approval_card(Event, Meta);
deliver(_Other, _Event, _Meta) -> ok.

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

%% 终态：可靠群消息（落库 + 离线补拉）。重复终态事件在持久层已被去重，不会到达此处。
durable_terminal(Event, StatusBin, Meta) ->
    #{task_id := TaskId} = Event,
    {AgentUid, Gid} = actor_and_group(Event, Meta),
    durable_group_message(Gid, AgentUid, status_text(Event), #{
        <<"task_id">> => TaskId,
        <<"status">> => StatusBin
    }),
    ok.

%% awaiting_approval：可靠审批卡片（登记/去重已在持久层完成）
approval_card(Event, Meta) ->
    #{task_id := TaskId} = Event,
    {AgentUid, Gid} = actor_and_group(Event, Meta),
    durable_group_message(Gid, AgentUid, status_text(Event), #{
        <<"task_id">> => TaskId,
        <<"status">> => <<"awaiting_approval">>,
        <<"actions">> => [<<"approve">>, <<"reject">>]
    }),
    ok.

%% Meta（持久层回带的任务行字段）优先，事件自带值兜底（兼容直发事件路径）
actor_and_group(Event, Meta) when is_map(Meta), map_size(Meta) > 0 ->
    AgentUid = to_int(maps:get(<<"agent_uid">>, Meta, maps:get(agent_uid, Event, 0))),
    Gid = to_int(maps:get(<<"group_id">>, Meta, maps:get(group_id, Event, 0))),
    {AgentUid, Gid};
actor_and_group(Event, _) ->
    {maps:get(agent_uid, Event, 0), maps:get(group_id, Event, 0)}.

%% ===================================================================
%% Internal — 审批仲裁（委托 agent_task_logic，本层负责结果投递）
%% ===================================================================

decide(TaskId, ApproverUid, Decision) ->
    try
        case agent_task_logic:decide(TaskId, ApproverUid, Decision) of
            {ok, Decision2, Meta} ->
                {AgentUid, Gid} = actor_and_group(#{}, Meta),
                durable_group_message(
                    Gid,
                    AgentUid,
                    decision_text(Decision2, ApproverUid),
                    #{
                        <<"task_id">> => TaskId,
                        <<"status">> => atom_to_binary(Decision2, utf8),
                        <<"decided_by">> => ApproverUid
                    }
                ),
                {ok, Decision2};
            {error, DecideError} ->
                {error, DecideError}
        end
    catch
        Class:CatchReason ->
            ok = ?ERROR_LOG("[AGENT_TASK_DECIDE] task=~p ~p:~p~n", [TaskId, Class, CatchReason]),
            {error, internal_error}
    end.

%% ===================================================================
%% Internal — 投递 / 文本
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

status_bin(Event) ->
    S = maps:get(status, Event, <<>>),
    if
        is_atom(S) -> atom_to_binary(S, utf8);
        is_binary(S) -> S;
        true -> <<"unknown">>
    end.

%% 事件文本：优先事件自带 text，否则按状态给默认文案
status_text(Event) ->
    case maps:get(text, Event, <<>>) of
        T when is_binary(T), T =/= <<>> -> T;
        _ -> default_text(status_bin(Event))
    end.

default_text(<<"working">>) -> <<"🔧 正在执行任务…"/utf8>>;
default_text(<<"submitted">>) -> <<"📥 任务已提交"/utf8>>;
default_text(<<"progress">>) -> <<"⏳ 任务进行中…"/utf8>>;
default_text(<<"completed">>) -> <<"✅ 任务完成"/utf8>>;
default_text(<<"failed">>) -> <<"❌ 任务失败"/utf8>>;
default_text(<<"cancelled">>) -> <<"⏹️ 任务已取消"/utf8>>;
default_text(<<"expired">>) -> <<"⏰ 任务已过期"/utf8>>;
default_text(<<"awaiting_approval">>) -> <<"⏳ 待审批：请群内有权成员批准或拒绝"/utf8>>;
default_text(_) -> <<"任务状态更新"/utf8>>.

decision_text(approved, Uid) ->
    iolist_to_binary([<<"✅ 审批通过（by "/utf8>>, ec_cnv:to_binary(Uid), <<"）"/utf8>>]);
decision_text(rejected, Uid) ->
    iolist_to_binary([<<"🚫 审批拒绝（by "/utf8>>, ec_cnv:to_binary(Uid), <<"）"/utf8>>]).

to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    try binary_to_integer(V) of
        I -> I
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.
