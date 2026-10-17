%%% @doc B 组（v4 02-B）投递协议编排 + C 组（v4 02-C）人工接管闩锁触发与
%%% 投递前置门。自 `cs_ai_app` 按域拆出（文件行数规范收口，2026-10-15）；
%%% 函数体与语义未改，数据面经 `impl/1` 动态派发（注入面同构）。
-module(cs_ai_delivery_app).

-moduledoc "智能客服 AI 投递编排（B 组：租约投递协议 / C 组：闩锁触发与前置门）。".

-export([
    deliver/3,
    on_human_takeover/4,
    on_human_takeover/5,
    on_config_revoked/2,
    ai_delivery_allowed/4
]).

%% ===================================================================
%% B 组：投递协议编排
%% ===================================================================

%% @doc 投递一条已生成的 AI 回复（02-B.4 伪代码的编排面）。
%%
%% 事务结构：canonical 唯一写路径承载整个投递事务——L1 由方案甲的
%% `fetch_conversation FOR NO KEY UPDATE`（conversation 行锁，cs 会话↔
%% conversation 一对一）取得；deadline / input_version 复核、L4 delivery
%% 幂等 INSERT、job → completed CAS 全部在 `ai_delivery_hook` 内同事务
%% 执行（`cs_session_app:compose_ai_hook/2` 组合），任一失败 ⇒ 消息与
%% delivery 原子回滚。
%%
%% 返回值语义（02-B.4 冻结，调用方**不得**对 `{skipped, _}` 重试）：
%% `{ok, delivered}` | `{skipped, expired | superseded}` |
%% `{error, lease_lost | conflict | term()}`。
%%
%% Params：`job_id` / `worker_id` / `reply` 必填；`seat_identity_id`
%% 可选（缺省从 agent 配置取）；`gate_fun`（P0-5 D 组注入位）可选。
-spec deliver(integer(), integer(), map()) ->
    {ok, delivered} | {skipped, expired | superseded} | {error, term()}.
deliver(OrgId, WsId, #{job_id := JobId, worker_id := WorkerId} = Params) when
    is_integer(OrgId), is_integer(WsId), is_binary(WorkerId)
->
    Impl = cs_ai_port:impl(Params),
    case Impl:fetch_job(OrgId, JobId) of
        {ok, #{status := running, lease_owner := WorkerId, version := V} = Job} ->
            case deliver_preflight(Impl, OrgId, Job, Params) of
                {skipped, _} = Skip ->
                    Skip;
                {ok, SeatIdentity, ActorUser} ->
                    deliver_message(
                        OrgId, WsId, Impl, Job, WorkerId, V, SeatIdentity, ActorUser, Params
                    )
            end;
        {ok, #{status := running}} ->
            %% running 但 owner/version 失配 = 陈旧租约（I4：无任何副作用）。
            {error, lease_lost};
        {ok, #{status := Status}} ->
            {error, {conflict, Status}};
        {ok, _} ->
            {error, lease_lost};
        {error, Reason} ->
            {error, Reason}
    end;
deliver(_OrgId, _WsId, _Params) ->
    {error, {invalid_argument, deliver}}.

%% 快速预检：deadline 过 ⇒ {skipped, expired}；闩锁置位 ⇒ {skipped,
%% superseded}（C-3：接管后 AI 不再产生新投递）；会话已关闭 ⇒ {skipped,
%% superseded}（gate_7 前置快检——canonical 事务外同谓语提前求值，避免
%% 整条消息链组装后在 append 侧以 session_already_closed 报错；事务内
%% 复核仍由 delivery_hook 的 gate_fun 承担）；然后坐席身份与 actor 解析。
deliver_preflight(Impl, OrgId, Job, Params) ->
    case deadline_passed(Job) of
        true ->
            _ = Impl:mark_job_skipped(OrgId, maps:get(id, Job), expired),
            {skipped, expired};
        false ->
            case
                ai_delivery_allowed(
                    Impl, OrgId, maps:get(workspace_id, Job), maps:get(session_id, Job)
                )
            of
                {error, handoff_latched} ->
                    _ = Impl:mark_job_skipped(OrgId, maps:get(id, Job), superseded),
                    {skipped, superseded};
                ok ->
                    case Impl:session_open(OrgId, maps:get(session_id, Job)) of
                        {ok, true} ->
                            preflight_seat(Impl, OrgId, Params);
                        {ok, false} ->
                            _ = Impl:mark_job_skipped(
                                OrgId, maps:get(id, Job), superseded
                            ),
                            {skipped, superseded};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% seat 身份与 actor 解析（deadline/闩锁检查通过后）。
preflight_seat(Impl, OrgId, Params) ->
    case maps:get(seat_identity_id, Params, undefined) of
        Seat when is_integer(Seat) ->
            seat_actor(Impl, OrgId, Seat, Params);
        _ ->
            case seat_identity_from_config(Impl, OrgId) of
                {ok, Seat} -> seat_actor(Impl, OrgId, Seat, Params);
                {error, _} = Err -> Err
            end
    end.

%% actor 解析：Params.actor_user_id 显式注入优先（测试面），否则取
%% seat 身份归属用户（canonical 出站 actor 非空不变量；见
%% cs_pg_ai:identity_actor/2 注释——C 组「actor NULL」字面与既有
%% eb_message domain 不变量冲突的实现性偏差，P0-4 state 详述）。
seat_actor(Impl, OrgId, Seat, Params) ->
    case maps:get(actor_user_id, Params, undefined) of
        Actor when is_integer(Actor) ->
            {ok, Seat, Actor};
        _ ->
            case Impl:identity_actor(OrgId, Seat) of
                {ok, Actor} -> {ok, Seat, Actor};
                {error, Reason} -> {error, {ai_seat_actor_unavailable, Reason}}
            end
    end.

deadline_passed(#{deadline_at := Dl}) when is_integer(Dl), Dl > 0 ->
    os:system_time(second) >= Dl;
deadline_passed(_) ->
    true.

seat_identity_from_config(Impl, OrgId) ->
    case Impl:fetch_config(OrgId) of
        {ok, #{seat_identity_id := Seat}} when is_integer(Seat) -> {ok, Seat};
        {ok, _} -> {error, ai_seat_unconfigured};
        {error, Reason} -> {error, {ai_config_unavailable, Reason}}
    end.

deliver_message(
    OrgId, WsId, Impl, Job, WorkerId, ExpectedVersion, SeatIdentity, ActorUser, Params
) ->
    JobId = maps:get(id, Job),
    Hook = Impl:delivery_hook(OrgId, JobId, WorkerId, ExpectedVersion, #{
        seat_identity_id => SeatIdentity,
        gate_fun => maps:get(gate_fun, Params, undefined)
    }),
    MsgParams = #{
        workspace_id => WsId,
        conversation_id => maps:get(conversation_id, Job),
        sender_type => <<"business_identity">>,
        identity_id => SeatIdentity,
        actor_user_id => ActorUser,
        client_msg_id => reply_key(JobId),
        body => maps:get(reply, Params, <<>>),
        ai_delivery_hook => Hook
    },
    case cs_session_app:append_conversation_message(OrgId, MsgParams, ai) of
        {ok, _} ->
            {ok, delivered};
        %% B.4 冻结映射：expired/superseded/stale_citation ⇒ job 终态
        %% （dead/cancelled/cancelled），no_grounding ⇒ failed（I9）；
        %% last_error_code 细分见 cs_pg_ai:mark_job_skipped/3。
        {error, {skipped, Reason}} when
            Reason =:= expired; Reason =:= superseded; Reason =:= stale_citation
        ->
            _ = Impl:mark_job_skipped(OrgId, JobId, Reason),
            {skipped, Reason};
        {error, {skipped, no_grounding}} ->
            _ = Impl:mark_job_skipped(OrgId, JobId, no_grounding),
            {skipped, no_grounding};
        {error, {skipped, Other}} ->
            {error, Other};
        {error, Reason} ->
            {error, Reason}
    end.

%% 投递幂等键（02-B.4 `reply_key(j)`）：跨重试稳定，一条逻辑 job 一条
%% 逻辑回复（与 uq_csaid_reply 同键集）。
reply_key(JobId) ->
    <<"ai-job-", (integer_to_binary(JobId))/binary>>.

%% ===================================================================
%% C 组：人工接管闩锁触发 + 投递前置门
%% ===================================================================

%% @doc E2 触发一（人工 claim / transfer 至 human seat）：置粘性闩锁 +
%% generation_epoch+1。**永不抛错**——人工坐席路径绝不被 AI 侧故障拖垮
%% （与 respond/2 的失败哲学同构），失败只留 error 日志。
-spec on_human_takeover(integer(), integer(), integer(), binary()) -> ok.
on_human_takeover(OrgId, WsId, SessionId, Reason) ->
    on_human_takeover(OrgId, WsId, SessionId, Reason, #{}).

-spec on_human_takeover(integer(), integer(), integer(), binary(), map()) -> ok.
on_human_takeover(OrgId, WsId, SessionId, Reason, Params) when
    is_integer(OrgId), is_integer(WsId), is_integer(SessionId)
->
    Impl = cs_ai_port:impl(Params),
    try
        case Impl:latch_handoff(OrgId, WsId, SessionId, Reason) of
            {ok, _} ->
                ok;
            {error, Reason0} ->
                logger:warning(
                    "cs_ai_latch_failed ~p org=~p ws=~p session=~p",
                    [Reason0, OrgId, WsId, SessionId]
                ),
                ok
        end
    catch
        _Class:_Reason -> ok
    end;
on_human_takeover(_OrgId, _WsId, _SessionId, _Reason, _Params) ->
    ok.

%% @doc E2 触发二（auto_reply_mode / canary 撤销）：org 内全部未闩锁的
%% AI 会话置位。同样永不抛错（管理面写路径的附带动作）。
-spec on_config_revoked(integer(), binary()) -> ok.
on_config_revoked(OrgId, Reason) when is_integer(OrgId) ->
    Impl = cs_ai_port:impl(#{}),
    try
        case Impl:latch_org_pending(OrgId, Reason) of
            {ok, _N} ->
                ok;
            {error, Reason0} ->
                logger:warning("cs_ai_latch_all_failed ~p org=~p", [Reason0, OrgId]),
                ok
        end
    catch
        _Class:_Reason -> ok
    end;
on_config_revoked(_OrgId, _Reason) ->
    ok.

%% @doc C 组投递前置门（C-3）：闩锁置位 ⇒ AI 不再产生新投递。
%% 粘性 ⇒ 无解除 API（E3 清除仅发生在 closed 后新会话 = 新 ai_session 行）；
%% C-4 的「尝试清除被拒」由该函数无清除入口 + 02-C.3 冻结边集共同保证。
-spec ai_delivery_allowed(module(), integer(), integer(), integer()) ->
    ok | {error, handoff_latched}.
ai_delivery_allowed(Impl, OrgId, WsId, SessionId) ->
    case Impl:fetch_session_state(OrgId, WsId, SessionId) of
        {ok, #{handoff := true}} -> {error, handoff_latched};
        _ -> ok
    end.
