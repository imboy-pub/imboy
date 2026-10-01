%%% @doc 客服会话（queued/active/closed）的应用层用例。
%%%
%%% 依据：plan v4.1 §4.2、§5.2、CS-01-A01..A05、EB-D03/EB-D05/EB-D08。
%%%
%%% 职责边界：
%%%   * 状态机的**语义唯一真源**是 domain `cs_session`；本模块做参数收敛 →
%%%     domain 判定 → 经 `cs_store_port` 的 CAS 用例写库（状态推进与审计事件
%%%     同事务，恰好一次）。
%%%   * claim 的**并发裁决点**在 DB（`claim_session/7` 的 seat 行锁内 CAS）；
%%%     `cs_dispatch` 是同一容量判定的 domain 真源（A02）。
%%%   * **消息真源是 enterprise**（A03 / EB-D08）：`append_session_message/2`
%%%     只把客服会话上下文映射进 `enterprise_business_facade:append_message/2`
%%%     的参数面——本 feature 不建消息副本、不直接引用 enterprise 内层模块。
%%%   * session 绑定 `business_identity_id`（不是 user_id）：transfer/rebind
%%%     只换 identity，主体字段零迁移；每次改绑后用 domain
%%%     `assert_rebind_continuity/2` 自检（A04）。
%%%
%%% 授权（seat actor / tenant admin / cs_visit）由 CS-02 的认证分流判定；
%%% 本模块只判业务前提（会话归属、状态、坐席绑定）。
-module(cs_session_app).

-include("generated/imboy_product_features.hrl").

-export([
    open_session/2,
    fetch_session/2,
    claim/2,
    transfer/2,
    close/2,
    rate/2,
    append_session_message/2,
    append_conversation_message/2,
    list_contact_sessions/2,
    list_sessions/2,
    seat_session_page/2,
    %% CS-BE-07：按需统计
    session_stats/2
]).

%% C1（contracts-w2）投影白名单：**逐字**；visit_token_id / close_reason /
%% 任何 digest/secret/cipher 永不进响应（page_view 唯一出口裁剪）。
-define(SESSION_LIST_PROJECTION, [
    id,
    organization_id,
    workspace_id,
    contact_id,
    business_identity_id,
    status,
    rating,
    queued_at,
    claimed_at,
    closed_at,
    version
]).

%% ===================================================================
%% 开会话（queued）
%% ===================================================================

%% @doc 为 (Org, workspace, contact, conversation) 建立排队会话。
%% 同一会话上已有未关闭客服 session 时 conflict（DB 部分唯一索引裁决）。
%%
%% Params：workspace_id / contact_id / conversation_id 必填；visit_token_id、
%% created_by_user_id、at 可选；store / id 可注入。
-spec open_session(integer(), map()) -> {ok, map()} | {error, term()}.
open_session(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            open_session_in(OrgId, WorkspaceId, Params)
    end;
open_session(_OrgId, _Params) ->
    {error, {invalid_argument, open_session}}.

open_session_in(OrgId, WorkspaceId, Params) ->
    %% ORG-08（C16 / compatibility §4.1）：archived Org 拒绝**新** Session
    %% （稳定 denial：{error, organization_archived}）；授权只读不受影响。
    case cs_org_lifecycle_gate:assert_session_writable(OrgId, Params) of
        {error, _} = Err ->
            Err;
        ok ->
            open_session_gated(OrgId, WorkspaceId, Params)
    end.

open_session_gated(OrgId, WorkspaceId, Params) ->
    ContactId = maps:get(contact_id, Params, undefined),
    ConversationId = maps:get(conversation_id, Params, undefined),
    At = maps:get(at, Params, undefined),
    case pos_int(ContactId) andalso pos_int(ConversationId) of
        false ->
            {error, {invalid_argument, open_session}};
        true ->
            insert_session(OrgId, WorkspaceId, ContactId, ConversationId, At, Params)
    end.

insert_session(OrgId, WorkspaceId, ContactId, ConversationId, At, Params) ->
    case cs_app_support:new_id(cs_session, Params) of
        {error, _} = Err ->
            Err;
        {ok, SessionId} ->
            Draft = #{
                id => SessionId,
                organization_id => OrgId,
                workspace_id => WorkspaceId,
                contact_id => ContactId,
                conversation_id => ConversationId,
                visit_token_id => maps:get(visit_token_id, Params, undefined),
                queued_at => At,
                created_by_user_id => maps:get(created_by_user_id, Params, undefined)
            },
            Event = #{
                session_id => SessionId,
                actor_user_id => maps:get(created_by_user_id, Params, undefined),
                actor_kind => <<"visitor">>,
                action => <<"session.opened">>,
                detail => #{<<"contact_id">> => ContactId},
                workspace_id => WorkspaceId
            },
            with_store(Params, fun(Store) ->
                Store:insert_session(OrgId, WorkspaceId, Draft, Event)
            end)
    end.

%% ===================================================================
%% 读取
%% ===================================================================

%% @doc 会话详情（租户作用域由 store 同语句裁决；跨 Org 一律 not_found）。
-spec fetch_session(integer(), map()) -> {ok, map()} | {error, term()}.
fetch_session(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            SessionId = maps:get(session_id, Params, undefined),
            fetch_by(Params, OrgId, WorkspaceId, SessionId)
    end;
fetch_session(_OrgId, _Params) ->
    {error, {invalid_argument, fetch_session}}.

fetch_by(Params, OrgId, WorkspaceId, SessionId) ->
    case pos_int(SessionId) of
        false ->
            {error, {invalid_session_id, SessionId}};
        true ->
            with_store(Params, fun(Store) ->
                Store:fetch_session(OrgId, WorkspaceId, SessionId)
            end)
    end.

%% @doc 访客视角：只列**自己的** (Org, contact) 会话（A05 读取边界；
%% 访客 token 绑定的 contact 之外一律空列表，与 store 同语句过滤一致）。
-spec list_contact_sessions(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_contact_sessions(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            ContactId = maps:get(contact_id, Params, undefined),
            case pos_int(ContactId) of
                false ->
                    {error, {invalid_contact_id, ContactId}};
                true ->
                    with_store(Params, fun(Store) ->
                        Store:list_sessions_for_contact(OrgId, WorkspaceId, ContactId)
                    end)
            end
    end;
list_contact_sessions(_OrgId, _Params) ->
    {error, {invalid_argument, list_contact_sessions}}.

%% @doc C1（contracts-w2）平台 session 列表（只读；租户/平台共用同一用例）。
%%
%% Params：workspace_id 必填；status 白名单 queued|active|closed（非法
%% `{invalid_status,_}` 422）；after_id TSID（非法 `{invalid_after_id,_}` 422）；
%% limit 1..200 缺省 50（越界 `{invalid_limit,_}` 422）。
%%
%% 读取是键集下推（store 同语句 `id > after ORDER BY id DESC LIMIT n`，
%% OrgId+WorkspaceId 前两个业务参数）；投影白名单由 `cs_app_support:page_view`
%% 唯一出口裁剪——visit_token_id / close_reason / digest 绝不出本用例。
-spec list_sessions(integer(), map()) -> {ok, map()} | {error, term()}.
list_sessions(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case cs_app_support:page_cursor(Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, AfterId, Limit} ->
                    case cs_app_support:session_status(maps:get(status, Params, undefined)) of
                        {error, _} = Err3 ->
                            Err3;
                        {ok, Status} ->
                            list_sessions_page(OrgId, WorkspaceId, Status, AfterId, Limit, Params)
                    end
            end
    end;
list_sessions(_OrgId, _Params) ->
    {error, {invalid_argument, list_sessions}}.

list_sessions_page(OrgId, WorkspaceId, Status, AfterId, Limit, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:list_sessions_page(OrgId, WorkspaceId, Status, AfterId, Limit)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            cs_app_support:page_view(sessions, ?SESSION_LIST_PROJECTION, Rows, Limit, id)
    end.

%% ===================================================================
%% claim（A02：并发恰好一个成功，且不超 max_concurrent）
%% ===================================================================

%% @doc 接单：把 queued 会话推进为 active 并绑定坐席 identity。
%%
%% Params：workspace_id / session_id / expected_version / at 必填；
%% `business_identity_id` 可选——给出则显式 claim 该坐席（坐席停用即拒），
%% 缺省则经 `list_dispatchable_seats` + `cs_dispatch:select_seat` 做 least-active 派单。
%%
%% 并发裁决：最终以 `cs_store_port:claim_session/7` 的 DB CAS 为准；
%% domain `cs_session:assert_cas_expectation/3` 是进入 DB 前的同一判定。
-spec claim(integer(), map()) -> {ok, map()} | {error, term()}.
claim(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            claim_in(OrgId, WorkspaceId, Params)
    end;
claim(_OrgId, _Params) ->
    {error, {invalid_argument, claim}}.

claim_in(OrgId, WorkspaceId, Params) ->
    %% ORG-08（C16 / compatibility §4.1）：archived Org 拒绝**新** Seat claim
    %% （稳定 denial：{error, organization_archived}）；既有 Session 历史保留。
    case cs_org_lifecycle_gate:assert_session_writable(OrgId, Params) of
        {error, _} = Err ->
            Err;
        ok ->
            claim_gated(OrgId, WorkspaceId, Params)
    end.

claim_gated(OrgId, WorkspaceId, Params) ->
    SessionId = maps:get(session_id, Params, undefined),
    ExpectedVersion = maps:get(expected_version, Params, undefined),
    At = maps:get(at, Params, undefined),
    case pos_int(SessionId) andalso pos_int(ExpectedVersion) andalso pos_int(At) of
        false ->
            {error, {invalid_argument, claim}};
        true ->
            case fetch_session_for(Params, OrgId, WorkspaceId, SessionId) of
                {error, _} = Err ->
                    Err;
                {ok, Session} ->
                    claim_session(OrgId, WorkspaceId, Session, ExpectedVersion, At, Params)
            end
    end.

claim_session(OrgId, WorkspaceId, Session, ExpectedVersion, At, Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    case pick_seat(OrgId, IdentityId, Params) of
        {error, _} = Err ->
            Err;
        {ok, SeatIdentityId} ->
            do_claim(OrgId, WorkspaceId, Session, SeatIdentityId, ExpectedVersion, At, Params)
    end.

do_claim(OrgId, WorkspaceId, Session, IdentityId, ExpectedVersion, At, Params) ->
    case cs_session:assert_cas_expectation(Session, queued, ExpectedVersion) of
        {error, _} = Err ->
            Err;
        ok ->
            Event = claim_event(Session, IdentityId, At, Params),
            with_store(Params, fun(Store) ->
                Store:claim_session(
                    OrgId,
                    WorkspaceId,
                    maps:get(id, Session),
                    IdentityId,
                    ExpectedVersion,
                    At,
                    Event
                )
            end)
    end.

claim_event(Session, IdentityId, At, Params) ->
    #{
        session_id => maps:get(id, Session),
        business_identity_id => IdentityId,
        actor_user_id => maps:get(actor_user_id, Params, undefined),
        actor_kind => <<"seat">>,
        action => <<"session.claimed">>,
        detail => #{<<"at">> => At},
        workspace_id => maps:get(workspace_id, Session)
    }.

%% 显式 claim：坐席必须存在；停用由 DB CAS 拒（这里先给可读错误）。
%%
%% CS-BE-05：默认派单（identity 缺省）注入 presence 派生——只选运行态
%% `online` 的坐席（away/busy/offline 跳过；无 presence 行 = 从未上报心跳
%% = offline，从严）。全部不在线 → no_seat_available → 会话保持 queued
%% （CS-RUNTIME-02）。显式 claim（坐席主动接单）是在线事实本身，不做
%% presence 过滤（enabled + 容量门照旧）。
pick_seat(OrgId, undefined, Params) ->
    case with_store(Params, fun(Store) -> Store:list_dispatchable_seats(OrgId) end) of
        {error, _} = Err ->
            Err;
        {ok, Seats} ->
            Snapshot = presence_annotated(OrgId, Seats, Params),
            cs_dispatch:select_seat(OrgId, Snapshot)
    end;
pick_seat(OrgId, IdentityId, Params) ->
    case with_store(Params, fun(Store) -> Store:fetch_seat(OrgId, IdentityId) end) of
        {error, not_found} ->
            {error, {seat_not_found, IdentityId}};
        {error, _} = Err ->
            Err;
        {ok, Seat} ->
            case maps:get(enabled, Seat, false) of
                false -> {error, seat_disabled};
                true -> {ok, IdentityId}
            end
    end.

%% ===================================================================
%% transfer（A04：改绑 identity，主体字段零迁移）
%% ===================================================================

%% @doc 把 active 会话改绑给另一坐席 identity（owner/主体字段不变）。
%% Params：workspace_id / session_id / to_identity_id / expected_version / at 必填。
-spec transfer(integer(), map()) -> {ok, map()} | {error, term()}.
transfer(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            transfer_in(OrgId, WorkspaceId, Params)
    end;
transfer(_OrgId, _Params) ->
    {error, {invalid_argument, transfer}}.

transfer_in(OrgId, WorkspaceId, Params) ->
    SessionId = maps:get(session_id, Params, undefined),
    ToIdentityId = maps:get(to_identity_id, Params, undefined),
    ExpectedVersion = maps:get(expected_version, Params, undefined),
    At = maps:get(at, Params, undefined),
    case pos_int(SessionId) andalso pos_int(ToIdentityId) andalso pos_int(At) of
        false ->
            {error, {invalid_argument, transfer}};
        true ->
            do_transfer(OrgId, WorkspaceId, SessionId, ToIdentityId, ExpectedVersion, At, Params)
    end.

do_transfer(OrgId, WorkspaceId, SessionId, ToIdentityId, ExpectedVersion, At, Params) ->
    case fetch_control_session_for(Params, OrgId, WorkspaceId, SessionId) of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            case cs_session:transfer(Session, ToIdentityId, At) of
                {error, _} = Err ->
                    Err;
                {ok, Transferred} ->
                    Event = #{
                        session_id => SessionId,
                        business_identity_id => ToIdentityId,
                        actor_user_id => maps:get(actor_user_id, Params, undefined),
                        actor_kind => <<"seat">>,
                        action => <<"session.transferred">>,
                        detail => #{<<"from">> => maps:get(business_identity_id, Session)},
                        workspace_id => WorkspaceId
                    },
                    cas_write(
                        Params,
                        OrgId,
                        WorkspaceId,
                        fun(Store) ->
                            Store:transfer_session(
                                OrgId,
                                WorkspaceId,
                                SessionId,
                                ToIdentityId,
                                ExpectedVersion,
                                At,
                                Event
                            )
                        end,
                        Session,
                        Transferred
                    )
            end
    end.

%% ===================================================================
%% close
%% ===================================================================

%% @doc 关闭会话（queued|active → closed）。Params：workspace_id / session_id /
%% expected_version / at 必填；reason 可选（审计文本）。
-spec close(integer(), map()) -> {ok, map()} | {error, term()}.
close(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            close_in(OrgId, WorkspaceId, Params)
    end;
close(_OrgId, _Params) ->
    {error, {invalid_argument, close}}.

close_in(OrgId, WorkspaceId, Params) ->
    SessionId = maps:get(session_id, Params, undefined),
    ExpectedVersion = maps:get(expected_version, Params, undefined),
    At = maps:get(at, Params, undefined),
    case pos_int(SessionId) andalso pos_int(At) of
        false ->
            {error, {invalid_argument, close}};
        true ->
            do_close(OrgId, WorkspaceId, SessionId, ExpectedVersion, At, Params)
    end.

do_close(OrgId, WorkspaceId, SessionId, ExpectedVersion, At, Params) ->
    Reason = maps:get(reason, Params, undefined),
    case fetch_control_session_for(Params, OrgId, WorkspaceId, SessionId) of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            case cs_session:transition(Session, closed, #{at => At, reason => Reason}) of
                {error, _} = Err ->
                    Err;
                {ok, Closed} ->
                    Event = #{
                        session_id => SessionId,
                        business_identity_id => maps:get(business_identity_id, Session),
                        actor_user_id => maps:get(actor_user_id, Params, undefined),
                        actor_kind => actor_kind(Params),
                        action => <<"session.closed">>,
                        detail => #{<<"reason">> => Reason},
                        workspace_id => WorkspaceId
                    },
                    cas_write(
                        Params,
                        OrgId,
                        WorkspaceId,
                        fun(Store) ->
                            Store:close_session(
                                OrgId,
                                WorkspaceId,
                                SessionId,
                                Reason,
                                ExpectedVersion,
                                At,
                                Event
                            )
                        end,
                        Session,
                        Closed
                    )
            end
    end.

%% ===================================================================
%% rating（1..5；仅 closed；不可重复）
%% ===================================================================

%% @doc 给 closed 会话评分。Params：workspace_id / session_id / rating /
%% expected_version / at 必填。
-spec rate(integer(), map()) -> {ok, map()} | {error, term()}.
rate(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            rate_in(OrgId, WorkspaceId, Params)
    end;
rate(_OrgId, _Params) ->
    {error, {invalid_argument, rate}}.

rate_in(OrgId, WorkspaceId, Params) ->
    SessionId = maps:get(session_id, Params, undefined),
    Rating = maps:get(rating, Params, undefined),
    ExpectedVersion = maps:get(expected_version, Params, undefined),
    At = maps:get(at, Params, undefined),
    case pos_int(SessionId) andalso pos_int(At) of
        false ->
            {error, {invalid_argument, rate}};
        true ->
            do_rate(OrgId, WorkspaceId, SessionId, Rating, ExpectedVersion, At, Params)
    end.

do_rate(OrgId, WorkspaceId, SessionId, Rating, ExpectedVersion, At, Params) ->
    case fetch_session_for(Params, OrgId, WorkspaceId, SessionId) of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            case cs_session:rate(Session, Rating, At) of
                {error, _} = Err ->
                    Err;
                {ok, Rated} ->
                    Event = #{
                        session_id => SessionId,
                        business_identity_id => maps:get(business_identity_id, Session),
                        actor_user_id => maps:get(actor_user_id, Params, undefined),
                        actor_kind => actor_kind(Params),
                        action => <<"session.rated">>,
                        detail => #{<<"rating">> => Rating},
                        workspace_id => WorkspaceId
                    },
                    cas_write(
                        Params,
                        OrgId,
                        WorkspaceId,
                        fun(Store) ->
                            Store:rate_session(
                                OrgId,
                                WorkspaceId,
                                SessionId,
                                Rating,
                                ExpectedVersion,
                                At,
                                Event
                            )
                        end,
                        Session,
                        Rated
                    )
            end
    end.

%% ===================================================================
%% A03：客服消息只经 enterprise_business_facade 写 enterprise 真源
%% ===================================================================

%% @doc 在客服会话上发一条消息——**唯一**写入路径是
%% `enterprise_business_facade:append_message/2`（canonical message + policy
%% snapshot + audit 同事务，由 enterprise 侧裁决）。本 feature 不建
%% `customer_service_message` 等副本，也不引用 enterprise 的内层模块。
%%
%% 发送者二选一（与会话绑定一致，否则拒绝）：
%%   * 坐席出站：`business_identity_id`（必须等于会话当前经办）+ `actor_user_id`
%%     → `sender_type=business_identity`；
%%   * 访客入站：`contact_id`（必须等于会话绑定的 contact）→ `sender_type=contact`。
%%
%% Params：workspace_id / session_id / body / client_msg_id 必填；
%% `key_ref` 可选（显式注入优先；缺省由 enterprise 侧经 `imboy.eb_enterprise_keyring`
%% 装配——F6/RULING-2026-09-15 §七，密钥材料不经 HTTP 面）；
%% `canonical_tx` / `store` / `clock` / `id` / `notify` / `accepted_at` 等键
%% 原样透传给 facade（测试注入面）。
-spec append_session_message(integer(), map()) -> {ok, map()} | {error, term()}.
append_session_message(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            append_message_in(OrgId, WorkspaceId, Params)
    end;
append_session_message(_OrgId, _Params) ->
    {error, {invalid_argument, append_session_message}}.

append_message_in(OrgId, WorkspaceId, Params) ->
    SessionId = maps:get(session_id, Params, undefined),
    case fetch_session_for(Params, OrgId, WorkspaceId, SessionId) of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            case sender_shape(Session, Params) of
                {error, _} = Err ->
                    Err;
                {ok, SenderType} ->
                    case message_event_hook(Params, OrgId, WorkspaceId, Session) of
                        {error, _} = HookErr ->
                            HookErr;
                        {ok, Hook} ->
                            FacadeParams = facade_message_params(
                                Session, SenderType, WorkspaceId, Params
                            ),
                            %% BE-S01b（sse-event-contract）+ REVIEW-3 F-2：
                            %% message.appended 事件——坐席/访客/widget 三条消息
                            %% 路径的唯一写入点都汇经本用例，一次埋点全覆盖。
                            %% 事件行经 `persist_hook` **并入 canonical 事务**：
                            %% 消息与事件原子可见，"消息已入库、坐席/访客无推送"
                            %% 的瞬时窗口消失；事件写失败 ⇒ 整个事务回滚（消息
                            %% 不落库），同 client_msg_id 重试即安全（无半态）。
                            %% 审计丢失显式失败（audit_append_failed），不静默
                            %% 降级为"发了没事件"。
                            dispatch_message(OrgId, FacadeParams#{persist_hook => Hook})
                    end
            end
    end.

%% F-2：canonical 事务内的 `message.appended` 事件写钩子。canonical tx 在
%% 消息+审计+附件绑定写毕、事务仍开放时以 `(Conn, StoredMessage)` 调用本闭包；
%% 闭包用与外层同源的 store 端口（注入面一致）在**同一事务**内写事件行。
%% 返回 `{error, {audit_append_failed, _}}` 由 canonical tx 裁决整体回滚。
message_event_hook(Params, OrgId, WorkspaceId, Session) ->
    case cs_app_support:store_port(Params) of
        {error, _} = Err ->
            Err;
        {ok, Store} ->
            Base0 = #{
                session_id => maps:get(id, Session),
                business_identity_id => maps:get(business_identity_id, Session, undefined),
                actor_user_id => maps:get(actor_user_id, Params, undefined),
                actor_kind => actor_kind_of(Params),
                action => <<"message.appended">>,
                workspace_id => WorkspaceId
            },
            Base = maps:merge(Base0, maps:with([widget_credential], Params)),
            {ok, fun(Conn, StoredMessage) ->
                %% 事件只带资源 ID（payload_limits 合同：零正文/零附件引用细节）。
                Event = Base#{
                    detail => #{<<"message_id">> => message_id_of(StoredMessage)}
                },
                case Store:append_event_in(Conn, OrgId, Event) of
                    {ok, _EventId} -> ok;
                    {error, session_already_closed} = Err -> Err;
                    {error, conflict} = Err -> Err;
                    {error, seat_disabled} = Err -> Err;
                    {error, installation_revoked} = Err -> Err;
                    {error, token_revoked} = Err -> Err;
                    {error, token_expired} = Err -> Err;
                    {error, visit_token_invalid} = Err -> Err;
                    {error, Reason} -> {error, {audit_append_failed, Reason}}
                end
            end}
    end.

message_id_of(#{message_id := Id}) when is_integer(Id) -> Id;
message_id_of(#{message := #{id := Id}}) when is_integer(Id) -> Id;
%% canonical tx 侧传回的 StoredMessage 是 enterprise 归一化消息行（原子键 id）。
message_id_of(#{id := Id}) when is_integer(Id) -> Id;
message_id_of(_Other) -> undefined.

actor_kind_of(Params) ->
    case maps:get(business_identity_id, Params, undefined) of
        undefined -> <<"visitor">>;
        _Identity -> <<"seat">>
    end.

%% A03：客服消息的唯一写入路径是 `enterprise_business_facade:append_message`。
%% 跨裁剪调用按 F-EB10-1 加 `-ifdef` 保护：enterprise_business 未被选中的档位里
%% 本用例 fail-closed（显式不可用），不做任何客服侧消息副本兜底。
-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).
dispatch_message(OrgId, FacadeParams) ->
    enterprise_business_facade:append_message(OrgId, FacadeParams).
-else.
dispatch_message(_OrgId, _FacadeParams) ->
    {error, {enterprise_business_feature_not_selected, append_session_message}}.
-endif.

%% 坐席必须active且是当前经办；访客必须等于会话 contact。
sender_shape(#{status := closed}, _Params) ->
    {error, session_already_closed};
sender_shape(Session, Params) ->
    SessionIdentity = maps:get(business_identity_id, Session, undefined),
    SessionContact = maps:get(contact_id, Session, undefined),
    case maps:get(business_identity_id, Params, undefined) of
        undefined ->
            case maps:get(contact_id, Params, undefined) of
                SessionContact when is_integer(SessionContact) -> {ok, contact};
                VisitorContact -> {error, {not_session_contact, VisitorContact, SessionContact}}
            end;
        ParamIdentity when is_integer(ParamIdentity) ->
            Active = maps:get(status, Session) =:= active,
            case Active andalso ParamIdentity =:= SessionIdentity of
                true -> {ok, business_identity};
                false -> {error, {not_session_seat, ParamIdentity, SessionIdentity}}
            end;
        BadIdentity ->
            {error, {invalid_identity_id, BadIdentity}}
    end.

facade_message_params(Session, SenderType, WorkspaceId, Params) ->
    Base = #{
        workspace_id => WorkspaceId,
        conversation_id => maps:get(conversation_id, Session),
        client_msg_id => maps:get(client_msg_id, Params),
        sender_type => sender_type_bin(SenderType),
        %% BE-PATCH-01：body 可选（widget 附件消息 = 空正文 + asset_ids）——
        %% 缺键归一为空二进制交给企业 canonical（载荷规则在企业侧裁决）。
        body => maps:get(body, Params, <<>>),
        %% F6（RULING-2026-09-15 §七）：key_ref 不再是调用方必填——HTTP 面已删除
        %% 该参数（显式提交即 422）。这里只透传**显式注入**（测试/内部合同），
        %% 缺省为 undefined；主密钥的**装配**统一在 enterprise 侧 application 层
        %% 进行（eb_message_app 经 eb_env_keyring 解析 env keyring），跨 feature
        %% 不新增 facade 之外的直接引用。
        key_ref => maps:get(key_ref, Params, undefined)
    },
    WithSender =
        case SenderType of
            business_identity ->
                Base#{
                    identity_id => maps:get(business_identity_id, Session),
                    actor_user_id => maps:get(actor_user_id, Params, undefined)
                };
            contact ->
                Base#{contact_id => maps:get(contact_id, Session)}
        end,
    %% 注入面与可选键原样透传（facade/application 侧按需消费）。
    %% BE-PATCH-01：asset_ids（已投影 pos int）随载荷进企业 append_message。
    PassKeys = [canonical_tx, store, clock, id, notify, accepted_at, audit_action, asset_ids],
    maps:merge(WithSender, passthrough(Params, PassKeys)).

passthrough(Params, Keys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case maps:get(Key, Params, undefined) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        #{},
        Keys
    ).

sender_type_bin(business_identity) -> <<"business_identity">>;
sender_type_bin(contact) -> <<"contact">>.

%% ===================================================================
%% 内部辅助
%% ===================================================================

%% CAS 写入统一出口：写库后用 domain 断言主体字段零迁移（A04 自检）。
cas_write(Params, OrgId, WorkspaceId, StoreFun, Before, _ExpectedNext) ->
    case with_store(Params, StoreFun) of
        {error, _} = Err ->
            Err;
        {ok, After} ->
            ok = cs_session:assert_rebind_continuity(Before, After),
            _ = OrgId,
            _ = WorkspaceId,
            {ok, After}
    end.

fetch_session_for(Params, OrgId, WorkspaceId, SessionId) ->
    case pos_int(SessionId) of
        false ->
            {error, {invalid_session_id, SessionId}};
        true ->
            with_store(Params, fun(Store) ->
                Store:fetch_session(OrgId, WorkspaceId, SessionId)
            end)
    end.

actor_kind(Params) ->
    maps:get(actor_kind, Params, <<"tenant_admin">>).

with_store(Params, Fun) ->
    cs_app_support:with_store(Params, Fun).

pos_int(V) ->
    cs_app_support:pos_int(V).

%% ===================================================================
%% 坐席工作台列表（CSB-02R §12.4）：队列 GET / active / closed 三视图共用。
%% 与平台面 list_sessions/2 同源（同一 store 分页原语 + 键集口径），不复制
%% 业务规则；本用例只补坐席面投影（来源 / contact 掩码名 / 末条摘要 /
%% 稳定计数）。CS-BE-02（队列摘要与等待时长）增补：queued 视图行带服务端
%% `waiting_seconds`，`last_message` 增 `preview`（截断明文摘要，零密文出站）。
%% ===================================================================

%% @doc 坐席作用域的会话分页（org-wide；`workspace_id` 可选收窄到 0=不限）。
%%
%% Params：status 必填（queued | active | closed；坐席面无「全状态页」——
%% 三视图各自冻结）；after_id / limit 走 C1~C4 冻结键集口径（DESC、`id <` 游标、
%% 满页 = 尾行游标）；workspace_id / store 可选。
%%
%% 返回：`{ok, #{sessions, total, total_by_status, next_after_id}}`。计数与
%% 列表同作用域（同 Org + 同 workspace 收窄），稳定可对账。
-spec seat_session_page(integer(), map()) -> {ok, map()} | {error, term()}.
seat_session_page(OrgId, Params) when is_map(Params) ->
    case pos_int(OrgId) of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case cs_app_support:page_cursor(Params) of
                {error, _} = Err ->
                    Err;
                {ok, AfterId, Limit} ->
                    seat_session_page_status(OrgId, Params, AfterId, Limit)
            end
    end;
seat_session_page(_OrgId, _Params) ->
    {error, {invalid_argument, seat_session_page}}.

seat_session_page_status(OrgId, Params, AfterId, Limit) ->
    case cs_app_support:session_status(maps:get(status, Params, undefined)) of
        {error, _} = Err ->
            Err;
        {ok, undefined} ->
            %% 坐席面必须显式选视图；无「全部会话」页（队列语义 = status=queued）。
            {error, {invalid_status, undefined}};
        {ok, Status} ->
            seat_session_page_fetch(OrgId, Status, AfterId, Limit, Params)
    end.

seat_session_page_fetch(OrgId, Status, AfterId, Limit, Params) ->
    WorkspaceId = workspace_scope(Params),
    case
        with_store(Params, fun(Store) ->
            Store:seat_session_page(OrgId, Status, AfterId, Limit, WorkspaceId)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, #{rows := Rows, total := Total, total_by_status := ByStatus}} ->
            %% CS-BE-02 / F-R5：preview 的解密失败（body_open_failed 族）在
            %% `cs_message_preview` 内降级为 null 占位 + warning——单条解不开
            %% 的正文不拖垮整页（密文未解出，零内容泄漏）；其余真实异常
            %% （DB 错、编程错误）仍整页 {error,_} 上抛。
            case seat_session_views(Rows, Status, Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, Views} ->
                    Next = next_cursor(Rows, Limit),
                    {ok, #{
                        sessions => Views,
                        total => Total,
                        total_by_status => ByStatus,
                        next_after_id => Next
                    }}
            end
    end.

seat_session_views(Rows, Status, Params) ->
    seat_session_views(Rows, Status, Params, []).

seat_session_views([], _Status, _Params, Acc) ->
    {ok, lists:reverse(Acc)};
seat_session_views([Row | Rest], Status, Params, Acc) ->
    case seat_session_view(Row, Status, Params) of
        {error, _} = Err ->
            Err;
        {ok, View} ->
            seat_session_views(Rest, Status, Params, [View | Acc])
    end.

%% workspace 收窄：显式正整数生效；缺省/0 = org-wide（坐席作用域以 Org +
%% customer_service 职能 assignment 为界，cs_auth 已裁决）。
workspace_scope(Params) ->
    case maps:get(workspace_id, Params, 0) of
        Ws when is_integer(Ws), Ws > 0 -> Ws;
        _ -> 0
    end.

%% DESC 键集：满页 = 本页尾行（最小 id）游标；不足一页 = undefined。
next_cursor(Rows, Limit) ->
    case length(Rows) =:= Limit andalso Rows =/= [] of
        true -> maps:get(id, lists:last(Rows));
        false -> undefined
    end.

%% 坐席面行投影白名单（内部审计列 visit_token_id / close_reason /
%% created_by_user_id 不直接出站——created_by_user_id 只参与来源推导）。
%% CS-BE-02 增补：密文中转三列（last_message_body_cipher / _key_version /
%% _aad_hash）也不在白名单——密文绝不出站，只喂 cs_message_preview。
seat_session_view(Row, Status, Params) ->
    case last_message_view(Row, Params) of
        {error, _} = Err ->
            Err;
        {ok, LastMessage} ->
            {ok, seat_view_with(Row, Status, Params, LastMessage)}
    end.

seat_view_with(Row, Status, Params, LastMessage) ->
    Base = maps:with(
        [
            id,
            organization_id,
            workspace_id,
            contact_id,
            conversation_id,
            business_identity_id,
            status,
            version,
            queued_at,
            claimed_at,
            closed_at
        ],
        Row
    ),
    View0 = Base#{
        source => source_of(Row),
        contact => #{masked_name => masked_name(Row)},
        last_message => LastMessage
    },
    seat_waiting(View0, Status, Params, Row).

%% CS-BE-02：`waiting_seconds`（仅队列视图）= 服务端时钟 `at` − `queued_at`，
%% 下限 0（时钟回拨/同秒竞争防护）。`at` 由 handler 服务端注入（坐席队列 GET
%% 的动作表已声明 clock_unit => second，与 DF-6 同族——毫秒量纲会放大 1000
%% 倍）。active/closed 视图与缺 `at` 的直驱调用**不出该键**：字段只在语义
%% 成立时存在，形状稳定可测。status 白名单两种形态（binary 透传 / atom）都
%% 是 queued 语义（cs_app_support:session_status 的归一结果）。
seat_waiting(View, Queued, Params, Row) when Queued =:= queued; Queued =:= <<"queued">> ->
    case {maps:get(at, Params, undefined), maps:get(queued_at, Row, undefined)} of
        {At, QueuedAt} when is_integer(At), is_integer(QueuedAt) ->
            View#{waiting_seconds => erlang:max(0, At - QueuedAt)};
        _MissingClock ->
            View
    end;
seat_waiting(View, _ActiveOrClosed, _Params, _Row) ->
    View.

%% 来源推导与 contact 掩码名是坐席面读模型的公共语义（CS-BE-03 起由
%% cs_session_app 与会话上下文共用）——唯一实现点在 `cs_app_support`
%% （CS-DEC-01 白名单字段族，零复制）。
source_of(Row) ->
    cs_app_support:source_of(Row).

%% contact 掩码名：优先 enterprise 侧既有 subject_mask（本就是掩码），
%% 其次 display_name 打码（保留首尾各一字符，中间 ***），二者皆缺 →
%% 稳定匿名柄 `guest#NNNNN`（contact_id 低五位，零 PII）。
masked_name(Row) ->
    cs_app_support:masked_name(Row).

%% 末条消息摘要（CS-BE-02 起含 preview）：id / sender_type / created_at +
%% `preview`（服务端解密后按 Unicode 码点截断的前 64 个字符，见
%% `cs_message_preview`）。密文/密钥面（body_cipher、key_version、aad_hash、
%% client_msg_id）不进本投影；撤回（hidden）行与附件-only（空正文）消息的
%% preview 为 null 占位；keyring 未装配与解密失败（F-R5）时整体降级为
%% null（不吐密文、不整页失败）。
last_message_view(Row, Params) ->
    case cs_message_preview:preview(Row, Params) of
        {error, _} = Err ->
            Err;
        {ok, Preview} ->
            {ok, last_message_with(Row, Preview)}
    end.

last_message_with(Row, Preview) ->
    case maps:get(last_message_id, Row, undefined) of
        undefined ->
            undefined;
        Id ->
            #{
                id => Id,
                sender_type => maps:get(last_message_sender_type, Row, undefined),
                created_at => maps:get(last_message_created_at, Row, undefined),
                preview => Preview
            }
    end.

%% ===================================================================
%% CS-BE-07：按需统计（纯读；date + tz_offset 显式窗口，不依赖 DB 时区）
%% ===================================================================

%% Unix epoch 的格里历秒基准（calendar 换算用）。
-define(GREGORIAN_EPOCH_SECONDS, 62167219200).
%% 时区偏移上界（分钟）：UTC±14 覆盖全部现役时区（含 Kiribati +14）。
-define(MAX_TZ_OFFSET_MINUTES, 840).

%% @doc 客服统计（治理面 GET；零预聚合、零缓存，每次现算）。
%%
%% Params：
%%   * `date`（可选，`YYYY-MM-DD`）：统计日；缺省 = 服务端时钟 `at` 的
%%     **UTC 当日**（at 亦缺省时取服务器当前秒——两步兜底都显式，绝不隐式
%%     依赖 DB 会话时区）；
%%   * `tz_offset`（可选，整数分钟，缺省 0=UTC，界 ±840）：`date` 在该偏移
%%     时区的 [00:00, 24:00) 折算为 UTC epoch 窗口；
%%   * `workspace_id`（可选）：给出则收窄到该 workspace，缺省 org-wide；
%%   * `store` / `at` 可注入（测试面）。
%%
%% 窗口 W = [window_start, window_end)（epoch 秒，UTC instant），指标公式：
%%   * `new_sessions` = |queued_at ∈ W|（开会话轴）；
%%   * `first_response` = {count, avg_seconds} over queued_at ∈ W 且已 claim
%%     （avg = 平均 claimed_at − queued_at 秒）——**未 claim 不进分母**，
%%     无样本 count=0 / avg_seconds=null；
%%   * `closed_sessions` = |closed_at ∈ W|（关闭轴独立：昨日开今日关也计入）；
%%   * `rating` = {count, avg} over rating_at ∈ W——**未评分不进分母**，
%%     无样本 count=0 / avg=null；
%%   * `current` = 当前时刻 queued/active 计数（不看窗口）。
-spec session_stats(integer(), map()) -> {ok, map()} | {error, term()}.
session_stats(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    case stats_workspace_scope(Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case stats_window(Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, Date, Tz, Start, End} ->
                    stats_view(OrgId, WorkspaceId, Date, Tz, Start, End, Params)
            end
    end;
session_stats(OrgId, _Params) ->
    {error, {invalid_organization_id, OrgId}}.

%% workspace 门：本用例 org-wide 聚合（缺省 0=不限）——显式给出须正整数，
%% 其余形状与租户门口径一致（{invalid_workspace_id, 原值}）。
stats_workspace_scope(Params) ->
    case maps:get(workspace_id, Params, undefined) of
        undefined ->
            {ok, 0};
        Ws when is_integer(Ws), Ws > 0 ->
            {ok, Ws};
        Other ->
            {error, {invalid_workspace_id, Other}}
    end.

%% 窗口换算：date（缺省 = at 的 UTC 当日）+ tz_offset（缺省 0，界 ±840）→
%% UTC epoch [start, end)。date 二进制形状严格 YYYY-MM-DD 且为真实日历日。
stats_window(Params) ->
    case stats_date(maps:get(date, Params, undefined), maps:get(at, Params, undefined)) of
        {error, _} = Err ->
            Err;
        {ok, Date} ->
            case stats_tz_offset(maps:get(tz_offset, Params, 0)) of
                {error, _} = Err2 ->
                    Err2;
                {ok, Tz} ->
                    Start = gregorian_day_epoch(Date) - Tz * 60,
                    {ok, date_binary(Date), Tz, Start, Start + 86400}
            end
    end.

stats_date(undefined, At) ->
    %% 缺省日 = at（服务端派生秒；测试可注入）的 UTC 当日；at 亦缺省取
    %% 服务器当前秒——两步都显式 UTC（calendar:gregorian_seconds_to_date
    %% 就是 UTC 换算），与 DB 会话时区无关。
    Epoch =
        case is_integer(At) of
            true -> At;
            false -> os:system_time(second)
        end,
    {Date, _Time} = calendar:gregorian_seconds_to_datetime(Epoch + ?GREGORIAN_EPOCH_SECONDS),
    {ok, Date};
stats_date(DateBin, _At) when is_binary(DateBin) ->
    case DateBin of
        <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> ->
            case {digits_only(Y), digits_only(M), digits_only(D)} of
                {true, true, true} ->
                    Date = {b2i(Y), b2i(M), b2i(D)},
                    case calendar:valid_date(Date) of
                        true -> {ok, Date};
                        false -> {error, {invalid_date, DateBin}}
                    end;
                _ ->
                    {error, {invalid_date, DateBin}}
            end;
        _ ->
            {error, {invalid_date, DateBin}}
    end;
stats_date(Other, _At) ->
    {error, {invalid_date, Other}}.

stats_tz_offset(V) when is_integer(V), abs(V) =< ?MAX_TZ_OFFSET_MINUTES ->
    {ok, V};
stats_tz_offset(V) ->
    {error, {invalid_tz_offset, V}}.

gregorian_day_epoch({Y, M, D}) ->
    calendar:datetime_to_gregorian_seconds({{Y, M, D}, {0, 0, 0}}) - ?GREGORIAN_EPOCH_SECONDS.

date_binary({Y, M, D}) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

%% 输入来自 stats_date/2 的定长段（Y 4 字节、M/D 各 2 字节），不可能为空
%% 二进制——无需（dialyzer 可证的死）空串卫兵。
digits_only(Bin) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Bin)).

b2i(Bin) ->
    binary_to_integer(Bin).

%% 出站组装：store 事实 → 白名单视图（唯一出口；红线键在此裁剪）。
%% count=0 时均值强制 undefined——PG AVG 无样本为 NULL，这里双保险，
%% 0 绝不伪装成均值。
stats_view(OrgId, WorkspaceId, DateBin, Tz, Start, End, Params) ->
    case
        with_store(Params, fun(Store) -> Store:session_stats(OrgId, WorkspaceId, Start, End) end)
    of
        {error, _} = Err ->
            Err;
        {ok, Facts} ->
            Claimed = maps:get(claimed_in_window, Facts, 0),
            Rated = maps:get(rated_in_window, Facts, 0),
            StatusCounts = maps:get(status_counts, Facts, #{}),
            {ok, #{
                organization_id => OrgId,
                workspace_id => WorkspaceId,
                date => DateBin,
                tz_offset => Tz,
                window_start => Start,
                window_end => End,
                new_sessions => maps:get(new_sessions, Facts, 0),
                first_response => #{
                    count => Claimed,
                    avg_seconds => zero_means_no_samples(
                        Claimed, maps:get(first_response_avg_seconds, Facts, undefined)
                    )
                },
                closed_sessions => maps:get(closed_sessions, Facts, 0),
                rating => #{
                    count => Rated,
                    avg => zero_means_no_samples(Rated, maps:get(avg_rating, Facts, undefined))
                },
                current => #{
                    queued => maps:get(queued, StatusCounts, 0),
                    active => maps:get(active, StatusCounts, 0)
                }
            }}
    end.

zero_means_no_samples(0, _Avg) -> undefined;
zero_means_no_samples(_N, Avg) -> Avg.

%% ===================================================================
%% CS-BE-05：默认派单的 presence 注入（annotate-then-select）
%% ===================================================================

%% 拉 org 级 presence 事实行并逐行注入 derived_status；时钟取 Params.at
%% （测试注入）缺省服务器当前秒。presence 读失败不阻断派单（退化为
%% 无 derived_status 键 = 历史行为）——派单偏好不应因派生数据源抖动而
%% 把可接单坐席全部排除；真正的容量/状态裁决在 claim DB CAS。
presence_annotated(OrgId, Seats, Params) ->
    Now =
        case maps:get(at, Params, undefined) of
            At when is_integer(At) -> At;
            _ -> os:system_time(second)
        end,
    case with_store(Params, fun(Store) -> Store:list_seat_presence(OrgId) end) of
        {ok, Presences} -> cs_presence:annotate(Seats, Presences, Now);
        {error, _} -> Seats
    end.

%% Server-derived CS identity uses the same lifecycle/audit path as visitor writes.
append_conversation_message(OrgId, #{sender_type := <<"business_identity">>} = Params) ->
    case cs_app_support:store_port(Params) of
        {ok, Store} ->
            Ws = maps:get(workspace_id, Params),
            case Store:fetch_conversation_session(OrgId, Ws, maps:get(conversation_id, Params)) of
                {ok, Session} ->
                    append_session_message(OrgId, Params#{
                        session_id => maps:get(id, Session),
                        business_identity_id => maps:get(identity_id, Params)
                    });
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
append_conversation_message(_, _) ->
    {error, {invalid_argument, sender_type}}.

%% Seat principal must be current handler. Governance callers have no seat identity.
fetch_control_session_for(Params, Org, Ws, Id) ->
    case fetch_session_for(Params, Org, Ws, Id) of
        {ok, Session} = Found ->
            case maps:is_key(business_identity_id, Params) of
                false ->
                    Found;
                true ->
                    case sender_shape(Session, Params) of
                        {ok, business_identity} -> Found;
                        {error, _} = Err -> Err
                    end
            end;
        {error, _} = Err ->
            Err
    end.
