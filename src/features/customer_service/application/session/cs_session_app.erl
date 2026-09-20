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
    list_contact_sessions/2,
    list_sessions/2,
    seat_session_page/2
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
            case
                with_store(Params, fun(Store) ->
                    Store:insert_session(OrgId, WorkspaceId, Draft)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    case
                        append_event(Params, OrgId, #{
                            session_id => SessionId,
                            actor_user_id => maps:get(created_by_user_id, Params, undefined),
                            actor_kind => <<"visitor">>,
                            action => <<"session.opened">>,
                            detail => #{<<"contact_id">> => ContactId},
                            workspace_id => WorkspaceId
                        })
                    of
                        ok -> {ok, Stored};
                        {error, _} = AuditErr -> AuditErr
                    end
            end
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
pick_seat(OrgId, undefined, Params) ->
    case with_store(Params, fun(Store) -> Store:list_dispatchable_seats(OrgId) end) of
        {error, _} = Err -> Err;
        {ok, Seats} -> cs_dispatch:select_seat(OrgId, Seats)
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
    case fetch_session_for(Params, OrgId, WorkspaceId, SessionId) of
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
    case fetch_session_for(Params, OrgId, WorkspaceId, SessionId) of
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
                    FacadeParams = facade_message_params(Session, SenderType, WorkspaceId, Params),
                    case dispatch_message(OrgId, FacadeParams) of
                        {error, _} = Err2 ->
                            Err2;
                        {ok, Accepted} ->
                            %% BE-S01b（sse-event-contract）：message.appended
                            %% 事件——坐席/访客/widget 三条消息路径的唯一写入点
                            %% 都汇经本用例，一次埋点全覆盖。审计丢失显式失败
                            %% （audit_append_failed），不静默降级为"发了没事件"。
                            case message_event(Params, OrgId, WorkspaceId, Session, Accepted) of
                                ok -> {ok, Accepted};
                                {error, _} = AuditErr -> AuditErr
                            end
                    end
            end
    end.

%% 事件只带资源 ID（payload_limits 合同：零正文/零附件引用细节）。
message_event(Params, OrgId, WorkspaceId, Session, Accepted) ->
    append_event(Params, OrgId, #{
        session_id => maps:get(id, Session),
        business_identity_id => maps:get(business_identity_id, Session, undefined),
        actor_user_id => maps:get(actor_user_id, Params, undefined),
        actor_kind => actor_kind_of(Params),
        action => <<"message.appended">>,
        detail => #{<<"message_id">> => message_id_of(Accepted)},
        workspace_id => WorkspaceId
    }).

message_id_of(#{message_id := Id}) when is_integer(Id) -> Id;
message_id_of(#{message := #{id := Id}}) when is_integer(Id) -> Id;
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
        body => maps:get(body, Params),
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
    PassKeys = [canonical_tx, store, clock, id, notify, accepted_at, audit_action],
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

append_event(Params, OrgId, Event) ->
    cs_app_support:append_event(Params, OrgId, Event).

with_store(Params, Fun) ->
    cs_app_support:with_store(Params, Fun).

pos_int(V) ->
    cs_app_support:pos_int(V).

%% ===================================================================
%% 坐席工作台列表（CSB-02R §12.4）：队列 GET / active / closed 三视图共用。
%% 与平台面 list_sessions/2 同源（同一 store 分页原语 + 键集口径），不复制
%% 业务规则；本用例只补坐席面投影（来源 / contact 掩码名 / 末条安全摘要 /
%% 稳定计数）。
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
            Views = [seat_session_view(Row) || Row <- Rows],
            Next = next_cursor(Rows, Limit),
            {ok, #{
                sessions => Views,
                total => Total,
                total_by_status => ByStatus,
                next_after_id => Next
            }}
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
seat_session_view(Row) ->
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
    Base#{
        source => source_of(Row),
        contact => #{masked_name => masked_name(Row)},
        last_message => last_message_view(Row)
    }.

%% 来源推导（存储派生事实，浏览器不可申报）：
%%   * visit_token_id 非空   → widget（访客经 widget/visit 令牌开会话）；
%%   * created_by_user_id 非空 → seat（坐席在建会话时创建）；
%%   * 其余                   → shop_key（门店接入 POST /sessions/queue）。
source_of(#{visit_token_id := V}) when V =/= undefined -> <<"widget">>;
source_of(#{created_by_user_id := U}) when U =/= undefined -> <<"seat">>;
source_of(_Row) -> <<"shop_key">>.

%% contact 掩码名：优先 enterprise 侧既有 subject_mask（本就是掩码），
%% 其次 display_name 打码（保留首尾各一字符，中间 ***），二者皆缺 →
%% 稳定匿名柄 `guest#NNNNN`（contact_id 低五位，零 PII）。
masked_name(Row) ->
    Mask = maps:get(contact_subject_mask, Row, undefined),
    Name = maps:get(contact_display_name, Row, undefined),
    case {is_binary(Mask), Mask =/= <<>>, Mask =/= undefined} of
        {true, true, true} ->
            Mask;
        _ ->
            case is_binary(Name) andalso Name =/= <<>> of
                true -> mask_display_name(Name);
                false -> default_masked_name(maps:get(contact_id, Row, 0))
            end
    end.

mask_display_name(Name) ->
    Chars = unicode:characters_to_list(Name, utf8),
    Masked =
        case length(Chars) of
            0 -> [];
            1 -> "*";
            2 -> [hd(Chars), $*];
            _ -> [hd(Chars), $*, $*, $*, lists:last(Chars)]
        end,
    unicode:characters_to_binary(Masked, utf8).

default_masked_name(ContactId) when is_integer(ContactId), ContactId > 0 ->
    <<"guest#", (pad5(integer_to_binary(ContactId rem 100000)))/binary>>;
default_masked_name(_ContactId) ->
    <<"guest#00000">>.

pad5(Bin) when byte_size(Bin) >= 5 ->
    Bin;
pad5(Bin) ->
    pad5(<<"0", Bin/binary>>).

%% 末条消息**安全摘要**：只含 id / sender_type / created_at——body_cipher、
%% key_version、client_msg_id 等（密文与密钥面）不进本投影，workbench 未解锁
%% E2EE 前不透出任何消息内容。
last_message_view(Row) ->
    case maps:get(last_message_id, Row, undefined) of
        undefined ->
            undefined;
        Id ->
            #{
                id => Id,
                sender_type => maps:get(last_message_sender_type, Row, undefined),
                created_at => maps:get(last_message_created_at, Row, undefined)
            }
    end.
