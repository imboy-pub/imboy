%%% @doc `cs_store_port` 的 PG 实现（cs_pg_seat / cs_pg_session / cs_pg_token 的装配面）。
%%%
%%% 本模块是端口契约与 PG 子模块之间的**薄委派层**：让 application 只看到
%%% `cs_store_port` 一个契约，而按域拆分的 SQL 细节留在各自子模块。
%%% 零业务规则；租户门由各子模块的 SQL 同语句约束承担（铁律 6）。
-module(cs_pg_store).

-behaviour(cs_store_port).

-export([
    fetch_identity_function/2,
    %% seat
    insert_seat/2,
    fetch_seat/2,
    list_dispatchable_seats/1,
    list_dispatchable_seats_page/3,
    list_all_seats_page/3,
    set_seat_enabled/4,
    %% CS-BE-06：席位 entitlement
    seat_limit/1,
    set_seat_limit/2,
    govern_seat/4,
    create_seat_limit_checked/5,
    create_seat_limit_checked/6,
    set_enabled_checked/4,
    set_enabled_checked/5,
    %% BE-S01a：坐席上下文聚合 / 转接目标
    list_seat_org_contexts/1,
    list_transfer_targets_page/4,
    %% CS-BE-05：presence 心跳 lease
    heartbeat_seat/4,
    set_seat_manual_status/4,
    fetch_seat_presence/2,
    list_seat_presence/1,
    %% session
    insert_session/3,
    fetch_session/3,
    claim_session/7,
    transfer_session/7,
    close_session/7,
    rate_session/7,
    list_sessions_for_contact/3,
    list_sessions_page/5,
    %% CSB-02R：坐席工作台分页 / widget 装配缺省 Workspace
    seat_session_page/5,
    default_workspace/1,
    %% CS-BE-03：客户上下文只读事实（会话锚定事实行 / 历史页 / 备注页）
    fetch_session_customer_context/3,
    list_session_history_page/4,
    list_contact_notes_page/3,
    %% CS-BE-04：已读游标（单调 ACK 幂等 + 未读事实现算）
    ack_session_read/6,
    fetch_session_read_state/4,
    %% CS-BE-07：按需统计（窗口聚合 + 当前 status 计数）
    session_stats/4,
    %% shop key / visit token
    insert_shop_key/2,
    fetch_shop_key/2,
    fetch_shop_key_by_digest/2,
    list_shop_keys_page/3,
    revoke_shop_key/3,
    insert_visit_token/2,
    fetch_visit_token/2,
    fetch_visit_token_by_digest/2,
    list_visit_tokens_page/3,
    revoke_visit_token/3,
    %% widget installation / identity key / bootstrap token / nonce（CSB-01）
    insert_widget_installation/2,
    fetch_widget_installation/2,
    fetch_widget_installation_by_public_id/2,
    fetch_widget_installation_by_public_id_global/1,
    list_widget_installations_page/3,
    revoke_widget_installation/3,
    update_widget_installation/4,
    insert_widget_identity_key/3,
    fetch_widget_identity_key/3,
    revoke_widget_identity_key/4,
    insert_widget_bootstrap_token/2,
    fetch_widget_bootstrap_token_by_digest/3,
    fetch_widget_bootstrap_token_by_digest_global/2,
    touch_widget_bootstrap_token/4,
    revoke_widget_bootstrap_token/4,
    record_widget_nonce/4,
    %% seat console 嵌入（seat-console-embed SC-BE）
    insert_seat_console/2,
    fetch_seat_console/3,
    fetch_seat_console_by_public_id_global/1,
    list_seat_consoles_page/4,
    revoke_seat_console/4,
    update_seat_console/5,
    %% event + SSE 读面（BE-S01b）
    append_event/2,
    append_event_in/3,
    fetch_event_scope/2,
    list_events_page/4,
    event_watermark/2,
    %% admin provisioning（BE-S01b）
    provision_seat/3
]).

%% identity 事实（A01）
fetch_identity_function(OrgId, IdentityId) ->
    cs_pg_seat:fetch_identity_function(OrgId, IdentityId).

%% seat
insert_seat(OrgId, Seat) -> cs_pg_seat:insert_seat(OrgId, Seat).
fetch_seat(OrgId, IdentityId) -> cs_pg_seat:fetch_seat(OrgId, IdentityId).
list_dispatchable_seats(OrgId) -> cs_pg_seat:list_dispatchable_seats(OrgId).
list_dispatchable_seats_page(OrgId, AfterId, Limit) ->
    cs_pg_seat:list_dispatchable_seats_page(OrgId, AfterId, Limit).
list_all_seats_page(OrgFilter, AfterId, Limit) ->
    cs_pg_seat:list_all_seats_page(OrgFilter, AfterId, Limit).
set_seat_enabled(OrgId, IdentityId, Enabled, At) ->
    cs_pg_seat:set_seat_enabled(OrgId, IdentityId, Enabled, At).

%% CS-BE-06：席位 entitlement（薄委派；tx 版由 store 侧 with_tx 包裹）。
seat_limit(OrgId) ->
    cs_pg_seat:seat_limit(OrgId, pool).
set_seat_limit(OrgId, Limit) ->
    cs_pg_seat:set_seat_limit(OrgId, Limit, pool).
%% elib_pg:with_tx 的业务回滚信号是 {rollback, Reason}（throw 拦截分支）——
%% 归一为调用方处处期望的 {error, Reason}（seat_limit_exceeded 等）。
govern_seat(Conn, OrgId, Operation, Params) ->
    cs_pg_seat_governance:govern(Conn, OrgId, Operation, Params).

create_seat_limit_checked(OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy) ->
    create_seat_limit_checked(OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy, undefined).

create_seat_limit_checked(OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy, Event) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            Result = cs_pg_seat:create_seat_limit_tx(
                Conn, OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy
            ),
            audited_seat(Conn, OrgId, Result, Event)
        end)
    of
        {rollback, {error, Reason}} -> {error, Reason};
        {rollback, Reason} -> {error, Reason};
        Other -> Other
    end.
set_enabled_checked(OrgId, IdentityId, Enabled, At) ->
    set_enabled_checked(OrgId, IdentityId, Enabled, At, undefined).

set_enabled_checked(OrgId, IdentityId, Enabled, At, Event) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            Result = cs_pg_seat:set_enabled_limit_tx(Conn, OrgId, IdentityId, Enabled, At),
            audited_seat(Conn, OrgId, Result, Event)
        end)
    of
        {rollback, {error, Reason}} -> {error, Reason};
        {rollback, Reason} -> {error, Reason};
        Other -> Other
    end.
%% Audited entry points commit the seat and its event together; old arities remain
%% available for existing infrastructure callers that do not supply an event.
audited_seat(_Conn, _OrgId, Result, undefined) ->
    Result;
audited_seat(Conn, OrgId, {ok, _} = Result, Event) ->
    case cs_pg_seat:insert_event_in(Conn, OrgId, Event) of
        {ok, _} -> Result;
        {error, Reason} -> throw({rollback, {error, {audit_append_failed, Reason}}})
    end.

list_seat_org_contexts(UserId) ->
    cs_pg_seat:list_seat_org_contexts(UserId).
list_transfer_targets_page(OrgId, ExcludeIdentityId, AfterId, Limit) ->
    cs_pg_seat:list_transfer_targets_page(OrgId, ExcludeIdentityId, AfterId, Limit).

%% CS-BE-05：presence（cs_store_port 同名 callback 的薄委派）。
heartbeat_seat(OrgId, IdentityId, AtSec, Opts) ->
    cs_pg_seat:heartbeat_seat(OrgId, IdentityId, AtSec, Opts).
set_seat_manual_status(OrgId, IdentityId, AtSec, ManualStatus) ->
    cs_pg_seat:set_seat_manual_status(OrgId, IdentityId, AtSec, ManualStatus).
fetch_seat_presence(OrgId, IdentityId) ->
    cs_pg_seat:fetch_seat_presence(OrgId, IdentityId).
list_seat_presence(OrgId) ->
    cs_pg_seat:list_seat_presence(OrgId).

%% session
insert_session(OrgId, WorkspaceId, Session) ->
    cs_pg_session:insert_session(OrgId, WorkspaceId, Session).
fetch_session(OrgId, WorkspaceId, SessionId) ->
    cs_pg_session:fetch_session(OrgId, WorkspaceId, SessionId).
claim_session(OrgId, WorkspaceId, SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event) ->
    cs_pg_session:claim_session(
        OrgId, WorkspaceId, SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event
    ).
transfer_session(OrgId, WorkspaceId, SessionId, ToIdentityId, ExpectedVersion, At, Event) ->
    cs_pg_session:transfer_session(
        OrgId, WorkspaceId, SessionId, ToIdentityId, ExpectedVersion, At, Event
    ).
close_session(OrgId, WorkspaceId, SessionId, Reason, ExpectedVersion, At, Event) ->
    cs_pg_session:close_session(
        OrgId, WorkspaceId, SessionId, Reason, ExpectedVersion, At, Event
    ).
rate_session(OrgId, WorkspaceId, SessionId, Rating, ExpectedVersion, At, Event) ->
    cs_pg_session:rate_session(
        OrgId, WorkspaceId, SessionId, Rating, ExpectedVersion, At, Event
    ).
list_sessions_for_contact(OrgId, WorkspaceId, ContactId) ->
    cs_pg_session:list_sessions_for_contact(OrgId, WorkspaceId, ContactId).
list_sessions_page(OrgId, WorkspaceId, Status, AfterId, Limit) ->
    cs_pg_session:list_sessions_page(OrgId, WorkspaceId, Status, AfterId, Limit).

%% CSB-02R：坐席工作台分页（含稳定计数）。
seat_session_page(OrgId, Status, AfterId, Limit, WorkspaceId) ->
    cs_pg_session:seat_session_page(OrgId, Status, AfterId, Limit, WorkspaceId).

%% CS-BE-03：客户上下文只读事实（cs_store_port 同名 callback 的薄委派）。
fetch_session_customer_context(OrgId, WorkspaceId, SessionId) ->
    cs_pg_session:fetch_session_customer_context(OrgId, WorkspaceId, SessionId).
list_session_history_page(OrgId, ContactId, AfterId, Limit) ->
    cs_pg_session:list_session_history_page(OrgId, ContactId, AfterId, Limit).
list_contact_notes_page(OrgId, ContactId, Limit) ->
    cs_pg_session:list_contact_notes_page(OrgId, ContactId, Limit).

%% CS-BE-04：已读游标（cs_store_port 同名 callback 的薄委派）。
ack_session_read(OrgId, WorkspaceId, SessionId, IdentityId, LastReadMessageId, At) ->
    cs_pg_session:ack_session_read(
        OrgId, WorkspaceId, SessionId, IdentityId, LastReadMessageId, At
    ).
fetch_session_read_state(OrgId, WorkspaceId, SessionId, IdentityId) ->
    cs_pg_session:fetch_session_read_state(OrgId, WorkspaceId, SessionId, IdentityId).

%% CS-BE-07：按需统计（cs_store_port 同名 callback 的薄委派；纯读）。
session_stats(OrgId, WorkspaceId, WindowStart, WindowEnd) ->
    cs_pg_session:session_stats(OrgId, WorkspaceId, WindowStart, WindowEnd).

%% CSB-02R：widget 装配的本 Org 缺省 Workspace 解析。
default_workspace(OrgId) ->
    cs_pg_widget:default_workspace(OrgId).

%% shop key / visit token
insert_shop_key(OrgId, Key) -> cs_pg_token:insert_shop_key(OrgId, Key).
fetch_shop_key(OrgId, KeyId) -> cs_pg_token:fetch_shop_key(OrgId, KeyId).
fetch_shop_key_by_digest(OrgId, Digest) -> cs_pg_token:fetch_shop_key_by_digest(OrgId, Digest).
list_shop_keys_page(OrgId, AfterId, Limit) ->
    cs_pg_token:list_shop_keys_page(OrgId, AfterId, Limit).
revoke_shop_key(OrgId, KeyId, At) -> cs_pg_token:revoke_shop_key(OrgId, KeyId, At).
insert_visit_token(OrgId, Token) -> cs_pg_token:insert_visit_token(OrgId, Token).
fetch_visit_token(OrgId, TokenId) -> cs_pg_token:fetch_visit_token(OrgId, TokenId).
fetch_visit_token_by_digest(OrgId, Digest) ->
    cs_pg_token:fetch_visit_token_by_digest(OrgId, Digest).
list_visit_tokens_page(OrgId, AfterId, Limit) ->
    cs_pg_token:list_visit_tokens_page(OrgId, AfterId, Limit).
revoke_visit_token(OrgId, TokenId, At) -> cs_pg_token:revoke_visit_token(OrgId, TokenId, At).

%% widget installation / identity key / bootstrap token / nonce（CSB-01/02）
insert_widget_installation(OrgId, Installation) ->
    cs_pg_widget:insert_widget_installation(OrgId, Installation).
fetch_widget_installation(OrgId, InstallationId) ->
    cs_pg_widget:fetch_widget_installation(OrgId, InstallationId).
fetch_widget_installation_by_public_id(OrgId, PublicWidgetId) ->
    cs_pg_widget:fetch_widget_installation_by_public_id(OrgId, PublicWidgetId).
%% CSD-BE-01（hosted-widget-contract S3）：public_widget_id 全局反查（/w/ 面，
%% Org 是行输出的派生值而非查询输入）。
fetch_widget_installation_by_public_id_global(PublicWidgetId) ->
    cs_pg_widget:fetch_widget_installation_by_public_id_global(PublicWidgetId).
list_widget_installations_page(OrgId, AfterId, Limit) ->
    cs_pg_widget:list_widget_installations_page(OrgId, AfterId, Limit).
revoke_widget_installation(OrgId, InstallationId, At) ->
    cs_pg_widget:revoke_widget_installation(OrgId, InstallationId, At).
update_widget_installation(OrgId, InstallationId, At, Updates) ->
    cs_pg_widget:update_widget_installation(OrgId, InstallationId, At, Updates).
insert_widget_identity_key(OrgId, InstallationId, Key) ->
    cs_pg_widget:insert_widget_identity_key(OrgId, InstallationId, Key).
fetch_widget_identity_key(OrgId, InstallationId, KeyVersion) ->
    cs_pg_widget:fetch_widget_identity_key(OrgId, InstallationId, KeyVersion).
revoke_widget_identity_key(OrgId, InstallationId, KeyVersion, At) ->
    cs_pg_widget:revoke_widget_identity_key(OrgId, InstallationId, KeyVersion, At).
insert_widget_bootstrap_token(OrgId, Token) ->
    cs_pg_widget:insert_widget_bootstrap_token(OrgId, Token).
fetch_widget_bootstrap_token_by_digest(OrgId, InstallationId, Digest) ->
    cs_pg_widget:fetch_widget_bootstrap_token_by_digest(OrgId, InstallationId, Digest).
fetch_widget_bootstrap_token_by_digest_global(InstallationId, Digest) ->
    cs_pg_widget:fetch_widget_bootstrap_token_by_digest_global(InstallationId, Digest).
touch_widget_bootstrap_token(OrgId, InstallationId, TokenId, At) ->
    cs_pg_widget:touch_widget_bootstrap_token(OrgId, InstallationId, TokenId, At).
revoke_widget_bootstrap_token(OrgId, InstallationId, TokenId, At) ->
    cs_pg_widget:revoke_widget_bootstrap_token(OrgId, InstallationId, TokenId, At).
record_widget_nonce(OrgId, InstallationId, JtiDigest, ExpiresAt) ->
    cs_pg_widget:record_widget_nonce(OrgId, InstallationId, JtiDigest, ExpiresAt).

%% seat console 嵌入（seat-console-embed SC-BE）
insert_seat_console(OrgId, Console) ->
    cs_pg_seat_console:insert_seat_console(OrgId, Console).
fetch_seat_console(OrgId, WorkspaceId, ConsoleId) ->
    cs_pg_seat_console:fetch_seat_console(OrgId, WorkspaceId, ConsoleId).
fetch_seat_console_by_public_id_global(PublicSeatConsoleId) ->
    cs_pg_seat_console:fetch_seat_console_by_public_id_global(PublicSeatConsoleId).
list_seat_consoles_page(OrgId, WorkspaceId, AfterId, Limit) ->
    cs_pg_seat_console:list_seat_consoles_page(OrgId, WorkspaceId, AfterId, Limit).
revoke_seat_console(OrgId, WorkspaceId, ConsoleId, At) ->
    cs_pg_seat_console:revoke_seat_console(OrgId, WorkspaceId, ConsoleId, At).
update_seat_console(OrgId, WorkspaceId, ConsoleId, At, Updates) ->
    cs_pg_seat_console:update_seat_console(OrgId, WorkspaceId, ConsoleId, At, Updates).

%% event（append-only 审计）
append_event(OrgId, Event) -> cs_pg_seat:insert_event(OrgId, Event).
%% REVIEW-3 F-2：调用方事务内变体（canonical 事务并轨写，消息与事件原子可见）。
append_event_in(Conn, OrgId, Event) -> cs_pg_seat:insert_event_in(Conn, OrgId, Event).

%% event SSE 读面（BE-S01b：游标裁决 / 键集读页 / 水位）
fetch_event_scope(OrgId, EventId) -> cs_pg_seat:fetch_event_scope(OrgId, EventId).
list_events_page(OrgId, WorkspaceId, AfterId, Limit) ->
    cs_pg_seat:list_events_page(OrgId, WorkspaceId, AfterId, Limit).
event_watermark(OrgId, WorkspaceId) -> cs_pg_seat:event_watermark(OrgId, WorkspaceId).

%% admin provisioning（BE-S01b：单事务开通/修复坐席）
provision_seat(OrgId, WorkspaceId, Provision) ->
    cs_pg_seat:provision_seat(OrgId, WorkspaceId, Provision).
