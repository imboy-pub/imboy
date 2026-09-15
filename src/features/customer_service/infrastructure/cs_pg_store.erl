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
    set_seat_enabled/4,
    %% session
    insert_session/3,
    fetch_session/3,
    claim_session/7,
    transfer_session/7,
    close_session/7,
    rate_session/7,
    list_sessions_for_contact/3,
    %% shop key / visit token
    insert_shop_key/2,
    fetch_shop_key/2,
    fetch_shop_key_by_digest/2,
    revoke_shop_key/3,
    insert_visit_token/2,
    fetch_visit_token/2,
    fetch_visit_token_by_digest/2,
    revoke_visit_token/3,
    %% event
    append_event/2
]).

%% identity 事实（A01）
fetch_identity_function(OrgId, IdentityId) ->
    cs_pg_seat:fetch_identity_function(OrgId, IdentityId).

%% seat
insert_seat(OrgId, Seat) -> cs_pg_seat:insert_seat(OrgId, Seat).
fetch_seat(OrgId, IdentityId) -> cs_pg_seat:fetch_seat(OrgId, IdentityId).
list_dispatchable_seats(OrgId) -> cs_pg_seat:list_dispatchable_seats(OrgId).
set_seat_enabled(OrgId, IdentityId, Enabled, At) ->
    cs_pg_seat:set_seat_enabled(OrgId, IdentityId, Enabled, At).

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

%% shop key / visit token
insert_shop_key(OrgId, Key) -> cs_pg_token:insert_shop_key(OrgId, Key).
fetch_shop_key(OrgId, KeyId) -> cs_pg_token:fetch_shop_key(OrgId, KeyId).
fetch_shop_key_by_digest(OrgId, Digest) -> cs_pg_token:fetch_shop_key_by_digest(OrgId, Digest).
revoke_shop_key(OrgId, KeyId, At) -> cs_pg_token:revoke_shop_key(OrgId, KeyId, At).
insert_visit_token(OrgId, Token) -> cs_pg_token:insert_visit_token(OrgId, Token).
fetch_visit_token(OrgId, TokenId) -> cs_pg_token:fetch_visit_token(OrgId, TokenId).
fetch_visit_token_by_digest(OrgId, Digest) ->
    cs_pg_token:fetch_visit_token_by_digest(OrgId, Digest).
revoke_visit_token(OrgId, TokenId, At) -> cs_pg_token:revoke_visit_token(OrgId, TokenId, At).

%% event（append-only 审计）
append_event(OrgId, Event) -> cs_pg_seat:insert_event(OrgId, Event).
