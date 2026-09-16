%%% @doc cs_store_port 的进程内 fake 实现（test-only；ETS 支持，零 SQL、零 mock 框架）。
%%%
%%% 决策语义**镜像真库**（`cs_pg_session` / `cs_pg_seat` / `cs_pg_token`）：
%%%   * claim：seat 存在 → enabled → active 计数 < max_concurrent →
%%%     (status=queued, version) CAS，恰一步失败即对应错误；
%%%   * close：queued|active + version CAS；rate：closed + 未评 + version CAS；
%%%   * transfer：active + version CAS + 目标坐席必须存在；
%%%   * insert_session：同一 conversation 同时最多一个未关闭 session（部分唯一）。
%%% 并发竞态的**真实裁决**在 cs_pg_tests（真库 + spawn 并发）里覆盖；
%%% 本 fake 只用于 application 编排测试。
-module(cs_fake_store).

-export([
    init/0,
    destroy/0,
    seed_identity_function/3,
    seed_assignment_user/3,
    assignment_user/2,
    next_seq/0,
    events/0,
    events_with_action/1,
    %% C1~C4 列表测试注入/读取面
    put_session_for_list/1,
    put_shop_key_for_list/1,
    put_visit_token_for_list/1,
    put_seat_for_list/2,
    last_page_limit/0,
    %% cs_store_port callbacks
    fetch_identity_function/2,
    insert_seat/2,
    fetch_seat/2,
    list_dispatchable_seats/1,
    list_dispatchable_seats_page/3,
    set_seat_enabled/4,
    insert_session/3,
    fetch_session/3,
    claim_session/7,
    transfer_session/7,
    close_session/7,
    rate_session/7,
    list_sessions_for_contact/3,
    list_sessions_page/5,
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
    append_event/2
]).

-define(TAB, cs_fake_store_tab).

%% ===================================================================
%% 生命周期 / 种子
%% ===================================================================

init() ->
    catch ets:delete(?TAB),
    ets:new(?TAB, [named_table, public, set]),
    ets:insert(?TAB, [
        {counter, 0},
        {seats, #{}},
        {sessions, #{}},
        {shop_keys, #{}},
        {visit_tokens, #{}},
        {events, []},
        {identity_functions, #{}},
        {assignment_users, #{}}
    ]),
    ok.

destroy() ->
    catch ets:delete(?TAB),
    ok.

seed_identity_function(OrgId, IdentityId, FunctionKey) ->
    update(identity_functions, fun(M) -> M#{{OrgId, IdentityId} => FunctionKey} end).

%% rebind 模拟：identity ↔ user 的经办映射（cs 代码从不读它——这正是 A04）。
seed_assignment_user(OrgId, IdentityId, UserId) ->
    update(assignment_users, fun(M) -> M#{{OrgId, IdentityId} => UserId} end).

assignment_user(OrgId, IdentityId) ->
    {assignment_users, M} = hd(ets:lookup(?TAB, assignment_users)),
    maps:get({OrgId, IdentityId}, M, undefined).

%% ===================================================================
%% cs_store_port callbacks
%% ===================================================================

fetch_identity_function(OrgId, IdentityId) ->
    {identity_functions, M} = hd(ets:lookup(?TAB, identity_functions)),
    case maps:get({OrgId, IdentityId}, M, undefined) of
        undefined -> {error, not_found};
        FunctionKey -> {ok, FunctionKey}
    end.

insert_seat(OrgId, Seat) ->
    IdentityId = maps:get(business_identity_id, Seat),
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    case maps:is_key({OrgId, IdentityId}, Seats) of
        true ->
            {error, conflict};
        false ->
            Row = Seat#{
                version => 1,
                created_at => 1700000000,
                updated_at => 1700000000
            },
            update(seats, fun(M) -> M#{{OrgId, IdentityId} => Row} end),
            {ok, Row}
    end.

fetch_seat(OrgId, IdentityId) ->
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    case maps:get({OrgId, IdentityId}, Seats, undefined) of
        undefined -> {error, not_found};
        Row -> {ok, Row}
    end.

list_dispatchable_seats(OrgId) ->
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    Rows = [
        with_active_count(OrgId, Row)
     || {{Org, _Id}, Row} <- maps:to_list(Seats),
        Org =:= OrgId,
        maps:get(enabled, Row, false) =:= true
    ],
    {ok,
        lists:sort(
            fun(A, B) ->
                maps:get(business_identity_id, A) =< maps:get(business_identity_id, B)
            end,
            Rows
        )}.

%% ===================================================================
%% C1~C4 列表 callbacks（镜像 cs_pg_* 的键集语义：排序 + after 过滤 + LIMIT）
%% ===================================================================

list_dispatchable_seats_page(OrgId, AfterId, Limit) ->
    note_limit(Limit),
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    Rows = [
        project_page_seat(with_active_count(OrgId, Row))
     || {{Org, _Id}, Row} <- maps:to_list(Seats),
        Org =:= OrgId,
        maps:get(enabled, Row, false) =:= true,
        maps:get(business_identity_id, Row) > AfterId
    ],
    {ok, take(Rows, Limit)}.

%% 列表页行只含列表 SQL 的列（镜像 SQL_LIST_DISPATCHABLE_PAGE 的 SELECT 列表）。
project_page_seat(Row) ->
    maps:with(
        [
            organization_id,
            business_identity_id,
            function_key,
            enabled,
            max_concurrent,
            active_count
        ],
        Row
    ).

list_sessions_page(OrgId, WorkspaceId, Status, AfterId, Limit) ->
    note_limit(Limit),
    {sessions, Sessions} = hd(ets:lookup(?TAB, sessions)),
    Rows = [
        S
     || S <- maps:values(Sessions),
        maps:get(organization_id, S) =:= OrgId,
        maps:get(workspace_id, S) =:= WorkspaceId,
        status_matches(S, Status),
        cursor_pass(maps:get(id, S), AfterId)
    ],
    Ordered = lists:sort(fun(A, B) -> maps:get(id, A) >= maps:get(id, B) end, Rows),
    {ok, take(Ordered, Limit)}.

status_matches(_S, undefined) ->
    true;
status_matches(S, StatusBin) when is_binary(StatusBin) ->
    %% SQL text 参数与行的 status 同为文本形态比较。
    maps:get(status, S) =:= StatusBin orelse
        atom_to_binary(maps:get(status, S), utf8) =:= StatusBin;
status_matches(_S, _Other) ->
    false.

list_shop_keys_page(OrgId, AfterId, Limit) ->
    note_limit(Limit),
    {shop_keys, Keys} = hd(ets:lookup(?TAB, shop_keys)),
    Rows = [
        K
     || K <- maps:values(Keys),
        maps:get(organization_id, K) =:= OrgId,
        cursor_pass(maps:get(id, K), AfterId)
    ],
    Ordered = lists:sort(fun(A, B) -> maps:get(id, A) >= maps:get(id, B) end, Rows),
    {ok, take(Ordered, Limit)}.

list_visit_tokens_page(OrgId, AfterId, Limit) ->
    note_limit(Limit),
    {visit_tokens, Tokens} = hd(ets:lookup(?TAB, visit_tokens)),
    Rows = [
        T
     || T <- maps:values(Tokens),
        maps:get(organization_id, T) =:= OrgId,
        cursor_pass(maps:get(id, T), AfterId)
    ],
    Ordered = lists:sort(fun(A, B) -> maps:get(id, A) >= maps:get(id, B) end, Rows),
    {ok, take(Ordered, Limit)}.

%% DESC 键集镜像：after=0 首页，否则取比游标更小的 id。
cursor_pass(Id, AfterId) -> AfterId =:= 0 orelse Id < AfterId.

take(Rows, Limit) ->
    {Taken, _} = lists:split(min(Limit, length(Rows)), Rows),
    Taken.

note_limit(Limit) ->
    ets:insert(?TAB, {last_page_limit, Limit}),
    ok.

%% @doc 最近一次分页读取收到的 limit（application 缺省值/边界断言用）。
last_page_limit() ->
    case ets:lookup(?TAB, last_page_limit) of
        [{last_page_limit, L}] -> L;
        [] -> undefined
    end.

%% —— 列表测试注入面（绕过 insert 语义直接置行，测试专用）——

put_session_for_list(Row) ->
    update(sessions, fun(M) -> M#{maps:get(id, Row) => Row} end),
    ok.

put_shop_key_for_list(Row) ->
    update(shop_keys, fun(M) -> M#{maps:get(id, Row) => Row} end),
    ok.

put_visit_token_for_list(Row) ->
    update(visit_tokens, fun(M) -> M#{maps:get(id, Row) => Row} end),
    ok.

put_seat_for_list(OrgId, Row) ->
    IdentityId = maps:get(business_identity_id, Row),
    update(seats, fun(M) -> M#{{OrgId, IdentityId} => Row} end),
    ok.

with_active_count(OrgId, Row) ->
    {sessions, Sessions} = hd(ets:lookup(?TAB, sessions)),
    IdentityId = maps:get(business_identity_id, Row),
    Active = length([
        S
     || S <- maps:values(Sessions),
        maps:get(organization_id, S) =:= OrgId,
        maps:get(business_identity_id, S, undefined) =:= IdentityId,
        maps:get(status, S) =:= active
    ]),
    Row#{active_count => Active}.

set_seat_enabled(OrgId, IdentityId, Enabled, At) ->
    case fetch_seat(OrgId, IdentityId) of
        {error, _} = Err ->
            Err;
        {ok, Row} ->
            NewRow = Row#{
                enabled => Enabled, version => maps:get(version, Row) + 1, updated_at => At
            },
            update(seats, fun(M) -> M#{{OrgId, IdentityId} => NewRow} end),
            {ok, NewRow}
    end.

insert_session(OrgId, WorkspaceId, Draft) ->
    SessionId = maps:get(id, Draft),
    {sessions, Sessions} = hd(ets:lookup(?TAB, sessions)),
    ConversationId = maps:get(conversation_id, Draft),
    Open = [
        S
     || S <- maps:values(Sessions),
        maps:get(organization_id, S) =:= OrgId,
        maps:get(conversation_id, S) =:= ConversationId,
        maps:get(status, S) =/= closed
    ],
    case Open of
        [_ | _] ->
            {error, conflict};
        [] ->
            Row = Draft#{
                organization_id => OrgId,
                workspace_id => WorkspaceId,
                status => queued,
                business_identity_id => undefined,
                rating => undefined,
                rating_at => undefined,
                claimed_at => undefined,
                closed_at => undefined,
                close_reason => undefined,
                version => 1
            },
            update(sessions, fun(M) -> M#{SessionId => Row} end),
            {ok, Row}
    end.

fetch_session(OrgId, WorkspaceId, SessionId) ->
    {sessions, Sessions} = hd(ets:lookup(?TAB, sessions)),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            {error, not_found};
        Row ->
            case
                maps:get(organization_id, Row) =:= OrgId andalso
                    maps:get(workspace_id, Row) =:= WorkspaceId
            of
                true -> {ok, Row};
                false -> {error, not_found}
            end
    end.

claim_session(OrgId, WorkspaceId, SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event) ->
    case fetch_session(OrgId, WorkspaceId, SessionId) of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            case fetch_seat(OrgId, IdentityId) of
                {error, _} = Err ->
                    Err;
                {ok, Seat} ->
                    claim_cas(
                        OrgId,
                        WorkspaceId,
                        Session,
                        SessionId,
                        IdentityId,
                        ExpectedVersion,
                        ClaimedAt,
                        Event,
                        Seat
                    )
            end
    end.

claim_cas(
    OrgId, _WorkspaceId, Session, _SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event, Seat
) ->
    ActiveCount = active_count(OrgId, IdentityId),
    Max = maps:get(max_concurrent, Seat),
    Status = maps:get(status, Session),
    Version = maps:get(version, Session),
    Enabled = maps:get(enabled, Seat),
    if
        Enabled =:= false ->
            {error, seat_disabled};
        ActiveCount >= Max ->
            {error, seat_at_capacity};
        Status =/= queued orelse Version =/= ExpectedVersion ->
            {error, conflict};
        true ->
            NewSession = Session#{
                status => active,
                business_identity_id => IdentityId,
                claimed_at => ClaimedAt,
                version => Version + 1,
                updated_at => ClaimedAt
            },
            put_session(NewSession),
            append_event(OrgId, Event),
            {ok, NewSession}
    end.

transfer_session(OrgId, WorkspaceId, SessionId, ToIdentityId, ExpectedVersion, At, Event) ->
    cas_advance(
        OrgId,
        WorkspaceId,
        SessionId,
        ExpectedVersion,
        At,
        Event,
        fun(Session) ->
            case maps:get(status, Session) of
                active ->
                    case fetch_seat(OrgId, ToIdentityId) of
                        {error, _} ->
                            conflict;
                        {ok, _} ->
                            {ok, Session#{
                                business_identity_id => ToIdentityId,
                                version => maps:get(version, Session) + 1,
                                updated_at => At
                            }}
                    end;
                _ ->
                    conflict
            end
        end
    ).

close_session(OrgId, WorkspaceId, SessionId, Reason, ExpectedVersion, At, Event) ->
    cas_advance(
        OrgId,
        WorkspaceId,
        SessionId,
        ExpectedVersion,
        At,
        Event,
        fun(Session) ->
            case lists:member(maps:get(status, Session), [queued, active]) of
                false ->
                    conflict;
                true ->
                    {ok, Session#{
                        status => closed,
                        closed_at => At,
                        close_reason => Reason,
                        version => maps:get(version, Session) + 1,
                        updated_at => At
                    }}
            end
        end
    ).

rate_session(OrgId, WorkspaceId, SessionId, Rating, ExpectedVersion, At, Event) ->
    cas_advance(
        OrgId,
        WorkspaceId,
        SessionId,
        ExpectedVersion,
        At,
        Event,
        fun(Session) ->
            Closed = maps:get(status, Session) =:= closed,
            Unrated = maps:get(rating, Session, undefined) =:= undefined,
            VersionOk = maps:get(version, Session) =:= ExpectedVersion,
            case Closed andalso Unrated andalso VersionOk of
                false ->
                    conflict;
                true ->
                    {ok, Session#{
                        rating => Rating,
                        rating_at => At,
                        version => maps:get(version, Session) + 1,
                        updated_at => At
                    }}
            end
        end
    ).

cas_advance(OrgId, WorkspaceId, SessionId, _ExpectedVersion, _At, Event, Mutate) ->
    case fetch_session(OrgId, WorkspaceId, SessionId) of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            case Mutate(Session) of
                conflict ->
                    {error, conflict};
                {error, _} = Err ->
                    Err;
                {ok, NewSession} ->
                    put_session(NewSession),
                    append_event(OrgId, Event),
                    {ok, NewSession}
            end
    end.

list_sessions_for_contact(OrgId, WorkspaceId, ContactId) ->
    {sessions, Sessions} = hd(ets:lookup(?TAB, sessions)),
    Rows = [
        S
     || S <- maps:values(Sessions),
        maps:get(organization_id, S) =:= OrgId,
        maps:get(workspace_id, S) =:= WorkspaceId,
        maps:get(contact_id, S) =:= ContactId
    ],
    {ok, lists:sort(fun(A, B) -> maps:get(id, A) =< maps:get(id, B) end, Rows)}.

insert_shop_key(OrgId, Key) ->
    KeyId = maps:get(id, Key),
    {shop_keys, Keys} = hd(ets:lookup(?TAB, shop_keys)),
    Digest = maps:get(key_digest, Key),
    Dup = [
        K
     || K <- maps:values(Keys),
        maps:get(organization_id, K) =:= OrgId,
        maps:get(key_digest, K) =:= Digest
    ],
    case Dup of
        [_ | _] ->
            {error, conflict};
        [] ->
            Row = Key#{
                status => active,
                revoked_at => undefined,
                version => 1,
                created_at => 1700000000,
                updated_at => 1700000000
            },
            update(shop_keys, fun(M) -> M#{KeyId => Row} end),
            {ok, Row}
    end.

fetch_shop_key(OrgId, KeyId) ->
    {shop_keys, Keys} = hd(ets:lookup(?TAB, shop_keys)),
    case maps:get(KeyId, Keys, undefined) of
        undefined ->
            {error, not_found};
        Row ->
            case same_org(Row, OrgId) of
                true -> {ok, Row};
                false -> {error, not_found}
            end
    end.

fetch_shop_key_by_digest(OrgId, Digest) ->
    {shop_keys, Keys} = hd(ets:lookup(?TAB, shop_keys)),
    Match = [
        K
     || K <- maps:values(Keys),
        maps:get(organization_id, K) =:= OrgId,
        maps:get(key_digest, K) =:= Digest
    ],
    case Match of
        [Row | _] -> {ok, Row};
        [] -> {error, not_found}
    end.

revoke_shop_key(OrgId, KeyId, At) ->
    case fetch_shop_key(OrgId, KeyId) of
        {error, _} = Err ->
            Err;
        {ok, Row} ->
            case maps:get(status, Row) of
                active ->
                    NewRow = Row#{
                        status => revoked, revoked_at => At, version => maps:get(version, Row) + 1
                    },
                    update(shop_keys, fun(M) -> M#{KeyId => NewRow} end),
                    ok;
                _ ->
                    {error, not_found}
            end
    end.

insert_visit_token(_OrgId, Token) ->
    TokenId = maps:get(id, Token),
    {visit_tokens, Tokens} = hd(ets:lookup(?TAB, visit_tokens)),
    Row = Token#{
        revoked_at => undefined,
        display_hint => maps:get(display_hint, Token, undefined),
        version => 1,
        created_at => 1700000000,
        updated_at => 1700000000
    },
    update(visit_tokens, fun(M) -> M#{TokenId => Row} end),
    _ = Tokens,
    {ok, Row}.

fetch_visit_token(OrgId, TokenId) ->
    {visit_tokens, Tokens} = hd(ets:lookup(?TAB, visit_tokens)),
    case maps:get(TokenId, Tokens, undefined) of
        undefined ->
            {error, not_found};
        Row ->
            case same_org(Row, OrgId) of
                true -> {ok, Row};
                false -> {error, not_found}
            end
    end.

fetch_visit_token_by_digest(OrgId, Digest) ->
    {visit_tokens, Tokens} = hd(ets:lookup(?TAB, visit_tokens)),
    Match = [
        T
     || T <- maps:values(Tokens),
        maps:get(organization_id, T) =:= OrgId,
        maps:get(token_digest, T) =:= Digest
    ],
    case Match of
        [Row | _] -> {ok, Row};
        [] -> {error, not_found}
    end.

revoke_visit_token(OrgId, TokenId, At) ->
    case fetch_visit_token(OrgId, TokenId) of
        {error, _} = Err ->
            Err;
        {ok, Row} ->
            case maps:get(revoked_at, Row) of
                undefined ->
                    NewRow = Row#{revoked_at => At, version => maps:get(version, Row) + 1},
                    update(visit_tokens, fun(M) -> M#{TokenId => NewRow} end),
                    ok;
                _ ->
                    {error, not_found}
            end
    end.

append_event(OrgId, Event) ->
    EventId = next_counter(),
    update(events, fun(L) -> L ++ [Event#{id => EventId, organization_id => OrgId}] end),
    {ok, EventId}.

%% ===================================================================
%% 测试读取面（断言用）
%% ===================================================================

events() ->
    {events, L} = hd(ets:lookup(?TAB, events)),
    L.

events_with_action(Action) ->
    [E || E <- events(), maps:get(action, E) =:= Action].

%% @doc 每次调用递增的唯一序号（测试造独立 conversation id 用）。
next_seq() ->
    next_counter().

next_counter() ->
    [{counter, N}] = ets:lookup(?TAB, counter),
    ets:insert(?TAB, {counter, N + 1}),
    N + 1000.

%% ===================================================================
%% 内部辅助
%% ===================================================================

same_org(Row, OrgId) ->
    maps:get(organization_id, Row, undefined) =:= OrgId.

update(Key, Fun) ->
    [{Key, Value}] = ets:lookup(?TAB, Key),
    ets:insert(?TAB, {Key, Fun(Value)}),
    ok.

put_session(Session) ->
    SessionId = maps:get(id, Session),
    update(sessions, fun(M) -> M#{SessionId => Session} end),
    ok.

active_count(OrgId, IdentityId) ->
    {sessions, Sessions} = hd(ets:lookup(?TAB, sessions)),
    length([
        S
     || S <- maps:values(Sessions),
        maps:get(organization_id, S) =:= OrgId,
        maps:get(business_identity_id, S, undefined) =:= IdentityId,
        maps:get(status, S) =:= active
    ]).
