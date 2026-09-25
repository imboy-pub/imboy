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
    seed_workspace/2,
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
    %% BE-S01a：坐席上下文 / 转接目标种子与读取面
    put_org_context/1,
    put_identity_display/3,
    %% 平台全局面坐席分页的种子（SQL 侧 JOIN organization 的 fake 镜像）
    seed_org/2,
    %% BE-S01b：provisioning 前置事实种子与故障注入
    seed_member/2,
    seed_provision_fail_after/1,
    %% cs_store_port callbacks
    fetch_identity_function/2,
    insert_seat/2,
    fetch_seat/2,
    list_dispatchable_seats/1,
    list_dispatchable_seats_page/3,
    list_all_seats_page/3,
    set_seat_enabled/4,
    list_seat_org_contexts/1,
    list_transfer_targets_page/4,
    %% CS-BE-06：席位 entitlement（内存 fake；seat_limits 表）
    seat_limit/1,
    set_seat_limit/2,
    create_seat_limit_checked/5,
    set_enabled_checked/4,
    %% CS-BE-05：presence（内存表：{Org, Identity} => #{last_heartbeat_at, manual_status}）
    heartbeat_seat/4,
    set_seat_manual_status/4,
    fetch_seat_presence/2,
    list_seat_presence/1,
    insert_session/3,
    fetch_session/3,
    claim_session/7,
    transfer_session/7,
    close_session/7,
    rate_session/7,
    list_sessions_for_contact/3,
    list_sessions_page/5,
    seat_session_page/5,
    default_workspace/1,
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
    %% widget installation / identity key / bootstrap token / nonce（CSB-01/02）
    insert_widget_installation/2,
    fetch_widget_installation/2,
    fetch_widget_installation_by_public_id/2,
    fetch_widget_installation_by_public_id_global/1,
    list_widget_installations_page/3,
    revoke_widget_installation/3,
    %% CSD-BE-01：测试注入面——强制 installation 状态（disabled 三态归一用）
    force_widget_installation_status/2,
    insert_widget_identity_key/3,
    fetch_widget_identity_key/3,
    revoke_widget_identity_key/4,
    insert_widget_bootstrap_token/2,
    fetch_widget_bootstrap_token_by_digest/3,
    fetch_widget_bootstrap_token_by_digest_global/2,
    touch_widget_bootstrap_token/4,
    revoke_widget_bootstrap_token/4,
    record_widget_nonce/4,
    append_event/2,
    %% BE-S01b：SSE 读面 + admin provisioning
    fetch_event_scope/2,
    list_events_page/4,
    event_watermark/2,
    provision_seat/3,
    %% CS-BE-04：已读游标（单调 ACK 幂等 + 未读事实现算的 fake 镜像）
    seed_message/4,
    ack_session_read/6,
    fetch_session_read_state/4
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
        {orgs, #{}},
        {sessions, #{}},
        {shop_keys, #{}},
        {visit_tokens, #{}},
        {widget_installations, #{}},
        {widget_identity_keys, #{}},
        {widget_nonces, #{}},
        {events, []},
        {identity_functions, #{}},
        {assignment_users, #{}},
        {workspaces, #{}},
        {org_contexts, #{}},
        {identity_displays, #{}},
        {members, #{}},
        {provision_fail_after, infinity},
        {read_cursors, #{}},
        {messages, []},
        {seat_presence, #{}},
        {seat_limits, #{}}
    ]),
    ok.

destroy() ->
    catch ets:delete(?TAB),
    ok.

seed_identity_function(OrgId, IdentityId, FunctionKey) ->
    update(identity_functions, fun(M) -> M#{{OrgId, IdentityId} => FunctionKey} end).

%% CSB-02R：default_workspace 解析种子（Org → 最小 workspace id 的行）。
seed_workspace(OrgId, WorkspaceId) ->
    update(workspaces, fun(M) ->
        M#{WorkspaceId => #{id => WorkspaceId, organization_id => OrgId, status => active}}
    end).

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

%% 平台运营面坐席分页（镜像 SQL_LIST_ALL_SEATS_PAGE：跨企业可选 Org 过滤、
%% 不按 enabled 过滤、键集升序 + LIMIT；行含企业名/显示名/默认 workspace）。
list_all_seats_page(OrgFilter, AfterId, Limit) ->
    note_limit(Limit),
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    {orgs, Orgs} = hd(ets:lookup(?TAB, orgs)),
    {workspaces, WsMap} = hd(ets:lookup(?TAB, workspaces)),
    {identity_displays, Displays} = hd(ets:lookup(?TAB, identity_displays)),
    Rows0 = [
        begin
            IdentityId = maps:get(business_identity_id, Row),
            Row0 = with_active_count(Org, Row),
            project_all_seats_row(
                Row0#{
                    organization_name => org_name(Orgs, Org),
                    display_name => maps:get({Org, IdentityId}, Displays, undefined),
                    workspace_id => default_ws_id(WsMap, Org)
                }
            )
        end
     || {{Org, _Id}, Row} <- maps:to_list(Seats),
        OrgFilter =:= 0 orelse Org =:= OrgFilter,
        maps:get(business_identity_id, Row) > AfterId
    ],
    Ordered = lists:sort(
        fun(A, B) ->
            maps:get(business_identity_id, A) =< maps:get(business_identity_id, B)
        end,
        Rows0
    ),
    {ok, take(Ordered, Limit)}.

%% 列表页行只含列表 SQL 的列（镜像 SQL_LIST_ALL_SEATS_PAGE 的 SELECT 列表）。
project_all_seats_row(Row) ->
    maps:with(
        [
            organization_id,
            organization_name,
            display_name,
            business_identity_id,
            function_key,
            enabled,
            max_concurrent,
            active_count,
            workspace_id
        ],
        Row
    ).

org_name(Orgs, OrgId) ->
    case maps:get(OrgId, Orgs, undefined) of
        #{name := Name} -> Name;
        _ -> <<>>
    end.

%% SQL 侧 LATERAL 的 fake 镜像：该 Org 最小 active workspace id（无则 undefined）。
default_ws_id(WsMap, OrgId) ->
    Ids = [
        maps:get(id, W)
     || W <- maps:values(WsMap),
        maps:get(organization_id, W, undefined) =:= OrgId,
        maps:get(status, W, inactive) =:= active
    ],
    case Ids of
        [] -> undefined;
        _ -> lists:min(Ids)
    end.

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

%% CSB-02R：坐席工作台分页（同作用域稳定计数；掩码/末条摘要原料列由
%% 行直接携带——store 行外无第二真相源）。
seat_session_page(OrgId, Status, AfterId, Limit, WorkspaceId) ->
    note_limit(Limit),
    {sessions, Sessions} = hd(ets:lookup(?TAB, sessions)),
    %% 同作用域（Org + workspace 收窄）全集：计数与列表同口径。
    InScopeAll = [
        S
     || S <- maps:values(Sessions),
        maps:get(organization_id, S) =:= OrgId,
        WorkspaceId =:= 0 orelse maps:get(workspace_id, S) =:= WorkspaceId
    ],
    InScope = [
        S
     || S <- InScopeAll,
        status_matches(S, Status),
        cursor_pass(maps:get(id, S), AfterId)
    ],
    Ordered = lists:sort(fun(A, B) -> maps:get(id, A) >= maps:get(id, B) end, InScope),
    %% DF-8：queued/active/closed 三键恒在、零计数显式出 0（与 cs_pg_session
    %% 的构建层默认对齐）——前端 toCounts 要求三键均为数字，缺键即 TypeError。
    TotalByStatus =
        lists:foldl(
            fun(S, Acc) ->
                K = atom_to_binary(maps:get(status, S), utf8),
                Acc#{K => maps:get(K, Acc, 0) + 1}
            end,
            #{<<"queued">> => 0, <<"active">> => 0, <<"closed">> => 0},
            InScopeAll
        ),
    Total = maps:get(binary_status(Status), TotalByStatus, 0),
    {ok, #{rows => take(Ordered, Limit), total => Total, total_by_status => TotalByStatus}}.

binary_status(StatusBin) when is_binary(StatusBin) -> StatusBin;
binary_status(Status) when is_atom(Status) -> atom_to_binary(Status, utf8).

%% CSB-02R：widget 装配缺省 Workspace 解析（fake = 该 Org 最小 workspace 行）。
default_workspace(OrgId) ->
    {workspaces, Workspaces} = hd(ets:lookup(?TAB, workspaces)),
    case
        lists:sort([
            maps:get(id, W)
         || W <- maps:values(Workspaces),
            maps:get(organization_id, W, undefined) =:= OrgId
        ])
    of
        [Min | _] -> {ok, Min};
        [] -> {error, not_found}
    end.

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
    Row#{active_count => fake_active_count(OrgId, maps:get(business_identity_id, Row))}.

fake_active_count(OrgId, IdentityId) ->
    {sessions, Sessions} = hd(ets:lookup(?TAB, sessions)),
    length([
        S
     || S <- maps:values(Sessions),
        maps:get(organization_id, S) =:= OrgId,
        maps:get(business_identity_id, S, undefined) =:= IdentityId,
        maps:get(status, S) =:= active
    ]).

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

%% ===================================================================
%% BE-S01a：坐席上下文聚合 / 转接目标（镜像 cs_pg_seat 的过滤与键集语义）
%% ===================================================================

%% 平台全局面坐席分页种子：organization 行（SQL 侧 JOIN organization.name）。
seed_org(OrgId, Name) ->
    update(orgs, fun(M) -> M#{OrgId => #{id => OrgId, name => Name}} end).

%% 坐席上下文种子：Row 形如真库投影
%% #{user_id, organization_id, organization_name, business_identity_id,
%%   seat_enabled, workspaces => [#{id, name}]}。
put_org_context(Row) ->
    UserId = maps:get(user_id, Row),
    update(org_contexts, fun(M) -> M#{UserId => [Row | maps_get_list(UserId, M)]} end).

maps_get_list(K, M) ->
    case maps:get(K, M, undefined) of
        L when is_list(L) -> L;
        _ -> []
    end.

%% identity 显示名种子（转接目标投影用）。
put_identity_display(OrgId, IdentityId, DisplayName) ->
    update(identity_displays, fun(M) -> M#{{OrgId, IdentityId} => DisplayName} end).

list_seat_org_contexts(UserId) ->
    {org_contexts, M} = hd(ets:lookup(?TAB, org_contexts)),
    {ok, lists:sort(maps_get_list(UserId, M))}.

%% ===================================================================
%% CS-BE-06：席位 entitlement（内存 fake）
%% ===================================================================

seat_limit(OrgId) ->
    {seat_limits, M} = hd(ets:lookup(?TAB, seat_limits)),
    case maps:get(OrgId, M, undefined) of
        N when is_integer(N), N >= 1 -> {ok, N};
        _ -> {ok, unlimited}
    end.

set_seat_limit(OrgId, undefined) ->
    {seat_limits, M} = hd(ets:lookup(?TAB, seat_limits)),
    ets:insert(?TAB, {seat_limits, maps:remove(OrgId, M)}),
    {ok, unlimited};
set_seat_limit(OrgId, Limit) when is_integer(Limit), Limit >= 1 ->
    {seat_limits, M} = hd(ets:lookup(?TAB, seat_limits)),
    ets:insert(?TAB, {seat_limits, maps:put(OrgId, Limit, M)}),
    {ok, Limit}.

create_seat_limit_checked(OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy) ->
    case Enabled of
        false ->
            insert_seat(OrgId, full_seat_map(OrgId, IdentityId, false, MaxConcurrent, CreatedBy));
        true ->
            Limit = limit_of(OrgId),
            Used = used_count(OrgId),
            case Used >= Limit of
                true ->
                    {error, seat_limit_exceeded};
                false ->
                    insert_seat(
                        OrgId, full_seat_map(OrgId, IdentityId, true, MaxConcurrent, CreatedBy)
                    )
            end
    end.

%% 与 app 原构造同形（function_key/organization_id 必在——fetch 回读投影依赖）。
full_seat_map(OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy) ->
    #{
        organization_id => OrgId,
        business_identity_id => IdentityId,
        function_key => <<"customer_service">>,
        enabled => Enabled,
        max_concurrent => MaxConcurrent,
        created_by_user_id => CreatedBy
    }.

set_enabled_checked(OrgId, IdentityId, Enabled, At) ->
    case set_seat_enabled(OrgId, IdentityId, Enabled, At) of
        {ok, _} = Ok ->
            Ok;
        {error, not_active} ->
            %% 假体语义对齐：fake 的 set_seat_enabled 在已启用重放时返回
            %% not_active——重放应幂等成功（回读现态）。
            case fetch_seat(OrgId, IdentityId) of
                {ok, Row} -> {ok, Row};
                Err -> Err
            end;
        Err ->
            Err
    end.

limit_of(OrgId) ->
    case seat_limit(OrgId) of
        {ok, unlimited} -> 999999;
        {ok, N} -> N
    end.

used_count(OrgId) ->
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    length([
        1
     || {{O, _I}, S} <- maps:to_list(Seats),
        O =:= OrgId,
        maps:get(enabled, S, false) =:= true
    ]).

%% ===================================================================
%% CS-BE-05：presence（内存 fake；跨 init 隔离）
%% ===================================================================

heartbeat_seat(OrgId, IdentityId, AtSec, _Opts) ->
    ensure_presence_row(OrgId, IdentityId),
    {seat_presence, M} = hd(ets:lookup(?TAB, seat_presence)),
    Row = maps:get({OrgId, IdentityId}, M),
    Row1 = Row#{last_heartbeat_at => AtSec},
    ets:insert(?TAB, {seat_presence, maps:put({OrgId, IdentityId}, Row1, M)}),
    presence_view(OrgId, IdentityId).

set_seat_manual_status(OrgId, IdentityId, AtSec, ManualStatus) ->
    ensure_presence_row(OrgId, IdentityId),
    {seat_presence, M} = hd(ets:lookup(?TAB, seat_presence)),
    Row0 = maps:get({OrgId, IdentityId}, M),
    Row1 = Row0#{
        manual_status =>
            case ManualStatus of
                <<"away">> -> <<"away">>;
                _ -> undefined
            end,
        last_heartbeat_at => maps:get(last_heartbeat_at, Row0, AtSec)
    },
    ets:insert(?TAB, {seat_presence, maps:put({OrgId, IdentityId}, Row1, M)}),
    presence_view(OrgId, IdentityId).

fetch_seat_presence(OrgId, IdentityId) ->
    %% seat 不存在 → not_found（与 PG LEFT JOIN 语义一致：锚在 seat 行）。
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    case maps:get({OrgId, IdentityId}, Seats, undefined) of
        undefined ->
            {error, not_found};
        Seat ->
            ensure_presence_row(OrgId, IdentityId),
            {seat_presence, M} = hd(ets:lookup(?TAB, seat_presence)),
            P = maps:get({OrgId, IdentityId}, M),
            {ok, presence_row(P, Seat)}
    end.

list_seat_presence(OrgId) ->
    {seat_presence, M} = hd(ets:lookup(?TAB, seat_presence)),
    {ok, [
        maps:with(
            [organization_id, business_identity_id, last_heartbeat_at, manual_status], P
        )
     || {{O, _I}, P} <- maps:to_list(M),
        O =:= OrgId,
        is_map_key(last_heartbeat_at, P) orelse is_map_key(manual_status, P)
    ]}.

ensure_presence_row(OrgId, IdentityId) ->
    {seat_presence, M} = hd(ets:lookup(?TAB, seat_presence)),
    case maps:is_key({OrgId, IdentityId}, M) of
        true ->
            ok;
        false ->
            ets:insert(
                ?TAB,
                {seat_presence,
                    maps:put(
                        {OrgId, IdentityId},
                        #{organization_id => OrgId, business_identity_id => IdentityId},
                        M
                    )}
            )
    end.

presence_view(OrgId, IdentityId) ->
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    Seat = maps:get({OrgId, IdentityId}, Seats, #{}),
    {seat_presence, M} = hd(ets:lookup(?TAB, seat_presence)),
    P = maps:get({OrgId, IdentityId}, M),
    {ok, presence_row(P, Seat)}.

presence_row(P, Seat) ->
    Base = maps:with(
        [organization_id, business_identity_id, last_heartbeat_at, manual_status], P
    ),
    Base#{
        enabled => maps:get(enabled, Seat, true),
        max_concurrent => maps:get(max_concurrent, Seat, 1),
        active_count => fake_active_count(
            maps:get(organization_id, P), maps:get(business_identity_id, P)
        )
    }.

list_transfer_targets_page(OrgId, ExcludeIdentityId, AfterId, Limit) ->
    {seats, Seats} = hd(ets:lookup(?TAB, seats)),
    {identity_displays, Displays} = hd(ets:lookup(?TAB, identity_displays)),
    Rows0 = [
        with_active_count(OrgId, Seat#{
            display_name => maps:get(
                {OrgId, maps:get(business_identity_id, Seat)}, Displays, undefined
            )
        })
     || {{O, _I}, Seat} <- maps:to_list(Seats),
        O =:= OrgId,
        maps:get(enabled, Seat, false) =:= true,
        maps:get(business_identity_id, Seat) =/= ExcludeIdentityId,
        maps:get(business_identity_id, Seat) > AfterId
    ],
    Rows1 = lists:sort(
        fun(A, B) ->
            maps:get(business_identity_id, A) =< maps:get(business_identity_id, B)
        end,
        Rows0
    ),
    {ok, lists:sublist(Rows1, Limit)}.

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

%% ===================================================================
%% widget installation / identity key / bootstrap token / nonce callbacks
%% （镜像 cs_pg_widget 的决策语义；digest-only、Org 同语句裁决、23505=replay）
%% ===================================================================

insert_widget_installation(OrgId, Installation) ->
    PublicId = maps:get(public_widget_id, Installation),
    {widget_installations, Insts} = hd(ets:lookup(?TAB, widget_installations)),
    %% uq_cswi_public_widget_id 是**全局**唯一（不分 Org）。
    Dup = [I || I <- maps:values(Insts), maps:get(public_widget_id, I) =:= PublicId],
    case Dup of
        [_ | _] ->
            {error, conflict};
        [] ->
            Id = maps:get(id, Installation),
            Row = Installation#{
                organization_id => OrgId,
                status => active,
                revoked_at => undefined,
                version => 1,
                created_at => 1700000000,
                updated_at => 1700000000
            },
            update(widget_installations, fun(M) -> M#{Id => Row} end),
            {ok, Row}
    end.

fetch_widget_installation(OrgId, InstallationId) ->
    {widget_installations, Insts} = hd(ets:lookup(?TAB, widget_installations)),
    case maps:get(InstallationId, Insts, undefined) of
        undefined ->
            {error, not_found};
        Row ->
            case same_org(Row, OrgId) of
                true -> {ok, Row};
                false -> {error, not_found}
            end
    end.

fetch_widget_installation_by_public_id(OrgId, PublicWidgetId) ->
    {widget_installations, Insts} = hd(ets:lookup(?TAB, widget_installations)),
    Match = [
        I
     || I <- maps:values(Insts),
        maps:get(organization_id, I) =:= OrgId,
        maps:get(public_widget_id, I) =:= PublicWidgetId
    ],
    case Match of
        [Row | _] -> {ok, Row};
        [] -> {error, not_found}
    end.

%% CSD-BE-01（hosted-widget-contract S3）：public_widget_id **全局**反查——
%% 无 Org 输入，镜像真库 `WHERE public_widget_id = $1`（全局唯一 → 单行）；
%% 行的 organization_id 是派生输出， Org 归属证明由命中行本身承担。
fetch_widget_installation_by_public_id_global(PublicWidgetId) ->
    {widget_installations, Insts} = hd(ets:lookup(?TAB, widget_installations)),
    Match = [
        I
     || I <- maps:values(Insts),
        maps:get(public_widget_id, I) =:= PublicWidgetId
    ],
    case Match of
        [Row | _] -> {ok, Row};
        [] -> {error, not_found}
    end.

%% CSD-BE-01 测试注入面：绕过正常生命周期把 installation 置为任意状态
%% （disabled 等 store 正常路径不产出的状态），供三态归一断言使用。
force_widget_installation_status(InstallationId, Status) ->
    {widget_installations, Insts} = hd(ets:lookup(?TAB, widget_installations)),
    case maps:get(InstallationId, Insts, undefined) of
        undefined ->
            {error, not_found};
        Row ->
            update(widget_installations, fun(M) ->
                M#{InstallationId => Row#{status => Status}}
            end),
            ok
    end.

list_widget_installations_page(OrgId, AfterId, Limit) ->
    {widget_installations, Insts} = hd(ets:lookup(?TAB, widget_installations)),
    Rows0 = [
        Row
     || Row <- maps:values(Insts),
        maps:get(organization_id, Row) =:= OrgId,
        AfterId =:= 0 orelse maps:get(id, Row) < AfterId
    ],
    Rows = lists:sublist(
        lists:sort(fun(A, B) -> maps:get(id, A) > maps:get(id, B) end, Rows0), Limit
    ),
    {ok, Rows}.

revoke_widget_installation(OrgId, InstallationId, At) ->
    case fetch_widget_installation(OrgId, InstallationId) of
        {error, _} = Err ->
            Err;
        {ok, Row} ->
            case maps:get(status, Row) of
                active ->
                    NewRow = Row#{
                        status => revoked,
                        revoked_at => At,
                        version => maps:get(version, Row) + 1,
                        updated_at => At
                    },
                    update(widget_installations, fun(M) ->
                        M#{InstallationId => NewRow}
                    end),
                    ok;
                _ ->
                    {error, not_found}
            end
    end.

insert_widget_identity_key(OrgId, InstallationId, Key) ->
    KeyVersion = maps:get(key_version, Key),
    {widget_identity_keys, Keys} = hd(ets:lookup(?TAB, widget_identity_keys)),
    Dup = [
        K
     || K <- maps:values(Keys),
        maps:get(organization_id, K) =:= OrgId,
        maps:get(installation_id, K) =:= InstallationId,
        maps:get(key_version, K) =:= KeyVersion
    ],
    case Dup of
        [_ | _] ->
            {error, conflict};
        [] ->
            Id = maps:get(id, Key),
            Row = Key#{
                organization_id => OrgId,
                installation_id => InstallationId,
                status => active,
                revoked_at => undefined,
                created_at => 1700000000,
                updated_at => 1700000000
            },
            update(widget_identity_keys, fun(M) -> M#{Id => Row} end),
            {ok, Row}
    end.

fetch_widget_identity_key(OrgId, InstallationId, KeyVersion) ->
    {widget_identity_keys, Keys} = hd(ets:lookup(?TAB, widget_identity_keys)),
    Match = [
        K
     || K <- maps:values(Keys),
        maps:get(organization_id, K) =:= OrgId,
        maps:get(installation_id, K) =:= InstallationId,
        maps:get(key_version, K) =:= KeyVersion
    ],
    case Match of
        [Row | _] -> {ok, Row};
        [] -> {error, not_found}
    end.

revoke_widget_identity_key(OrgId, InstallationId, KeyVersion, At) ->
    case fetch_widget_identity_key(OrgId, InstallationId, KeyVersion) of
        {error, _} = Err ->
            Err;
        {ok, Row} ->
            case maps:get(status, Row) of
                active ->
                    NewRow = Row#{
                        status => revoked,
                        revoked_at => At,
                        updated_at => At
                    },
                    update(widget_identity_keys, fun(M) ->
                        M#{maps:get(id, Row) => NewRow}
                    end),
                    ok;
                _ ->
                    {error, not_found}
            end
    end.

%% bootstrap 令牌落 visit_tokens（复用存储，镜像 cs_pg_widget：只带 widget 列）。
insert_widget_bootstrap_token(OrgId, Token) ->
    Digest = maps:get(token_digest, Token),
    {visit_tokens, Tokens} = hd(ets:lookup(?TAB, visit_tokens)),
    %% uq_csvt_org_digest：同 Org 内 digest 唯一。
    Dup = [
        T
     || T <- maps:values(Tokens),
        maps:get(organization_id, T) =:= OrgId,
        maps:get(token_digest, T) =:= Digest
    ],
    case Dup of
        [_ | _] ->
            {error, conflict};
        [] ->
            Id = maps:get(id, Token),
            Row = Token#{
                organization_id => OrgId,
                revoked_at => undefined,
                last_seen_at => undefined,
                display_hint => undefined,
                version => 1,
                created_at => 1700000000,
                updated_at => 1700000000
            },
            update(visit_tokens, fun(M) -> M#{Id => Row} end),
            {ok, Row}
    end.

fetch_widget_bootstrap_token_by_digest(OrgId, InstallationId, Digest) ->
    {visit_tokens, Tokens} = hd(ets:lookup(?TAB, visit_tokens)),
    Match = [
        T
     || T <- maps:values(Tokens),
        maps:get(organization_id, T) =:= OrgId,
        maps:get(widget_installation_id, T, undefined) =:= InstallationId,
        maps:get(token_digest, T) =:= Digest
    ],
    case Match of
        [Row | _] -> {ok, Row};
        [] -> {error, not_found}
    end.

%% CSD-BE-01S：digest 全局命中——无 Org 输入，organization_id 从行输出
%% （持 token 动作面的租户派生真源；与 PG 语句同语义）。
fetch_widget_bootstrap_token_by_digest_global(InstallationId, Digest) ->
    {visit_tokens, Tokens} = hd(ets:lookup(?TAB, visit_tokens)),
    Match = [
        T
     || T <- maps:values(Tokens),
        maps:get(widget_installation_id, T, undefined) =:= InstallationId,
        maps:get(token_digest, T) =:= Digest
    ],
    case Match of
        [Row | _] -> {ok, Row};
        [] -> {error, not_found}
    end.

touch_widget_bootstrap_token(OrgId, InstallationId, TokenId, At) ->
    case fetch_widget_bootstrap_token_row(OrgId, InstallationId, TokenId) of
        {error, _} = Err ->
            Err;
        {ok, Row} ->
            case maps:get(revoked_at, Row) of
                undefined ->
                    NewRow = Row#{last_seen_at => At},
                    update(visit_tokens, fun(M) -> M#{TokenId => NewRow} end),
                    ok;
                _ ->
                    {error, not_found}
            end
    end.

revoke_widget_bootstrap_token(OrgId, InstallationId, TokenId, At) ->
    case fetch_widget_bootstrap_token_row(OrgId, InstallationId, TokenId) of
        {error, _} = Err ->
            Err;
        {ok, Row} ->
            case maps:get(revoked_at, Row) of
                undefined ->
                    NewRow = Row#{
                        revoked_at => At,
                        version => maps:get(version, Row) + 1,
                        updated_at => At
                    },
                    update(visit_tokens, fun(M) -> M#{TokenId => NewRow} end),
                    ok;
                _ ->
                    {error, not_found}
            end
    end.

fetch_widget_bootstrap_token_row(OrgId, InstallationId, TokenId) ->
    {visit_tokens, Tokens} = hd(ets:lookup(?TAB, visit_tokens)),
    case maps:get(TokenId, Tokens, undefined) of
        undefined ->
            {error, not_found};
        Row ->
            case
                maps:get(organization_id, Row) =:= OrgId andalso
                    maps:get(widget_installation_id, Row, undefined) =:= InstallationId
            of
                true -> {ok, Row};
                false -> {error, not_found}
            end
    end.

record_widget_nonce(OrgId, InstallationId, JtiDigest, ExpiresAt) ->
    {widget_nonces, Nonces} = hd(ets:lookup(?TAB, widget_nonces)),
    Key = {OrgId, InstallationId, JtiDigest},
    case maps:is_key(Key, Nonces) of
        true ->
            {error, replay};
        false ->
            update(widget_nonces, fun(M) ->
                M#{Key => #{expires_at => ExpiresAt}}
            end),
            ok
    end.

append_event(OrgId, Event) ->
    EventId = next_counter(),
    update(events, fun(L) -> L ++ [Event#{id => EventId, organization_id => OrgId}] end),
    {ok, EventId}.

%% ===================================================================
%% BE-S01b：SSE 读面（fake 镜像键集升序读页 / 游标裁决 / 水位）+
%% admin provisioning（镜像单事务顺序：workspace/member 门 → identity/assignment
%% 复用或创建 → seat upsert → 审计；`fail_after` 注入点模拟"部分失败"）
%% ===================================================================

fetch_event_scope(OrgId, EventId) ->
    case [E || E <- events(), maps:get(id, E, undefined) =:= EventId] of
        [E | _] ->
            case same_org(E, OrgId) of
                true ->
                    {ok, #{
                        organization_id => maps:get(organization_id, E),
                        workspace_id => maps:get(workspace_id, E, undefined)
                    }};
                false ->
                    %% 按 id 唯一：存在的行不属于本 Org 即跨作用域。
                    {ok, #{
                        organization_id => maps:get(organization_id, E), workspace_id => undefined
                    }}
            end;
        [] ->
            {error, not_found}
    end.

list_events_page(OrgId, WorkspaceId, AfterId, Limit) ->
    Rows =
        [
            E
         || E <- events(),
            maps:get(organization_id, E) =:= OrgId,
            maps:get(workspace_id, E, undefined) =:= WorkspaceId,
            maps:get(id, E, 0) > AfterId
        ],
    Sorted = lists:sort(fun(A, B) -> maps:get(id, A) =< maps:get(id, B) end, Rows),
    {ok, lists:sublist(Sorted, Limit)}.

event_watermark(OrgId, WorkspaceId) ->
    Ids = [
        maps:get(id, E)
     || E <- events(),
        maps:get(organization_id, E) =:= OrgId,
        maps:get(workspace_id, E, undefined) =:= WorkspaceId
    ],
    {ok,
        case Ids of
            [] -> 0;
            _ -> lists:max(Ids)
        end}.

%% Provisioning 故障注入：第 N 次 seat upsert 前失败（回滚语义测试用）。
seed_provision_fail_after(N) ->
    update(provision_fail_after, fun(_) -> N end).

%% provisioning 前置事实：目标 user 是本 Org 的 active member。
seed_member(OrgId, UserId) ->
    update(members, fun(M) ->
        M#{{OrgId, UserId} => #{organization_id => OrgId, user_id => UserId, status => active}}
    end).

provision_seat(OrgId, WorkspaceId, Provision) ->
    {members, Members} = hd(ets:lookup(?TAB, members)),
    UserId = maps:get(user_id, Provision),
    case maps:is_key({OrgId, UserId}, Members) of
        false ->
            {error, {not_found, member}};
        true ->
            {workspaces, Ws} = hd(ets:lookup(?TAB, workspaces)),
            case
                [
                    W
                 || W <- maps:values(Ws),
                    maps:get(organization_id, W) =:= OrgId,
                    maps:get(id, W) =:= WorkspaceId,
                    maps:get(status, W) =:= active
                ]
            of
                [] ->
                    {error, {not_found, workspace}};
                _ ->
                    provision_in(OrgId, WorkspaceId, Provision)
            end
    end.

provision_in(OrgId, WorkspaceId, Provision) ->
    UserId = maps:get(user_id, Provision),
    %% 故障注入点在一切写入之前：fake 无真事务，用「失败即零写入」镜像
    %% PG 单事务回滚的可观测终态（all-or-nothing）。返回形状与 PG 路径一致
    %%（elib_pg 回滚解包后 cs_seat_app 见到的就是 {error, Reason}）。
    case maybe_fail_provision() of
        {error, _} = Err ->
            Err;
        ok ->
            provision_in_tx(OrgId, WorkspaceId, Provision, UserId)
    end.

provision_in_tx(OrgId, WorkspaceId, Provision, UserId) ->
    %% 复用：既有 active customer_service identity+assignment（assignment_users
    %% 即「identity ↔ user」事实；无则走创建分支）。
    Cands = [
        I
     || {{O, I}, U} <- maps:to_list(assignment_users_map()),
        O =:= OrgId,
        U =:= UserId,
        maps:get({OrgId, I}, identity_functions_map(), undefined) =:= <<"customer_service">>
    ],
    {IdentityId, Created} =
        case Cands of
            [I | _] -> {I, false};
            [] -> {next_counter(), true}
        end,
    Created andalso
        update(identity_functions, fun(M) ->
            M#{{OrgId, IdentityId} => <<"customer_service">>}
        end),
    Created andalso
        update(assignment_users, fun(M) -> M#{{OrgId, IdentityId} => UserId} end),
    Before = maps:get({OrgId, IdentityId}, seats_map(), undefined),
    Seat0 = #{
        organization_id => OrgId,
        business_identity_id => IdentityId,
        function_key => <<"customer_service">>,
        enabled => true,
        max_concurrent => maps:get(max_concurrent, Provision, 1),
        created_by_user_id => undefined,
        %% 行形状与 insert_seat 镜像（set_seat_enabled 依赖 version/updated_at）。
        version => 1,
        created_at => 1700000000,
        updated_at => 1700000000
    },
    Seat =
        case Before of
            undefined -> Seat0;
            B -> B#{enabled => true, max_concurrent => maps:get(max_concurrent, Provision, 1)}
        end,
    put_seat(Seat),
    EventId = next_counter(),
    update(events, fun(L) ->
        L ++
            [
                #{
                    id => EventId,
                    organization_id => OrgId,
                    workspace_id => WorkspaceId,
                    business_identity_id => IdentityId,
                    actor_kind => <<"platform_admin">>,
                    action => <<"platform.provisioned">>,
                    detail => #{
                        <<"adm_user_id">> => maps:get(adm_user_id, Provision, undefined),
                        <<"target_user_id">> => UserId,
                        <<"identity_created">> => Created,
                        <<"before">> => before_bin(Before),
                        <<"after">> => <<"enabled">>
                    }
                }
            ]
    end),
    {ok, #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        business_identity_id => IdentityId,
        identity_created => Created,
        seat => #{enabled => true, max_concurrent => maps:get(max_concurrent, Provision, 1)}
    }}.

maybe_fail_provision() ->
    {provision_fail_after, N} = hd(ets:lookup(?TAB, provision_fail_after)),
    case N of
        infinity ->
            ok;
        0 ->
            update(provision_fail_after, fun(_) -> infinity end),
            {error, seat_conflict_injected};
        _ when is_integer(N) ->
            update(provision_fail_after, fun(_) -> N - 1 end),
            ok;
        _ ->
            ok
    end.

before_bin(undefined) -> <<"absent">>;
before_bin(#{enabled := true}) -> <<"enabled">>;
before_bin(_) -> <<"disabled">>.

assignment_users_map() ->
    {assignment_users, M} = hd(ets:lookup(?TAB, assignment_users)),
    M.

identity_functions_map() ->
    {identity_functions, M} = hd(ets:lookup(?TAB, identity_functions)),
    M.

seats_map() ->
    {seats, M} = hd(ets:lookup(?TAB, seats)),
    M.

put_seat(Seat) ->
    IdentityId = maps:get(business_identity_id, Seat),
    OrgId = maps:get(organization_id, Seat),
    update(seats, fun(M) -> M#{{OrgId, IdentityId} => Seat} end),
    ok.

%% ===================================================================
%% CS-BE-04：已读游标 fake（镜像 cs_pg_session 的机械单调语义；
%% 授权复核在 application——本 fake 只提供游标与消息事实面）
%% ===================================================================

%% 消息事实种子（镜像 enterprise_message 的未读计算所需列）。
seed_message(OrgId, WorkspaceId, ConversationId, Message) ->
    Row = Message#{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => ConversationId
    },
    update(messages, fun(L) -> [Row | L] end),
    maps:get(id, Message).

ack_session_read(OrgId, WorkspaceId, SessionId, IdentityId, LastReadMessageId, _At) ->
    case fetch_session(OrgId, WorkspaceId, SessionId) of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            ConvId = maps:get(conversation_id, Session),
            %% 候选收敛：只承认会话 conversation 内已存在的消息 id（同
            %% SQL_ACK_EFFECTIVE_CURSOR）；无更早消息则 0。
            Effective =
                lists:max(
                    [0] ++
                        [
                            Id
                         || #{
                                id := Id,
                                organization_id := OrgId,
                                workspace_id := WorkspaceId,
                                conversation_id := ConvId
                            } <- messages(),
                            Id =< LastReadMessageId
                        ]
                ),
            Key = {OrgId, SessionId, IdentityId},
            {read_cursors, Cursors} = hd(ets:lookup(?TAB, read_cursors)),
            Prev = maps:get(Key, Cursors, 0),
            %% 单调前进：新值 > 旧值才写（同 SQL_ACK_CURSOR_UPSERT 的 WHERE）。
            case Effective > Prev of
                true -> update(read_cursors, fun(M) -> M#{Key => Effective} end);
                false -> ok
            end,
            read_state_of(OrgId, WorkspaceId, SessionId, IdentityId, Session)
    end.

fetch_session_read_state(OrgId, WorkspaceId, SessionId, IdentityId) ->
    case fetch_session(OrgId, WorkspaceId, SessionId) of
        {error, _} = Err ->
            Err;
        {ok, Session} ->
            read_state_of(OrgId, WorkspaceId, SessionId, IdentityId, Session)
    end.

read_state_of(OrgId, WorkspaceId, SessionId, IdentityId, Session) ->
    ConvId = maps:get(conversation_id, Session),
    Cursor = cursor_of(OrgId, SessionId, IdentityId),
    Unread = length([
        ok
     || #{
            id := Id,
            organization_id := OrgId,
            workspace_id := WorkspaceId,
            conversation_id := ConvId,
            sender_type := contact,
            visibility := visible
        } <-
            messages(),
        Id > Cursor
    ]),
    {ok, #{
        session_id => SessionId,
        business_identity_id => IdentityId,
        last_read_message_id => Cursor,
        unread_count => Unread
    }}.

cursor_of(OrgId, SessionId, IdentityId) ->
    {read_cursors, Cursors} = hd(ets:lookup(?TAB, read_cursors)),
    maps:get({OrgId, SessionId, IdentityId}, Cursors, 0).

messages() ->
    {messages, L} = hd(ets:lookup(?TAB, messages)),
    L.

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
