%%% @doc 扩展点：客服持久化读写（CS-01 冻结契约；实现随 cs_pg_* 落地）。
%%%
%%% 真扩展点：只有 `-callback` 声明，零实现、零 mock。实现按装配选择
%%% （`cs_infra_ports` → `cs_pg_store`），测试可注入 fake（`Params` 里同键覆盖）。
%%%
%%% 铁律 6（租户作用域必须显式贯穿）：
%%%   * session 级 callback 的前两个业务参数是 `organization_id` 与
%%%     `workspace_id`，实现 SQL 必须同语句带这两个约束；
%%%   * seat / shop_key / visit_token / event 是 Org 级资源（无 workspace 列），
%%%     callback 首参为 `organization_id`，实现 SQL 必须同语句带 Org。
%%%
%%% 铁律 7（并发状态迁移）：`claim_session/7` 等状态推进 callback 是 CAS 形状——
%%% 实现必须在单事务里以 (status, version) 条件 UPDATE 裁决，仅当恰写入 1 行
%%% （且事件同事务落库）时返回 `{ok, session()}`，否则返回 `{error, conflict}`；
%%% 并发下**恰好一个**调用方成功（CS-01-A02 的 DB 裁决点）。
%%%
%%% 三分（EB-D03）：所有返回行带 `organization_id`；user 列只是审计快照；
%%% 实现禁止任何 CASCADE 到 user 的写路径。
-module(cs_store_port).

-export_type([
    seat/0,
    session/0,
    shop_key/0,
    visit_token/0,
    event/0
]).

-type seat() :: map().
-type session() :: map().
-type shop_key() :: map().
-type visit_token() :: map().
-type event() :: map().

%% -- identity 事实（A01 的应用侧前置校验数据源）----------------------------

%% @doc 读取业务身份的 function_key（只读事实；A01 应用侧双重校验用）。
-callback fetch_identity_function(OrgId :: integer(), IdentityId :: integer()) ->
    {ok, binary()} | {error, not_found | term()}.

%% -- seat ------------------------------------------------------------------

-callback insert_seat(OrgId :: integer(), Seat :: seat()) ->
    {ok, seat()} | {error, conflict | term()}.
-callback fetch_seat(OrgId :: integer(), IdentityId :: integer()) ->
    {ok, seat()} | {error, not_found | term()}.
%% @doc 派单快照：本 Org 的 enabled 坐席及各自 active 会话计数（least-active 输入）。
-callback list_dispatchable_seats(OrgId :: integer()) ->
    {ok, [seat()]} | {error, term()}.
%% @doc C4（contracts-w2）seat 列表分页：键集下推（`business_identity_id > after`
%% + `ORDER BY business_identity_id ASC LIMIT n`，eb_pg_message_ext 模板口径），
%% 同语句带 Org 且仅 enabled 坐席。
-callback list_dispatchable_seats_page(
    OrgId :: integer(), AfterId :: non_neg_integer(), Limit :: pos_integer()
) ->
    {ok, [seat()]} | {error, term()}.
%% @doc 坐席开关（suspend/resume）；`enabled=false` 后新 claim 立即被拒。
-callback set_seat_enabled(
    OrgId :: integer(), IdentityId :: integer(), Enabled :: boolean(), At :: integer()
) ->
    {ok, seat()} | {error, not_found | term()}.

%% -- session ---------------------------------------------------------------

-callback insert_session(OrgId :: integer(), WorkspaceId :: integer(), Session :: session()) ->
    {ok, session()} | {error, conflict | term()}.
-callback fetch_session(OrgId :: integer(), WorkspaceId :: integer(), SessionId :: integer()) ->
    {ok, session()} | {error, not_found | term()}.
%% @doc A02 的 DB 裁决点：单事务内锁定 seat 行 → enabled / max_concurrent 复核 →
%% (status='queued', version) 条件 UPDATE → 审计事件同事务落库。
%% 并发同一会话恰好一个 `{ok, session()}`，其余 `{error, conflict}`；
%% 坐席停用 → `{error, seat_disabled}`；达到 max_concurrent → `{error, seat_at_capacity}`。
-callback claim_session(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    SessionId :: integer(),
    IdentityId :: integer(),
    ExpectedVersion :: integer(),
    ClaimedAt :: integer(),
    Event :: event()
) ->
    {ok, session()} | {error, conflict | seat_disabled | seat_at_capacity | term()}.
%% @doc 改绑经办 identity（主体字段零迁移；A04）。CAS：期望 (active, version)。
-callback transfer_session(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    SessionId :: integer(),
    ToIdentityId :: integer(),
    ExpectedVersion :: integer(),
    At :: integer(),
    Event :: event()
) ->
    {ok, session()} | {error, conflict | not_found | term()}.
%% @doc 关闭会话（queued|active → closed）。CAS：期望 (status, version)。
-callback close_session(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    SessionId :: integer(),
    Reason :: binary() | undefined,
    ExpectedVersion :: integer(),
    At :: integer(),
    Event :: event()
) ->
    {ok, session()} | {error, conflict | not_found | term()}.
%% @doc 评分（仅 closed 且未评过的会话；1..5 由 domain 先行校验）。
-callback rate_session(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    SessionId :: integer(),
    Rating :: pos_integer(),
    ExpectedVersion :: integer(),
    At :: integer(),
    Event :: event()
) ->
    {ok, session()} | {error, conflict | not_found | term()}.
%% @doc 访客视角：只列**自己的** (Org, contact) 会话（A05 的读取边界）。
-callback list_sessions_for_contact(
    OrgId :: integer(), WorkspaceId :: integer(), ContactId :: integer()
) ->
    {ok, [session()]} | {error, term()}.
%% @doc C1（contracts-w2）平台 session 列表：键集下推（`id > after` +
%% `ORDER BY id DESC LIMIT n`），OrgId+WorkspaceId 同语句；`Status` 为
%% binary 白名单值（<<"queued">>|<<"active">>|<<"closed">>）或 undefined
%% （不过滤）。行原样返回（投影由 application 白名单裁剪）。
-callback list_sessions_page(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    Status :: binary() | undefined,
    AfterId :: non_neg_integer(),
    Limit :: pos_integer()
) ->
    {ok, [session()]} | {error, term()}.

%% -- shop key / visit token（digest 存储；明文不落库）------------------------

-callback insert_shop_key(OrgId :: integer(), Key :: shop_key()) ->
    {ok, shop_key()} | {error, conflict | term()}.
-callback fetch_shop_key(OrgId :: integer(), KeyId :: integer()) ->
    {ok, shop_key()} | {error, not_found | term()}.
-callback fetch_shop_key_by_digest(OrgId :: integer(), Digest :: binary()) ->
    {ok, shop_key()} | {error, not_found | term()}.
-callback revoke_shop_key(OrgId :: integer(), KeyId :: integer(), At :: integer()) ->
    ok | {error, not_found | term()}.
%% @doc C2（contracts-w2）shop key 列表：键集下推（`id > after` +
%% `ORDER BY id DESC LIMIT n`），同语句带 Org。行含 digest——投影由
%% application 白名单裁剪（绝不外泄）。
-callback list_shop_keys_page(
    OrgId :: integer(), AfterId :: non_neg_integer(), Limit :: pos_integer()
) ->
    {ok, [shop_key()]} | {error, term()}.

-callback insert_visit_token(OrgId :: integer(), Token :: visit_token()) ->
    {ok, visit_token()} | {error, conflict | term()}.
-callback fetch_visit_token(OrgId :: integer(), TokenId :: integer()) ->
    {ok, visit_token()} | {error, not_found | term()}.
-callback fetch_visit_token_by_digest(OrgId :: integer(), Digest :: binary()) ->
    {ok, visit_token()} | {error, not_found | term()}.
-callback revoke_visit_token(OrgId :: integer(), TokenId :: integer(), At :: integer()) ->
    ok | {error, not_found | term()}.
%% @doc C3（contracts-w2）visit token 列表：键集下推（`id > after` +
%% `ORDER BY id DESC LIMIT n`），同语句带 Org。行含 digest——投影由
%% application 白名单裁剪（绝不外泄）。
-callback list_visit_tokens_page(
    OrgId :: integer(), AfterId :: non_neg_integer(), Limit :: pos_integer()
) ->
    {ok, [visit_token()]} | {error, term()}.

%% -- event（客服域 append-only 状态审计）------------------------------------

%% @doc 追加一条客服状态审计（append-only；实现侧 UPDATE/DELETE 被触发器拒绝）。
-callback append_event(OrgId :: integer(), Event :: event()) ->
    {ok, integer()} | {error, term()}.
