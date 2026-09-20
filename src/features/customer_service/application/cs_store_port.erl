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
    event/0,
    widget_installation/0,
    widget_identity_key/0,
    widget_bootstrap_token/0
]).

-type seat() :: map().
-type session() :: map().
-type shop_key() :: map().
-type visit_token() :: map().
-type event() :: map().
-type widget_installation() :: map().
-type widget_identity_key() :: map().
-type widget_bootstrap_token() :: map().

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
%% @doc BE-S01a：用户维度坐席上下文聚合（主体自身作用域，无 Org 前参）。
%% 单语句同过滤：organization_member active + 组织 active；LEFT JOIN active
%% customer_service assignment 与 seat 行（无坐席身份的 Org 也返回——
%% seat_enabled=false 让客户端区分「成员但未开通坐席」）。workspaces 是
%% 同 Org active Workspace 的 `#{id, name}` 列表（json 聚合，store 侧已解码）。
-callback list_seat_org_contexts(UserId :: integer()) ->
    {ok, [map()]} | {error, term()}.
%% @doc BE-S01a：转接目标分页（键集下推 `business_identity_id > after` 升序 +
%% `LIMIT`，C1~C4 模板口径）；同语句带 Org、仅 enabled、排除 ExcludeIdentityId
%% （调用者本人）；行含 identity 显示名与 active 会话同语句计数。
-callback list_transfer_targets_page(
    OrgId :: integer(),
    ExcludeIdentityId :: integer(),
    AfterId :: non_neg_integer(),
    Limit :: pos_integer()
) ->
    {ok, [map()]} | {error, term()}.

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

%% @doc CSB-02R 坐席工作台分页（org-wide；WorkspaceId=0 表示不限 workspace）。
%% 返回同作用域的稳定计数（列表页 + status 全量分布）；行含 contact 掩码
%% 原料（display_name / subject_mask）与末条消息安全摘要（id / sender_type /
%% created_at——密文与密钥列不出 store）。
-callback seat_session_page(
    OrgId :: integer(),
    Status :: binary(),
    AfterId :: non_neg_integer(),
    Limit :: pos_integer(),
    WorkspaceId :: non_neg_integer()
) ->
    {ok, #{
        rows := [map()],
        total := non_neg_integer(),
        total_by_status := map()
    }}
    | {error, term()}.

%% @doc CSB-02R widget 装配：本 Org 缺省 Workspace 解析（org 作用域 active 且
%% id 最小——访客无 membership，不做成员连接；EB 同款确定性规则）。
-callback default_workspace(OrgId :: integer()) ->
    {ok, integer()} | {error, not_found | term()}.

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

%% -- widget installation / identity key（CSB-01；digest 存储，明文不落库）----

%% @doc 创建 Widget 安装：public_widget_id 全局唯一，冲突归一为 conflict。
-callback insert_widget_installation(OrgId :: integer(), Installation :: widget_installation()) ->
    {ok, widget_installation()} | {error, conflict | term()}.
%% @doc 按 id 读取（同语句带 Org；跨 Org 命中不了行）。
-callback fetch_widget_installation(OrgId :: integer(), InstallationId :: integer()) ->
    {ok, widget_installation()} | {error, not_found | term()}.
%% @doc 按公开标识读取：public_widget_id 可公开分发，但解析必须同语句携带
%% OrgId——错 Org 的查询拿到 not_found（CSB-01-A02 的 store 裁决点）。
-callback fetch_widget_installation_by_public_id(OrgId :: integer(), PublicWidgetId :: binary()) ->
    {ok, widget_installation()} | {error, not_found | term()}.
%% @doc CSD-BE-01（hosted-widget-contract S3）：public_widget_id **全局**反查——
%% 输入只有公开 ID（浏览器不申报 Org），Org/Workspace 是**输出**（权威派生自
%% 命中的唯一 active installation 行，`public_widget_id` 全局唯一约束保证单行）。
%% 消费面仅限零凭证导航面（/w/:public_widget_id frame HTML）；token 面
%% （bootstrap 等）仍必须走 Org 同语句的 `fetch_widget_installation_by_public_id/2`。
%% 不存在 → `{error, not_found}`（三态归一为 installation_unavailable 由
%% application 承担）。
-callback fetch_widget_installation_by_public_id_global(PublicWidgetId :: binary()) ->
    {ok, widget_installation()} | {error, not_found | term()}.
%% @doc 管理面列表：DESC 键集分页，同语句绑定 Org。
-callback list_widget_installations_page(
    OrgId :: integer(), AfterId :: non_neg_integer(), Limit :: pos_integer()
) ->
    {ok, [widget_installation()]} | {error, term()}.
%% @doc 吊销安装（status='revoked' + revoked_at；行保留以审计）。
-callback revoke_widget_installation(
    OrgId :: integer(), InstallationId :: integer(), At :: integer()
) ->
    ok | {error, not_found | term()}.

%% @doc 登记 signing key：只存 key_digest（sha256 hex），明文密钥绝不落库；
%% (org, installation, key_version) 复合唯一，并发同版本归一为 conflict。
-callback insert_widget_identity_key(
    OrgId :: integer(), InstallationId :: integer(), Key :: widget_identity_key()
) ->
    {ok, widget_identity_key()} | {error, conflict | term()}.
%% @doc 按 key_version 读取密钥行（含 key_digest，投影由 application 裁剪）。
-callback fetch_widget_identity_key(
    OrgId :: integer(), InstallationId :: integer(), KeyVersion :: pos_integer()
) ->
    {ok, widget_identity_key()} | {error, not_found | term()}.
%% @doc 吊销指定版本密钥（status='revoked' + revoked_at）。
-callback revoke_widget_identity_key(
    OrgId :: integer(), InstallationId :: integer(), KeyVersion :: pos_integer(), At :: integer()
) ->
    ok | {error, not_found | term()}.

%% @doc 签发 Widget bootstrap 令牌：复用 customer_service_visit_token（追加
%% widget_installation_id / anonymous_subject_hmac 列），不复制新表；digest 与
%% expiry 口径与既有 visit token 完全一致。
-callback insert_widget_bootstrap_token(OrgId :: integer(), Token :: widget_bootstrap_token()) ->
    {ok, widget_bootstrap_token()} | {error, conflict | term()}.
%% @doc 按 digest 校验 bootstrap 令牌：同语句绑定 (Org, installation)——
%% 跨 Org / 跨安装命中不了行（not_found，不做存在性枚举）。
-callback fetch_widget_bootstrap_token_by_digest(
    OrgId :: integer(), InstallationId :: integer(), Digest :: binary()
) ->
    {ok, widget_bootstrap_token()} | {error, not_found | term()}.
%% @doc 活跃心跳：更新 last_seen_at（不改 digest / 不动 version 语义）。
-callback touch_widget_bootstrap_token(
    OrgId :: integer(), InstallationId :: integer(), TokenId :: integer(), At :: integer()
) ->
    ok | {error, not_found | term()}.
%% @doc 吊销 bootstrap 令牌（revoked_at 口径同 revoke_visit_token）。
-callback revoke_widget_bootstrap_token(
    OrgId :: integer(), InstallationId :: integer(), TokenId :: integer(), At :: integer()
) ->
    ok | {error, not_found | term()}.

%% @doc 记录请求 JTI（重放防护的 DB 裁决点）：(org, installation, jti_digest)
%% 复合唯一——并发/重放同 jti 恰好一个 `ok`，其余 `{error, replay}`（23505）。
-callback record_widget_nonce(
    OrgId :: integer(),
    InstallationId :: integer(),
    JtiDigest :: binary(),
    ExpiresAt :: integer()
) ->
    ok | {error, replay | term()}.

%% -- event（客服域 append-only 状态审计 + BE-S01b 坐席 SSE 流读取）---------

%% @doc 追加一条客服状态审计（append-only；实现侧 UPDATE/DELETE 被触发器拒绝）。
-callback append_event(OrgId :: integer(), Event :: event()) ->
    {ok, integer()} | {error, term()}.

%% @doc BE-S01b（sse-event-contract）：按事件 id 读取作用域（Org+Workspace），
%% 供游标合法性裁决——事件存在但 (Org, Workspace) 与流作用域不符 ⇒ 跨租户
%% 游标（403 面）；不存在 ⇒ 游标缺失/超窗（resync 面）。`{error, not_found}`
%% 与其他错误同形状（实现侧跨作用域不可能命中他行——按 id 全局唯一）。
-callback fetch_event_scope(OrgId :: integer(), EventId :: integer()) ->
    {ok, #{organization_id := integer(), workspace_id := integer()}} | {error, term()}.

%% @doc BE-S01b：SSE 补偿/轮询读页（键集下推 `id > after` + `ORDER BY id ASC`
%% + `LIMIT`，迁移 135 的 i_cse_org_ws_id (organization_id, workspace_id, id)
%% 是唯一入口）。同语句绑定 (Org, Workspace)——跨租户/跨 Workspace 恒空页。
-callback list_events_page(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    AfterId :: non_neg_integer(),
    Limit :: pos_integer()
) ->
    {ok, [event()]} | {error, term()}.

%% @doc BE-S01b：当前水位（(Org, Workspace) 内最大事件 id；空域为 0）。
%% resync（游标缺失/超窗）后从水位继续，不重放历史。
-callback event_watermark(OrgId :: integer(), WorkspaceId :: integer()) ->
    {ok, non_neg_integer()} | {error, term()}.

%% -- Admin provisioning（BE-S01b；api-surface-freeze admin_provisioning）-----

%% @doc 平台面事务化开通/修复坐席：单数据库事务内
%%   1. (Org, user, customer_service) 已有 active identity+assignment ⇒ 复用；
%%      否则创建 active customer_service identity + active assignment；
%%   2. seat upsert（enabled=true，已存在则修复为 enabled）；
%%   3. 审计事件同事务落库（actor/target/before/after）。
%% 任一步失败全回滚；幂等（重复调用返回既有事实，不重复创建）。
-callback provision_seat(OrgId :: integer(), WorkspaceId :: integer(), Provision :: map()) ->
    {ok, map()} | {error, term()}.
