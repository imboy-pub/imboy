%%% @doc Customer Service 的扩展点注册表与装配契约（镜像 `eb_ports` 的角色）。
%%%
%%% 依据：plan v4.1 EB-D08（feature 边界：客服排队/claim/transfer/rating/max
%%% concurrency 属 customer_service；消息/客户/附件经 `enterprise_business_facade`）、
%%% `docs/architecture/feature-slice-rules.md` 铁律 3/6/7/9。
%%%
%%% 本模块只聚合**装配事实**：端口模块名、冻结 callback 契约、facade 委派目标与
%%% 引用白名单。不承载业务规则，不含 I/O。
%%%
%%% **跨 Feature 引用面（EB-D08 / 铁律 5）**：customer_service 对 enterprise_business
%%% 的唯一合法引用是 facade 模块 `enterprise_business_facade`——消息/备注/附件只经
%%% 它写 enterprise 真源（CS-01-A03），不引用其 application/infrastructure 内层。
-module(cs_ports).

-export([
    all/0,
    store/0,
    id/0,
    org_lifecycle/0,
    port_module_for/1,
    contracts/0,
    facade_targets/0,
    facade_reference_whitelist/0,
    external_reference_whitelist/0
]).

-type port_module() :: module().
-type contract() :: [{atom(), arity()}].
-type contracts() :: #{port_module() => contract()}.

-export_type([port_module/0, contract/0, contracts/0]).

%% ===================================================================
%% 端口装配
%% ===================================================================

%% @doc 全部已冻结的扩展点模块（顺序固定，便于逐字审计）。
-spec all() -> [port_module()].
all() ->
    [store(), id(), org_lifecycle()].

%% @doc 持久化读写扩展点。session 级 callback 前两个业务参数是
%% `organization_id` / `workspace_id`（铁律 6）；Org 级资源首参为 OrgId。
-spec store() -> port_module().
store() -> cs_store_port.

%% @doc 注入 ID 生成扩展点（TSID）。
-spec id() -> port_module().
id() -> cs_id_port.

%% @doc Organization 生命周期事实扩展点（ORG-08 / C16；只读）。
%% archived Org → 新 Session / 新 claim 稳定拒绝的唯一事实来源。
-spec org_lifecycle() -> port_module().
org_lifecycle() -> cs_org_lifecycle_port.

%% @doc 按域键取端口模块；未知键 fail-closed。
-spec port_module_for(term()) -> port_module() | {error, term()}.
port_module_for(store) -> store();
port_module_for(id) -> id();
port_module_for(org_lifecycle) -> org_lifecycle();
port_module_for(Unknown) -> {error, {unknown_port, Unknown}}.

%% ===================================================================
%% 冻结的扩展点契约
%% ===================================================================

%% @doc 每个端口的冻结 callback 列表（实现侧 `behaviour_info(callbacks)`
%% 必须与之一致；cs 闭环测试核对）。
-spec contracts() -> contracts().
contracts() ->
    #{
        store() => [
            %% identity 事实（A01）
            {fetch_identity_function, 2},
            %% seat
            {insert_seat, 2},
            {fetch_seat, 2},
            {list_dispatchable_seats, 1},
            {list_dispatchable_seats_page, 3},
            {list_all_seats_page, 3},
            {set_seat_enabled, 4},
            %% BE-S01a：坐席上下文聚合 / 转接目标
            {list_seat_org_contexts, 1},
            {list_transfer_targets_page, 4},
            %% session
            {insert_session, 3},
            {fetch_session, 3},
            {claim_session, 7},
            {transfer_session, 7},
            {close_session, 7},
            {rate_session, 7},
            {list_sessions_for_contact, 3},
            {list_sessions_page, 5},
            {seat_session_page, 5},
            %% CS-BE-03（CS-DEC-01）：客户上下文只读事实（会话锚定事实行 /
            %% 同 contact 历史页 / 授权备注事实页）。
            {fetch_session_customer_context, 3},
            {list_session_history_page, 4},
            {list_contact_notes_page, 3},
            {default_workspace, 1},
            %% shop key / visit token
            {insert_shop_key, 2},
            {fetch_shop_key, 2},
            {fetch_shop_key_by_digest, 2},
            {list_shop_keys_page, 3},
            {revoke_shop_key, 3},
            {insert_visit_token, 2},
            {fetch_visit_token, 2},
            {fetch_visit_token_by_digest, 2},
            {list_visit_tokens_page, 3},
            {revoke_visit_token, 3},
            %% widget installation / identity key（CSB-01）
            {insert_widget_installation, 2},
            {fetch_widget_installation, 2},
            {fetch_widget_installation_by_public_id, 2},
            %% CSD-BE-01（hosted-widget-contract S3）：public_widget_id 全局
            %% 反查（/w/ 面；Org 是行输出的派生值，非查询输入）。
            {fetch_widget_installation_by_public_id_global, 1},
            {list_widget_installations_page, 3},
            {revoke_widget_installation, 3},
            {insert_widget_identity_key, 3},
            {fetch_widget_identity_key, 3},
            {revoke_widget_identity_key, 4},
            %% widget bootstrap token（复用 visit_token 存储）
            {insert_widget_bootstrap_token, 2},
            {fetch_widget_bootstrap_token_by_digest, 3},
            %% CSD-BE-01S：digest 全局命中（S3 v1.1 持 token 面 Org 派生）。
            {fetch_widget_bootstrap_token_by_digest_global, 2},
            {touch_widget_bootstrap_token, 4},
            {revoke_widget_bootstrap_token, 4},
            %% widget JTI nonce（重放防护）
            {record_widget_nonce, 4},
            %% event（append-only 状态审计）+ BE-S01b SSE 读面 / provisioning
            {append_event, 2},
            {fetch_event_scope, 2},
            {list_events_page, 4},
            {event_watermark, 2},
            {provision_seat, 3}
        ],
        id() => [
            {new_id, 1}
        ],
        org_lifecycle() => [
            %% 只读生命周期事实（active | archived）；ORG-08 / C16。
            {status, 1}
        ]
    }.

%% ===================================================================
%% facade 委派契约
%% ===================================================================

%% @doc `customer_service_facade` 允许委派的 application 用例模块（铁律 3）。
-spec facade_targets() -> [module()].
facade_targets() ->
    [
        cs_seat_app,
        %% BE-S01b：坐席 SSE 事件流用例（sse-event-contract 的 application 面）。
        cs_seat_event_app,
        cs_session_app,
        cs_access_app,
        cs_widget_app,
        cs_widget_session_app,
        %% CSD-BE-01S：widget 接入面的机械辅助（derive_org_by_token——持
        %% token 动作面的租户解析，facade 在 env 装配前调用）。
        cs_widget_support,
        %% CSB-02R：widget env 装配（subject_key/default_workspace/intake/
        %% assertion_verifier 的解析与合并——facade 参数收敛职责的一部分）。
        cs_widget_env
    ].

%% @doc facade 可引用的非 application 模块白名单：空集（纯「参数收敛 + 委派」）。
-spec facade_reference_whitelist() -> [module()].
facade_reference_whitelist() ->
    [].

%% @doc customer_service 全部模块允许引用的**跨单元**白名单。
%%
%% EB-D08 + ORG-08：
%%   * `enterprise_business_facade`——消息/客户/附件真源的唯一入口（A03）；
%%   * `organization_repo`——Organization 生命周期事实 adapter 的只读依赖
%%     （cs_org_lifecycle_facts 经 `find_by_id/1` 读 organization.status，
%%     不自查 org 表；Feature → Core/legacy 单向，铁律 5 合法方向）；
%%   * core/lib（`src/lib`）与 OTP 模块不属于「跨 Feature」，由 cs 闭环测试的
%%     OTP/lib 白名单单独放行，不在此列。
-spec external_reference_whitelist() -> [module()].
external_reference_whitelist() ->
    [enterprise_business_facade, organization_repo].
