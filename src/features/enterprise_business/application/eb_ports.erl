%%% @doc Enterprise Business 的扩展点（Port）注册表与装配常量。
%%%
%%% 依据：plan v4.1 EB-02「最小 store/crypto/asset ports」、
%%% `docs/architecture/feature-slice-rules.md` §0.3（扩展点 ≡ Port ≡ behaviour）
%%% 与 §铁律 9（扩展点由所属单元声明，实现按装配选择）。
%%%
%%% 本模块只做**装配事实**的聚合：端口模块名、冻结的 callback 契约、
%%% facade 委派目标与引用白名单。它不承载业务规则，也不含 I/O。
%%%
%%% 默认实现必须存在才能"零配置可运行"：本卡只冻结契约（EB-02），
%%% 具体内置实现随 EB-03/05/06/07/08 落地，并在此处登记。
%%%
%%% **EB-03R 变更纪律（A02）**：本模块相对 EB-02 只有**追加**——
%%% `store/crypto/clock/id/audit/asset` 六个既有取值与既有 callback 逐条未动，
%%% 新增 `auth`（C5 注册）、`member_fact`（P10 只读事实）、`tx`（T1/T2 用例级事务）
%%% 三个端口，并在 `contracts/0` 把 R0-1/R0-3/R0-4 判定的「真实依赖面」并入契约面。
-module(eb_ports).

-export([
    all/0,
    store/0,
    crypto/0,
    clock/0,
    id/0,
    audit/0,
    asset/0,
    auth/0,
    member_fact/0,
    tx/0,
    purge/0,
    port_module_for/1,
    contracts/0,
    assembly_missing/1,
    facade_targets/0,
    facade_reference_whitelist/0
]).

-type port_module() :: module().
-type contract() :: [{atom(), arity()}].
-type contracts() :: #{port_module() => contract()}.

-export_type([port_module/0, contract/0, contracts/0]).

%% ===================================================================
%% 端口装配
%% ===================================================================

%% @doc 全部已冻结的扩展点模块（顺序固定，便于逐字审计）。
%%
%% EB-03R：追加 `auth`（EB-04 的授权事实扩展点，此前只在 wb-a3 落地、**未注册**，
%% 见 R0-5/D7）、`member_fact`（P10 的最小只读事实）、`tx`（T1/T2 的用例级事务）。
%% 前六个的位置与取值逐字未动。
-spec all() -> [port_module()].
all() ->
    [
        store(),
        crypto(),
        clock(),
        id(),
        audit(),
        asset(),
        auth(),
        member_fact(),
        tx(),
        purge()
    ].

%% @doc 持久化读写扩展点。所有资源级 callback 的前两个业务参数必须是
%% `organization_id` 与 `workspace_id`（铁律 6）。
-spec store() -> port_module().
store() -> eb_store_port.

%% @doc 企业托管加密扩展点（AAD 绑定 OrgId/WorkspaceId/ConversationId/MessageId）。
-spec crypto() -> port_module().
crypto() -> eb_crypto_port.

%% @doc 注入时钟扩展点。domain 不得读取系统时间，必须经此端口注入。
-spec clock() -> port_module().
clock() -> eb_clock_port.

%% @doc 注入 ID 生成扩展点。
-spec id() -> port_module().
id() -> eb_id_port.

%% @doc append-only 审计写入扩展点。
-spec audit() -> port_module().
audit() -> eb_audit_port.

%% @doc 企业私有对象读写 + 鉴权代理取流扩展点。
-spec asset() -> port_module().
asset() -> eb_asset_port.

%% @doc 授权事实扩展点（**只读**、逐请求加载；EB-04 的契约，EB-03R 只做注册）。
-spec auth() -> port_module().
auth() -> eb_auth_port.

%% @doc 最小只读事实扩展点（成员状态 / 默认 Workspace 解析；P10）。
%%
%% 事实 ≠ 授权结论：读事实走本端口，授权判定仍必须走 `auth()`。
-spec member_fact() -> port_module().
member_fact() -> eb_member_fact_port.

%% @doc 用例级事务扩展点（原子接受消息 / 会话审计同事务；T1/T2）。
-spec tx() -> port_module().
tx() -> eb_tx_port.

%% @doc bounded purge 用例级端口（契约与实现都在 infrastructure 层：application 层
%% 不得出现 PG 直连模块名，A04 的机械判据——本文件因此不写它的实现模块名）。
-spec purge() -> port_module().
purge() -> eb_purge_port.

%% @doc 按域键取端口模块；未知键 fail-closed。
-spec port_module_for(term()) -> port_module() | {error, term()}.
port_module_for(store) -> store();
port_module_for(crypto) -> crypto();
port_module_for(clock) -> clock();
port_module_for(id) -> id();
port_module_for(audit) -> audit();
port_module_for(asset) -> asset();
port_module_for(auth) -> auth();
port_module_for(member_fact) -> member_fact();
port_module_for(tx) -> tx();
port_module_for(purge) -> purge();
port_module_for(Unknown) -> {error, {unknown_port, Unknown}}.

%% ===================================================================
%% 冻结的扩展点契约
%% ===================================================================

%% @doc 每个端口的冻结 callback 列表。
%%
%% 该映射是 `test/features/enterprise_business/application/eb_ports_tests.erl`
%% 的判定基准：端口模块的 `behaviour_info(callbacks)` 必须与之一致
%% （多声明、少声明、改参数都红）。
%%
%% **EB-03R 的追加**（A02：既有条目逐条未动）：
%%   * `store`：并入 R0-1 判定的 10 个「契约外但已在用」callback +
%%     P1..P9/P11 的正向能力 + `fetch_hold/3`（P6 四件齐）。
%%   * `crypto`：并入 R0-3 指出「声明了错的签名」后真实依赖的
%%     `seal_scoped/3`、`subject_hmac/4`（`seal/3` / `open/3` 保留）。
%%   * `asset`：并入 R0-4 判定缺失的 metadata 生命周期（P12）。
%%   * `auth`：EB-04 的只读授权事实契约（本卡仅注册，不改其形状）。
%%   * `member_fact` / `tx`：P10 与 T1/T2 的新用例级 Port。
%%
%% **`append_message/3` 的死面处置（C3，R0-2）**：该 callback 的**真路径**是
%% `eb_tx_port:accept_message/3`（canonical message + policy snapshot + audit 同事务）。
%% `append_message/3` 应用层**从不调用**（R0-2 实测），但按「只追加」原则
%% **保留声明**，不删除、不改签名：它仍是实现侧的合法单语句入口（store 实现
%% 与 canonical 事务共用同一套 sender 合同）。待后续卡决定「接入或标记 deprecated」。
-spec contracts() -> contracts().
contracts() ->
    #{
        store() => [
            %% ---- EB-02 冻结的 7 个（逐字未动）----
            {fetch_identity, 3},
            {insert_identity, 3},
            {fetch_conversation, 3},
            {insert_conversation, 3},
            {append_message, 3},
            {advance_assignment, 5},
            {list_assignments, 2},
            %% ---- EB-03R 追加：R0-1 的 10 个「契约外但已在用」----
            {insert_contact, 3},
            {fetch_contact, 3},
            {insert_contact_identity, 3},
            {insert_policy, 3},
            {latest_policy, 3},
            {insert_hold, 3},
            {release_hold, 4},
            {list_active_holds, 2},
            {ack_delivery, 3},
            {fetch_message, 3},
            %% ---- EB-03R 追加：P1..P9 / P11 的正向能力 ----
            {insert_assignment, 3},
            {list_identities, 2},
            {list_identities_page, 3},
            {insert_note, 3},
            {insert_contact_assignment, 3},
            {update_conversation_assignee, 4},
            {list_contacts, 2},
            {update_contact, 3},
            {list_conversations, 2},
            {list_messages_after, 3},
            {insert_offboarding_case, 3},
            {fetch_offboarding_case, 3},
            {list_offboarding_cases, 2},
            {insert_offboarding_item, 3},
            {list_offboarding_items, 3},
            {update_offboarding_item, 3},
            {advance_offboarding_case, 6},
            {update_offboarding_case_counts, 6},
            %% ---- EB-03R 追加：P6 四件齐 ----
            {fetch_hold, 3}
        ],
        crypto() => [
            %% ---- EB-02 冻结的 2 个（逐字未动）----
            {seal, 3},
            {open, 3},
            %% ---- EB-03R 追加：R0-3 的真实签名 ----
            {seal_scoped, 3},
            {subject_hmac, 4}
        ],
        clock() => [
            {now, 0}
        ],
        id() => [
            {new_id, 1}
        ],
        audit() => [
            {append, 2}
        ],
        asset() => [
            %% ---- EB-02 冻结的 3 个（逐字未动）----
            {put_private, 3},
            {stream_content, 3},
            {delete_private, 3},
            %% ---- EB-03R 追加：R0-4 的 metadata 生命周期（P12）----
            {insert_asset, 3},
            {fetch_asset, 3},
            {confirm_asset, 3},
            {cleanup_asset, 3}
        ],
        auth() => [
            {load_request_facts, 1}
        ],
        member_fact() => [
            {member_status, 2},
            {default_workspace, 2}
        ],
        tx() => [
            {accept_message, 3},
            {append_conversation_audit, 3}
        ],
        %% 用例级 purge 端口：**只保留** `/4`（T3 的窄形状）。EB-06 重开时把唯一调用点
        %% （`eb_retention_app:run_purge/5`）迁到 `/4` 后，过渡信封 `/3` 按 E6-D3 /
        %% E5-D2 删除（A18）——契约面因此不再可能接受一个 Opts map。
        purge() => [
            {purge_batch, 4}
        ]
    }.

%% @doc **契约与装配的同步校验**（C6 / R0-5）。
%%
%% 入参是「已装配的 (契约模块, 实现模块) 列表」（`eb_infra_ports:implementations/0`）。
%% 只要有一个 `all/0` 里声明、却不在装配里的端口，就返回
%% `{error, {unimplemented_port, Port}}`——**不静默放行**。
%%
%% R0-5 的教训：`eb_asset_port` 契约早已冻结，而 `resolve(asset)` 仍返回
%% `{error, {not_implemented_yet, asset}}`；契约先行、装配后补的通道当时**没有
%% 任何对齐检查**。本函数把「声明了就必须有实现」变成可机械判定的事实。
-spec assembly_missing([{port_module(), module()}]) ->
    ok | {error, {unimplemented_port, port_module()}}.
assembly_missing(Implementations) when is_list(Implementations) ->
    Assembled = [Port || {Port, _Impl} <- Implementations],
    case [Port || Port <- all(), not lists:member(Port, Assembled)] of
        [] -> ok;
        [Port | _] -> {error, {unimplemented_port, Port}}
    end.

%% ===================================================================
%% facade 委派契约
%% ===================================================================

%% @doc `enterprise_business_facade` 允许委派的 application 用例模块。
%%
%% 这些模块由 EB-05/06/07/08 落地；本卡先冻结名字，使「facade 只调
%% application」成为可静态判定的事实（铁律 3）。命名契约为 `eb_<域>_app`。
-spec facade_targets() -> [module()].
facade_targets() ->
    [
        eb_identity_app,
        eb_contact_app,
        eb_conversation_app,
        eb_message_app,
        eb_asset_app,
        eb_member_app,
        eb_offboarding_app,
        eb_retention_app
    ].

%% @doc facade 可引用的非 application 模块白名单。
%%
%% **显式且最小**：本卡为空集——facade 是纯「参数收敛 + 委派」层，除本
%% feature 的 application 用例模块外不引用任何模块（连 OTP 工具模块也不需要）。
%% 若未来确需放宽，必须在此登记并在评审中给出理由。
-spec facade_reference_whitelist() -> [module()].
facade_reference_whitelist() ->
    [].
