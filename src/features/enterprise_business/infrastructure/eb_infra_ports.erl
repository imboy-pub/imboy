%%% @doc 企业业务基础设施的**端口装配映射**（Port → 具体实现）。
%%%
%%% 依据：`docs/architecture/feature-slice-rules.md` 铁律 9（扩展点由所属单元声明，
%%% **实现按装配选择**）与 EB-02 的 `application/eb_ports.erl`（冻结契约，
%%% 只声明端口模块名与 callback 形状，不含实现）。
%%%
%%% 本模块只回答一个问题：「这个端口当前由哪个模块实现」。它不做任何 I/O，
%%% 不含业务规则，**也不改写 EB-02 的冻结事实**（`eb_ports:all/0` 仍然返回
%%% 契约模块；本模块另立一份实现映射，避免把实现名塞进冻结契约）。
%%%
%%% EB-03R 的变更：
%%%   * R0-5 修复：`resolve(asset)` 不再是 `{error, {not_implemented_yet, asset}}`，
%%%     而是 `{ok, eb_asset_store}`（P13/A11 的装配证据）；
%%%   * 追加 `member_fact`（P10）、`tx`（T1/T2）、`purge`（T3）三个用例级端口；
%%%   * C6：`resolve/1` 增加**契约与装配的同步校验**——任一 `eb_ports:all/0`
%%%     声明但未装配的端口，显式返回 `{error, {unimplemented_port, Port}}`
%%%     （不静默、不返回空实现），并由 `eb_ports_tests` 覆盖。
-module(eb_infra_ports).

-export([
    store/0,
    crypto/0,
    clock/0,
    id/0,
    audit/0,
    asset/0,
    member_fact/0,
    tx/0,
    purge/0,
    resolve/1,
    implementations/0,
    declared_ports/0
]).

%% 注意：不把类型命名为 `port()`——那是 Erlang 内建类型，重定义会被
%% warnings-as-errors 直接拦下。
-type port_contract() ::
    eb_store_port
    | eb_crypto_port
    | eb_clock_port
    | eb_id_port
    | eb_audit_port
    | eb_asset_port
    | eb_auth_port
    | eb_member_fact_port
    | eb_tx_port
    | eb_purge_port.
-type implementation() :: module().

-export_type([port_contract/0, implementation/0]).

%% @doc 持久化读写端口实现。
-spec store() -> implementation().
store() -> eb_pg_store.

%% @doc 企业托管加密端口实现。
-spec crypto() -> implementation().
crypto() -> eb_managed_crypto.

%% @doc 注入时钟端口实现（唯一允许读系统时间的地方；domain 只接受其返回值）。
-spec clock() -> implementation().
clock() -> eb_system_clock.

%% @doc 注入 ID 端口实现（TSID）。
-spec id() -> implementation().
id() -> eb_tsid.

%% @doc append-only 审计写入端口实现。
-spec audit() -> implementation().
audit() -> eb_pg_audit.

%% @doc 企业私有对象 + asset metadata 端口实现（EB-03R：R0-5 修复，不再 not_implemented）。
%%
%% 对象读写经**本地替身** adapter（进程内 ETS 桶）实现契约语义（装配 / 作用域 /
%% 错误传播）；真实 Garage 验收**未运行**（A11 边界声明），报告不得据此声明真实
%% 对象存储验收通过。
-spec asset() -> implementation().
asset() -> eb_asset_store.

%% @doc 授权事实端口实现（**只读**、逐请求加载；C5/D7 的装配补齐）。
%%
%% EB-04 的 `eb_auth_app:authorize_via_port/3` 接受注入的端口模块；本模块是装配
%% 选择的默认实现（成员关系 + 有效经办 + 基于角色的静态权限集，全部只读）。
-spec auth_impl() -> implementation().
auth_impl() -> eb_pg_auth_facts.

%% @doc 最小只读事实端口实现（成员状态 / 默认 Workspace，PG 直读）。
-spec member_fact() -> implementation().
member_fact() -> eb_member_fact_pg.

%% @doc 用例级事务端口实现（canonical message + policy snapshot + audit 同事务）。
-spec tx() -> implementation().
tx() -> eb_pg_tx.

%% @doc bounded purge 用例级端口实现（唯一物理删除通道的窄信封）。
-spec purge() -> implementation().
purge() -> eb_pg_purge_port.

%% @doc 端口 → 已装配实现；未装配的端口显式失败（不返回空实现）。
%%
%% 判定顺序固定：**已装配** → **已声明但未装配**（`{error, {unimplemented_port, P}}`）
%% → **未知端口**（`{error, {unknown_port, P}}`）。
-spec resolve(term()) -> {ok, implementation()} | {error, term()}.
resolve(Port) ->
    case lists:keyfind(Port, 1, by_key()) of
        {Port, Impl} ->
            {ok, Impl};
        false ->
            case lists:member(Port, eb_ports:all()) of
                true -> {error, {unimplemented_port, Port}};
                false -> {error, {unknown_port, Port}}
            end
    end.

%% @doc 已装配的 (契约模块, 实现模块) 列表，供契约一致性测试逐条核对。
-spec implementations() -> [{port_contract(), implementation()}].
implementations() ->
    [
        {eb_ports:store(), store()},
        {eb_ports:crypto(), crypto()},
        {eb_ports:clock(), clock()},
        {eb_ports:id(), id()},
        {eb_ports:audit(), audit()},
        {eb_ports:asset(), asset()},
        {eb_ports:auth(), auth_impl()},
        {eb_ports:member_fact(), member_fact()},
        {eb_ports:tx(), tx()},
        %% 用例级 purge 端口由本层拥有（application/** 不得出现 eb_pg_ 前缀模块名）。
        {eb_purge_port, purge()}
    ].

%% @doc 本层声明的全部端口契约模块（= `eb_ports:all/0`）。
-spec declared_ports() -> [port_contract()].
declared_ports() ->
    eb_ports:all().

%% 域键 → (契约模块, 实现模块)：键是调用方用的短名，值是装配事实。
by_key() ->
    [
        {store, store()},
        {crypto, crypto()},
        {clock, clock()},
        {id, id()},
        {audit, audit()},
        {asset, asset()},
        {auth, auth_impl()},
        {member_fact, member_fact()},
        {tx, tx()},
        {purge, purge()}
    ].
