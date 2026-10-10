-module(elib_tsid).

%%%
% TSID - 时间有序分布式唯一 ID 生成器（V10：多 Namespace 独立游标、无持久化）
%
% 64-bit 有符号整数，适配 PostgreSQL BIGINT
%
% 位布局 (63 可用位 + 1 符号位):
%   [0] [timestamp: 42 bits] [node: 10 bits] [sequence: 11 bits]
%   sign  毫秒时间戳           DC+节点标识      毫秒内序列号
%
% 容量:
%   - 时间跨度: 2^42 ms ≈ 139.5 年 (纪元 2025-01-01 → 2164 年)
%   - 节点总数: 2^10 = 1024 (DC×Node 任意分配)
%   - 每节点每毫秒每 Namespace: 2^11 = 2048 个 ID
%   - 数字位数: 永远 ≤ 19 位 (BIGINT 最大值 9.2 × 10^18)
%
% 唯一性保证（V10 保障边界，不得夸大）:
%   - 同一 Namespace 内、单次 BEAM 生命周期内：严格唯一（含时钟任意回拨/前跳）。
%   - 跨 Namespace：数值允许重复；跨业务引用必须 Domain + ID。
%   - 跨重启：仅当时钟未回拨跨重启点时不重复；系统时钟在两次运行之间被
%     回拨（NTP 修正/手动改时/快照恢复）可能产生与历史 ID 的数值重叠——
%     这是无持久化设计的固有边界，不是缺陷。
%
% 时钟（V10 冻结，见 .Codex/tsid_v10/W2-time-algorithm-report.md）:
%   Monotonic Clock + Fixed Offset：init 采集 Offset = system - monotonic，
%   运行期 wall = monotonic + Offset。系统时钟运行期跳变对生成器不可见。
%
% 无借支铁律:
%   Sequence 耗尽时绝不主动推进逻辑毫秒。仅当下一号开启的新毫秒超出
%   游标当前毫秒且超出时钟当前毫秒时，有界等待真实时间推进（默认上限
%   capacity_wait_timeout_ms=100ms）；超时抛 {elib_tsid_capacity_exhausted, _}。
%
% Namespace（V10）:
%   一个业务实体一张主键 ID 空间 = 一个独立 Cursor（atomics 槽位）；
%   写同一张表的标签静态映射到同一槽位（group→group_info；
%   audit/enterprise_audit→enterprise_audit_event）。未知名 typed error，
%   无动态注册、无静默兜底。热路径零 GenServer/ETS/磁盘/数据库。
%
% 使用:
%   elib_tsid:init(#{dc_id => 1, node_id => 1}).   %% imboy_sup 启动时
%   Id = elib_tsid:generate(user).
%   Info = elib_tsid:parse(Id).
%
% default 生成器仅供测试/脚手架生成唯一整数；生产代码必须使用显式
% namespace（elib_tsid_registration_guard_tests 扫描 src 强制）。
%%%

-export([init/1, register/1]).
-export([generate/0, generate/1, generate_n/1, generate_n/2]).
-export([parse/1, timestamp/1, node_id/1, from_binary/1]).
-export([registered/0]).
-export([namespaces/0, aliases/0]).
-export([readiness/0]).

%% 内部测试/实现 seam：供 EUnit 固定时钟模型测试与 reservation 实现共用。
%% 不属于公开 API 契约——本组函数语义变更必须同步 elib_tsid_tests。
-export([reserve_candidate/3, slot_to_id/2, id_to_slot/1]).
-export([wall_clock_ms/0, monotonic_ms/0]).
%% 跨配置测试显式 reset（禁止静默重配节点偷跑）
-export([reset_for_test/0]).
%% 观测 instrumentation（测试与监控共用，非公开 API 契约）
-export([reservation_count/0]).
%% 部署配置链最后一道布局校验（imboy_sup 装配 init 配置时直调）
-export([combine_node/3]).

%% ===================================================================
%% 位布局常量
%% ===================================================================

%% 自定义纪元: 2025-01-01 00:00:00 UTC (毫秒)
-define(EPOCH_MS, 1735689600000).

%% 139.5 年
-define(TIMESTAMP_BITS, 42).
%% 1024 节点
-define(NODE_BITS, 10).
%% 2048/ms/node/namespace
-define(SEQUENCE_BITS, 11).

%% 21
-define(TIMESTAMP_SHIFT, (?NODE_BITS + ?SEQUENCE_BITS)).
%% 11
-define(NODE_SHIFT, ?SEQUENCE_BITS).

%% 2047
-define(SEQUENCE_MASK, ((1 bsl ?SEQUENCE_BITS) - 1)).
%% 1023
-define(NODE_MASK, ((1 bsl ?NODE_BITS) - 1)).
-define(TIMESTAMP_MASK, ((1 bsl ?TIMESTAMP_BITS) - 1)).

%% 42-bit 相对时间戳上界（最后合法毫秒 2164-05-15T07:35:11.103Z）
-define(MAX_REL_TS, ?TIMESTAMP_MASK).
%% signed 63-bit 上界 = 最后合法 (ts,node,seq) 组合
-define(MAX_ID, ((1 bsl 63) - 1)).

%% generate_n 分块：单次 CAS 预留的 slot 区间上限（= 单毫秒容量，保守不跨毫秒放大会大）
-define(MAX_BATCH_CHUNK, 2048).

-define(PT_RUNTIME, elib_tsid_runtime).
-define(PT_NODE_ID, elib_tsid_node_id).
-define(PT_DC_BITS, elib_tsid_dc_bits).

%% 进程级时钟 seam 键（仅测试进程显式 put；生产路径无全局可变状态）
-define(TEST_WALL_MS, {elib_tsid, test_wall_ms}).
-define(TEST_MONOTONIC_MS, {elib_tsid, test_monotonic_ms}).

%% ===================================================================
%% Namespace 静态清单（W1 冻结，见 .Codex/tsid_v10/W1-namespace-design.md）
%%
%% 一个 canonical 名称 = 一个业务实体的主键 ID 空间 = 一个独立游标槽位。
%% 追加新实体时只能在列表尾部追加（顺序即槽位号，保持稳定便于诊断）。
%% 未在此清单且不在 aliases/0 的名称一律 generate 报错。
%% ===================================================================

-spec namespaces() -> [atom()].
namespaces() ->
    [
        %% ── 用户 ──
        user,
        user_device,
        user_collect,
        user_denylist,
        user_tag,
        user_tag_relation,
        user_deletion_request,
        user_deletion_job,
        %% ── 审核治理 ──
        moderation_action,
        moderation_appeal,
        review_queue,
        %% ── 好友 ──
        friend,
        friend_category,
        %% ── 群组（group 为 group_info 的别名：同写 public.group 表）──
        group_info,
        group_member,
        group_notice,
        group_log,
        group_random_code,
        group_category,
        group_tag,
        group_vote,
        group_vote_option,
        group_schedule,
        group_schedule_reminder,
        group_album,
        group_album_photo,
        group_album_comment,
        group_file,
        group_task,
        group_task_assignment,
        %% ── 消息（msg_id 标识用，不做排序）──
        msg_c2c,
        msg_c2g,
        msg_s2c,
        msg_store,
        msg_mention,
        msg_forward,
        msg_reaction,
        %% ── 会话 ──
        conversation_pin,
        conversation_delete,
        %% ── 附件 / 媒体 ──
        attachment,
        %% ── 朋友圈 ──
        moment_post,
        moment_comment,
        moment_like,
        moment_timeline,
        moment_post_acl,
        moment_report,
        %% ── 频道 ──
        channel,
        channel_message,
        channel_subscription,
        channel_admin,
        channel_message_view,
        channel_order,
        channel_invitation,
        channel_webhook,
        channel_comment,
        channel_price,
        %% ── Organization / 工作区 ──
        organization,
        organization_invitation,
        organization_department,
        workspace,
        workspace_invite,
        organization_invite_code,
        owner_activation_invite,
        %% ── 项目 ──
        project,
        project_task,
        project_event,
        project_milestone,
        %% ── 反馈 ──
        feedback,
        feedback_reply,
        %% ── E2EE 恢复 ──
        e2ee_key_backup,
        %% ── 举报 ──
        report_ticket,
        report_action_log,
        %% ── 钱包 / 支付 ──
        wallet,
        wallet_transaction,
        recharge_order,
        payment_transaction,
        red_packet,
        red_packet_receive,
        transfer_order,
        agent_payment_compensation,
        agent_payment_mandate,
        %% ── SaaS 计费 ──
        billing_plan,
        billing_subscription,
        billing_invoice,
        billing_usage,
        %% ── 直播 ──
        live_room,
        %% ── 推送 ──
        push_token,
        %% ── 合规 ──
        compliance_key,
        sensitive_word,
        %% ── Olm（X3DH + Double Ratchet）单聊 E2EE ──
        olm_identity,
        olm_one_time_key,
        olm_fallback_key,
        trust_audit,
        %% ── 管理员 ──
        adm_user,
        adm_role,
        admin_op_log,
        %% ── 插件 ──
        plugin_audit_log,
        %% ── 应用配置 ──
        app_version,
        app_ddl,
        app_upgrade_log,
        %% ── MCP 治理 ──
        mcp_client,
        mcp_client_grant,
        mcp_audit_log,
        %% ── SSO ──
        sso_identity,
        sso_config,
        %% ── AI Agent ──
        ai_agent_role_version,
        agent_task,
        %% ── Moya（教学域）──
        moya_task,
        moya_assignment,
        moya_submission,
        submission_asset,
        calligraphy_review_draft,
        teacher_review,
        review_asset,
        teaching_admin_audit,
        %% ── Customer Service（cs_id_port 的 Kind）──
        business_identity,
        cs_event,
        cs_read_cursor,
        cs_widget_nonce,
        organization_business_identity_assignment,
        %% ── Enterprise Business ──
        enterprise_message,
        enterprise_message_delivery,
        enterprise_message_origin,
        enterprise_audit_event,
        enterprise_application,
        enterprise_application_grant,
        enterprise_application_credential,
        enterprise_external_identity,
        enterprise_oa_sso_code,
        %% ── Enterprise Business（动态 Kind 补录，W7 全量取证：这些 Kind 经
        %% eb_*_app:new_id(Kind) 变量传入，W0 字面量扫描未覆盖）──
        enterprise_asset,
        enterprise_conversation,
        enterprise_offboarding_case,
        enterprise_contact_assignment,
        enterprise_note,
        enterprise_retention_hold,
        enterprise_retention_policy,
        enterprise_offboarding_item,
        %% ── Customer Service（动态 Kind 补录，同上）──
        cs_session,
        cs_visit_token,
        cs_shop_key,
        cs_seat_console,
        cs_seat_console_public,
        cs_widget_installation,
        cs_widget_installation_public
    ].

%% @doc 别名 → canonical 映射：写同一张表的旧标签静态归并到同一游标。
%% group → group_info（同写 public.group）；
%% audit / enterprise_audit → enterprise_audit_event（同写 enterprise_audit_event 表）。
-spec aliases() -> #{atom() => atom()}.
aliases() ->
    #{
        group => group_info,
        audit => enterprise_audit_event,
        enterprise_audit => enterprise_audit_event
    }.

%% ===================================================================
%% 初始化
%% ===================================================================

%% @doc 初始化 TSID 生成器（imboy_sup 启动时调用一次）
%%
%% Opts:
%%   dc_id    - 数据中心 ID (必填)
%%   node_id  - 节点 ID (必填)
%%   dc_bits  - DC 占用的位数 (默认 3, 即 8 个 DC × 128 个节点)
%%   capacity_wait_timeout_ms - 序列耗尽等待上限 (默认 100)
%%   max_clock_drift_ms       - 固定 Offset 时钟与系统钟的允许偏差上界
%%                              (默认 5000；readiness 检查超限即阻断发号)
%%
%% DC/Node 位分配示例:
%%   dc_bits=0 → 1 DC × 1024 nodes
%%   dc_bits=2 → 4 DC × 256 nodes
%%   dc_bits=3 → 8 DC × 128 nodes (默认)
%%
%% 幂等契约：相同配置重复 init 幂等返回 ok 且游标不回退；不同配置 re-init 抛
%% `{elib_tsid_already_initialized, _}'，旧 runtime 保持不变。
%% 测试如需换配置必须先 reset_for_test/0。
-spec init(map()) -> ok.
init(Opts) ->
    DcBits = maps:get(dc_bits, Opts, 3),
    DcId = maps:get(dc_id, Opts),
    NodeId = maps:get(node_id, Opts),
    CapWait = maps:get(capacity_wait_timeout_ms, Opts, 100),
    MaxDrift = maps:get(max_clock_drift_ms, Opts, 5000),
    true =
        (is_integer(DcBits) andalso DcBits >= 0 andalso DcBits =< ?NODE_BITS) orelse
            error({elib_tsid_invalid_config, #{dc_bits => DcBits}}),
    CombinedNode = combine_node(DcId, NodeId, DcBits),
    true =
        (is_integer(CapWait) andalso CapWait > 0) orelse
            error({elib_tsid_invalid_config, #{capacity_wait_timeout_ms => CapWait}}),
    true =
        (is_integer(MaxDrift) andalso MaxDrift > 0) orelse
            error({elib_tsid_invalid_config, #{max_clock_drift_ms => MaxDrift}}),

    case persistent_term:get(?PT_RUNTIME, undefined) of
        undefined ->
            publish_runtime(CombinedNode, DcBits, CapWait, MaxDrift),
            ok;
        #{
            combined_node := CombinedNode,
            dc_bits := DcBits,
            capacity_wait_timeout_ms := CapWait,
            max_clock_drift_ms := MaxDrift
        } ->
            %% 幂等：同配置保留既有游标（不回退）
            ok;
        Existing ->
            error(
                {elib_tsid_already_initialized, #{
                    existing => maps:with(
                        [combined_node, dc_bits, capacity_wait_timeout_ms, max_clock_drift_ms],
                        Existing
                    ),
                    requested => #{
                        combined_node => CombinedNode,
                        dc_bits => DcBits,
                        capacity_wait_timeout_ms => CapWait,
                        max_clock_drift_ms => MaxDrift
                    }
                }}
            )
    end.

%% @private 一次性发布完整 runtime（固定 Offset 采集 + 静态槽位表 + atomics 游标数组）
publish_runtime(CombinedNode, DcBits, CapWait, MaxDrift) ->
    Names = namespaces(),
    AliasMap = aliases(),
    %% canonical 槽位：namespaces/0 顺序 + 1（atomics 1-based）；default 追加尾部
    CanonicalSlots =
        #{
            Name => I
         || {I, Name} <- lists:zip(lists:seq(1, length(Names)), Names)
        },
    AliasSlots =
        #{
            Alias => maps:get(Canonical, CanonicalSlots)
         || Alias := Canonical <- AliasMap
        },
    %% default 占尾槽（仅供测试/脚手架；生产由 registration_guard 禁用）
    DefaultSlot = length(Names) + 1,
    Cursors = atomics:new(DefaultSlot, [{signed, true}]),
    Lookup = maps:merge(maps:merge(CanonicalSlots, AliasSlots), #{default => DefaultSlot}),
    Stats = atomics:new(1, [{signed, true}]),
    %% 时钟安全闸（W4）：1=时钟可用，0=readiness 检出偏差超限（阻断发号）。
    %% readiness/0 低频评估并翻转此闸，生成热路径一次 atomic load 消费——
    %% 异常状态作用于实际生成入口，而非只改健康检查返回值。
    ClockOk = atomics:new(1, [{signed, true}]),
    ok = atomics:put(ClockOk, 1, 1),
    OffsetMs = erlang:system_time(millisecond) - erlang:monotonic_time(millisecond),
    Handle = #{
        cursors => Cursors,
        lookup => Lookup,
        reservations => Stats,
        clock_ok => ClockOk,
        combined_node => CombinedNode,
        dc_bits => DcBits,
        capacity_wait_timeout_ms => CapWait,
        max_clock_drift_ms => MaxDrift,
        clock_offset_ms => OffsetMs
    },
    persistent_term:put(?PT_NODE_ID, CombinedNode),
    persistent_term:put(?PT_DC_BITS, DcBits),
    persistent_term:put(?PT_RUNTIME, Handle),
    ok.

%% @doc 校验并组合 10-bit CombinedNode。
%% 越界输入必须 error 而非静默 bor：NodeId 溢出 node 位段会覆盖 dc 位
%% （如 (1,200,3) bor 出 200——dc 信息丢失且与其它合法组合碰撞可能），
%% 这是部署配置链的最后一道布局校验。
-spec combine_node(non_neg_integer(), non_neg_integer(), 0..10) -> 0..1023.
combine_node(DcId, NodeId, DcBits) when
    is_integer(DcId),
    DcId >= 0,
    DcBits >= 0,
    DcBits =< ?NODE_BITS,
    is_integer(NodeId),
    NodeId >= 0,
    NodeId < (1 bsl (?NODE_BITS - DcBits)),
    DcId < (1 bsl DcBits)
->
    NodeBits = ?NODE_BITS - DcBits,
    (DcId bsl NodeBits) bor NodeId;
combine_node(DcId, NodeId, DcBits) ->
    error(
        {elib_tsid_invalid_config, #{
            dc_id => DcId,
            node_id => NodeId,
            dc_bits => DcBits,
            constraint => {combined_node_bits, ?NODE_BITS}
        }}
    ).

%% @doc 校验名称已在静态清单中（幂等；未知名 error）。
%%
%% V10 无动态注册：本函数只做清单断言，供历史 ensure-registered 调用点
%% （cs_tsid / eb_tsid / enterprise_message_repo 等）与测试平滑过渡。
%% 对已知名恒 ok，不产生任何状态变更。
-spec register(atom() | [atom()]) -> ok.
register(Names) when is_list(Names) ->
    lists:foreach(fun register/1, Names),
    ok;
register(Name) when is_atom(Name) ->
    case persistent_term:get(?PT_RUNTIME, undefined) of
        #{lookup := Lookup} ->
            case maps:is_key(Name, Lookup) of
                true ->
                    ok;
                false ->
                    error(
                        {elib_tsid_generator_not_registered,
                            {Name, 'not in elib_tsid:namespaces/0 or aliases/0'}}
                    )
            end;
        undefined ->
            error({elib_tsid_not_initialized, 'call elib_tsid:init/1 before register/1'})
    end.

%% @private 列出全部可用生成器名称（canonical + 别名 + default，排序）。
%% lookup 在 publish_runtime 已合并 default 键，这里不得再前置追加。
-spec registered() -> [atom()].
registered() ->
    lists:sort(maps:keys(lookup_or_empty())).

lookup_or_empty() ->
    case persistent_term:get(?PT_RUNTIME, undefined) of
        #{lookup := Lookup} -> Lookup;
        undefined -> #{}
    end.

%% @doc 就绪状态（/readyz 消费）。
%%
%% 低频时钟安全检查：固定 Offset 时钟（mono+offset）与真实系统钟的当前
%% 偏差超过 max_clock_drift_ms 时，翻转 runtime 的 clock_ok 闸并返回
%% clock_unsafe——生成热路径读取同一闸，阻断发号（异常作用于生成入口）。
%% 偏差回落后的下一次 readiness() 评估即解除（clock_ok 翻回 1）。边界
%% 如实声明：主机/VM 挂起（monotonic 不计休眠时长）造成的偏差**不会
%% 自愈**，将持续 clock_unsafe 直至节点重启（init 重新采集 Offset）；
%% 生产服务器不休眠，影响面为开发机与可暂停 VM。
%%
%% 探针合同（如实声明，不得夸大）：本模块**没有内置周期探针**。clock_ok
%% 闸只在 readiness()/0 被调用时重新评估（HTTP /readyz、/healthz 及
%% healthz_handler:tsid_readiness/0 直调）；两次调用之间偏差突增不受影响，
%% 生成照常放行。偏差暴露窗口 = 外部就绪探测的轮询间隔；部署侧未配置
%% 轮询时窗口无固定上界。收敛要求：compose/k8s 必须为 imboy 服务配置
%% 打 /readyz 的 healthcheck/readinessProbe，该间隔即窗口上界。
-spec readiness() -> ready | clock_unsafe | not_initialized.
readiness() ->
    case persistent_term:get(?PT_RUNTIME, undefined) of
        #{
            clock_ok := ClockOk,
            clock_offset_ms := Offset,
            max_clock_drift_ms := MaxDrift
        } ->
            Drift = abs(
                erlang:monotonic_time(millisecond) + Offset - erlang:system_time(millisecond)
            ),
            case Drift > MaxDrift of
                true ->
                    ok = atomics:put(ClockOk, 1, 0),
                    clock_unsafe;
                false ->
                    ok = atomics:put(ClockOk, 1, 1),
                    ready
            end;
        undefined ->
            not_initialized
    end.

%% ===================================================================
%% ID 生成
%% ===================================================================

%% @doc default 生成器（过渡期保留；业务代码应使用显式 namespace）
-spec generate() -> pos_integer().
generate() ->
    generate(default).

%% @doc 使用指定 Namespace 生成一个 TSID
%%
%% 同 Namespace 内严格唯一（单次运行内）；跨 Namespace 允许数值重复。
-spec generate(atom()) -> pos_integer().
generate(Name) when is_atom(Name) ->
    Handle = require_runtime(Name),
    #{combined_node := NodeId} = Handle,
    Idx = slot_of(Handle, Name),
    Budget = {timeout, maps:get(capacity_wait_timeout_ms, Handle)},
    {First, _Last} = reserve(Handle, Idx, 1, Budget),
    slot_to_id(First, NodeId).

%% @doc default 批量生成 N 个 TSID (有序)
-spec generate_n(pos_integer()) -> [pos_integer()].
generate_n(N) when N > 0 ->
    generate_n(default, N).

%% @doc 指定 Namespace 批量生成 N 个 TSID (有序)
%%
%% 按 ?MAX_BATCH_CHUNK 分块，每块一次 CAS 预留连续区间；块间从最新游标
%% 重新 reserve。预算跨块守恒：进入批量即一次性物化为绝对 monotonic
%% deadline，所有块共享同一剩余预算；块内冲突/等待重试沿用该 deadline，
%% 不因换块重置。
-spec generate_n(atom(), pos_integer()) -> [pos_integer()].
generate_n(Name, N) when is_atom(Name), N > 0 ->
    Handle = require_runtime(Name),
    #{combined_node := NodeId} = Handle,
    Idx = slot_of(Handle, Name),
    Budget = {deadline, monotonic_ms() + maps:get(capacity_wait_timeout_ms, Handle)},
    lists:append([
        materialize_range(First, Last, NodeId)
     || {First, Last} <-
            [reserve(Handle, Idx, C, Budget) || C <- chunk_sizes(N, ?MAX_BATCH_CHUNK)]
    ]).

%% @private 名称校验 + runtime 读取（返回槽位已解析的 Handle）
-spec require_runtime(atom()) -> map().
require_runtime(Name) ->
    case persistent_term:get(?PT_RUNTIME, undefined) of
        #{lookup := Lookup} = Handle ->
            case maps:find(Name, Lookup) of
                {ok, _Slot} ->
                    Handle;
                error ->
                    error(
                        {elib_tsid_generator_not_registered,
                            {Name, 'not in elib_tsid:namespaces/0 or aliases/0'}}
                    )
            end;
        undefined ->
            error({elib_tsid_not_initialized, 'call elib_tsid:init/1 first'})
    end.

%% @private 超大 N 分有界 chunk
-spec chunk_sizes(pos_integer(), pos_integer()) -> [pos_integer()].
chunk_sizes(N, MaxChunk) when N =< MaxChunk ->
    [N];
chunk_sizes(N, MaxChunk) ->
    [MaxChunk | chunk_sizes(N - MaxChunk, MaxChunk)].

%% @private 单次 reservation：读时钟 → 纯 candidate → 无借支检查 → CAS。
%% Budget 为 {timeout, Ms}（未物化）或 {deadline, AbsMs}（已物化）：
%% 热路径零单调钟开销，首次需要等待/冲突重试时才惰性物化，物化后
%% 递归全程携带同一绝对 deadline（预算守恒，不因重试重置）。
-spec reserve(
    map(), pos_integer(), pos_integer(), {timeout, pos_integer()} | {deadline, integer()}
) ->
    {FirstSlot :: non_neg_integer(), LastSlot :: non_neg_integer()}.
reserve(#{cursors := Cursors, clock_ok := ClockOk} = Handle, Idx, Count, Budget0) ->
    case atomics:get(ClockOk, 1) of
        1 ->
            ok;
        0 ->
            error({elib_tsid_clock_unsafe, #{hint => 'readiness() reports clock drift'}})
    end,
    Now = wall_clock_ms() - ?EPOCH_MS,
    case Now < 0 of
        true -> error({elib_tsid_clock_before_epoch, #{now_rel => Now}});
        false -> ok
    end,
    case Now > ?MAX_REL_TS of
        true ->
            error(
                {elib_tsid_timestamp_exhausted, #{
                    now_rel => Now, max_rel_ts => ?MAX_REL_TS
                }}
            );
        false ->
            ok
    end,
    Old = atomics:get(Cursors, Idx),
    case reserve_candidate(Old, Now, Count) of
        {ok, First, Last} ->
            commit_reserve(Handle, Idx, Old, First, Last, Now, Budget0, Count);
        {error, Reason} ->
            error(Reason)
    end.

slot_of(#{lookup := Lookup}, Name) ->
    maps:get(Name, Lookup).

%% @private 无借支检查 + CAS 提交
%%
%% 无借支铁律：First 开启的新毫秒（FirstTs > CurTs）且超出时钟当前毫秒
%% （FirstTs > Now）时，必须等待真实时间推进到该毫秒——绝不借未来。
%% reserve_candidate 已保证区间不跨毫秒（终点毫秒 == 起点毫秒），
%% 因此只需校验起点即可覆盖整个区间，含批量终点。
%% 游标领先于时钟（回拨后 CurTs >= Now）不算借支：这些槽位属于已被
%% 消费过的毫秒，继续用完它们不会重复。
commit_reserve(Handle, Idx, Old, First, Last, Now, Budget0, Count) ->
    FirstTs = First bsr ?SEQUENCE_BITS,
    CurTs = Old bsr ?SEQUENCE_BITS,
    case FirstTs > Now andalso FirstTs > CurTs of
        true ->
            Budget = ensure_deadline(Budget0),
            case wait_step(Budget, FirstTs - Now) of
                ok ->
                    reserve(Handle, Idx, Count, {deadline, Budget});
                timeout ->
                    error(
                        {elib_tsid_capacity_exhausted, #{
                            lead_ms => FirstTs - Now,
                            phase => clock_wait
                        }}
                    )
            end;
        false ->
            commit_cas(Handle, Idx, Old, First, Last, Budget0, Count)
    end.

commit_cas(
    #{cursors := Cursors, reservations := Stats} = Handle, Idx, Old, First, Last, Budget0, Count
) ->
    case atomics:compare_exchange(Cursors, Idx, Old, Last) of
        ok ->
            _ = atomics:add(Stats, 1, 1),
            {First, Last};
        _Other ->
            %% 冲突：deadline 预算内用刷新后的墙钟重试；
            %% 预算在此物化一次并随重试守恒
            Budget = ensure_deadline(Budget0),
            case monotonic_ms() >= Budget of
                true ->
                    error(
                        {elib_tsid_capacity_exhausted, #{
                            phase => cas_contention
                        }}
                    );
                false ->
                    reserve(Handle, Idx, Count, {deadline, Budget})
            end
    end.

-spec ensure_deadline({timeout, pos_integer()} | {deadline, integer()}) -> integer().
ensure_deadline({timeout, T}) -> monotonic_ms() + T;
ensure_deadline({deadline, D}) -> D.

%% @private 无 busy-spin 等待：睡眠由 deadline 剩余量约束；单调钟停滞
%% （异常/seam 冻结）立即 fail-closed，绝不无限等待。
-spec wait_step(integer(), pos_integer()) -> ok | timeout.
wait_step(Deadline, NeededMs) ->
    Before = monotonic_ms(),
    case Before >= Deadline of
        true ->
            timeout;
        false ->
            Remaining = Deadline - Before,
            Sleep = erlang:min(erlang:max(1, NeededMs), Remaining),
            ok = timer:sleep(Sleep),
            case monotonic_advanced(Before) of
                true -> ok;
                false -> timeout
            end
    end.

-spec monotonic_advanced(integer()) -> boolean().
monotonic_advanced(BeforeMs) ->
    case get(?TEST_MONOTONIC_MS) of
        undefined ->
            erlang:monotonic_time(microsecond) > BeforeMs * 1000;
        _SeamValue ->
            monotonic_ms() > BeforeMs
    end.

%% @private slot 区间本地展开为 ID 列表（纯计算，无共享状态访问）
-spec materialize_range(
    non_neg_integer(), non_neg_integer(), 0..1023
) -> [pos_integer()].
materialize_range(First, Last, NodeId) ->
    [slot_to_id(S, NodeId) || S <- lists:seq(First, Last)].

%% @private reservation 成功计数（全局观测）
-spec reservation_count() -> non_neg_integer().
reservation_count() ->
    case persistent_term:get(?PT_RUNTIME, undefined) of
        #{reservations := Stats} -> atomics:get(Stats, 1);
        undefined -> 0
    end.

%% ===================================================================
%% 解析 / 提取
%% ===================================================================

%% @doc 解析 TSID 为各组成部分
%%
%% 仅接受 `1..?MAX_ID'（正 signed 63-bit）；非 integer、`=<0'、
%% `>2^63-1' 抛 `{elib_tsid_invalid_input, _}'（不再掩码截断）。
%%
%% 精度声明：`timestamp' 保留毫秒精度；
%% `created_at' 为秒精度 datetime（毫秒被截断，不是四舍五入）。
-spec parse(pos_integer()) -> map().
parse(Id) ->
    validate_id(Id),
    RelTs = (Id bsr ?TIMESTAMP_SHIFT) band ?TIMESTAMP_MASK,
    Node = (Id bsr ?NODE_SHIFT) band ?NODE_MASK,
    Seq = Id band ?SEQUENCE_MASK,

    AbsMs = RelTs + ?EPOCH_MS,
    DcBits = persistent_term:get(?PT_DC_BITS, 3),
    NodeBits = ?NODE_BITS - DcBits,

    #{
        id => Id,
        timestamp => AbsMs,
        dc_id => Node bsr NodeBits,
        node_id => Node band ((1 bsl NodeBits) - 1),
        sequence => Seq,
        created_at => calendar:system_time_to_universal_time(AbsMs * 1000, microsecond)
    }.

%% @doc 从 TSID 提取 Unix 毫秒时间戳
-spec timestamp(pos_integer()) -> pos_integer().
timestamp(Id) ->
    validate_id(Id),
    ((Id bsr ?TIMESTAMP_SHIFT) band ?TIMESTAMP_MASK) + ?EPOCH_MS.

%% @doc 从 TSID 提取节点标识 (含 DC)
-spec node_id(pos_integer()) -> non_neg_integer().
node_id(Id) ->
    validate_id(Id),
    (Id bsr ?NODE_SHIFT) band ?NODE_MASK.

%% @private 统一输入边界：1..MAX_ID（signed PostgreSQL BIGINT 正数域）
-spec validate_id(term()) -> ok.
validate_id(Id) when is_integer(Id), Id >= 1, Id =< ?MAX_ID ->
    ok;
validate_id(Id) ->
    error({elib_tsid_invalid_input, #{id => Id, valid_range => {1, ?MAX_ID}}}).

%% ===================================================================
%% 内部 seam：纯 reservation 模型与时钟注入
%%
%% 契约要点（变更须同步 elib_tsid_tests）：
%%   - cursor 是线性 slot：Slot = Ts bsl 11 bor Seq，单游标内全程单调；
%%   - candidate 区间 [First, Last] 满足 First = max(Old+1, Now bsl 11)，
%%     且区间绝不跨毫秒（中段起点 + 满容量时整体滚入下一毫秒 seq 0）；
%%   - 时钟回拨由 cursor 吸收（First 沿用旧毫秒），绝不回退；
%%   - 无借支：开新毫秒且超时钟 → 有界等待，超时 typed fail；
%%   - 纪元前 / 越过 42-bit 上界一律 typed fail-closed，绝不掩码截断；
%%   - wall_clock_ms / monotonic_ms 为进程级测试 seam：仅当调用进程
%%     显式 put 测试键时偏离真实时钟，生产路径无全局可变状态。
%% ===================================================================

%% @private 纯函数：给定 cursor、当前相对毫秒与数量，计算连续 slot 区间
%%
%% 区间绝不跨毫秒：起点落在毫秒中段且 Count 顶到容量边界时，整体滚入
%% 下一毫秒的 seq 0。由此终点毫秒恒等于起点毫秒，无借支检查只需校验
%% 区间起点（见 commit_reserve）。Count 超过单毫秒容量为调用方错误。
-spec reserve_candidate(integer(), integer(), pos_integer()) ->
    {ok, FirstSlot :: non_neg_integer(), LastSlot :: non_neg_integer()}
    | {error, {
        elib_tsid_clock_before_epoch
        | elib_tsid_timestamp_exhausted
        | elib_tsid_invalid_input,
        map()
    }}.
reserve_candidate(OldCursor, NowRel, Count) when
    is_integer(OldCursor), is_integer(NowRel), is_integer(Count), Count > 0
->
    case NowRel < 0 of
        true ->
            {error, {elib_tsid_clock_before_epoch, #{now_rel => NowRel}}};
        false when Count > ?MAX_BATCH_CHUNK ->
            {error,
                {elib_tsid_invalid_input, #{
                    count => Count, max_chunk => ?MAX_BATCH_CHUNK
                }}};
        false ->
            First0 = max(OldCursor + 1, NowRel bsl ?SEQUENCE_BITS),
            First =
                case First0 band ?SEQUENCE_MASK of
                    Seq0 when Seq0 > 0, Seq0 + Count > ?MAX_BATCH_CHUNK ->
                        ((First0 bsr ?SEQUENCE_BITS) + 1) bsl ?SEQUENCE_BITS;
                    _ ->
                        First0
                end,
            Last = First + Count - 1,
            LastTs = Last bsr ?SEQUENCE_BITS,
            case LastTs > ?MAX_REL_TS of
                true ->
                    {error,
                        {elib_tsid_timestamp_exhausted, #{
                            last_ts => LastTs, max_rel_ts => ?MAX_REL_TS
                        }}};
                false ->
                    {ok, First, Last}
            end
    end.

%% @private 纯函数：slot 展开为完整 ID（42/10/11 布局）
-spec slot_to_id(non_neg_integer(), 0..1023) -> pos_integer().
slot_to_id(Slot, CombinedNode) when is_integer(Slot), Slot >= 0 ->
    Ts = Slot bsr ?SEQUENCE_BITS,
    Seq = Slot band ?SEQUENCE_MASK,
    (Ts bsl ?TIMESTAMP_SHIFT) bor (CombinedNode bsl ?NODE_SHIFT) bor Seq.

%% @private 纯函数：ID 折叠回线性 slot（丢弃 node 段）。
-spec id_to_slot(pos_integer()) -> non_neg_integer().
id_to_slot(Id) when is_integer(Id), Id >= 1, Id =< ?MAX_ID ->
    ((Id bsr ?TIMESTAMP_SHIFT) bsl ?SEQUENCE_BITS) bor (Id band ?SEQUENCE_MASK);
id_to_slot(Id) ->
    error({elib_tsid_invalid_input, #{id => Id, valid_range => {1, ?MAX_ID}}}).

%% @private 墙钟：monotonic + 固定 Offset（init 采集，运行期不重算）。
%% 测试 seam 优先：put({elib_tsid, test_wall_ms}, Ms) 注入绝对毫秒。
-spec wall_clock_ms() -> integer().
wall_clock_ms() ->
    case get(?TEST_WALL_MS) of
        undefined ->
            #{clock_offset_ms := Offset} = persistent_term:get(?PT_RUNTIME),
            erlang:monotonic_time(millisecond) + Offset;
        Ms ->
            Ms
    end.

%% @private 单调钟 seam：仅用于等待 deadline/超时，绝不编码进 ID
-spec monotonic_ms() -> integer().
monotonic_ms() ->
    case get(?TEST_MONOTONIC_MS) of
        undefined -> erlang:monotonic_time(millisecond);
        Ms -> Ms
    end.

%% @doc 解析十进制字符串形式的 TSID（客户端以 decimal string 传输 64-bit ID）
%%
%% 仅接受值为 `1..?MAX_ID' 的纯十进制数字串；空串、符号（+/-）、空白、
%% 非十进制、0、超界一律返回 error。
-spec from_binary(binary()) -> {ok, pos_integer()} | error.
from_binary(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    case is_decimal_digits(Bin) of
        true ->
            case binary_to_integer(Bin) of
                Int when Int >= 1, Int =< ?MAX_ID -> {ok, Int};
                _ -> error
            end;
        false ->
            error
    end;
from_binary(_) ->
    error.

%% @private 纯 ASCII 十进制数字（无符号/空白/其他字符）
-spec is_decimal_digits(binary()) -> boolean().
is_decimal_digits(Bin) ->
    is_decimal_digits_1(Bin).

is_decimal_digits_1(<<C, Rest/binary>>) when C >= $0, C =< $9 ->
    is_decimal_digits_1(Rest);
is_decimal_digits_1(<<>>) ->
    true;
is_decimal_digits_1(_) ->
    false.

%% ===================================================================
%% 测试 seam：显式重置（仅测试使用）
%% ===================================================================

%% @private 清除全部 persistent_term 状态（runtime 与节点配置）。
%% 生产代码禁止调用：跨配置/跨节点场景的唯一合法重置入口是测试 seam。
-spec reset_for_test() -> ok.
reset_for_test() ->
    persistent_term:erase(?PT_RUNTIME),
    persistent_term:erase(?PT_NODE_ID),
    persistent_term:erase(?PT_DC_BITS),
    ok.
