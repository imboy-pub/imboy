-module(elib_tsid).

%%%
% TSID - 时间有序分布式唯一 ID 生成器
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
%   - 每节点每毫秒: 2^11 = 2048 个 ID
%   - 每节点每秒: 2,048,000 个 ID
%   - 数字位数: 永远 ≤ 19 位 (BIGINT 最大值 9.2 × 10^18)
%
% 唯一性保证:
%   - 不同 NodeId → 跨节点唯一
%   - 同节点同毫秒 → Sequence 单调递增保证唯一
%   - 时钟回拨 → 沿用上次时间戳 + 递增序列，绝不产生重复
%   - 序列溢出 → 借用下一毫秒时间戳，绝不阻塞
%
% 命名生成器（TSID-03 起全局 cursor）:
%   所有 label 共享同一全局 sequence cursor。
%   同一节点上任意两个 label（含 default）产生的 ID 数值永不相同，
%   跨表/跨域可直接按数值关联；label 仅作治理与兼容标签。
%
% 使用:
%   elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}).
%   elib_tsid:register(user).
%   elib_tsid:register([group, attachment, user_tag]).
%   Id = elib_tsid:generate(user).
%   Id = elib_tsid:generate().          %% 使用 default 生成器
%   Info = elib_tsid:parse(Id).
%%%

-export([init/1, register/1]).
-export([generate/0, generate/1, generate_n/1, generate_n/2]).
-export([parse/1, timestamp/1, node_id/1, from_binary/1]).
-export([to_base62/1, from_base62/1]).
-export([registered/0]).

%% 内部测试/实现 seam（TSID-01 冻结）：供 EUnit 固定时钟模型测试与新
%% reservation 实现共用。不属于公开 API 契约——公开 arity 与健康路径
%% 返回形状见上方 -export；本组函数语义变更必须同步冻结的测试。
-export([reserve_candidate/3, slot_to_id/2, id_to_slot/1]).
-export([wall_clock_ms/0, monotonic_ms/0]).
%% TSID-02：跨配置测试显式 reset（F-12：禁止静默重配节点偷跑）
-export([reset_for_test/0]).
%% TSID-04：reservation 计数（观测 instrumentation，非公开 API 契约）
-export([reservation_count/0]).
%% TSID-06：guard 集成 seam（guarded runtime 发布与 fence 检查共用）
-export([runtime_handle/0, guarded_publish/1, combine_node/3]).

%% ===================================================================
%% 位布局常量
%% ===================================================================

%% 自定义纪元: 2025-01-01 00:00:00 UTC (毫秒)
-define(EPOCH_MS, 1735689600000).

%% 位宽分配

%% 139.5 年
-define(TIMESTAMP_BITS, 42).
%% 1024 节点
-define(NODE_BITS, 10).
%% 2048/ms/node
-define(SEQUENCE_BITS, 11).

%% 移位量

%% 21
-define(TIMESTAMP_SHIFT, (?NODE_BITS + ?SEQUENCE_BITS)).
%% 11
-define(NODE_SHIFT, ?SEQUENCE_BITS).

%% 掩码

%% 2047
-define(SEQUENCE_MASK, ((1 bsl ?SEQUENCE_BITS) - 1)).
%% 1023
-define(NODE_MASK, ((1 bsl ?NODE_BITS) - 1)).
-define(TIMESTAMP_MASK, ((1 bsl ?TIMESTAMP_BITS) - 1)).

%% 42-bit 相对时间戳上界（最后合法毫秒 2164-05-15T07:35:11.103Z）
-define(MAX_REL_TS, ?TIMESTAMP_MASK).
%% signed 63-bit 上界 = 最后合法 (ts,node,seq) 组合
-define(MAX_ID, ((1 bsl 63) - 1)).

%% persistent_term 键
%% TSID-03 起唯一权威：完整 runtime handle 一次发布（全局 cursor + 节点
%% 配置 + label 集合快照 + 注册互斥锁）。旧每 name 独立 state 键废弃。
-define(PT_RUNTIME, elib_tsid_runtime).
-define(PT_NODE_ID, elib_tsid_node_id).
-define(PT_DC_BITS, elib_tsid_dc_bits).

%% 进程级时钟 seam 键（仅测试进程显式 put；生产路径无全局可变状态）
-define(TEST_WALL_MS, {elib_tsid, test_wall_ms}).
-define(TEST_MONOTONIC_MS, {elib_tsid, test_monotonic_ms}).

%% ===================================================================
%% 初始化
%% ===================================================================

%% @doc 初始化 TSID 生成器
%%
%% Opts:
%%   dc_id    - 数据中心 ID (必填)
%%   node_id  - 节点 ID (必填)
%%   dc_bits  - DC 占用的位数 (默认 3, 即 8 个 DC × 128 个节点)
%%   names    - 命名生成器列表 (可选, 如 [user, group, attachment])
%%
%% DC/Node 位分配示例:
%%   dc_bits=0 → 1 DC × 1024 nodes
%%   dc_bits=2 → 4 DC × 256 nodes
%%   dc_bits=3 → 8 DC × 128 nodes (默认)
%%   dc_bits=4 → 16 DC × 64 nodes
%%   dc_bits=5 → 32 DC × 32 nodes
%%
%% 幂等契约（TSID-02 冻结，F-12）：相同配置重复 init 幂等返回 ok 且
%% cursor 不回退；不同配置 re-init 抛
%% `{elib_tsid_already_initialized, #{existing => ..., requested => ...}}'，
%% 旧 runtime 保持不变。测试如需换配置必须先 reset_for_test/0。
-spec init(map()) -> ok.
init(Opts) ->
    DcBits = maps:get(dc_bits, Opts, 3),
    DcId = maps:get(dc_id, Opts),
    NodeId = maps:get(node_id, Opts),
    Names = maps:get(names, Opts, []),

    %% 校验位宽
    NodeBits = ?NODE_BITS - DcBits,
    true = (DcBits >= 0 andalso DcBits =< ?NODE_BITS),
    true = (NodeBits >= 0),

    %% 校验 ID 范围
    MaxDcId = (1 bsl DcBits) - 1,
    MaxNodeId = (1 bsl NodeBits) - 1,
    true = (DcId >= 0 andalso DcId =< MaxDcId),
    true = (NodeId >= 0 andalso NodeId =< MaxNodeId),

    %% 合成 10-bit 节点标识
    CombinedNode = (DcId bsl NodeBits) bor NodeId,

    %% 配置冻结（F-12）：已初始化时仅接受幂等同配置，拒绝静默重配节点
    ExistingNode = persistent_term:get(?PT_NODE_ID, undefined),
    ExistingDcBits = persistent_term:get(?PT_DC_BITS, undefined),
    case
        ExistingNode =:= undefined orelse
            (ExistingNode =:= CombinedNode andalso ExistingDcBits =:= DcBits)
    of
        true ->
            ok;
        false ->
            error(
                {elib_tsid_already_initialized, #{
                    existing => #{combined_node => ExistingNode, dc_bits => ExistingDcBits},
                    requested => #{combined_node => CombinedNode, dc_bits => DcBits}
                }}
            )
    end,

    persistent_term:put(?PT_NODE_ID, CombinedNode),
    persistent_term:put(?PT_DC_BITS, DcBits),

    %% TSID-04：有界逻辑时间参数。缺省 lead=512 由 TSID-10 定标：实测
    %% 峰值 9.9M/s（基线 CPU 速率）下 1M-id 突发需借支 ~388ms 逻辑时间，
    %% 512ms 留 32% 余量；且 lead 上限先于 durable fence（window=1000ms）
    %% 绑定，热路径零磁盘 I/O；崩溃烧槽由 fence window 承担，与 lead
    %% 无关（guard 恢复从持久化 safe_before 续起）。
    %% §5.6 不变量：max_batch_chunk <= (lead + 1) * 2048。
    Lead = maps:get(max_logical_lead_ms, Opts, 512),
    CapWait = maps:get(capacity_wait_timeout_ms, Opts, 100),
    MaxChunk = maps:get(max_batch_chunk, Opts, (Lead + 1) * 2048),
    case
        is_integer(Lead) andalso Lead >= 0 andalso
            is_integer(CapWait) andalso CapWait > 0 andalso
            is_integer(MaxChunk) andalso MaxChunk > 0 andalso
            MaxChunk =< (Lead + 1) * 2048
    of
        true ->
            ok;
        false ->
            error(
                {elib_tsid_invalid_config, #{
                    max_logical_lead_ms => Lead,
                    capacity_wait_timeout_ms => CapWait,
                    max_batch_chunk => MaxChunk,
                    constraint => {max_batch_chunk_max, (Lead + 1) * 2048}
                }}
            )
    end,

    %% TSID-03：完整 runtime 一次发布。全部 label 共享同一全局 cursor
    %% （跨 name 数值零交集）；幂等 re-init 保留既有 cursor 不回退。
    %% TSID-04：limits 亦纳入配置冻结（静默改限 = 配置漂移）。
    Existing = persistent_term:get(?PT_RUNTIME, undefined),
    {Cursor, RegLock, Stats, ExistingNames, ExistingLimits} =
        case Existing of
            undefined ->
                {
                    atomics:new(1, [{signed, true}]),
                    atomics:new(1, [{signed, true}]),
                    atomics:new(1, [{signed, true}]),
                    [],
                    undefined
                };
            #{cursor := C, reg_lock := L, stats := S, names := Ns} = H ->
                {C, L, S, sets:to_list(Ns), #{
                    max_logical_lead_ms => maps:get(max_logical_lead_ms, H),
                    capacity_wait_timeout_ms => maps:get(capacity_wait_timeout_ms, H),
                    max_batch_chunk => maps:get(max_batch_chunk, H)
                }}
        end,
    RequestedLimits = #{
        max_logical_lead_ms => Lead,
        capacity_wait_timeout_ms => CapWait,
        max_batch_chunk => MaxChunk
    },
    case ExistingLimits =:= undefined orelse ExistingLimits =:= RequestedLimits of
        true ->
            ok;
        false ->
            error(
                {elib_tsid_already_initialized, #{
                    existing => ExistingLimits,
                    requested => RequestedLimits
                }}
            )
    end,
    AllNames = lists:usort([default | Names] ++ ExistingNames),
    publish_runtime(
        Cursor, RegLock, Stats, CombinedNode, DcBits, AllNames, Lead, CapWait, MaxChunk
    ),
    ok.

%% @private 原子发布完整 runtime handle（单一 PT 键，读者见旧或新，无撕裂）
-spec publish_runtime(
    atomics:atomics_ref(),
    atomics:atomics_ref(),
    atomics:atomics_ref(),
    0..1023,
    0..10,
    [atom()],
    non_neg_integer(),
    pos_integer(),
    pos_integer()
) -> ok.
publish_runtime(Cursor, RegLock, Stats, CombinedNode, DcBits, AllNames, Lead, CapWait, MaxChunk) ->
    Handle = #{
        cursor => Cursor,
        reg_lock => RegLock,
        stats => Stats,
        combined_node => CombinedNode,
        dc_bits => DcBits,
        names => sets:from_list(AllNames, [{version, 2}]),
        max_logical_lead_ms => Lead,
        capacity_wait_timeout_ms => CapWait,
        max_batch_chunk => MaxChunk
    },
    persistent_term:put(?PT_RUNTIME, Handle),
    ok.

%% @private TSID-06 guard 集成：发布 guarded runtime。
%% cursor 初始化为 (FloorTs bsl 11) - 1——首个可分配 slot 在 FloorTs 毫秒
%% seq=0，绝不低于 durable floor；guard_ref atomics[status, safe_before]
%% 只在 guard 成功持久化后由 guard 写入（publish-before-durable 机械不存在）。
%% 同 VM guard 重启：新 floor 恒 >= 既有 cursor（fence 不变式），只前跳
%% 不回退（跳号合法，重复不合法）。
-spec guarded_publish(map()) -> ok | {error, term()}.
guarded_publish(#{
    combined_node := CombinedNode,
    dc_bits := DcBits,
    names := Names,
    max_logical_lead_ms := Lead,
    capacity_wait_timeout_ms := CapWait,
    max_batch_chunk := MaxChunk,
    cursor_floor_ts := FloorTs,
    guard_ref := GRef,
    guard_pid := GPid
}) ->
    case
        is_integer(CombinedNode) andalso CombinedNode >= 0 andalso CombinedNode =< 1023 andalso
            is_integer(FloorTs) andalso FloorTs >= 0 andalso FloorTs =< ?MAX_REL_TS andalso
            is_integer(Lead) andalso Lead >= 0 andalso
            is_integer(CapWait) andalso CapWait > 0 andalso
            is_integer(MaxChunk) andalso MaxChunk > 0 andalso MaxChunk =< (Lead + 1) * 2048
    of
        true ->
            %% 配置冻结：同 VM 已有 runtime 时仅接受幂等同配置 guard 重启
            Existing = persistent_term:get(?PT_RUNTIME, undefined),
            case Existing of
                undefined ->
                    ok;
                #{
                    combined_node := CombinedNode,
                    dc_bits := DcBits,
                    max_logical_lead_ms := Lead,
                    capacity_wait_timeout_ms := CapWait,
                    max_batch_chunk := MaxChunk
                } ->
                    ok;
                _ ->
                    error(
                        {elib_tsid_already_initialized, #{
                            existing => maps:with(
                                [combined_node, dc_bits, max_logical_lead_ms], Existing
                            )
                        }}
                    )
            end,
            Cursor = atomics:new(1, [{signed, true}]),
            ok = atomics:put(Cursor, 1, (FloorTs bsl ?SEQUENCE_BITS) - 1),
            persistent_term:put(?PT_NODE_ID, CombinedNode),
            persistent_term:put(?PT_DC_BITS, DcBits),
            %% 合并既有 runtime 的动态注册 label（guard crash 被 sup
            %% 重启后不丢 register/1 加的 label——review F6）
            ExistingNames =
                case Existing of
                    #{names := EN} -> sets:to_list(EN);
                    _ -> []
                end,
            Handle = #{
                cursor => Cursor,
                reg_lock => atomics:new(1, [{signed, true}]),
                stats => atomics:new(1, [{signed, true}]),
                combined_node => CombinedNode,
                dc_bits => DcBits,
                names =>
                    sets:from_list(
                        [default | Names] ++ ExistingNames,
                        [{version, 2}]
                    ),
                max_logical_lead_ms => Lead,
                capacity_wait_timeout_ms => CapWait,
                max_batch_chunk => MaxChunk,
                guard_ref => GRef,
                guard_pid => GPid
            },
            persistent_term:put(?PT_RUNTIME, Handle),
            ok;
        false ->
            {error, {elib_tsid_invalid_config, #{guard_publish => bad_args}}}
    end.

%% @doc 注册命名生成器
%%
%% 可在 init/1 之后动态注册新的命名生成器。
%% 重复注册同一名称是安全的（幂等）。并发注册不同名称经注册互斥锁
%% 串行化，无丢更新（TSID-03，F-13）；注册是冷路径，生成热路径不经锁。
%%
%% 示例:
%%   elib_tsid:register(user).
%%   elib_tsid:register([group, attachment, channel]).
-spec register(atom() | [atom()]) -> ok.
register(Names) when is_list(Names) ->
    lists:foreach(fun(N) -> register(N) end, Names),
    ok;
register(Name) when is_atom(Name) ->
    case runtime_handle() of
        {ok, #{reg_lock := RegLock}} ->
            with_reg_lock(RegLock, fun() ->
                %% 锁内重读：合并到最新 handle 再发布
                #{names := Names0} = H0 = persistent_term:get(?PT_RUNTIME),
                case sets:is_element(Name, Names0) of
                    true ->
                        ok;
                    false ->
                        H1 = H0#{names := sets:add_element(Name, Names0)},
                        persistent_term:put(?PT_RUNTIME, H1)
                end
            end),
            ok;
        error ->
            error({elib_tsid_not_initialized, 'call elib_tsid:init/1 before register/1'})
    end.

%% @doc 列出所有已注册的生成器名称
-spec registered() -> [atom()].
registered() ->
    case runtime_handle() of
        {ok, #{names := Names}} ->
            lists:sort(sets:to_list(Names));
        error ->
            [default]
    end.

%% @private 组合 10-bit CombinedNode（guard 配置装配用）。
%% 越界输入必须 error 而非静默 bor：NodeId 溢出 node 位段会覆盖 dc 位
%% （如 (1,200,3) bor 出 200——dc 信息丢失且与其它合法组合碰撞可能），
%% 这是部署配置链的最后一道布局校验（TSID-07 / AC-07C）。
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

%% @private 读取当前 runtime handle
-spec runtime_handle() -> {ok, map()} | error.
runtime_handle() ->
    case persistent_term:get(?PT_RUNTIME, undefined) of
        #{} = H -> {ok, H};
        undefined -> error
    end.

%% @private 注册互斥锁：CAS 自旋（仅冷路径；持锁区间内无热路径操作）
-spec with_reg_lock(atomics:atomics_ref(), fun()) -> term().
with_reg_lock(RegLock, Fun) ->
    case atomics:compare_exchange(RegLock, 1, 0, 1) of
        ok ->
            try
                Fun()
            after
                ok = atomics:put(RegLock, 1, 0)
            end;
        _ ->
            with_reg_lock(RegLock, Fun)
    end.

%% ===================================================================
%% ID 生成
%% ===================================================================

%% @doc 使用 default 生成器生成一个 TSID
-spec generate() -> pos_integer().
generate() ->
    generate(default).

%% @doc 使用指定的命名生成器生成一个 TSID
%%
%% TSID-03 起所有 label 共享同一全局 cursor：同一节点上任意两个
%% 生成器（含 default）产生的 ID 数值永不相同，跨表/跨域引用可
%% 直接按数值关联。label 仅作治理与兼容标签，不再分配独立数值空间。
%%
%% 示例:
%%   UserId  = elib_tsid:generate(user).
%%   GroupId = elib_tsid:generate(group).
-spec generate(atom()) -> pos_integer().
generate(Name) when is_atom(Name) ->
    Handle = require_runtime(Name),
    #{combined_node := NodeId} = Handle,
    Budget = {timeout, maps:get(capacity_wait_timeout_ms, Handle)},
    {First, _Last} = reserve(Handle, 1, Budget),
    %% count=1：First == Last，直接展开单值
    slot_to_id(First, NodeId).

%% @doc 使用 default 生成器批量生成 N 个 TSID (有序)
-spec generate_n(pos_integer()) -> [pos_integer()].
generate_n(N) when N > 0 ->
    generate_n(default, N).

%% @doc 使用指定生成器批量生成 N 个 TSID (有序)
%%
%% TSID-04：线性 slot 一次 CAS 预留连续区间，本地纯计算展开 ID。
%% 普通批量（<= max_batch_chunk）恰一次成功 CAS；超大 N 按最大安全
%% 连续区间分有界 chunk，每个 chunk 从最新 cursor 重新 reserve。单次
%% 调用返回严格升序；任一 chunk 失败即整批 typed 失败（已消耗 slot
%% 成为合法跳号，绝不重复）。等待一律用 monotonic deadline，不用
%% 墙钟测超时。
-spec generate_n(atom(), pos_integer()) -> [pos_integer()].
generate_n(Name, N) when is_atom(Name), N > 0 ->
    Handle = require_runtime(Name),
    #{combined_node := NodeId} = Handle,
    MaxChunk = maps:get(max_batch_chunk, Handle),
    Budget = {timeout, maps:get(capacity_wait_timeout_ms, Handle)},
    lists:append([
        materialize_range(First, Last, NodeId)
     || {First, Last} <- [reserve(Handle, C, Budget) || C <- chunk_sizes(N, MaxChunk)]
    ]).

%% @private label 校验 + runtime 读取（一次通过，chunk 循环前完成）
-spec require_runtime(atom()) -> map().
require_runtime(Name) ->
    case runtime_handle() of
        {ok, #{names := Names} = Handle} ->
            case sets:is_element(Name, Names) of
                true ->
                    Handle;
                false ->
                    error(
                        {elib_tsid_generator_not_registered,
                            {Name, 'call elib_tsid:register/1 first'}}
                    )
            end;
        error when Name =:= default ->
            error({elib_tsid_not_initialized, 'call elib_tsid:init/1 first'});
        error ->
            error(
                {elib_tsid_generator_not_registered, {Name, 'call elib_tsid:register/1 first'}}
            )
    end.

%% @private 超大 N 分有界 chunk（每个 chunk <= max_batch_chunk）
-spec chunk_sizes(pos_integer(), pos_integer()) -> [pos_integer()].
chunk_sizes(N, MaxChunk) when N =< MaxChunk ->
    [N];
chunk_sizes(N, MaxChunk) ->
    [MaxChunk | chunk_sizes(N - MaxChunk, MaxChunk)].

%% @private 单次 reservation：读时钟 → 纯 candidate → lead 检查 → CAS。
%% CAS 冲突后从墙钟重读（F-05：绝不复用陈旧墙钟自旋）；冲突重试受
%% monotonic deadline 预算约束，无饥饿自旋。
%% Budget 为 {timeout, Ms}（未物化）或 {deadline, AbsMs}（已物化）：
%% 热路径零单调钟开销，首次需要等待/冲突重试时才惰性物化，物化后
%% 递归全程携带同一绝对 deadline（预算守恒，不因重试重置）。
-spec reserve(map(), pos_integer(), {timeout, pos_integer()} | {deadline, integer()}) ->
    {FirstSlot :: non_neg_integer(), LastSlot :: non_neg_integer()}.
reserve(#{cursor := Cursor} = Handle, Count, Budget) ->
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
    Old = atomics:get(Cursor, 1),
    case reserve_candidate(Old, Now, Count) of
        {ok, First, Last} ->
            %% TSID-10：lead 判定复用同一读数 Now——commit_reserve 里重新
            %% 取钟会把 VM 毫秒 tick 的读数滞后（同一毫秒读出两次相同值）
            %% 算成领先，触发无谓的 wait_step/sleep（基准实测 p99 +2.3ms）
            commit_reserve(Handle, Old, First, Last, Now, Budget, Count);
        {error, Reason} ->
            error(Reason)
    end.

%% @private lead 检查 + CAS 提交（单值与 chunk 共用）
commit_reserve(Handle, Old, First, Last, Now, Budget, Count) ->
    #{max_logical_lead_ms := MaxLead} = Handle,
    Lead = (Last bsr ?SEQUENCE_BITS) - Now,
    case Lead > MaxLead of
        true ->
            Deadline = deadline_from(Budget),
            case wait_step(Deadline, Lead - MaxLead) of
                ok ->
                    reserve(Handle, Count, {deadline, Deadline});
                timeout ->
                    error(
                        {elib_tsid_capacity_exhausted, #{
                            lead_ms => Lead,
                            max_logical_lead_ms => MaxLead,
                            phase => clock_wait
                        }}
                    )
            end;
        false ->
            %% durable fence 检查（TSID-06，仅 guarded runtime）：
            %% ts 必须低于已持久化 safe_before；逼近则同步请求 guard
            %% 续租（合并去重），绝不越过 durable horizon
            case fence_gate(Handle, Last, Budget) of
                ok ->
                    commit_cas(Handle, Old, First, Last, Budget, Count);
                retry ->
                    reserve(Handle, Count, Budget)
            end
    end.

commit_cas(Handle, Old, First, Last, Budget, Count) ->
    #{cursor := Cursor, stats := Stats} = Handle,
    case atomics:compare_exchange(Cursor, 1, Old, Last) of
        ok ->
            _ = atomics:add(Stats, 1, 1),
            {First, Last};
        _Other ->
            %% 冲突：deadline 预算内用刷新后的墙钟重试（F-05）；
            %% 预算在此物化一次并随重试守恒
            Deadline = deadline_from(Budget),
            case monotonic_ms() >= Deadline of
                true ->
                    error(
                        {elib_tsid_capacity_exhausted, #{
                            phase => cas_contention
                        }}
                    );
                false ->
                    reserve(Handle, Count, {deadline, Deadline})
            end
    end.

%% @private 等待预算物化：{timeout, Ms} 首次转绝对 deadline；{deadline, D} 直通
-spec deadline_from({timeout, pos_integer()} | {deadline, integer()}) -> integer().
deadline_from({deadline, D}) -> D;
deadline_from({timeout, T}) -> monotonic_ms() + T.

%% @private fence 门（standalone 测试 runtime 无 guard_ref 直通）
%% 返回 ok=可提交 | retry=已续租需重走 reserve；fenced/续租失败直接抛
-spec fence_gate(map(), non_neg_integer(), {timeout, pos_integer()} | {deadline, integer()}) ->
    ok | retry.
fence_gate(Handle, Last, Budget) ->
    case maps:find(guard_ref, Handle) of
        error ->
            ok;
        {ok, GRef} ->
            case atomics:get(GRef, 1) of
                1 ->
                    LastTs = Last bsr ?SEQUENCE_BITS,
                    case LastTs >= atomics:get(GRef, 2) of
                        true ->
                            renew_fence(Handle, LastTs, Budget);
                        false ->
                            ok
                    end;
                Status ->
                    error({elib_tsid_fenced, #{status => Status}})
            end
    end.

%% @private 请求 guard 续租并重验；预算耗尽即 typed 失败
-spec renew_fence(
    map(), non_neg_integer(), {timeout, pos_integer()} | {deadline, integer()}
) -> retry.
renew_fence(#{guard_pid := GPid} = _Handle, Horizon, Budget) ->
    Deadline = deadline_from(Budget),
    Remaining = Deadline - monotonic_ms(),
    case Remaining =< 0 of
        true ->
            error({elib_tsid_fenced, #{phase => renew_deadline}});
        false ->
            RenewRes =
                try
                    gen_server:call(GPid, {renew_fence, Horizon}, Remaining)
                catch
                    %% guard 已死（noproc）/call 超时——fail-closed 且保持
                    %% typed 错误族（review F5：裸 exit 违反错误族合同）
                    _C:_R -> {exit, unreachable}
                end,
            case RenewRes of
                {ok, _NewSafeBefore} ->
                    %% 续租成功：重走完整 reserve（含新 fence 检查）
                    retry;
                {error, Reason} ->
                    error(Reason);
                {exit, _} ->
                    error({elib_tsid_fenced, #{phase => renew_unreachable}})
            end
    end.

%% @private 无 busy-spin 等待：睡眠由 deadline 剩余量约束；单调钟停滞
%% （异常/seam 冻结）立即 fail-closed，绝不无限等待。
%% 停滞检测：真实单调钟用微秒粒度（毫秒截断会把 <1ms 睡眠误判为
%% 停滞）；seam 注入时 seam 值未变即停滞。
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

%% @private reservation 成功计数（观测 instrumentation；测试与监控共用）
-spec reservation_count() -> non_neg_integer().
reservation_count() ->
    case runtime_handle() of
        {ok, #{stats := Stats}} -> atomics:get(Stats, 1);
        error -> 0
    end.

%% ===================================================================
%% 解析 / 提取
%% ===================================================================

%% @doc 解析 TSID 为各组成部分
%%
%% 仅接受 `1..?MAX_ID'（正 signed 63-bit）；非 integer、`=<0'、
%% `>2^63-1' 抛 `{elib_tsid_invalid_input, _}'（F-09：不再掩码截断）。
%%
%% 精度声明（F-11）：`timestamp' 保留毫秒精度；
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
%%
%% 仅接受 `1..?MAX_ID'；越界输入抛 `{elib_tsid_invalid_input, _}'。
-spec timestamp(pos_integer()) -> pos_integer().
timestamp(Id) ->
    validate_id(Id),
    ((Id bsr ?TIMESTAMP_SHIFT) band ?TIMESTAMP_MASK) + ?EPOCH_MS.

%% @doc 从 TSID 提取节点标识 (含 DC)
%%
%% 仅接受 `1..?MAX_ID'；越界输入抛 `{elib_tsid_invalid_input, _}'。
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
%% 内部 seam：纯 reservation 模型与时钟注入（TSID-01 冻结）
%%
%% 契约要点（变更须同步 elib_tsid_tests 的 TSID-01 测试组）：
%%   - cursor 是线性 slot：Slot = Ts bsl 11 bor Seq，全程单调；
%%   - candidate 区间 [First, Last] 满足 First = max(Old+1, Now bsl 11)；
%%   - 时钟回拨由 cursor 吸收（First 沿用旧毫秒），绝不回退；
%%   - 纪元前 / 越过 42-bit 上界一律 typed fail-closed，绝不掩码截断；
%%   - wall_clock_ms / monotonic_ms 为进程级测试 seam：仅当调用进程
%%     显式 put 测试键时偏离真实时钟，生产路径无全局可变状态。
%% ===================================================================

%% @private 纯函数：给定 cursor、当前相对毫秒与数量，计算连续 slot 区间
-spec reserve_candidate(integer(), integer(), pos_integer()) ->
    {ok, FirstSlot :: non_neg_integer(), LastSlot :: non_neg_integer()}
    | {error, {elib_tsid_clock_before_epoch | elib_tsid_timestamp_exhausted, map()}}.
reserve_candidate(OldCursor, NowRel, Count) when
    is_integer(OldCursor), is_integer(NowRel), is_integer(Count), Count > 0
->
    case NowRel < 0 of
        true ->
            {error, {elib_tsid_clock_before_epoch, #{now_rel => NowRel}}};
        false ->
            First = max(OldCursor + 1, NowRel bsl ?SEQUENCE_BITS),
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
%% 输入合同与 parse/1 同口径（F-09）：仅 canonical `1..MAX_ID'，越界
%% typed 拒绝——TSID-09 fuzz 实证 0/负值曾走 function_clause 而非
%% typed error，oracle 口径不一致。
-spec id_to_slot(pos_integer()) -> non_neg_integer().
id_to_slot(Id) when is_integer(Id), Id >= 1, Id =< ?MAX_ID ->
    ((Id bsr ?TIMESTAMP_SHIFT) bsl ?SEQUENCE_BITS) bor (Id band ?SEQUENCE_MASK);
id_to_slot(Id) ->
    error({elib_tsid_invalid_input, #{id => Id, valid_range => {1, ?MAX_ID}}}).

%% @private 进程级墙钟 seam：测试用 put({elib_tsid, test_wall_ms}, Ms) 注入
-spec wall_clock_ms() -> integer().
wall_clock_ms() ->
    case get(?TEST_WALL_MS) of
        undefined -> erlang:system_time(millisecond);
        Ms -> Ms
    end.

%% @private 进程级单调钟 seam：仅用于等待 deadline/超时，绝不编码进 ID
-spec monotonic_ms() -> integer().
monotonic_ms() ->
    case get(?TEST_MONOTONIC_MS) of
        undefined -> erlang:monotonic_time(millisecond);
        Ms -> Ms
    end.

%% @doc 解析十进制字符串形式的 TSID（客户端以 decimal string 传输 64-bit ID）
%%
%% 仅接受值为 `1..?MAX_ID' 的纯十进制数字串；空串、符号（+/-）、空白、
%% 非十进制、0、超界一律返回 error（TSID-02 冻结，F-09）。
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
%% Base62 编码 (可选, 用于 URL/日志场景)
%% ===================================================================

-define(BASE62_CHARS, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").
%% 62^11 > MAX_ID >= 62^10 → 合法 TSID 的 Base62 最长 11 字符
-define(MAX_BASE62_LEN, 11).

%% @doc 将 TSID 编码为 Base62 字符串 (最长 11 字符)
%%
%% 仅接受 `1..?MAX_ID'；非 TSID 输入抛 `{elib_tsid_invalid_input, _}'。
-spec to_base62(pos_integer()) -> binary().
to_base62(Id) when is_integer(Id), Id >= 1, Id =< ?MAX_ID ->
    list_to_binary(to_base62_chars(Id, []));
to_base62(Id) ->
    error({elib_tsid_invalid_input, #{id => Id, valid_range => {1, ?MAX_ID}}}).

to_base62_chars(0, Acc) ->
    Acc;
to_base62_chars(N, Acc) ->
    Rem = N rem 62,
    Char = lists:nth(Rem + 1, ?BASE62_CHARS),
    to_base62_chars(N div 62, [Char | Acc]).

%% @doc 从 Base62 字符串解码为 TSID
%%
%% 仅接受可解码到 `1..?MAX_ID' 的合法 Base62 串；空串、非 Base62 字符、
%% 超长（>11 字符必然溢出）、解码值越界一律抛
%% `{elib_tsid_invalid_input, _}'（TSID-02 冻结，F-10）。
-spec from_base62(binary()) -> pos_integer().
from_base62(Bin) when is_binary(Bin), byte_size(Bin) > 0, byte_size(Bin) =< ?MAX_BASE62_LEN ->
    case base62_to_int(Bin, 0) of
        {ok, Id} when Id >= 1, Id =< ?MAX_ID ->
            Id;
        {ok, Id} ->
            error(
                {elib_tsid_invalid_input, #{
                    decoded => Id, valid_range => {1, ?MAX_ID}
                }}
            );
        error ->
            error({elib_tsid_invalid_input, #{input => not_base62}})
    end;
from_base62(Bin) ->
    error({elib_tsid_invalid_input, #{input => Bin, reason => empty_or_too_long}}).

base62_to_int(<<C, Rest/binary>>, Acc) ->
    case base62_index(C) of
        {ok, Idx} -> base62_to_int(Rest, Acc * 62 + Idx);
        error -> error
    end;
base62_to_int(<<>>, Acc) ->
    {ok, Acc}.

base62_index(C) when C >= $0, C =< $9 -> {ok, C - $0};
base62_index(C) when C >= $A, C =< $Z -> {ok, C - $A + 10};
base62_index(C) when C >= $a, C =< $z -> {ok, C - $a + 36};
base62_index(_) -> error.

%% ===================================================================
%% 测试 seam：显式重置（仅测试使用）
%% ===================================================================

%% @private 清除全部 persistent_term 状态（全局 cursor 与注册表）。
%% 生产代码禁止调用：跨配置/跨节点场景的唯一合法重置入口是测试 seam，
%% 用于满足 F-12「测试必须使用显式 reset」的冻结要求。
-spec reset_for_test() -> ok.
reset_for_test() ->
    persistent_term:erase(?PT_RUNTIME),
    persistent_term:erase(?PT_NODE_ID),
    persistent_term:erase(?PT_DC_BITS),
    ok.
