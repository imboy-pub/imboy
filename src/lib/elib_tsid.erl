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
% 命名生成器:
%   每张表/业务有独立的 sequence 计数器，互不干扰。
%   同一节点同一毫秒内，不同命名生成器可能产生相同数值的 ID，
%   但因为它们属于不同的数据库表，主键不冲突。
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
-export([parse/1, timestamp/1, node_id/1]).
-export([to_base62/1, from_base62/1]).
-export([registered/0]).

%% ===================================================================
%% 位布局常量
%% ===================================================================

%% 自定义纪元: 2025-01-01 00:00:00 UTC (毫秒)
-define(EPOCH_MS, 1735689600000).

%% 位宽分配
-define(TIMESTAMP_BITS, 42).   %% 139.5 年
-define(NODE_BITS,      10).   %% 1024 节点
-define(SEQUENCE_BITS,  11).   %% 2048/ms/node

%% 移位量
-define(TIMESTAMP_SHIFT, (?NODE_BITS + ?SEQUENCE_BITS)).  %% 21
-define(NODE_SHIFT,      ?SEQUENCE_BITS).                 %% 11

%% 掩码
-define(SEQUENCE_MASK,  ((1 bsl ?SEQUENCE_BITS)  - 1)).   %% 2047
-define(NODE_MASK,      ((1 bsl ?NODE_BITS)      - 1)).   %% 1023
-define(TIMESTAMP_MASK, ((1 bsl ?TIMESTAMP_BITS) - 1)).

%% persistent_term 键
-define(PT_STATE(Name),   {elib_tsid_state, Name}).
-define(PT_NODE_ID,       elib_tsid_node_id).
-define(PT_DC_BITS,       elib_tsid_dc_bits).
-define(PT_NAMES,         elib_tsid_names).

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
-spec init(map()) -> ok.
init(Opts) ->
    DcBits = maps:get(dc_bits, Opts, 3),
    DcId   = maps:get(dc_id,   Opts),
    NodeId = maps:get(node_id, Opts),
    Names  = maps:get(names,   Opts, []),

    %% 校验位宽
    NodeBits = ?NODE_BITS - DcBits,
    true = (DcBits >= 0 andalso DcBits =< ?NODE_BITS),
    true = (NodeBits >= 0),

    %% 校验 ID 范围
    MaxDcId   = (1 bsl DcBits) - 1,
    MaxNodeId = (1 bsl NodeBits) - 1,
    true = (DcId >= 0 andalso DcId =< MaxDcId),
    true = (NodeId >= 0 andalso NodeId =< MaxNodeId),

    %% 合成 10-bit 节点标识
    CombinedNode = (DcId bsl NodeBits) bor NodeId,

    persistent_term:put(?PT_NODE_ID, CombinedNode),
    persistent_term:put(?PT_DC_BITS, DcBits),

    %% 初始化 default 生成器
    init_generator(default),

    %% 初始化命名生成器
    ExistingNames = persistent_term:get(?PT_NAMES, []),
    AllNames = lists:usort([default | Names] ++ ExistingNames),
    lists:foreach(fun init_generator/1, Names),
    persistent_term:put(?PT_NAMES, AllNames),
    ok.

%% @doc 注册命名生成器
%%
%% 可在 init/1 之后动态注册新的命名生成器。
%% 重复注册同一名称是安全的（幂等）。
%%
%% 示例:
%%   elib_tsid:register(user).
%%   elib_tsid:register([group, attachment, channel]).
-spec register(atom() | [atom()]) -> ok.
register(Names) when is_list(Names) ->
    lists:foreach(fun(N) -> register(N) end, Names),
    ok;
register(Name) when is_atom(Name) ->
    init_generator(Name),
    ExistingNames = persistent_term:get(?PT_NAMES, []),
    case lists:member(Name, ExistingNames) of
        true  -> ok;
        false -> persistent_term:put(?PT_NAMES, lists:sort([Name | ExistingNames]))
    end,
    ok.

%% @doc 列出所有已注册的生成器名称
-spec registered() -> [atom()].
registered() ->
    persistent_term:get(?PT_NAMES, [default]).

%% @private 初始化单个生成器的 atomics 状态
init_generator(Name) ->
    Key = ?PT_STATE(Name),
    case persistent_term:get(Key, undefined) of
        undefined ->
            StateRef = atomics:new(1, [{signed, true}]),
            persistent_term:put(Key, StateRef);
        _Exists ->
            %% 已存在则跳过（幂等）
            ok
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
%% 每个命名生成器拥有独立的 sequence 计数器，互不干扰。
%% 不同生成器在同一毫秒可能产生相同数值的 ID，
%% 但因为它们对应不同的数据库表，不会冲突。
%%
%% 示例:
%%   UserId  = elib_tsid:generate(user).
%%   GroupId = elib_tsid:generate(group).
-spec generate(atom()) -> pos_integer().
generate(Name) when is_atom(Name) ->
    NowRel = erlang:system_time(millisecond) - ?EPOCH_MS,
    try
        StateRef = persistent_term:get(?PT_STATE(Name)),
        NodeId   = persistent_term:get(?PT_NODE_ID),
        cas_loop(StateRef, NodeId, NowRel)
    catch
        error:badarg when Name =:= default ->
            error({elib_tsid_not_initialized,
                   'call elib_tsid:init/1 first'});
        error:badarg ->
            error({elib_tsid_generator_not_registered,
                   {Name, 'call elib_tsid:register/1 first'}})
    end.

%% @doc 使用 default 生成器批量生成 N 个 TSID (有序)
-spec generate_n(pos_integer()) -> [pos_integer()].
generate_n(N) when N > 0 ->
    generate_n(default, N).

%% @doc 使用指定生成器批量生成 N 个 TSID (有序)
-spec generate_n(atom(), pos_integer()) -> [pos_integer()].
generate_n(Name, N) when is_atom(Name), N > 0 ->
    [generate(Name) || _ <- lists:seq(1, N)].

%% @private CAS 循环 — 核心算法
cas_loop(StateRef, NodeId, NowRel) ->
    OldState = atomics:get(StateRef, 1),
    OldTs    = OldState bsr ?SEQUENCE_BITS,
    OldSeq   = OldState band ?SEQUENCE_MASK,

    %% 有效时间戳: 取 max(当前时间, 上次时间) — 绝不倒退
    %% 这是时钟回拨保护的核心：NTP 校时导致系统时间倒退时，
    %% 沿用上次时间戳继续递增序列号，保证唯一性
    EffTs = max(NowRel, OldTs),

    %% 计算新的 (时间戳, 序列号)
    {NewTs, NewSeq} =
        case EffTs of
            OldTs when OldSeq >= ?SEQUENCE_MASK ->
                %% 同毫秒 + 序列溢出 → 借用下一毫秒
                {OldTs + 1, 0};
            OldTs ->
                %% 同毫秒 + 序列未满 → 递增
                {OldTs, OldSeq + 1};
            _ ->
                %% 新毫秒 → 序列归零
                {EffTs, 0}
        end,

    NewState = (NewTs bsl ?SEQUENCE_BITS) bor NewSeq,

    %% 原子 CAS: 只有状态未被其他进程改变时才成功
    case atomics:compare_exchange(StateRef, 1, OldState, NewState) of
        ok ->
            %% 成功 → 组装 64-bit ID
            (NewTs bsl ?TIMESTAMP_SHIFT)
            bor (NodeId bsl ?NODE_SHIFT)
            bor NewSeq;
        _ ->
            %% 另一个进程抢先更新 → 重试 (无锁自旋)
            cas_loop(StateRef, NodeId, NowRel)
    end.

%% ===================================================================
%% 解析 / 提取
%% ===================================================================

%% @doc 解析 TSID 为各组成部分
-spec parse(pos_integer()) -> map().
parse(Id) when is_integer(Id), Id > 0 ->
    RelTs  = (Id bsr ?TIMESTAMP_SHIFT) band ?TIMESTAMP_MASK,
    Node   = (Id bsr ?NODE_SHIFT) band ?NODE_MASK,
    Seq    = Id band ?SEQUENCE_MASK,

    AbsMs  = RelTs + ?EPOCH_MS,
    DcBits = persistent_term:get(?PT_DC_BITS, 3),
    NodeBits = ?NODE_BITS - DcBits,

    #{
        id         => Id,
        timestamp  => AbsMs,
        dc_id      => Node bsr NodeBits,
        node_id    => Node band ((1 bsl NodeBits) - 1),
        sequence   => Seq,
        created_at => calendar:system_time_to_universal_time(AbsMs * 1000, microsecond)
    }.

%% @doc 从 TSID 提取 Unix 毫秒时间戳
-spec timestamp(pos_integer()) -> pos_integer().
timestamp(Id) ->
    ((Id bsr ?TIMESTAMP_SHIFT) band ?TIMESTAMP_MASK) + ?EPOCH_MS.

%% @doc 从 TSID 提取节点标识 (含 DC)
-spec node_id(pos_integer()) -> non_neg_integer().
node_id(Id) ->
    (Id bsr ?NODE_SHIFT) band ?NODE_MASK.

%% ===================================================================
%% Base62 编码 (可选, 用于 URL/日志场景)
%% ===================================================================

-define(BASE62_CHARS, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").

%% @doc 将 TSID 编码为 Base62 字符串 (最长 11 字符)
-spec to_base62(pos_integer()) -> binary().
to_base62(0) -> <<"0">>;
to_base62(Id) when is_integer(Id), Id > 0 ->
    list_to_binary(to_base62_chars(Id, [])).

to_base62_chars(0, Acc) -> Acc;
to_base62_chars(N, Acc) ->
    Rem = N rem 62,
    Char = lists:nth(Rem + 1, ?BASE62_CHARS),
    to_base62_chars(N div 62, [Char | Acc]).

%% @doc 从 Base62 字符串解码为 TSID
-spec from_base62(binary()) -> pos_integer().
from_base62(Bin) when is_binary(Bin) ->
    from_base62_chars(binary_to_list(Bin), 0).

from_base62_chars([], Acc) -> Acc;
from_base62_chars([C | Rest], Acc) ->
    Idx = base62_index(C),
    from_base62_chars(Rest, Acc * 62 + Idx).

base62_index(C) when C >= $0, C =< $9 -> C - $0;
base62_index(C) when C >= $A, C =< $Z -> C - $A + 10;
base62_index(C) when C >= $a, C =< $z -> C - $a + 36.
