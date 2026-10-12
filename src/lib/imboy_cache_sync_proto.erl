%% @doc 缓存同步协议层（V2.2.1 收口 C02）
%%
%% 职责（从 imboy_cache_sync 拆出，C03 模块化）：
%% - V2 信封构造（epoch/config_hash/registry_hash，C01 冻结协议）；
%% - 事件结构校验（原 validate_v2 全量 + 新增字段）；
%% - 双端一致性：epoch 推移、config/registry hash 匹配、peer incarnation
%%   注册与旧化身拒绝（C01 第 1/2 节）；
%% - 混合能力交集：不一致 peer 标记 hash_divergent，不作为 authority。
%%
%% 本模块为纯函数 + 共享 META ETS 表操作，不持有进程状态；
%% 写路径仍由 imboy_cache_sync gen_server 串行调用（计划 5.2 方案 A）。
-module(imboy_cache_sync_proto).
-moduledoc "缓存同步 V2 协议：信封校验、epoch/incarnation/hash 双端一致性。".

-include("cache.hrl").

-define(META_TABLE, imboy_cache_meta).

-export([
    %% 信封
    event/7,
    validate/1,
    %% 双端一致性（对照本节点 META 状态）
    check_consistency/2,
    %% epoch
    epoch/0,
    bump_epoch/0,
    peer_epoch/1,
    %% incarnation
    register_incarnation/2,
    incarnation_changed/2,
    prune_grace/1,
    %% hash
    config_hash/0,
    registry_hash/0,
    hash_divergent/1,
    %% 快照握手帧构造（新协议帧）
    version_request_frame/1
]).

%% ===================================================================
%% 信封构造与校验
%% ===================================================================

%% @doc V2 事件信封（C01 第 1 节：新增 epoch/config_hash/registry_hash）。
-spec event(
    set | delete | invalidate,
    term(),
    term(),
    non_neg_integer(),
    {non_neg_integer(), node()},
    non_neg_integer(),
    binary()
) -> map().
event(Op, Key, Value, ExpiresAt, Rev, Seq, BootId) ->
    #{
        protocol => 2,
        operation => Op,
        namespace => imboy_cache_policy:namespace_of(Key),
        key => Key,
        value => Value,
        expires_at_ms => ExpiresAt,
        revision => Rev,
        origin_seq => Seq,
        origin_node => node(),
        origin_boot_id => BootId,
        epoch => epoch(),
        config_hash => config_hash(),
        registry_hash => registry_hash()
    }.

%% @doc 事件结构校验（W3 既有规则 + C01 新增字段，接收端独立执行）。
-spec validate(map()) -> ok | {error, term()}.
validate(Event) when is_map(Event) ->
    Op = maps:get(operation, Event, undefined),
    Key = maps:get(key, Event, undefined),
    checks([
        {fun() -> Key =/= undefined end, missing_key},
        {fun() -> lists:member(Op, [set, delete, invalidate]) end, unknown_operation},
        {
            fun() ->
                is_tuple(maps:get(revision, Event, none)) andalso
                    tuple_size(maps:get(revision, Event)) =:= 2
            end,
            invalid_revision
        },
        {fun() -> is_integer(maps:get(origin_seq, Event, none)) end, invalid_origin_seq},
        {fun() -> is_atom(maps:get(origin_node, Event, none)) end, invalid_origin_node},
        {fun() -> is_binary(maps:get(origin_boot_id, Event, none)) end, invalid_boot_id},
        %% C01 第 1 节：新增字段结构性校验（缺字段拒绝，不静默降级）
        {
            fun() ->
                is_integer(maps:get(epoch, Event, none)) andalso
                    maps:get(epoch, Event) >= 0
            end,
            missing_protocol_field
        },
        {
            fun() ->
                is_binary(maps:get(config_hash, Event, none)) andalso
                    byte_size(maps:get(config_hash, Event)) =:= 16
            end,
            missing_protocol_field
        },
        {
            fun() ->
                is_binary(maps:get(registry_hash, Event, none)) andalso
                    byte_size(maps:get(registry_hash, Event)) =:= 16
            end,
            missing_protocol_field
        },
        %% 接收端独立白名单/敏感性校验（19.5：两端独立，fail-closed）
        {
            fun() ->
                Op =:= invalidate orelse
                    imboy_cache_policy:replicate_allowed(Key)
            end,
            not_authorized
        },
        %% 接收端独立大小校验
        {
            fun() ->
                Op =/= set orelse
                    imboy_cache_policy:check_value_size(maps:get(value, Event))
            end,
            value_too_large
        }
    ]);
validate(_) ->
    {error, not_map}.

%% ===================================================================
%% 双端一致性（C01 第 1/2/3 节）
%% ===================================================================

%% @doc 对照本节点状态校验远端事件的一致性。
%% 依次：旧化身拒绝 → epoch 推移 → hash 匹配；任一失败返回 {error, Reason}。
%% hash 不一致同时标记 peer hash_divergent（19.8 可观测 + 排除 authority）。
-spec check_consistency(map(), binary()) ->
    ok | {ok, changed} | {error, term()}.
check_consistency(Event, SelfBootId) ->
    Origin = maps:get(origin_node, Event),
    BootId = maps:get(origin_boot_id, Event),
    RemoteEpoch = maps:get(epoch, Event),
    case self_loop(Origin, BootId, SelfBootId) of
        true ->
            {error, self_loop};
        false ->
            case check_incarnation(Origin, BootId) of
                {ok, changed} = Changed ->
                    case check_epoch(RemoteEpoch) of
                        {error, Reason} ->
                            {error, Reason};
                        ok ->
                            case check_hashes(Origin, Event) of
                                ok -> Changed;
                                HashErr -> HashErr
                            end
                    end;
                ok ->
                    case check_epoch(RemoteEpoch) of
                        {error, Reason} -> {error, Reason};
                        ok -> check_hashes(Origin, Event)
                    end
            end
    end.

-spec self_loop(node(), binary(), binary()) -> boolean().
self_loop(Origin, BootId, SelfBootId) ->
    Origin =:= node() andalso BootId =:= SelfBootId.

%% @doc 化身检测（C01 第 2 节，收口修订 r1）：
%% - 已知最新化身 → ok；
%% - 宽限集合内旧化身（W 窗口） → ok（旧化身重放放行，防复活由
%%   revision/epoch 承担，原计划 5.3）；
%% - 全新化身 → 接受并注册为最新（旧化身转入宽限），返回 {ok, changed}
%%   触发调用方 C05 受管 namespace 补偿。
%% 修订原因：单条消息无法区分"peer 重启后的新化身首现"与"超窗旧化身
%% 重放"，拒绝新化身会使重启后 peer 的全部事件被永久拒绝；正确性
%% （旧值不复活）由 Lamport revision + epoch 双重承担，化身层降级为
%% 检测/注册/补偿（拒绝语义不可实现且不必要）。
-spec check_incarnation(node(), binary()) -> ok | {ok, changed}.
check_incarnation(Origin, BootId) ->
    case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
        [{_, BootId}] ->
            ok;
        [{_, _KnownOther}] ->
            case grace_member(Origin, BootId) of
                true ->
                    ok;
                false ->
                    %% 全新化身（旧宽限或从未见过）：注册 + changed 信号
                    changed = register_incarnation(Origin, BootId),
                    {ok, changed}
            end;
        [] ->
            %% 首次观察（r1 语义修订：全新化身含首现。C09 实证：恢复窗口内
            %% peer 的 report 未到达时，本节点对该 peer 无化身记录，首条
            %% 事件若按 ok 放行则补偿信号永久丢失——首现同样视为化身变化，
            %% 注册 + changed，由调用方执行有界补偿）
            _ = register_incarnation(Origin, BootId),
            {ok, changed}
    end.

-spec grace_member(node(), binary()) -> boolean().
grace_member(Origin, BootId) ->
    case ets:lookup(?META_TABLE, {grace_incarnation, {Origin, BootId}}) of
        [{_, RegisteredAtMs}] ->
            RegisteredAtMs + imboy_cache_policy:grace_window_ms() >
                erlang:system_time(millisecond);
        [] ->
            false
    end.

%% @doc 登记 peer 当前化身；若与已知化身不同，旧化身转入宽限集合
%% （W=720s），并返回 changed=true（调用方对受管 namespace 执行
%% 有界补偿，C05）。
-spec register_incarnation(node(), binary()) -> changed | registered.
register_incarnation(Origin, BootId) ->
    case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
        [{_, BootId}] ->
            registered;
        [{_, OldBootId}] ->
            NowMs = erlang:system_time(millisecond),
            true = ets:insert(
                ?META_TABLE,
                {{grace_incarnation, {Origin, OldBootId}}, NowMs}
            ),
            true = ets:insert(?META_TABLE, {{peer_incarnation, Origin}, BootId}),
            changed;
        [] ->
            true = ets:insert(?META_TABLE, {{peer_incarnation, Origin}, BootId}),
            registered
    end.

%% @doc 化身是否发生变化（握手帧路径用：变化即触发 C05 补偿）。
-spec incarnation_changed(node(), binary()) -> boolean().
incarnation_changed(Origin, BootId) ->
    case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
        [{_, BootId}] -> false;
        [{_, _Other}] -> true;
        [] -> false
    end.

%% @doc 淘汰超窗宽限化身（bounded_sweep 周期调用，时间有界即数量有界）。
-spec prune_grace(non_neg_integer()) -> ok.
prune_grace(NowMs) ->
    WindowMs = imboy_cache_policy:grace_window_ms(),
    _ = [
        ets:delete(?META_TABLE, K)
     || {K = {grace_incarnation, _}, RegisteredAtMs} <- ets:tab2list(?META_TABLE),
        RegisteredAtMs + WindowMs =< NowMs
    ],
    ok.

%% @doc epoch 推移校验（C01 第 1 节）：远端 epoch 小于本节点 → 拒绝
%% （set/delete）；invalidate 不经此路径（宁多失效语义，见 sync 路由）。
-spec check_epoch(non_neg_integer()) -> ok | {error, stale_epoch}.
check_epoch(RemoteEpoch) ->
    case RemoteEpoch < epoch() of
        true -> {error, stale_epoch};
        false -> ok
    end.

%% @doc hash 双端校验：不一致 → 标记 divergent 并拒绝整事件
%% （不部分应用；C10 混合矩阵的 hash 不一致场景由此闭合）。
-spec check_hashes(node(), map()) -> ok | {error, hash_mismatch}.
check_hashes(Origin, Event) ->
    case
        maps:get(config_hash, Event) =:= config_hash() andalso
            maps:get(registry_hash, Event) =:= registry_hash()
    of
        true ->
            _ = ets:delete(?META_TABLE, {hash_divergent, Origin}),
            ok;
        false ->
            true = ets:insert(?META_TABLE, {{hash_divergent, Origin}, true}),
            {error, hash_mismatch}
    end.

%% @doc peer 是否 hash 不一致（排除 ready authority 用，C01 第 8 节）。
-spec hash_divergent(node()) -> boolean().
hash_divergent(Node) ->
    ets:member(?META_TABLE, {hash_divergent, Node}).

%% ===================================================================
%% 本节点状态（META 表）
%% ===================================================================

-spec epoch() -> non_neg_integer().
epoch() ->
    case ets:lookup(?META_TABLE, sync_epoch) of
        [{_, E}] when is_integer(E) -> E;
        [] -> 0
    end.

%% @doc 推进本节点 epoch（C01 第 3 节：冷恢复完成或受控清理时）。
-spec bump_epoch() -> non_neg_integer().
bump_epoch() ->
    E = epoch() + 1,
    true = ets:insert(?META_TABLE, {sync_epoch, E}),
    E.

%% @doc 观察 peer epoch（握手帧路径记录；authority 判定用）。
-spec peer_epoch(node()) -> non_neg_integer().
peer_epoch(Node) ->
    case ets:lookup(?META_TABLE, {peer_epoch, Node}) of
        [{_, E}] when is_integer(E) -> E;
        [] -> 0
    end.

-spec config_hash() -> binary().
config_hash() ->
    imboy_cache_policy:config_hash().

-spec registry_hash() -> binary().
registry_hash() ->
    imboy_cache_policy:registry_hash().

%% @doc 版本上界请求帧（新协议 r2：带请求者 pid 与 epoch/hash/boot 供对端判定；
%% 应答方直达 FromPid，规避 syn 视图传播滞后导致的应答丢失）。
-spec version_request_frame(pid()) -> term().
version_request_frame(FromPid) when is_pid(FromPid) ->
    {cache_version_request, FromPid, node()}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec checks([{fun(() -> boolean()), term()}]) -> ok | {error, term()}.
checks([{Pred, Reason} | Rest]) ->
    case Pred() of
        true -> checks(Rest);
        false -> {error, Reason}
    end;
checks([]) ->
    ok.
