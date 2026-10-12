%% 分布式缓存同步族共享状态（imboy_cache_sync / _apply / _recover 三模块共用）。
%% 仅放跨模块合同：#state{} 记录与共享 ?define。拆分见 closeout 例外 4
%% （imboy_cache_sync.erl 拆分重构，函数体逐字迁移、零行为变更）。

-define(META_TABLE, imboy_cache_meta).
%% 恢复单轮等待与有界重试（9.1 RETRY_WITH_LIMIT；总预算受 Dmax 约束）
-define(RECOVER_ROUND_MS, 5000).
-define(RECOVER_MAX_RETRY, 3).
%% 定期状态校对周期（9.4，本地工程候选）
-define(RECONCILE_INTERVAL_MS, 60000).

-record(state, {
    boot_id :: binary(),
    counter = 0 :: non_neg_integer(),
    origin_seq = 0 :: non_neg_integer(),
    status = starting :: starting | discovering | recovering | ready | degraded,
    retries = 0 :: non_neg_integer(),
    %% 恢复总 deadline（Dmax，C01 第 6 节）
    recover_deadline_ms :: non_neg_integer() | undefined,
    %% 快照回合状态（imboy_cache_sync_snapshot 的 plain map；undefined=非恢复中）
    snapshot_round = undefined :: map() | undefined
}).
