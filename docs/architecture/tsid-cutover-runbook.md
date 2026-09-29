# TSID Cutover Runbook — 高水位 bootstrap 操作手册（TSID-08）

> 状态：**LOCAL_EVIDENCE_ONLY**。本 runbook 全部步骤只在 scratch/clone DB 上验证过；
> 生产 cutover 属外部验证（未执行），RELEASE=NO_GO 不变。
>
> 工具：`tsid_scanner.escript`（只读扫描）、`tsid_bootstrap_floor.escript`（合同验证参考实现）。

## 0. 前置条件（全部满足才可进入步骤 1）

- [ ] 新版后端镜像已部署但 **旧版仍在跑写流量**（或两者都停写——见 §3 停写合同）
- [ ] scratch/clone DB 可达（**禁止直连生产**：scanner 只允许对 clone/从库/停写后的主库执行）
- [ ] `inventory-merged.tsv` 与目标库 schema 版本一致（迁移 head 相同）
- [ ] `inventory-supplement.tsv` 的逐表判定（本卡已完成 76 表 + 6 排除）仍适用于目标库

## 1. 只读扫描（停写前，获取基线）

```bash
ERL_FLAGS="-pa deps/epgsql/ebin" escript tsid_scanner.escript \
    inventory-merged.tsv <outdir-run1> <allow_exclude_csv> -- \
    <host> <port> <user> <password> <database>
```

- 期望 `SCAN_OK`；任何 `BLOCKED_CUTOVER`（db_only 新表 / missing 表 / future ID）都必须先处置，**不得跳过**。
- 记录 `scratch-high-water.json` 的 digest（剔除 `scanned_at_ms` 后 SHA-256）。

## 2. 停写（§3 合同）后二次扫描（高水位稳定证明）

- 同命令再跑一次 → `outdir-run2`。
- digest 必须与 run1 **一致**；不一致 = 停写不彻底或仍有后台写入 → **BLOCKED_CUTOVER，回到 §3**。
- 取 run2 的 `high_water.floor_candidate = max(全表 max_id 的 ts) + 1`。

## 3. 停写合同（AC-08C，全程必须明确）

停写范围 = **所有写 TSID 列的路径**（193 表，见 inventory-merged.tsv），最小集合：

1. 停后端应用流量（nginx/LB 摘除或 `docker compose stop imboy_backend` / helm scale 0）
2. 停后台 worker（消息清扫、账单、审核等全部 imboy_sup 子进程随应用停止）
3. 确认无旁路写入：cron 任务、运维脚本、psql 手工导入全部暂停
4. 停写验证：双扫 digest 一致（§2）

> 旧版 generator 的 cursor 在**内存 atomics**里，进程停止即消失——不存在"残留 writer"，
> 但**必须确认所有旧 BEAM 进程已退出**（`pg_stat_activity` 无应用连接）再二次扫描。

## 4. 离线 bootstrap（floor 写入 durable store）

1. 在新部署的持久卷目录（`IMBOY_TSID_STATE_DIR`）预置 floor：
   - 参考 `tsid_bootstrap_floor.escript` 的场景 A：fresh store + `persist(SafeBefore = FloorCandidate + fence_window_ms)`
   - `SafeBefore` 必须 ≥ `FloorCandidate + fence_window_ms`（否则首批 ID 无分配空间）
2. **bootstrap 合同（fresh/existing 明确）**：
   - `IMBOY_TSID_STORE_BOOTSTRAP=fresh` 仅本步骤的离线工具使用；
   - 后端进程**永远**用 `existing`：无有效槽即拒绝启动，绝不静默当 fresh；
   - cutover 完成后从 `.env`/values 移除任何 `fresh` 残留（preflight 会拒绝 fresh+非空目录）。

## 5. 启动新版与首批验证（AC-08B 执行口径）

1. 以 `existing` 启动新版后端；guard 从 durable store 恢复 floor
2. 观察 `/readyz` → `tsid: ready`
3. 生成首批 ID（如创建测试用户），断言 **ts ≥ floor_candidate**
   （参考 `bootstrap-floor.json`：5000/5000 unique，min_ts ≥ floor）
4. 恢复流量

### 双层容忍合同（实测证据）

| floor 超前墙钟 | 行为 | 出处 |
|---|---|---|
| ≤ `max_logical_lead_ms` (512ms，缺省定标值) | 首批 ID ts 全 ≥ floor | bootstrap-floor.json 场景 A |
| > 512ms 且 ≤ `max_initial_lead_ms` (60s) | guard 可启动，但 generate 在墙钟追上 floor 前 typed `capacity_exhausted(clock_wait)`（deadline 内等待） | bootstrap 脚本首轮实测（30s floor） |
| > `max_initial_lead_ms` (60s) | guard **拒绝启动** `clock_behind`（= BLOCKED_CUTOVER 的运行时防线） | bootstrap-floor.json 场景 B |

> 结论：**cutover floor 只允许超前墙钟 ≤ 60s**（boot 容忍）；> max_logical_lead_ms（缺省 512ms）时首批 ID 会等墙钟
> 追平（deadline 内）。若历史高水位超前当前时钟超过 60s（如导量携带未来 ts），
> 必须 **BLOCKED_CUTOVER**：先人工修正数据或调整容忍参数并评审，不得强行启动。

## 6. 异常处置

| 现象 | 处置 |
|---|---|
| 二扫 digest 漂移 | 停写不彻底：找 writer（§3.4），重新停写 |
| `future_beyond_tolerance` | 有数据 ts 超前墙钟 > 60s：BLOCKED_CUTOVER，人工核查来源 |
| `table_scan_errors`（scanner） | 任一表扫描失败即 BLOCKED_CUTOVER——失败表不计入高水位，floor 会偏低，禁止放行；修复查询/权限后重扫 |
| `negative_id` / `beyond_max_id` | 历史 bug 遗留：评估修复或剔除，**不可静默放行** |
| guard 启动 `clock_behind` | floor 超前 > 60s：BLOCKED_CUTOVER（见上） |
| guard 启动 `no_valid_slot` | bootstrap 未执行或路径错：检查 `IMBOY_TSID_STATE_DIR` 挂载 |
| 双实例锁拒绝 `lock_taken` | 同 Node 另一实例存活：确认旧实例已退出 |
