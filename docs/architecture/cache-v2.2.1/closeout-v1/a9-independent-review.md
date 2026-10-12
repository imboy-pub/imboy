# A9 独立审查 — CACHE_V221_BACKEND_CLOSEOUT_V1（重启轮）

- 审查者: A9（独立，git 只读）
- 日期: 2026-10-12
- 对象提交: 7db4e0af / e001fb8e / 675b0744 / e903dc65 / 9f863af2（+ 范围 6217ec8e..HEAD 全量核查）
- 证据链: `~/.codex/cache-closeout-runs/zcode-20261011T130725Z/`（raw + raw/perf + candidate_freeze.json）

## 判定

### 1. e903dc65 三处产品修复回归风险 — PASS

- **a) check_incarnation 首现 changed（补偿风暴风险）**: PASS — changed 仅触发 `apply_v2` → `gap_compensation_key(Key)`（src/lib/imboy_cache_sync.erl:513），单键 O(1)（非 fenced 键为 no-op），且注册后同 (peer boot_id, 节点) 后续事件均返回 ok，每节点每 peer 至多一次；宽限集由 `prune_grace` 有界；version_report 路径的 `changed_compensation({peer_epoch, Node})` 用 meta 键、非 fenced → no-op。冷启动风暴下无放大机制，补偿上限 = 节点数 × peer 数。仅补偿首事件携带键、其余靠 TTL/权威回源为代码内已声明的 C05 既有边界，非本次回归。
- **b) r2 帧 pid 直达兼容子句**: PASS — 新 3 元组请求 `when is_pid(FromPid)` 直达应答（:424）；旧 2 元组请求子句保留并改经视图反查应答（:431）；旧 2 元组 report 仍被处理（:362）；M10「legacy 2-tuple still answered」在 13/13 ×2 中实证。备注（非阻塞）：对旧请求的应答是新 9 元组帧，唯一的 pre-r2 V2 代码（7db4e0af）为本交付内未发布中间态，真实 legacy V1 节点对不可解析应答按超时有界回退（M4 fail-closed 覆盖），方向安全。
- **c) take_page 字节预算**: PASS — 新守卫仅在 `length(Acc) >= Pmax` 切页；字节预算判定 `Acc =/= [] andalso entry_sum_bytes > PmaxBytes` 保证空页时单条超预算条目仍被收下单独成页（imboy_cache_sync_snapshot.erl:160-172），双预算语义完整；附带 `maps:get_values` → `maps:values` 修复潜在 undef。

### 2. G-QR 原子消费 — PASS

- claim SQL（src/repo/qr_login_session_repo.erl:164-179）：单语句 `UPDATE ... WHERE session_token=$1 AND status='confirmed' AND login_token IS NOT NULL AND consumed_at IS NULL AND expires_at > now() RETURNING OLD.login_token, scanned_by`。PG 行锁串行化 + READ COMMITTED 下 EvalPlanQual 对已更新行重评 WHERE（login_token IS NOT NULL 变假）→ 并发恰好一次；`RETURNING OLD.*` 需 PG18（项目基线 PG 18+，一致）；真实 PG 100 轮 4 路并发恰好一次测试佐证。
- 迁移 down（priv/migrations/00000170_qr_login_session.down.sql）：`DROP TABLE IF EXISTS` + 头注明确声明不可逆（token 不恢复、无残留再领取路径），对一次性凭据为安全设计；up 侧 `qr_login_authority` 默认 cache、生产行为不变，生产启用另轮决策。
- 备注（非阻塞）：claim 将 PG 错误归并为 not_claimable——fail-closed 方向安全，瞬时错误与已领取对客户端同语义。

### 3. 证据链一致性 — PASS

- 4 个绑定轮（three_node_c09_b2/b3、mixed_c10_b3/b4）`candidate_sha256` = `f1aa1ce0…020c`，与 candidate_freeze.json 逐字一致（freeze: candidate_git_sha1=2873dbfd，frozen_at 2026-10-12T03:45:31Z；2873dbfd 起仅 test/docs 变更，与 note「product code unchanged」相符）。
- 用例数实证：scenarios 22 pass/22 ×2、13 pass/13 ×2。
- 时间戳顺序：c09 b2 03:59:45Z < b3 04:14:26Z；mixed b3 04:17:56Z < b4 04:23:06Z；全部晚于 freeze；perf v2_cand_4-6（04:48Z）在 freeze 之后。
- perf 系列绑定仅在 v2_cand_4/5/6（v2_cand_1-3 为 null、w0_cand_1-3 无该字段），符合声明。

### 4. 红线合规 — PASS

- `git log 6217ec8e..HEAD --name-only` 全量无 `imboyapp/`、`imboyadmin/` 路径。
- deploy/ 仅触及 `deploy/grafana/dashboards/imboy-cache-sync.json` 与 `deploy/prometheus/rules/imboy-alerts.yml`；告警 diff 为纯追加的 cache sync 三条告警（StuckRecovering/Degraded/SnapshotAborting），面板为 cache sync 仪表盘（7→12），无越界。

## 总结论

**APPROVE_WITH_CONCERNS**（两条均非阻塞，不构成打回理由）：

1. **证据格式瑕疵**：perf/v2_cand_4/5/6.json 的 `candidate_sha256` 值内嵌了字面双引号（值为 `"<sha>"`），与 freeze 文件仅在剥引号后相等；属记录卫生问题，绑定仍可核验。建议后续批次修正生成端引号拼接。
2. **r2 应答格式不对称**：旧 2 元组请求收到的是 9 元组应答，pre-r2 V2 中间态代码（7db4e0af，未发布）无法解析；真实 legacy V1 节点按超时有界回退（M4 已覆盖 fail-closed），且 8 元组中间帧从未发布，无现实混布暴露面。记录在案即可。
