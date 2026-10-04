# 基线漂移注记 — seat-console 并行工作线（2026-10-04）

> 记录人：验收协调（ZCode）| 性质：状态观测，无判定翻转，无决策变更

## 结论速览

- **DEC-1…DEC-4 状态不变**：仍全部 PENDING_USER（decision-requests-log.json 未动）。
- **原冻结基线仍为交付口径**：`imboyapp ab306ddf`（本验收线最后提交，HEAD 未动）；`imboy 398e9e47` 为本验收线最后提交，但 **imboy 仓 HEAD 已被并行工作线推进至 `7af11b49`**。
- 本注记目的：确保 RES-L3-RERUN（待全部 DEC 落定后执行）先固定时点基线，避免在未知脏树上跑回归。

## imboy 仓漂移（398e9e47 → 7af11b49，14 提交）

作者均为 `leeyi`，来自另一并行工作线（seat-console / 企业工作台验收 + dialyzer 基线清理），内容与本验收线无冲突归属：

| 主题 | 提交 |
|---|---|
| seat-console 合并预演（EWS-T1 fixture 跨 VM 唯一性等） | 7af11b49、7a1c2f05、c3518376、84e4db78 |
| dialyzer 类型清理（崩溃家族 9-15 例收官，347→335） | c0c13c19、246a2d2c、9238aa4e、e264897c、b253f524 |
| e2ee 测试修复（trust_audit TSID 注册、metrics 让进程） | 03ce8e44、b32c6bec |
| /seat 404 契约、api_contract 再生、DEC-2(a) 决策卡 | 0540a70b、ff99eb1c、1fe06240 |

## 工作树未提交 WIP（非本验收线产物，不触碰、不提交、不回退）

`git status` 脏文件：

- `src/ds/group_ds.erl`（12 行改动）：工作树移除 HEAD 中 `member_uids_strict` 的 `Other ->` 兜底
  （该兜底 blame 至 2026-07-31 41034a521，历史存量）、移除 `check_avatar(_)` 兜底、改写
  `face2face_save` spec（回退 `{rollback, term()}` 分支）。**face2face_save 属本验收 f2f 域，
  RES-L3-RERUN 前须确认该 WIP 落定方向。**
- `src/lib/elib_pg_sql.erl`（1 行）：`parse_result` spec 改写。
- `test/ct.cover.spec`：工作树删除（HEAD 仍在）。
- 未跟踪：`docs/business/imboy-open-source-productization-strategy-v1.md`、
  `docs/design/2026-10-04-enterprise-workspace-acceptance/`（另一验收线证据目录，互不归属）。

## imboyapp 仓状态

HEAD 未动（`ab306ddf`）。工作树仅 7 个 `.g.dart` 生成物再生成漂移（7+/7-，build_runner 良性产物），不构成实质漂移。

## 对 RES-L3-RERUN 的执行约束（写入流程，非新决策）

1. 执行前记录当时 `git rev-parse HEAD` + `git status --porcelain` 快照入本目录；
2. 若 imboy 工作树仍含上述未知 WIP（尤其 `group_ds.erl` 的 f2f 域改动），先留证快照再跑，
   或请示用户是否以 HEAD+脏树现状为准——不得静默 stash/revert 他线 WIP；
3. 连续两轮 L3 全量 exit=0 的判定以该固定时点状态为准。
