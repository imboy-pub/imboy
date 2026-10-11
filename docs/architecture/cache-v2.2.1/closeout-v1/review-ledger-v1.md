# C00 审查台账 — 历史结论复核与新分类

> task_id: CACHE_V221_BACKEND_CLOSEOUT_V1 生成于 2026-10-11T13:14:42+00:00

## 历史台账概览（不覆写，仅复核）

- 历史台账：86 项 = 77 PASS / 6 BLOCKED / 2 NOT_RUN / 1 PARTIAL（源：zcode-20261010T160733Z/evidence/ledger.json）
- ID 对齐：原计划两位格式（W4-10/AC-22）与历史台账三位格式（W4-010/AC-022）以显式 alias 对齐，26 项 alias 登记，无重复、无缺失、无多余

## 按收口计划第 3 节八条纠正意见的分类

| 纠正意见 | 受影响 ID | 新分类 | 收口任务 |
|---|---|---|---|
| 1. 有界保留时间≠数量/bytes 有界 | 19.6-有界恢复、W4-06、W4-07、W4-09 | GAP_IMPLEMENTATION/REVERIFY | C01/C03/C04 |
| 2. 快照整批枚举/空快照完成/仅 counter 校对不足 | W4-03、W4-04、AC-19 | GAP_IMPLEMENTATION/GAP_EVIDENCE | C04/C09 |
| 3. invalidate 缺口补偿未完成；重启回填≠分区重连验证 | W4-05、AC-20、W4-11 | GAP_IMPLEMENTATION/GAP_EVIDENCE | C05/C09 |
| 4. boot_id 类型检查≠incarnation/epoch/config 校验 | W3-04、AC-17、19.8 | GAP_IMPLEMENTATION/GAP_EVIDENCE | C01/C02/C11 |
| 5. 全量 Failed 4/Passed 11078/exit 非零 ≠ 全量 PASS | W5-10、AC-30 | GAP_EVIDENCE（须新候选 exit=0 重跑） | C12 |
| 6. 旧性能数据不能替代新 V2 路径压测 | W4-10、W5-07、AC-26、AC-27 | NOT_RUN_CARRIED/GAP_EVIDENCE/BLOCKED_EXTERNAL | C12/C13 |
| 7. 旧 candidate 字段/1970 时间戳纠正 | （全台账纪律） | verifier 拒绝（C14） | C14 |
| 8. sync 894 行 > 800 行规范 | （代码规范） | C03 最小拆分 | C03 |
| G-QR 独立方案已交付，本轮实施 | G-QR、W1-06、AC-22、W5-04 | GAP_IMPLEMENTATION | C08 |
| W5-02 三节点完整 SET 保留 | W5-02 | GAP_EVIDENCE（含合法例外说明） | C09/C14 |
| W0 SLO 冻结缺生产输入 | W0-09、AC-26、AC-27 | BLOCKED_EXTERNAL | C13 |

## 分类统计

- BLOCKED_EXTERNAL: 3
- GAP_EVIDENCE: 7
- GAP_IMPLEMENTATION: 13
- NOT_RUN_CARRIED: 1
- REVERIFY: 63
- REVERIFY_HIST_INHERIT: 9

## 全部条目明细

见 ledger-v1.json（机器可读）。旧 PASS 一律待重新绑定最终候选；例外仅在原计划明确允许时成立。
