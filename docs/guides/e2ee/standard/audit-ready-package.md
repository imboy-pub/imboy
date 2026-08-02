# Audit-ready Package：审计就绪包索引

> **层**：交付层 ｜ **建立**：2026-08-01 ｜ **组装任务**：Hardening Plan P5-1..P5-9
> **定位**：第三方密码学审计方（或买家安全团队）进场时**唯一入口**。目标：审计方仅凭本索引+仓库即可完成报价与开工。
> **形态说明**：按用户 2026-08-01 决策，本包为"就绪包"而非已采购审计（TT-D5 SHOULD 降级形态）。

## 1. 包内容物（六件套+二附录）

| # | 工件 | 内容 | 当前状态 | 来源任务 |
|---|---|---|---|---|
| 1 | 威胁模型 | 攻击者能力分级；每威胁→防御→证据三对照 | ✅ [`../v2/08-threat-model.md`](../v2/08-threat-model.md)（2026-08-02 补齐 T10 附件 / T11 分叉视图，含「有守护测试 ≠ 运行时生效」读法警告） | P5-1 |
| 2 | 密码学清单 | 原语/参数/曲线/库/版本/许可证；**AGPL 状态醒目标注** | ✅ [`crypto-inventory.md`](./crypto-inventory.md)（原语与参数，每行标注 📄本仓实证 / 📕上游规范 / ⚙️上游默认）+ [`../../../legal/third-party-licenses.md`](../../../legal/third-party-licenses.md)（143 条许可证清单）。**未含 SBOM hash 锚** | P5-2 |
| 3 | 协议规范白皮书 | wire v1/v2/v3、PFv3 canonical、room-key 双包、trust 11 字段、附件 descriptor、KT profile v1、备份容器；golden 向量附录；修订史 | 🟡 素材=ADR 00-31，待汇总 | P5-3 |
| 4 | 密钥生命周期 | 生成/存储/轮换/备份/销毁/吊销级联全链路 | ✅ [`../key-lifecycle.md`](../key-lifecycle.md)（10 类密钥材料矩阵 + 服务端 8 表敏感度分级 + 三条销毁路径 + 可验证性主张） | P5-4 |
| 5 | 已知问题台账 | Acknowledged/Open/Blocked 状态机 | ✅ [`known-issues-ledger.md`](./known-issues-ledger.md)（IMB-2026-001..027，按问题而非按标准条款组织；含 §2「名义防御与运行时不符」三条与 §6 审计方使用说明） | P5-5 |
| 6 | 审计范围书（SOW 模板） | 范围锚定/方法/环境复现/交付物格式/答疑通道 | ❌ 待建（格式依据 `research/public-audit-cases.md` §3 八条惯例） | P5-6 |
| 附 A | 自审计报告 | 按 `top-tier-standard-2026.md` 逐条核验+证据链接+签字 | ❌ 待建 | P5-7 |
| 附 B | Evidence Manifest | 版本/commit/依赖 hash/测试计数/真机结果/向量 hash/故障注入计数 | ✅ [`evidence-manifest.generated.md`](./evidence-manifest.generated.md)，由 `scripts/evidence_manifest.sh` 生成（格式=v2/20 §13）。**11 项拿不到的字段全部打印「⛔ 未提供 + 原因」而非省略**——漏报的证据清单比没有清单更危险 | P5-8 |

## 2. 审计方快速上手（SOW 模板核心段草稿）

```text
范围锚定：
  后端  imboy @ <commit>：src/{api,logic,ds,repo,lib} 的 e2ee/olm/trust/kt/backup 模块
        （清单见 gap-matrix.md E 类行）+ priv/migrations 36-53
  客户端 imboyapp @ <commit>：lib/service/e2ee/（33 模块）+
        lib/service/{e2ee_service,olm_session_service,group_session_service,
        e2ee_crypto_service,e2ee_local_backup_service,e2ee_server_backup_service}.dart
  依赖  vodozemac 0.5.0（AGPL-3.0，Least Authority 2022-03 已审计——
        建议复用其结论，重点审我方胶水层与协议集成）
  排除项：MLS（未实现）、PQ（路线图）、Web SDK（未建）
方法：白盒代码审计 + 协议层分析（对照白皮书 §3）+ 测试套件复跑（D7）
环境复现：
  后端  Docker imboy_pg18 + make eunit-local + make e2ee-verify（一键门禁）
  客户端 flutter test test/service/e2ee/（60 文件）+ 契约测试（真后端）
  双端  test harness（P4-2 交付后可用）
交付物格式：编号 findings/严重度分级/漏洞 vs 弱点/retest 轮/Resolved-Acknowledged
```

## 3. 自审计报告骨架（P5-7 填充）

```text
1. 范围与版本锚（commit 双仓）
2. 标准符合性总表：TT-A/B/C/D/E 逐条 [达成/部分/未达+理由+证据链接]
3. 攻击面复核：对照 ETH 七攻击/2022 Matrix 攻击/TEST_HITLIST 逐条给防御与测试证据
4. 发现清单（编号 IMB-2026-XXX，严重度，Resolved/Acknowledged）
5. 签字：安全负责人/日期
```

## 4. 交付前检查单

- [ ] 六件套全部入库（git 跟踪）且与代码现状逐点核对（文档不超前于实现）
- [ ] Evidence Matrix 全 MUST 行绿
- [ ] 不可宣称清单与售前材料零冲突（对照 07-31 审计七项口径）
- [ ] AGPL 决策（P0-6）已闭环且许可证文件一致
- [ ] 测试套件按 D7 说明可由第三方独立跑通
- [ ] 台账无"隐瞒项"——所有 Acknowledged 都有理由与负责人
