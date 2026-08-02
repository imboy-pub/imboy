# 审计范围书模板（SOW Template）

> 审计就绪包**第 6 件工件**（P5-6）。审计方据此报价与开工。
> 格式逐项对齐 [`research/public-audit-cases.md`](./research/public-audit-cases.md) §3 的八条行业惯例。
> 最后核对：2026-08-02

方括号 `[...]` 是签约时填写的空位；其余为我方已确定内容。

---

## 1. 范围与版本锚定

**锚定原则**：精确到 commit 与库版本，**含排除项声明**。当前锚见
[`evidence-manifest.generated.md`](./evidence-manifest.generated.md) §1（机器生成，含工作树脏否）。

### 范围内

| 侧 | 内容 |
|---|---|
| 后端 `imboy` @ `[commit]` | `src/{api,logic,ds,repo,lib}` 中 e2ee / olm / trust / kt / backup 模块；`priv/migrations` 36–53 |
| 客户端 `imboyapp` @ `[commit]` | `lib/service/e2ee/`（33 模块）+ `lib/service/{e2ee_service, olm_session_service, group_session_service, e2ee_crypto_service, e2ee_local_backup_service, e2ee_server_backup_service}.dart` |
| 依赖 | `vodozemac` 0.5.0 —— Least Authority 2022-03 已审计。**建议复用其结论**，重点审我方胶水层与协议集成 |

### 明确排除

| 排除项 | 理由 |
|---|---|
| MLS | 未实现，R3 已裁定不做 |
| 后量子（PQ） | 路线图，明确不在当前范围 |
| Web SDK | 未建 |
| 附件加密的**运行时**行为 | 开关未翻开（IMB-2026-005），只能审代码与设计，无运行时可测 |
| KT 的**运行时**行为 | 未部署（IMB-2026-007），同上 |

> ⚠️ 后两条排除是**能力限制而非范围偏好**。若审计方认为必须覆盖运行时，
> 需先由我方完成开关翻开 / KT 部署，属前置条件而非 SOW 内工作。

---

## 2. 人日账目

行业公开报告普遍披露工作量（20 人日 / 35 人日 / 16 天×N / 4–12 人周）。
我方要求报价中包含**人日数与角色分工**，并在最终报告中披露。

预估区间（**仅供审计方参考，非我方承诺**）：`[  ]` 人日，其中协议层分析 `[  ]`、代码审计 `[  ]`、retest `[  ]`。

---

## 3. 方法论

- **白盒代码审计**：源码全量提供，含 git 历史。
- **协议层分析**：对照协议白皮书（就绪包第 3 件，**编写中**）与
  [`../v2/08-threat-model.md`](../v2/08-threat-model.md) 的 T1–T11。
- **测试套件复跑**：见 §5 环境复现。
- 若含形式化验证：需注明工具（Tamarin / ProVerif / CryptoVerif / hax）与**模型假设**。

---

## 4. Findings 格式

| 要求 | 说明 |
|---|---|
| 唯一编号 | ⚠️ **不要用 `IMB-2026-NNN` 前缀** —— 该命名空间已被我方[已知问题台账](./known-issues-ledger.md)占用（001..027）。请用贵方自有前缀或 `EXT-2026-NNN` |
| 严重度分级 | Critical / High / Medium / Low / Info |
| 漏洞 vs 弱点 | 必须区分：可利用的缺陷 vs 加固建议 |
| 复现步骤 | 每条 finding 附最小复现路径 |
| 对照已知项 | 若某 finding 与台账中某条重合，请注明其 `IMB-2026-NNN`，并**明确表态是否认可我方给出的接受理由** |

> 最后一条是刻意要求的：台账里的 `Acknowledged` 理由写出来就是为了让它可被反驳。
> 审计方沉默不等于认可。

---

## 5. 环境复现

| 侧 | 命令 |
|---|---|
| 后端 | Docker `imboy_pg18` + `make eunit-local` + `make e2ee-verify`（一键门禁） |
| 客户端 | `flutter test test/service/e2ee/`（60 文件） + 契约测试（真后端） |
| 双端 | ⛔ **不可用** —— 无跨进程 harness（IMB-2026-022），真机双端从未验证（IMB-2026-021） |
| 许可证门 | `scripts/license_inventory.sh --check`（当前必然退出 1，AGPL 未解，属预期） |
| 证据清单 | `scripts/evidence_manifest.sh` |

已知会红 / 会跳过的项，以及理由，见[已知问题台账](./known-issues-ledger.md) §5。

---

## 6. Retest 轮

- 修复后包含 `[N]` 轮 retest（行业惯例：Least Authority verification phase / NCC fix review）。
- retest 范围限于原 findings 及其直接回归面，不含新增功能。
- 每条 finding 最终落 `Resolved` 或 `Acknowledged`；**允许"协议层限制不修"**
  （先例：vodozemac Issue J 的 MAC 截断继承自 libolm）。

---

## 7. 报告公开发布

🔒 **待我方拍板，SOW 签署前必须明确。**

行业惯例是经客户同意后公开，并由被审方博客背书（Least Authority 明示此惯例）。
公开对产品可信度有正面价值，但也会把**本台账 §1/§2 的阻断项与未生效防御一并公开**。
这是商业判断，不在本模板中预设立场。

选项：① 完全公开 ② 公开摘要、细节仅对买家 NDA 下提供 ③ 不公开。

---

## 8. 我方提供物清单

对照行业惯例第 8 条「被审方需提供」，逐项落位：

| 惯例要求 | 我方对应 | 状态 |
|---|---|---|
| 协议规范文档 | 就绪包第 3 件（白皮书） | ❌ **编写中**，当前须直接读 ADR 00–31 |
| 源代码（白盒） | 双仓全量 + git 历史 | ✅ |
| 可复现构建/测试环境 | §5 | 🟡 双端 harness 缺 |
| 威胁模型声明 | [`../v2/08-threat-model.md`](../v2/08-threat-model.md)（T1–T11） | ✅ |
| 答疑通道 | `[联系方式 —— 签约时由我方指定]` | 🔒 待定 |
| 密码学清单 | [`crypto-inventory.md`](./crypto-inventory.md) | ✅ |
| 密钥生命周期 | [`../key-lifecycle.md`](../key-lifecycle.md) | ✅ |
| 已知问题台账 | [`known-issues-ledger.md`](./known-issues-ledger.md) | ✅ |
| 证据清单 | [`evidence-manifest.generated.md`](./evidence-manifest.generated.md) | ✅ |

> **开工前置**：白皮书缺位不阻塞代码审计，但会显著抬高协议层分析的成本
> ——审计方需自行从 31 份 ADR（`v2/00`–`31`，其中 23 号缺号）里重建协议全貌。
> 若报价对此敏感，请在报价中单列该项。
