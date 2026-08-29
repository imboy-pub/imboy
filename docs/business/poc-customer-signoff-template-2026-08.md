# 单节点 PoC 客户签字记录模板 / PoC Customer Sign-off Template

> **版本 / Version**: 2026-08 v1.0 | **配套**：[PoC 范围表](./poc-single-node-scope-2026-08.md) ｜ [验收矩阵](./poc-single-node-acceptance-matrix-2026-08.md)
> **使用说明**：本模板为空白表单，`<>` 为占位符，签署时逐项填写；**不得**在仓内文档中填写真实客户名称、联系人、邮箱、电话或 IM 账号——已签署的纸质/扫描件归档于商业资料流程，不进入产品仓。
> 本签字仅对**该次 PoC 的环境、版本与范围**负责，不构成对集群、HA、信创、生产支付等路线图能力的承诺。

---

## 1. PoC 标识

| 字段 | 值 |
|---|---|
| PoC 编号 | `<POC-YYYYMMDD-序号>` |
| 主场景 | `<按 D1 确认的主场景描述>` |
| 执行区间 | `<YYYY-MM-DD> 至 <YYYY-MM-DD>`（10 个工作日） |
| 范围表版本 | poc-single-node-scope-2026-08.md（v1.0） |
| 验收矩阵版本 | poc-single-node-acceptance-matrix-2026-08.md（v1.0） |

## 2. 软件版本指纹（Release Identity，安装完成后由 `install.sh` 打印）

| 字段 | 值 | 核对 |
|---|---|---|
| `IMBOY_VERSION` | `<安装完成时打印>` | 与 Release 说明三元组一致 ☐ |
| `IMBOY_GIT_SHA` | `<安装完成时打印>` | 与 Release 说明三元组一致 ☐ |
| `IMBOY_IMAGE_DIGEST` | `sha256:<安装完成时打印>` | 与 Release 说明三元组一致 ☐ |
| 部署形态 | ☐ 社区版（docker-compose.community.yml） ☐ 商务版（prod.yml + sales-policy overlay） | |
| E2EE 模式（验收时） | ☐ `required` ☐ `compliance`（已向客户披露第二收件人机制） | |

> 若 PoC 执行时首个正式 semver 版本尚未发布（见 `RELEASES.md` 当前状态），此处如实填写 alpha 内部版本号，并在 §5 限制说明中注明"A9 升级路径按内部版本演练"。

## 3. 环境指纹

| 字段 | 值 |
|---|---|
| 服务器规格 | `<如 4C8G / OS 发行版与版本>` |
| Docker / Compose 版本 | `<docker -v / docker compose version 输出>` |
| API 域名 | `<API_DOMAIN>`（域名本身非 PII，按客户意见填写或留占位） |
| Admin 域名 | `<ADMIN_DOMAIN>`（同上） |
| 数据库 / 对象存储 | PostgreSQL `<版本>` / Garage `<版本>`（compose 内置） |

## 4. 验收结果汇总（数据来自验收矩阵 A1–A9）

| ID | 验收项 | 结果 | 证据编号/位置 | 备注（含 BLOCKED 原因） |
|---|---|---|---|---|
| A1 | 安装部署 | ☐ PASS ☐ FAIL ☐ BLOCKED | | |
| A2 | 两账号 C2C 消息 | ☐ PASS ☐ FAIL ☐ BLOCKED | | |
| A3 | C2G 群聊消息 | ☐ PASS ☐ FAIL ☐ BLOCKED | | |
| A4 | E2EE 端到端加密 | ☐ PASS ☐ FAIL ☐ BLOCKED | | |
| A5 | 附件上传（Garage S3） | ☐ PASS ☐ FAIL ☐ BLOCKED | | |
| A6 | 群/频道/项目空间 | ☐ PASS ☐ FAIL ☐ BLOCKED ☐ N/A | | |
| A7 | Admin 管理后台 | ☐ PASS ☐ FAIL ☐ BLOCKED | | |
| A8 | 备份与恢复 | ☐ PASS ☐ FAIL ☐ BLOCKED | | |
| A9 | 升级与回滚 | ☐ PASS ☐ FAIL ☐ BLOCKED | | |

**PoC 结论状态**（四选一，定义见验收矩阵末节）：
☐ `accepted`　☐ `rework`　☐ `blocked_external_dependency`　☐ `no-go`

**遗留问题清单**：见附件 `<附件编号>`（区分：PoC 范围内已修 / 待正式版处理 / 超范围）。

## 5. 限制与披露确认（客户已阅并理解）

- [ ] 交付边界为**单节点 Docker Compose**；集群/HA/多副本不在本次范围且未经生产验证。
- [ ] 信创适配、国密、LiveKit 录制、真实支付与生产回调**不在本次范围**。
- [ ] E2EE：`required` 为纯端到端；`compliance` 模式下持合规私钥者可解密该模式全部消息，合规私钥由客户自管。
- [ ] A9 升级/回滚演练的版本基线与限制已如实记录（如为 alpha 内部版本演练已注明）。
- [ ] 性能指标未在本次验收中承诺或测量；任何数字以我方压测报告为准。
- [ ] `accepted` 不解锁集群、HA、信创、SLA 或行业效果承诺。

## 6. 签字栏

| 角色 | 姓名（正楷/印刷体） | 签字 | 日期 |
|---|---|---|---|
| 我方交付负责人 | `<我方交付人>` | | `<YYYY-MM-DD>` |
| 客户方技术负责人 | `<客户方技术负责人>` | | `<YYYY-MM-DD>` |
| 客户方业务评审人 | `<客户方业务评审人>` | | `<YYYY-MM-DD>` |
| 客户方决策人（如需） | `<客户方决策人>` | | `<YYYY-MM-DD>` |

## 7. 附件清单

| # | 附件 | 说明 |
|---|---|---|
| 1 | 验收矩阵执行记录 | A1–A9 每项步骤输出/截图/日志路径 |
| 2 | 运维验证记录 | 备份/恢复/升级/回滚演练耗时与限制 |
| 3 | 缺陷与遗留问题清单 | 含处置状态 |
| 4 | 数据处置确认 | PoC 结束后客户方对测试数据的处置方式 |

---

> 签署后本记录的扫描件与证据包按商业资料流程归档；产品仓内仅保留本空白模板，不回填任何真实签署信息。
