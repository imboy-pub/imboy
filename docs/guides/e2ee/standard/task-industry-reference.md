# Task ↔ Industry Reference：任务行业依据映射表

> **层**：计划层配套 ｜ **日期**：2026-08-01
> **用途**：Hardening Plan 每个任务的外部依据。立项评审时回答"这项任务为什么存在"；标准升版时回答"哪些任务受影响的逆查表"。
> **计划真源**：`.claude/PRPs/plans/e2ee-top-tier-hardening.plan.md`（工作区，非 git）

## P0 收口与解冻

| 任务 | TT 需求 | 行业依据 | 一句话理由 |
|---|---|---|---|
| P0-1 ADR 签字门 | B1/B3/E4（解锁前提） | 审计惯例"范围锚定需决策人确认"（public-audit-cases §3.1） | 协议变更未签字=审计范围无法锚定 |
| P0-2 状态机裁定 | C5 | vodozemac Issue J Resolved/Acknowledged 状态机先例 | 失效 PASS 必须诚实回退，台账不许粉饰 |
| P0-3 推送决策 | C6 | 门禁前提：代码不到 canonical remote，CI 无从谈起 | 工程收口，无直接行业对应 |
| P0-4 CI 复活 | C6 | complement-crypto GitHub Action 模式 | 加密套件 red 不可合入是行业地板 |
| P0-5 生产部署 | E4/E1 | WhatsApp/Signal 设备密钥服务可用性惯例 | pro 无端点=一切真机验收的前提缺失 |
| P0-6 AGPL 拍板 | D3 | vodozemac README 审计状态声明惯例；AGPL 分发义务 | 许可证不清=商业化法律阻断 |
| P0-7 基线冻结 | D1/D3 | NCC"范围与版本锚定"惯例 | 验收必须有冻结基线否则移动靶 |
| P0-8 跨线登记 | E2 | XFF 最左=攻击者可控（限流绕过通识） | OTK 限流有效性依赖别线修复，登记不越界 |

## P1 验收可信度重建

| 任务 | TT 需求 | 行业依据 | 一句话理由 |
|---|---|---|---|
| P1-1 counter 语义重验 | A6/C3 | ETH 七攻击之重放/反射；ADR 26 定案选项 C | 生产链路当前整条拒收=功能不通 |
| P1-2 E2EE-012 重验 | A6/C3 | "全拒实现恒满分"方法论教训（ToB/审计界对 mutation testing 的双向要求） | 篡改拒收+未篡改收下必须双向证明 |
| P1-3 E2EE-024 重验 | A6 | 同上 | context binding 在真实入口重验 |
| P1-4 E2EE-029 重验 | A1/B5 | Sesame 多设备参照 | fan-out 只取本机信封的多设备矩阵 |
| P1-5 outbox 残留 | A1/C4 | complement-crypto 进程操纵恢复测试 | ratchet+outbox 原子性=崩溃安全底线 |
| P1-6 本地双端实证 | C2 | complement-crypto 架构（降级版） | 真机前的最后全链路防线 |

## P2 真机验收矩阵

| 任务 | TT 需求 | 行业依据 | 一句话理由 |
|---|---|---|---|
| P2-0 环境就绪 | C2 | complement-crypto 真实客户端原则 | 模拟器/旁路结果不算验收 |
| P2-1 PFS 攻击测试 | A1/C3 | Least Authority 审计方法（at-rest 取证） | PFS 必须用攻击方式证明 |
| P2-2 附件开关灰度 | A6/D6 | DAVE 规范附件加密+ToB key-committing 发现 | 明文直传是当今最大实际暴露面 |
| P2-3 room-key 跨平台 | A3 | Matrix Megolm 轮换规则 | 不把 RSA fallback 冒充 Olm（铁律） |
| P2-4 多设备集成 | B5/C2 | Sesame；Element 设备管理 | 2 用户×3 设备是行业惯例矩阵 |
| P2-5 旅程走查包 | B4/E4 | Matrix 4S 恢复旅程；WhatsApp 设备管理 | logout/备份/换机是用户可感知底线 |

## P3 能力缺口补齐

| 任务 | TT 需求 | 行业依据 | 一句话理由 |
|---|---|---|---|
| P3-1 Megolm 入备份 | B4 | Matrix megolm key backup 模型；4S/SSSS | 换设备群历史全灭=最大用户可感知洞 |
| P3-2 API fail-open | E1/C5 | "静默失败是安全功能头号敌人"（审计界通识） | backup/info 误报"无备份"直接误导恢复决策 |
| P3-3 吊销级联 | E4 | Signal/WhatsApp 设备吊销即时生效惯例 | 被吊销设备仍可被 claim=吊销形同虚设 |
| P3-4 cross-signing | B1 | Matrix 交叉签名；2022 Matrix 实战漏洞（跨签名绕过=反面教材） | 顶级可验证性的标志性能力 |
| P3-5 SAS UX | B1 | Signal safety number；Element emoji 验证流 | 算法在零 UI=能力不存在 |
| P3-6 PCS | A4 | CRYPTO 2020；PQ3 Tamarin 属性 | 泄露后自愈是顶级与及格的分水岭 |
| P3-7 062 残留 | C3/E2 | complement-crypto OTK 耗尽类目 | 各半边分别实证≠端到端成立 |
| P3-8 KT 实施 | B3/B6/E3/E5 | WhatsApp AKD+Plexi；Signal auditor；RFC 6962；IETF keytrans | 服务端可审计性=独立验证的终极形态 |
| P3-9 policy TTL | E2 | 配置漂移=降级窗口（通识） | strict 升级后缓存 plaintext=降级 |
| P3-10 Megolm 完备 | A3 | RFC 9420 成员变更成本对照 | 大群全量重发不可持续 |
| P3-11 device-bound session | E4 | WhatsApp/Signal 设备-会话绑定 | token 不带 did=设备维度授权缺失 |

## P4 自动化测试体系

| 任务 | TT 需求 | 行业依据 | 一句话理由 |
|---|---|---|---|
| P4-1 CI 硬门 | C6 | complement-crypto Action 模式 | 见 P0-4，本项是持续运营化 |
| P4-2 双端 harness | C2/C4 | complement-crypto（行业最高水位，公开可抄） | 零跨进程测试=双端正确性无证据 |
| P4-3 golden 体系 | C1 | 2key-ratchet；官方 KAT 包不存在=双实现互验是正解 | 编码层错误只有向量能抓 |
| P4-4 对抗闭环 | C3 | ETH 七攻击；2022 Matrix 攻击路径 | 6❌ 逐项关闭 |
| P4-5 故障注入 | C4 | complement-crypto 进程操纵；G5 可靠性条目 | 10000 次 0 违规是 GA 硬指标 |
| P4-6 零知识证明 | E1 | NCC 审计范围惯例（服务端不接触明文私钥须验证非自述） | "零知识"从话术变可执行 |
| P4-7 harness 提速 | C6/D7 | CI 可用性前提 | >40min 的套件=不存在 |
| P4-8 覆盖率 | C5 | 项目自身标准（Repo80/Logic70/Handler60） | 覆盖率是审计方第一问 |
| P4-9 回归索引 | C5 | vodozemac 修复+测试先例 | 每个历史 bug 都要有名字 |

## P5 审计就绪包

| 任务 | TT 需求 | 行业依据 | 一句话理由 |
|---|---|---|---|
| P5-1 威胁模型 | D2 | Apple PQ3 Level 0-3；ETH 三模型 | 审计方第一份索要的文档 |
| P5-2 密码学清单 | D3 | DAVE ciphersuite 声明；NCC 版本锚定 | 含 AGPL 状态=商业化前提 |
| P5-3 白皮书 | D1 | Signal specs；Threema 白皮书 | 第三方仅凭文档可互操作是标尺 |
| P5-4 生命周期 | D4 | WhatsApp 备份白皮书；Apple CKV | 密钥从生到灭全链路 |
| P5-5 台账 | D6 | vodozemac Issue J；Threema ETH 回应页 | Acknowledged 是合法状态，隐瞒才致命 |
| P5-6 SOW 模板 | D5/D7 | 审计交付物 8 条惯例 | 第三方凭此报价=就绪的操作定义 |
| P5-7 自审计报告 | D5 | 公开报告结构（范围/findings/严重度/retest） | 自己先按行业标准过一遍 |
| P5-8 manifest 机制 | D7 | SBOM/依赖锁定惯例 | 证据可复现=证据存在 |
| P5-9 披露政策 | E6 | matrix.org/Threema 披露页 | 商业化信任基础设施 |

## P6 验收门禁与灰度

| 任务 | TT 需求 | 行业依据 | 一句话理由 |
|---|---|---|---|
| P6-1 GA 修正 | 全部 | ADR 31（本体系上位决策） | 顶级按属性不按协议名 |
| P6-2 bar→执行映射 | 全部 | 本体系 evidence-matrix.md | 每条标准必须有可点检的核验方式 |
| P6-3 灰度计划 | C6 | 行业灰度惯例（停止条件>0 即停） | 加密功能灰度=安全事件保险 |
| P6-4 红队演练 | C3 | ETH/Matrix 攻击目录实战化 | 纸面防御≠有效防御 |
| P6-5 runbook 演练 | E5/E6 | 运维审计惯例（KT 分叉处置先例=CT 生态） | 事故时现学=事故扩大 |
