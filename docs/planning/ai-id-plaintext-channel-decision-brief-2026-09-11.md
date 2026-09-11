# AI-ID 平台 AI 明文通道身份决策包

日期：2026-09-11

状态：`CURRENT_IMPLEMENTATION_B / DECISION_RECORDED / AI_ID_B_SELECTED / RISK_ACCEPTED / RELEASE_NO-GO`

适用范围：仅 `account_type=1` 的平台 AI 与真人之间的 C2C。本文不适用于真人 C2C、C2G、`account_type=2` system bot 或 `account_type=3` developer bot。

本文是当前 AI-ID 决策工件。用户于 2026-09-11 书面确认 `F=F2，R=R2，D=D3，M=M1，AI-ID=B`，其中本文件记录 `AI-ID=B`；随后在收到该剩余风险的明确说明后回复原文 `确认接受`。据此，用户明确接受“恶意或被攻陷的运行时服务端可伪造 Agent 身份，并诱导进入非 E2EE 明文通道”的 AI-ID=B 剩余风险，治理状态记为 `RISK_ACCEPTED`。该接受不覆盖 D3 授权拆分或任何外部测试/发布授权。

## 1. 当前源码事实

1. `imboyapp/lib/service/e2ee/ai_plaintext_gate.dart` 已实现 AI-ID=B 形态：AI badge 只是必要条件，不能单独授权明文；用户确认绑定 deployment、owner UID、target UID、identity fingerprint 和 identity version。
2. 当前生产 identity fingerprint 是 `SHA-256(imboy.ai-peer.v1 | deploymentId | targetUid)`，`deploymentId` 来自 API base URL 指纹，identity version 固定为 0。它是服务端作用域派生标识，不是独立签名身份锚。
3. 消息、附件和重试共用 `AiPlaintextGate`；重试是非交互式路径，只能复用已有有效确认，不能自行弹窗授权。缺字段、存储错误、身份变化、账号切换或重试缺少有效确认均 fail-closed，转回 E2EE 或拒发。
4. 后端 `msg_c2c_logic` 在 `required/compliance` 下只对服务端权威 `ai_agent_ds:is_agent/1` 为真的目标接受明文；普通真人目标被拒，拒绝后不落库、不触发 Agent、不计费、不 Push。
5. `user/show` 返回的 `account_type=1` 只用于公开 AI badge，不能成为明文授权源。
6. App 已有 `DeviceManifest`、Ed25519 自签、identity cross-binding 和 Signed Capabilities 的模型与测试，但尚无完整生产上传、独立信任根分发、拉取和协商闭环，不能直接宣称 AI-ID=A 已具备。
7. 无论选择 A 还是 B，平台 AI 明文通道都不是 E2EE：后端和 AI 执行环境会看到用户主动发送给 AI 的内容。签名身份锚只能约束“哪个账号有资格触发明文通道”，不能让服务器对明文零知识。

当前本地回归只能证明 B 的实现防住已知 badge 污染、异步 TOCTOU 和客户端误判路径；它不构成用户风险接受，也不构成恶意服务端或真实传输/存储的 A 级证据。

## 2. 不可改变的安全不变量

- 真人 C2C 在 `required/compliance` 下必须 E2EE，未知身份不能降级明文。
- AI badge、昵称、头像、contact 缓存和未签名 API 响应永远不能单独授权明文。
- AI 明文能力只允许 C2C；C2G 继续使用 Megolm，不新增群级 AI 明文豁免。
- 明文授权必须发生在消息或附件离开设备之前；后端权威检查是二道门，不能替代客户端门。
- 用户拒绝、身份缺失、版本回退、签名失败、部署变化、账号变化或存储异常必须 fail-closed。
- UI 和对外材料必须明确写“发给平台 AI 的消息和附件不是端到端加密”，不得把 A/B 描述成 E2EE。
- 任何选项都不自动授权真实账号、真机、生产部署、密钥导出、流量抓取或第三方操作。

## 3. 方案定义

| 选项 | 行为 | 能解决什么 | 明确上限 | 工程影响 |
|---|---|---|---|---|
| **AI-ID=A 独立签名身份锚** | 客户端只接受由运行时 API 服务器之外的受信根签署、且绑定 deployment/agent UID/identity version/plaintext capability 的 Agent 身份清单；用户首次发送仍需显式确认 | 防止普通 API 响应或被污染数据库仅靠 `account_type=1` 把真人伪装成 Agent；身份轮换可使旧确认失效 | AI 内容仍为服务端可见明文；签名根或客户端被攻陷仍可失守；当前生产分发链未实现 | 新增受信根配置、签名/轮换/撤销、清单上传与读取、客户端验签和 A 级复测，属于独立安全项目 |
| **AI-ID=B 部署作用域确认** | 保留当前实现：服务端权威 Agent 行 + 本地 badge + 五元组用户确认；identity 由 deployment 与 target UID 派生 | 防止裸 badge 直接降级，隔离本机账号和部署，身份或绑定变化后重新确认 | 不抵御恶意/被攻陷服务端伪造 Agent 身份；version=0 不是真实轮换锚；只能作为显式风险接受 | 当前代码已本地实现，仍需用户书面接受剩余风险和真实链路 A 级复测 |
| **AI-ID=C 严格模式无明文豁免** | `required/compliance` 下平台 AI 与其他账号一样必须提交合法 E2EE；没有设备密钥就拒发。`disabled/optional` 的既有明文语义不变 | 删除严格 E2EE 模式的身份降级面，不需要新增签名身份基础设施 | 服务端平台 AI 无法读取 E2EE 内容，因此严格模式下当前 Agent 对话不可用；以后若需要，只能另立项做受控显式分享、可信执行环境或端侧 Agent | 删除/关闭 App 和后端的 AI 明文例外，保留 badge 展示；改动最小、边界最清楚 |

## 4. 已选择方案与历史推荐

2026-09-11 用户已选择 **AI-ID=B** 并明确接受上述剩余风险；当前代码行为与该方案一致，无需为本次选择修改运行时代码。真实链路 A 级复测仍为 `BLOCKED`。

本文此前推荐 C：当前路线是先收口 Agent Hub/Bot/E2EE/GA 安全，不再扩展大而全功能。A 会引入新的信任根、签名、轮换、撤销和部署配置面；B 要求明确排除恶意服务端身份伪造，并长期保留 `required/compliance` 的明文例外；C 直接让严格 E2EE 的名称、客户端行为和后端门一致，最容易形成可验证的发布边界。该推荐保留为决策历史，但不覆盖用户已选择的 B。

不要把 B 当作 A 的临时等价物，也不要用“已有 90/90 本地测试”替代身份根和真实链路证据。未来若选择 A，必须作为独立安全项目另行批准。

## 5. 各选项实施门

### 选择 A

实施前还必须确定：

- 信任根所有者：产品发布方、部署所有者或二者的明确组合；运行时 API 服务器自签不构成独立锚。
- 客户端信任根如何在安装或私有部署初始化时获得、更新、撤销和审计。
- 签名清单至少绑定 `schema_version/deployment_id/agent_uid/identity_key/identity_version/plaintext_capability/issued_at/expires_at/previous_hash/signer_kid`。
- identity version 单调不回退；过期、未知 signer、断链轮换和撤销状态全部 fail-closed。
- App 验签通过后仍需用户确认；后端仍需在落库前复核 active Agent 身份。
- A 级复测必须覆盖 API/database badge 污染、清单篡改、旧清单重放、版本回退、签名根轮换、账号/部署切换、附件和重试旁路。

### 选择 B

批准文本必须同时明确接受：

- 平台 AI 通道不是 E2EE，服务端和 AI 执行环境可见内容。
- 威胁模型不覆盖运行时服务端伪造 Agent 身份；当前派生 identity 只防客户端状态混淆。
- 每个 owner/deployment/target/identity 组合首次使用需要确认，任一绑定变化必须重新确认。
- 未完成真实账号、真实传输/存储、附件、重试和恶意服务端模拟复测前，仍不得发布 GO。

### 选择 C

实施只允许做最小收口：

- App 删除 `required/compliance` 下的 AI 明文授权分支；AI badge 继续只做展示。
- 后端删除 `required/compliance` 下 `ai_agent_ds:is_agent/1` 对明文写入的豁免。
- 消息、附件和重试在严格模式下统一 E2EE 或拒发；C2G 行为不变。
- 增加普通消息、附件、重试、伪造 badge 和后端 Agent/非 Agent 的零副作用回归。
- 不在本轮追加显式分享、TEE、端侧模型、Agent 设备密钥或新的身份基础设施。

## 6. 决策后的状态规则

- 只创建本文：`DECISION_BRIEF_READY`，不改变发布结论。
- 用户书面选择 A/B/C：`DECISION_RECORDED`，仍不是 `ATTACK_RETEST_PASS`。
- 选择项实现、定向回归与独立复核通过：最高 `LOCAL_REGRESSION_PASS`。
- 对应真实账号/设备/传输/存储攻击矩阵完成前：`A_LEVEL_ATTACK_RETEST=BLOCKED`。
- E2EE 其他门禁未完成前：`E2EE_RELEASE=NO-GO`，不得进入 GA。

## 7. 已记录决策与风险接受

```text
用户原文：确认 F=F2，R=R2，D=D3，M=M1，AI-ID=B
风险说明后的用户原文：确认接受
本文件生效：AI-ID=B
接受范围：恶意或被攻陷的运行时服务端可伪造 Agent 身份，并诱导进入非 E2EE 明文通道
治理状态：DECISION_RECORDED / AI_ID_B_SELECTED / RISK_ACCEPTED
```

`RISK_ACCEPTED` 只改变上述已知剩余风险的治理记录，不会自动把 `LT02-SEC-01` 标为 `ATTACK_RETEST_PASS/CLOSED`，更不会改变 `E2EE_RELEASE=NO-GO`。
