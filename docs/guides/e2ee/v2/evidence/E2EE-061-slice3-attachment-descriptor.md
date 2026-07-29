# E2EE-061 Slice 3 —— `attachment_descriptor` 结构 / canonical 编码 / 严格 parser

> **会话**：20260729-2330-claude-code ｜ **仓库**：imboyapp ｜ **状态**：Slice 3 完成，E2EE-061 整体仍 `PENDING`
> **授权依据**：用户放行「纯函数实施刀」（`30-...` §2 决策四）。本刀**不接线、不改协议版本、不加依赖**。

---

## 1. 做了什么

按 `27-...-design.md` §2.1 的字段清单，新增 `lib/service/e2ee/attachment_descriptor.dart`：
不可变值类 + `toMap`/`fromMap`（严格 parser）+ `toCanonicalBytes`/`fromCanonicalBytes`
（复用 PFv3 已在产的 `CanonicalCbor`，不引入第三套编码）。

**未做**：不接线（Slice 4/6）、不生成 content key、不选 `chunk_size`。

---

## 2. 三处需要说明的裁决

### 2.1 `chunk_count` 自洽闸门（本刀最有价值的一条）

`chunk_count` 进每块 AAD（Slice 2），但 **AAD 只保证「发送方当初声明的那个数」，
不保证那个数与文件本身自洽**。也就是说：一个从头到尾就按 3 块封装、
却实际只有 3/4 内容的附件，光靠 AAD 是拦不住的——每块都验得过。

因此 parser 强制 `chunk_count == expectedChunkCount(plain_size, chunk_size)`。
闸门不是「只准一种取值」：三者同步改动仍然接受（已有正向用例）。

### 2.2 空文件取 1 块而非 0

`plain_size == 0` 时 `chunk_count` 必须为 1。取 0 意味着**一个字节都没有被认证**，
空附件的 AEAD 保护形同虚设。空验证 G（改成返回 0）已使该用例精确变红。

### 2.3 `toString` 抹掉 `content_key` / `base_nonce`

HOTFIX-01 的教训是日志/异常里不得出现消息正文。descriptor 里躺着的是
**能解开整个附件的密钥**，一次 `'$descriptor'` 就足以把它写进日志。
`toString` 保留可诊断的非敏感字段并输出 `<redacted>`，两条用例分别钉「不泄漏」与
「仍可诊断」（后者否掉「toString 返回空串」这种恒过实现）。

### 2.4 不做套件协商

`cipher` 只接受 `AES-256-GCM`。多一个可选值就多一条降级路径；
用例里显式钉死 `AES-128-GCM` 与 `none`（"明文伪装成一种套件"）均拒收。

`maxPlainSize` 直接引用 `AttachmentApi.maxUploadBytes`（已核实存在于
`lib/store/api/attachment_api.dart:53`，值 100MB），**不另立一套数字**。

---

## 3. 空验证（negative control）

七条，每条摘掉一处闸门，**全部精确变红且无正向用例塌方**：

| 空验证 | 手法 | 变红 |
|---|---|---|
| A | 接受未知字段 | 1 条 —— 唯独「未知字段 → 拒收」 |
| B | 关掉 `chunk_count` 自洽 | **4 条** —— 谎报少/多一块、改 plain_size、改 chunk_size |
| C | 允许 thumb 复用主体 content_key | 1 条 |
| D | 取消 cipher 钉死 | 2 条 —— 主体 + thumb 各一（证明 thumb 走同一套校验） |
| E | 把 content_key 放回 `toString` | 1 条 —— 泄漏用例 |
| F | 取消 content_key 长度校验 | 1 条 |
| G | 空文件返回 0 块 | 1 条 |

恢复后 43/43 全绿，无漂移。

> ⚠️ D 变红两条这一点值得单记：它同时证明了**缩略图不是"顺带塞进去的字段"**，
> 而是走完全同一套严格校验——设计 §3.3 要求缩略图必须同等加密，
> 若 thumb 走一条宽松分支，这条就只会红一次。

---

## 4. 覆盖矩阵

| 组 | 条数 | 覆盖 |
|---|---:|---|
| 1 正向可用性 | 5 | map 往返（含 thumb）、CBOR 往返逐字段、编码确定性、无 thumb 不写 null 占位 |
| 2 chunk_count 自洽 | 7 | 整除/非整除/空文件对照组 + 谎报少一块（截断）/多一块/改 size 不改 count（×2）+ **三者同步改动仍接受** |
| 3 严格 parser | 16 | 11 个必填字段逐一缺失 + 未知字段 + int↔String 双向类型 + thumb 类型 + 非法 base64url |
| 4 安全字段边界 | 9 | key/nonce/hash 三处长度、cipher 两个反例、四个空串、负 size、超上限、chunk_size<1 |
| 5 缩略图独立性 | 5 | 正向 + 复用 key/nonce + 嵌套 thumb + thumb 自身受同等校验 |
| 6 日志泄漏 | 2 | 不泄漏（三种形态）+ **仍可诊断** |

---

## 5. 验收

```
flutter test .../attachment_descriptor_test.dart → All 43 passed
flutter test test/service/e2ee/                  → All 471 passed（上轮 428，+43）
flutter test test/service/                       → All 1351 passed（上轮 1308，+43）
dart analyze lib                                 → 1 issue（既有 info，ios_settings_ui.dart）
```

imboy 侧仅文档改动，`make e2ee-verify` 不适用。

---

## 6. 残留风险

1. ⚠️ **未接线** —— 生产附件路径至今明文直传，ATT-01..05 全部不成立。
   本刀与 Slice 2 都只是纯函数，**不得据此认为附件加密"已经有了"**；
2. **`chunk_size` 仍未拍板**（设计 §6）—— parser 只校验「≥1 且不超过上传上限」，
   **不设默认值、不建议取值**，未预支该决定；
3. **descriptor 与实际密文对象未做交叉校验** —— parser 只保证 descriptor **自洽**，
   「声明的 object_key 真的存在」「密文对象真的是 chunk_count 块」属 Slice 6 的
   下载侧完整性门，本刀不涉及；
4. **`plain_sha256` 只校验长度，未校验它真的是明文哈希** —— 那要等下载侧解密完
   才能验（Slice 6）；
5. **`mime` / `name` 只校验非空** —— 设计 §3.2 指出隐藏 MIME 需要改整个
   presign/confirm 契约（Slice 4/5），本刀不碰契约，因此这里的 mime 只是**承载**，
   不代表服务端已经看不到它；
6. **未做恒定时间比较** —— `_sameBytes` 用于「thumb 是否复用主体密钥」这一自检，
   两边都是本端自己的值，不构成对外 oracle；但它**不是**恒定时间实现，
   不得被复用到比对外部输入的场景。

---

## 7. 认识论状态

| 结论 | 状态 |
|---|---|
| 七处闸门各自生效且互不遮蔽 | **已实证**（空验证逐条精确变红） |
| thumb 走与主体完全同一套严格校验 | **已实证**（空验证 D 同时红两条） |
| `AttachmentApi.maxUploadBytes = 100MB` 存在 | **已实证**（`attachment_api.dart:53`） |
| 本刀对生产附件路径的影响 | **零** —— `lib/` 内零个 import 指向本模块 |
| descriptor 与真实密文对象的一致性 | **未实证**（属 Slice 6） |

---

## 8. 未做

- 不接线、不改协议版本、不改 ADR、不改任何既有任务的状态标记。
- 不 push、不部署、不访问生产、不通知第三方。
