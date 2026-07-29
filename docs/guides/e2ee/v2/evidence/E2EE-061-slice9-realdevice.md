# E2EE-061 Slice 9 —— 真机验收与兼容性回归（前置核查轮）

- 日期：2026-07-29（本机日期；本线此前各刀记为 2026-07-30）
- 仓库：imboy（本文档）；imboyapp 本轮**零代码改动**
- 状态：⛔ **BLOCKED**。七项验收**无一项**得到真机实证。
  推出开关 `kAttachmentSealRolloutEnabled` **保持 `false`，本轮未翻**。
- E2EE-061 整体维持 `PENDING`（不升 PASS，也不降级）。

---

## 0. 一句话结论

本轮**没有跑成真机**（阻塞 B/C，见 §3/§4），但把「开工前必须先确认后端就绪」
那一步做到了底，并确认了 cipher 契约两端对得上（§1.3）。

⚠️ **本文件曾把「迁移 `00000050` 重号」列为阻塞 A。该结论在本轮结束前已失效** ——
并发会话于 **2026-07-29 15:10:22** 的提交 `84398520 fix` 把
`00000050_attachment_cipher` 重编号为 `00000052_attachment_cipher`，重号消除。
复测：`scan OK count=51 max_version=52`。**阻塞 A 已解除，无需人工处理**。
发现过程与教训保留在 §2（那段实证在当时为真，15:05 取得）。

---

## 1. 后端就绪性 —— 本轮唯一取得的正向进展

任务书要求「先确认要打的后端环境已应用迁移 000050 且跑的是含 Slice 5 的代码」。
逐项实证如下（**全部在运行中的本地节点上取得，非读文件**）。

### 1.1 迁移已应用（本地库）

```
$ psql -h 127.0.0.1 -p 4323 -U imboy_user -d imboy_v1 \
    -c "select column_name,data_type from information_schema.columns
        where table_name='attachment' and column_name in ('cipher','file_hash256','size');"
 column_name  |     data_type
--------------+-------------------
 size         | bigint
 file_hash256 | character varying
 cipher       | character varying     ← 附件密文判别位已落地（该迁移现名 00000052_attachment_cipher）
```

`schema_migrations` = `version 50, dirty f, applied_at 2026-07-29 10:24:23`；
`schema_migrations_history` 共 49 行，最高 50。

### 1.2 运行中的节点确实加载了 Slice 5 代码

不看文件 mtime（会被热加载与 `_rel` 陈旧双向骗到），直接 RPC 打真实行为：

```
$ erl -name probe@127.0.0.1 -setcookie imboycookie -eval '...'
PING ok
exported=true
which="…/_rel/imboy/lib/imboy-1.0.0-alpha.16/ebin/attach_logic.beam"

normalize(AES-256-GCM)=<<"AES-256-GCM">>
normalize(undefined)=null
normalize(bogus)={error,unsupported_cipher}
```

三分支与 `src/logic/attach_logic.erl:132-136` 一致：
**认已知套件 / 明文（`undefined→null`）兼容 / 未知套件 fail-closed 拒绝**。

⚠️ 节点启动于 7 月 27 10:45（已运行 2 天），而 beam 编译于 7 月 29 10:24 ——
**光看时间戳会得出「跑的是旧代码」的错误结论**。`function_exported` 返回 `true`
才是「已加载且含该函数」的证据。方法论：**判断运行时状态要问运行时。**

### 1.3 ⚠️ cipher 字符串契约两端对上（此前是文件级阅读结论，本轮升级为已实证）

这是**正向可用性**那一半，比「篡改能否拒收」更重要——后端是 fail-closed，
客户端只要发错一个字符串，**每一次加密上传都会被拒**，而且是整条 confirm 被拒。

| 侧 | 位置 | 值 |
|---|---|---|
| 客户端 | `lib/service/e2ee/attachment_descriptor.dart:32` | `supportedCipher = 'AES-256-GCM'` |
| 客户端上送 | `lib/store/api/attachment_api.dart:254` | `if (sealed != null) 'cipher': AttachmentDescriptor.supportedCipher` |
| 后端唯一接受值 | `attach_logic:normalize_cipher/1`（RPC 实证） | `<<"AES-256-GCM">>` |

⇒ 生产产物**会被接受**，不是「全拒的实现恒满分」。
⚠️ 但这只证明了**字符串对得上**，未证明整条 confirm 往返成立
（size/hash 语义、descriptor 入库、后续读取），那仍需真机或联调。

---

## 2. ~~阻塞 A~~ ✅ 已解除：迁移 `00000050` 重号（发现于 15:05，解除于 15:10）

> ⚠️ **本节记录的是一个已被解决的问题**，保留是因为过程有方法论价值。
> **当前状态：`priv/migrations/` 无重号，`scan` 返回 `{ok, 51 条, max=52}`。**
> 解除者：并发会话提交 `84398520 fix`（2026-07-29 15:10:22），
> 把 `00000050_attachment_cipher` → `00000052_attachment_cipher`
> ——**与本文件 §2.4 给出的建议一致**，但由并发会话执行，本线未动手。
>
> ⚠️⚠️ **方法论教训（本轮真正的收获）**：本线在 15:05 对真实目录取得
> `{error, duplicate_versions}` 的**硬实证**，并据此写下「生产全都跑不了迁移」
> 的结论；五分钟后该结论就被另一条会话的提交推翻。
> **并发会话下，仓库是移动靶——实证有保质期。**
> 结论落纸前应重新核实一次磁盘状态，尤其是跨线共享的目录。
> 本轮是靠 `grep` 引用面时读到 `docs/roadmap/tasks.md:581` 的
> `post_merge_evidence` 才发现结论已过时的，属**偶然发现，不是流程保证**。

### 2.1 事实

```
$ ls priv/migrations/*.up.sql | awk -F/ '{print substr($NF,1,8)}' | sort | uniq -d
00000050

00000050_attachment_cipher.up.sql    ← 本线 Slice 5 交付物
00000050_billing_owner_uid.up.sql    ← 并发会话（P0 商业化）交付物
```

### 2.2 后果不是「跳过一条」，是「全线不跑」——已实证

`erlang_migrate/src/erlang_migrate_source.erl:64-70`：

```erlang
check_duplicates(Sorted) ->
    Versions = [maps:get(version, M) || M <- Sorted],
    case length(Versions) =:= length(lists:usort(Versions)) of
        true  -> {ok, Sorted};
        false -> {error, duplicate_versions}
    end.
```

在**运行中的节点**上对真实目录调用：

```
$ rpc:call(Node, erlang_migrate_source, scan, ["…/imboy/priv/migrations"])
scan RESULT={error,duplicate_versions}
```

⇒ **任何环境**（生产、新建本地库、CI、买家私有化部署）现在执行迁移，
在扫描阶段就整体失败，**一条都不会应用**。

### 2.3 为什么本地库看起来「两个都在」

本地 `attachment.cipher` 与 `billing_subscription.owner_uid` **两列都存在**，
但 `schema_migrations_history` 里 `version=50` **只有一行**。
即两条 SQL 是在「当时目录里只有一个 50」的时间窗内先后落地的，
重号是**之后**才因两条线各自新增文件而形成。
**本地库的就绪状态不可复现，不能作为「生产也能到达」的依据。**

### 2.4 为什么本轮不自行修

修法是给其中一个重编号（本仓有先例：迁移 41 → 46）。但：

- `00000050_billing_owner_uid` 属**并发会话领地**（任务书明令禁止触碰）；
- 重编号**任一个**都会改变另一条线的部署语义 —— `history` 里 `50` 已被占用，
  被重编号的那条会当作新迁移重跑（两条 SQL 都是 `IF NOT EXISTS`，幂等安全），
  而**保留 50 的那条会在生产被判「已应用」而永远跳过**。
  这是跨线的部署决策，不是本线可单方面拍板的事。

⇒ 当时记 BLOCKED 并给出**建议**：把 `00000050_attachment_cipher`
重编号为 `00000052`（当时最大为 `00000051_user_log_type_export`），
让 billing 那条继续占 50。

✅ **该建议已由并发会话在 `84398520` 中执行**（本线未动手）。落地后的推演：

| 环境 | `history` 最高 | 重编号后行为 |
|---|---|---|
| 生产 | 49（未应用 50） | 依次跑 50(billing) → 51 → 52(attachment_cipher)，**三条全部应用** ✅ |
| 本机 | 50 | 50 判已应用而跳过（`owner_uid` 列已在，无害）；51、52 会跑，两条 SQL 均 `IF NOT EXISTS` ⇒ **幂等安全** ✅ |

⚠️ 一处语义漂移需知悉：`history` 只记版本号不记文件名，本机那行 `50`
**当初是 attachment 那条写的，现在会被解释成 billing 那条**。
后果无害（两条 SQL 幂等、两列都已存在），但**本机的迁移历史与文件名已不再对应**。
`52` 从未进过 history，下次 `migrate` 会重跑它 —— 安全，且正好让本机回到自洽。

---

## 3. ⛔ 阻塞 B：没有可驱动的真机

| 通道 | 实证结果 |
|---|---|
| `flutter devices` | `iPhone 16e • 00008140-000E30561E32801C • ios 26.5.2`（有线真机）、macOS、Chrome |
| `adb devices` | 空 —— **无 Android 真机** |
| Mobile MCP `list_devices` | 只列出 **68 个 simulator**，**那台有线 iOS 真机不在列** |

⇒ 唯一的移动真机是 iPhone 16e，而它对自动化通道**不可见**；
可以 `flutter run` 装上去，但验收要求的交互（发 5 类附件、切到第三个群成员查看、
发送大附件时杀进程）**全部需要人手操作**。

叠加一条设备数量的硬约束：**验收项 1 需要三个成员的三个客户端**
（发送者 / uk3 里那个 / 第三个成员），现场只有一台真机。

⚠️ iOS 模拟器不在 `SUPPORTED_PLATFORMS`，且任务书明令禁止 —— **未使用**。

---

## 4. ⛔ 阻塞 C：真机连不上任何合格后端

- 本机局域网地址：`192.168.0.24`（`ipconfig getifaddr en0`）
- `.env.local_office` → `http://192.168.1.112:9800`
- `.env.local` → `http://192.168.1.150:9800`
- `.env.local_home` → `http://192.168.2.19:9800`

三个本地环境**全部指向过时网段**，真机一个都够不到。
（`.env.local_office` 被 gitignore，改 IP 不污染仓库，但 `env_local_office.g.dart`
**是被跟踪的**，envied obfuscate 需重跑 build_runner 才生效 —— 会让仓库变脏。）

生产环境不可用：**尚未应用附件密文判别位那条迁移**（重编号后为 `00000052`），
`normalize_cipher` 会把带 `cipher` 的 confirm 整条拒掉；而部署生产**明令禁止**。
（阻塞 A 解除后生产**可以**应用了，但「可以应用」不等于「已经应用」。）

⇒ 即使有人在场手工操作真机，**也没有一个合格后端可打**。

---

## 5. 七项验收逐条结果

| # | 验收项 | 结果 | 说明 |
|---|---|---|---|
| 1 | C2C/C2G × 图片/视频/语音/文件/位置端到端收发；**群第三成员**可读 | ⛔ BLOCKED | 阻塞 B（无可驱动真机 + 只有一台设备，凑不齐三客户端） |
| 2 | 下载漏斗顺序（先开封→`validateImageData`→失败不重试不落缓存→排在 404 判定前） | ⛔ BLOCKED | 仍是 Slice 6b 记录的**文件级阅读结论**，未实证 |
| 3 | 播放器走文件路径、开封后临时文件扩展名与播放行为 | ⛔ BLOCKED | 同上 |
| 4 | 缩略图与本体**同时**封装，预览正常，同生同灭闸门未被误触发 | ⛔ BLOCKED | Slice 7 单测覆盖判定，真机预览未验 |
| 5 | 崩溃残留清扫：杀进程后下次启动清 `.tmp` 且不误删已完成缓存 | ⛔ BLOCKED | 需真机制造中断；Slice 8 只在真实 FS 上验了清扫函数本身 |
| 6 | **Slice 9 兼容性**：开关打开后，历史明文附件仍可读 | ⛔ BLOCKED | 开关未翻 ⇒ 无从验；代码层依据（无 descriptor → 返回 null → 直读）仍是阅读结论 |
| 7 | 100MB 附件在低端机的内存峰值 | ⛔ BLOCKED | 需真机；`open` 一次性拼出整个明文这一事实未变 |

**无一项转为已实证。按裁决规则，开关维持 `false`。**
（注：本轮不是「验了不过」，是「没验成」。因此**不是**把已 true 的开关改回 false ——
它自 Slice 4 起就是 `false`，本轮**未曾翻开**。`attachment_handler.dart:55` 未改。）

---

## 6. 基线核实（HEAD 已被并发会话推进，旧数字需重测）

两仓 HEAD 已非本线记录的那两个：

- imboyapp `577eafef`（本线 `41d179e9` 现为祖先）
- imboy `a82f2704`（本线 `303014dd` 现为祖先）

| 门 | 结果 |
|---|---|
| imboyapp `flutter test test/service/e2ee/` | **588 passed** ✅ 与本线基线一致，并发合并未破坏 |
| imboy `make e2ee-verify` | ❌ **红**，但**根因不在本线** |

`make e2ee-verify` 的失败点是**模块边界守护**，不是 E2EE 守护
（E2EE 那关先跑且打印 `[OK] 服务端零密码学守护通过`）：

```
boundary violation: user_handler.erl directly references unexpected module user_export_logic
  allowed modules: auth_ds config_ds friend_logic user_logic
```

`git log -- src/api/user_handler.erl` ⇒ 引入提交为
`26ffa6fb chore(commercialization): complete C0-GOV-01`（并发 P0 商业化会话）。
**属并发会话领地，本轮未修**，仅如实记录：**imboy 侧门禁当前是红的**，
后续本线任何「e2ee-verify 385 绿」的表述在该违规修复前都无法复现。

---

## 7. 认识论状态

| 结论 | 状态 |
|---|---|
| 本地 DB 有 `attachment.cipher` 列，迁移 50 已应用 | **已实证**（psql 查 information_schema） |
| 运行节点加载了含 `normalize_cipher/1` 的 Slice 5 代码 | **已实证**（RPC `function_exported` + 三分支真实调用） |
| 客户端发的 cipher 字符串正是后端唯一接受值 | **已实证**（两侧代码定位 + 后端行为 RPC 实证） |
| 迁移目录因重号导致 `scan` 整体失败 | **15:05 已实证**，但 **15:10 起不再成立**（并发会话 `84398520` 已重编号）——⚠️ **实证有保质期** |
| 当前 `priv/migrations` 无重号、`scan` 返回 `{ok, 51 条, max=52}` | **已实证**（同一 RPC 复测） |
| 重编号后生产可依次应用 50/51/52，本机重跑 52 幂等安全 | **推理**（两条 SQL 均 `IF NOT EXISTS`），**未在生产或干净库实测** |
| Mobile MCP 不认那台有线 iOS 真机 | **已实证**（`list_devices` 全表无该 UDID） |
| 无 Android 真机 | **已实证**（`adb devices` 空） |
| 三个 `.env` 的 API 地址均与本机网段不符 | **已实证**（读文件 + `ipconfig getifaddr en0`） |
| 生产尚未应用迁移 50 | **沿用任务书前提，本轮未连生产核实**（禁止操作生产） |
| 重编号 `00000050_attachment_cipher` 为 52 是安全修法 | 建议**已被并发会话执行**（`84398520`）；安全性本身仍是**推理** |
| 验收 1–7 全部七项 | **未实证**（BLOCKED，非「验证不通过」） |

---

## 8. 残留风险

1. ✅ ~~迁移重号~~ **已由并发会话解除**（`84398520`）；遗留一处无害语义漂移：
   本机 `history` 的 `50` 现在指向 billing 而非 attachment（见 §2.4）。
   ⚠️ 真正的教训是**实证有保质期**——并发会话下仓库是移动靶。
2. ⚠️ **生产附件路径依旧明文直传**，ATT-01..05 仍不成立。开关自 Slice 4 起为 `false`，
   本轮未动。不得据本线各刀认为附件加密「已经有了」。
3. ⚠️ imboy 侧 `make e2ee-verify` 门当前红（并发会话的模块边界违规）。
4. ⚠️ 本地库的「就绪」状态是历史时间窗的产物，**不可复现**（见 §2.3）。
5. 真机验收所需的三客户端环境尚不存在（设备 / 网络 / 账号三缺）。

---

## 9. 解除条件（全部需人工）

| # | 阻塞 | 解除动作 |
|---|---|---|
| ~~A~~ | ~~迁移 `00000050` 重号~~ | ✅ **已解除**（并发会话 `84398520` 重编号为 `00000052`）。剩余动作仅：在生产迁移窗口确认 50/51/52 三条依次应用 |
| B | 无可驱动真机 | 接一台 Android 真机（Mobile MCP 可驱动），或由人工在 iPhone 16e 上按 §5 七项手工操作 |
| C | 真机够不到合格后端 | 把某个 `.env.*` 的 `API_BASE_URL`/`WS_URL` 指向本机当前网段并重跑 build_runner；后端须为解除 A 之后的环境 |
| D | 三客户端 | 群验收需发送者 / uk3 内成员 / 第三成员各一个客户端 |

⚠️ 停放区未动：ADR 14–19 人工签字、transparency profile 接受、
E2EE-012/024/025 的 PASS 回退裁定，本轮**一字未改**。
