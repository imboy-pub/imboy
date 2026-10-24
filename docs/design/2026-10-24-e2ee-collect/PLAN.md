# E2EE 独立收藏 — 权威方案 v7.1

> 状态：**M1 实施中（2026-10-24 授权）；staging/生产迁移/存量清洗仍需另行确认。**
> 决策已录入：D1 source/remark/tag 入密（§7.3 同步）；D2 留 M2；D3 T3 保留不实施；S 默认不做。
> v7 = v6 + 四项矛盾修正：①放弃"零迁移"——B0 迁移补 `generation`/`key_version` 列与 `user_collect_key_state` 高水位表，复活=复用原行（§7.3）；②`generation` 统一为记录修订号，重包裹等**一切持久化变更都 +1**（§7.3）；③legacy 接口对 sealed 行加守卫，禁止物理删墓碑/明文改列/绕过版本控制（§9）；④key_version 门改为高水位行锁串行化，只升不降（§7.2）。
> v6 = v5 + 四项实现约束（CK 批准前置、并发/删除语义、CKv1 清除状态机、加密接口契约）+ 两处验收修正 + §14-16 落地清单/测试向量/验收安排。

## 1. 目标与非目标

**目标**：收藏具备独立保存价值——内容对服务端不可解；按 §5 声明的条件，清空聊天、删除原消息、换机后仍可恢复、渲染、播放、转发。

**非目标**：历史暴露的追溯性消除（§11）；群附件退群后字节可得性（M2，D2）；止血方案 S。

## 2. 数据边界不变量

1. **网络上送**：唯一暴露面，处理只作用于请求构造处拷贝（copy-on-write）；
2. **本地收藏副本**：全量 info（含 descriptor）落本地 SQLite，合法恢复材料，上送侧不得改动；
3. **原消息行**：不由收藏流程修改。

## 3. 已验证事实基线（file:line 实证）

- info 落库为服务端持钥 AES-256-GCM 密文（`elib_hasher.erl:37-46,106-108`）；到达即服务端可解。
- 附件授权（`attach_logic.erl:929-960`）：public=任何人；private=仅上传者；**c2c=会话双成员（与消息存在无关）**；group=活跃成员+世代内（`attachment_repo.erl:607-641`，退群失权）。
- 字节保留由 `attach_file_hash256` 引用豁免保证（00ec9eaf，逗号守卫+触发器减计数）。
- 消费路径：语音详情经 `toTypeMessage()` 注册（`user_collect_provider.dart:351-378`，跨设备可播**待验证**）；视频/图片直接渲染（隐性失败）；收藏转发源 id 被覆盖现状即失败（`user_collect_detail_page.dart:170`+`send_to_provider.dart:221/267`）。
- 旧后端 add 收 map info（`user_collect_handler.erl:119-135`）；sealed 落旧后端实证丢失 GC 保护。
- 旧端读 sealed：空预览/失败占位（`user_collect_provider.dart:356-364`），待实测。
- 能力面：`app_manifest_handler` features 可挂 `e2ee_collect_v1`。
- C2C `skipped_devices` 不重投（`chat_network_service.dart:1035-1130`）；新设备无历史副本。
- **schema 事实（v7 取证，推翻 v6"无需迁移"）**：`user_collect`（migration 1:2878-2916）无 `generation`/`key_version` 列；存在**两个**唯一约束 `uk_user_collect_uid_kind UNIQUE(user_id, kind_id)`（**跨 status**）与 `uk_user_collect_userid_status_kindid UNIQUE(user_id, status, kind_id)`；status CHECK 允许 -1/0/1。
- **现有接口行为（v7 取证）**：`remove`=物理 `DELETE FROM user_collect WHERE user_id AND kind_id`（`user_collect_repo.erl:52-59`，无 status 条件）；`change(remark)`=直改 remark 明文列（`user_collect_logic.erl:169-177`，无守卫）；`add`=upsert，冲突目标仅 `(user_id, status, kind_id)`，命中 `uk_user_collect_uid_kind` 时**不**被 ON CONFLICT 覆盖而是 SQL 异常（`user_collect_logic.erl:371-383`）——墓碑行存在时 legacy add 现状即 500。
- **结论**：CAS/墓碑/单调门均需 B0 迁移（§14-B0）支撑；"零迁移"声明作废。

## 4. 分层模型

记录层（M1）：内容客户端加密；字节与授权层（M2）：群授权/恢复短语/D2。

## 5. M1 范围与验收

**能力声明（条件化，端到端验收通过后方为结论）**：
1. 新收藏内容服务端不可解（§8 范围）；
2. C2C 来源：删除原消息后字节仍在且可下载 → 可恢复/渲染/播放（待验证）/转发（随绑定修复）；
3. 换机/新设备：经 CK 分发（§6）后同上；
4. 群来源：记录层加密生效；退群后字节不可下载（M2）。

**不承诺**：全部旧设备丢失后的恢复（§6.6）；群退群；历史暴露消除（§11）。

**验收条件**：
- A1 请求面：抓包除 §7 声明的可见元数据外无明文内容/密钥；
- A2 存储面：DB 全列+应用密钥齐备解不出 sealed；`attach_file_hash256` 与消息附件密文哈希一致且非空；
- A3 响应面：列表/详情响应无额外明文或密钥副本；
- A4 C2C 真机端到端：收藏 →（a）删本地消息行（b）服务端撤回 → 清缓存冷启动 → 删本地副本仅凭服务端 sealed 恢复 → 渲染/播放/转发（语音跨设备待验证）；
- A5 数据边界不变量断言；
- A6 旧端读 sealed：失败占位，实测记录形态；
- A7 能力协商：旧后端组合拒绝创建+升级提示，无静默回退（实测）；
- A8 GC 端到端（staging）：构造孤儿候选并触发 `attach_cleanup_logic` 扫描，被引用行与对象存活——sealed 路径确实建立保留引用；
- A9 引用对账：创建/编辑（引用变化）/取消三操作后 `referer_time` 增减对称；
- **A10（修正版）nonce 审计**：测试期间对**每一次 seal 操作**记录密钥域标识与 nonce（`kind_id/key_version/nonce` 三元组，不含密钥与正文，经操作日志采集）；事后检查全部记录在同一密钥域内无重复——**不依赖最终库中密文**（最终行只剩最新密文，抽查最终行无法覆盖编辑过程）。随机并发测试用于发现实现错误，**不构成永不碰撞的证明**；安全性依据是 §8.3 的密钥域收窄+量级论证，测试是缺陷检测。

## 6. CK（收藏密钥）设计

6.1 **形态**：每用户 CK（256-bit）+ 版本链 CKv1/v2…；本地安全存储；服务端永不持有。
6.2 **批准前置的设备入组流程（v6 重写，替代 v5 §6.4）**：
```
① 新设备 → 发 collect_key_enroll_request{device_did, device 指纹摘要}
   （经自发 C2C 通道，当前在线的全部确认设备可见）；
② 可信旧设备核验目标设备身份（设备指纹 + 登录来源比对）→ 用户在【旧设备】上批准；
③ 批准后旧设备【发送前重查授权】：目标设备仍在当前设备清单、未被撤销
   （权威清单为服务端 user_device 域）→ 通过才以目标设备会话包裹 CK 发送 collect_key_sync；
④ 新设备存储后回 ACK——**ACK 仅代表确认接收，不构成授权**；
⑤ 拒绝=不发送；enroll 重试每次重新执行 ②③ 授权检查；撤销设备不再接收新 CK。
```
6.3 **确认与重试**：ACK 指数退避重发；未 ACK 设备标记 pending，不阻塞其他设备。
6.4 **轮换边界**：撤销设备已持 CKv1 与已见明文**不可回收**；CKv2 保护轮换后所有新 seal；旧收藏经急迫重包裹迁移（重包裹同样 generation+1，§7.3）；已升级记录拒绝回退 v1 密文（§8.2）+ **服务端高水位门拒绝低于 max_key_version 的写入（§7.2，权威数据源 `user_collect_key_state`）**——堵住离线掉队设备的迟到 v1 写入。
6.5 **轮换状态机与 CKv1 清除条件（v6 重写）**：
```
状态（持久化于密钥链元数据，中断可恢复）：
NORMAL(vN) → ROTATING（本地生成 CKv2，即刻停止用 vN seal——旧版本写入门槛）
  → REWRAP_RUNNING（分页遍历+断点续跑；每条重封用 CAS 写入，冲突则重读重封）
  → REWRAP_VERIFY（全量复扫，断言 0 条依赖 vN 的记录；发现掉队 vN 行则回到 RUNNING）
  → PEERS_CONFIRMED（其余确认设备均 ACK 已切换：本地门已生效）
  → CLEAR_vN
```
**清除 vN 的全部前置（缺一不可，后台任务跑完不构成依据）**：
(a) VERIFY 通过：全量复扫 0 条 vN 记录；
(b) CKvN+1 已安全持久化于本设备**且至少一台其他确认设备回报已持久化**（单设备丢失不致失钥）；
(c) 全部确认设备 ACK 切换完成（离线设备未 ACK 则维持 REWRAP/保留 vN，不得清除）；
(d) 服务端 key_version 高水位门已在 §16-F4 类验证中生效（迟到 vN 写入会被服务端拒绝；权威数据源 `user_collect_key_state`，§7.2）。
未满足 → 无限期保留 vN（保留无害，清除是优化）。任何 vN 行再现（理论仅剩服务端拒绝前的窗口）→ 重跑 REWRAP。
6.6 **全部旧设备丢失不可恢复矩阵**：无 M2 恢复短语时，全部 key version 的 sealed 收藏永久不可读；尚存一台持最新 CK 设备：其 key version 记录可恢复；更低版本记录依赖 §6.5(b) 的跨设备持久化冗余与已完成的清除门槛。撤销设备保留旧钥属暴露而非数据丢失。
6.7 **logout / 换号生命周期（2026-10-24 实施补记）**：CK 版本链是账号级秘密，持久键落 C05 秘密清单 `collect_ck_` 前缀（`E2eeSecretInventory` 条目 `collect-keychain`），logout/换号经 `purgeAll` fail-closed 清理；`CollectKeychain` 单例内存副本同步经 `clearMemoryCaches` 丢弃——缺此一步，换号后 `load()` 命中缓存会把前一账号的 CK 交给下一账号（已由 `collect_keychain_purge_test` 固化为回归守卫）。purge 后本设备无 CK，重新登录须经 §6.2 跨设备同步取回，**无本地降级路径**；全部设备均 purge 等价于 §6.6 丢失矩阵。

## 7. 上传结构 v1 字段表

| 字段 | 明文/密文 | 说明 |
|---|---|---|
| `ver` | 明文 | 进 AAD |
| `kind`、`kind_id` | 明文 | upsert 键；进 AAD |
| `key_version` | 明文 | 行级列（B0 新增，NULL=legacy 行，sealed 判据 `key_version IS NOT NULL`）；进 AAD；**服务端按用户高水位门（§7.2）** |
| `generation` | 明文 | **记录修订号（B0 新增列）**：一切持久化变更（创建/编辑/墓碑/复活/重包裹）都必须 +1；CAS 与防回放核心，也是客户端同步检测的唯一信号（§7.3） |
| `expected_generation` | 明文 | 条件更新的期望值（§7.3） |
| `attach_file_hash256[]` | 明文 | GC 引用；**摘要进 AAD（§8.2）**；服务端 §7.2 校验 |
| `created_at/updated_at/status` | 服务端生成 | — |
| `info_sealed` | 密文 | 完整 payload（含 source/remark/tag，D1） |

### 7.2 服务端校验（GC 引用与写入门）

- **引用校验**：存在性（哈希须解析到现存 attachment 行，任一失败整单拒绝）+ 授权归属（请求者按 §3 规则对每行有权限）+ 数量上限（9 图+单视频/文件口径）；
- **key_version 高水位门（v7 重写，替代 v6"行聚合最大值"）**：
  - 数据结构：新表 `user_collect_key_state(user_id bigint PRIMARY KEY, max_key_version integer NOT NULL DEFAULT 0, updated_at timestamptz NOT NULL DEFAULT now())`（B0 迁移创建）；
  - **串行化机制**：一切 sealed 写路径（v1 创建/编辑/重包裹/墓碑/复活，以及 §9 legacy remove 的墓碑化）在**同一事务内、操作 user_collect 行之前**执行
    `INSERT INTO user_collect_key_state(user_id, max_key_version) VALUES($1, 0) ON CONFLICT (user_id) DO UPDATE SET updated_at = now() RETURNING max_key_version`
    ——INSERT ON CONFLICT DO UPDATE 对冲突行取行锁，效果=按用户取写锁，天然串行化同用户的全部 sealed 写；
  - **门判定**：返回值 `hw` 满足 `提交 key_version >= hw` 方可继续；`提交 key_version > hw` 时同事务推进 `UPDATE user_collect_key_state SET max_key_version = 提交值 WHERE user_id = $1`；
  - **高水位只升不降**：不依赖 user_collect 行聚合——行被删除、墓碑被未来清理任务物理清除，均不影响门；`user_collect_key_state` 行不随收藏删除；
  - **锁顺序约定（防死锁）**：全路径统一"先 key_state 行，后 user_collect 行"，与 CAS 的行 UPDATE 构成一致顺序；
  - **失败用例（v6 方案的穿透路径，v7 必须测试不重现）**：
    (a) *并发穿透*：T1 读聚合 max=1（快照）→ T2 轮换写 v2 行提交 → T1 以 v1 通过检查提交 → 库内出现 v1 < 实际 max=2 的行，门失效。REPEATABLE READ 下聚合读是快照读，不阻塞写入，v6 方案无法阻止；
    (b) *持久性*：用户仅有的 v3 行被删（物理清除/admin 操作），聚合 max 回落 2 → 迟到 v2 写入重新被接受。v7 高水位独立存续，两类路径都被行锁串行化与只升不降阻断。
- **三操作一致差分（v7.1 修订：触发器为唯一权威，机制见 §7.4）**：创建=校验后逐行 +1；编辑=同事务对引用差集 −1/+1；取消=墓碑（§7.3）+ 触发器语义减计数；A9 对账。

### 7.4 引用计数触发器权威机制（v7.1 新增，堵 v7 漏洞）

**漏洞（实施取证）**：`imboy_user_collect_fun`（migration 26 终版）**UPDATE 分支直接 RETURN NEW、无任何差分**（原文注释"业务上不会有单独修改哈希的可能性"）；触发器仅在 `UPDATE OF attach_file_hash256` 且 SET 子句含该列时触发；+1 现状在应用层 `attachment_repo:save`（ON CONFLICT (path) referer_time+1）。⇒ v7 的墓碑化/复活/编辑（均为 UPDATE，不改 attach 列）**全部不会引起 referer_time 变化**：取消收藏不减计数（引用虚高，不误删）、**复活不加计数**（引用虚低 → 附件可能被 GC 误判孤儿删除 → sealed 收藏字节丢失）。

**修正**：迁移 227 `CREATE OR REPLACE imboy_user_collect_fun()`，UPDATE 分支改为**差集差分**（第 6 次函数替换，遵循 220/221/226 惯例）：
```sql
ELSIF (TG_OP = 'UPDATE') THEN
  IF (OLD.attach_file_hash256 IS DISTINCT FROM NEW.attach_file_hash256) THEN
    UPDATE public.attachment SET referer_time = referer_time - 1
     WHERE file_hash256 <> '' AND file_hash256 = ANY(string_to_array(OLD.attach_file_hash256, ','))
       AND NOT (file_hash256 = ANY(string_to_array(NEW.attach_file_hash256, ',')));
    UPDATE public.attachment SET referer_time = referer_time + 1
     WHERE file_hash256 <> '' AND file_hash256 = ANY(string_to_array(NEW.attach_file_hash256, ','))
       AND NOT (file_hash256 = ANY(string_to_array(OLD.attach_file_hash256, ',')));
  END IF;
  RETURN NEW;
```
（差集而非"全减全加"，保留引用在值不变/部分重叠时不重复计数；`<> ''` 防空串匹配；INSERT/DELETE 分支与 26 终版逐字节一致，down 恢复 26 版。）

**v1 全生命周期计数规则（单点权威，防双计；v7.1 实施修订：v1 客户端只上送哈希、无 path/url 元数据，无法走 legacy 的应用层 save——全部分派给触发器）**：
| 操作 | attach_file_hash256 列 | 计数效果 |
|---|---|---|
| v1 创建 | INSERT 行=新值（key_version 非空） | **触发器 INSERT 分支条件 +1**（仅 NEW.key_version 非空时；legacy 行 INSERT 保持无操作，+1 归应用层 save，行为不变） |
| v1 编辑（引用差集） | UPDATE 行=新值 | **触发器差分**（−差集/+差集）；**不调 save**（附件行已确认，调用即双计） |
| v1 墓碑 / legacy remove sealed | UPDATE 行 `attach=''`、status=0 | 触发器对 OLD 值 −1（NEW 为空串无 +1）——"取消收藏释放引用" |
| v1 复活 | UPDATE 行=新值（客户端重 seal 新 payload） | 触发器对 NEW 差集 +1；§7.2 存在性校验保证 attachment 行已存在，**不调 save** |
| legacy 行全操作 | — | 与迁移前逐字节一致：不改 attach 列的 UPDATE 不触发触发器；INSERT 无操作（+1 归 save）；物理 DELETE 走原 −1 分支 |

### 7.3 并发编辑、删除与幂等（v7 修订：修订号统一）

- **generation=记录修订号（v7 核心修正）**：**一切持久化变更都必须 generation+1**——创建=1、编辑=+1、墓碑=+1、复活=+1、**重包裹=+1**。禁止"重包裹只升 key_version 不动 generation"（v6 表述作废）。
  - *失败用例（v6 方案的失效路径，v7 必须测试不重现）*：
    (a) *CAS 失效*：设备 A 编辑提交（G→G+1）与设备 B 重包裹提交（按 v6 不推进 G）并发，两者 CAS 条件 `generation=expected(G)`——A 提交后 B 的条件仍为真、照样成功，B 以旧 payload 密文覆盖 A 的编辑内容，**编辑丢失且无冲突信号**；
    (b) *同步漏检*：重包裹后 generation 不变，其他设备按"generation 更新才重新拉取"的规则感知不到 key_version 变化；待 §6.5 清除 CKv1 后，这些设备持有的旧密文行与其本地状态失配。v7 中重包裹也 +1，两类失败同源消除。
- **条件更新（CAS）**：客户端提交 `expected_generation` 与新 `generation=G+1`；服务端事务内（已按 §7.2 持 key_state 行锁）执行 `UPDATE ... WHERE user_id=? AND kind_id=? AND status=1 AND generation=expected`；影响 0 行 → 读当前行判定：
  - 当前行 `generation==G+1 && info_sealed==提交字节 && key_version==提交值 && attach_file_hash256==提交值` → **幂等成功**（v7.1 补 attach 同值：幂等判定覆盖**完整持久化状态**而非仅密文字节，防止同代同密文但引用列表不同的重放漏网；同请求字节重试，无需 request_id 列）；
  - 否则 → 409 冲突，客户端重读 → 冲突处理（以服务端 generation 为基重新编辑或放弃）→ 重新提交。
  - **幂等重放不得产生副作用**（v7.1 显式）：触发器差分为空集、referer_time 不变——F1/A9 断言点。
- **删除=墓碑**：取消收藏不物理删行，置 `status=0`、`generation+1`、**`attach_file_hash256=''`（触发器 −1 释放引用，§7.4）**；密文不重封（`seal_generation` 保持，§8.1）；任何 `generation ≤ 墓碑 generation` 的迟到编辑被 CAS 自然拒绝——**旧请求不能复活已取消收藏**。服务端 status CHECK(-1/0/1) 兼容墓碑值。
- **复活=复用原行 UPDATE（v7 修正，替代 v6"新建 status=1 行"）**：重新收藏**禁止 INSERT**——`uk_user_collect_uid_kind UNIQUE(user_id, kind_id)` 跨 status，墓碑行存在时 INSERT 必然撞该约束，且现有 add 的 ON CONFLICT 目标 `(user_id, status, kind_id)` 不覆盖此冲突（SQL 异常，§3 已取证）。正确语义：客户端**重新 seal**（`seal_generation=墓碑G+1`，§8.1）后提交，服务端执行 `UPDATE 墓碑行 SET status=1, generation=墓碑G+1, info_sealed=..., key_version=..., attach_file_hash256=... WHERE user_id AND kind_id AND generation=墓碑G`（触发器对差集 +1，§7.4；不调 save）；行数 0 → 按冲突/幂等规则处理。唯一约束体系（两条 UNIQUE）不动，"每 (user,kind_id) 至多一行，行内 status 表达活动/墓碑"即单行模型。
- **CK 轮换与编辑并发**：重包裹 generation+1 后与编辑并发时必然一方 CAS 失败 → 重读重封重试（§6.5 REWRAP_RUNNING 已按此描述）；轮换期间设备的写入门见 §6.5 与 §7.2。
- **跨设备同步（D1）**：编辑=本地改 → generation+1 → 重 seal → CAS 上传；其他设备拉取**仅检测 generation 变化**（重包裹同样推进，故 key_version 变化不会漏检）→ 解密（AAD 内 generation/key_version 双核对，§8.2）→ 本地 upsert；服务端行为 last-write-wins by generation（同代字节序平局仅作为网络竞态兜底，正常路径不触达）；tag/备注检索仅本地解密缓存。

## 8. 加密接口契约（v6 无歧义化）

### 8.1 密钥派生与封装

```
IKM  = CKvN（32 字节）
salt = ASCII "imboy/e2ee-collect/hkdf/v1"（常量，HKDF salt 参数）
info = TLV("e2ee-collect/v1" || user_id:uint64BE || kind_id:len(u16BE)+utf8 || key_version:uint32BE)
RK   = HKDF-SHA256(salt, IKM, info, L=32)
AAD  = TLV("e2ee-collect/v1" || ver:uint8 || key_version:uint32BE || user_id:uint64BE
           || kind_id:len(u16BE)+utf8 || kind:uint8 || seal_generation:uint64BE
           || attach_digest:len(u16BE)+ascii)
N    = 96-bit CSPRNG（随密文头存储）
info_sealed = b64( header || AES-256-GCM(RK, N, payload JSON utf8, AAD) )
header = magic "EC1"(3B) || N(12B) || seal_generation:uint64BE
attach_digest = SHA-256( hex(小写)哈希按字典序升序 join("\n") )  —— 绑定可见引用列表
```

**seal_generation 语义（v7.1 新增，解墓碑-AAD 脱节）**：`seal_generation` = **封印该密文时刻的行修订号**，明文存于密文头；AAD 绑定的也是它（不再直接绑行当前 generation）。理由：墓碑（服务端代执行或 v1 remove）推进行 generation 但**不重封**（服务端无密钥；取消收藏无需新密文）——若 AAD 绑行修订号，墓碑后任何设备对旧密文构造 AAD 必失败。规则：
- open 从密文头读 `seal_generation` 构造 AAD，并断言 `seal_generation ≤ 行generation`（相等=行未再变更；小于=行发生过墓碑/复活等未重封变更，密文仍为最后一次封印内容，合法）；
- 编辑/复活/重包裹产生新密文时 `seal_generation = 新行修订号`（相等）；
- 复活（重新收藏）**必须重新 seal**（客户端以本地副本/墓碑行解密所得 payload 重封），服务端不接受复活请求携带旧密文字节。

**TLV 编码规则**：字符串=2 字节 BE 长度前缀+utf8 字节；整数=固定宽度 BE；定界符域分隔 ASCII 常量；编码唯一、无歧义、可直接作为向量输入。

### 8.2 认证绑定与攻击区分

- **防串用/篡改**：AAD 覆盖全部解析相关字段 + `attach_digest`；客户端解密后另做两项核对：①从 payload 内声明的附件哈希列表重算 `attach_digest` 比对；②payload 声明列表 == 请求/行内可见列表。服务端不能独立替换引用列表或跨记录拼装。
- **防旧版回放**：`generation` 单调（拒绝同记录回退）+ `key_version` 单调（拒绝已升级记录回退 v1）+ 服务端 key_version 写入门（§7.2）；残留边界：对"本设备从未前进过"的记录无服务器侧承诺机制，列为接受残留，M2 评估 commitment。

### 8.3 nonce 唯一性（按密钥域）

每记录派生 RK → 跨收藏重复无害；同 RK 内 CSPRNG 现取（每次 seal：新建/编辑/重包裹/并发设备各自加密）；传输重试重放同一请求字节不重新加密；量级：单记录 10⁴ 次编辑碰撞 < 1e-17；验证：万次 seal 单测断言全异 + CSPRNG-only lint + A10 全操作日志审计。

## 9. 兼容矩阵（v7 重写 legacy 行守卫）

**判定**：sealed 行 = `key_version IS NOT NULL`；legacy 行 = `key_version IS NULL`。

| 组合 | 行为 |
|---|---|
| 新端+新后端 | v1 sealed；§7.2 校验+CAS+墓碑+高水位门；全功能 |
| 新端+旧后端 | 能力协商无 `e2ee_collect_v1` → 拒绝创建+升级提示（实证：落旧后端丢 GC 保护）【待实测】 |
| 旧端+新后端，操作 **legacy 行** | 与现状完全一致：add upsert / remove 物理删除（legacy 行无防复活需求，不产生墓碑）/ change 直改 remark 列；**不受单调门/墓碑/版本控制影响** |
| 旧端+新后端，操作 **sealed 行** | **受统一保护，禁止绕过版本控制**（v6"legacy 不受单调门/墓碑影响"的表述作废——那只对 legacy 行成立）：见下表 |
| 旧端读 sealed | 空预览/失败占位【待实测】 |
| 新端读 legacy | 兼容读（照旧 toTypeMessage） |

**legacy 接口对 sealed 行的三类操作语义（v7 新增，B2/B3 落地）**：

| legacy 操作 | sealed 目标行 | 行为 |
|---|---|---|
| `remove`（现为物理 DELETE，`user_collect_repo.erl:52-59`） | status=1 | **服务端代执行墓碑**：同事务内（§7.2 高水位行锁先行）置 `status=0, generation+1, attach_file_hash256=''`（触发器 −1，§7.4）；**绝不物理 DELETE**（物理删会拆掉防复活墓碑与 GC 引用豁免，且删除后高水位门虽不降（§7.2），但迟到 v1 编辑可无约束 INSERT 复活）。旧端效果=删除成功（page 只查 status=1） |
| `remove` | status=0（已是墓碑） | 幂等成功，不变更 |
| `change(remark)`（现直改 remark 明文列） | 任意 status | **拒绝**（错误码 `sealed_readonly` + 升级提示）——sealed 行 source/remark/tag 为密文内字段（D1），旧端写明文列既污染结构又绕过 generation 版本控制 |
| `change(transpond_callback)`（仅 updated_at） | 任意 status | 放行——不触数据面/引用/版本，语义=最近使用排序 |
| `add`（upsert） | status=1 | 现状行为即安全：ON CONFLICT 分支只 SET updated_at/status，不覆盖 info_sealed/attach（`user_collect_logic.erl:373-377`）——等价"已收藏"，放行 |
| `add` | status=0（sealed 墓碑） | **拒绝**（`sealed_readonly` + 升级提示）：INSERT 必撞 `uk_user_collect_uid_kind`（跨 status，ON CONFLICT 目标不匹配 → SQL 异常 500，§3 已取证），且不可由旧端复活；前置检查存在 sealed 行（任意 status）即拒绝，兼修现状 500 |

*失败用例（v6 方案的破坏路径，v7 必须测试不重现）*：用户设备 A（新端）创建 sealed 收藏 → 设备 B（旧端）调用 remove → v6 语义下物理 DELETE：墓碑消失、GC 触发器减计数、防复活拦截失效 → B 或另一掉队设备以迟到 v1 编辑 INSERT 重建 status=1 行（无墓碑可拦）→ 已取消的收藏被复活且引用计数被重复累计。v7 中 legacy remove 对 sealed 行只能走墓碑化，该路径不存在。

## 10. M2 范围

恢复短语；群退群授权/副本（D2）；服务器侧 commitment 评估；惰性重包裹兜底；T2/T3 执行器。

## 11. 存量处置（均未授权执行）

- **T1（修正表述）**：**限定为"支持 sealed 协议的新端创建的新收藏停止相关暴露"**——旧端继续走 legacy 路径，M1 上线**不能**宣称"所有收藏停止暴露"；
- **T2**：legacy 行客户端重封替换；只断"服务端再读取"路径，历史事实不可追溯消除；
- **T3**：附件级重密（新钥新对象+引用改写），语义是"不再需要信任"；**保留可选，本次不实施不执行**（D3）。

## 12. 修订差异

**v7.1（实施首日修订，用户已授权）**：①§8.1 密文头加 `seal_generation`（解墓碑不重封导致 AAD-行修订号脱节），open 断言 `seal_generation ≤ 行generation`，复活必须重 seal；②§7.4 新增：取证触发器 UPDATE 分支无差分（migration 26 终版）→ 227 替换 `imboy_user_collect_fun` 为差集差分，v1 全生命周期计数单点权威表；③§7.3 CAS 幂等判定补 attach 同值 + 幂等零副作用断言；④§15-F4 断言修正（轮换中间态合法混存，门对请求生效）。
**v6 → v7**：①§3 撤回"无需迁移"，取证两处 UNIQUE 跨 status 语义与 add ON CONFLICT 目标不匹配的现状 500 隐患；新增 B0 迁移（`generation`/`key_version` 列 + `user_collect_key_state`）；复活由"新建行"改为"复用原行 UPDATE"（§7.3）；②`generation` 统一为记录修订号，重包裹等一切持久化变更 +1，CAS 幂等判定加 key_version，客户端同步检测=仅看 generation（§7.3）；③§9 重写 legacy 行守卫：remove sealed→墓碑化、change(remark) sealed→拒绝、add 遇 sealed 墓碑→拒绝（兼修现状 500）；④§7.2 单调门重写为高水位表行锁串行化、只升不降；⑤§14 任务清单加 B0 与守卫实现点；⑥§15/§16 加四项失败用例针对性测试 F1-F4 与验收硬门。
**v5 → v6**：①§6.2 批准前置（旧设备核验+发送前重查授权，ACK 仅收讫）；②§7.3 CAS 条件更新+幂等（同请求字节判定）+墓碑删除+轮换并发；③§6.5 轮换状态机与 CKv1 清除四前置（含服务端 key_version 单调门 §7.2）；④§8.1 显式 HKDF salt/info/L 与 TLV 编码表+`attach_digest` 入 AAD 及解密后双核对；⑤A10 改为全操作日志审计、T1 表述限定；⑥新增 §14-16。
**v4 → v5**（摘要）：nonce 密钥域收窄；AAD 全字段+防回放区分；轮换边界；GC 服务端校验；A8-A10。
**v3 → v4**（摘要）：M1 收窄 C2C；兼容实证化；CK 完整化；逐字段表；语音降待验证；存量三档。

## 13. 决策记录

D1 入密（含 §7.3 客户端化编辑/检索与同步）；D2 留 M2；D3 T3 保留不实施；S 不做。

## 14. 分任务落地清单（路径级；均为 M1 必要实现，无外围能力）

### 客户端 imboyapp（fix_leeyi 分支）
| # | 任务 | 路径 | 内容 |
|---|---|---|---|
| C1 | 加密模块 | `lib/service/e2ee/collect_crypto.dart`（新） | TLV 编解码、HKDF 派生、seal/open、CSPRNG nonce、attach_digest；纯函数 |
| C1t | 向量测试 | `test/unit_test/service/e2ee/collect_crypto_vectors_test.dart`（新） | §15 全部向量 |
| C2 | 密钥链 | `lib/service/e2ee/collect_keychain.dart`（新） | CK 版本化存储、§6.5 状态机持久化、写入门槛；**§6.7 logout/换号接线**：持久键 `collect_ck_` 前缀登记 `E2eeSecretInventory` + `clearCache` 进 `clearMemoryCaches`（守卫：`collect_keychain_purge_test.dart`、`secret_inventory_registry_test.dart` 归类表） |
| C2t | 状态机测试 | `test/unit_test/service/e2ee/collect_keychain_rotation_test.dart`（新） | 中断恢复/掉队行/清除四前置 |
| C3 | 密钥同步 | `lib/service/e2ee/collect_key_sync.dart`（新） | §6.2 入组协议（enroll/批准/发送前重查/ACK/重试）、自发通道承载 |
| C4 | 收藏 API v1 | `lib/store/api/user_collect_api.dart`（改）+ `lib/app_core/feature_flags/feature_keys.dart` | 能力协商读取；v1 add（含墓碑复活路径：复用原行 CAS，不 INSERT）/update(CAS)/remove(墓碑)。**实施注记（2026-10-24）**：能力门已落地——`FeatureKeys.e2eeCollect` + `AppFeatureRegistry.isEnabled`（键缺失 fail-closed）；`e2ee`/`e2ee_collect` 均为组合级编译特性（generated 清单，广州一期组合未选），**rollout 耦合警示：客户端本门生效后，组合未含 e2ee 域时收藏入口一律拒绝+升级提示（§9 无明文回退），发布须与组合重生成（generate_product_features.py 选入 e2ee 域）配对** |
| C5 | 收藏 Provider | `lib/page/mine/user_collect/user_collect_provider.dart`（改）+ `lib/service/e2ee/collect_sealed_service.dart` | sealed 解析+本地优先读（decryptModels，失败占位）、D1 编辑流（remark 全量委托 service：sealed 走 v1 CAS/legacy 走原接口）、同步检测=仅看 generation 变化（needsResync 已实现并测试；provider 增量刷新接线留真机联调段，当前 onRefresh 全量路径已覆盖同步语义）。**实施注记（2026-10-24）：provider 受 800 行 only-shrink 门禁（基线 1366），逻辑全部外移 service，净行数不增**。**创建流接线（2026-10-24 补，§5 能力1/A4 前置）：`collectSealed` 全量落 service——attach 提取与服务端 add_legacy 各 kind 同口径（多图 images[]、视频 thumb+video 双对象、其余 payload 顶层 file_hash256??md5，去重）；密封前 payload 内嵌 `attach_file_hash256` 声明（§8.2 双核对②数据源，缺此键非文本收藏不可解）；本地已有活动 sealed 行已收短接；seal_generation 创建恒 1（§8.1 断言为 ≤，复活 G+1 仍成立；旧密文重放不存在——每次全新 seal）；legacy 行遇 5506 如实透出** |
| C6 | 详情页转发修复 | `lib/page/mine/user_collect/user_collect_detail_page.dart`（改） | 保留原 id/会话/发送者作源绑定，目标 id 入 SendToPage 后生成 |
| C7 | 本地库 | `lib/store/repository/user_collect_repo_sqlite.dart`（改）+ `lib/service/migrations/upgrade_edges_v34.dart` / `downgrade_edges_all.dart` | generation/密文行 upsert、解密缓存；本地 v34 迁移 ADD COLUMN ×3。**纪律（2026-10-24 纠偏）**：`kBaselineSchemaSql` 是冻结的 v16 基线（`_onCreate` 应用后再逐级升级），新列**只**经 v34 upgrade 块进入，不得写进 baseline（否则 v16 schema 金样漂移；且 v17–24 的 user_collect 重建块会使 baseline 注入列失效）；downgrade 块头须为裸 `-- VERSION: 34`（inventory 解析器 `int.tryParse`，带后缀即静默丢块） |
| C8 | 集成测试 | `test/.../collect_e2ee_integration_test.dart`（新） | A4/A5 场景自动化可自动部分 |

### 后端 imboy（fix_leeyi 分支）
| # | 任务 | 路径 | 内容 |
|---|---|---|---|
| B0 | **迁移（v7 新增，v7.1 扩充）** | `priv/migrations/00000227_user_collect_e2ee_v1.{up,down}.sql`（新） | up：①`ALTER TABLE user_collect ADD COLUMN generation bigint NOT NULL DEFAULT 0, ADD COLUMN key_version integer`（NULL=legacy）；②`CREATE TABLE user_collect_key_state(...)`（§7.2）；③`CREATE OR REPLACE imboy_user_collect_fun()`——UPDATE 分支差集差分（§7.4），INSERT/DELETE 分支与 26 终版逐字节一致。存量行 generation=0 不动 legacy 路径。down：恢复 26 版函数、DROP TABLE/COLUMN 逆序。遵循函数替换惯例（第 6 次）与 erlang_migrate 约定 |
| B1 | 能力位 | `src/imboy_feature.erl` 相关清单 + `src/api/app_manifest_handler.erl` | `e2ee_collect_v1` |
| B2 | handler v1 + legacy 守卫 | `src/api/user_collect_handler.erl`（改） | v1 参数、CAS 语义、墓碑、冲突码；**legacy 分支加 sealed 守卫（§9 表）**：remove→logic 判 sealed 走墓碑化；change(remark) sealed→`sealed_readonly` 错误响应；add 前置查 sealed 行（任意 status）→拒绝 |
| B3 | logic v1 | `src/logic/user_collect_logic.erl`（改） | sealed 分支：§7.2 引用校验+差分+高水位门（key_state 行锁先行）；legacy 分支行为对 legacy 行逐字节不变 |
| B4 | ds/repo | `src/ds/user_collect_ds.erl`（改）+ `src/repo/user_collect_repo.erl`（改） | CAS SQL、墓碑 status=0+generation+1、**复活=复用原行 UPDATE（不 INSERT）**、key_state upsert-锁 SQL、事务内引用增减、锁顺序"先 key_state 后 user_collect" |
| B5 | EUnit | `test/logic/user_collect_v1_tests.erl`（新）+ 迁移 227 起停测试 | §7.2/§7.3 全分支（存在性/授权/CAS/幂等/墓碑/复活单行约束/高水位门/差分对账）+ **F1-F4 针对性测试（§15）** |

**顺序**：C1/C1t → **B0** → B1-B5 → C4 → C2/C2t/C3 → C5/C6/C7 → C8 + §16 验收。
**迁移安排**：仅 B0 一个迁移文件（00000227）；不改任何现有约束/触发器；`imboy_for_user_collect` 触发器仅监听 attach_file_hash256 变更，新增列与其无交互（迁移内加注释说明）。

## 15. 协议测试向量（实现期生成并冻结）

确定性输入集（固定测试 CK/用户/kind_id/多语言 payload/边界长度），向量文件 `collect_crypto_vectors.json` 由参考实现生成、双人评审后冻结，所有实现必须逐字节通过：
- V1 派生：固定 CK/user/kind_id/kv → RK 黄金值（HKDF salt/info/L 按 §8.1）；
- V2 封装：空 payload/最小/最大（1MiB）/全 unicode 多语言 → 密文+tag 黄金值（nonce 固定向量专用）；
- V3 AAD：同 payload 异 kind_id/kind/generation/attach_digest → 密文互异；
- V4 篡改：tag/密文/AAD 任一字节翻转 → open 必败；
- V5 串用：记录 A 密文配记录 B AAD → 必败；
- V6 回放：generation 回退、key_version 回退 → 客户端拒绝（协议层断言）；
- V7 引用绑定：attach_digest 与 payload 声明列表不一致 → 解密后核对必败；
- V8 nonce 语料：同 RK 万次 seal nonce 全异（供 A10 方法链验证）。

**四项矛盾修正的针对性测试 F1-F4（B5 + staging 必测，各自复现 v6 失败用例并断言 v7 不重现）**：
- **F1 迁移与单行模型**：B0 起停（up→结构断言：两列+key_state 表+存量行 generation=0/key_version NULL；down→恢复）；sealed 墓碑存在时 v1 重新收藏 → 复活 UPDATE 成功且 (user,kind_id) 仍恰一行；legacy add 遇 sealed 墓碑 → 业务错误 `sealed_readonly`（非 SQL 500）；两条 UNIQUE 全程无违例。
- **F2 修订号统一**：并发"编辑 CAS（G→G+1）"与"重包裹 CAS（G→G+1，key_version 1→2）"交错 → 后提交方必 409、重读重封后成功，先提交方内容不丢失；重包裹成功后其他设备 generation 变化触发重新拉取解密（key_version 变化不漏检）。
- **F3 legacy 守卫**：旧端 remove sealed(status=1) → 行存续 status=0、generation+1、触发器 referer 减计数、无物理 DELETE；旧端 change(remark) sealed → `sealed_readonly`；旧端 add 遇 sealed(status=1) → 不覆盖 info_sealed/attach；旧端对 legacy 行 add/remove/change → 与迁移前行为逐字节一致（回归组）。
- **F4 高水位门（v7.1 断言修正——轮换中间态库内**合法混存**新旧 key_version 行：第一条 rewrap 推进高水位后，其余 v1 行瞬时低于 hw，属 REWRAP_RUNNING 正常状态，"库内不存在 v<hw 行"只在轮换终态成立）**：①两并发事务分别提交 key_version 1 与 2（2 为轮换推进）任意交错 → **提交时 key_version < 当时 hw 的写请求被 409**（门对请求生效，非对存量行）；②任何已存在行被重写时其 key_version 单调不降；③模拟轮换完成（全部行 rewrap 到 v2）后终态无 v<hw 行；④删除该用户全部 v2 行（模拟物理清除）后再提交 v1 写入 → 仍被 409 拒绝（高水位不随行回落）；⑤迟到旧版写被拒后客户端走同步-重试路径。

## 16. 针对性验证与最终集成验收安排

**分层**：单元（C1t/C2t/B5，向量+状态机+服务端分支+**F1-F4**）→ staging 后端（A2/A8/A9：DB 断言、强制 `age_days` 触发 GC 扫描、referer 对账；**F4 并发交错在 staging 以双连接事务复演**）→ 真机矩阵（A4 四变体、A5、A6 旧端占位、A7 旧后端拒绝；**F3 以旧版本安装包操作 sealed 行**）→ 抓包（A1/A3，代理采集请求与响应）→ 并发审计（A10：双设备编辑风暴全操作日志）。

**staging 层本地等价预验（2026-10-24，本地真实服务器 imboy_local@127.0.0.1 + PG 4323 迁移 227）**：
- **A9 三操作对账全绿**：迁移 227 真实触发器差分——创建（attach='a,b' 逐行 +1）、编辑差集（旧−新释放 a、新−旧建立 c，交集 b 计数不动）、同值重放（差集空、计数零变化，F1 幂等零副作用断言点）、墓碑（status=0+attach='' 全释放归零）；
- **A2 存储面**：sealed 行 info 为密文（非明文 JSON）、attach_file_hash256 声明非空、generation=1/key_version=1 齐备；
- **A8 GC 保留引用全绿**（含多哈希）：被引用行（单哈希行 + 多哈希列表成员）三道防线存活——①孤儿谓词 referer_time=1 不入候选；②`refresh_gc_verified_tx`（生产代码路径）对被引用行 proven=false（收藏 ANY(string_to_array) 匹配拦截）；③fn 锁后复核路径走通；无引用孤儿 delete_intent + attachment_object_deletion 入队（cleaned=1）；引用释放（收藏删除）后同批附件恢复 proven=true 并被正常回收——保留/回收对称闭环；
- **F4 高水位门 HTTP 全栈复演**：kv=2 首写成功（hw 0→2）→ kv=1 迟到写与重放均 5504 → kv=2 等值写成功；`user_collect_key_state` hw=2 与落库行一致，kv=1 行从未落库；
- **范围外观察（attachment 域，报 attachment owner 裁决）**：gc_verified 仅在 group_file/talk_record 绑定事件时逐 path 刷新，上传 confirm 与孤儿扫描链无刷新入口——普通附件孤儿清理现状对 gc_verified=false 行保守 skipped；staging 正式复演 A8 时需经 refresh 路径（或补批量刷新入口）后方可演示孤儿被清的对照组；
- 仍需真实环境：A1/A3 抓包、A4 真机四变体、A6 旧端占位、A7 组合 manifest 实测、A10 nonce 审计，及 staging 正式复跑本层。
**门禁**：A1-A10 + F1-F4 全绿且无未声明偏差方可宣布 §5 能力成立；任一失败按 §5 条件化措辞降级声明（如语音跨设备失败归 M2）。
**验收硬门（v7 新增）**：**F1-F4 属 M1 强制项**——失败时必须修复后重验，或重新评审并**明文缩减 §5/§7 声明范围**；禁止在强制项失败的状态下转入 M2 并宣称 M1 完成。§5 能力声明与验收结果一一对应，未通过的条件不得保留在声明中。
**回归面**：legacy 收藏全路径（创建/编辑 remark/取消/列表/转发）不因 v1 分支引入回归；迁移 226 相关守卫测试保持绿。
