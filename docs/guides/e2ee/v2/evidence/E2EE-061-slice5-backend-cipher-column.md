# E2EE-061 Slice 5 —— 后端字段语义：附件密文判别位（迁移 000050）

> **会话**：20260730-0200-claude-code ｜ **仓库**：imboy
> **状态**：后端 expand 完成。**客户端仍未切换**，E2EE-061 整体仍 `PENDING`

---

## 1. 为什么后端先走（expand-then-migrate）

上一刀的结论是「Slice 4/5 不可分开交付」。**准确的说法是不能让客户端先走**：
客户端先发密文哈希、后端字段语义仍是明文哈希，中间态不自洽。

**后端先扩展则没有这个问题**：本刀只是让 `attachment` **有能力**记录「这一行是密文」。
不传 `cipher` 的旧客户端行为**逐字节不变**，落库 `cipher IS NULL` = 明文，
与今天完全一致。客户端切换是下一刀，切换后中间态也自洽。

---

## 2. 改了什么

| 文件 | 改动 |
|---|---|
| `priv/migrations/00000050_attachment_cipher.{up,down}.sql` | 新增可空列 `cipher varchar(32)`；改写 `cipher`/`file_hash256`/`size` 三个列注释以记录语义变更；新增**部分索引** `WHERE cipher IS NULL`（拍板 ② 的"预留"：盘点明文积压与日后分批回迁） |
| `src/logic/attach_logic.erl` | `normalize_cipher/1` + `do_save` 前置校验；`cipher` 进 Attach |
| `src/repo/attachment_repo.erl` | `cipher` 透传，缺省 `null` |
| `src/api/attach_handler.erl` | 新增 `{error, unsupported_cipher}` 分支 |

### 2.1 为什么是可空字符串而不是 boolean

boolean 无法表达「是哪一种套件」，而客户端 `AttachmentDescriptor` 里 `cipher`
本就是具名字段。两侧用**同一个概念**，不再造一个只有真假的影子字段。

### 2.2 fail-closed：不做套件协商

`normalize_cipher/1` 只接受 `undefined/null/<<>>`（明文，含全部旧客户端）与
`<<"AES-256-GCM">>`；**任何其它取值拒绝整个 confirm**。

⚠️ 关键取舍：把未知套件**落成 NULL** 才是真正危险的降级——
那会把一个密文对象**标记成明文**，日后回迁盘点漏掉它、读取侧当明文直读。
宁可拒绝 confirm。

### 2.3 `size` 无需改动（实测发现）

`attach_logic` 原本就写着「mime_type/size 一律采用服务端 HEAD 核实的真实值」。
上传密文后服务端 HEAD 到的就是**密文大小**，因此密文大小**自动正确**，
本刀只需把这条语义写进列注释。

---

## 3. 引用计数触发器已核实不受影响

`user_collect.attach_file_hash256` ↔ `attachment.file_hash256` 是**按值 JOIN**
的引用计数触发器（迁移 000026）。收藏侧存的就是 attachment 里那一份哈希字符串，
同代数据仍然匹配，**触发器无需改动**。

⚠️ 但语义已变：加密附件的这个值是密文哈希，同一明文用不同 content key 上传两次
得到两个不同值，**跨用户去重就此不再成立**——这是拍板 ① 的既定后果，不是缺陷，
已写入迁移注释。

---

## 4. 空验证

| 空验证 | 手法 | 结果 |
|---|---|---|
| A | 未知套件静默降级为 `null` | `Failed: 1` —— 唯独 fail-closed 用例 |
| B | repo 不再透传 `cipher` 列 | `Failed: 3` —— 加密行/重放/盘点三条 |
| — | 恢复 | All 6 passed |

---

## 5. 覆盖（6 例）

| 用例 | 说明 |
|---|---|
| `normalize_cipher` fail-closed（纯函数，无需 DB） | 三种明文缺省形态 → `null`；唯一套件透传；**8 个反例**（含 `aes-256-gcm` 大小写、`AES-256-GCM ` 尾空格、`none`、`true`）一律 `{error, unsupported_cipher}` |
| 对照组 | 迁移已生效、列存在且可空。它红则后面结论都不成立 |
| **正向可用性** | 旧调用形状（不传 `cipher`）落库为 `NULL`，语义不变 |
| 加密上传 | 写入套件名 |
| confirm 重放 | `ON CONFLICT (path)` 只递增 `referer_time`，**不翻转 `cipher`** |
| 盘点查询 | 可区分明文/密文（拍板 ② 预留的用途） |

---

## 6. 验收

```
# 真 PG 集成（本模块不进 e2ee-verify 硬门禁：无 DB 会 skip，进了只得假绿）
IMBOYENV=local make eunit t=attachment_cipher_tests \
  EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
  → All 6 tests passed（DB 就绪，走的是真 PG 分支而非 skip 分支）

make e2ee-verify → All 385 tests passed（未变；本模块按上述理由不入清单）
erlfmt --check（4 个改动文件）/ git diff --check → 通过
```

迁移已在本地真 PG 应用并核实：

```
cipher | character varying | YES | NULL::character varying
既有 6 行全部 cipher IS NULL（= 明文，零回填，正是拍板 ②）
```

---

## 7. ⚠️ 浮出的第四项待拍板：MIME 是否隐藏

设计 §3.2 要求隐藏 Content-Type，但**这不在已拍板的三项之内**，且本轮实测发现
它比设计描述的更纠缠：

- `attach_logic:confirm` 对**服务端 HEAD 到的** Content-Type 跑
  `elib_oss:validate_file_type/1` **白名单**。若客户端把 MIME 改成
  `application/octet-stream`，**白名单要么放行一切、要么拒绝全部加密附件**；
- presign 与 confirm 两处服务端都收 `mime_type`（Slice 1 已实证）；
- 隐藏后服务端签发的下载 URL 也不再带真实 Content-Type，**预览行为会变**。

即：隐藏 MIME = 放弃服务端类型白名单这道防线 + 改变预览行为。
**这是一项独立的产品/安全取舍，不是技术细节，须人工拍板**，本刀不擅自决定，
也未做任何 MIME 相关改动。

---

## 8. 残留风险

1. ⚠️ **客户端仍未切换** —— 至今没有任何调用方传 `cipher`，
   **生产附件路径依旧明文直传**，ATT-01..05 全部不成立；
2. **MIME 隐藏未决**（§7），且未做任何相关改动；
3. **`down.sql` 是有损的** —— 回滚会丢失「哪些对象是密文」这一事实；
   已在 down 脚本抬头写明，仅在「尚未有任何加密上传」时回滚才无损；
4. **OpenAPI 未同步** —— confirm 的 `cipher` 入参未写进 `v1.yaml`。
   刻意留到客户端切换那一刀一起改，避免文档先于实现宣称能力；
5. **admin 侧未适配** —— `adm_attach_handler` / 附件统计未展示 `cipher`，
   运营看不出哪些是密文；
6. **未做加密附件的读取侧改动** —— `view_url` 照旧签发 presigned GET，
   解密在客户端（Slice 6），本刀不涉及；
7. **迁移未在生产执行**（不 push、不部署）。

---

## 9. 认识论状态

| 结论 | 状态 |
|---|---|
| 迁移生效、旧行全部 `NULL`、语义不变 | **已实证**（真 PG，6/6 且非 skip 分支） |
| 未知套件 fail-closed 不降级 | **已实证**（用例 + 空验证 A） |
| `cipher` 真的写进了库 | **已实证**（空验证 B 红 3 条） |
| confirm 重放不翻转 `cipher` | **已实证** |
| `size` 已是服务端 HEAD 真值，密文大小自动正确 | **已实证**（读 `attach_logic` 源码 + 列注释） |
| 引用计数触发器不受影响 | **推理**（按值 JOIN、同代匹配）——**未构造收藏场景实测** |
| 隐藏 MIME 会撞上服务端白名单 | **已实证**（`confirm` 里 `validate_file_type` 的调用位置） |

---

## 10. 未做

- 不改客户端、不改协议版本、不改 ADR、不改任务状态标记。
- 不 push、不部署、不访问生产、不通知第三方。
