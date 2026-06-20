# 资源访问控制改造 · 进度与早间验收报告 / Progress & Morning Review Report

> **生成 / Generated**: 2026-06-20（自动化 loop 一轮产出）
> **权威设计 / Design**: [resource-access-control.md](./resource-access-control.md)
> **阶段三 Runbook**: [resource-access-control-phase3-runbook.md](./resource-access-control-phase3-runbook.md)
> **硬边界遵守**: 全程**未做任何 git commit / 分支操作**，**未执行任何生产操作**，所有改动留工作树待你 review。

---

## 0. 一句话结论 / TL;DR

阶段二（客户端编码）+ 阶段四（安全死代码清理）**已完成并通过绿灯门**；阶段三仅产出 runbook（命令就绪、未执行）。客户端头像上传/渲染已切到 public 公开直读链路，聊天附件按 c2c/group 分流，go-fastdfs 旧上传死代码已删。**真机功能验收留给你早上点验**（见 §5 checklist）。

> ### 🔺 2026-06-20 后续 fire 追加（最重要变更）
> 本轮对抗式复核**发现并修复一个 prior fire 漏掉的真实回归**：聊天附件上传的 `_uploadScope` 把 `conversationUk3` 当**冒号权威格式** `c2c:/c2g:` 解析，但 `ConversationUk3Generator` 实际产出**大写下划线格式** `C2C_min_max`/`C2G_uid_gid` → `startsWith('c2c:')` **永不命中** → **所有聊天图片/文件上传都被静默打成 `scope=private`**，c2c/group 读鉴权完全失效（上传方自己看得到、对方/群成员看不到）。
> **修复**：抽出纯静态可测函数 `ChatAttachmentHandler.deriveUploadScope`，以**会话 `type` 为权威源**、uk3 前缀兜底；c2c 的 `c2c:min:max` 用 `BigInt` 整数归一化（修 Flutter Web dart2js 53 位 int 对 64 位 TSID 的排序失准）。新增 **11 个契约测试**全绿。详见 §2 / §3.5。
> ⚠️ 含义：**prior 报告"聊天 c2c/group 分流已完成"在本轮修复前实为失效**；现已真正生效（待真机验收 §5）。

---

## 1. 各阶段完成度 / Phase Status

| 阶段 | 范围 | 状态 | 说明 |
|------|------|------|------|
| 一·后端 | presign/confirm/view_url/六分支 authorize/迁移 | ✅ 既有（本轮仅 +1 行） | 本轮新增 `/v1/init` 下发 `public_base_url`（启用客户端拿基址），编译通过 |
| 二·客户端 | 头像 public + 聊天 c2c/group + 渲染分流 + public_base_url 链路 | ✅ 完成 | 见 §2 改动清单；绿灯门见 §4 |
| 三·生产配置 | Garage website / nginx / 旧 avatar 置空 / 域名下线 | 📋 仅 runbook | **未执行**，命令+回滚就绪：[runbook](./resource-access-control-phase3-runbook.md) |
| 四·清理 | 删 go-fastdfs 旧上传死代码 | ✅ 安全子集完成 | 删 5 个零调用上传方法+7 孤立 import；**保留** legacy view_url 渲染路径（见 §3） |

---

## 2. 改动文件清单 / Changed Files

### 后端 imboy（1 文件）

| 文件 | 改动 |
|------|------|
| `src/api/index_handler.erl` | `/v1/init` 响应新增 `<<"public_base_url">> => elib_oss:public_base_url()`，供客户端本地拼头像 URL |

### 客户端 imboyapp（编码）

| 文件 | 改动 |
|------|------|
| `lib/store/api/attachment_api.dart` | `uploadViaPresign` 及全部 wrapper（meta/image/video/compat）新增 `scope`/`scopeRef`，写入 presign query + confirm body；**删除** 5 个 go-fastdfs 死方法（`_upload`/`preUpload`/`uploadVideo`/`uploadFile`/`uploadBytes`，405 行）+ 7 个孤立 import |
| `lib/page/personal_info/profile/profile_provider.dart` | `uploadAvatar` 传 `scope:'public'`，object_key 作 `user.avatar` 值（方案 B），统一走 confirm |
| `lib/component/helper/crop_image.dart` | 头像裁剪上传路径（`prefix=='avatar'`）按 `scope:'public'`（**自审发现的第二条头像上传路径**，见 §3.1 C2） |
| `lib/page/chat/chat/attachment_handler.dart` | ⚠️**本轮修复回归**：原 `_uploadScope` 按 `conversationUk3.startsWith('c2c:'/'c2g:')` 派生，但生成器实际产出大写下划线格式 `C2C_/C2G_`，**永不命中→聊天上传永远回退 private**（c2c/group 鉴权形同虚设）。改为纯静态可测函数 `deriveUploadScope`：**会话 type 为权威源**、uk3 前缀兜底；c2c 用 `BigInt` 归一化构造 `c2c:min:max`（修 Web dart2js 53 位 int 大 TSID 失准）。7 个聊天上传点行为不变 |
| `lib/page/chat/chat/chat_page.dart` | 构建 `ChatAttachmentHandler` 透传 `type: _chatType`（权威会话类型，供 scope 派生兜底 uk3 形态漂移） |
| `test/page/chat/attachment_upload_scope_test.dart`（**新增**） | 11 个契约测试锁定 `deriveUploadScope`：C2G/C2C 整数与字符串序、type 权威优先、Web 大 TSID(BigInt)、C2S/S2C/空→private、legacy 冒号形态 |
| `lib/service/assets.dart` | 新增 `publicUrl(objectKey)` = `Env.publicBaseUrl + '/' + key`（公开直读，零 DB/不签名） |
| `lib/config/env.dart` | 新增 `publicBaseUrl` getter：读 StorageService（/v1/init 下发），回退内置默认 `https://s3.imboy.pub` |
| `lib/config/const.dart` | 新增 `Keys.publicBaseUrl` |
| `lib/config/init.dart` | `initConfig` 存 `public_base_url`；环境切换时清理 |
| `lib/component/extension/imboy_cache_manager.dart` | `getSingleFile` 新增 `publicDirect` 旁路：完整公开 URL 直下，不 resolve、不拼 HMAC |
| `lib/component/ui/imboy_cached_image_provider.dart` | 新增 `publicDirect` 字段并透传；`==`/`hashCode` 纳入 `publicDirect`（自审 C1） |
| `lib/component/helper/func.dart` | 新增 `avatarImageProvider`（object_key→公开直读，legacy 完整 URL 回退）；`dynamicAvatar` 改用之 |
| `lib/page/mine/mine/mine_page.dart`、`lib/page/personal_info/personal_info/personal_info_page.dart`、`lib/page/chat/send_to/send_to_page.dart`、`lib/component/chat/mention_list_widget.dart`、`lib/component/ui/avatar_group.dart`(×2) | 6 个**用户头像**渲染点从 `cachedImageProvider` 迁到 `avatarImageProvider`（公开直读，零 DB）（自审 H1） |
| `test/store/attachment_upload_presign_test.dart` | 更新 confirmBody 断言（含 `scope:'private'`）+ 新增 public/c2c scope 透传测试 |

> ⚠️ 注：工作树另含**前次 WebSocket 调试会话**遗留的无关改动（`websocket.dart`、`websocket_status_provider.dart`、`sticker_picker*`、`message_expression_builder.dart`、`env.dart` 的 staged 部分），**非本次任务**，review 时请区分。

---

## 3. 关键设计决策与自审发现 / Key Decisions & Self-Review

### 3.1 对抗式自审（flutter-reviewer）发现并已修复

| 级别 | 发现 | 处置 |
|------|------|------|
| CRITICAL **C1** | `IMBoyCachedImageProvider` 的 `==`/`hashCode` 未含 `publicDirect`，同 URL 不同访问模式会撞 Flutter image cache key | ✅ 已修：equality/hashCode 纳入 `publicDirect` |
| CRITICAL **C2** | **第二条头像上传路径** `crop_image.dart` 未传 `scope:'public'` → 头像落 private 桶 → 公开直读 404 | ✅ 已修：按 `prefix=='avatar'` 派生 public |
| HIGH **H1** | 多处用户头像仍走 `cachedImageProvider`(→view_url DB)，未达"零 DB 公开直读"设计目标 | ✅ 已迁 6 个**用户头像**点到 `avatarImageProvider`；**频道封面/背景**故意不迁（见 §3.2） |

### 3.2 有意保留 / 越界不动（含理由）

| 项 | 决策 | 理由 |
|----|------|------|
| `AssetsService.viewUrl`/`viewUrlAsync`/`authData`/`Env.uploadScene` | **保留，不删** | 设计 §10.5 把它们列入删除是**过早的**：它们是**历史消息附件（旧完整 URL）的活跃渲染路径**（`message_model_mapper`、`video_viewer`、`imboy_cache_manager` legacy 分支等多处在用）。删除会破坏所有迁移前历史附件渲染。须等历史消息附件迁移后再删。 |
| 频道封面 / 背景图 / 表情 上传与渲染 | **本期不改** | 任务范围明确为 avatar(public)+chat(c2c/group)。频道封面/背景虽设计上属 public，但其**上传仍走默认 private**，若把渲染改成公开直读会 404。需作为**独立后续任务**（连带把它们的上传改 public）。**这意味着"他人看不到频道封面/背景"的跨用户可见性问题本期仍存在**（与原头像 bug 同源）。 |
| 后端 `elib_oss:presign_put/3`（旧无 uid 前缀接口） | **保留，不删** | 已确认零调用（新链路用 `presign_put_for_key/4`），可安全删除，但属后端 phase-1 范围、价值极低、需重启才生效。记录为待清理项，不擅扩后端 diff。 |
| `message_s2c.dart` 的 `a.imboy.pub`（563/571） | **不动** | 在 `/* */` 注释块内（S2C 报文示例文档），非执行代码；改它会歪曲历史报文示例，无功能影响。 |

### 3.3 public_base_url 客户端来源 —— 待你确认的决策

- **现状排查结论**：客户端原本**无任何通道**拿到 `public_base_url`（env.dart 无此字段，`/v1/init` 原未下发）。
- **本轮采用方案**：后端 `/v1/init` 下发 + 客户端 `Env.publicBaseUrl` 读 StorageService、**缺省回退编译期硬默认 `https://s3.imboy.pub`**。
- **理由**：多环境免重新打包（与 `uploadUrl` 既有下发模式一致）；硬默认保证 init 完成前/未下发时头像即可渲染。
- **⚠️ 需你拍板/注意**：
  1. 若生产 `public_base_url` ≠ `https://s3.imboy.pub`，**必须**确保后端下发生效（runbook 步骤 3），否则客户端用错默认值。
  2. 本地/dev 环境：后端 `sys.local.config` 的 `public_base_url` 需指向本地 Garage 公开端点，否则本地头像会用生产默认值。
  3. `imboy_env:override_garage/1` 当前**不覆盖** `public_base_url`（仅 endpoint/bucket/key）。若想用 `IMBOY_*` 环境变量管理它，需扩展该函数——本轮未动，记录待决。

### 3.5 本轮（2026-06-20 后续 fire）对抗式自审（flutter-reviewer）

对修复后的 `attachment_handler.dart` 做对抗式审查，逐条对抗式评估，**采纳真实缺陷、驳回既有/过度项**：

| 级别 | 发现 | 处置 |
|------|------|------|
| HIGH（实为 Web 正确性） | `_c2cConvKey` 用 `int.tryParse`，Flutter Web（dart2js 53 位 int）对 64 位 TSID 失准 → conv_key min/max 错序，与后端整数序契约不符 | ✅ 已修：改用 `BigInt.tryParse`，并补大 TSID 测试锁定 |
| HIGH（健壮性） | `deriveUploadScope` 仅靠 uk3 前缀，`options['conversationUk3']` 传非标值会静默回退 private | ✅ 已修：增加 `type` 权威参数（chat_page 注入 `_chatType`），uk3 仅兜底 |
| MEDIUM | 测试缺 C2S / type 权威 / 大 TSID 用例 | ✅ 已补，共 11 用例 |
| HIGH（驳回） | `_currentUser` 可能抛 `StateError`、`on Object catch`、`debugPrint` 上报 | ⛔ 驳回：均为**改动前既有模式**，非本次引入；修它们会扩大 diff、违反"贴合周边代码"，仅在此标注为既有债务 |

> scope 回退到 `private` 是**fail-safe**（过度受限而非过度放行：最坏是"对方看不到图"，不会泄露），因此该 bug 是功能性可见性缺陷而非安全越权——但仍必须修，否则 c2c/group 上传链路名存实亡。

### 3.4 过渡期行为（设计内，非 bug）

- 本次改造前用 presign 上传的头像（object_key 在 **private** 桶）+ 旧 go-fastdfs 头像（完整 URL），在新渲染下会 404/走兜底 → 显示默认头像。这是设计 §1.2「旧图不迁移，用户重传」的既定行为，由 runbook 步骤 4 批量置空旧 avatar 引导重传兜底。

---

## 4. 绿灯门结果 / Green-Gate Results

### analyze（`flutter analyze lib`）

| 指标 | 基线（改前） | 改后 | 结论 |
|------|------|------|------|
| 全量 issues | 8（lib 范围；另全工程含 test 共 89，多为 test `avoid_print` info） | 8 | 持平 |
| 我**触及文件**告警 | — | **0** | ✅ 触及文件零告警、零新 error |

> 改后 8 issues 全部位于**未触及文件**（`mine_routes.dart`、`chat_provider.dart`、`withdraw_page.dart`、`e2ee_shard_message_handler.dart`），均为既存债务，非本次引入。

### flutter test

| 范围 | 结果 | 说明 |
|------|------|------|
| **针对性单测**（attachment_upload_presign / attachment_api / attachment_handler / assets_object_key / asset_url_resolver） | **38/38 全绿** | 含新增 public/c2c scope 透传测试；直接覆盖本次改动代码路径 |
| **本轮新增** `attachment_upload_scope_test` + 既有 `attachment_handler_test` | **16/16 全绿**（11 新 + 5 既有） | 锁定 `deriveUploadScope` 契约；触及文件 `flutter analyze` **零问题** |
| 全量 `flutter test` | +4109 通过 / ~94 跳过 / **36 失败** | 36 失败全为**既存环境性失败**：`test/api/*`（需活后端 127.0.0.1:9800）、`test/integration/*`（无头 UI 流程，已知不兼容，见项目记忆）、`e2ee_*`（加密环境敏感）、`quick_reply`。**经导入分析确认均不依赖本次改动代码路径**；唯一涉及 `/v1/init` 的 `auth_api init_config` 仅断言 `code==0` 且需活后端，与新增字段无关。 |

> ⚠️ 未做"git stash 全量基线对照"：工作树混有前次会话未提交改动，stash 风险高且会破坏你晨审的工作树（见项目记忆关于 auto-stage/sandbox 的踩坑）。改用**导入依赖分析**证明 36 个失败与本次无关。如需铁证可手动：`git stash && flutter test <那些文件> && git stash pop`。

### 后端

- `make compile` 通过（exit 0，无 index_handler 相关 error/warning）；`elib_oss:public_base_url/0` 已导出存在。
- 未跑全量 eunit（改动为单行 map 字段添加，编译即证）。建议你发布前 `make eunit` 兜底。

---

## 5. 阶段二真机功能验收 Checklist（早上点验，禁模拟器）

> 真机验收无法自动化，以下需你在真机逐项点验。**前提**：阶段三 runbook 已执行（Garage 公开读 + nginx + 后端下发 public_base_url 生效），否则头像公开直读会失败。

- [ ] **A 看 B 头像**：A、B 两台真机不同账号，B 设置新头像 → A 在通讯录/聊天/名片处能看到 B 头像（公开直读，无 403）
- [ ] **自己头像**：本人在「我的」「个人资料」页头像正常显示（已迁 avatarImageProvider 直读）
- [ ] **换头像一致性**：换头像后，本人与他人侧均更新为新头像
- [ ] **群成员头像**：群聊/群成员列表（avatar_group）头像正常显示
- [ ] **@提及列表头像**（mention_list）正常显示
- [ ] **单聊图片/文件/视频/语音**：仅会话双方可见；用第三方账号（非双方）尝试访问其 object_key 的 view_url → 403
- [ ] **群图片/文件**：仅群成员可见；非群成员访问 → 403
- [ ] **历史消息附件**（迁移前的旧完整 URL 图片）仍能正常渲染（legacy view_url 路径保留）
- [ ] **裁剪上传头像**（个人资料页点头像→裁剪→保存）走 public，保存后双方可见
- [ ] **弱网**：受限资源 600s 短签发 URL 加载正常，无频繁失败
- [ ] **旧头像用户**：runbook 步骤 4 置空后，登录显示默认头像，重传后双方可见

> ⚠️ **已知本期未覆盖（验收时会发现，属预期）**：频道封面、个人背景图 仍是 private 上传 → 跨用户可能看不到（同原头像 bug，留作后续任务，见 §3.2）。

---

## 6. 阶段三 Runbook 执行提示 / Phase 3 Pointer

- 路径：[`resource-access-control-phase3-runbook.md`](./resource-access-control-phase3-runbook.md)
- 6 步：①Garage website+imboy-public 公开读 ②nginx s3.imboy.pub 公开路由 ③后端下发 public_base_url（重启生效）④旧 avatar 批量置空（**先备份**）⑤下线 i/a.imboy.pub ⑥全链路验证。
- 每步均含**验证 + 回滚**。**所有命令未执行**，留你本人逐条审阅后执行。
- 强烈建议执行顺序：先 ③（后端下发）→ ①②（公开读通路）→ 真机验收 §5 → 再 ④⑤（不可逆的置空/下线）。

---

## 7. 遗留风险与待决问题 / Risks & Open Questions

| # | 项 | 等级 | 说明 / 待你决定 |
|---|----|------|------|
| R1 | **public_base_url 来源** | 待决 | 已采用"后端下发+硬默认"。生产值若非 `https://s3.imboy.pub` 必须确保下发生效；是否需要 `IMBOY_*` 环境变量覆盖（需扩展 `override_garage/1`）请定夺。见 §3.3 |
| R2 | **频道封面/背景跨用户不可见** | 中 | 本期越界未改，仍 private。需独立任务把其上传改 public + 渲染走 avatarImageProvider。见 §3.2 |
| R3 | **历史消息附件迁移** | 中 | 旧完整 URL 附件仍靠 viewUrl/uploadScene 渲染，故这些 legacy 代码**不能删**。彻底下线 go-fastdfs 渲染需先迁历史附件，超出本期 |
| R4 | **过渡期头像 404** | 低（设计内） | 改造前的 private 头像/旧 URL 头像会显示默认头像，靠 runbook 步骤 4 + 用户重传收敛 |
| R5 | **publicUrl 未对 object_key 做 percent-encoding** | 低 | 头像 object_key 由后端生成（`u<uid>/avatar/<Ymd>/<hex>.<ext>`，无用户文件名、无特殊字符），且 `user.avatar` 经 confirm 的 owner_of_key 校验，注入面极小。如未来 public 资源含用户文件名，需补分段编码 |
| R6 | **c2c scope_ref 顺序** | 低 | 客户端传整串 conv_key（`c2c:a:b`），后端 `conv_key_vo:c2c/2` 会归一化为 min:max；`conversationUk3` 生成侧通常已归一。双保险但未在客户端再归一，记录 |
| R7 | **全量 flutter test 36 失败基线** | 信息 | 均为既存环境性失败（活后端/无头 UI/加密），非本次引入。建议把这些 integration/api 测试纳入"需真机/活后端"标记，避免污染 CI 判读 |
| R8 | **后端死接口 presign_put/3** | 低 | 零调用，可删，本轮未动（避免扩后端 diff）。可随其他后端清理一并移除 |
| **R9** | **⚠️ garage.endpoint 必须=真机可达公网 S3 host**（2026-06-20 真机暴露） | **阻断** | presigned `put_url` 的 host **就是** `garage.endpoint`，生产默认 `http://127.0.0.1:3900` 真机不可达 → "上传地址不是 s3.imboy.pub / 上传失败"。S3v4 签名含 host，**不能 nginx 改写**。必须设 `IMBOY_GARAGE_ENDPOINT`/`garage.endpoint` = `https://s3.imboy.pub`（或独立 `s3api.imboy.pub`）+ nginx 按 `X-Amz-` 分流。**这是所有 scope 上传（不止头像）的共性前置**。详见 runbook 步骤 1.5（本轮已补） |

> **2026-06-20 真机回归结论**：阶段二客户端代码经真机日志验证**正确**（presign 已带 `scope=public`）。当前真机失败（`i.imboy.pub` 头像 401、上传地址非 s3.imboy.pub）**全部源于阶段三生产配置未执行**：①旧头像未迁移（i.imboy.pub 已停用）②`garage.endpoint` 仍为内网默认 ③Garage 公开读 + nginx s3.imboy.pub 未配。**头像/附件端到端必须先执行阶段三 runbook（尤其新增的步骤 1.5）才能在真机工作。**

---

## 8. 复现/验证命令 / Repro Commands

```bash
# 客户端 analyze（触及文件零告警）
cd ~/project/imboy.pub/imboyapp && /Users/leeyi/dev/flutter/bin/flutter analyze lib

# 针对性单测（38 全绿）
/Users/leeyi/dev/flutter/bin/flutter test \
  test/store/attachment_upload_presign_test.dart \
  test/store/attachment_api_test.dart \
  test/page/chat/chat/attachment_handler_test.dart \
  test/service/assets_object_key_test.dart \
  test/service/asset_url_resolver_test.dart

# 后端编译
cd ~/project/imboy.pub/imboy && make compile

# 查看本次工作树改动
cd ~/project/imboy.pub/imboyapp && git diff --stat
cd ~/project/imboy.pub/imboy && git diff --stat
```
