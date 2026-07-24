# 资源访问控制架构设计 / Resource Access Control Architecture

> **状态 / Status**: 设计已确认，待实现 / Approved, pending implementation
> **创建 / Created**: 2026-06-19
> **范围 / Scope**: 读鉴权一次性覆盖六种访问级别（`authorize` 六分支全就位）—— public（头像/表情）、private、c2c（单聊）、group（群聊）、channel（频道付费）、moment（个人动态/朋友圈）；上传链路本期仅 public/private/c2c/group，channel/moment 待功能上线再补
> **背景 / Background**: 弃用旧 `i.imboy.pub` + go-fastdfs，全面切换到 Garage S3；头像改为公共资源

---

## 1. 背景与问题 / Background

### 1.1 触发问题

- 旧附件域名 `i.imboy.pub`（go-fastdfs 存储）废弃，新链路走 Garage S3。
- 头像被错误地当作**私有附件**处理：B 的头像 object_key 是 `uB/...`，`creator_user_id=B`；A 调 `view_url` 时 SQL 条件 `creator_user_id=A` 查不到 → **403 forbidden** → "别人的头像看不到"。
- 头像本质是**公共资源**（任何登录用户都要能看），不该走归属校验。

### 1.2 决策

- **旧图不迁移**：用户重新上传（仅头像作为公共资源重新落地）。
- **访问方式**：方案 P —— 头像走**公开读 bucket**，存完整公开 URL，客户端直读、可 CDN，不调 `view_url`。
- **通用性**：设计一套通用资源访问控制模型，覆盖 public/private 及未来 c2c/group/channel/moment。

---

## 2. 设计目标 / Goals

一套模型覆盖所有资源访问级别，新增级别只需在**一个鉴权函数**（`authorize/N` dispatch）加一个分支。

**本期实现范围（按 YAGNI 分期）**：`authorize/2` 读鉴权 dispatch **一次写全六个分支**（边际成本极低，正是本模型"加分支即扩展"的价值兑现）；但**上传链路（presign/confirm/can_upload）+ 客户端 + EUnit 本期只做 public/private/c2c/group**。`channel`/`moment` 待其附件功能真正上线时再补上传侧，届时 dispatch 分支已就位。

| 访问级别 (scope) | 谁可读 | 典型资源 | 本期读鉴权 | 本期上传链路 |
|---|---|---|---|---|
| `public` | 任何人（含未登录），可 CDN | 头像、表情、频道封面、Logo | ✅ | ✅ |
| `private` | 仅本人 (owner) | 个人收藏、草稿、私密文件 | ✅ | ✅ |
| `c2c` | 会话双方 | 单聊图片/视频/文件 | ✅ | ✅ |
| `group` | 群成员 | 群聊图片、群文件、群相册 | ✅ | ✅ |
| `channel` | 频道订阅者（须含有效期/退订判断） | 频道付费内容 | ✅（分支就位） | ⏳ 待功能上线 |
| `moment` | 按动态可见性规则（公开/好友/指定分组/私密） | 个人动态（朋友圈）图片/视频 | ✅（分支就位） | ⏳ 待功能上线 |

---

## 3. 归属模型 / Ownership Model

资源归属是**两件独立的事**，分开设计：

| 归属层面 | 解决什么 | 实现机制 |
|---|---|---|
| **写归属**（上传权 write） | 谁能上传/覆盖该文件 | object_key 强制带 `u<Uid>/` 前缀，`confirm` 时 `owner_of_key` 校验前缀 == 当前 uid → **只能传到自己命名空间，不能伪造/覆盖别人** |
| **读归属**（访问权 read） | 谁能看该文件 | 按资源 `scope` 决定：public=任何人，private=仅 creator，c2c/group=范围成员，moment=动态可见关系 |

- **写归属对所有资源统一**（命名空间保护，现有 `owner_of_key` 已实现）。
- **读归属按 scope 区分**（新增 `authorize/N` 收口）。

---

## 4. 存储分层 / Storage Layering（双桶）

Garage 公开读是 **bucket 级**（website 模式），同 bucket 内无法按前缀分公私 → **物理分桶**：

```
imboy-public  (公开读 bucket)  → nginx 直接公开代理，可挂 CDN     → 放 public 资源
imboy         (私有 bucket)    → 后端 view_url 签发短时 URL（TTL 按 scope 分级）→ 放 private/c2c/group/channel/moment
```

- **public 资源**：存**完整公开 URL** `https://s3.imboy.pub/<key>`，客户端直读，不调 view_url。
- **受限资源**：存 object_key，客户端走 `AssetUrlResolver` → `view_url` 鉴权后签发短时 URL。

---

## 5. object_key 命名规范 / Naming

**所有 key 的第一段必须是 `u<Uid>/`** → 写归属（`owner_of_key`）统一生效。

> ⚠️ **关键约束**：现有 `owner_of_key/1`（`elib_oss.erl:124`）以 `<<"u", Rest/binary>>` 模式匹配，**只认开头第一段的 `u<Uid>/`**。因此 `u<Uid>/` 必须永远放在 key 的第一段；`<Ymd>`、`<category>`、scope 段一律放在其**之后**，`owner_of_key` 零改动即可复用。

```
public:   u<Uid>/avatar/<Ymd>/<hex>.<ext>              例 u52278/avatar/20260620/a3f5.jpg
private:  u<Uid>/file/<Ymd>/<ts>_<hex>/<name>
c2c:      u<Uid>/c2c/<Ymd>/<ts>_<hex>/<name>           scope_ref = c2c:<minUid>:<maxUid>
group:    u<Uid>/g<Gid>/<Ymd>/<ts>_<hex>/<name>        scope_ref = <group_id>
channel:  u<Uid>/ch<Cid>/<Ymd>/<ts>_<hex>/<name>       scope_ref = <channel_id>
moment:   u<Uid>/m<MomentId>/<Ymd>/<ts>_<hex>/<name>   scope_ref = <moment_id>
```

> - **所有 scope 统一带 `<Ymd>` 日期目录**（放 `u<Uid>/` 之后），便于按天分区做生命周期管理、冷归档、运维排查、避免单目录文件过多。
> - public 资源放 `imboy-public` 桶；其余放 `imboy` 桶。
> - 不引入独立的 `category` 维度（YAGNI）：当前 public 仅头像一种，直接用 `avatar` 段即可，待第二种 public 资源出现再泛化。

---

## 6. attachment 表扩展 / Schema

object_key 编码不足以鉴权（群成员要查 group_member 表），需落库归属元数据：

| 现有字段 | 新增字段 | 含义 |
|---|---|---|
| `id` (TSID) | | 主键 |
| `creator_user_id` | | 写归属/owner |
| `path` | | object_key |
| `url` | | 同 object_key（历史冗余，未来可合并） |
| `md5`, `mime_type`, `size`, `referer_time`, `status` | | 现有 |
| | `scope` | `public` \| `private` \| `c2c` \| `group` \| `channel` \| `moment`（决定读鉴权策略） |
| | `scope_ref` | scope 绑定实体：group→group_id，c2c→conv_key，channel→channel_id，moment→moment_id，public/private→NULL |

> **精简设计**：原设计中的 `bucket` 字段存在冗余，在 Schema 中予以**删除**。因为 bucket 可完全由 `scope` 派生（即 `scope =:= public` 选 `imboy-public` 桶，其他一律选 `imboy` 桶），在业务层动态判断即可，保持数据库模型纯粹。
> **所有 scope 统一落 attachment 表**（含 public）。落库是为了**统一管理**——存储配额统计、孤儿文件清理、合规删除（删用户时一并清其所有资源）、运维审计需要单一数据源。
> **落库 ≠ 鉴权**：是否落库与读取走哪条路由正交。public 落库后，读取仍直读公开 URL（不调 view_url）；受限资源读取走 view_url 鉴权。两者由 `scope` 决定，与落库无关。

---

## 7. 统一上传流程 / Upload Flow

```
① presign:  GET /v1/attachment/presign?filename=x.jpg&mime_type=image/jpeg&scope=public[&scope_ref=<id>]
       后端: 校验类型白名单
            → 校验 Uid 有权向该 scope 上传（如 group → 必须是该群成员）
            → 按 scope 选桶 + 生成 object_key（带 u<Uid>/）
            → 返回 { put_url, object_key, public_url(仅 public), expires_at }

② PUT:      客户端裸 HTTP PUT 直传 put_url（不带 JWT）到 Garage

③ confirm:  POST /v1/attachment/confirm  { object_key, md5, scope[, scope_ref] }
       后端: owner_of_key 校验写归属（u<Uid>/ == 当前 uid）
            → HEAD 核实真实 size/mime（覆盖客户端自报）
            → 落 attachment 表（creator_user_id, path, scope, scope_ref）
            → public 资源返回 public_url
```

**安全要点**：confirm 必须校验声明的 scope 权限（A 不能把资源传到自己不属于的群），防止伪造可见范围。

**各 scope 上传权校验（confirm 阶段，`can_upload(Uid, Scope, ScopeRef)`）**：

```
public  -> u<Uid>/ 命名空间匹配即可（本人）
private -> 同上
c2c     -> Uid ∈ parse_conv(ScopeRef)        % conv_key = c2c:<minUid>:<maxUid>，上传者必须是会话方
group   -> group_member_ds:is_member(Uid, ScopeRef)    % 必须是该群成员
channel -> channel_admin_ds:can_publish(Uid, ScopeRef) % 必须有频道发布权（主/管理员）
moment  -> u<Uid>/ 命名空间匹配即可（动态发表者为本人，上传时关联此 moment_id）
```

---

## 8. 统一读取鉴权与安全强化 / Read Authorization & Security Hardening

受限资源下载走 S3 预签名 URL，**统一 TTL = 600 秒**：

| scope 类别 | S3 预签名失效时间 (TTL) | 说明 |
|---|---|---|
| `public` | 不签发（直读公开 URL，可 CDN 缓存） | 公开资源，无需鉴权 |
| `private` / `c2c` / `group` / `channel` / `moment` | **600 秒** (10分钟) | 受限资源统一短时签发，兼顾加载缓冲 |

> **不做 TTL 分级（去过度设计）**：原设计对 channel/moment 用 120s「阻断外传」属安全剧场——预签名 URL 在 120s 内同样可被复制转发，缩短 TTL 的边际安全收益≈0，却换来弱网下加载失败/重试与客户端差异化缓存的真实成本。真正防外传应靠水印 / 一次性 token / 审计，而非缩 TTL。故统一 600s。

### 8.1 鉴权逻辑实现与避坑细节

```
view_url(Uid, ObjectKey):
  rec = attachment.find_by_path(ObjectKey)           % 取 scope, scope_ref, creator
  if authorize(Uid, rec):
       Bucket = case rec.scope of
                    <<"public">> -> imboy_public;
                    _            -> imboy
                end,
       presign_get(Bucket, ObjectKey, 600)            % 统一 TTL 600s
  else forbidden

authorize(Uid, #{scope, scope_ref, creator}):
  public  -> true                                      % (一般不走 view_url，公开 URL 直读)
  private -> Uid == creator
  c2c     -> Uid ∈ parse_conv(scope_ref)               % conv_key = c2c:<minUid>:<maxUid>

  % 【关键修正 0】：group_member_ds:is_member 参数顺序为 (Gid, Uid)，Gid 在前
  % 原设计写为 (Uid, scope_ref) 参数相反，此点已根据代码基线（group_member_ds.erl:46）修正
  group   -> group_member_ds:is_member(binary_to_integer(scope_ref), Uid) % 复用群成员判断
  
  % 【关键修正 1】：避坑 channel_subscription_ds:is_subscribed 参数顺序为 (ChannelId, Uid)
  % 原设计写为 (Uid, scope_ref) 为参数相反，此点已根据代码基线修正
  channel -> 
      ChannelId = binary_to_integer(scope_ref),
      channel_subscription_ds:is_subscribed(ChannelId, Uid) % 实时进行订阅状态 + 订阅有效期/退订状态校验
  
  % 【关键新增 2】：朋友圈动态鉴权，直接将鉴权下沉给已实现 can_view_post 规则的 moment_ds
  moment  ->
      MomentId = binary_to_integer(scope_ref),
      case moment_ds:get_post(MomentId) of
          Post when is_map(Post), map_size(Post) > 0 ->
              moment_ds:can_view_post(Uid, Post);  % 完美支持公开/好友/指定分组/私密的可见性匹配
          _ ->
              false
      end
```

**六种分支全量在 attach_logic 实现**，复用现有 ds：`group_member_ds`（群成员）、`channel_subscription_ds`（频道订阅及有效期判定）、`moment_ds`（朋友圈可见性与 ACL）、conv_key 解析。

---

## 9. 头像公共化链路 / Avatar (scope=public)

| 环节 | 设计 |
|---|---|
| **桶** | 公开读桶 `imboy-public`（Garage website 模式，nginx 公开代理，可 CDN） |
| **object_key** | `u<Uid>/avatar/<Ymd>/<hex>.<ext>`（`u<Uid>/` 第一段，保证写归属 + 兼容 `owner_of_key`） |
| **落库** | 统一落 attachment 表（scope=public），供配额/清理/审计 |
| **user.avatar 存哪（方案 B）** | 存 **object_key**（非完整 URL、非 attachment_id）。attachment 表为管理真相源，user.avatar 只持有引用 |
| **读/渲染** | 头像渲染器**直接拼接** `public_base_url + object_key` 直读，**不调 view_url、不校验读归属、可 CDN**。拼接为纯字符串、零 DB 查询（头像高频读关键） |
| **客户端分流** | ⚠️ public 与受限资源 object_key **形态相同**（均 `u<Uid>/`），通用 `isObjectKey` 无法区分。靠**字段语义分流**：`user.avatar` 走头像专用直读逻辑；消息附件走 `AssetUrlResolver → view_url`。两条渲染路径分开，头像不挤进通用判断 |
| **A 看 B 头像** | ✅ 由 B 的 object_key 拼公开 URL 直读，无归属阻拦 |
| **换头像一致性** | confirm 落新 attachment 行 + 更新 `user.avatar` = 新 object_key + 旧行 `status` 置可回收（孤儿清理异步处理）。头像更新统一收口 confirm 链路，禁止旁路写 `user.avatar` |

---

## 10. 改造清单 / Change List

### 10.1 后端（imboy）

| 文件 | 改动 |
|---|---|
| `src/lib/elib_oss.erl` | 多桶支持（根据 scope 动态映射 `get_bucket/1`）；`build_object_key/3`（`u<Uid>/` 第一段 + scope 段 + `<Ymd>`，**不引入 category 维度**）；`public_base_url/0` + `public_url_for_key/1`；`get_endpoint`/签名按桶。⚠️ `owner_of_key/1` 无需改（key 第一段仍是 `u<Uid>/`） |
| `src/api/attach_handler.erl` | `presign` 收 `scope`/`scope_ref` query 参数；`confirm` 收 `scope`/`scope_ref` |
| `src/logic/attach_logic.erl` | `presign/N` 按 scope 选桶+命名空间+返回 public_url；`confirm/N` **统一落 attachment 表（含 public）** + `can_upload/3` 上传权校验（本期 public/private/c2c/group；channel/moment 待功能上线再补）；`view_url/2` 统一调用 `authorize/2`（**六大 scope 分支全就位**，TTL 统一 600s，无分级） |
| 复用 ds | `group_member_ds:is_member/2`（**参数顺序 `(Gid, Uid)`，Gid 在前**）、`channel_subscription_ds:is_subscribed/2`（参数 `(ChannelId, Uid)`）、`moment_ds:can_view_post/2` 等 |
| `src/repo/attachment_repo.erl` | `save` 写入 scope/scope_ref（取消冗余 `bucket`）；`find_by_path/1`（不带 uid，鉴权在 logic 层做） |
| `src/ds/attachment_ds.erl` | 适配新字段 |
| `priv/migrations/*.sql` | attachment 加 `scope/scope_ref` 列（带默认值 private 兼容存量，无冗余 `bucket` 列） |
| 配置 | `garage` 配置加 `public_bucket`、`public_base_url`（`https://s3.imboy.pub`） |

### 10.2 客户端（imboyapp）

| 文件 | 改动 |
|---|---|
| `lib/store/api/attachment_api.dart` | 头像上传 presign 带 `scope=public`；confirm 后将返回的 **object_key** 作为 `user.avatar` 值（方案 B） |
| `lib/page/personal_info/profile/profile_provider.dart` | 头像上传链路传 scope=public，存 **object_key**；统一走 confirm，禁止旁路写 avatar |
| `lib/service/assets.dart` | 头像渲染器：`user.avatar`（object_key）→ 直接拼 `public_base_url + key` 直读，**不调 view_url、不加签名** |
| 显示分流（关键） | 靠**字段语义**而非值形态：`user.avatar` → 头像专用直读；消息附件 object_key → `AssetUrlResolver → view_url`。⚠️ 不要再用通用 `isObjectKey` 判断头像（public 与受限 key 形态相同，无法区分）。受限媒体客户端不做磁盘级长期缓存 |

### 10.3 服务器配置（生产）

| 项 | 改动 |
|---|---|
| Garage | `/etc/garage.toml` 加 `[s3_web]`（website 端点）；`garage bucket website --allow imboy-public`；重启 Garage |
| Garage bucket | `imboy-public` 配公开读；`imboy` 保持私有 |
| nginx | `s3.imboy.pub` 路由：`/public/*`（或公开前缀）→ Garage web 端点（公开读）；其余 → 3900（私有，靠 presign 签名） |
| nginx | `i.imboy.pub` / `a.imboy.pub`：用户重传完成后下线 go-fastdfs |

### 10.4 数据库

- 旧 `user.avatar` 含 `i.imboy.pub` 完整 URL 的记录：批量置空（触发默认头像 `def_avatar.png`），引导用户重传。

### 10.5 旧逻辑删除（最后做，确认新链路稳定后）

- 客户端：`attachment_api.dart` 旧 go-fastdfs `_upload/preUpload/uploadVideo`（488–891）；`env.dart` `uploadUrl/uploadScene`（108/111）；`init.dart` `upload_url/upload_scene` 存取（493–506）；`assets.dart` go-fastdfs HMAC 签名 `viewUrl`（147–195）；`message_s2c.dart` 硬编码 `a.imboy.pub`（563/571）
- 后端：`elib_oss` 遗留 `presign/3`（不含 uid 前缀的旧接口）

---

## 11. 分阶段实施 / Phased Plan

> 实施顺序：**后端 → 客户端 → 服务器配置**

1. **阶段一 · 后端**
   - DB 迁移：attachment 加 `scope/scope_ref`（默认 private，兼容存量）
   - `elib_oss` 多桶 + public 命名空间（`u<Uid>/` 第一段 + scope 段 + `<Ymd>`）+ public_url；`owner_of_key` 不变
   - `attach_logic`/`attach_handler`：presign 收 scope/scope_ref、confirm **统一落库（含 public）** + `can_upload/3`（本期 public/private/c2c/group）、view_url 统一 `authorize`（**六分支全就位，TTL 统一 600s**）
   - 配置加 public_bucket/public_base_url
   - EUnit 覆盖 presign/confirm/view_url：上传侧覆盖 public/private/c2c/group；`authorize` 六分支全覆盖（channel/moment 用 meck mock `channel_subscription_ds`/`moment_ds`）
2. **阶段二 · 客户端**
   - 头像上传带 scope=public，存 **object_key**（方案 B）；头像渲染器直拼 `public_base_url + key` 直读（不调 view_url、不加签名），与消息附件渲染路径分开
   - 聊天图片/文件上传带 scope=c2c/group（+scope_ref），显示走 view_url
   - 真机验证：A 看 B 头像、群图仅群成员可见、单聊图仅双方可见
   - （channel/moment 上传待其功能上线时再补）
3. **阶段三 · 服务器配置**
   - Garage website + imboy-public 公开读 + 重启
   - nginx s3.imboy.pub 公开路由
   - 旧 avatar 置空 + 用户重传验证
4. **阶段四 · 清理**（确认稳定后）
   - 删除 go-fastdfs 旧链路代码 + 下线 i.imboy.pub/a.imboy.pub

---

## 附录 A：现状代码事实基线 / Code Facts Baseline

> 实现前的权威基线（2026-06-19 通过代码审计确认），供对照改造。

### A.1 后端 object_key / 归属

- `build_object_key(Uid, FileName)` → `u<Uid>/file_<ts_ms>_<hex16>/<SafeName>`（`elib_oss.erl`）
- `owner_of_key/1`：从 `u<Uid>/` 解析 owner uid，confirm 越权守卫用
- `presign` PUT 有效期 3600s；`view_url` GET 有效期 600s（`attach_logic.erl`）
- `view_url` 归属 SQL（`attachment_repo.erl`）：
  ```sql
  SELECT id, path, creator_user_id FROM public.attachment
  WHERE path = $1 AND creator_user_id = $2 AND status >= 0 LIMIT 1
  ```
  → **仅 owner 能签发下载 URL**（这是"别人头像看不到"的根因）
- bucket 当前**私有读**，无公开读配置
- `confirm` 落库字段：creator_user_id, path, url(=path), md5, mime_type(HEAD实测), size(HEAD实测), referer_time, status
- 头像无专用路由，走通用 `/v1/user/update`（field=avatar），`check_avatar` 仅兜底默认值，不转换 URL（`user_logic.erl`）
- 文件类型白名单 26 种；最大 100MB（`elib_oss.erl`）

### A.2 客户端资源链路

- 新链路（主路径）：`uploadViaPresign` → `GET /v1/attachment/presign` → 裸 Dio PUT → `POST /v1/attachment/confirm` → 返回 object_key（`attachment_api.dart:104-184`）
- `isObjectKey`：正则 `^u\d+/`，含 `://` 一律 false（`assets.dart:97-106`）
- object_key 显示：`AssetUrlResolver.resolve` → `GET /v1/attachment/view_url?object_key=` → 600s presigned URL，客户端 540s TTL 缓存（`asset_url_resolver.dart:103-118`）
- 头像上传：`uploadFileViaPresignCompat('avatar', ...)` → 存 **object_key** 到 `user.avatar`（`profile_provider.dart:343`）
- 旧 go-fastdfs 链路：`_upload/preUpload/uploadVideo`（`attachment_api.dart:488-891`）保留但无正常调用点
- `viewUrl` go-fastdfs HMAC 签名（`assets.dart:147-195`）：`tk = md5("$uploadKey$v").substring(8,24)`，仅对完整 http URL 生效
- 空签名 `?s&a&v=` 根因：`uploadKey` 为空（initConfig 未完成/失败 / Keychain 缺失）
- 硬编码域名残留：`message_s2c.dart:563,571`（测试数据 `a.imboy.pub`）

---

## 附录 B：关键决策记录 / Decisions

| 决策 | 选择 | 理由 |
|---|---|---|
| 头像访问方式 | 方案 P：公开读 bucket | 简单、可 CDN、A 看 B 头像无归属阻拦 |
| 存储隔离 | 双桶（public/private 物理隔离） | Garage 公开读是 bucket 级，同桶无法分公私 |
| 实现范围 | `authorize` 六分支全就位；上传链路本期仅 public/private/c2c/group | 读鉴权加分支成本极低，一步到位；channel/moment 上传侧 YAGNI，待其附件功能上线再补 |
| 统一落库 | 所有 scope（含 public）统一落 attachment 表 | 单一数据源便于配额/清理/审计/合规删除；落库≠鉴权，public 读取仍直读 |
| user.avatar 存值 | 方案 B：存 object_key（非完整 URL/非 id） | 单一数据源无冗余；渲染拼 `public_base_url+key` 零查询直读，与现状基线一致 |
| 客户端显示分流 | 按字段语义（avatar 字段 vs 消息附件），非 object_key 值形态 | public 与受限 key 形态相同，`isObjectKey` 无法区分 |
| 下载 TTL | 受限资源统一 600s，不分级 | 缩短 TTL 防外传是安全剧场，边际收益≈0 却增客户端复杂度 |
| object_key 命名 | `u<Uid>/` 第一段 + scope 段 + `<Ymd>`；不引入 category | `u<Uid>/` 第一段兼容 `owner_of_key`；Ymd 利于生命周期；category YAGNI |
| 旧图迁移 | 不迁移，用户重传 | 旧 go-fastdfs 文件无 uid 归属信息，迁移成本高、价值低 |
| 写归属 | object_key `u<Uid>/` 第一段命名空间统一 | 复用现有 owner_of_key，防伪造 |
| 读归属 | scope 驱动的 authorize 收口 | 新增可见范围和敏感度判定只在 authorize dispatch 加分支 |
