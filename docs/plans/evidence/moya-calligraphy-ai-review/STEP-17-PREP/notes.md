# STEP-17-PREP imboy 附件端点形态记录（只读核验，给 B2/Coordinator 对照）

> 来源：`imboy/src/api/attach_handler.erl`、`src/logic/attach_logic.erl`、`src/repo/attachment_repo.erl`、`src/imboy_router.erl`（行 611/613/615）只读，未改 imboy 任何文件。
> 基线：imboy 工作区（HEAD 未动）；moya 9644a2e + 本波工作区变更。

## 路由

```text
GET  /api/v1/attachment/presign   attach_handler #{action => presign}   （JWT）
POST /api/v1/attachment/confirm   attach_handler #{action => confirm}   （JWT）
GET  /api/v1/attachment/view_url  attach_handler #{action => view_url}  （JWT）
```

## 1) presign

```text
GET /api/v1/attachment/presign?filename=x.jpg&mime_type=image/jpeg&scope=&scope_ref=
→ envelope.payload = { put_url, object_key, expires_at(epoch 秒) }
```

- put_url = S3 **presigned PUT**（elib_oss:presign_put_for_key，?PUT_EXPIRES 有效期）。
- object_key 由服务端 `build_object_key(Uid, Scope, ScopeRef, FileName)` 生成，Garage 键形如 `u<Uid>/...`。
- presign 同时登记 attach_pending（#20 孤儿回收），登记失败不阻断签发。
- 错误：invalid_file_type(400)/upload_not_supported(400)/forbidden(403)；can_upload 按 scope 校验。

## 2) confirm

```text
POST /api/v1/attachment/confirm
body = { object_key, file_hash256(或旧 md5 双读，可空), mime_type, size, cipher?, scope, scope_ref? }
→ envelope.payload = { object_key }   ← ⚠ 只有 object_key，没有 attachment id
```

- 服务端 HEAD 对象核实真实 size/mime（客户端自报仅参考）；超限/类型不符**直接删对象**并返回 file_too_large/invalid_file_type(400)。
- 落库幂等：attachment 表 `uk_attachment_path UNIQUE (path)` + INSERT ON CONFLICT（迁移 00000015/00000001）→ 同 object_key 重复 confirm 是 upsert。
- TSID 预生成（elib_tsid:generate(attachment)）但**不在响应里返回**。
- cipher 仅接受 null 或 "AES-256-GCM"（fail-closed）。
- scope=group/channel 时带 workspace 归档写守卫（archived → 980）。

## 3) view_url

```text
GET /api/v1/attachment/view_url?object_key=xxx
→ envelope.payload = { url }
```

- 每次**签发前重新鉴权**（authorize by scope/归属，fail-closed：查询失败也拒绝）。
- public scope → 公开直读 URL；旧 fastdfs `/path` 历史附件 → 回退存储 url；其余 → 短时 presigned GET（?GET_EXPIRES）。

## 联调 gap 清单（给 B2 / Step 10）

1. **confirm 不返回 attachment_id**：教学契约（submissions 创建）要求 assets[].attachment_id 为"confirm 落库的附件 ID"，但当前 confirm 只回 object_key。
   - moya 侧已按"Step 10 扩展 confirm 响应含 attachment_id"编程，并做了防御回退（从 `u<Uid>/...` 键首段提取数字）；**建议 B2 在教学 scope 的 confirm 响应补 `attachment_id` 字段**（或提供 object_key→id 查询）。回退值是 Uid 不是附件 ID，语义错误，仅防崩不可依赖。
2. **教学 scope 未定义**：can_upload 现支持 c2c/moment/private/public/group/channel；教学上传 scope（建议 `teaching`，moya 侧常量 TEACHING_SCOPE 已参数化，联调定名后只改一处）需要 Step 10 在 can_upload/教学 ACL 中注册。
3. **presigned PUT 与小程序限制**：wx.uploadFile 仅支持 POST multipart，无法直传 S3 presigned PUT。moya 默认传输采用 `fs.readFile → ArrayBuffer → wx.request PUT`；60s 视频约 30-60MB 的内存表现**必须真机验证**，若超限需要后端补 POST 直传端点（multipart 中转或 S3 POST policy）。
4. **file_hash256**：小程序无原生 SHA-256，moya 首版传空串（服务端仅作完整性参考、非安全边界，默认接受）；如需真实哈希再评估 JS 实现或 WASM。
5. **view_url TTL**：服务端 GET_EXPIRES 值未在 handler 层暴露给客户端；moya 侧保守 4 分钟本地缓存后重取（TEACHING_SCOPE 同文件常量区），联调时按真实 TTL 校准。
