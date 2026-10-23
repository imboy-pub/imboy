# 面聊记录后端数据结构与 API / Backend contract v5

修订10候选（2026-10-23）：**DRAFT_NOT_ACCEPTED**。用户已批准豆包 ASR 2.0 + 智谱 `glm-5.3-flash`；当前方案以 [DOUBAO-API.md](DOUBAO-API.md) 为异步增量合同、[design-preview-doubao.html](design-preview-doubao.html) 为候选交互原型。本文不是代码、真实供应商、真机或最终验收证明。仓外执行状态仍属修订9；主控制器审查、保留旧快照并显式迁移前，不得刷新旧证据为PASS。

用户已批准使用公开或合成样本进行真实 API 验收，本轮预算目标不超过人民币5元；Key仅在当前账号本设备填写并加密保存，不进IMBoy服务端、日志、云备份或设备迁移。此授权不扩展至真实用户录音、无限费用或部署；智谱年度Max不默认覆盖通用API额度。当前文档修改不执行任何调用。

日期：2026-10-22；业务命名统一为 `talk_record`。

状态：当前首版为App直连豆包 ASR 2.0 与智谱 `glm-5.3-flash`，Key仅在本设备加密保存。修订9的客户端执行补充见第12节；原服务端本地模型合同仅保留为后续可选部署路径，不能冒充已支持 BYOK。实际实现、未完成项与候选验收以仓外 execution-state.json 为准；本文不宣称已完成客户端执行、真机或真实供应商验收。新增迁移实施前核对各分支最新编号，不在设计中预占编号。

## 1. 领域与授权 / Domain and authorization

域名 `talk_record`，不混入 C2C/C2G 消息正文。organization 是业务归属对象，不是租户；部署实例提供隔离边界。每次读写必须满足记录 ACL，企业记录还需当前有效 organization 及所选 workspace 资源访问授权；单凭传入 org_id/workspace_id 不能授权。创建时服务器核验组合关系；记录建成后归属不可直接改为个人或其他企业。

个人记录 scope=personal，org/workspace 均 NULL；企业记录 scope=organization，organization_id 必填，workspace_id 仅在确实选中并有权的工作区时有值。默认两类都只有 owner 可见。服务器从 JWT 取 owner，忽略客户端冒充 owner 的字段。

| 主体 | 能力 |
|---|---|
| owner + 有效企业/工作区权限（如适用） | 读/录制/编辑/授权/删除；导出还受实例/企业策略限制 |
| 明确授予 editor + 有效归属权限 | 读/校对/编辑笔记；不可改 ACL 或删除原记录 |
| 明确授予 viewer + 有效归属权限 | 读/回听；导出须单独 allow_export=true |
| 名列参与者但没有 grant | 不自动获得访问 |
| 企业管理员、群成员 | 不因身份自动获得记录内容 |
| 模型执行者 | 有期限、单记录、指定输入/输出能力；服务端 worker 与客户端 BYOK 执行明确区分，均不能使用普通用户分享 token |

企业 V1 仅可授权本企业内且符合记录工作区权限的用户；个人 V1 可显式授权现有联系人。跨企业、公开匿名链接、通用群授权不在 V1；分享给群需后续明确定义成员变化语义，不伪装为已支持。

## 2. 逻辑表 / Logical schema

所有业务主键/引用为 bigint TSID，JSON 传输 int64 number，客户端使用项目现有无损解析；例子采用短数字仅为可读。时间 timestamptz/ISO8601；duration、位置统一 int64 毫秒。标注 * 为后续可选声纹能力，不在推荐 V1。

| 表 | 核心字段与约束 |
|---|---|
| talk_record | id PK; owner_user_id FK; client_recording_id UUID; UNIQUE(created_by,client_recording_id); created_by FK; source_type=live_recording/imported_audio/remote_call; scope enum; organization_id/workspace_id nullable FK; title varchar(120); language varchar(32); active_capture_id nullable FK; capture_status enum（当前采集状态投影）; media_status=local_only/uploading/verifying/assembling/available/retryable_failed; transcript_status enum; summary_status enum; processing_mode=record_only/transcribe/summarize; duration_ms>=0; audio_manifest_attachment_id nullable FK; current_transcript_id nullable FK; content_revision>=1; notes TEXT; revision>=1; acl_revision>=1; created_at/updated_at/ended_at/deleted_at; retention_until nullable; cleanup_status=none/pending/blocked_retention/blocked_hold/retained_by_other_reference/failed/complete; CHECK 个人/企业归属组合；活跃录制唯一约束落在 capture 表；不在此表存永久音频 URL |
| talk_record_participant | id PK; record_id FK; user_id nullable FK; display_name; attendance_source=selected/manual; UNIQUE(record_id,user_id) WHERE user_id NOT NULL；仅参与者信息，不等于授权或同意 |
| talk_record_grant | id PK; record_id; grantee_user_id; role=viewer/editor; allow_export bool default false; granted_by; revoked_at; expires_at nullable；活跃 UNIQUE(record_id,grantee_user_id)；不能移除最后 owner（owner 在主表） |
| talk_record_capture | id PK; record_id FK; device_session_id UUID; device_id; started_by FK; source_type; status=draft/recording/paused/interrupted/ended; reported_started_at/server_started_at/ended_at; duration_ms; revision; UNIQUE(record_id,device_session_id)；每 device 同时最多一条 active capture（partial unique）；record 同时只允许一条 active capture，active_capture_id CAS |
| talk_record_transcript | id PK; record_id FK; version int; source_manifest_hash; source_revision; language; status=queued/running/ready/no_speech/failed; model_version; created_at; UNIQUE(record_id,version)；每次模型重跑创建版本，发布前 CAS；人工修订由 segment revision 与 record.content_revision 追踪 |
| talk_record_capture_event | id PK; record_id; capture_id FK; device_session_id; client_event_id UUID; seq; event; position_ms; reported_at; received_at；UNIQUE(capture_id,client_event_id) 与 UNIQUE(capture_id,seq)；记录暂停、中断与离线重放顺序 |
| talk_record_audio_part | id PK; record_id; capture_id FK; seq int>=0; start_ms/end_ms; attachment_id; sha256; byte_size; codec/sample_rate/channels; upload_status；UNIQUE(capture_id,seq)；相同序号不同 hash 返回冲突；分段严格有序且无重叠，结束时校验完整清单 |
| talk_record_mark | id PK; record_id; author_id; client_mark_id; position_ms; text varchar(1000); created_at；UNIQUE(record_id,author_id,client_mark_id) 离线去重 |
| talk_record_speaker | id PK; record_id; local_label; display_name; matched_user_id nullable; identity_source=anonymous/self_claim/manual/voice_suggestion*; confirmed_by nullable; confidence nullable [0,1]; revision；UNIQUE(record_id,local_label)；自动建议不得直接变本人确认 |
| talk_record_segment | id PK; record_id; transcript_version; seq; speaker_id nullable; start_ms/end_ms; text; confidence; is_final; origin=model/human; revision; edited_by nullable；UNIQUE(record_id,transcript_version,seq)；CHECK end>=start>=0；speaker 与 segment 必须同 note，复合 FK；只查询当前可见版本 |
| talk_record_summary | id PK; record_id; source_transcript_version; source_content_revision; status=draft/confirmed/stale; overview; decisions JSONB; actions JSONB; model_version; revision; confirmed_by nullable；每条 decision/action 带 segment_ids 引用并服务端校验同记录；正文校对后旧摘要标 stale |
| talk_record_job | id PK; record_id; kind=assemble/transcribe/summarize/export/purge; execution_mode=server_local/client_byok（修订9新增）；input_version; dedupe_key UNIQUE; state=queued/running/retry_wait/succeeded/failed/cancelled; attempt; lease_owner/lease_until; retry_at; error_code; provider_request_id nullable; created_at/finished_at；转写最终发布与 note 当前版本 CAS 更新在事务内；服务端队列仅领取 server_local，不能抢手机任务 |
| talk_record_export | id PK; record_id; requester_id; format=txt/md; include_audio=false(V1); attachment_id nullable; job_id; state; expires_at；每次下载重验记录 ACL 与 export 权限，导出对象短期自动清理 |
| talk_record_consent_event | id PK; record_id nullable; actor_id; kind=recording_notice_ack/processing_opt_in/voice_enroll*/voice_withdraw*; policy_version; processing_mode; processor_config_version; captured_at；记录操作事实，不伪造现场全员同意 |
| talk_record_audit | id PK; record_id nullable; actor_id nullable; action; target_id; request_id; metadata_allowlist JSONB; created_at；记录授权/编辑/导出/删除/识别纠正，不写音频、原文、向量或外部授权 URL |
| voice_profile* | id PK; user_id UNIQUE active; embedding_ciphertext; key_version; model_version; quality_status; consent_event_id; status=enrolling/active/revoked/purge_pending/deleted; created_at/revoked_at；本人管理，不返回向量 |
| voice_profile_sample* | id PK; profile_id; attachment_id; quality_result; expires_at; purge_status；质量判定后删除原始采样，不作为普通附件对外分享 |

持久 talk_record_idempotency 保存 actor_id、method、resource、key、payload_hash、result_ref、expires_at，唯一键 actor+method+resource+key；不保存录音正文或签名 URL。notes 为跟随记录 ACL 的补充笔记，不是个人私密笔记。

公共 created_at、updated_at 与 FK 省略处按项目规范补齐。所有 note 子表 FK 删除由明确清理工作流处理，软删除后业务查询统一排除；事务写审计/outbox（可落同库持久 job，不假定现有队列已可用）。删除中 job 不能重新发布内容。

索引：note(owner_user_id,created_at DESC,id DESC) WHERE deleted_at IS NULL；grant(grantee_user_id,record_id) WHERE revoked_at IS NULL；note(organization_id,workspace_id,created_at DESC,id DESC) 仅作检索，不作为授权；segment(record_id,transcript_version,start_ms,id)；job(state,retry_at,lease_until)；mark(record_id,position_ms)。中文检索复用部署现有 pg_jieba/全文检索配置，先 ACL 缩小候选，再搜索；不将无权限命中数/摘要返回客户端。

## 3. 状态与一致性 / State machines

- capture：draft → recording ↔ paused → ended；中断→interrupted→paused（用户确认恢复），或→ended。每设备录音 session 单一写入者，其他设备可看但不能同时向它追加音频。
- media：local_only（客户端）→ uploading → verifying → available；失败→retryable_failed；丢段时返回 missing_sequences，不能标 available。manifest 固化在 complete 事务中。
- transcript、summary 分别：not_requested/unavailable/queued/running/ready/failed。record_only 不排队；文本为空返回 no_speech，不伪造总结。summary 必须绑定已成功转写版本。
- 记录删除→deleted_at + cleanup_status=pending，立即禁止正文 read 与新 download；任务 worker 取得结果时再次 CAS 检查未删除、输入版本未过时。purge 可退避重试，失败不能假 complete。
- 离线先本机分段/重点落盘；联网创建服务器 draft 并幂等补齐，客户端不得自行生成服务器 TSID。录音开始时间来自设备 reported_started_at，仅供显示；服务器时间用于审计。duration 以解码有效音频累加，排除暂停墙钟时间。
- 每个可变资源 ETag/revision；PATCH/删除/共享/complete 必须 If-Match，无头428，旧版本412。幂等动作重放先返回首次结果再校验版本，避免已成功 complete 的重试变412。

## 4. API 草案 / Proposed API

统一前缀 `/api/v1/talk-records`。JWT 必须；复用现有 success/error envelope，不另开一套认证。下表仅为新增契约草案。GET 列表使用 opaque cursor、limit 默认20最大100，稳定 created_at/id 倒序；segment/mark 为 start_ms/id 正序。cursor 绑定筛选和调用者，不能切 org 沿用。

| Method / path（相对前缀） | 请求/返回要点 |
|---|---|
| GET /capabilities | 部署支持的执行方式、公开 provider/model 策略版本、录音与请求限额；voice_id 在 V1=false 且无入口。不返回密钥，不宣称知道本设备是否配好 Key。企业供应商外发资格需在 creation-contexts 或具体记录上下文重验；关闭能力不能仅 UI 禁用 |
| GET / | scope、organization_id、workspace_id、filter=all/owned/shared、q、cursor、limit；只返回有权记录、各处理状态与可见范围；不扫描全企业记录 |
| POST / | client_recording_id(UUID)、title 可空、scope/归属、language、processing_mode、notice_ack=true、policy_version、reported_started_at；Idempotency-Key；201 note + revision；缺告知确认422；仅录音不要求转写同意 |
| GET /:id | 元数据、capabilities_for_actor、各状态、摘要版本标识、ETag；无权限或不存在统一404 |
| PATCH /:id | title/language（仅未开始处理时）/notes（共享补充笔记，可选，TEXT）；不接受 owner/scope 的直接修改；If-Match |
| POST /:id/captures | device_session_id、Idempotency-Key、If-Match；创建录制过程，返回 capture_id/revision；同一记录或同一设备已有活跃采集则409 |
| POST /:id/captures/:capture_id/events | client_event_id、device_session_id、seq、event=start/pause/resume/interrupt/end、position_ms、reported_at；幂等去重、顺序冲突409；更新 capture 状态并返回 revision；服务端不凭此宣称设备仍持有麦克风 |
| POST /:id/captures/:capture_id/audio-parts | 分段 seq/start_ms/end_ms/sha256/size/codec；Idempotency-Key；签发受限上传会话，返回 upload_ref、expiry 及既有附件服务所需上传信息；owner/合法 capture session 才可调用 |
| POST /:id/captures/:capture_id/audio-parts/:part_id/confirm | 上传完成回执；校验对象归属/真实大小/hash/类型；202 verifying 或200 confirmed；重复同 hash 安全重放 |
| POST /:id/complete | captures:[{capture_id,last_seq,manifest_hash}]；先校验内容与实际格式时长，缺段409 missing_sequences；202 media assembling+job_id。首版 BYOK 的录音保存不依赖设备 Key、供应商余额或推理任务；处理请求另走 processing-requests。历史 server_local 如支持自动入队，仍须有效处理同意 |
| GET /:id/jobs | 当前 assemble/transcribe/summarize/export/purge 的状态、可重试标记与归一化错误码；不回传外部服务原始异常/秘钥 |
| POST /:id/jobs/:job_id/retry | owner 或 editor（仅转写/摘要）+权限+If-Match；Idempotency-Key；只允许可重试失败；202；导出仅请求者，purge 由下述 record 域清理恢复端点重试，不授予普通 job retry 或全域运维能力 |
| POST /:id/processing-requests | 会后申请转写/摘要；明确 mode、execution_mode、公开 provider/model/config pin、处理同意、Idempotency-Key、If-Match。首版 client_byok 按第12节协调；未配置本地 executable 不能误判 BYOK 不可用。不借普通编辑隐式触发模型 |
| GET /:id/segments | cursor/limit/speaker_id/q；返回原文片段、稳定 ID、时间点与revision；q 命中返回高亮位置，不回全文扫描日志 |
| PATCH /:id/segments/:segment_id | text/speaker_id+If-Match；更新内容revision、标摘要 stale；保留模型原版在受控历史，不覆盖人工修订 |
| GET /:id/speakers | 匿名标签、人工姓名、绑定来源、可认领状态、revision；不暴露 voice embedding |
| PATCH /:id/speakers/:speaker_id | display_name、matched_user_id（editor 人工标记，目标须符合可见目录）；If-Match；服务端设 manual，不能由客户端指定 self_claim |
| POST /:id/speakers/:speaker_id/claim | 只允许本人 user_id（服务端从 JWT 取），选择示例片段确认；有记录访问权的现场参与者才可认领；已有他人认领返回409、走 owner 人工纠正并审计；认领记录不是生物认证 |
| GET /:id/marks / POST /:id/marks | 重点时间点；POST client_mark_id、position_ms、text；有效录音范围校验；离线幂等；手动编辑/删除使用 PATCH/DELETE /:id/marks/:mark_id+If-Match |
| GET /:id/summary | draft/confirmed/stale + overview/decisions/actions/source segment IDs +ETag；未生成返回状态对象，不伪装404 |
| PATCH /:id/summary | 人工编辑与 confirm=true；If-Match；source revision 不匹配409；仅确认文本，不自动建群任务/通知同事 |
| GET /:id/grants | owner 才可查全量 ACL；其他人只看自己的权限 |
| PUT /:id/grants | owner 提交全量受权用户/角色/export/expiry；If-Match，事务更新、审计；与并发删除互斥；不得授权超归属范围人员 |
| DELETE /:id/grants/:grant_id | owner 撤销；If-Match；立即使记录与附件授权失效 |
| POST /:id/share-card | owner 或可读者；只生成受控 `talk_record_card` 定位数据、不额外授权、不发消息；接收者打开仍需 ACL；发送行为由用户在 IM 确认 |
| POST /:id/playback | 校验 ACL，position_ms 可选；返回短时受控播放会话 URL/expiry；必须逐段/Range 重验授权或撤销版本，不暴露可长期直读的 S3 地址 |
| POST /:id/exports | format=txt/md、Idempotency-Key；有导出权限；202 export/job；不包含音频/未授权参与者信息 |
| GET /:id/exports/:export_id/download | 再验 ACL + requester，返回短时受控下载；到期410；不可向公共 bucket 开永久 URL |
| DELETE /:id | owner+If-Match，202 logical_deleted=true、cleanup_status=pending；立即撤权；处理器副本清除失败保留 purge_failed；GET 元数据仅 owner 可读清除状态、不能读正文 |

受控分享定位 `/talk-records/:id` 为未来 App 路由，未登录先登录再回原定位；非授权同事显示「暂无查看权限，请联系分享者」，不自动申请/发通知。匿名网页不显示标题、说话人或摘要。发卡片后不能靠卡片正文泄露完整摘要；预览标题须最小化并明示接收者权限，严格模式只发「面聊记录」定位。

### 示例 / Examples

```json
POST /api/v1/talk-records
Idempotency-Key: <uuid>
{
  "client_recording_id": "<uuid>",
  "title": "项目例会",
  "scope": "organization",
  "organization_id": 42,
  "workspace_id": 64,
  "language": "zh-CN",
  "processing_mode": "transcribe",
  "notice_ack": true,
  "policy_version": "notice-v1"
}
```

响应 data 示例（外层沿用项目 envelope）：
```json
{
  "id": 1001,
  "revision": 1,
  "capture_status": "draft",
  "media_status": "uploading",
  "transcript_status": "not_requested",
  "summary_status": "not_requested",
  "visibility": "owner_only",
  "capabilities_for_actor": {"edit": true, "share": true, "export": true}
}
```

共享客户端幂等键按 actor+method+resource+key 隔离，并保存 payload_hash；同键异 payload409。接受 complete/processing/retry/export 产生持久 job 的事务提交才返回202；worker 故障不丢任务。持久 job 去重至少覆盖客户端离线补传窗口（建议7天、capabilities公布）。本平台的幂等提交不能保证外部供应商 exactly-once 计费；响应丢失后的付费状态按第12节处理，不承诺绝不重复扣费。

错误 HTTP 语义：401未登录；404不存在或无读权限；403有读权限但无操作能力；409缺段/操作状态冲突/幂等键冲突；412版本过期；413超体积；415格式不支持；422输入不合法/超时长/无必要确认；428缺 If-Match；429配额限制含 Retry-After；503处理能力不可用。业务错误码实施时从项目错误码分配器登记，不在本提案任意占号。上传和模型调用分别限流；q、limit、title/text及外部回执都有限长和类型校验。

## 5. 可选声音档案 / Optional voice identity, later phase

独立 `/api/v1/me/voice-profile`：GET本人状态；POST /enrollments（用途明确同意、策略版本、采样上传 capability）；POST /enrollments/:id/complete（质量校验异步202）；GET /enrollments/:id；DELETE /（撤回→立即停匹配→后台清理）。所有操作只能本人，不能替同事建立。

新记录开启 use_my_voice_profile 需本人 opt-in；仅在本次允许的候选集做匹配，低置信度回退「说话人N」，姓名建议需确认；不支持全实例声音搜人。撤回后取消未开始识别任务，运行中的结果 CAS 拒绝，保留人工已经确认的历史署名并标明来源；若用户要求删除关联需另设清除范围确认，不能偷偷改写所有历史记录。

## 6. 附件、任务与模型 / Assets, jobs and model adapter

音频必须纳入既有附件管理生命周期，统一复用上传会话、资产元数据、受控读取、删除意图与物理删除队列；**新增 purpose=talk_record 与资源授权 adapter 是必要设计工作，不能仅复用旧用户附件授权**。音频 part 必须绑定 note、actor、device session，禁止拿其他记录/聊天 attachment_id 拼接。

每分段建议30秒，AAC-LC mono 16/24kHz（以平台编码支持验证为准），录音文件续传与服务端可解码校验；模型输入可由服务器转换为所需采样率。服务端核算时长与总大小、限制解压/解码资源；不信任客户端 MIME/报时。

模型 adapter：transcribe(audio_ref,language)->speaker clusters+segments；summarize(transcript_version)->结构化草稿+出处。provider选择通过实例受控配置，不接受客户端任意 URL（防止 SSRF）。默认不外传、不将记录用于训练的承诺须由实际部署/服务协议支持，UI不虚称。

V1 状态使用前台 GET jobs 退避轮询2→5→10秒，后台停止，恢复读取；不新增未经设计的实时 WS 事件。本地录音不依赖在线状态轮询。

## 7. 待确认与实施顺序 / Approval and implementation order

本轮只审设计。确认后：资源ACL/处理器能力契约 → 数据迁移与幂等上传 → 本机长录音恢复 → 处理任务/原文/回听 → 纠正/摘要 → 显式授权/导出/删除闭环 → 真机与越权验收。AI处理器配置、声音档案和平台保留区变更若需要另行明确，不使用模拟服务宣称业务闭环。


## 8. V2：个人与企业的契约差异 / Scope-specific contract

两种归属共用表与端点，scope 仅 personal/organization，不创建 personal_talk_record / enterprise_talk_record 两套表。source_type 当前只开放 live_recording；imported_audio/remote_call 是未来枚举扩展方向，capabilities 不得宣布它们已支持，当前提交请求含这些值返回422 unsupported_source_type。

| 维度 | 个人 personal | 企业 organization |
|---|---|---|
| 默认入口 | 我→个人工具→面聊记录 | 企业工作台→协作工具→面聊记录 |
| 创建上下文 | 不接受 organization_id/workspace_id | organization_id 必填；workspace_id 可选并核验组合关系与创建能力 |
| 参与者选择 | 手动标签、已存在联系人（可选） | 有权访问的企业目录；不是整个企业通讯录无条件可查 |
| 可授权对象 | 明确选择的现有联系人 | 当前企业且满足工作区资源策略的同事 |
| 列表默认 | 当前用户创建或显式共享给我 | 当前企业/工作区内创建或显式共享给我；无“默认看全企业” |
| 导出 | owner 可导出，grantee 依 allow_export | 还必须通过企业 export capability；owner 不能绕过 |
| 离开企业 | 不影响个人记录 | 立即失去该企业记录访问，不将录音自动转存个人空间 |
| 保留 | 部署允许的个人保留策略 | 部署配置的企业记录策略，UI 展示实际规则而不是固定承诺 |

增加 GET /creation-contexts：返回当前主体可创建的个人/企业/工作区集合与各自 capabilities、retention_policy 摘要、processing_notice_version。不能传 org ID 获取未授权的工作区元数据。分页参数 cursor/limit，limit默认20最大100；首次仅返回前20，客户端可搜索有权企业/工作区，不一次下载全组织树。

GET /share-candidates?scope=...&organization_id=...&workspace_id=...&q=...&cursor=...：准备页使用通用候选入口，但仅返回主体有权可见目录；existing record 场景使用 GET /:id/share-candidates，由服务端从记录取归属。分页默认20最大100，q最大100字；每次授权提交仍重新验证目标资格，候选查询结果不是永久授权凭证。

参与者 API：GET /:id/participants；PUT /:id/participants（owner/editor + If-Match +全量明确选择，包含本地 guest label 或合法 user_id）；不自动写 grant、不发通知。person 目录匹配不生成声纹、不等于认领。

采集 API V2：POST /:id/captures（Idempotency-Key、If-Match、device_session_id，返回 capture_id+revision）；GET /:id/captures（恢复）；POST /:id/captures/:capture_id/events；POST /:id/captures/:capture_id/audio-parts；POST /:id/captures/:capture_id/audio-parts/:part_id/confirm。这些路径替换上文 V1 `capture-events` 与 record 直属 audio-parts 草案，实施不得同时暴露两套重复 API。

POST /:id/complete 的清单改为 captures:[{capture_id,last_seq,manifest_hash}]；当前首版每条记录只有一段 capture，暂停/继续属于同 capture。未来追加采集必须用明确追加操作并生成新的聚合 manifest/version，不覆盖既有证据。

创建示例 personal：{client_recording_id,title,scope:"personal",source_type:"live_recording",language,processing_mode,notice_ack,policy_version}；organization 同例加 organization_id 及可选 workspace_id。source_type 不由客户端推断权限。客户端使用 creation-context 返回值展示「保存到」；POST 创建会重新检查，策略变化返回409 context_changed 并给出可公开的当前提示，不静默改归属。

实体间 FK 增强：capture(record_id,id) 复合唯一；audio_part(record_id,capture_id) 引用 capture；segment(record_id,transcript_version) 引用 transcript(record_id,version)；summary 引用同记录 transcript，speaker 引用同记录；所有 child API 的父ID和子ID必须在查询条件同时匹配。复合 FK 防止跨记录 ID 混用。模型自动重跑只创建新 transcript，人工修订不得被默认覆盖；发布新版本前显示版本对比并显式确认。

## 9. 企业记录的责任与退出 / Custody and departure

created_by 记录创建事实，owner_user_id 为当前管理责任人；个人两者初始相同，V1 不转移个人归属。企业退出/人员停用不删除记录、不自动开放给企业管理员。原 owner 不再有权访问内容，记录进入需移交状态（custody_status=active/handover_required，新增主表字段）。

POST /:id/ownership-transfers：当前 owner 在仍有权时，将管理责任转给同企业且符合工作区授权的同事；If-Match+Idempotency-Key，服务器事务更新 owner、审计、原 owner 是否保留显式 grant 必须由请求明确决定，不修改 created_by/scope。此动作与分享权限是独立入口，UI 放企业详情更多→移交管理责任，二次确认。

异常离职移交：由实例明确授予的 metadata-only recovery capability 处理待移交记录，只看到记录ID、原责任人、归属与状态，不返回标题/文本/媒体。使用 POST /:id/custody-reassignments，必须 reason、target_user_id、If-Match，写审计。它不授予执行人内容读取权，企业管理员普通角色不能直接调用。此机制属于后续企业治理实现，V1 若未接入需明确待移交，不临时放开内容权限。

企业详情 permissions 返回 can_transfer_owner 与 can_export/can_share/can_delete；分享页导出开关在策略禁止时 disabled 并展示原因。企业未开启 AI 时准备页仅可选录音；个人亦遵循实例 capabilities，不允许借切换空间绕过部署限制。


## 10. 附件生命周期接入修订 / Attachment lifecycle integration

本节补齐前版仅写“复用上传/授权”而没有完整规定的引用、治理和回收合同。当前为设计待实施，不能称为已接入。

### 10.1 唯一存储事实源

录音分段、可选合成音频、持久 manifest 文件、转码音频、文字导出、可选声纹采样都必须登记到现有附件系统；talk_record 不直接操作 S3/Garage/Disk，不新增面聊私有 bucket、上传 SDK、物理删除 worker 或独立文件元数据总表。客户端尚未上传的加密临时分段由本机恢复清单管理，成功绑定并校验后再按本机缓存策略清理，不是第二套服务器附件库。

现有代码同时存在普通 attachment 与企业集成 enterprise_asset 两条资产链，企业归属不意味着自动进入 enterprise_asset。V1 首选普通 attachment 链新增受控 talk_record 业务 scope/用途（待既有枚举、DB守卫、授权与策略适配评审）；企业记录照样须验证当前企业/工作区访问。不得硬填 application_id 把面聊伪装成外部应用上传，也不允许用 creator=当前用户绕过记录 ACL。若适配不能证明治理完整，则企业上传/回收保持未开放，不静默回落 user scope。

V1 正式字段 attachment_id、audio_manifest_attachment_id 均 FK 指向 public.attachment.id；export/sample 同样使用 attachment_id。若后续确实需要 enterprise_asset，必须增加明确资产种类和可校验关联，不能让一个 bigint 同时隐式指向两张表。

### 10.2 业务引用关系

补充 talk_record_attachment_ref：id、record_id FK、attachment_id FK、capture_id nullable、role=audio_part/original_audio/manifest/transcoded_audio/export、business_key、state=bound/released、created_at/released_at；UNIQUE(record_id,role,business_key)。audio_part 与 export 通过各自 ID 生成稳定 business_key，重试不能重复绑定。声纹采样走独立受控 voice_profile 引用，不能作为任何记录共享附件。

新绑定必须同事务完成“已确认资产存在+所属上传会话与 actor/record/capture 匹配+资产 live+无删除墓碑+业务关系写入”，沿用附件生命周期 V5 对象锁及既定父锁顺序。不能仅调用 bind_reference_lock_tx 后忽略不存在元数据的分支；必须明确要求 confirmed 关联的真实资产行存在。同一文件 hash 不代表同一资产，不能按 hash 擅自合并权限。

附件全局引用/GC 资格谓词必须识别本域的 bound 引用及有效处理引用，现有枚举是封闭集合，新增一张关系表不会自动被 GC 看见。接入前不得开启 talk_record 对象自动回收。所有绑定/释放走同一事务锁协议，防止正在完成录音时被后台当孤儿清理。

### 10.3 生命周期动作映射

| 业务动作 | 附件系统合同 |
|---|---|
| 初始化分段 | 注册既有 pending/upload session，绑定 talk_record/capture/actor 上下文，提交后签发上传能力 |
| 上传确认 | 校验实际对象、hash、size及录制会话；幂等 confirmed；业务引用首绑与确认在允许的同事务路径完成 |
| complete/合成/转码 | 每个持久产物登记资产和用途；任务处理期间保持源文件有效引用/期限明确的 processing lease，取消与超时都能释放；不冒用法务 hold |
| 回听/导出 | 先记录 ACL 再附件授权；逐次续票/Range或分段请求重验撤销版本；历史通用 view_url 不能成为绕过记录 ACL 的旁路 |
| 分享卡片 | 引用原记录，不复制音频/全文，不修改附件归属；转发不增加访问者 |
| 发送普通音频副本（未来） | 属于导出，需显式权限与告知；创建独立合法业务引用/副本，不承诺撤销原记录能收回已导出的副本 |
| 删除记录 | 立即撤销业务读取，写 released 与清理协调任务；不能从请求线程直接 DELETE 对象 |
| 孤儿/失败上传 | 使用既有 pending 过期、能力排空证明、delete_intent、可恢复删除队列；未确认分段不永久泄漏，也不按一个任意小时数冒险删除 |
| 存在其他引用或留存/hold | 保留字节，显示“记录已删除，附件受留存规则保护”；不伪报清理完成 |
| 满足物理删除资格 | 由既有附件 worker 按不可变对象身份、租约、墓碑和重试协议执行与确认 |

### 10.4 企业治理与删除状态

企业记录的 policy/hold 必须映射到实际支持该资源的附件治理入口。现有 workspace_attachment_repo 查询限定 group/channel，因此不能声称 talk_record 已出现在企业附件管理页；需要扩展资源分类、记录ACL、检索与治理覆盖。不指定工作区的企业记录也必须有可验证的组织归属及有效留存判定，不能因 workspace=NULL 逃过治理。

记录 retention_until 仅为策略快照/展示，不作为附件删除资格的唯一真源；以实际适用的全体有效引用、治理留存、hold、待结束上传能力与删除状态共同裁决。录音所有者删除、移交或账号注销均不得绕过既有治理。

删除响应拆分 logical_deleted、cleanup_status=pending/blocked_retention/blocked_hold/failed/complete、physical_delete_state。talk_record_job 的 purge 只协调业务数据、索引、外部处理器清除与附件引用释放，不执行存储 DELETE；附件物理状态从既有队列回读。只有所有应清理对象及处理器副本确认处理完毕才报告 cleanup complete；其他业务仍合法引用的资产显示 retained_by_other_reference。有效 hold/retention 期间需保留的治理元数据不能随本域删除。

### 10.4.1 实际治理入口与策略来源

`GET/POST /api/v1/talk-records/:id/attachments/:attachment_id/governance` 使用 JWT；仅真实所属企业的有效 owner/admin 可操作。治理权限独立于记录内容 ACL，响应仅含记录/附件/组织/工作区编号、版本、留存与保全状态、清理状态，不返回标题、正文、对象路径或下载地址。普通记录 owner 不因此获得法务治理权限。

POST 必须带记录版本 `If-Match` 和 `Idempotency-Key`。操作为 `hold`（`reason` 为非空、最多 200 个 Unicode 字符）、`release_hold`、`extend_retention`（RFC3339 `retention_until`，只能延长）、`purge`。真实变更递增记录版本，审计与幂等结果同事务；重放仍重验当前企业治理权限。缺版本 428、过期版本 412、保全/留存未解除 409。批准 purge 仅改变治理事实，不能绕过业务引用、未耗尽上传能力或未知处理器副本；符合条件后精确重排既有清理任务，不在请求线程删除存储对象。

组织级留存基线复用当前实例的 `enterprise_attachment_retention_days`，不声明已有工作区可配置策略。新资产首次登记以不可变资产创建时间固定策略下限，重试和处理租约续期不滚动延长；显式治理延长仍经现有 EAR 守卫。有工作区的 WAR 事实保持不可变，只有同附件、同记录、同企业的 EAR 已获批准 purged 时，面聊来源的 WAR 才不再单独阻挡 GC；群/频道的既有规则保持不变。

迁移 220 遇到历史企业面聊附件时，必须显式提供来自已批准实例策略的 PostgreSQL 参数 `imboy.talk_record_backfill_retention_days`（1..36500），缺失或非法值使事务失败；不得猜测默认值。空库无此参数要求。存在新增治理事实时，回滚拒绝有损降级。

### 10.4.2 附件资源列表

`GET /api/v1/organizations/:organization_id/attachments` 覆盖本企业有权查看的群、频道和面聊附件，包括 `workspace_id=NULL` 的面聊记录；现有工作区附件列表增加面聊来源。统一使用附件编号倒序游标，服务端逐项重验来源权限。面聊行返回 `open_kind=talk_record`、`record_id` 与必要治理元数据，点按进入受控记录详情，不签发通用下载地址，不放宽合成音频或导出的独立授权规则。

### 10.5 当前静态取证与新增验收

已核对 docs/architecture/attachment-lifecycle-v5/02-main-plan.md 的五对象职责/引用锁/GC合同；src/repo/attachment_repo.erl 的 bind_reference_lock_tx/2；src/repo/attachment_gc_repo.erl 的持久删除队列及租约；workspace_attachment_repo 的 group/channel 过滤；enterprise_attachment_retention_repo 的留存/hold守卫。代码存在不等于本次面聊已完成接入或已有线上验收。

实施必验：活跃录音与GC并发、首绑与删除意图竞争、丢confirm回执的幂等恢复、删除期间转写结果迟到、无工作区企业记录的留存、hold阻止删除、引用转发不扩大权限、历史下载票据撤权、源分段与转码/导出级联释放、账号注销和责任移交不误删、Disk/Garage两种驱动按现有实际支持范围验收。不另建一套“面聊清理定时器”绕开统一附件系统。


## 11. V3 评审收敛：实施必须遵守的补充合同

本节用于消除已有提案里的含混点，不授权开发。主表 client_recording_id 的唯一键已改为 **created_by+client_recording_id**，移交 owner 不改变创建请求的幂等身份，防止离线重放产生第二份记录。创建审计主体注销时不能级联删除记录；沿用既有账号匿名化策略保留不可变创建者标识映射/不可逆替身，迁移设计阶段确认实际 FK 策略，禁止未经授权保留可识别身份。

### 11.1 离线、录音段与失联恢复

在线：先读取上下文及策略→创建记录与 capture→获得本机麦克风后写 start。麦克风失败保留可取消草稿，不生成空的“已完成录音”。
离线：仅在已有未过期的部署上下文/录音策略缓存允许时创建本机 UUID 草稿；无缓存或策略到期时明确不可开始企业录音，不借个人模式偷偷保存。普通网络瞬断不要求在线心跳才能继续本机保存；策略硬到期/已收到撤权事件时停止企业采集并说明。

补传流程：创建请求以 created_by+client_recording_id 去重→创建 capture→按 capture_id/seq 注册分段→复用附件上传/confirm→补传事件和重点→complete。新策略、企业成员资格失效或超配额时停止补传，保留本机受保护草稿与明确处置入口，不自动上传到其他归属。

每个 audio_part 增加 capture_offset_ms（有效音频位置）、captured_wall_time（仅溯源），start_ms/end_ms 都是有效音频时间轴；暂停期间不制造空音频。有效时长以实际解码验证，时间点 mark 绑定 capture_id+capture_offset_ms，合成后发布统一时间映射，不能直接把多个 capture 的相对时间拼成全局位置。V1只有一个 capture，多 capture 增量需单独验收。

服务端活跃 capture 的唯一约束不能永远阻塞崩溃后的录音：记录 last_seen_at、lease_epoch、lease_until，弱网络导致过期仅表示设备失联，不证明现场已停止录音。POST /:id/captures/:capture_id/recover 返回已确认分段与状态，凭原 device_session_id 恢复；不同设备不能直接接管。用户显式“结束失联录音”后服务端推进 lease_epoch 并冻结原会话，旧设备后续写返回409 capture_fenced，界面保留未补传分段说明。新增 POST /:id/captures/:capture_id/abandon（owner、If-Match、明确确认）只终止服务端会话，不假称远程停止了离线设备麦克风。

### 11.2 版本域与并发

record.revision 用于标题、归属内设置、删除、移交及 ACL 操作；capture.revision 用于采集事件；segment/summary 各用自身revision；record.content_revision 只在原文/发言人署名等内容变化时推进；record.acl_revision 在授权、撤销、失效、责任移交时推进。上传进度/worker心跳不能不断修改 record.revision 让分享表单必然412。每个响应明确资源ETag，客户端不得把 segment 的ETag用于 record 更新。

处理任务带 input_manifest_hash、input_transcript_version、input_content_revision 和 lease_token。文本是模型输入数据，不作为系统指令；不执行记录内“发消息/导出/工具调用”等要求。模型结构化输出按schema和原文出处校验；事实不确定或无明确负责人/日期时保持未指定，不编造人员与截止时间。

转写成功且无可用语音：transcript.status=no_speech，摘要 not_requested，录音仍可回听；部分语音不可识别保留时间范围/unknown标签，不伪造连续原文。数值置信度只在模型实际提供且经过解释时保存，UI不显示虚假的精确百分比。

### 11.3 最小数据暴露与撤权边界

GET /:id 对无权限/不存在统一404；已授权客户端收到撤权只清除本地可读缓存并显示通用访问失效，不披露服务器具体成员关系原因。DELETE 后 owner 读取仅返回清理状态白名单，其余人仍404。

可读者可见“仅自己/已共享”的范围概述；完整接收者名单只有 owner可读。成员搜索、说话人用户绑定只返回当前目录允许的最小信息。参与者≠授权用户≠已确认身份，三者在字段与界面分开。

撤权阻止后续授权/票据续签、Range/分段访问并使可控缓存失效；**不能收回接收者已经导出、录屏或保存的内容**。UI和接口不得承诺瞬间擦除对方设备已经获得的字节。播放器预取量须受控，以最小预取窗口验证撤权效果。

### 11.4 首版能力清单与配额

完整首版目标不变：个人/企业入口→可靠录音→附件绑定→会后转写→发言人校对→回听→摘要草稿→明确分享/撤销→删除/留存状态。开发可以分阶段，但不能把“录音阶段通过”当作全部功能完成。

首版排除：永久声纹、实时字幕、音频导入、远程通话采集、自动创建正式任务、自动发通知、公开链接、跨企业共享、常规用户全企业检索。相关未来枚举不是当前有效请求值；声纹概念页不进入默认导航。

capabilities/creation-contexts 返回 max_duration_ms、max_recording_bytes、max_part_bytes、storage_remaining_bytes、processing_available、queue_limit、retention_policy、policy_version。秒/字节为服务器配置，启动前检查，运行中接近上限给出保存提示；达到限额结束并保留已落盘内容。转写配额不足仅阻止处理，不应把已有录音误报丢失或无限重试付费。首版供应商已选定；“服务端允许处理”“本设备已配置 Key”“真实供应商质量已验收”是不同事实，不得互相替代。

### 清理失败的域内恢复 / Scoped cleanup recovery

`POST /:id/cleanup/retry` 必须 JWT、设备签名、If-Match、Idempotency-Key，body 为 `cleanup_generation` 非负整数及可选 `reason`（最多200字符）。个人仅当前删除责任 owner；企业仅当前 active 组织 owner/admin 治理能力，不因普通企业成员身份获得权限，也不授正文读取或全域队列操作。已删metadata/治理metadata提供 generation 和 revision；当前权限在幂等重放前再次验证。

服务端只从原 record 的历史引用/derived-job链枚举资源并重验原 delete intent、完整冻结定位、hold/retention/其他引用及未知处理器/写入事实。混合保护时受保护资源不增加预算，符合原 intent/冻结身份及当前保护条件的耗尽资源可单独恢复；至少一个实际恢复才返回202，reason=partial_protection并保留其余受保护资源。没有实际可恢复资源则拒绝，不假202。当前代次CAS冲突409、旧revision412、缺If-Match428、无权限404。202仅证明新代次与purgejob原子持久化，不代表已物理删除。

ordinary队列原path永久墓碑保留，lifetime attempts永不下降；每次显式授权创建新代次，attempt_base取原attempts，每代仍8次。claim/ack/reaper/耗尽投影一致使用代预算。done不复活、旧lease不恢复、不改intent/定位/上传会话/有效期。审计保存真实actor、原因、幂等请求和原资源事实；不自动无限续代。当前实施/真实PG/并发/V5全调用方验证状态以交付证据为准，本文不宣称已验收。

The record-scoped retry renews one bounded physical retry generation only after current deletion/governance authorization and all immutable-resource protection checks. Permanent tombstones and lifetime attempts are retained. A 202 is durable acceptance, never physical completion.

已删除 owner metadata 与当前企业治理 metadata 提供只读 cleanup_retry:{allowed,generation,exhausted_resources,reason}；与POST锁后复核使用同一资格计划，不签发预算、不写审计。generation是实际整数，未知/null/缺失不默认0；未授权或不确定时allowed=false。reason白名单：eligible/partial_protection/protected/not_exhausted/identity_unverified/permission_denied/not_deleted/unknown。不返回资源路径或定位。UI仅依据该当前能力投影显示操作，角色标签不构成授权，POST仍重验。

## 12. 修订9历史：本设备 BYOK 执行 / Historical device BYOK

本节保留旧MiniMax同步合同及其历史限制，不用于新豆包ASR派发。修订10 ASR具体wire、状态、source与恢复规则以DOUBAO-API.md为准；通用身份/输入栅栏、GLM、附件和权限不变量继续适用。

本节是已批准的首版方向及待实施合同，不是当前服务端本机 processor 的能力说明。个人与企业复用业务模型，企业多执行供应商外发策略，不因成员拥有 Key 就允许发送企业录音。未配置或拒绝外部处理时，仍可保存、补传、回听及手工笔记。

### 12.1 密钥、供应商与同意

- Key 由用户在当前设备输入，按部署 origin＋当前账号＋供应商隔离，复用系统安全存储；禁用云备份、Keychain 同步及设备迁移。换机重填。不得放入普通 JSON checkpoint、数据库明文字段、日志、埋点、崩溃报告或 IMBoy API；服务端连 Key 的末尾掩码也不需要接收。不能承诺被控制设备中的明文请求凭据不可提取。
- 原生 App 使用独立 HTTP client，固定官方 HTTPS endpoint：MiniMax `https://api.minimax.cn/v1/speech_to_text`；智谱 `https://open.bigmodel.cn/api/paas/v4/chat/completions`。不使用携带 IMBoy JWT、业务签名或正文日志的默认 client；不跨域转发 Authorization，不接受任意 provider callback URL。首版不使用 Coding Plan 端点。
- 智谱年度 Max 如为 GLM Coding Plan，不覆盖自建 App 的通用模型调用，除非另有供应商书面协议；不得展示“Max 套餐免费抵扣”。公开计价只作估算，真实 Key 与测试预算另行授权，不以本合同授权任何付费调用。
- 处理同意分别说明“音频发送 MiniMax”“转写文本发送智谱”，绑定记录、当前操作者、execution_mode、provider/model/policy pin 与输入版本。录音告知不等于外发同意；录音同意也不代表现场所有人的法律同意事实。
- creation-contexts 与记录上下文返回当前主体可用的公开 provider policy；领取输入、续期、每个外发块之前及提交结果都重验。企业禁用、撤权、移交、Key 删除或账号切换后，合作客户端停止新外发并清除对应可读缓存；不能收回此前已发送供应商的数据。

### 12.2 新增执行数据，复用现有内容发布

| 表/字段 | 最小约束与职责 |
|---|---|
| talk_record_job.execution_mode | assemble/export/purge 仍 server_local；transcribe/summarize 可 client_byok。现有 server worker 的 claim 与通用 reaper 均必须显式排除 client_byok；客户端过期走下述专用屏障，不能回 queued 后自动重复付费；未知值拒绝 |
| talk_record_client_run | TSID id、job_id、attempt、服务端认证的 actor_user_id/device_session_id、lease_token_hash、lease_until、provider/model/policy pin、input_manifest_hash、input_content_revision、input_transcript_version、chunk_plan_hash、state、revision。每 job 至多一条有效执行；设备身份从认证会话取，不能相信 body 冒充；不存 Key、音频路径或原始供应商响应 |
| talk_record_client_run_part | run_id、seq、source_part_manifest_hash、source_ranges、offset_ms/duration_ms、dispatch_intent_id、request_hash、provider_state=not_dispatched/dispatching/confirmed/provider_outcome_unknown/rejected、result_hash、规范化且限长的 segments/摘要草稿、provider_request_id、revision；UNIQUE(run_id,seq)。同序同 hash 幂等返回，同序异 hash409。供应商返回原文是受记录 ACL 管理的内容，不能作为普通审计 metadata |

不复用 `talk_record_processor_run` 代表手机执行：该表用于服务器 OS 进程和临时副本清理证明。手机失联不构成服务器进程退出证明，也不应靠一条永不结束的客户端 lease 永久阻塞附件 GC。新的表/字段需正式迁移、索引与回滚策略，实施时核编号；设计文档不替代可执行迁移。

本地上传 `completed=true` 继续只代表附件 complete 已确认。另建加密的 AI checkpoint，绑定部署/账号/auth epoch、原空间/record/capture、job/run/attempt、输入版本、provider/model/policy pin 和各块状态；转写正文、响应片段均使用 SQLCipher 或加密文件，不写现有普通上传 checkpoint。Key 只在安全存储中，checkpoint 保存凭据引用而非副本。

### 12.3 客户端执行 API

相对 `/api/v1/talk-records`。全部需 JWT 与现有设备签名；run token 只授指定任务提交能力，不能替代 JWT 或取得通用附件访问。记录编号、job/run 编号均沿用无损 TSID，不由客户端生成服务器主键。

| Method / path | 合同 |
|---|---|
| POST /:id/processing-requests | owner/editor＋当前归属资格；If-Match、Idempotency-Key、处理模式、client_byok、公开配置 pin 及有效同意。输入已完整确认后持久化 job，返回202。Key 不存在于请求中；记录保存本身不被此请求失败阻塞 |
| POST /:id/jobs/:job_id/client-executions | 领取当前可执行 job，绑定当前认证设备，返回 run_id、attempt、短期不透明 token、输入版本与服务端确认的源分段清单。并发设备不能同时领取；旧 attempt 不复活 |
| PUT /:id/jobs/:job_id/client-executions/:run_id/chunk-plan | 首次外发前提交确定性分块方案，服务端依据完整源清单、有效音频范围和公开供应商限额复核，冻结 chunk_plan_hash、精确 seq 集合与 source_ranges/offset/duration/重叠去重规则。精确重放幂等；一旦存在 dispatch intent，不允许改计划或增删块；摘要使用绑定全部输入 segment IDs/hash 的文本计划 |
| GET /:id/jobs/:job_id/client-executions/:run_id | 当前有权主体读取归一化状态、已接收序号及结果 hash，用于丢回执恢复；不返回 Key、token hash、原始异常、他人设备信息或私有路径 |
| POST /:id/jobs/:job_id/client-executions/:run_id/heartbeat | 当前设备、token、attempt 与未到期 lease 才可续期；再次核权限/同意/policy/input pin。不能恢复过期或撤销的 lease，不推进 record.revision 制造普通编辑冲突 |
| POST /:id/jobs/:job_id/client-executions/:run_id/parts/:seq/dispatch-intents | 每次供应商请求之前，先持久化绑定计划/输入/provider/model 的请求意图与 request_hash，返回不透明操作编号。相同意图仅返回原事实；存在 dispatching/unknown 时不能产生可自动重发的新意图。此接口不代理供应商请求、不接收 Key，也不证明实际发生计费 |
| PUT /:id/jobs/:job_id/client-executions/:run_id/parts/:seq/result | 校验当前执行、固定源清单、时轴、内容大小、结果 schema、speaker 范围与 result_hash；规范化结果幂等落库。拒绝未知字段及凭据、任意 URL/服务器路径；供应商 trace_id 只是诊断信息，不是执行证明 |
| POST /:id/jobs/:job_id/client-executions/:run_id/complete | 逐项核结果 seq 与冻结计划精确相等、全部 confirmed、source_ranges 全覆盖且仅存在计划明确的重叠，并按固定去重规则归一时轴；不得按实际已提交数量推断完成。锁内重验 live record、资格、job token/attempt、输入 manifest、content revision、transcript version、同意/policy；复用内容仓储原子发布转写或摘要并完成 job。客户端提交只证明收到合法结构，不证明模型质量 |
| POST /:id/jobs/:job_id/client-executions/:run_id/settle | 当前设备报告受限状态 failed/provider_outcome_unknown/cancelled 及白名单错误码；不接受可执行补偿指令。unknown 不能当作未计费失败自动重发；后续重试须有明确处置与新 attempt，同样保留旧事实 |

所有写入均绑定服务端执行身份与版本，并按需要使用独立 run ETag/幂等键；token、attempt 或内容版本过期时拒绝迟到发布，不能以“幂等重放”恢复已撤销授权。摘要必须引用当前可读 transcript/segment IDs，正文校对使旧摘要 stale。处理失败不覆盖人工校对、原录音或已成功阶段。

客户端专用 reaper/重试屏障：存在 `dispatching` 意图但尚无可核结果时，lease 过期原子转为 `provider_outcome_unknown`，原意图、输入和计费不确定事实永久保留；禁止通用 reaper、普通 job retry、新设备领取或自动补传把它当成未发请求。未创建过意图的块才可安全续接；已 confirmed 的块在完全相同计划、源输入及 provider/model/config pin 下可由新 attempt 引用原结果，记录来源 run/seq/result_hash，不能再次调用供应商。版本或计划变化不得暗中复用。新设备接手仍须自己的 Key、当前资格与有效同意。

unknown 的显式重新请求需独立权限校验、run ETag、幂等键和绑定具体未知意图的 `ack_possible_duplicate_charge=true`，说明无法核实旧请求费用且可能重复扣费；服务器保存该确认后才创建新 attempt/新意图，旧 unknown 不改写为未发生。迟到旧响应可以保留受控诊断/对账事实，但不能凭旧 token 发布或覆盖新版本。手机只有本地成功回执、而服务器未确认时，先查询并幂等补交结果；不再次请求供应商。

### 12.4 分段、说话人与费用恢复

MiniMax 单次音频不超过500秒且不超过50MB；首版配置保留余量，以实际封装与字节复核为准。上传 audio_part 与 ASR chunk 是不同概念：现有30秒独立 AAC 分段可形成受控处理清单，但不得把多个独立容器直接拼字节冒充合法长文件；需要组合时必须验证合法重封装与有效音频时轴。每块保存源 part/attachment/hash、全局 offset、实际 duration 与明确的重叠/去重规则；暂停墙钟时间不能加入录音时轴。

使用 `verbose_json` 接收分人和句段时间戳，不与仅 json 支持的 stream 模式混用。每块 speaker 作用域独立，例如 `chunk_seq:S1`；不得因标签相同就自动合并，不由 GLM 猜真实姓名或跨块身份。提供原音回听与人工合并/重分配；全局分人质量和长会议跨块一致性仍按原验收门评测，不能把局部分人或人工修正结果冒充自动质量通过。

GLM 返回带 segment IDs 引用的结构化草稿；服务端校验来源、边界和当前内容版本，不执行原文里的工具/发消息指令。GLM-5.3-Flash 不支持关闭 thinking，不能发送不支持的禁用参数；限制输入、输出和请求次数，不以设置 max tokens 承诺严格金额上限。

401/402 停止并保留进度，429 按有限 Retry-After/退避处理，网络超时、进程退出和响应丢失保存 `provider_outcome_unknown`。已确认结果不重复调用；request ID 不等于供应商幂等保证，不能宣称 exactly-once 计费。预算/预估费用为本设备提示，供应商账户限额是实际约束；各设备消费不能仅靠一个本地计数承诺全账户硬限额。手机后台受限时显示“等待在本设备继续”，不承诺关闭 App 后持续执行。

### 12.5 清理与真实验收边界

录音、源分段、拼装音频、manifest 与导出仍进入既有附件管理系统；不另建 BYOK 音频桶或删除定时器。逻辑删除、撤权、留存/hold、其他引用与清理重试的原规则不变。服务端取消客户端执行并拒绝迟到结果；合作客户端清除本机临时明文、受控缓存与待提交结果，保存必要最小审计事实。

本平台物理删除状态仅证明自身可管理的资产及处理副本。供应商保存过的数据按其实际规则处理，独立披露外部处理状态；客户端声称“已删”不构成供应商清除证明。不得把 owned storage 已清理显示成“所有位置永久擦除”，也不能伪造供应商删除回执。现有服务器未知 OS 进程/副本仍遵守原阻塞清理规则，不以手机执行表替代证明。

必验：Key 不出设备/不进备份与日志、账号/部署切换、企业外发禁用、每块权限与内容版本失效、过期 lease/旧 attempt 迟到、PUT/confirm/complete 与 AI 阶段独立恢复、供应商未知付费结果、500秒/50MB 边界、跨块重复文本/时轴/说话人、无语音不虚构、摘要引用和注入防护。真实供应商 Key/预算/允许上传的样本另行授权；mock、HTTP200、结构校验及本机测试数不能替代原 T13/T14 的质量验收。

参考：[MiniMax ASR 接口](https://platform.minimax.cn/docs/api-reference/speech-to-text)、[GLM-5.3-Flash](https://docs.bigmodel.cn/cn/guide/models/vlm/glm-5.3-flash)、[智谱订阅使用规范](https://docs.bigmodel.cn/cn/terms/subscription-agreement)、[官方端点与额度区别](https://zcode.z.ai/cn/docs/configuration)。供应商能力、价格与条款需在真实测试前重新核验。

## 13. 修订10：豆包异步合同候选

完整端点与表增量见[DOUBAO-API.md](DOUBAO-API.md)，共享请求profile见[fixture](fixtures/doubao-profile-v1.json)。BE R7/App R8代码候选不等于正式验收。迁移222/223/224及provider-task事实依实际冻结源码核对，编号合并前再查所有分支；不改历史223。source purpose只允许成功assembly、当前ACL/同意/策略、bound/live引用及保留/删除语义下的短时读取，不放宽derived通用view_url，不接受任意URL/路径，不新建公开桶或GC链。

submission UUID不是供应商返回taskID。durable intent只首次授权may_submit；knownID pending/unknown只能query；首版query_receipt failed不开放，HTTP拒绝不是ASR终态。响应丢失通过latest发现task/run或recover-unprepared，已保存taskID可reconcile-receipt；source300秒期限不截断受理后的查询和完成。明确未派发prepared恢复不收虚假重复费用确认；无ID未知重提须双风险确认，旧未知事实不抹除。完整恢复/撤权/旧generic旁路拒绝须定向与真实事务验证，不由文档认定通过。
