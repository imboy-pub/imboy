# STEP-10 备注 — 实现说明与设计决策

## 交付文件

| 文件 | 层 | 说明 |
|---|---|---|
| `src/logic/teaching_attach_logic.erl` | 新 Logic | 教学附件域：can_upload/1（教学身份守卫）、check_mime/1（presign 声明预检）、verify_upload/3（HEAD 真实值复核：MIME 白名单/大小/时长）、authorize/2（view_url 读授权）、list_unbound/1 + cleanup_unbound/1（孤儿清理） |
| `src/logic/attach_logic.erl` | M（4 钩子，最小侵入） | ① presign 教学白名单预检（presign_authorized 拆分）；② verify_and_save 教学复核钩子（失败删对象）；③ can_upload 加 `<<"teaching">>` 子句；④ authorize 加 `<<"teaching">>` 子句；confirm 成功 payload 补 `attachment_id`（confirm_payload/1） |
| `src/repo/teaching_submission_repo.erl` | M | submission_for_asset_path(/_tx)：附件路径→submission 绑定；unbound_teaching_attachments(/_tx)：超龄未绑定教学附件（NOT EXISTS 守卫） |
| `test/logic/teaching_attach_logic_tests.erl` | 新 | 19 用例：can_upload×4、MIME 白名单、video/photo 上限与时长、MEDIA-01 授权 wiring×9、MEDIA-02 清理×3、MEDIA-03 源扫描 |
| `test/repo/teaching_attach_integration_tests.erl` | 新 | 真库 3 用例：路径解析、孤儿列举不误删、MEDIA-03 行级断言 |
| STEP-10/{commands,tests,notes,handoff}.md | 证据 | |

## 关键设计决策

1. **scope 定名 `teaching`**（gap#2 收口）：moya 侧 TEACHING_SCOPE 常量改一处即可。
   bucket 经 elib_oss:get_bucket 默认私有桶（teaching 非公开）。
2. **confirm 响应补 attachment_id**（gap#1 收口）：字符串（TSID 契约）；**保留
   object_key**（旧客户端零影响）；按 path 回读（uk path upsert 后必中），回读失败
   仅返回旧字段不阻断。
3. **上传权两层**：presign/confirm 时 can_upload = "持任一有效教学身份"
   （active guardian/staff，fail-closed）；强归属（附件只能进本人提交）在
   create_submission 的 validate_assets（creator_user_id==提交人，Step 9 已就绪）。
4. **读授权三重门**（MEDIA-01）：① 附件必须已绑定 submission（未绑定素材一律无
   读 URL——上传后未提交不外泄）；② submission withdrawn → 拒绝（T17：证据仅
   审计路径）；③ teaching_acl:submission_access（guardian 需 can_view_review /
   本班 staff；Owner/同班成员/非任课老师拒——复用 Step 8 矩阵）。每次签发前
   重新检查（attach_logic:view_url 既有语义，无新增缓存）。
5. **服务端校验**（任务 4，即 B2 被清除的 MIME 校验经评审后按本人语义重做）：
   - presign：声明 MIME 预检（早失败）
   - confirm：HEAD 真实值复核——video/mp4|video/quicktime ≤100MB（iPhone 原录
     .mov 必须放行）；image/jpeg|png ≤20MB；duration（客户端上报 duration/
     duration_seconds，秒）≤60s 才校验，未上报放行（服务端抽帧复核留 Step 11）
   - 上限可配：teaching_video_max_mb / teaching_photo_max_mb /
     teaching_video_max_duration（config_ds env）
   - 错误映射复用 attach_handler 既有：file_too_large(413)/invalid_file_type(415)
6. **生命周期**（MEDIA-02）：
   - 未 confirm 的 presign：既有 attach_pending + pending_cleanup 机制（小时级，
     NOT EXISTS attachment 守卫）——不重复建设
   - confirm 后未绑定 submission 的教学孤儿：`cleanup_unbound(AgeHours)`（默认
     24h，下限 2h）——先删对象成功再软删行（status=-1）；删对象失败保留行下轮
     重试（绝不先删行留对象）；NOT EXISTS submission_asset 守卫保证已绑定附件
     （含撤回证据）不误删；仅 scope='teaching'（其他 scope 不碰）
   - 撤回附件证据保留：撤回不移除绑定关系 → NOT EXISTS 守卫天然不删
   - **定时接线遗留**：cleanup_unbound 未挂 ecron（首版交付函数+测试，符合任务
     边界）；接线只需在 attach_cleanup_logic 同款 ecron 条目加一行
     `teaching_attach_logic:cleanup_unbound/0` 包装（建议 AgeHours 走
     config teaching_unbound_cleanup_age_hours）
7. **MEDIA-03**：presigned URL 仅按请求签发（sign_view），落库 map path/url 恒绑
   ObjectKey（源码断言）；教学 repo 零 presign 调用（源码断言）；真库行级断言
   path==url 且无 X-Amz-/Expires/Signature 串；submission_asset 无 URL 列。

## 给 moya 联调的回复（STEP-17-PREP gap 清单）

| # | gap | 本 Step 处置 |
|---|---|---|
| 1 | confirm 无 attachment_id | ✅ 已补（字符串），object_key 保留 |
| 2 | teaching scope 未注册 | ✅ 定名 `teaching`（can_upload/authorize/presign 预检全链路） |
| 3 | presigned PUT vs wx.uploadFile | ⚠ **POST 直传端点首版不实现**（真机 ArrayBuffer PUT 验证后决策——本 notes 标注即视为任务书要求的登记） |
| 4 | file_hash256 空串 | ✅ 服务端仅参考（双读兼容 md5），confirm 以 HEAD 真实 size/mime 为准 |
| 5 | view_url TTL | **GET_EXPIRES = 600 秒（10 分钟）**（attach_logic ?GET_EXPIRES）；moya 4 分钟缓存安全（保守侧） |

## 已知限制 / 遗留

- cleanup_unbound 未接 ecron 定时（见上，接线一行）。
- duration 为客户端上报值（≤60s 校验仅在字段存在时生效）；抽帧级服务端时长
  校验在 Step 11 Worker。
- 教学上传的强归属（附件只能进本人提交）依赖 Step 9 validate_assets；presign
  阶段不预绑定（上传时 submission 尚未创建，契约如此设计）。
- attach_logic_tests 的 moment 用例在本构建（moment 编译期裁剪）既有失败，
  与本 Step 无关（36/37 通过，失败为 {undefined_module, moment_ds}）。
