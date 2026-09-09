# STEP-10 证据 — 测试结果明细

## teaching_attach_logic_tests（meck，19 用例 PASS）

### can_upload（教学身份守卫）

| 用例 | 断言 | 结果 |
|---|---|---|
| guardian 身份 | {ok,[g]},[] → ok | PASS |
| 仅 staff 身份 | [],{ok,[s]} → ok | PASS |
| 无任何教学身份 | [],[] → false | PASS |
| DB 异常 | 双查 error → false（fail-closed） | PASS |

### 白名单/上限/时长

| 用例 | 断言 | 结果 |
|---|---|---|
| MIME 白名单 | mp4/quicktime/jpeg/png ok；avi/webp/gif/octet-stream → invalid | PASS |
| video ≤100MB | 100MB ok；100MB+1 → file_too_large | PASS |
| duration ≤60s | 60 ok；61/90(duration_seconds) → invalid；未上报放行 | PASS |
| photo ≤20MB | 20MB ok；+1 → file_too_large | PASS |

### MEDIA-01（授权 wiring；完整 ACL 矩阵在 teaching_acl_tests Step 8）

| 场景 | submission_access 注入 | authorize | 结果 |
|---|---|---|---|
| 合法监护人读 | {ok, guardian, _} | true | PASS |
| 本班老师读 | {ok, staff, _} | true | PASS |
| 未授权家长/同班成员/非任课老师 | {error, forbidden} | false | PASS×2 |
| 仅 Org Owner | {error, owner_not_granted} | false | PASS |
| 资源缺失 | {error, not_found} | false | PASS |
| **未绑定附件**（上传未提交） | —（无绑定即拒） | false | PASS |
| **已撤回 submission** | —（withdrawn 即拒，T17） | false | PASS |

### MEDIA-02（清理不误删）

| 用例 | 断言 | 结果 |
|---|---|---|
| 超龄孤儿 | 删对象 ok + soft_delete → {cleaned:1,errors:0} | PASS |
| 删对象失败 | 不 soft_delete（保留行下轮重试）→ {cleaned:0,errors:1} | PASS |
| 空列表 | {cleaned:0,errors:0} | PASS |

### MEDIA-03（源码扫描）

| 断言 | 结果 |
|---|---|
| 三个教学 repo 无 presign_put/get_for_key、无 X-Amz- 串 | PASS |
| attach_logic 落库 map `<<"url">> => ObjectKey`、`<<"path">> => ObjectKey` | PASS |

## teaching_attach_integration_tests（真库 4323，3/3 PASS）

| 用例 | 断言 | 结果 |
|---|---|---|
| 路径解析 | 绑定附件→(submission_id,status)；撤回绑定→withdrawn；未绑定/不存在→undefined | PASS |
| 孤儿列举 | 5 个附件（绑定/撤回绑定/超龄未绑定/新近未绑定/private 超龄）→ 恰好只列超龄未绑定 teaching 一个 | PASS |
| MEDIA-03 行级 | 5 行 path==url、无 X-Amz-/Expires/Signature；submission_asset 无 URL 列 | PASS |

## 回归

teaching_flow_integration 6/6、teaching_acl 13、teaching_auth 8、
auth_middleware/billing_route/auth_ds、attach_pending_cleanup = ok；
attach_logic_tests = 36/37（唯一失败 moment_ds 未编译，既有环境性，与本 Step 无关）。
