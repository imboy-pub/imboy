# STEP-17 契约偏差实测（R8，本地 HTTP 级）— moya-teaching.yaml 冻结契约

Owner: Agent B | 日期: 2026-09-09 | 环境: 冒烟节点 imboy_smoke@127.0.0.1:9811（直连 erl，配置 /tmp/smoke_sys.config 副本指向 moya_boot_smoke@4323，迁移已自动到 100）；种子 7xxxx 段（orgA=771000/learner=774001/guardian=770002/teacher+orgA owner=770001/manager=770004/assistant=770003/orgB owner=770005/target=770006）；JWT=jwerl hs256 claims {sub,exp,uid}（照 token_ds:do_encrypt_token/4 实际形态，jwt_key 取 sys.local.config）。

## 1. 操作×结论矩阵（12 冻结操作 + Step 16 bind/unbind + 认证边界）

| # | 操作 | 初跑 | 修复后 | 结论 |
|---|---|---|---|---|
| 1 | POST /auth/wechat-mini/login | — | — | **NOT_TESTABLE**（wechat jscode2session 外呼，铁律禁真实第三方） |
| 2 | GET /teaching/contexts | code=1 db_error | code=0，contexts 含 learner/group/org 全链，TSID 全 string | **MATCH**（修复 D-1 后） |
| 3 | POST /teaching/context/switch | HTTP 500 空体（crash） | code=0 回显快照；非本人 learner→5420（T12）| **MATCH**（修复 D-1 后） |
| 4 | GET /teaching/assignments?learner_id= | 缺参 422（调用姿势）；带参后 code=0 list+page/size/total | 同左；非监护人 learner→5422（T3）| **MATCH**（修复 D-1 后） |
| 5 | GET /teaching/assignments/{id} | code=1 db_error | code=0 详情；跨 Org（orgB owner）→ 403"无权访问该资源"（T14）| **MATCH**（修复 D-1 后） |
| 6 | POST /teaching/assignments/{id}/submissions | code=1 db_error | code=0 创建（submission_id string TSID、ai_status=queued）；同 key 同 body 重放=同 sid+idempotent_replayed=true；同 key 异 body→**5460**（修复 D-2 后）；缺 key→5461 | **MATCH**（修复 D-1/D-2 后） |
| 7 | GET /teaching/submissions/{id} | HTTP 500 | code=0，ai_status_hint/assets/published_review 视角字段齐 | **MATCH**（修复 D-3 后） |
| 8 | GET /teaching/review-queue | — | code=0；队列含 ai_status=failed 行（真实 AI worker 在冒烟节点跑过并正确降级 provider_unavailable——Step 11 闭环实证） | **MATCH** |
| 9 | GET /teaching/submissions/{id}/review-workbench | HTTP 500 | teacher code=0（含 ai_draft.failed+error_code）；guardian→5424（T4）| **MATCH**（修复 D-3 后） |
| 10 | PUT /teaching/submissions/{id}/review-draft | HTTP 500 | code=0 upsert（review_id string）；assistant→5425（T13）| **MATCH**（修复 D-3 后） |
| 11 | POST /teaching/submissions/{id}/reviews/publish | HTTP 500 / 有草稿+withdrawn 时 code=1 | confirm 不一致→5483；正确发布 code=0；重复发布 already_published=true 幂等；withdrawn+有草稿→**5482**（修复 D-5 后）| **MATCH**（修复 D-3/D-5 后） |
| 12 | GET /teaching/learners/{id}/history | — | code=0（已发布+未发布按 learner 聚合跨 ws）；绑定账号本人 code=0（R6 第三分支）；解绑后本人立即 5423（BIND-02 实证）；跨 Org→5423 | **MATCH**（D-6 经 Coordinator R9 ruling 裁定为非偏差，见 §2） |
| 13 | POST /teaching/learners/{id}/bind | — | guardian→403+5429；invalid target→422+5428；manager→code=0（payload id/user_id/account_bound_by 归一 string，修复 D-4 后）| **MATCH**（修复 D-4 后） |
| 14 | POST /teaching/learners/{id}/unbind | — | 未绑定→409+409"该学员当前未绑定账号"；成功解绑 user_id=null | **MATCH** |
| 15 | 认证边界 | — | 无 token→真实 HTTP 401 + envelope {code:401,...}；401 body 四键齐 | **MATCH**（契约硬规则2） |
| 16 | 404 路由 | — | HTTP 404 | **MATCH** |
| 17 | 附件 presign/confirm/view_url（teaching 三重门） | presign teaching→**HTTP 500**；view 五场景全拒/放行正确 | presign guardian code=0（object_key 含 teaching/ 段+签名 PUT URL）；invalid mime→400；无教学身份→403；confirm 未上传对象→400 fail-closed；view：不存在键/guardian 已绑已提交/stranger/撤回态/staff 全部正确（2 放行 3 拒）| **MATCH**（修复 D-7 后；confirm 真传腿需 Garage，冒烟环境未起——fail-closed 语义已证，真传留联调） |

矩阵结论：**12 冻结操作中 10 个有最终结论（9 MATCH + 1 DEVIATION 轻微）+ 2 NOT_TESTABLE**；另有 Step 16 两操作 MATCH、认证/404 边界 MATCH。

## 2. 偏差/缺陷明细（R8 实测发现并当场修复 6 项）

全部为「直连 erl 单测不可见、真实 HTTP+全配置节点暴露」——单测 VM 无 sql_driver 等配置、eunit mock 掩盖 repo 层行为。

| ID | 层 | 症状 | 根因 | 修复 | 影响 |
|---|---|---|---|---|---|
| D-1 | repo | contexts/switch/list/detail/create 全线 db_error/500：`relation "public.group" does not exist`（42P01） | `teaching_context_repo:tb(group)/0` 与 `teaching_submission_repo:tb(group)/0` 把 `public.` 前缀包进引号生成 `"public.group"`（单个带点标识符）；sql_driver=pgsql 的真实节点必炸，直连测试 VM 无 sql_driver 故测不出 | 两文件 tb(group) 改为只引表名段 `public."group"` | **P0**：教学全 API 在真实节点不可用（生产同配置同炸） |
| D-2 | logic | 同 key 异 body → HTTP 500 crash（case_clause {rollback, idempotency_conflict}） | `teaching_assignment_logic:run_create_tx/7` case 漏该分支 | 补 `{rollback, idempotency_conflict} -> {error, idempotency_conflict}` | 5460 契约码不可达，T8b 场景 500 |
| D-3 | repo | detail/workbench/draft/publish/withdraw 全线 500（case_clause {ok,[#{<<"organization_id">>=>…}]}） | `teaching_context_repo:learner_org/1、group_org/1、org_owner_uid/1` 用**原子键**匹配 elib_pg **二进制键**行 | 三处改二进制键 | 老师/发布全链在真实节点不可用 |
| D-4 | api | bind/unbind 响应 payload 的 id/user_id/account_bound_by/organization_id 为 integer | handler 直接透传 repo 行，违反契约硬规则1（TSID 一律 string） | `teaching_learner_bind_handler:tsid_strings/1` 归一（null 保持） | 小程序 JS 精度丢失风险 |
| D-5 | logic | 有草稿 + 已撤回的 publish → code=1（应 5482） | repo `publish_tx` 守卫失败返回**裸** `{error, withdrawn}`（非 {rollback,…}），`run_publish_tx` case 只认 rollback 形态 → 折叠 db_error | 补 `{error, Reason} when is_atom(Reason) -> {error, Reason}` 透传 | 5482 契约码不可达 |
| D-6 | logic（**Coordinator R9 ruling：非偏差，结案**） | 跨 Org 访问 history → 5423"无监护提交权限"；assignment detail 同场景 → 403 | **裁决理由**：冻结契约 error-codes.md 使用规则 4 明文 542x 用于"身份关系明确不匹配"；history 是关系型端点（access=guardian_learner/self/staff 关系判定），learner_id 与请求者无关系→5423 引导绑定，符合契约；assignment detail 是资源型端点（staff 域资源）→403。二者为**有意区分**而非不一致 | 不改代码。T14 复审条件：**若 learner ID 未来改为可枚举序列需重审本裁决**（当前 TSID 不可枚举，5423 泄漏信息实质为零） | 无（语义码引导绑定，客户端可据此出"联系机构绑定"引导） |

## 3. 附加实证（本次实测的域外收获）

- **AI-03/Step 11 真环境闭环**：冒烟节点 ecron 每分钟真实驱动 `teaching_ai_worker:run_once/0`，3 笔提交的 AI draft 全部正确走 `queued→failed(provider_unavailable)` 降级（workbench/queue 可见 ai_status=failed + error_code），老师人工发布不受影响——Step 11 验收在真实 HTTP+调度环境下复证。
- 冒烟配置 /tmp/smoke_sys.config 的 ecron 段仍是 Coordinator 手工 `{local_jobs,[教学×2]}` 形态（仓内修复已由 R7 落地）。

## 4. 复现命令

```bash
# 种子（7xxxx 段，见本仓 test 夹具改造）→ 打 moya_boot_smoke@4323
# 节点：HTTP_PORT=9811 erl -noshell -pa ebin -pa deps/*/ebin -name imboy_smoke@127.0.0.1 \
#   -setcookie imboy_smoke_ck -config /tmp/smoke_sys.config \
#   -eval 'application:ensure_all_started(imboy).'
# JWT：jwerl:sign(#{sub=><<"tk">>, exp=><now+7200>, uid=>770002}, hs256, jwt_key)
# 打点脚本：/tmp/r8_matrix.sh /tmp/r8_matrix2.sh /tmp/r8_matrix3.sh /tmp/r8_matrix4.sh
# 收尾：rpc halt（节点已退，9811/epmd 释放确认干净）
```

## 5. 验证与回归

- `make compile` EXIT=0 零源码警告（4 个修复文件 + handler 测试）。
- R7 七模块回归（flow/acl/auth_logic/learner_bind_integration/ai_provider/ai_worker/bind_handler）→ **REGRESSION: ok**（handler 测试断言已同步 D-4 string 契约）。
- 修复代码经两通道验证：冒烟节点热加载（code:purge+load_abs）后 HTTP 重打确认 + 本地 eunit 回归。
- 节点收尾：imboy_smoke 已 halt，`epmd -names` 仅剩用户节点（imboy/imboy9801 未触碰），9811 无监听。

## 6. R9 增补（附件三门实测 + D-7）

### D-7（已修）：elib_oss:scope_segment/2 缺 teaching 子句 → 教学上传 presign HTTP 500

- 症状：`GET /attachment/presign?scope=teaching`（有教学身份者）→ function_clause `scope_segment(<<"teaching">>,undefined)` → HTTP 500。**教学上传在真实节点全断**。
- 根因：Step 10 新增 teaching scope 时，`elib_oss:scope_segment/2` 未加对应子句——与该文件注释中记载的 channel/moment 历史 bug 完全同款（当时也是漏子句导致上传全断）。
- 修复：照先例补 `scope_segment(<<"teaching">>, _) -> <<"teaching">>;`（elib_oss.erl，一行子句+注释），热加载后 G1 即 code=0。
- 影响面：所有 teaching scope 上传（家长提交前必须的 presign 腿）。

### 三门场景矩阵（R9 实测）

| 场景 | 期望 | 实测 | 结论 |
|---|---|---|---|
| presign：guardian+合法 video/mp4+teaching | code=0 + object_key(teaching 段)+PUT URL | 修复 D-7 后 code=0 | MATCH |
| presign：非法 mime（application/x-msdownload） | 400 | 400 不支持的文件类型 | MATCH |
| presign：无教学身份者（orgB owner） | 403 | 403 无权向该范围上传 | MATCH |
| confirm：对象未真实上传（fail-closed） | 400 拒 | 400 附件落库失败（Garage HEAD 失败折叠，deny 语义成立） | MATCH（fail-closed；真传腿留联调） |
| view：不存在的 object_key | 拒 | 400 无权访问该附件 | MATCH |
| view：已绑定已提交 submission × guardian(can_view_review) | code=0 短时签名 GET URL | code=0（600s 签名 URL，离线签名不依赖 Garage 存活） | MATCH |
| view：同资产 × 跨 Org 陌生人 | 拒 | 400 | MATCH |
| view：仅绑定**已撤回** submission 的资产 × guardian | 拒（T17 撤回证据仅审计可达） | 400 | MATCH |
| view：同资产 × 本班 staff | code=0 | code=0 | MATCH |
| 孤儿对象清理（MEDIA-02） | — | 非 HTTP 面：ecron `teaching_unbound_cleanup`/`attachment_orphan_cleanup`（R7 已修 local_jobs 激活），逻辑覆盖见 STEP-10 证据 | NOT_TESTABLE(HTTP)，登记 |

### 打点资产固化（R9）

`STEP-17-PREP/smoke-scripts/`：README.md（用途/前置/顺序/配置来源）、sign_jwt.escript（jwt_key 运行时读 config，零硬编码）、seed_smoke.sql、attach_scope_prep.sql、env.sh、matrix_teaching.sh、matrix_bind.sh、matrix_attach.sh、halt_smoke.sh。脱敏自查通过（无 jwt_key/garage key/任何 secret）。
