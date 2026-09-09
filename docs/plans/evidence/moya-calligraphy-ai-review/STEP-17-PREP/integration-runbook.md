# 墨芽习字 · 联调就绪 Runbook（R14）

> **定位**：任务工程侧 17/18 PASS，证据等级 LOCAL PASS。本文件把「外部门禁清零之后如何把链路真正跑通」固化成可照做的剧本，按门槛从低到高分三个场景。
> **读者**：用户本人（执行者）。Agent 只能在每个外呼点获得用户明确授权后参与。
> **输入**：`backend-boot-smoke.md`（boot 配方）· `smoke-scripts/`（打点资产）· `contract-deviations.md`（17 行契约基线）· `docs/plans/2026-09-09-moya-compliance-verification.md`（门禁依据）· `STEP-18/`（试点包 7 材料）。

---

## 0. 三场景与门槛对照

| 场景 | 新增门槛 | 能闭合的遗留项 | ⚠ 新增禁令授权点（须用户逐项授权） |
|---|---|---|---|
| **A** 微信开发版联调（测试 AppID + 开发者工具 + 真机） | 开发者工具安装；测试后台隐私指引配置；Garage 起活 | 契约矩阵 #1 wechat jscode2session 真腿；#17 confirm 真传腿；presigned PUT vs wx.uploadFile 真机兼容（gap#3） | 微信 jscode2session 首次真实外呼 |
| **B** 接真实 vision 模型 | 模型选型 + API key（成本对比已备）；（可选）ffmpeg 抽帧管线 | Step 11 真实模型腿（当前全 mock/降级） | 付费模型调用 |
| **C** 真实试点（单班真实数据） | 合规文档 §3 阶段二 8 项 GO 必要条件 | 真实班级全链 | 真实儿童数据 + 试点启动（用户人工批准） |

---

## 1. 场景 A：微信开发版联调（预计半天）

### A1 后端就绪

1. 独立冒烟库 `moya_boot_smoke@127.0.0.1:4323`（配方见 `backend-boot-smoke.md`；**勿 `make run`**——会重建用户 `_rel`；种子勿打进 `imboy_v1`/用户库）。
2. `psql -d moya_boot_smoke -f smoke-scripts/seed_smoke.sql`（7xxxx 段：orgA=771000 / teacher+owner=770001 / guardian=770002 / assistant=770003 / manager=770004 / learner=774001）。
3. boot 冒烟节点：`HTTP_PORT=9811 erl -name imboy_smoke@127.0.0.1 …`（直连 erl + /tmp 配置副本，见 boot-smoke 文档）。
4. 验活：`ecron:statistic()` 应见 8/8 activate（`teaching_ai_worker` / `teaching_unbound_cleanup` / `teaching_ai_stuck_reclaim` + 存量 5 条）。
   - 若改用重启后的 9800/9801：**知悉项**——重启后定时作业首次真正激活，支付对账首跑回看 25h 属预期非告警。
5. Garage 起活（`imboy/scripts/garage-local-setup`）——confirm 真传腿依赖真实对象存储。

### A2 小程序侧

1. `project.config.json` 的 `appid: touristappid` → 换测试 AppID（用户操作）。
2. ⚠ **先在测试后台配置「用户隐私保护指引」**（声明摄像头/相册/视频），否则 `wx.chooseMedia`/`camera` 被平台拦截（合规文档 §1.6）——测试 AppID 也要配。
3. 微信开发者工具导入 `moya/`，控制台执行：
   `wx.setStorageSync('moya.debug.api_base', 'http://127.0.0.1:9811')`（`src/core/env.ts` 调试覆盖机制）。
4. 真机预览（本人设备；拍摄按 ≤60s 设计，chooseMedia 上限）。

### A3 链路剧本（括号 = 契约矩阵行号，见 contract-deviations §1）

1. 真机微信登录（#1 真腿首次实测）→ 家长侧 guardian 身份确认。
2. 真机登录产生的新 uid 需与 seed 的 learner 补 bind（API 姿势照 `matrix_bind.sh`；守卫：非 guardian→403+5429、invalid target→422+5428）。
3. 家长端拍摄提交作业：presign（#17）→ PUT 真传 → confirm（**真传腿首次实测**；对象未真实上传时 confirm 保持 400 fail-closed）。
4. 教师端 workbench 查看提交 → 此刻 AI 草稿为 `failed(provider_unavailable)` **属预期**（场景 B 后变 drafted）→ 教师人工撰写/编辑并发布（#11 幂等：重复发布 already_published=true）。
5. 家长 history 双视角 + 解绑后本人立即 5423（BIND-02 回归）。
6. 联调后回归：跑 `smoke-scripts/` 7 步全序列，基线=contract-deviations §1；**新偏差记入其 §7 新节**（沿用 D-8 起编号）。

### A4 场景 A 重点观察项

- **presigned PUT vs `wx.uploadFile` 真机兼容性**：30-60MB 视频走 ArrayBuffer 的内存风险（final-report gap#3）；若不兼容，评估后端补 POST 直传端点并登记 STEP-17-PREP。
- **TSID 全 string**：D-4 修复后 bind/unbind 已归一 string，真机重点盯 JS 侧无精度丢失（id/user_id/organization_id）。

---

## 2. 场景 B：接真实 vision 模型（须先授权付费调用）

1. **选型**（依据 `2026-09-09-moya-ai-model-cost-comparison.md` §五）：开发/内测期 GLM-4.6V-Flash（免费）；试点主力 qwen3-vl-flash（单次批改 ≈¥0.0006–0.005）；质量对照 GLM-4.6V / qwen3-vl-plus 每周抽样。
2. **provider 接线**：imboy_llm registry 配置该 provider，API key 走 env 注入（⚠ 密钥空串会跳过 fail-fast 的历史教训）；核对注册表中该 provider 的 vision 能力标记与实际一致（当前全 vision=false 是 Step 11 BLOCKED 的唯一原因）。
3. **抽帧规格**：按 `2026-09-09-moya-frame-sampling-and-rubric-spec.md`（12–24 过程帧 + 成品图 + 范字图，~20K tokens/次；**P0=纯成品图路径**，视频过程帧是增值项——薄冷启动可先不做视频）。该文为用户调研文档，落地前与用户确认采纳范围；服务端需 ffmpeg 抽帧 + 帧差去重（SSIM>0.95 丢弃）。
4. **标尺进 prompt**：三层（笔法/结构/章法）+ 结构化 JSON 输出；硬规则：表扬具体到笔画、每篇改进点 ≤2、用等级不用分数。
5. **验收清单**：
   - 提交 → `teaching_ai_worker:run_once` → ai_status=queued→drafted → workbench ai_draft 可见；
   - AI-03 四形降级回归（拔 key → failed 且不阻塞教师人工发布）；
   - 1–2 笔真实提交后核对账单与成本预期；
   - 教师编辑 diff 回写机构标尺 few-shot（护城河项，P1 可后置）。

---

## 3. 场景 C：真实试点——只列门，不执行

- 门禁 = 合规文档 §3 阶段二表，8 项 GO 必要条件逐项勾（主体承载视频类目 / 处理者-受托方协议 / 监护人单独同意 / 儿童隐私规则发布 / 四路径演练 / ACL E2E / ICP 备案 / Go-NoGo 人工批准）。
- 材料 = `STEP-18/`：pilot-design · go-nogo-report · guardian-materials · teacher-playbook · metrics-template · baseline-survey（当前判定 NO-GO 等门禁；律师复核 §4.1/§4.4）。
- 四路径演练（删除/导出/更正/解绑）按合规 §4.3 验证标准执行。
- **Agent 不启动试点、不接触真实儿童数据；Go/No-Go 由用户人工批准。**

---

## 4. 红线延续（对既有禁令的增量说明）

- §0 表「新增授权点」每一项都是独立授权：微信外呼 / 付费模型 / 真实儿童数据 / 试点启动——逐项经用户明确同意后才能动，一项授权不外溢另一项。
- 联调全程使用 7xxxx 段合成种子；正式库与用户节点（9800/9801）不打测试种子。
- 冒烟节点收尾照 `halt_smoke.sh`，确认 9811/epmd 释放，不触碰用户节点。
