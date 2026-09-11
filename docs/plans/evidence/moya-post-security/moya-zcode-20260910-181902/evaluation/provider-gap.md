# 墨芽习字 AI provider 现状与 rubric 映射缺口 provider-gap-v0

> 卡片：MN-EVAL-04 | 计划：`docs/plans/2026-09-10-moya-post-security-product-plan.md` §2.4 / §Phase 1 / §Phase 2B / §7
> 代码基线：imboy @ 8fb7ed11b76efeb6a5c32fe605897dd10f1a66a5（A1 worktree）
> 日期：2026-09-10 | RUN_ID：moya-zcode-20260910-181902 | Agent：A1
> 结论先行：**AI 点评当前不可用；`provider_unavailable` 为现行主路径，降级老师人工队列。本文不虚构任何视觉能力。**

---

## 1. 现状真值（源码行号佐证）

### 1.1 无 provider 声明 vision=true

LLM behaviour 契约要求实现方声明能力：

- `src/lib/imboy_llm.erl:23-24`：`-callback capabilities() -> #{stream := boolean(), vision := boolean(), tools := boolean()}.`

注册表内**仅有两个**实现模块，两者 vision 均为 false：

- `src/lib/imboy_llm_qianfan.erl:37-39`：`capabilities() -> #{stream => false, vision => false, tools => false}.`
- `src/lib/imboy_llm_openai.erl:60-62`：`capabilities() -> #{stream => true, vision => false, tools => false}.`
- `src/lib/imboy_llm_registry.erl:33-34`：内置注册表仅 `builtin() -> [#{name => <<"qianfan">>, module => imboy_llm_qianfan}]`（配置可追加 openai 等，但现有实现均无 vision）。

`teaching_ai_provider` 对 vision/key 的守卫（fail-closed 折叠）：

- `src/logic/teaching_ai_provider.erl:80-106` `resolve_provider/1`：
  - L84-94：读取 `Mod:capabilities()`，仅 `#{vision := true}` 视为通过（`VisionOK`）；
  - L95-102：`{VisionOK, HasKey}` 非 `{true, true}` 一律 → `{error, provider_unavailable}`（不重试）；
  - L78-79 / L104-106：未配置 provider 名 / registry 查无 → `{error, provider_unavailable}`。
- 模块头注释 L6-10 明示：「imboy_llm 现有 provider capabilities().vision 全部 = false —— 真实多模态调用被外部条件阻塞」。

### 1.2 请求只携带附件 object_key 文本元数据（无图像/视频内容）

- `src/lib/imboy_llm.erl:9-10`：Messages 契约为 `[#{<<"role">> => binary(), <<"content">> => binary()}]` —— `content` 是**纯文本 binary**，不存在多模态 content parts（`image_url`/`input_audio` 等）结构。
- `src/logic/teaching_ai_provider.erl:159-182` `build_messages/2`：
  - L163-175：user 消息体是 `jsone:encode` 的任务 JSON，其中 `attachment` 对象**只有** `object_key`（取 `Attachment.path`）与 `mime_type` 两个文本字段（L167-170）；
  - L171-174：`output_schema` 以文本提示白名单键名；
  - L176-181：system 提示「只输出 JSON，不输出推理过程」。
- 即：**图片/视频字节、帧、任何视觉内容从未进入请求**；模型仅收到「存在一个附件，其对象键与 MIME 类型」的文本描述。注释 L132-133 佐证：「多模态帧引用 BLOCKED_EXTERNAL 留待 vision provider 接入，骨架阶段 prompt 只携带业务元数据与附件 object_key 引用」。
- Worker 侧附件解析：`src/logic/teaching_ai_worker.erl:156-178` `load_attachment/2` 只 SELECT `att.id, att.path, att.mime_type, att.size`（元数据）供 provider 使用。

### 1.3 provider_unavailable 降级人工闭环

- `src/logic/teaching_ai_worker.erl:25-27`：`?TRANSIENT_ERRORS = [timeout, provider_error]`；`provider_unavailable` **不在**瞬时错误表 → 直接终态。
- `src/logic/teaching_ai_worker.erl:222-237` `dispatch_failure/4`：非瞬时 → `ai_finish_failed_tx(Conn, DraftId, <<"provider_unavailable">>)`，草稿行 status=failed + error_code 落库。
- `src/logic/teaching_ai_worker.erl:6-9` 模块头注释：「provider_unavailable 是当前**主路径**：status=failed + error_code=provider_unavailable → 老师人工队列照常工作（Step 9 队列不过滤 ai_status，failed 仍显示）」。

## 2. 现有结构化输出契约字段清单（源码提取）

`teaching_ai_provider:validate_result/1`（`src/logic/teaching_ai_provider.erl:45-70`）白名单重建，仅以下键允许落库，其余键（含思维链片段）被结构性丢弃（L14-15 注释）：

| 字段 | 类型与约束（源码行号） | 必填 |
|---|---|---|
| `positive_point` | binary，1-300 字节（L47, L186-191 `req_text`） | 是 |
| `focus_problem` | binary，1-300 字节（L48, L186-191） | 是 |
| `practice_action` | binary，1-300 字节（L49, L186-191） | 是 |
| `evidence_moments` | 数字列表，1-5 个，每个 ≥0（L50, L193-209 `req_moments`） | 是 |
| `script_outline` | binary 列表，≤3 条，每条 ≤200 字节（L51, L211-227 `req_outline`） | 是 |
| `needs_human_check` | boolean（L52-56） | 是 |
| `confidence` | 数字 0-1；缺失或越界则不落库（L65, L229-236 `maybe_confidence`） | 否 |

任一字段非法 → 整体 `{error, bad_output}`（L66-68），Worker 映射为 error_code=`bad_schema`（`teaching_ai_worker.erl:262-263`）。

## 3. 逐字段映射到 rubric-v0 五维度（MN-EVAL-01）

| 契约字段 | rubric 维度 | 支撑程度 | 说明 |
|---|---|---|---|
| `positive_point` | D1 事实正确性 / D2 具体性 | 部分 | 文本可承载事实陈述与具体位置；但正确性无从校验（当前无视觉输入，内容系模型凭任务元数据想象，D1 在真实 vision 接入前**不可信**） |
| `focus_problem` | D1 事实正确性 / D2 具体性 | 部分 | 同上 |
| `practice_action` | D3 可行动性 | 直接对应 | 字段语义即练习指引；是否有执行参数取决于文本质量 |
| `evidence_moments` | D2 具体性 | 形式对应、实质空转 | 时间点数值可定位（对应视频时刻）；**当前无视觉输入，数值不可信**，接入 vision 后才具评测意义 |
| `script_outline` | D3 可行动性 / D4 语气适龄 | 部分 | 大纲承载录制顺序（D3）与措辞基调（D4 的间接来源） |
| `needs_human_check` | D5 危险/错误教学判断 | 弱信号 | 仅表达「模型自认需人工复核」，不是危险教学判断的检测器 |
| `confidence` | （无 rubric 对应） | — | 供统计/阈值参考，不映射评分维度 |

### 无对应输出=缺口的维度

- **D4 语气适龄**：无专用字段。语气评估完全依赖老师阅读全文；无结构化信号（如 audience/tone 声明）。
- **D5 危险/错误教学判断（一票否决项）**：无专用输出字段。`needs_human_check=true` 不区分「拿不准」与「检测到危险/错误教学」；一票否决的判定 100% 依赖老师，契约无结构性支撑。这是最大的安全缺口：**若未来 AI 输出直接触达家长（当前计划禁止），此缺口为硬阻断**。
- **第 6 项老师修改量**：与 provider 无关，纯外部评测量。

## 4. 明确声明

1. **AI 点评当前不可用。** 无 provider 声明 `vision=true`、请求无视觉内容（§1.1/§1.2），任何对当前输出 D1/D2/D5 的评分都没有意义；`provider_unavailable` 降级老师人工是现行主路径且已闭合。
2. **不虚构视觉能力。** 本文档所述均为源码可证事实；对未来 vision provider 接入后的能力不做任何预期性声明。
3. 上述契约字段清单即 Phase 2B「冻结单一结构化输出契约」的起点；新增字段（如 D5 专用标记）须在 Phase 2B 授权后经共享契约复核，不得在本卡擅改。

## 5. 候选 provider 接入需要的外部授权清单（引用计划 §7 外部门表）

接入任一 vision provider 前，以下决定**必须逐项取得用户明确授权**（计划 §7：「没有这些决定时……Phase 2-4 保持 BLOCKED_EXTERNAL，不得用 mock 或合成声明替代」）：

| # | 需授权项 | 计划 §7 对应行 | 备注 |
|---|---|---|---|
| 1 | provider 选择（厂商/接入方式） | 视觉模型行：「provider、model、费用上限、凭据位置、数据可发送范围」 | 复用 `imboy_llm` registry + `teaching_ai_provider` 校验路径（§Phase 2B 最小实现） |
| 2 | 具体 vision model ID | 同上 | 模型价格执行当日以官方控制台为准 |
| 3 | 费用上限（单次/全量） | 同上 | 20 份 A/B 成对调用的预算口径 |
| 4 | 凭据位置（独立于仓库） | 同上 | 经 `imboy_llm_registry:resolve_env` 的 `{env, VAR}` 注入，不写死配置（`src/lib/imboy_llm_registry.erl:44-55`） |
| 5 | 数据可发送范围（哪些媒体可出域、保存期） | 同上 | 请求只发送此次评测所需媒体，不携带姓名/微信身份/监护关系/无关历史（§Phase 2B） |
| 6 | 20 份样本的来源、去标识方式、授权与保存期 | 样本行：「20 份材料来源、去标识方式、授权和保存期」 | manifest（MN-EVAL-02）只存非身份元数据 |

技术前置（授权后实施，非本卡范围）：候选模块须实现 `capabilities() -> #{vision := true, ...}` 并扩展 Messages 为多模态 content parts；`teaching_ai_provider` 的守卫（L90-102）与白名单校验无需改动即可复用。
