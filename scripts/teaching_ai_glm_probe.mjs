#!/usr/bin/env node
/**
 * teaching_ai_glm_probe.mjs — 墨芽 AI 回课视频理解对接验证（GLM-4.6V-Flash）。
 *
 * 复刻 teaching_ai_provider:build_messages/2 的视频路径 prompt（system +
 * video_url + task JSON），直打智谱 OpenAI 兼容端点，展示结构化点评效果。
 * 与后端 registry 条目 bigmodel（config/sys.local.config）同参：model/thinking。
 *
 * 用法：
 *   export BIGMODEL_API_KEY=<id.secret>
 *   node imboy/scripts/teaching_ai_glm_probe.mjs [视频URL]
 * 视频缺省用智谱官方示例（链路验证）；书法实测请传练习视频公网 URL。
 *
 * 模型切换（对比效果用）：export BIGMODEL_MODEL=<模型名>，缺省 glm-4.6v-flash，
 * 与后端 sys.local.config bigmodel 条目 {env, <<"BIGMODEL_MODEL">>, ...} 同源，
 * 免费视觉理解模型如 glm-4.1v-thinking-flash。注意 CogVideoX-Flash 是
 * 视频生成模型（异步生成接口，非 chat/completions），不能用于本脚本/回课理解。
 */
import process from "node:process";

const KEY = process.env.BIGMODEL_API_KEY;
if (!KEY) {
  console.error("缺少 BIGMODEL_API_KEY（export BIGMODEL_API_KEY=<id.secret>）");
  process.exit(2);
}

const VIDEO_URL =
  process.argv[2] ?? "https://cdn.bigmodel.cn/agent-demos/lark/113123.mov";
const MODEL = process.env.BIGMODEL_MODEL ?? "glm-4.6v-flash";
const ENDPOINT = "https://open.bigmodel.cn/api/paas/v4/chat/completions";

const task = {
  task: "calligraphy_video_review",
  rubric_version: "r-hardpen-1",
  prompt_version: "p-2026-09-09.1",
  attachment: { object_key: "probe", mime_type: "video/quicktime" },
  output_schema:
    "positive_point/focus_problem/evidence_moments/" +
    "practice_action/script_outline/needs_human_check/confidence",
};
const instruction =
  "请观看视频中的书写过程，依据上述 output_schema 输出 JSON 点评；" +
  "evidence_moments 为视频内秒级时间点（0-5 个）。";

const body = {
  model: MODEL,
  messages: [
    {
      role: "system",
      content: "你是书法老师的教学助手。只输出 JSON，不输出推理过程。",
    },
    {
      role: "user",
      content: [
        { type: "video_url", video_url: { url: VIDEO_URL } },
        { type: "text", text: `${JSON.stringify(task)}\n${instruction}` },
      ],
    },
  ],
  thinking: { type: "disabled" },
};

const t0 = Date.now();
const resp = await fetch(ENDPOINT, {
  method: "POST",
  headers: { "Content-Type": "application/json", Authorization: `Bearer ${KEY}` },
  body: JSON.stringify(body),
});
const ms = Date.now() - t0;
const raw = await resp.text();

console.log(`HTTP ${resp.status}（${ms}ms）model=${MODEL}`);
let data;
try {
  data = JSON.parse(raw);
} catch {
  console.error("非 JSON 响应：", raw.slice(0, 2000));
  process.exit(1);
}
if (!resp.ok) {
  console.error("调用失败：", JSON.stringify(data, null, 2));
  process.exit(1);
}

const msg = data.choices?.[0]?.message ?? {};
if (msg.reasoning_content) {
  console.log("--- reasoning_content（思考模式输出，后端不落库）---");
  console.log(String(msg.reasoning_content).slice(0, 800));
}
console.log("--- content ---");
console.log(msg.content ?? "(空)");

// 复刻 validate_result/1 白名单（AI-02：思维链/额外键不落库）
try {
  const parsed = JSON.parse(msg.content);
  const allow = [
    "positive_point",
    "focus_problem",
    "evidence_moments",
    "practice_action",
    "script_outline",
    "needs_human_check",
    "confidence",
  ];
  const whitelisted = Object.fromEntries(
    Object.entries(parsed).filter(([k]) => allow.includes(k)),
  );
  console.log("--- 白名单落库结果（validate_result 同款）---");
  console.log(JSON.stringify(whitelisted, null, 2));
} catch {
  console.error("⚠ content 不是合法 JSON（bad_output 路径）");
}
console.log(`--- usage --- ${JSON.stringify(data.usage ?? {})}`);
