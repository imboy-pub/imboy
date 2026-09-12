#!/usr/bin/env bash
# BIGMODEL（智谱开放平台）probe：验证 API key、模型可用性与多模态输入。
# 用法：
#   BIGMODEL_API_KEY=xxx ./bigmodel_probe.sh [VIDEO_URL]
#   PROBE_VIDEO=1 同步跑 CogVideoX-Flash 生成任务提交（异步，只提交不轮询）
# 说明：
#   - key 只从环境变量读取，绝不写入任何文件
#   - BIGMODEL_MODEL 覆盖模型（与 imboy sys config 的 {env,"BIGMODEL_MODEL"} 同源，
#     默认 glm-5.3-flash）。glm-4.6v-flash 免费但实测 100% 返回 1305 已弃用；
#     免费备选 glm-4.1v-thinking-flash（思维链内联，需脱壳）。
#     CogVideoX-Flash/-3 是【生成】模型（异步 /videos/generations），
#     不接 chat/vision；glm-5.3-flash 亦不能生成视频
#   - 该模型【始终思考】：max_tokens 给小了会把额度耗在思考上、content 为空，
#     因此段1 给足 400 并同时校验 content 非空（只看 finish_reason 会假绿）
#   - 远端图片 URL【不被接受】（实测 4/4 报 1210），图片需 base64 data URI；
#     远端视频 URL 正常。故段2 用视频 URL（与生产回课链路一致）
# Exit 0 = PASS, non-zero = FAIL。

set -u
set -o pipefail

API_BASE="https://open.bigmodel.cn/api/paas/v4"
MODEL="${BIGMODEL_MODEL:-glm-5.3-flash}"
VIDEO_URL="${1:-https://test-videos.co.uk/vids/bigbuckbunny/mp4/h264/360/Big_Buck_Bunny_360_10s_1MB.mp4}"

if [[ -z "${BIGMODEL_API_KEY:-}" ]]; then
    echo "FAIL: BIGMODEL_API_KEY 未设置（export 后重跑；key 不落盘）"
    exit 1
fi
echo "=== BIGMODEL probe ==="
echo "model=${MODEL}"
echo

# 1) 基础 chat：key 与模型可用性（始终思考模型需给足 max_tokens）
echo "--- [1] chat（key/模型可用性） ---"
CHAT_RESP=$(curl -sS --max-time 90 "${API_BASE}/chat/completions" \
    -H "Authorization: Bearer ${BIGMODEL_API_KEY}" \
    -H "Content-Type: application/json" \
    -d "{\"model\":\"${MODEL}\",\"messages\":[{\"role\":\"user\",\"content\":\"回复两个字：正常\"}],\"max_tokens\":400}")
CHAT_RC=$?
echo "${CHAT_RESP}" | head -c 400
echo
if [[ ${CHAT_RC} -ne 0 ]]; then
    echo "FAIL: chat curl rc=${CHAT_RC}"
    exit 1
fi
printf '%s' "${CHAT_RESP}" | grep -q '"finish_reason"' || {
    echo "FAIL: 响应无 finish_reason（key 无效/模型名错误/额度问题，见上方原文）"
    exit 1
}
printf '%s' "${CHAT_RESP}" | grep -qE '"content":"[^"]' || {
    echo "FAIL: content 为空（该模型始终思考，max_tokens 不足；或模型名有误）"
    exit 1
}
echo

# 2) 视频理解：远端 video_url（与生产回课链路一致）
echo "--- [2] 视频理解（video_url） ---"
VISION_RESP=$(curl -sS --max-time 180 "${API_BASE}/chat/completions" \
    -H "Authorization: Bearer ${BIGMODEL_API_KEY}" \
    -H "Content-Type: application/json" \
    -d "{\"model\":\"${MODEL}\",\"messages\":[{\"role\":\"system\",\"content\":\"你是书法老师的教学助手。只输出 JSON，不输出推理过程。\"},{\"role\":\"user\",\"content\":[{\"type\":\"video_url\",\"video_url\":{\"url\":\"${VIDEO_URL}\"}},{\"type\":\"text\",\"text\":\"用一句话描述视频内容\"}]}],\"max_tokens\":1000}")
VISION_RC=$?
echo "${VISION_RESP}" | head -c 500
echo
if [[ ${VISION_RC} -ne 0 ]]; then
    echo "FAIL: video curl rc=${VISION_RC}"
    exit 1
fi
printf '%s' "${VISION_RESP}" | grep -q '"finish_reason"' || {
    echo "FAIL: 视频理解异常（1210=远端视频抓取/解析失败，可重试；"
    echo "      若报『模型不支持』说明当前模型非视觉系列，检查 BIGMODEL_MODEL）"
    exit 1
}
printf '%s' "${VISION_RESP}" | grep -qE '"content":"[^"]' || {
    echo "FAIL: content 为空（思考额度耗尽或模型不支持视频）"
    exit 1
}
echo

# 3) 可选：CogVideoX 生成任务提交（PROBE_VIDEO=1 时）
if [[ "${PROBE_VIDEO:-0}" = "1" ]]; then
    echo "--- [3] CogVideoX-Flash 生成任务提交 ---"
    VIDEO_RESP=$(curl -sS --max-time 60 "${API_BASE}/videos/generations" \
        -H "Authorization: Bearer ${BIGMODEL_API_KEY}" \
        -H "Content-Type: application/json" \
        -d '{"model":"cogvideox-flash","prompt":"一只猫在草地上打滚，写实风格"}')
    echo "${VIDEO_RESP}" | head -c 400
    echo
    printf '%s' "${VIDEO_RESP}" | grep -q '"id"' && echo "生成任务已提交（异步，id 见上，可轮询 /api/paas/v4/async-result/{id}）"
    echo
fi

echo "=== BIGMODEL probe PASS（model=${MODEL}） ==="
exit 0
