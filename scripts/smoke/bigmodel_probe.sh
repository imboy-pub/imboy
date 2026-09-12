#!/usr/bin/env bash
# BIGMODEL（智谱开放平台）probe：验证 API key 与模型切换配置。
# 用法：
#   BIGMODEL_API_KEY=xxx ./bigmodel_probe.sh [VISION_IMAGE_URL]
#   PROBE_VIDEO=1 同步跑 CogVideoX-Flash 生成任务提交（异步，只提交不轮询）
# 说明：
#   - key 只从环境变量读取，绝不写入任何文件
#   - BIGMODEL_MODEL 覆盖模型（与 imboy sys config 的 {env,"BIGMODEL_MODEL"} 同源，
#     默认 glm-4.6v-flash；切 CogVideoX-Flash 等生成模型不影响理解链路——
#     生成模型只接视频生成，不接 chat/vision）
#   - Vision 段需要一张公网可达的图片 URL（默认参数 2 或内置样例）
# Exit 0 = PASS, non-zero = FAIL。

set -u
set -o pipefail

API_BASE="https://open.bigmodel.cn/api/paas/v4"
MODEL="${BIGMODEL_MODEL:-glm-4.6v-flash}"
IMAGE_URL="${1:-https://upload.wikimedia.org/wikipedia/commons/thumb/3/3a/Cat03.jpg/320px-Cat03.jpg}"

if [[ -z "${BIGMODEL_API_KEY:-}" ]]; then
    echo "FAIL: BIGMODEL_API_KEY 未设置（export 后重跑；key 不落盘）"
    exit 1
fi
echo "=== BIGMODEL probe ==="
echo "model=${MODEL}"
echo

# 1) 基础 chat：key 与模型可用性
echo "--- [1] chat（key/模型可用性） ---"
CHAT_RESP=$(curl -sS --max-time 60 "${API_BASE}/chat/completions" \
    -H "Authorization: Bearer ${BIGMODEL_API_KEY}" \
    -H "Content-Type: application/json" \
    -d "{\"model\":\"${MODEL}\",\"messages\":[{\"role\":\"user\",\"content\":\"回复两个字：正常\"}],\"max_tokens\":16}")
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
echo

# 2) vision：图片理解（GLM-4.6V 系列；纯文本模型会报错=预期切模型）
echo "--- [2] vision（image_url 理解） ---"
VISION_RESP=$(curl -sS --max-time 90 "${API_BASE}/chat/completions" \
    -H "Authorization: Bearer ${BIGMODEL_API_KEY}" \
    -H "Content-Type: application/json" \
    -d "{\"model\":\"${MODEL}\",\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"image_url\",\"image_url\":{\"url\":\"${IMAGE_URL}\"}},{\"type\":\"text\",\"text\":\"用一句话描述图片内容\"}]}]}")
VISION_RC=$?
echo "${VISION_RESP}" | head -c 500
echo
if [[ ${VISION_RC} -ne 0 ]]; then
    echo "FAIL: vision curl rc=${VISION_RC}"
    exit 1
fi
printf '%s' "${VISION_RESP}" | grep -q '"finish_reason"' || {
    echo "FAIL: vision 响应异常（若报『模型不支持』说明当前模型非 vision 系列，检查 BIGMODEL_MODEL）"
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
