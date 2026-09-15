#!/usr/bin/env bash
# ============================================================
# 微信小程序登录配置体检 / WeChat mini-program login config preflight
# ------------------------------------------------------------
# 为什么需要它：
#   「家长打开小程序 → wx.login → 后端 jscode2session → openid」是**唯一**的
#   进门通道。它的两个配置项（wechat_mini_appid / wechat_mini_secret）只要有一个
#   是错的，表现都是**登录失败**，而且：
#     - 静态检查看不出来：AppSecret 是 32 位随机串，写错一位和写对一位长得一样；
#     - 客户端只看到笼统的错误文案，真机上的第一反应会是「域名白名单没配」
#       「网络问题」，从而在完全错误的方向上排查很久（本项目真实踩过）。
#
#   本脚本用**微信自己的判据**把这一项验掉：拿一个**故意无效的 js_code** 去调
#   jscode2session，然后读 errcode：
#     - 40013 invalid appid     → AppID 错
#     - 40125 invalid appsecret → **AppSecret 错**（AppID 已被微信接受）
#     - 40029 invalid code      → ✅ AppID 与 AppSecret **都已被微信接受**
#       假 code 本来就该报这个 —— 所以 40029 正是「配置正确」的信号。
#   这个判据不需要真实用户、不需要真机、不影响任何线上数据。
#
#   注意 errcode 的先后顺序本身也是信息：拿到 40125 而不是 40013，说明
#   **AppID 是真实存在的**，只有 secret 有问题 —— 不必怀疑 AppID 抄错。
#
# 用法：
#   bash scripts/check_wechat_login_config.sh                    # 默认 config/sys.pro.config
#   bash scripts/check_wechat_login_config.sh config/sys.local.config
#   bash scripts/check_wechat_login_config.sh --no-net            # 只做静态体检，不出网
#
# 退出码：0 = 配置被微信接受（或 --no-net 且静态检查无碍）；1 = 失败；2 = 用法错误。
# ============================================================
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="${IMBOY_ROOT:-$(cd "${SCRIPT_DIR}/.." && pwd)}"

CONF=""
DO_NET=1
for arg in "$@"; do
  case "${arg}" in
    --no-net) DO_NET=0 ;;
    -h|--help) sed -n '2,40p' "${BASH_SOURCE[0]}"; exit 0 ;;
    -*) echo "未知参数：${arg}" >&2; exit 2 ;;
    *) CONF="${arg}" ;;
  esac
done
if [ -z "${CONF}" ]; then
  CONF="${ROOT}/config/sys.pro.config"
elif [ "${CONF#/}" = "${CONF}" ]; then
  CONF="${ROOT}/${CONF}"
fi

if [ ! -f "${CONF}" ]; then
  echo "✗ 配置文件不存在：${CONF}" >&2
  echo "  提示：生产配置是 gitignored 的真实值文件（config/sys.pro.config），" >&2
  echo "        若本机没有，请把该文件拷到本地或显式传入路径。" >&2
  exit 1
fi

echo "配置文件：${CONF}"

# 取某个键的 binary 值：跳过注释行，只认行首就是该键的条目。
read_key() { # $1 = 键名
  grep -vE '^[[:space:]]*%' "${CONF}" \
    | grep -E "^[[:space:]]*[{]$1[[:space:]]*," \
    | head -1 \
    | sed -E 's/.*<<"([^"]*)".*/\1/'
}

APPID="$(read_key wechat_mini_appid)"
SECRET="$(read_key wechat_mini_secret)"

if [ -z "${APPID}" ]; then
  echo "✗ 未在本文件读到 wechat_mini_appid" >&2
  exit 1
fi
if [ -z "${SECRET}" ]; then
  echo "✗ 未在本文件读到 wechat_mini_secret（缺该项 ⇒ 登录必失败）" >&2
  exit 1
fi

# 只在本地比对时用，绝不整串回显 —— 体检脚本的输出常被贴进聊天窗口。
SECRET_HEAD="$(printf '%s' "${SECRET}" | cut -c1-4)"
SECRET_LEN="$(printf '%s' "${SECRET}" | wc -c | tr -d ' ')"

echo "  wechat_mini_appid  = ${APPID}"
echo "  wechat_mini_secret = ${SECRET_HEAD}****（长度 ${SECRET_LEN}）"

STATIC_BAD=0

# 真实 AppSecret 恒为 32 位小写十六进制。格式不符基本可以断定是占位值/抄错列。
case "${SECRET_LEN}" in
  32) ;;
  *)
    echo "  ✗ AppSecret 长度 ${SECRET_LEN}，真实值是 32 —— 长度不符 ⇒ 几乎可以确定不是真值"
    STATIC_BAD=1
    ;;
esac
if printf '%s' "${SECRET}" | grep -qE '^[0-9a-f]{32}$'; then
  :
elif [ "${SECRET_LEN}" = "32" ]; then
  echo "  ✗ AppSecret 含非小写十六进制字符 —— 真实值只由 0-9a-f 组成"
  STATIC_BAD=1
fi

if [ "${DO_NET}" = "0" ]; then
  if [ "${STATIC_BAD}" = "1" ]; then
    echo "✗ 静态体检未通过（--no-net，未出网验证）"
    exit 1
  fi
  echo "· 已跳过出网验证（--no-net）：仅证明格式合法，**未证明微信接受这对凭据**"
  exit 0
fi

echo "  · 调用 jscode2session（故意使用无效 js_code，不改动任何线上数据）..."
RESP="$(curl -s -m 15 -G "https://api.weixin.qq.com/sns/jscode2session" \
  --data-urlencode "appid=${APPID}" \
  --data-urlencode "secret=${SECRET}" \
  --data-urlencode "js_code=moya_preflight_invalid_code" \
  --data-urlencode "grant_type=authorization_code" 2>/dev/null)"

if [ -z "${RESP}" ]; then
  echo "✗ 调不通 api.weixin.qq.com（网络/DNS/代理问题，与凭据无关）" >&2
  exit 1
fi

ERRCODE="$(printf '%s' "${RESP}" | sed -E 's/.*"errcode":([0-9-]+).*/\1/')"
case "${ERRCODE}" in
  ''|*[!0-9-]*) ERRCODE="0" ;;
esac

case "${ERRCODE}" in
  40029)
    echo "  ✓ 40029 invalid code —— 这是**预期**结果：假 code 本应无效，"
    echo "    而错误码不是 40013/40125 ⇒ AppID 与 AppSecret **都已通过微信校验**。"
    echo "✓ 通过：wechat_mini_appid / wechat_mini_secret 可用（重启节点后生效）"
    exit 0
    ;;
  40013)
    echo "  ✗ 40013 invalid appid —— AppID 不是本 AppSecret 对应的那个" >&2
    exit 1
    ;;
  40125)
    echo "  ✗ 40125 invalid appsecret —— **AppSecret 是错的**" >&2
    echo "    注意：报的是 40125 而不是 40013 ⇒ AppID 真实存在，只有 secret 有问题。" >&2
    echo "    修法：微信公众平台 → 开发管理 → 开发设置 → 重置 AppSecret，" >&2
    echo "          把新值写进本文件的 wechat_mini_secret 并重启节点。" >&2
    exit 1
    ;;
  -1)
    echo "✗ -1 系统繁忙（微信侧限流/抖动）—— 非配置问题，稍后重试" >&2
    exit 1
    ;;
  0)
    echo "  ? 竟然成功返回了（假 code 不应成功）—— 可能是 wechat_mini_jscode_url" >&2
    echo "    被指向了 mock 服务。响应原文：${RESP}" >&2
    exit 1
    ;;
  *)
    echo "✗ 未预期的 errcode=${ERRCODE}：${RESP}" >&2
    exit 1
    ;;
esac
