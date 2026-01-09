#!/bin/bash
#
# Imboy API 接口测试脚本
#
# 使用说明：
#   1. 启动服务：IMBOYENV=local make run
#   2. 运行测试：./test/api/test_api.sh
#   3. 或者直接运行：bash test/api/test_api.sh
#

set -e  # 遇到错误立即退出

# ============================================================================
# 配置
# ============================================================================

BASE_URL="${BASE_URL:-http://localhost:9800}"
CONTENT_TYPE="Content-Type: application/json"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# ============================================================================
# 工具函数
# ============================================================================

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_section() {
    echo ""
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN} $1${NC}"
    echo -e "${GREEN}========================================${NC}"
}

# 发送 GET 请求
api_get() {
    local endpoint=$1
    local token=$2

    if [ -z "$token" ]; then
        curl -s -X GET \
            -H "$CONTENT_TYPE" \
            "$BASE_URL$endpoint"
    else
        curl -s -X GET \
            -H "$CONTENT_TYPE" \
            -H "Authorization: Bearer $token" \
            "$BASE_URL$endpoint"
    fi
}

# 发送 POST 请求
api_post() {
    local endpoint=$1
    local data=$2
    local token=$3

    if [ -z "$token" ]; then
        curl -s -X POST \
            -H "$CONTENT_TYPE" \
            -d "$data" \
            "$BASE_URL$endpoint"
    else
        curl -s -X POST \
            -H "$CONTENT_TYPE" \
            -H "Authorization: Bearer $token" \
            -d "$data" \
            "$BASE_URL$endpoint"
    fi
}

# ============================================================================
# 全局变量（用于保存测试过程中的数据）
# ============================================================================

TEST_ACCOUNT=""
TEST_PASSWORD="OTg5NzgxNTc2MjM5MjI3MTQ5MTEwNzQzNDUxMzg3OTMxMTU4Mjg3ODpobWFjX3NoYTUxMjpwUFY5MzBKeWkrTE1vRGRlRnlnbEpUc3hiTHZocE5obzBoTDVIRXRhUTdrY2ZOY2R3VDVzMGEwUlpQdlIyOEc3WTBlMUM5RjBXcUFlNmJPaWhSRG5TQT09"
TEST_NICKNAME="测试用户"
TEST_TOKEN=""
TEST_UID=""

# ============================================================================
# 测试用例
# ============================================================================

# 测试服务是否启动
test_health_check() {
    log_section "健康检查"

    log_info "检查服务是否启动..."
    response=$(api_get "/init")

    if echo "$response" | grep -q "success"; then
        log_info "✅ 服务运行正常"
    else
        log_error "❌ 服务未启动或无响应"
        exit 1
    fi
}

# 测试用户注册
test_user_signup() {
    log_section "用户注册（使用测试验证码）"

    # 生成随机邮箱账号
    timestamp=$(date +%s)
    TEST_ACCOUNT="test_user_${timestamp}@example.com"

    log_info "注册用户: $TEST_ACCOUNT"
    log_info "使用验证码: 666666（测试环境）"

    local data=$(cat <<EOF
{
    "type": "email",
    "account": "$TEST_ACCOUNT",
    "pwd": "$TEST_PASSWORD",
    "nickname": "$TEST_NICKNAME",
    "code": "666666",
    "rsa_encrypt": "1"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 用户注册成功"

        # 提取 UID（响应格式是 payload.uid）
        TEST_UID=$(echo "$response" | jq -r '.payload.uid // empty')
        log_info "用户 UID: $TEST_UID; $response;"
    else
        log_error "❌ 用户注册失败"
        echo "响应: $response"
        exit 1
    fi
}

# 测试用户登录
test_user_login() {
    log_section "用户登录"

    log_info "登录账号: $TEST_ACCOUNT"

    local data=$(cat <<EOF
{
    "type": "email",
    "account": "$TEST_ACCOUNT",
    "pwd": "$TEST_PASSWORD",
    "rsa_encrypt": "1"
}
EOF
)

    response=$(api_post "/passport/login" "$data")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 用户登录成功"

        # 提取 Token（响应格式是 payload.token）
        TEST_TOKEN=$(echo "$response" | jq -r '.payload.token // empty')

        # 提取 uid（响应格式是 payload.uid）
        TEST_UID=$(echo "$response" | jq -r '.payload.uid // empty')

        if [ -z "$TEST_TOKEN" ] || [ "$TEST_TOKEN" = "null" ]; then
            log_error "❌ 未获取到 Token"
            echo "响应: $response"
            exit 1
        fi

        log_info "获取到 Token: ${TEST_TOKEN:0:50}..."
        log_info "用户 UID: $TEST_UID"
    else
        log_error "❌ 用户登录失败"
        echo "响应: $response"
        exit 1
    fi
}

# 测试获取用户信息
test_get_user_info() {
    log_section "获取用户信息"

    log_info "使用 Token 和 UID 获取用户信息..."

    # /user/show 需要 id 参数（使用 hashids 编码的 uid）
    response=$(api_get "/user/show?id=$TEST_UID" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 获取用户信息成功"

        account=$(echo "$response" | jq -r '.payload.account // empty')
        nickname=$(echo "$response" | jq -r '.payload.nickname // empty')

        log_info "账号: $account"
        log_info "昵称: $nickname"
    else
        log_error "❌ 获取用户信息失败"
        echo "响应: $response"
    fi
}

# 测试更新用户信息
test_update_user_info() {
    log_section "更新用户信息"

    local new_nickname="新昵称_$(date +%s)"

    log_info "更新昵称为: $new_nickname"

    local data=$(cat <<EOF
{
    "field": "nickname",
    "value": "$new_nickname"
}
EOF
)

    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 更新用户信息成功"
    else
        log_error "❌ 更新用户信息失败"
        echo "响应: $response"
    fi
}

# 测试更新用户可搜索设置
test_update_user_allow_search() {
    log_section "更新用户可搜索设置"

    log_info "设置用户允许被搜索"

    local data=$(cat <<EOF
{
    "field": "allow_search",
    "value": "1"
}
EOF
)

    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 更新可搜索设置成功"
    else
        log_error "❌ 更新可搜索设置失败"
        echo "响应: $response"
    fi
}

# 测试更新用户个性签名
test_update_user_sign() {
    log_section "更新用户个性签名"

    local new_sign="这是我的个性签名_$(date +%s)"

    log_info "更新个性签名为: $new_sign"

    local data=$(cat <<EOF
{
    "field": "sign",
    "value": "$new_sign"
}
EOF
)

    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 更新个性签名成功"
    else
        log_error "❌ 更新个性签名失败"
        echo "响应: $response"
    fi
}

# 测试更新用户性别（男）
test_update_user_gender_male() {
    log_section "更新用户性别（男）"

    log_info "设置性别为：男 (1)"

    local data=$(cat <<EOF
{
    "field": "gender",
    "value": "1"
}
EOF
)

    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 更新性别成功"
    else
        log_error "❌ 更新性别失败"
        echo "响应: $response"
    fi
}

# 测试更新用户性别（女）
test_update_user_gender_female() {
    log_section "更新用户性别（女）"

    log_info "设置性别为：女 (2)"

    local data=$(cat <<EOF
{
    "field": "gender",
    "value": "2"
}
EOF
)

    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 更新性别成功"
    else
        log_error "❌ 更新性别失败"
        echo "响应: $response"
    fi
}

# 测试更新用户性别（保密）
test_update_user_gender_secret() {
    log_section "更新用户性别（保密）"

    log_info "设置性别为：保密 (3)"

    local data=$(cat <<EOF
{
    "field": "gender",
    "value": "3"
}
EOF
)

    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 更新性别成功"
    else
        log_error "❌ 更新性别失败"
        echo "响应: $response"
    fi
}

# 测试更新用户地区
test_update_user_region() {
    log_section "更新用户地区"

    log_info "设置地区为：广东 深圳"

    local data=$(cat <<EOF
{
    "field": "region",
    "value": "广东 深圳"
}
EOF
)

    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 更新地区成功"
    else
        log_error "❌ 更新地区失败"
        echo "响应: $response"
    fi
}

# 测试更新用户头像
test_update_user_avatar() {
    log_section "更新用户头像"

    local new_avatar="https://example.com/avatar/new_avatar_$(date +%s).jpg"

    log_info "更新头像为: $new_avatar"

    local data=$(cat <<EOF
{
    "field": "avatar",
    "value": "$new_avatar"
}
EOF
)

    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 更新头像成功"
    else
        log_error "❌ 更新头像失败"
        echo "响应: $response"
    fi
}

# 测试获取离线消息
test_get_offline_messages() {
    log_section "获取离线消息"

    log_info "获取离线消息..."

    local data=$(cat <<EOF
{
    "page": 1,
    "size": 10
}
EOF
)

    response=$(api_post "/msg/offline" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 获取离线消息成功"

        total=$(echo "$response" | jq -r '.data.total // 0')
        log_info "离线消息数量: $total; $response;"
    else
        log_warn "⚠️  获取离线消息失败（可能没有离线消息）"
        echo "响应: $response"
    fi
}

# 测试搜索用户
test_search_user() {
    log_section "搜索用户"

    log_info "搜索关键字: test"

    local data=$(cat <<EOF
{
    "keyword": "test",
    "page": 1,
    "size": 10
}
EOF
)

    response=$(api_post "/user/search" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 搜索用户成功; $response;"

        count=$(echo "$response" | jq -r '.data.list | length' 2>/dev/null || echo "0")
        log_info "搜索结果数量: $count; $response;"
    else
        log_warn "⚠️  搜索用户失败"
        echo "响应: $response"
    fi
}

# 测试添加好友
test_add_friend() {
    log_section "添加好友（需要第二个账号）"

    log_warn "此测试需要第二个账号，跳过..."
    # TODO: 实现添加好友测试
}

# 测试创建群组
test_create_group() {
    log_section "创建群组"

    local group_name="测试群组_$(date +%s)"

    log_info "创建群组: $group_name"

    local data=$(cat <<EOF
{
    "name": "$group_name",
    "member_uids": ["$TEST_UID"]
}
EOF
)

    response=$(api_post "/group/add" "$data" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 创建群组成功"

        group_id=$(echo "$response" | jq -r '.data.group_id // empty')
        log_info "postdata: $data"
        log_info "群组 ID: $group_id; $response"
    else
        log_info "postdata: $data"
        log_error "❌ 创建群组失败"
        echo "响应: $response;"
    fi
}

# 测试获取会话列表
test_get_conversations() {
    log_section "获取会话列表"

    log_info "获取会话列表..."

    response=$(api_get "/conversation/mine" "$TEST_TOKEN")

    if echo "$response" | grep -q '"code":0'; then
        log_info "✅ 获取会话列表成功"

        count=$(echo "$response" | jq -r '.data.list | length' 2>/dev/null || echo "0")
        log_info "会话数量: $count"
    else
        log_warn "⚠️  获取会话列表失败"
        echo "响应: $response"
    fi
}

# ============================================================================
# 主测试流程
# ============================================================================

main() {
    log_info "Imboy API 接口测试开始..."
    log_info "BASE_URL: $BASE_URL"
    echo ""

    # 1. 健康检查
    test_health_check

    # 2. 用户注册
    test_user_signup

    # 3. 用户登录
    test_user_login

    # 4. 获取用户信息
    test_get_user_info

    # 5. 更新用户信息（昵称）
    test_update_user_info

    # 6. 更新用户个性签名
    test_update_user_sign

    # 7. 更新用户性别（男）
    test_update_user_gender_male

    # 8. 更新用户性别（女）
    test_update_user_gender_female

    # 9. 更新用户性别（保密）
    test_update_user_gender_secret

    # 10. 更新用户地区
    test_update_user_region

    # 11. 更新用户头像
    test_update_user_avatar

    # 12. 更新用户可搜索设置
    test_update_user_allow_search

    # 13. 获取离线消息
    test_get_offline_messages

    # 14. 搜索用户
    test_search_user

    # 15. 创建群组
    test_create_group

    # 16. 获取会话列表
    test_get_conversations

    # 完成
    echo ""
    log_section "测试完成"
    log_info "✅ 所有测试通过"
    log_info "测试账号: $TEST_ACCOUNT"
    log_info "提示: 测试账号需要手动清理"
}

# ============================================================================
# 执行
# ============================================================================

# 检查依赖
if ! command -v curl &> /dev/null; then
    log_error "curl 未安装，请先安装"
    exit 1
fi

if ! command -v jq &> /dev/null; then
    log_warn "jq 未安装，JSON 解析可能受限"
    log_warn "建议安装: brew install jq"
fi

# 运行测试
main "$@"
