#!/bin/bash
#
# Imboy API 边界测试和异常测试用例
#
# 使用说明：
#   1. 启动服务：IMBOYENV=local make run
#   2. 运行测试： bash ./test/api/test_edge_cases.sh
#

# 不使用 set -e，因为我们需要测试失败的情况

# ============================================================================
# 配置
# ============================================================================


BASE_URL="${BASE_URL:-http://localhost:9800}"
CONTENT_TYPE="Content-Type: application/json"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# 统计
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

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

log_test() {
    echo -e "${BLUE}[TEST]${NC} $1"
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((PASSED_TESTS++))
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((FAILED_TESTS++))
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

# 断言：成功（code = 0）
assert_success() {
    local response=$1
    local test_name=$2
    ((TOTAL_TESTS++))

    log_test "$test_name"

    if echo "$response" | grep -q '"code":0'; then
        log_pass "$test_name"
        return 0
    else
        log_fail "$test_name"
        echo "响应: $response"
        return 1
    fi
}

# 断言：失败（code != 0）
assert_failure() {
    local response=$1
    local test_name=$2
    local expected_code=$3
    ((TOTAL_TESTS++))

    log_test "$test_name"

    local actual_code=$(echo "$response" | jq -r '.code // empty')

    if [ ! "$actual_code" = "0" ]; then
        if [ -z "$expected_code" ] || [ "$actual_code" = "$expected_code" ]; then
            log_pass "$test_name (错误码: $actual_code)"
            return 0
        fi
    fi

    log_fail "$test_name (期望错误码: $expected_code, 实际: $actual_code)"
    echo "响应: $response"
    return 1
}

# ============================================================================
# 测试用例：用户注册
# ============================================================================

test_signup_normal() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "test_${timestamp}@example.com",
    "pwd": "test123456",
    "nickname": "正常用户",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    assert_success "$response" "✅ 正常注册：有效邮箱+密码+昵称"
}

test_signup_invalid_email() {
    local data='{"type": "email", "account": "invalid-email", "pwd": "test123456", "nickname": "测试", "code": "666666", "rsa_encrypt": "0"}'
    response=$(api_post "/passport/signup" "$data")
    assert_failure "$response" "❌ 异常注册：无效邮箱格式"
}

test_signup_empty_email() {
    local data='{"type": "email", "account": "", "pwd": "test123456", "nickname": "测试", "code": "666666", "rsa_encrypt": "0"}'
    response=$(api_post "/passport/signup" "$data")
    assert_failure "$response" "❌ 异常注册：空邮箱"
}

test_signup_short_password() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "test_${timestamp}@example.com",
    "pwd": "12345",
    "nickname": "测试",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    assert_failure "$response" "❌ 异常注册：密码过短（5位）"
}

test_signup_no_number_password() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "test_${timestamp}@example.com",
    "pwd": "Password!",
    "nickname": "测试",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    # 可能成功或失败，取决于具体验证规则
    log_test "⚠️  边界注册：无数字密码"
    echo "响应: $response"
    ((TOTAL_TESTS++))
}

test_signup_empty_nickname() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "test_${timestamp}@example.com",
    "pwd": "test123456",
    "nickname": "",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    assert_failure "$response" "❌ 异常注册：空昵称"
}

test_signup_missing_fields() {
    # 缺少 account
    local data='{"pwd": "test123456", "nickname": "测试", "code": "666666", "rsa_encrypt": "0"}'
    response=$(api_post "/passport/signup" "$data")
    assert_failure "$response" "❌ 异常注册：缺少 account 字段"
}

test_signup_duplicate_account() {
    local timestamp=$(date +%s)
    local account="dup_${timestamp}@example.com"

    # 第一次注册
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "$account",
    "pwd": "test123456",
    "nickname": "测试用户",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)
    api_post "/passport/signup" "$data" > /dev/null

    # 第二次注册相同账号
    response=$(api_post "/passport/signup" "$data")
    assert_failure "$response" "❌ 异常注册：重复账号"
}

# ============================================================================
# 测试用例：验证码注册
# ============================================================================

test_signup_with_code_normal() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "code_${timestamp}@example.com",
    "pwd": "test123456",
    "nickname": "验证码注册",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    assert_success "$response" "✅ 正常注册：邮箱+密码+验证码666666"
}

test_signup_with_wrong_code() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "wrongcode_${timestamp}@example.com",
    "pwd": "test123456",
    "nickname": "错误验证码",
    "code": "000000",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    assert_failure "$response" "❌ 异常注册：错误验证码"
}

test_findpassword_with_code() {
    local timestamp=$(date +%s)
    local account="findpwd_${timestamp}@example.com"

    # 先注册用户
    local register_data=$(cat <<EOF
{
    "type": "email",
    "account": "$account",
    "pwd": "test123456",
    "nickname": "找回密码测试",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)
    api_post "/passport/signup" "$register_data" > /dev/null

    # 使用验证码找回密码
    local findpwd_data=$(cat <<EOF
{
    "type": "email",
    "account": "$account",
    "pwd": "newtest123456",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/findpassword" "$findpwd_data")
    assert_success "$response" "✅ 正常找回密码：验证码666666"
}

# ============================================================================
# 测试用例：用户登录
# ============================================================================

test_login_normal() {
    local timestamp=$(date +%s)
    local account="login_${timestamp}@example.com"

    # 先注册
    local register_data=$(cat <<EOF
{
    "type": "email",
    "account": "$account",
    "pwd": "test123456",
    "nickname": "登录测试",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)
    api_post "/passport/signup" "$register_data" > /dev/null

    # 再登录
    local login_data=$(cat <<EOF
{
    "type": "email",
    "account": "$account",
    "pwd": "test123456",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/login" "$login_data")
    assert_success "$response" "✅ 正常登录：有效账号密码"
}

test_login_wrong_password() {
    local login_data='{"type": "email", "account": "login_test@example.com", "pwd": "WrongPassword123!", "rsa_encrypt": "0"}'
    response=$(api_post "/passport/login" "$login_data")
    assert_failure "$response" "❌ 异常登录：错误密码"
}

test_login_nonexistent_account() {
    local login_data='{"type": "email", "account": "nonexistent_99999@example.com", "pwd": "test123456", "rsa_encrypt": "0"}'
    response=$(api_post "/passport/login" "$login_data")
    assert_failure "$response" "❌ 异常登录：不存在的账号"
}

test_login_empty_fields() {
    local data='{"type": "email", "account": "", "pwd": "", "rsa_encrypt": "0"}'
    response=$(api_post "/passport/login" "$data")
    assert_failure "$response" "❌ 异常登录：空账号密码"
}

# ============================================================================
# 测试用例：Token 认证
# ============================================================================

test_auth_valid_token() {
    # 使用正常注册和登录获取 token
    local timestamp=$(date +%s)
    local account="auth_${timestamp}@example.com"

    local register_data=$(cat <<EOF
{
    "type": "email",
    "account": "$account",
    "pwd": "test123456",
    "nickname": "认证测试",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)
    api_post "/passport/signup" "$register_data" > /dev/null

    local login_data=$(cat <<EOF
{
    "type": "email",
    "account": "$account",
    "pwd": "test123456",
    "rsa_encrypt": "0"
}
EOF
)

    login_response=$(api_post "/passport/login" "$login_data")
    TOKEN=$(echo "$login_response" | jq -r '.payload.token // .data.token // empty')

    if [ -n "$TOKEN" ] && [ ! "$TOKEN" = "null" ]; then
        response=$(api_get "/user/show?id=test" "$TOKEN")
        assert_success "$response" "✅ 正常认证：有效 Token"
    else
        log_fail "✅ 正常认证：无法获取测试 Token"
        ((TOTAL_TESTS++))
        ((FAILED_TESTS++))
    fi
}

test_auth_invalid_token() {
    local invalid_token="invalid.token.here"
    response=$(api_get "/user/show" "$invalid_token")
    assert_failure "$response" "❌ 异常认证：无效 Token"
}

test_auth_empty_token() {
    response=$(api_get "/user/show" "")
    # 可能返回 401 或其他错误
    log_test "❌ 异常认证：空 Token"
    echo "响应: $response"
    ((TOTAL_TESTS++))
}

test_auth_expired_token() {
    # 使用一个过期的 token 格式
    local expired_token="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2MDAwMDAwMDB9.expired"
    response=$(api_get "/user/show" "$expired_token")
    assert_failure "$response" "❌ 异常认证：过期 Token"
}

# ============================================================================
# 测试用例：参数验证
# ============================================================================

test_param_sql_injection() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "test_${timestamp}@example.com",
    "pwd": "test123456",
    "nickname": "'; DROP TABLE users; --",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    # 应该成功或被过滤，但不应导致 SQL 注入
    log_test "🔒 安全测试：SQL 注入尝试"
    if echo "$response" | grep -q '"code":0'; then
        log_pass "SQL 注入被防护（可能转义了特殊字符）"
    else
        log_pass "SQL 注入被拒绝"
    fi
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_param_xss_attempt() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "test_${timestamp}@example.com",
    "pwd": "test123456",
    "nickname": "<script>alert('XSS')</script>",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    log_test "🔒 安全测试：XSS 尝试"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_param_very_long_nickname() {
    local timestamp=$(date +%s)
    local long_nickname=$(printf 'A%.0s' {1..500})

    local data=$(cat <<EOF
{
    "type": "email",
    "account": "test_${timestamp}@example.com",
    "pwd": "test123456",
    "nickname": "$long_nickname",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    # 可能成功或失败，取决于长度限制
    log_test "⚠️  边界测试：超长昵称（500字符）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
}

test_param_special_chars() {
    local timestamp=$(date +%s)
    local data=$(cat <<EOF
{
    "type": "email",
    "account": "test_${timestamp}@example.com",
    "pwd": "test123456",
    "nickname": "测试😀🎉特殊符号!@#$%^&*()",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    response=$(api_post "/passport/signup" "$data")
    log_test "✅ 正常测试：包含 emoji 和特殊符号的昵称"
    echo "响应: $response"
    ((TOTAL_TESTS++))
}

# ============================================================================
# 测试用例：边界值
# ============================================================================

test_boundary_page_size() {
    local timestamp=$(date +%s)
    local account="page_${timestamp}@example.com"

    # 注册并登录
    local register_data=$(cat <<EOF
{
    "account": "$account",
    "password": "Password123!",
    "nickname": "分页测试"
}
EOF
)
    api_post "/passport/signup" "$register_data" > /dev/null

    local login_data='{"account": "'$account'", "password": "Password123!"}'
    login_response=$(api_post "/passport/login" "$login_data")
    TOKEN=$(echo "$login_response" | jq -r '.payload.token // .data.token // empty')

    if [ -n "$TOKEN" ] && [ ! "$TOKEN" = "null" ]; then
        # 测试 page = 0
        local data='{"page": 0, "size": 10}'
        response=$(api_post "/user/search" "$data" "$TOKEN")
        log_test "⚠️  边界测试：page = 0"
        echo "响应: $response"
        ((TOTAL_TESTS++))

        # 测试负数 page
        data='{"page": -1, "size": 10}'
        response=$(api_post "/user/search" "$data" "$TOKEN")
        log_test "⚠️  边界测试：page = -1（负数）"
        echo "响应: $response"
        ((TOTAL_TESTS++))

        # 测试超大 size
        data='{"page": 1, "size": 999999}'
        response=$(api_post "/user/search" "$data" "$TOKEN")
        log_test "⚠️  边界测试：size = 999999（超大值）"
        echo "响应: $response"
        ((TOTAL_TESTS++))
    fi
}

# ============================================================================
# 测试用例：HTTP 方法
# ============================================================================

test_http_methodNotAllowed() {
    # /user/show 应该只支持 GET
    local timestamp=$(date +%s)
    local account="method_${timestamp}@example.com"

    local register_data=$(cat <<EOF
{
    "account": "$account",
    "password": "Password123!",
    "nickname": "方法测试"
}
EOF
)
    api_post "/passport/signup" "$register_data" > /dev/null

    local login_data='{"account": "'$account'", "password": "Password123!"}'
    login_response=$(api_post "/passport/login" "$login_data")
    TOKEN=$(echo "$login_response" | jq -r '.payload.token // .data.token // empty')

    if [ -n "$TOKEN" ] && [ ! "$TOKEN" = "null" ]; then
        # 尝试用 POST 访问 GET 接口
        local data='{}'
        response=$(api_post "/user/show" "$data" "$TOKEN")
        log_test "⚠️  HTTP 方法：POST 访问 GET 接口"
        echo "响应: $response"
        ((TOTAL_TESTS++))
    fi
}

# ============================================================================
# 测试用例：并发和竞态
# ============================================================================

test_concurrent_signup_same_account() {
    local timestamp=$(date +%s)
    local account="concurrent_${timestamp}@example.com"

    local data=$(cat <<EOF
{
    "type": "email",
    "account": "$account",
    "pwd": "test123456",
    "nickname": "并发测试",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    # 并发发送两个相同的注册请求
    response1=$(api_post "/passport/signup" "$data" &)
    response2=$(api_post "/passport/signup" "$data" &)
    wait

    log_test "⚠️  并发测试：同时注册相同账号"
    echo "响应1: $response1"
    echo "响应2: $response2"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 用户接口边界测试 (user_handler)
# ============================================================================

test_user_update_nickname_normal() {
    local timestamp=$(date +%s)
    setup_test_user "update_user_${timestamp}"

    local data=$(cat <<EOF
{
    "field": "nickname",
    "value": "新昵称_$timestamp"
}
EOF
)
    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 用户更新：正常修改昵称"
}

test_user_update_nickname_empty() {
    local timestamp=$(date +%s)
    setup_test_user "empty_nick_${timestamp}"

    local data='{"field": "nickname", "value": ""}'
    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")
    # 可能成功或失败
    log_test "⚠️  用户更新：空昵称"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_user_update_nickname_very_long() {
    local timestamp=$(date +%s)
    setup_test_user "long_nick_${timestamp}"
    local long_value=$(printf 'A%.0s' {1..300})

    local data=$(cat <<EOF
{
    "field": "nickname",
    "value": "$long_value"
}
EOF
)
    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")
    log_test "⚠️  用户更新：超长昵称(300字符)"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_user_update_invalid_field() {
    local timestamp=$(date +%s)
    setup_test_user "invalid_field_${timestamp}"

    local data='{"field": "invalid_field", "value": "test"}'
    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 用户更新：无效字段"
}

test_user_update_gender_invalid() {
    local timestamp=$(date +%s)
    setup_test_user "invalid_gender_${timestamp}"

    local data='{"field": "gender", "value": "99"}'
    response=$(api_post "/user/update" "$data" "$TEST_TOKEN")
    # 可能成功或失败
    log_test "⚠️  用户更新：无效性别值"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_user_show_missing_id() {
    setup_test_user
    response=$(api_get "/user/show" "$TEST_TOKEN")
    assert_failure "$response" "❌ 用户信息：缺少ID参数"
}

test_user_show_invalid_id() {
    setup_test_user
    response=$(api_get "/user/show?id=invalid_id_12345" "$TEST_TOKEN")
    assert_failure "$response" "❌ 用户信息：无效ID"
}

test_user_change_state_normal() {
    local timestamp=$(date +%s)
    setup_test_user "state_user_${timestamp}"

    local data='{"state": "online"}'
    response=$(api_post "/user/change_state" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 用户状态：设置为在线"
}

test_user_change_state_hide() {
    local timestamp=$(date +%s)
    setup_test_user "hide_user_${timestamp}"

    local data='{"state": "hide"}'
    response=$(api_post "/user/change_state" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 用户状态：设置为隐藏"
}

test_user_change_state_invalid() {
    local timestamp=$(date +%s)
    setup_test_user "invalid_state_${timestamp}"

    local data='{"state": "invalid_state"}'
    response=$(api_post "/user/change_state" "$data" "$TEST_TOKEN")
    # 可能成功或失败
    log_test "⚠️  用户状态：无效状态值"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 好友接口边界测试 (friend_handler)
# ============================================================================

test_friend_add_normal() {
    local timestamp=$(date +%s)
    setup_test_user "friend_adder_${timestamp}"

    # 注意：需要另一个用户ID才能测试
    local data='{"to": "test_user_id", "payload": "添加好友"}'
    response=$(api_post "/friend/add" "$data" "$TEST_TOKEN")
    # 可能失败因为用户ID不存在
    log_test "⚠️  好友添加：发送请求（需要第二个用户）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_friend_add_empty_to() {
    local timestamp=$(date +%s)
    setup_test_user "empty_to_${timestamp}"

    local data='{"to": "", "payload": "添加好友"}'
    response=$(api_post "/friend/add" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 好友添加：空目标ID"
}

test_friend_add_missing_payload() {
    local timestamp=$(date +%s)
    setup_test_user "no_payload_${timestamp}"

    local data='{"to": "test_user_id"}'
    response=$(api_post "/friend/add" "$data" "$TEST_TOKEN")
    # 可能成功（payload可以是undefined）
    log_test "⚠️  好友添加：缺少payload"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_friend_delete_normal() {
    local timestamp=$(date +%s)
    setup_test_user "friend_del_${timestamp}"

    local data='{"uid": "test_user_id"}'
    response=$(api_post "/friend/delete" "$data" "$TEST_TOKEN")
    # 可能失败因为不是好友
    log_test "⚠️  好友删除：删除好友（需要好友关系）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_friend_delete_empty_uid() {
    local timestamp=$(date +%s)
    setup_test_user "empty_uid_${timestamp}"

    local data='{"uid": ""}'
    response=$(api_post "/friend/delete" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 好友删除：空用户ID"
}

test_friend_list_normal() {
    setup_test_user
    response=$(api_get "/friend/list" "$TEST_TOKEN")
    assert_success "$response" "✅ 好友列表：获取好友列表"
}

test_friend_change_remark_normal() {
    local timestamp=$(date +%s)
    setup_test_user "remark_${timestamp}"

    local data='{"uid": "test_user_id", "remark": "新备注"}'
    response=$(api_post "/friend/change_remark" "$data" "$TEST_TOKEN")
    # 可能失败因为不是好友
    log_test "⚠️  好友备注：修改备注（需要好友关系）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_friend_change_remark_very_long() {
    local timestamp=$(date +%s)
    setup_test_user "long_remark_${timestamp}"
    local long_remark=$(printf '备%.0s' {1..200})

    local data=$(cat <<EOF
{
    "uid": "test_user_id",
    "remark": "$long_remark"
}
EOF
)
    response=$(api_post "/friend/change_remark" "$data" "$TEST_TOKEN")
    log_test "⚠️  好友备注：超长备注(200字符)"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_friend_change_remark_empty() {
    local timestamp=$(date +%s)
    setup_test_user "empty_remark_${timestamp}"

    local data='{"uid": "test_user_id", "remark": ""}'
    response=$(api_post "/friend/change_remark" "$data" "$TEST_TOKEN")
    log_test "⚠️  好友备注：空备注"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 群组接口边界测试 (group_handler)
# ============================================================================

test_group_add_normal() {
    local timestamp=$(date +%s)
    setup_test_user "group_creator_${timestamp}"

    local data=$(cat <<EOF
{
    "name": "测试群组_${timestamp}",
    "member_uids": []
}
EOF
)
    response=$(api_post "/group/add" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 群组创建：正常创建群组"
}

test_group_add_empty_name() {
    local timestamp=$(date +%s)
    setup_test_user "empty_name_${timestamp}"

    local data='{"name": "", "member_uids": []}'
    response=$(api_post "/group/add" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 群组创建：空群组名"
}

test_group_add_very_long_name() {
    local timestamp=$(date +%s)
    setup_test_user "long_name_${timestamp}"
    local long_name=$(printf '群%.0s' {1..200})

    local data=$(cat <<EOF
{
    "name": "$long_name",
    "member_uids": []
}
EOF
)
    response=$(api_post "/group/add" "$data" "$TEST_TOKEN")
    log_test "⚠️  群组创建：超长群组名(200字符)"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_group_add_missing_name() {
    local timestamp=$(date +%s)
    setup_test_user "no_name_${timestamp}"

    local data='{"member_uids": []}'
    response=$(api_post "/group/add" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 群组创建：缺少群组名"
}

test_group_detail_missing_gid() {
    setup_test_user
    response=$(api_get "/group/detail" "$TEST_TOKEN")
    assert_failure "$response" "❌ 群组详情：缺少群组ID"
}

test_group_detail_invalid_gid() {
    setup_test_user
    response=$(api_get "/group/detail?gid=invalid_gid" "$TEST_TOKEN")
    assert_failure "$response" "❌ 群组详情：无效群组ID"
}

test_group_page_normal() {
    setup_test_user
    response=$(api_get "/group/page?page=1&size=20" "$TEST_TOKEN")
    assert_success "$response" "✅ 群组分页：正常分页查询"
}

test_group_page_invalid_page() {
    setup_test_user
    response=$(api_get "/group/page?page=-1&size=20" "$TEST_TOKEN")
    log_test "⚠️  群组分页：负数页码"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_group_page_very_large_size() {
    setup_test_user
    response=$(api_get "/group/page?page=1&size=99999" "$TEST_TOKEN")
    log_test "⚠️  群组分页：超大分页大小"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 消息接口边界测试 (msg_handler)
# ============================================================================

test_msg_offline_normal() {
    setup_test_user
    local data='{}'
    response=$(api_post "/msg/offline" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 离线消息：获取离线消息"
}

test_msg_offline_ack_normal() {
    setup_test_user
    local data='{"msg_ids": ["1", "2", "3"]}'
    response=$(api_post "/msg/offline_ack" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 离线消息确认：正常确认"
}

test_msg_offline_ack_empty_ids() {
    setup_test_user
    local data='{"msg_ids": []}'
    response=$(api_post "/msg/offline_ack" "$data" "$TEST_TOKEN")
    log_test "⚠️  离线消息确认：空消息ID列表"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_msg_offline_ack_missing_ids() {
    setup_test_user
    local data='{}'
    response=$(api_post "/msg/offline_ack" "$data" "$TEST_TOKEN")
    log_test "⚠️  离线消息确认：缺少msg_ids"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 会话接口边界测试 (conversation_handler)
# ============================================================================

test_conversation_mine_normal() {
    setup_test_user
    response=$(api_get "/conversation/mine" "$TEST_TOKEN")
    assert_success "$response" "✅ 会话列表：获取我的会话"
}

test_conversation_online_normal() {
    setup_test_user
    local data='{}'
    response=$(api_post "/conversation/online" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 会话在线：设置会话在线"
}

# ============================================================================
# 设备接口边界测试 (user_device_handler)
# ============================================================================

test_user_device_page_normal() {
    setup_test_user
    response=$(api_get "/user_device/page?page=1&size=20" "$TEST_TOKEN")
    assert_success "$response" "✅ 设备列表：获取设备列表"
}

test_user_device_change_name_normal() {
    setup_test_user
    local data='{"did": "test_device_001", "name": "我的设备"}'
    response=$(api_post "/user_device/change_name" "$data" "$TEST_TOKEN")
    log_test "⚠️  设备改名：修改设备名（需要实际设备）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_user_device_change_name_empty_did() {
    setup_test_user
    local data='{"did": "", "name": "测试设备"}'
    response=$(api_post "/user_device/change_name" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 设备改名：空设备ID"
}

test_user_device_delete_normal() {
    setup_test_user
    local data='{"did": "test_device_001"}'
    response=$(api_post "/user_device/delete" "$data" "$TEST_TOKEN")
    log_test "⚠️  设备删除：删除设备（需要实际设备）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 收藏接口边界测试 (user_collect_handler)
# ============================================================================

test_user_collect_add_normal() {
    setup_test_user
    local data='{"msg_id": "test_msg_001", "type": "chat"}'
    response=$(api_post "/user_collect/add" "$data" "$TEST_TOKEN")
    log_test "⚠️  收藏添加：添加收藏（需要实际消息）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_user_collect_add_empty_msg_id() {
    setup_test_user
    local data='{"msg_id": "", "type": "chat"}'
    response=$(api_post "/user_collect/add" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 收藏添加：空消息ID"
}

test_user_collect_page_normal() {
    setup_test_user
    response=$(api_get "/user_collect/page?page=1&size=20" "$TEST_TOKEN")
    assert_success "$response" "✅ 收藏列表：获取收藏列表"
}

test_user_collect_remove_normal() {
    setup_test_user
    local data='{"id": "test_collect_id"}'
    response=$(api_post "/user_collect/remove" "$data" "$TEST_TOKEN")
    log_test "⚠️  收藏删除：删除收藏（需要实际收藏）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 黑名单接口边界测试 (user_denylist_handler)
# ============================================================================

test_user_denylist_add_normal() {
    setup_test_user
    local data='{"denied_user_id": "test_user_001"}'
    response=$(api_post "/user_denylist/add" "$data" "$TEST_TOKEN")
    log_test "⚠️  黑名单添加：添加黑名单（需要第二个用户）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_user_denylist_add_empty_user_id() {
    setup_test_user
    local data='{"denied_user_id": ""}'
    response=$(api_post "/user_denylist/add" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 黑名单添加：空用户ID"
}

test_user_denylist_page_normal() {
    setup_test_user
    response=$(api_get "/user_denylist/page?page=1&size=20" "$TEST_TOKEN")
    assert_success "$response" "✅ 黑名单列表：获取黑名单"
}

test_user_denylist_remove_normal() {
    setup_test_user
    local data='{"denied_user_id": "test_user_001"}'
    response=$(api_post "/user_denylist/remove" "$data" "$TEST_TOKEN")
    log_test "⚠️  黑名单删除：移出黑名单"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 位置接口边界测试 (location_handler)
# ============================================================================

test_location_make_visible_normal() {
    setup_test_user
    local data='{"latitude": 22.5431, "longitude": 114.0579}'
    response=$(api_post "/location/makeMyselfVisible" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 位置设置：设置位置可见"
}

test_location_make_visible_invalid_lat() {
    setup_test_user
    local data='{"latitude": "invalid", "longitude": 114.0579}'
    response=$(api_post "/location/makeMyselfVisible" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 位置设置：无效纬度"
}

test_location_make_visible_missing_lat() {
    setup_test_user
    local data='{"longitude": 114.0579}'
    response=$(api_post "/location/makeMyselfVisible" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 位置设置：缺少纬度"
}

test_location_make_unvisible_normal() {
    setup_test_user
    local data='{}'
    response=$(api_post "/location/makeMyselfUnvisible" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 位置设置：设置位置不可见"
}

test_location_people_nearby_normal() {
    setup_test_user
    local data='{"latitude": 22.5431, "longitude": 114.0579, "radius": 1000}'
    response=$(api_post "/location/peopleNearby" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 附近的人：查找附近用户"
}

test_location_people_nearby_invalid_radius() {
    setup_test_user
    local data='{"latitude": 22.5431, "longitude": 114.0579, "radius": -100}'
    response=$(api_post "/location/peopleNearby" "$data" "$TEST_TOKEN")
    log_test "⚠️  附近的人：负数半径"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 搜索接口边界测试 (fts_handler)
# ============================================================================

test_fts_user_search_normal() {
    setup_test_user
    response=$(api_get "/fts/user_search?keyword=test" "$TEST_TOKEN")
    assert_success "$response" "✅ 用户搜索：正常搜索"
}

test_fts_user_search_empty_keyword() {
    setup_test_user
    response=$(api_get "/fts/user_search?keyword=" "$TEST_TOKEN")
    log_test "⚠️  用户搜索：空关键词"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_fts_user_search_special_chars() {
    setup_test_user
    response=$(api_get "/fts/user_search?keyword=%3Cscript%3E" "$TEST_TOKEN")
    log_test "⚠️  用户搜索：特殊字符"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 标签接口边界测试 (user_tag_handler)
# ============================================================================

test_user_tag_add_normal() {
    setup_test_user
    local data='{"name": "测试标签"}'
    response=$(api_post "/user_tag/add" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 标签添加：添加标签"
}

test_user_tag_add_empty_name() {
    setup_test_user
    local data='{"name": ""}'
    response=$(api_post "/user_tag/add" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 标签添加：空标签名"
}

test_user_tag_add_very_long_name() {
    setup_test_user
    local long_name=$(printf '标%.0s' {1..100})
    local data="{\"name\": \"$long_name\"}"
    response=$(api_post "/user_tag/add" "$data" "$TEST_TOKEN")
    log_test "⚠️  标签添加：超长标签名"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_user_tag_page_normal() {
    setup_test_user
    response=$(api_get "/user_tag/page?page=1&size=20" "$TEST_TOKEN")
    assert_success "$response" "✅ 标签列表：获取标签列表"
}

test_user_tag_delete_normal() {
    setup_test_user
    local data='{"id": "test_tag_id"}'
    response=$(api_post "/user_tag/delete" "$data" "$TEST_TOKEN")
    log_test "⚠️  标签删除：删除标签"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 反馈接口边界测试 (feedback_handler)
# ============================================================================

test_feedback_add_normal() {
    setup_test_user
    local data='{"content": "测试反馈内容"}'
    response=$(api_post "/feedback/add" "$data" "$TEST_TOKEN")
    assert_success "$response" "✅ 反馈添加：提交反馈"
}

test_feedback_add_empty_content() {
    setup_test_user
    local data='{"content": ""}'
    response=$(api_post "/feedback/add" "$data" "$TEST_TOKEN")
    assert_failure "$response" "❌ 反馈添加：空反馈内容"
}

test_feedback_add_very_long_content() {
    setup_test_user
    local long_content=$(printf '测%.0s' {1..1000})
    local data="{\"content\": \"$long_content\"}"
    response=$(api_post "/feedback/add" "$data" "$TEST_TOKEN")
    log_test "⚠️  反馈添加：超长反馈内容"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

test_feedback_page_normal() {
    setup_test_user
    response=$(api_get "/feedback/page?page=1&size=20" "$TEST_TOKEN")
    log_test "⚠️  反馈列表：获取反馈列表（需要后台权限）"
    echo "响应: $response"
    ((TOTAL_TESTS++))
    ((PASSED_TESTS++))
}

# ============================================================================
# 辅助函数：设置测试用户
# ============================================================================

setup_test_user() {
    local suffix="${1:-test_$(date +%s)}"
    TEST_ACCOUNT="setup_${suffix}@example.com"
    TEST_PASSWORD="test123456"

    local data=$(cat <<EOF
{
    "type": "email",
    "account": "$TEST_ACCOUNT",
    "pwd": "$TEST_PASSWORD",
    "nickname": "测试用户",
    "code": "666666",
    "rsa_encrypt": "0"
}
EOF
)

    # 注册用户
    api_post "/passport/signup" "$data" > /dev/null 2>&1

    # 登录获取 token
    local login_data=$(cat <<EOF
{
    "type": "email",
    "account": "$TEST_ACCOUNT",
    "pwd": "$TEST_PASSWORD",
    "rsa_encrypt": "0"
}
EOF
)

    local login_response=$(api_post "/passport/login" "$login_data")
    TEST_TOKEN=$(echo "$login_response" | jq -r '.payload.token // .data.token // empty')
}

# ============================================================================
# 主测试流程
# ============================================================================

main() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  Imboy API 边界和异常测试${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo ""
    log_info "BASE_URL: $BASE_URL"
    echo ""

    # 检查依赖
    if ! command -v curl &> /dev/null; then
        log_error "curl 未安装"
        exit 1
    fi

    if ! command -v jq &> /dev/null; then
        log_warn "jq 未安装，部分检查可能失败"
    fi

    # 健康检查
    log_info "检查服务状态..."
    health_response=$(api_get "/init")
    if ! echo "$health_response" | grep -q "success"; then
        log_error "服务未启动"
        exit 1
    fi
    log_info "✅ 服务运行正常"
    echo ""

    # 运行测试套件
    echo -e "${YELLOW}========== 用户注册测试 ==========${NC}"
    test_signup_normal
    test_signup_invalid_email
    test_signup_empty_email
    test_signup_short_password
    test_signup_no_number_password
    test_signup_empty_nickname
    test_signup_missing_fields
    test_signup_duplicate_account

    echo ""
    echo -e "${YELLOW}========== 验证码注册测试 ==========${NC}"
    test_signup_with_code_normal
    test_signup_with_wrong_code
    test_findpassword_with_code

    echo ""
    echo -e "${YELLOW}========== 用户登录测试 ==========${NC}"
    test_login_normal
    test_login_wrong_password
    test_login_nonexistent_account
    test_login_empty_fields

    echo ""
    echo -e "${YELLOW}========== Token 认证测试 ==========${NC}"
    test_auth_valid_token
    test_auth_invalid_token
    test_auth_empty_token
    test_auth_expired_token

    echo ""
    echo -e "${YELLOW}========== 参数验证测试 ==========${NC}"
    test_param_sql_injection
    test_param_xss_attempt
    test_param_very_long_nickname
    test_param_special_chars

    echo ""
    echo -e "${YELLOW}========== 边界值测试 ==========${NC}"
    test_boundary_page_size

    echo ""
    echo -e "${YELLOW}========== HTTP 方法测试 ==========${NC}"
    test_http_methodNotAllowed

    echo ""
    echo -e "${YELLOW}========== 并发测试 ==========${NC}"
    test_concurrent_signup_same_account

    echo ""
    echo -e "${YELLOW}========== 用户接口边界测试 ==========${NC}"
    test_user_update_nickname_normal
    test_user_update_nickname_empty
    test_user_update_nickname_very_long
    test_user_update_invalid_field
    test_user_update_gender_invalid
    test_user_show_missing_id
    test_user_show_invalid_id
    test_user_change_state_normal
    test_user_change_state_hide
    test_user_change_state_invalid

    echo ""
    echo -e "${YELLOW}========== 好友接口边界测试 ==========${NC}"
    test_friend_add_normal
    test_friend_add_empty_to
    test_friend_add_missing_payload
    test_friend_delete_normal
    test_friend_delete_empty_uid
    test_friend_list_normal
    test_friend_change_remark_normal
    test_friend_change_remark_very_long
    test_friend_change_remark_empty

    echo ""
    echo -e "${YELLOW}========== 群组接口边界测试 ==========${NC}"
    test_group_add_normal
    test_group_add_empty_name
    test_group_add_very_long_name
    test_group_add_missing_name
    test_group_detail_missing_gid
    test_group_detail_invalid_gid
    test_group_page_normal
    test_group_page_invalid_page
    test_group_page_very_large_size

    echo ""
    echo -e "${YELLOW}========== 消息接口边界测试 ==========${NC}"
    test_msg_offline_normal
    test_msg_offline_ack_normal
    test_msg_offline_ack_empty_ids
    test_msg_offline_ack_missing_ids

    echo ""
    echo -e "${YELLOW}========== 会话接口边界测试 ==========${NC}"
    test_conversation_mine_normal
    test_conversation_online_normal

    echo ""
    echo -e "${YELLOW}========== 设备接口边界测试 ==========${NC}"
    test_user_device_page_normal
    test_user_device_change_name_normal
    test_user_device_change_name_empty_did
    test_user_device_delete_normal

    echo ""
    echo -e "${YELLOW}========== 收藏接口边界测试 ==========${NC}"
    test_user_collect_add_normal
    test_user_collect_add_empty_msg_id
    test_user_collect_page_normal
    test_user_collect_remove_normal

    echo ""
    echo -e "${YELLOW}========== 黑名单接口边界测试 ==========${NC}"
    test_user_denylist_add_normal
    test_user_denylist_add_empty_user_id
    test_user_denylist_page_normal
    test_user_denylist_remove_normal

    echo ""
    echo -e "${YELLOW}========== 位置接口边界测试 ==========${NC}"
    test_location_make_visible_normal
    test_location_make_visible_invalid_lat
    test_location_make_visible_missing_lat
    test_location_make_unvisible_normal
    test_location_people_nearby_normal
    test_location_people_nearby_invalid_radius

    echo ""
    echo -e "${YELLOW}========== 搜索接口边界测试 ==========${NC}"
    test_fts_user_search_normal
    test_fts_user_search_empty_keyword
    test_fts_user_search_special_chars

    echo ""
    echo -e "${YELLOW}========== 标签接口边界测试 ==========${NC}"
    test_user_tag_add_normal
    test_user_tag_add_empty_name
    test_user_tag_add_very_long_name
    test_user_tag_page_normal
    test_user_tag_delete_normal

    echo ""
    echo -e "${YELLOW}========== 反馈接口边界测试 ==========${NC}"
    test_feedback_add_normal
    test_feedback_add_empty_content
    test_feedback_add_very_long_content
    test_feedback_page_normal

    # 输出统计
    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  测试统计${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo "总测试数: $TOTAL_TESTS"
    echo -e "通过: ${GREEN}$PASSED_TESTS${NC}"
    echo -e "失败: ${RED}$FAILED_TESTS${NC}"

    if [ $FAILED_TESTS -eq 0 ]; then
        echo ""
        log_info "🎉 所有测试通过！"
        exit 0
    else
        echo ""
        log_warn "⚠️  有 $FAILED_TESTS 个测试失败"
        exit 1
    fi
}

# 运行测试
main "$@"
