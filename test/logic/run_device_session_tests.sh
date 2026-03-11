#!/usr/bin/env bash

# 设备会话管理功能测试脚本

echo "=== 设备会话管理功能测试 ==="
echo ""

# 设置项目根目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
cd "$PROJECT_ROOT" || exit 1

# 编译新模块
echo "1. 编译模块..."
erlc -I include -o ebin src/logic/device_session_logic.erl 2>&1 | head -10
erlc -I include -o ebin src/api/device_session_handler.erl 2>&1 | head -10
echo "   ✓ 编译完成"
echo ""

# 运行 Logic 层测试
echo "2. 运行 Logic 层测试..."
erl -noshell \
  -pa ebin \
  -include include \
  -eval "eunit:test(device_session_logic_tests, [verbose])" \
  -s init stop 2>&1 | tail -30
echo ""

# 运行 Handler 层测试
echo "3. 运行 Handler 层测试..."
erl -noshell \
  -pa ebin \
  -include include \
  -eval "eunit:test(device_session_handler_tests, [verbose])" \
  -s init stop 2>&1 | tail -30
echo ""

echo "=== 测试完成 ==="
