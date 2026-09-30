# Internal 资源 ID 校验验收（2026-10-01）

基线：`664eb5d1f5621857be5a022788f5c0c242c79371`。本地候选通过；完整交付仍在进行，未部署。

## 变更与边界

- 复用 `elib_tsid:from_binary/1`；群、工作区、项目、频道路径 ID 和项目／频道列表 workspace_id 必须为正数 int64。非法值返回 400，认证失败优先 401。
- 删除四处重复解析。Webhook delivery_id 按既有 string 契约原样传递，包括前导零和非数字值。
- 三个只读 handler 直接调用仍校验范围，避免越界 ID 进入 SQL。

## 验证

- 编译实际候选的四个 API 模块、TSID helper 和两个测试模块至 `/tmp/gz-internal-id-beams`，只读复用主仓 ebin 和 deps/*/ebin；未修改主仓构建产物。
- 新 EUnit 五项通过：四类路径的九种非法值、合法上下界和前导零、字符串投递 ID、认证优先级、两类查询参数、三个 handler 直接调用。认证、请求和数据库使用 meck；不是数据库业务验收。
- 现有 wiring 模块选取六项：route_table_test_、manifest_v21_entries_test_、http_wiring_test_、boundary_parity_test_、dispatch_compiles_test_、normalize_code_test_；全部通过。http_wiring 使用真实 Cowboy 认证拒绝分支。
- 合计 11 项，输出 `/tmp/gz-internal-id-tests.log`。
- `python3 scripts/check_enterprise_release_manifest.py`：12/12，通过 32 端点、26 路径清单一致性检查。
- 首次运行完整 wiring 模块因 intbe02_http_support 缺少编译产物而 setup undef。随后明确运行上述六项；数据库 conformance_test_ 未在本轮运行，不能称整套通过。
- 手工审查修正 Webhook 字符串 ID 与数字 ID 的区别；未运行独立审查代理。

## 复现核心命令

编译后，在包含候选 beam、主仓 ebin 和依赖 ebin 的 code path 下执行：

```erlang
Tests = [enterprise_internal_id_validation_tests |
    [enterprise_internal_wiring_http_tests:F() || F <-
        [route_table_test_, manifest_v21_entries_test_, http_wiring_test_,
         boundary_parity_test_, dispatch_compiles_test_, normalize_code_test_]]],
case eunit:test(Tests, [verbose]) of ok -> halt(0); _ -> halt(1) end.
```

English summary: positive int64 resource IDs use the existing TSID parser; opaque webhook delivery IDs remain unchanged. Eleven focused tests and twelve manifest checks passed. Full database conformance and production readiness are not claimed.
