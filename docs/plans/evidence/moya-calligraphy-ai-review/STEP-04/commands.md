# STEP-04 契约校验命令记录（API-01 证据）

> 环境：macOS darwin 25.6.0 arm64；工作目录
> `imboy/docs/plans/evidence/moya-calligraphy-ai-review/STEP-04/`
> 基线：imboy=5b7e2055（main，无业务文件改动）moya=9644a2e（未触碰）

## 1. redocly lint（主校验器，npx 自动下载，网络可用）

```
$ npx -y @redocly/cli@latest lint openapi/moya-teaching.yaml \
    --skip-rule=security-defined --skip-rule=no-empty-servers
```

- 首轮：YAML 解析失败（2 处未加引号 description 含 `: `，已修复）；随后 11 errors（`nullable-type-sibling`，已通过 `NullableTsidString` + `type/nullable/allOf` 规范写法修复）、若干 example/required 告警（已修复）。
- **最终轮（文本格式）：`openapi/moya-teaching.yaml: validated in 40ms`，退出码 0，errors=0**。
- 最终轮（JSON 格式汇总）：

```
$ npx -y @redocly/cli@latest lint openapi/moya-teaching.yaml \
    --skip-rule=security-defined --skip-rule=no-empty-servers --format=json
EXIT_CODE=0
totals: {'errors': 0, 'warnings': 38, 'ignored': 0}
warnings by rule: no-illogical-composition-keywords=36, info-license=1, operation-4xx-response=1
```

跳过的 2 条规则说明：`security-defined`（login 端点 security: [] 属设计——免 Bearer 的一次性 code 换 token）；`no-empty-servers`（servers 用占位 baseUrl 变量，生产域名属 Step 18 人工确认点，不由契约擅自指定）。

38 个 warnings 全部为已知可接受样式项：
- 36 × `no-illogical-composition-keywords`：单元素 `allOf: [$ref]` 是 OpenAPI 3.0 中给 `$ref` 附加 pattern/description 的唯一合法手段（3.0 不允许 $ref siblings），有意保留；
- 1 × `info-license`：内部冻结契约，无 license 元信息需求；
- 1 × `operation-4xx-response`：教学端点业务错误按 imboy 惯例走 HTTP 200 + envelope code（elib_response），认证 401 已显式定义。

## 2. 结构性校验脚本（TSID/幂等/家长视图隔离，API-01 专项）

```
$ python3 validate_contract.py
contract validation: 37 checks, 0 failures
  [PASS] C1 OpenAPI YAML parses ...
  [PASS] C2 ...（12 个端点逐一：operationId ✓ responses ✓）
  [PASS] C2 endpoint operation count == 12 (got 12)
  [PASS] C3 all TSID-typed fields are string-typed (violations: none)
  [PASS] C3 example ID values are all JSON strings (violations: none)
  [PASS] C4 createSubmission requires Idempotency-Key header
  [PASS] C5 SubmissionParentView (guardian payload) has no ai_draft key
  [PASS] C5 PublishedReview has no ai_draft
  [PASS] C6 5 JSON Schema files present (got 5)
  [PASS] C6 <每个 schema 文件>: valid schema, examples pass   ×5
ALL PASS
EXIT_CODE=0
```

依赖：python3 + PyYAML + jsonschema 4.25.1（本机均已安装；未新增任何依赖安装）。

## 3. 校验深度声明

- redocly 提供 OpenAPI 3.0.3 规范级校验（结构、$ref 解析、schema/example 一致性）。
- validate_contract.py 提供 STEP-04 专项断言：12 端点全覆盖、TSID 字段与 example 值零 integer、幂等头强制、家长 payload 结构性无 AI 草稿键、5 个 JSON Schema 自校验 + examples 自通过。
- 未做：服务端实现级契约测试（属 Step 17）；moya 前端类型生成验证（属 Step 13）。

## 4. 修复历史（同一命令多轮）

| 轮次 | 结果 | 动作 |
|---|---|---|
| 1 | 解析失败（605:76, 729:56） | 2 处 description 加引号（含 `Authorization: Bearer` 冒号） |
| 2 | 11 errors + 47 warnings | nullable/type 兄弟修复（NullableTsidString 等） |
| 3 | 0 errors + 50 warnings | 修 PublishedReview/ReviewWorkbench required、login oneOf、publish example 字段名/时间格式、补 tag descriptions |
| 4（最终） | **0 errors + 38 warnings，EXIT 0** | 无（见上） |
