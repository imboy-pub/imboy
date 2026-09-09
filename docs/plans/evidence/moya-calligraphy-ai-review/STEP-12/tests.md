# STEP-12 测试证据 — moya 工程基线

> `npm test` = `node --test "tests/**/*.test.ts"`（Node 26 原生 TS type-stripping，无测试框架依赖）
> utils 为纯函数，无需微信 API mock（微信 API 类型仅用于 typecheck，来自 miniprogram-api-typings）。

## 汇总

```text
ℹ tests 26
ℹ suites 7
ℹ pass 26
ℹ fail 0
ℹ cancelled 0
ℹ skipped 0
```

## 覆盖清单

### tests/tsid.test.ts（TSID 惯例：全程 string，禁止转 number）

- `isTsid`：接受 "0"/"1"/18 位常规值；接受 int64 上界 "9223372036854775807"；
  拒绝上界+1 与 19 位全 9（超 int64）、20 位串、number 类型（`Number("9007199254740993")` 等 2^53 精度丢失证明）、
  前导零、非十进制/正负号/科学计数/首尾空格、null/undefined/对象/数组。
- `assertTsid`：合法值原样返回；非法值抛 TypeError 且 message 含字段名。
- `compareTsid`（纯字符串数值序，无任何数值转换）：等值 0；不同位数长度即数量级（999<1000）；
  同位数字典序即数值序（含 int64 上界邻值）；0 与 1 的序。

### tests/format.test.ts

- `pad2` 补零；`toDate` 接受 ISO/时间戳/Date，并把空格分隔本地时间规整为 iOS JSC 可解析形态；
- `formatDateTime` 输出 `YYYY-MM-DD HH:mm`，年末进位正确；
- `formatRelativeTime`：刚刚 / N 分钟前 / N 小时前 / 昨天 / 更早回退绝对时间 / 未来时间回退绝对时间（不出负相对值）。

## 发现并修复的 bug（测试价值证明）

初版 `TSID_PATTERN = /^(0|[1-9][0-9]{0,17})$/` 最多 18 位，导致 19 位合法 int64 上界
`9223372036854775807` 被拒 —— `assertTsid("9223372036854775807")` 单测失败暴露；修正为 `{0,18}` 后全绿。
