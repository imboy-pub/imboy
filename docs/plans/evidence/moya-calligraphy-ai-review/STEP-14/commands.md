# STEP-14 命令证据 — 家长端作业提交与成长记录

> Agent D（MOYA），2026-09-09。对 STEP-04 契约 + STEP-03 error-copy.md 编程；后端未实现属预期，全部 mock 驱动（真实联调 Step 17）。
> 基线：moya HEAD = `9644a2e`（无 git 写操作）。

## 质量门（最终态）

| 命令 | 退出码 |
|---|---|
| `npm run check`（typecheck+lint+test+scan 串联） | 0 |
| `npm run typecheck` | 0 |
| `npm run lint`（含全部新增家长域代码） | 0 |
| `npm run test`（77 tests / 25+ suites / 77 pass / 0 fail） | 0 |
| `npm run scan` | 0 |
| `npm run build`（新增 3 页面 + core 5 模块全部入 dist） | 0 |

依赖清单不变：`dependencies: {}`（无状态库/UI 库/跨端框架）。

## dist 冒烟（node require CJS）

```text
new SubmitFlow(...).begin("9223372036854775807", "562949953421312") → phase=idle（极值 TSID 无损）
parentErrorCopy(ApiError(business, 5422)) → "您未监护该孩子，无法查看"
dist/pages/parent/{home,assignment-detail,submit,submission-detail,growth,profile,core}/ 全部产物就绪
```

## 过程中的真实失败与修复

1. **真 bug（测试抓出）**：`uploadAll` 把"用户取消"与"上传失败"都置 `uploadFailed`——取消测试断言 videoReady 失败暴露；修为 cancelled → videoReady 保留选择，失败 → uploadFailed 保留待提交态。
2. Node type-stripping 不支持 constructor 参数属性语法 → SubmitFlow 改显式字段赋值。
3. 测试 import 路径深度写错（tests 根应为 `../src`）→ 批量修正。
4. branded Tsid 常量在测试中需 `assertTsid` 构造（string 不能直接赋 branded）→ 统一修正。
5. `as const` 的 chooseMedia 常量与 wx API mutable 数组类型不兼容 → 改显式 `WechatMinipromm.ChooseMediaOption` 类型标注。
6. 双击防重测试初版断言 Promise 引用相等——async 函数返回必被包装，引用必不等 → 改行为级断言（同结果 + 只发一次 POST）。
7. 页面可空 Tsid 传参（assignmentId|submissionId）→ 局部非空收窄。
