# STEP-13 命令证据 — moya 登录/请求层/身份切换/公共壳

> Agent D（MOYA），2026-09-09。恢复自限流中断：src/core/{env,errors,platform,request,types}.ts 为中断前落盘，本次续写完成。
> 基线：moya HEAD = `9644a2e`（无 git 写操作）；对 STEP-04 冻结契约编程，测试全 mock。

## 质量门（最终态）

| 命令 | 退出码 |
|---|---|
| `npm run check`（typecheck+lint+test+scan 串联） | 0 |
| `npm run typecheck`（tsc --noEmit） | 0 |
| `npm run lint`（biome check src tests scripts） | 0 |
| `npm run test`（node --test，52 tests / 15 suites / 52 pass / 0 fail） | 0 |
| `npm run scan`（secret 扫描，含新增 core/页面/组件代码） | 0 |
| `npm run build`（tsc + 静态复制 → dist/，45 静态文件 + 25 JS） | 0 |

依赖清单不变：`dependencies: {}`（无运行时依赖；devDeps 仍为 4 个，无状态库/UI 库/跨端框架）。

## dist 产物冒烟（node require CJS）

```text
require("./dist/core/nav.js")    → PARENT_TABS 3 项 / TEACHER_TABS 4 项
require("./dist/core/context.js")→ modeForContext(guardian) = "parent"
buildRoute("/p", {id: "9223372036854775807"}) = "/p?id=9223372036854775807"（极值原样）
```

import 重写验证：`dist/core/context.js` 内 `require("./errors.js")`（rewriteRelativeImportExtensions 生效）。

## 微信开发者工具导入

Step 12 已确认本机未安装（`/Applications/wechatwebdevtools.app` 不存在）→ 维持 `BLOCKED_EXTERNAL`。

## 过程中的真实失败与修复

1. 恢复时 request.ts 有 2 处 Biome 错误（import 排序 + 行宽）→ lint:fix 归零；随后删除冗余 `requestWithNetworkRetry`/无意义 attemptOnce 包装。
2. src/ 内部 import 无扩展名在 node ESM 下 ERR_MODULE_NOT_FOUND → 批量补 `.ts` 后缀（tsconfig 已有 allowImportingTsExtensions）。
3. build（emit 模式）不允许 `.ts` 后缀 import → tsconfig.build.json 启用 TS 5.7+ `rewriteRelativeImportExtensions`（编译时重写为 .js）。
4. 测试 afterEach 顺序错误（先解绑 wx 再 logout/clearStoredSelection 抛错）→ logout/clearStoredSelection 对 storage 不可用容错 + 调整顺序。
5. **真 bug（测试抓到）**：`pickStoredSelection` 把记忆中空串字段当作"列表必须缺失"匹配 → 快照缺可选字段时上次选择失配；修正为"空值字段不参与匹配"。
6. noPrecisionLoss 拦截测试中的超精度字面量 → 统一改 `Number(MAX)`/`BigInt()` 形态（测试语义不变）。
7. no-token-log 断言初版正则跨行误报 → 改行级精确断言。
