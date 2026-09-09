# STEP-12 决策与风险备忘 — moya 工程基线

> Agent D（MOYA），2026-09-09。计划 §8/Step 12/D-05/D-13/D-14 已读并执行。

## 构建链路选型（Step 13 需要遵守）

**源码 `src/` → tsc(CommonJS) + 静态复制 → `dist/`，`project.config.json` 的 `miniprogramRoot: "dist/"`，微信开发者工具导入仓根。**

- 未选"工具内编译 TS"：质量门必须不依赖微信工具（本机未装工具，CI 也没有）；源码-产物分离使 typecheck/lint/test 全命令行。
- `tsconfig.json`（typecheck，include src+tests，allowImportingTsExtensions）与 `tsconfig.build.json`（emit dist，types 仅 miniprogram-api-typings）分离。
- 测试走 Node 26 原生 TS type-stripping（`node --test "tests/**/*.test.ts"`），无 vitest/esbuild 依赖链，符合"最小依赖集"。
- `dist/package.json` 写 `{"type":"commonjs"}`：根 package.json 是 ESM（scripts 需要），产物标记 CJS 供小程序模块体系与 node 冒烟加载。
- Step 13 新增页面/组件：在 `src/pages/**` 建四件套（.ts/.json/.wxml/.wxss）+ 注册 `src/app.json`，`npm run build` 后产物自动进 dist；公共请求层建议放 `src/utils/` 或新建 `src/core/`（命名再定），保持纯函数部分可单测。

## lint 选型

biome 2.x（单二进制、安装快、flat JSON 配置）：`linter.recommended` + src 级 `noConsole: error`（小程序代码不允许散落 console，Step 13 日志策略另行封装）；`scripts/**` override 豁免 noConsole（CLI 必须 console）。eslint+插件链安装更重，未选。

## 用户资产（PNG）处理

`moyalogo_144X144.png` / `moyalogo_256X256.png` 保持根级原位、未修改/移动/重命名（shasum：144=1b88fb5c…、256=ec4090fd…，mtime 未变）。首屏引用方式：build.mjs 以 **cp 复制**（非移动）进 `dist/assets/`，WXML 用绝对路径 `/assets/moyalogo_144X144.png`。任务书允许"按文件名引用"；复制生成新副本不属于修改原资产。

## AppID / 环境约定

- `project.config.json.appid = "touristappid"`（官方游客占位），真实 AppID 由用户在微信工具中手动填入并落 `project.private.config.json`（gitignored）。
- `env.example` 定义 `MOYA_MPA_API_BASE`（IMBoy 教学 API origin）；`.env*` 已 gitignore；运行时读取在 Step 13 实现。

## 验收判定

- MOYA-BASE-01：**PARTIAL** —— 命令行 typecheck/lint/test/scan/build 全 0；"微信工具导入显示非空首屏"因本机未安装微信开发者工具（`/Applications/wechatwebdevtools.app` 不存在）标 `BLOCKED_EXTERNAL`。首屏内容静态可核：`src/pages/index/`（品牌名/口号/logo/印章红点缀）+ `dist/` 产物完整，sitemap 全站 disallow（默认私密，呼应 D-11）。
- MOYA-BASE-02：**PASS** —— scan 0 命中（且灵敏度自测 3/3 命中后清理）；appid=touristappid；无 PII；无 IMBoy 服务端凭据；依赖锁文件无运行时依赖。
- MOYA-BASE-03：**PASS** —— `dependencies: {}`；devDeps 仅 typescript/@biomejs/biome/miniprogram-api-typings/@types/node；无跨端框架/状态库/UI 库/飞书钉钉适配器。

## 已知风险

1. **未做真机/工具验证**：dist 能否在微信工具中零警告导入未证实（工具未安装）。`ignoreUploadUnusedFiles`、`enhance` 等 setting 字段按现行文档惯例填写，工具版本差异可能重置个别项。
2. **libVersion 未锁定**：project.config.json 未写 libVersion，工具将用默认基础库版本；Step 13 冻结公共壳时建议锁定并记录最低基础库版本。
3. **Node 26 type-stripping 依赖**：`npm test` 依赖 Node >= 22.6（默认启用 strip types 需 23.6+）。CI 或他人环境用旧 node 会失败；README 已注明 Node 26。团队若需兼容旧 node，可后续加 vitest（当时再评估）。
4. **miniprogram-api-typings 与 @types/node 同置一个 tsconfig**：当前无冲突（wx 全局与 node 全局正交），但 Step 13 引入更多 wx API 后如遇全局类型冲突，可将 tests 拆独立 tsconfig。
5. **scan 为正则启发式**：能挡常见形态（AppSecret/私钥/AKIA/ghp_/sk-/wx+16hex），不能替代人工审查；pattern 拼接构造避免自误报，新增模式时保持该写法。
6. **README.md 已被修改但未提交**（git 层面仍为 working tree 变更）：任务书禁止 git 操作，提交由用户/coordinator 决定。

## 给 Step 13 的公共壳现状

- 目录：`src/app.{ts,json,wxss}`、`src/sitemap.json`、`src/pages/index/`（品牌首屏可改造为登录落点）、`src/utils/{tsid,format}.ts`、`tests/`。
- 入口命令：`npm run check`（四门）、`npm run build`（必须构建后微信工具才可见变更）。
- TSID：直接复用 `assertTsid/isTsid/compareTsid`（branded type `Tsid`，DTO 边界校验）。
- 品牌色 token 见 README 表（纸白 #F7F4EC / 墨黑 #1A1A1A / 芽绿 #4C9A5B / 印章红 #B03A2E）；Step 13 可抽为 wxss 变量或 design token 文件。
- tabBar：当前无 tabBar（单页）；家长/老师双模式导航（计划 §8.1）在 Step 13 按服务端身份驱动实现。
