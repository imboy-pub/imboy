# STEP-12 命令证据 — moya 工程基线

> 采集时间：2026-09-09（Agent D）；目录：`/Users/leeyi/project/imboy.pub/moya`
> 环境：node v26.4.0 / npm 11.17.0 / macOS darwin 25.6.0 arm64
> 基线：moya HEAD = `9644a2e804ecabc232fd313d87f7825339b4aceb`（main），开工时仅 README.md 已跟踪 + 两个未跟踪 PNG

## 质量门（最终态，全部真实执行）

| 命令 | 作用 | 退出码 |
|---|---|---|
| `npm run typecheck`（tsc --noEmit，含 src+tests） | 类型检查 | 0 |
| `npm run lint`（biome check src tests scripts） | lint + 格式 | 0 |
| `npm run test`（node --test "tests/**/*.test.ts"） | 单元测试 | 0 |
| `npm run scan`（node scripts/scan.mjs） | secret 扫描 | 0 |
| `npm run check`（以上四项串联） | 聚合门 | 0 |
| `npm run build`（node scripts/build.mjs） | 构建产物 dist/ | 0 |
| `npm install` | 依赖安装 | 0（0 vulnerabilities） |

## scan 灵敏度自测（防假绿灯）

在仓内临时创建 `scan-selftest.tmp.ts`（内容为三个公开示例假凭据：`wx1234567890abcdef` / `AKIAIOSFODNN7EXAMPLE` / 32 位 hex app_secret），执行 `node scripts/scan.mjs`：

```text
[scan] 命中 微信小程序真实 AppID: scan-selftest.tmp.ts:1
[scan] 命中 AWS AccessKey: scan-selftest.tmp.ts:2
[scan] 命中 AppSecret/密钥赋值（32 位 hex）: scan-selftest.tmp.ts:3
[scan] 失败：共 3 处疑似敏感信息
```

删除该文件后重跑：`[scan] 通过`，退出码 0；`ls scan-selftest.tmp.ts` 确认文件已不存在。

## dist 编译产物冒烟（node require CJS）

```text
isTsid(max)= true | isTsid(overflow)= false | isTsid(num)= false | cmp(999,1000)= -1
```

构建产物文件清单（11 个）：

```text
dist/app.js  dist/app.json  dist/app.wxss  dist/sitemap.json
dist/package.json            # {"type":"commonjs"}，node 冒烟/工具兼容标记
dist/pages/index/{index.js,index.json,index.wxml,index.wxss}
dist/utils/{format.js,tsid.js}
dist/assets/moyalogo_144X144.png
```

## 微信开发者工具导入验证

```text
ls /Applications/wechatwebdevtools.app → No such file or directory
ls /Applications/wechatwebdevtools.app/Contents/MacOS/cli → No such file or directory
mdfind "kMDItemCFBundleIdentifier == 'com.tencent.wechat.devtools'" → 无结果
```

本机仅有 WeChat.app（微信本体），未安装开发者工具 → 该子项 `BLOCKED_EXTERNAL`（需用户在微信开发者工具中手动导入验证）。

## 过程中真实的失败与修复（非一次通过）

1. `node --test tests/`（裸目录）在 Node 26 报 MODULE_NOT_FOUND → 改为 glob `"tests/**/*.test.ts"`。
2. tsid 正则 `[1-9][0-9]{0,17}` 上限 18 位，19 位合法 int64 被误拒 → 改 `{0,18}`（单测先抓到，typecheck 抓不到的运行时 bug）。
3. biome 2.x 配置键 `assists` 应为 `assist`；块注释内 `src/**/*.ts` 的 `*/` 序列提前终止注释导致 parse error → 修正。
4. 测试中故意写入的超精度数字字面量触发 noPrecisionLoss → 改用 `Number("...")` 字符串构造，保留测试语义。
5. scripts 为 CLI 工具需要 console → biome overrides 对 `scripts/**` 豁免 noConsole（src 保持 error）。
6. package.json `"type":"module"` 与 tsc CJS 产物冲突 → build 时向 dist 写入 `{"type":"commonjs"}` 标记。
