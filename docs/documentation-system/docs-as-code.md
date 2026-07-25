# Docs-as-Code 工具链与 CI 流水线方案

> **类型**：解释 + 指南 · **读者**：工程效能 / 文档负责人 · **版本**：v1.0 · **最后更新**：2026-07-24

目标：文档与代码同流程管理——同仓存储、PR 评审、CI 门禁、自动发布。

---

## 1. 工具选型

| 用途 | 选型 | 理由 |
|------|------|------|
| 文档站点生成 | **VitePress** | 团队已有 bun/Node 工具链；配置极简；Markdown 原生；不需要 React 定制（Docusaurus 对本团队过重） |
| API 参考生成 | **Redocly CLI** | 从 `api/openapi.yaml` 生成静态 HTML，可嵌入 VitePress |
| SDK 参考生成 | **TypeDoc** | imboy-sdk-js 从 TS 注释生成 API 参考 |
| Markdown 检查 | **markdownlint-cli2** | 统一格式，规则可配 |
| 死链检查 | **lychee** | Rust 实现，快，支持本地文件与外链 |
| 写作风格检查 | **Vale**（可选，Phase 4 引入） | 强制术语表与语气规则 |

**否决项记录**：Docusaurus（功能过剩）、Sphinx（Python 生态，团队不匹配）、GitBook（闭源 SaaS，不符合私有化基因）。

## 2. 仓库布局

文档站点工程放 `imboy/docs-site/`（imboy 仓内）：

```
imboy/
├── api/openapi.yaml            # API 契约唯一真源
├── docs/                       # 内容真源（Markdown）
└── docs-site/
    ├── package.json            # vitepress + redocly + lint 工具
    ├── .vitepress/config.ts    # 站点配置
    ├── public/                 # 静态资源（logo、favicon）
    └── scripts/
        ├── sync-content.sh     # 拉取 docs/ + 各仓文档到站点
        └── gen-api-reference.sh# openapi.yaml → reference/api/
```

**内容真源始终在 `docs/`**，`docs-site/` 只是构建层。 contributors 永远只编辑 `docs/` 下的 Markdown。

## 3. 站点配置（VitePress）

```typescript
// imboy/docs-site/.vitepress/config.ts
import { defineConfig } from 'vitepress';

export default defineConfig({
  title: 'IMBoy Docs',
  description: 'IMBoy 私有化即时通讯平台 — 部署、集成与开发文档',
  lang: 'zh-CN',
  cleanUrls: true,
  lastUpdated: true,

  themeConfig: {
    logo: '/imboy_logo.svg',
    nav: [
      { text: '教程', link: '/tutorials/' },
      { text: '操作指南', link: '/guides/' },
      { text: '参考', link: '/reference/' },
      { text: '设计解析', link: '/explanation/' },
      { text: 'ADR', link: '/adr/' },
    ],
    sidebar: {
      '/tutorials/': [{ text: '快速上手', items: [
        { text: '本地跑通后端', link: '/tutorials/quickstart-backend' },
        { text: '私有化部署', link: '/tutorials/quickstart-deploy' },
        { text: 'SDK 发出第一条消息', link: '/tutorials/first-message-with-sdk' },
      ]}],
      // guides/ reference/ explanation/ 由构建脚本按目录自动生成
    },
    search: { provider: 'local' },   // 私有化友好，不依赖 Algolia
    editLink: {
      pattern: 'https://github.com/imboy-pub/imboy/edit/main/docs/:path',
      text: '在 GitHub 上编辑此页',
    },
  },
});
```

## 4. CI 流水线

在 imboy 仓现有 CI 中新增 `docs.yml` workflow：

```yaml
# .github/workflows/docs.yml（或 gitee 等价 CI）
name: docs
on:
  pull_request:
    paths: ['docs/**', 'api/openapi.yaml', 'documentation-system/**']

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: markdownlint
        run: bunx markdownlint-cli2 "docs/**/*.md"
      - name: 元信息头校验
        run: bash docs-site/scripts/check-doc-meta.sh
      - name: 死链检查
        run: bunx lychee --offline --base docs 'docs/**/*.md'

  api-reference-sync:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: 重新生成 API 参考
        run: bash docs-site/scripts/gen-api-reference.sh
      - name: 检查生成物与提交一致
        run: git diff --exit-code docs/reference/api/
        # 不一致 = 作者改了 openapi.yaml 但没重新生成 → CI 红
```

### 4.1 元信息头校验脚本

```bash
#!/usr/bin/env bash
# docs-site/scripts/check-doc-meta.sh
# 校验新增/修改的 md 文件含必备元信息
set -e
failed=0
for f in $(git diff --name-only --diff-filter=AM origin/main...HEAD -- 'docs/*.md'); do
  case "$f" in
    docs/archive/*|docs/adr/*|*/README.md) continue ;;  # 豁免目录
  esac
  grep -q '\*\*类型\*\*' "$f" || { echo "缺少类型元信息: $f"; failed=1; }
  grep -q '\*\*读者\*\*' "$f" || { echo "缺少读者元信息: $f"; failed=1; }
done
exit $failed
```

## 5. API 参考自动生成

```bash
#!/usr/bin/env bash
# docs-site/scripts/gen-api-reference.sh
set -euo pipefail
cd "$(dirname "$0")/.."

# openapi.yaml → 单文件静态 HTML（Redoc）
bunx @redocly/cli build-docs ../api/openapi.yaml \
  --output ../docs/reference/api/index.html \
  --title "IMBoy REST API 参考"

echo "API 参考已生成: docs/reference/api/index.html"
```

**原则**：`api/openapi.yaml` 是唯一真源。任何手写 API 参考页面都视为草案，建模进 openapi.yaml 后删除手写版。

## 6. 多仓内容汇聚

文档站点需展示 SDK、插件开发等跨仓内容。采用 **构建期拉取**（而非 git submodule，避免贡献者负担）：

```bash
#!/usr/bin/env bash
# docs-site/scripts/sync-content.sh
# 构建站点前，将各仓文档复制到站点内容目录
set -euo pipefail
SITE_CONTENT="$(dirname "$0")/../content"
mkdir -p "$SITE_CONTENT"/{sdk,plugin}

rsync -a --delete ../../imboy-sdk-js/docs/      "$SITE_CONTENT/sdk/"
rsync -a --delete ../../imboy-plugin-marketplace/docs/ "$SITE_CONTENT/plugin/"
rsync -a --delete ../docs/                      "$SITE_CONTENT/main/"
```

各仓在自己 CI 里跑自己的 lint；站点构建时汇聚。

## 7. 版本化策略

| 对象 | 策略 |
|------|------|
| 文档站点 | 跟随 imboy 后端 release tag 打版本快照（VitePress 多版本开关），默认展示 latest stable |
| `docs/` 内文档 | 头部「适用版本」字段声明；行为随版本变化时原地更新并标注版本区间 |
| API 参考 | openapi.yaml 随代码打 tag；站点提供 `v2.x / v1.x` 切换 |
| 过期文档 | 移入 `docs/archive/`，站内 404 页提供搜索入口；**永不删除** |

## 8. 落地顺序

1. **Phase 3 第 1 步**：`docs-site/` 脚手架 + 本地 `bun run dev` 可预览现有 docs/
2. **第 2 步**：CI 加 lint + 死链检查（先 warn 一周，再转 error）
3. **第 3 步**：gen-api-reference.sh 接入 CI，openapi.yaml 变更强制同步
4. **第 4 步**：站点部署到内网 / docs 子域名（nginx 反代，复用现有 deploy 栈）
5. **Phase 4**：引入 Vale 术语检查；多版本切换
