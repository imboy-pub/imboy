#!/usr/bin/env bash
# sync-content.sh — 将 docs/ 内容同步到 VitePress srcDir (content/)
# 内容真源始终在 docs/，本脚本只是构建期的桥梁。
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SITE_ROOT="$(dirname "$SCRIPT_DIR")"
REPO_ROOT="$(dirname "$SITE_ROOT")"
CONTENT="$SITE_ROOT/content"

echo "==> 同步 docs/ → docs-site/content/"

# 清理旧内容（保留 index.md 首页）
find "$CONTENT" -mindepth 1 -maxdepth 1 ! -name 'index.md' -exec rm -rf {} + 2>/dev/null || true
mkdir -p "$CONTENT"

# 同步四象限 + 合规 + 架构 + 商业
for dir in tutorials guides reference explanation compliance architecture business; do
  if [ -d "$REPO_ROOT/docs/$dir" ]; then
    rsync -a --exclude='README.md' "$REPO_ROOT/docs/$dir/" "$CONTENT/$dir/"
    # 将 README.md 转为 index.md（VitePress 目录索引约定）
    if [ -f "$REPO_ROOT/docs/$dir/README.md" ]; then
      cp "$REPO_ROOT/docs/$dir/README.md" "$CONTENT/$dir/index.md"
    fi
  fi
done

# 顶层 docs/README.md → content/overview.md（门户页备用）
if [ -f "$REPO_ROOT/docs/README.md" ]; then
  cp "$REPO_ROOT/docs/README.md" "$CONTENT/overview.md"
fi

echo "==> 同步完成: $(find "$CONTENT" -name '*.md' | wc -l | tr -d ' ') 篇文档"
