#!/usr/bin/env bash
# 在远端仓库根目录执行；构建缓存异常时仅重建应用，不清理依赖。
set -Eeuo pipefail
vsn="${1:?缺少 release 版本}"
rebuild=0
if [[ -f .erlang.mk/imboy.test || ! -f ebin/imboy.app ]] \
  || ! grep -Fq "{vsn, \"$vsn\"}" ebin/imboy.app; then
  rebuild=1
fi

# 删除/重命名模块后必须刷新 .app，并清掉不再属于源码的旧 beam。
sources=$'\n'
while IFS= read -r source; do
  module="${source##*/}"
  sources+="${module%.erl}"$'\n'
done < <(find src -name '*.erl')
for beam in ebin/*.beam; do
  [[ -f "$beam" ]] || continue
  module="${beam##*/}"
  case "$sources" in
    *$'\n'"${module%.beam}"$'\n'*) ;;
    *) rebuild=1; break ;;
  esac
done
if [[ "$rebuild" -eq 1 ]]; then
  make clean-app
fi
# 复用现有缺 beam 自愈与 feature 排除规则。
make beam-presence-guard
