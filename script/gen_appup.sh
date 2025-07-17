#!/bin/bash

# 用法: bash gen_appup_from_git.sh 0.6.1 0.6.2
OLD_TAG="$1"
NEW_TAG="$2"

if [ -z "$OLD_TAG" ] || [ -z "$NEW_TAG" ]; then
  echo "用法: $0 <old_tag> <new_tag>"
  exit 1
fi

for appdir in apps/*; do
  [ -d "$appdir/src" ] || continue
  app=$(basename "$appdir")

  # 获取旧tag和新tag下的模块名列表（小写原子）
  old_mods=$(git ls-tree -r --name-only "$OLD_TAG" -- "$appdir/src/" | grep '\.erl$' | sed 's/.*\///;s/\.erl$//' | tr 'A-Z' 'a-z' | sort)
  new_mods=$(git ls-tree -r --name-only "$NEW_TAG" -- "$appdir/src/" | grep '\.erl$' | sed 's/.*\///;s/\.erl$//' | tr 'A-Z' 'a-z' | sort)

  # 新增模块
  added=$(comm -13 <(echo "$old_mods") <(echo "$new_mods"))
  # 删除模块
  removed=$(comm -23 <(echo "$old_mods") <(echo "$new_mods"))
  # 可能修改的模块（交集）
  common=$(comm -12 <(echo "$old_mods") <(echo "$new_mods"))

  # 检查内容有变动的模块
  changed=""
  for mod in $common; do
    oldfile=$(git show "$OLD_TAG:$appdir/src/$mod.erl")
    newfile=$(git show "$NEW_TAG:$appdir/src/$mod.erl")
    if ! diff -q <(echo "$oldfile") <(echo "$newfile") >/dev/null; then
      changed="$changed $mod"
    fi
  done

  # 构造 .appup 升级/降级指令
  up_cmds=""
  for mod in $added $changed; do
    up_cmds="$up_cmds{load_module, $mod}, "
  done
  for mod in $removed; do
    up_cmds="$up_cmds{delete_module, $mod}, "
  done
  up_cmds=$(echo "$up_cmds" | sed 's/, $//;s/^[ ]*//')

  down_cmds=""
  for mod in $changed $removed; do
    down_cmds="$down_cmds{load_module, $mod}, "
  done
  for mod in $added; do
    down_cmds="$down_cmds{delete_module, $mod}, "
  done
  down_cmds=$(echo "$down_cmds" | sed 's/, $//;s/^[ ]*//')

  # 写入 .appup 文件
  appup_path="$appdir/src/${app}.appup"
  cat > "$appup_path" <<EOF
{"$OLD_TAG", [
    {"$NEW_TAG", [ $up_cmds ], []}
], [
    {"$OLD_TAG", [ $down_cmds ], []}
]}.
EOF

  echo "Generated $appup_path"
done
