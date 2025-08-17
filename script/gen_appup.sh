#!/bin/bash

# 生成符合relx规范的.appup文件脚本
# 用法1 (首次发布): bash script/gen_appup.sh 0.6.1
# 用法2 (版本升级): bash script/gen_appup.sh 0.6.1 0.6.2
# 用法2 (版本升级): bash script/gen_appup.sh 0.6.2 0.6.3
# 功能: 使用 Erlang systools 模块生成 appup 文件，比较git tag之间的差异，生成升级/降级指令

# 解析参数
if [ $# -eq 1 ]; then
  # 首次发布模式
  NEW_TAG="$1"
  OLD_TAG=""
  FIRST_RELEASE=true
  echo "首次发布模式: 生成版本 $NEW_TAG 的 appup 文件"
elif [ $# -eq 2 ]; then
  # 版本升级模式
  OLD_TAG="$1"
  NEW_TAG="$2"
  FIRST_RELEASE=false
  echo "版本升级模式: 从 $OLD_TAG 升级到 $NEW_TAG"
else
  echo "用法:"
  echo "  首次发布: $0 <new_tag>"
  echo "  版本升级: $0 <old_tag> <new_tag>"
  echo "示例:"
  echo "  $0 0.6.1"
  echo "  $0 0.6.1 0.6.2"
  exit 1
fi

# 检查git tag是否存在
if [ "$FIRST_RELEASE" = "false" ]; then
  if ! git rev-parse "$OLD_TAG" >/dev/null 2>&1; then
    echo "错误: git tag '$OLD_TAG' 不存在"
    exit 1
  fi
  
  if ! git rev-parse "$NEW_TAG" >/dev/null 2>&1; then
    echo "错误: git tag '$NEW_TAG' 不存在"
    exit 1
  fi
else
  echo "首次发布模式: 不检查 git tag 是否存在"
fi

if [ "$FIRST_RELEASE" = "true" ]; then
  echo "正在为首次发布生成版本 $NEW_TAG 的 .appup 文件..."
else
  echo "正在生成从 $OLD_TAG 到 $NEW_TAG 的 .appup 文件..."
fi
echo ""

# 使用 systools 生成 appup 文件的函数
# 参数: app_name app_dir old_vsn new_vsn added_modules deleted_modules modified_modules
generate_appup_with_systools() {
  local app_name="$1"
  local app_dir="$2"
  local old_vsn="$3"
  local new_vsn="$4"
  local added_modules="$5"
  local deleted_modules="$6"
  local modified_modules="$7"
  local appup_file="$app_dir/src/$app_name.appup"
  
  # 构建升级指令
  local upgrade_instructions=""
  local downgrade_instructions=""
  
  # 处理新增模块（真正的新增模块）
  if [ -n "$added_modules" ]; then
    for module in $added_modules; do
      upgrade_instructions="$upgrade_instructions{add_module, $module},"
      downgrade_instructions="{delete_module, $module},$downgrade_instructions"
    done
  fi
  
  # 处理删除模块（真正的删除模块）
  if [ -n "$deleted_modules" ]; then
    for module in $deleted_modules; do
      upgrade_instructions="$upgrade_instructions{delete_module, $module},"
      downgrade_instructions="{add_module, $module},$downgrade_instructions"
    done
  fi
  
  # 处理修改模块（只需要重新加载）
  if [ -n "$modified_modules" ]; then
    for module in $modified_modules; do
      upgrade_instructions="$upgrade_instructions{load_module, $module},"
      downgrade_instructions="{load_module, $module},$downgrade_instructions"
    done
  fi
  
  # 移除末尾的逗号
  upgrade_instructions=$(echo "$upgrade_instructions" | sed 's/,$//')
  downgrade_instructions=$(echo "$downgrade_instructions" | sed 's/,$//')
  
  # 如果没有任何变更，跳过
  if [ -z "$upgrade_instructions" ]; then
    return 1
  fi
  
  # 使用 Erlang 调用 systools 生成 appup 文件
  local temp_script="/tmp/gen_appup_${app_name}.erl"
  cat > "$temp_script" << EOF
-module(gen_appup_${app_name}).
-export([generate/0]).

generate() ->
    UpgradeInstructions = [$upgrade_instructions],
    DowngradeInstructions = [$downgrade_instructions],
    AppupContent = {
        "$new_vsn",
        [{
            "$old_vsn",
            UpgradeInstructions
        }],
        [{
            "$old_vsn",
            DowngradeInstructions
        }]
    },
    file:write_file("$appup_file", io_lib:format("~p.~n", [AppupContent])).
EOF
  
  # 执行 Erlang 脚本
  if erl -noshell -pa ebin -eval "c:c('$temp_script'), gen_appup_${app_name}:generate(), halt()." 2>/dev/null; then
    rm -f "$temp_script"
    return 0
  else
    rm -f "$temp_script"
    return 1
  fi
}

# 验证 appup 文件格式的函数
validate_appup_file() {
  local appup_file="$1"
  if [ -f "$appup_file" ]; then
    if erl -noshell -eval "case file:consult('$appup_file') of {ok, _} -> halt(0); {error, Reason} -> io:format('Error: ~p~n', [Reason]), halt(1) end." 2>/dev/null; then
      return 0
    else
      return 1
    fi
  else
    return 1
  fi
}

# 获取指定 git tag 下的模块列表
get_modules_at_tag() {
  local tag="$1"
  local app_dir="$2"
  if [ -n "$tag" ]; then
    # 使用 git ls-tree 获取指定 tag 下的文件列表
    git ls-tree -r --name-only "$tag" "$app_dir/src/" 2>/dev/null | \
      grep '\.erl$' | \
      xargs -I {} basename {} .erl | \
      sort
  else
    echo ""
  fi
}

# 获取当前的模块列表
get_current_modules() {
  local app_dir="$1"
  if [ -d "$app_dir/src" ]; then
    find "$app_dir/src" -name '*.erl' -exec basename {} .erl \; | sort
  else
    echo ""
  fi
}

# 统计变量
processed_count=0
skipped_count=0
failed_count=0

# 遍历 apps 目录下的每个应用
for app_dir in apps/*/; do
  if [ ! -d "$app_dir" ]; then
    continue
  fi
  
  app_name=$(basename "$app_dir")
  appup_file="$app_dir/src/$app_name.appup"
  
  echo "处理应用: $app_name"
  
  if [ "$FIRST_RELEASE" = "true" ]; then
    # 首次发布模式：创建基础 appup 文件
    current_modules=$(get_current_modules "$app_dir")
    
    if [ -z "$current_modules" ]; then
      echo "  跳过 $app_name: 没有找到 Erlang 模块"
      skipped_count=$((skipped_count + 1))
      continue
    fi
    
    # 创建基础 appup 文件（首次发布通常只包含版本信息）
    cat > "$appup_file" << EOF
{"$NEW_TAG", [], []}.
EOF
    
    if validate_appup_file "$appup_file"; then
      echo "  ✓ 已生成 $app_name.appup (首次发布)"
      processed_count=$((processed_count + 1))
    else
      echo "  ✗ 生成 $app_name.appup 失败"
      failed_count=$((failed_count + 1))
    fi
  else
    # 版本升级模式：比较模块差异
    old_modules=$(get_modules_at_tag "$OLD_TAG" "$app_dir")
    new_modules=$(get_current_modules "$app_dir")
    
    if [ -z "$new_modules" ]; then
      echo "  跳过 $app_name: 没有找到 Erlang 模块"
      skipped_count=$((skipped_count + 1))
      continue
    fi
    
    # 计算模块差异
    # 确保模块列表不为空时才进行比较
    if [ -n "$old_modules" ] && [ -n "$new_modules" ]; then
      added_modules=$(comm -13 <(echo "$old_modules" | sort) <(echo "$new_modules" | sort))
      deleted_modules=$(comm -23 <(echo "$old_modules" | sort) <(echo "$new_modules" | sort))
      common_modules=$(comm -12 <(echo "$old_modules" | sort) <(echo "$new_modules" | sort))
    elif [ -z "$old_modules" ] && [ -n "$new_modules" ]; then
      # 如果旧版本没有模块，所有当前模块都是新增的
      added_modules="$new_modules"
      deleted_modules=""
      common_modules=""
    elif [ -n "$old_modules" ] && [ -z "$new_modules" ]; then
      # 如果当前版本没有模块，所有旧模块都被删除了
      added_modules=""
      deleted_modules="$old_modules"
      common_modules=""
    else
      # 两个版本都没有模块
      added_modules=""
      deleted_modules=""
      common_modules=""
    fi
    
    # 检查修改的模块（通过比较文件内容）
    modified_modules=""
    if [ -n "$common_modules" ]; then
      for module in $common_modules; do
        old_content=$(git show "$OLD_TAG:$app_dir/src/$module.erl" 2>/dev/null || echo "")
        new_content=$(cat "$app_dir/src/$module.erl" 2>/dev/null || echo "")
        if [ "$old_content" != "$new_content" ]; then
          modified_modules="$modified_modules $module"
        fi
      done
    fi
    
    # 如果没有任何变更，跳过
    if [ -z "$added_modules" ] && [ -z "$deleted_modules" ] && [ -z "$modified_modules" ]; then
      echo "  跳过 $app_name: 没有模块变更"
      skipped_count=$((skipped_count + 1))
      continue
    fi
    
    # 备份现有的 appup 文件
    if [ -f "$appup_file" ]; then
      cp "$appup_file" "$appup_file.bak"
    fi
    
    # 尝试使用 systools 生成 appup 文件
    if generate_appup_with_systools "$app_name" "$app_dir" "$OLD_TAG" "$NEW_TAG" "$added_modules" "$deleted_modules" "$modified_modules"; then
      echo "  ✓ 使用 systools 生成 $app_name.appup"
      processed_count=$((processed_count + 1))
    else
      # 回退到传统方法
      echo "  ! systools 生成失败，使用传统方法"
      
      # 构建升级指令
      upgrade_instructions=""
      downgrade_instructions=""
      
      for module in $added_modules; do
        upgrade_instructions="$upgrade_instructions    {add_module, $module},\n"
        downgrade_instructions="    {delete_module, $module},\n$downgrade_instructions"
      done
      
      for module in $deleted_modules; do
        upgrade_instructions="$upgrade_instructions    {delete_module, $module},\n"
        downgrade_instructions="    {add_module, $module},\n$downgrade_instructions"
      done
      
      for module in $modified_modules; do
        upgrade_instructions="$upgrade_instructions    {load_module, $module},\n"
        downgrade_instructions="    {load_module, $module},\n$downgrade_instructions"
      done
      
      # 移除末尾的逗号和换行符
      upgrade_instructions=$(echo -e "$upgrade_instructions" | sed 's/,$//' | sed '/^$/d')
      downgrade_instructions=$(echo -e "$downgrade_instructions" | sed 's/,$//' | sed '/^$/d')
      
      # 处理现有的 appup 文件
      if [ -f "$appup_file.bak" ]; then
        # 读取现有版本记录
        existing_content=$(cat "$appup_file.bak")
        
        # 检查是否已存在当前版本的升级路径
        if echo "$existing_content" | grep -q "\"$NEW_TAG\""; then
          echo "  跳过 $app_name: 版本 $NEW_TAG 的升级路径已存在"
          mv "$appup_file.bak" "$appup_file"
          skipped_count=$((skipped_count + 1))
          continue
        fi
        
        # 解析现有的升级和降级记录
        existing_upgrades=$(echo "$existing_content" | sed -n 's/.*\[\([^]]*\)\].*/\1/p' | head -1)
        existing_downgrades=$(echo "$existing_content" | sed -n 's/.*\[\([^]]*\)\].*/\1/p' | tail -1)
        
        # 构建新的 appup 文件内容
        cat > "$appup_file" << EOF
{"$NEW_TAG",
  [
    {"$OLD_TAG", [
$upgrade_instructions
    ]},
$existing_upgrades
  ],
  [
    {"$OLD_TAG", [
$downgrade_instructions
    ]},
$existing_downgrades
  ]
}.
EOF
      else
        # 创建新的 appup 文件
        cat > "$appup_file" << EOF
{"$NEW_TAG",
  [
    {"$OLD_TAG", [
$upgrade_instructions
    ]}
  ],
  [
    {"$OLD_TAG", [
$downgrade_instructions
    ]}
  ]
}.
EOF
      fi
      
      if validate_appup_file "$appup_file"; then
        echo "  ✓ 已生成 $app_name.appup"
        processed_count=$((processed_count + 1))
      else
        echo "  ✗ 生成 $app_name.appup 失败"
        failed_count=$((failed_count + 1))
      fi
    fi
  fi
done

# 验证所有生成的 appup 文件
echo ""
echo "验证生成的 appup 文件格式..."
validation_failed=false

for app_dir in apps/*/; do
  if [ ! -d "$app_dir" ]; then
    continue
  fi
  
  app_name=$(basename "$app_dir")
  appup_file="$app_dir/src/$app_name.appup"
  
  if [ -f "$appup_file" ]; then
    if validate_appup_file "$appup_file"; then
      echo "  ✓ $app_name.appup 格式正确"
    else
      echo "  ✗ $app_name.appup 格式错误"
      validation_failed=true
    fi
  fi
done

echo ""
echo "处理完成!"
echo "  已处理: $processed_count 个应用"
echo "  已跳过: $skipped_count 个应用"
echo "  失败: $failed_count 个应用"

if [ "$validation_failed" = "true" ]; then
  echo "警告: 部分 appup 文件格式验证失败"
  exit 1
fi

echo "所有 appup 文件格式验证通过!"
