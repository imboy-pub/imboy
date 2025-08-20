#!/bin/bash

# 生成符合 relx 规范的 .appup 文件
# 用法:
#   首次发布: bash script/gen_appup.sh 0.6.4
#   版本升级: bash script/gen_appup.sh 0.6.3 0.6.4
# 行为:
#   - 子应用 apps/*: 基于 git 标签差异生成 add_module/delete_module/load_module，生成“多 term 历史格式”
#   - 顶层 src/imboy.appup: 生成“多 term 历史格式”，新版本置顶；如 imboy_sup 变更，注入 {update, imboy_sup, supervisor}；对子应用变化与依赖变化、vsn 变化注入 {restart_application, App}
#   - 依赖变化: 若 Makefile DEPS 或 include/deps.mk dep_* 规格变更，也纳入顶层重启集合
#   - 幂等: 同一 {OLD -> NEW} 重复执行，顶层会覆盖同版本条目；子应用会覆盖同版本 appup 内容
#   - 写入流程: 优先 Erlang 模块 imboy_appup 生成，失败回退到原 Bash 文本拼装

set -e

normalize_tag() {
  local t="$1"
  if git rev-parse "$t" >/dev/null 2>&1; then
    echo "$t"
  elif git rev-parse "v$t" >/dev/null 2>&1; then
    echo "v$t"
  else
    echo "$t"
  fi
}

# 解析参数（支持环境变量回退）
if [ $# -eq 2 ]; then
  OLD_TAG="$1"
  NEW_TAG="$2"
  FIRST_RELEASE=false
  echo "版本升级模式: 从 $OLD_TAG 升级到 $NEW_TAG"
elif [ $# -eq 1 ]; then
  NEW_TAG="$1"
  OLD_TAG=""
  FIRST_RELEASE=true
  echo "首次发布模式: 生成版本 $NEW_TAG 的 appup 文件"
else
  # 环境变量兼容 Makefile
  NEW_TAG="${PROJECT_VERSION:-${NEW_VERSION:-${NEW_TAG:-}}}"
  OLD_TAG="${OLD_VERSION:-${OLD_TAG:-${PREV_VERSION:-}}}"
  if [ -n "$NEW_TAG" ] && [ -z "$OLD_TAG" ]; then
    FIRST_RELEASE=true
    echo "首次发布模式(环境变量): 生成版本 $NEW_TAG 的 appup 文件"
  elif [ -n "$NEW_TAG" ] && [ -n "$OLD_TAG" ]; then
    FIRST_RELEASE=false
    echo "版本升级模式(环境变量): 从 $OLD_TAG 升级到 $NEW_TAG"
  else
    echo "用法:"
    echo "  首次发布: $0 <new_tag> 或设置 PROJECT_VERSION/NEW_VERSION/NEW_TAG"
    echo "  版本升级: $0 <old_tag> <new_tag> 或设置 OLD_VERSION/OLD_TAG/PREV_VERSION 与 PROJECT_VERSION/NEW_VERSION/NEW_TAG"
    exit 1
  fi
fi

# 校验/规范化 tag
if [ "$FIRST_RELEASE" = "false" ]; then
  OLD_TAG="$(normalize_tag "$OLD_TAG")"
  NEW_TAG="$(normalize_tag "$NEW_TAG")"
  git rev-parse "$OLD_TAG" >/dev/null 2>&1 || { echo "错误: git tag '$OLD_TAG' 不存在"; exit 1; }
  git rev-parse "$NEW_TAG" >/dev/null 2>&1 || { echo "错误: git tag '$NEW_TAG' 不存在"; exit 1; }
else
  echo "首次发布模式: 不检查 git tag 是否存在"
fi

echo "正在生成从 '$OLD_TAG' 到 '$NEW_TAG' 的 .appup ..."; echo ""

# 优先使用 Erlang 模块 imboy_appup 生成，失败则回退到 Bash 实现
erlang_ok=0
if [ "$FIRST_RELEASE" = "true" ]; then
  if erl -noshell -eval "case c:c(\"apps/imlib/src/imboy_appup.erl\") of {ok, imboy_appup} -> case imboy_appup:first_release(\"$NEW_TAG\") of ok -> halt(0); {error, R} -> io:format(\"~p~n\", [R]), halt(1) end; _ -> halt(1) end." -s init stop 2>/dev/null; then
    erlang_ok=1
  fi
else
  if erl -noshell -eval "case c:c(\"apps/imlib/src/imboy_appup.erl\") of {ok, imboy_appup} -> case imboy_appup:run(\"$OLD_TAG\", \"$NEW_TAG\") of ok -> halt(0); {error, R} -> io:format(\"~p~n\", [R]), halt(1) end; _ -> halt(1) end." -s init stop 2>/dev/null; then
    erlang_ok=1
  fi
fi

if [ $erlang_ok -eq 1 ]; then
  echo "✓ 使用 imboy_appup 生成完成"
  exit 0
else
  echo "Erlang 生成失败，回退到 Bash 逻辑..."
fi

# ----------------------- 下面保留原 Bash 回退实现 -----------------------

# 函数: 验证 .appup 文件是否为 Erlang terms（可多 term）
validate_appup_file() {
  local appup_file="$1"
  if [ -f "$appup_file" ]; then
    if erl -noshell -eval "case file:consult(\"$appup_file\") of {ok, _} -> halt(0); _ -> halt(1) end." 2>/dev/null; then
      return 0
    else
      return 1
    fi
  else
    return 1
  fi
}

# 函数: 获取指定 tag 下 app_dir/src 的模块名列表
get_modules_at_tag() {
  local tag="$1"; local app_dir="$2"
  if [ -n "$tag" ]; then
    git ls-tree -r --name-only "$tag" -- "$app_dir/src" 2>/dev/null | \
      grep '\.erl$' | xargs -I {} basename {} .erl | sort
  else
    echo ""
  fi
}

# 函数: 使用 Erlang 生成单个 app 的 appup（允许空指令）
# 参数: app_name app_dir old_vsn new_vsn added deleted modified
generate_appup_with_systools() {
  local app_name="$1" app_dir="$2" old_vsn="$3" new_vsn="$4"
  local added="$5" deleted="$6" modified="$7"
  local appup_file="$app_dir/src/$app_name.appup"

  local up=""; local down=""
  for m in $added; do
    up="$up{add_module, $m},"; down="{delete_module, $m},$down"
  done
  for m in $deleted; do
    up="$up{delete_module, $m},"; down="{add_module, $m},$down"
  done
  for m in $modified; do
    up="$up{load_module, $m},"; down="{load_module, $m},$down"
  done
  up=$(echo "$up" | sed 's/,$//'); down=$(echo "$down" | sed 's/,$//')

  local tmp="/tmp/gen_appup_${app_name}.erl"
  cat > "$tmp" <<EOF
-module(gen_appup_${app_name}).
-export([generate/0]).
generate() ->
  NewVsn = "$new_vsn",
  OldVsn = "$old_vsn",
  UpInstr = [$up],
  DownInstr = [$down],
  File = "$appup_file",
  file:write_file(File, io_lib:format("~p.~n", [{NewVsn, [{OldVsn, UpInstr}], [{OldVsn, DownInstr}]}])).
EOF
  if erl -noshell -pa ebin -eval "c:c(\"$tmp\"), gen_appup_${app_name}:generate(), halt()." 2>/dev/null; then
    rm -f "$tmp"
    return 0
  else
    rm -f "$tmp"
    return 1
  fi
}

processed_count=0
skipped_count=0
failed_count=0
changed_apps=""

# 遍历 apps/*
for app_dir in apps/*/; do
  [ -d "$app_dir" ] || continue
  app_name=$(basename "$app_dir")
  appup_file="$app_dir/src/$app_name.appup"
  echo "处理应用: $app_name"

  if [ "$FIRST_RELEASE" = "true" ]; then
    # 首发: 仅写空指令 term
    mkdir -p "$app_dir/src"
    cat > "$appup_file" <<EOF
{"$NEW_TAG", [], []}.
EOF
    if validate_appup_file "$appup_file"; then
      echo "  ✓ $app_name.appup 生成 (首次)"
      processed_count=$((processed_count+1))
    else
      echo "  ✗ $app_name.appup 验证失败"
      failed_count=$((failed_count+1))
    fi
    continue
  fi

  old_modules=$(get_modules_at_tag "$OLD_TAG" "$app_dir")
  new_modules=$(get_modules_at_tag "$NEW_TAG" "$app_dir")
  if [ -z "$new_modules" ]; then
    echo "  跳过: 未找到 .erl"
    skipped_count=$((skipped_count+1))
    continue
  fi

  if [ -n "$old_modules" ] && [ -n "$new_modules" ]; then
    added=$(comm -13 <(echo "$old_modules") <(echo "$new_modules"))
    deleted=$(comm -23 <(echo "$old_modules") <(echo "$new_modules"))
    common=$(comm -12 <(echo "$old_modules") <(echo "$new_modules"))
  elif [ -z "$old_modules" ] && [ -n "$new_modules" ]; then
    added="$new_modules"; deleted=""; common=""
  elif [ -n "$old_modules" ] && [ -z "$new_modules" ]; then
    added=""; deleted="$old_modules"; common=""
  else
    added=""; deleted=""; common=""
  fi

  modified=""
  if [ -n "$common" ]; then
    while IFS= read -r m; do
      [ -z "$m" ] && continue
      old_c=$(git show "$OLD_TAG:$app_dir/src/$m.erl" 2>/dev/null || echo "")
      new_c=$(git show "$NEW_TAG:$app_dir/src/$m.erl" 2>/dev/null || echo "")
      if [ -z "$old_c" ] || [ "$old_c" != "$new_c" ]; then
        modified="$modified $m"
      fi
    done < <(echo "$common")
  fi

  has_changes=false
  if [ -n "$added$deleted$modified" ]; then has_changes=true; fi

  if $has_changes; then
    changed_apps="$changed_apps $app_name"
  fi

  mkdir -p "$app_dir/src"
  if generate_appup_with_systools "$app_name" "$app_dir" "$OLD_TAG" "$NEW_TAG" "$added" "$deleted" "$modified"; then
    echo "  ✓ 生成 $app_name.appup"
    processed_count=$((processed_count+1))
  else
    echo "  ✗ 生成 $app_name.appup 失败"
    failed_count=$((failed_count+1))
  fi
done

# 依赖变化检测纳入 changed_apps
if [ "$FIRST_RELEASE" = "false" ]; then
  if git show "$NEW_TAG:Makefile" >/dev/null 2>&1 && git show "$OLD_TAG:Makefile" >/dev/null 2>&1; then
    cur_mk=$(git show "$NEW_TAG:Makefile" 2>/dev/null || echo "")
    old_mk=$(git show "$OLD_TAG:Makefile" 2>/dev/null || echo "")
    cur_deps_mk=$(git show "$NEW_TAG:include/deps.mk" 2>/dev/null || echo "")
    old_deps_mk=$(git show "$OLD_TAG:include/deps.mk" 2>/dev/null || echo "")

    get_deps() { echo "$1" | grep -E '^[[:space:]]*DEPS[[:space:]]*(\+=|:=)' | sed -E 's/^[[:space:]]*DEPS[[:space:]]*(\+=|:=)[[:space:]]*//' | tr ' ' '\n' | sed '/^$/d' | sort -u; }
    cur_deps=$(get_deps "$cur_mk"); old_deps=$(get_deps "$old_mk")

    get_specs() { echo "$1" | grep -E '^[[:space:]]*dep_[A-Za-z0-9_]+[[:space:]]*[:]?=' | sed -E 's/^[[:space:]]*dep_([A-Za-z0-9_]+)[[:space:]]*[:]?=[[:space:]]*(.*)$/\1|||\2/'; }
    cur_specs=$(get_specs "$cur_deps_mk"); old_specs=$(get_specs "$old_deps_mk")
    spec_for(){ echo "$2" | awk -F '|||' -v n="$1" '$1==n{print $2; exit}'; }

    union=$(printf "%s\n%s\n" "$cur_deps" "$old_deps" | sort -u)
    for dep in $union; do
      [ -z "$dep" ] && continue
      in_cur=$(echo "$cur_deps" | grep -qx "$dep" && echo yes || echo no)
      in_old=$(echo "$old_deps" | grep -qx "$dep" && echo yes || echo no)
      if [ "$in_cur" != "$in_old" ]; then
        changed_apps="$changed_apps $dep"
        echo "  * 第三方依赖成员变更: $dep"
        continue
      fi
      cs=$(spec_for "$dep" "$cur_specs"); os=$(spec_for "$dep" "$old_specs")
      if [ -n "$cs$os" ] && [ "$cs" != "$os" ]; then
        changed_apps="$changed_apps $dep"
        echo "  * 第三方依赖规格变更: $dep"
      fi
    done
  fi
fi

# 顶层 imboy.appup 生成（多 term 历史格式）
imboy_appup_file="src/imboy.appup"
mkdir -p "$(dirname "$imboy_appup_file")"

if [ "$FIRST_RELEASE" = "true" ]; then
  # 首发仅写空指令 term
  cat > "$imboy_appup_file" <<EOF
{"$NEW_TAG", [], []}.
EOF
  if validate_appup_file "$imboy_appup_file"; then
    echo "  ✓ 生成 imboy.appup (首次)"
  else
    echo "  ✗ 生成 imboy.appup 失败"
    failed_count=$((failed_count+1))
  fi
else
  # 升级: 组装顶层升级/降级指令
  # 检测 imboy_sup 变化
  old_sup=$(git show "$OLD_TAG:src/imboy_sup.erl" 2>/dev/null || echo "")
  new_sup=$(git show "$NEW_TAG:src/imboy_sup.erl" 2>/dev/null || echo "")
  up=""; down=""
  if [ -n "$old_sup" ] && [ -n "$new_sup" ] && [ "$old_sup" != "$new_sup" ]; then
    up="${up}    {update, imboy_sup, supervisor},\n"
    down="${down}    {update, imboy_sup, supervisor},\n"
  fi
  # 追加重启的应用
  unique_apps=$(echo "$changed_apps" | tr ' ' '\n' | sed '/^$/d' | sort -u | xargs)
  for a in $unique_apps; do
    up="${up}    {restart_application, $a},\n"
    down="${down}    {restart_application, $a},\n"
  done
  up=$(echo -e "$up" | sed 's/,$//' ); down=$(echo -e "$down" | sed 's/,$//' )

  # 新版本 term 写入临时文件
  new_term_file="/tmp/new_term.appup"
  cat > "$new_term_file" <<EOF
{"$NEW_TAG",
 [{"$OLD_TAG", [
$up
 ]}],
 [{"$OLD_TAG", [
$down
 ]}]
}.
EOF

  # 合并新旧 term: 新版本置顶；过滤相同版本
  merge="/tmp/merge_appup.erl"
  cat > "$merge" << EOF
-module(merge_appup).
-export([merge/0]).
merge() ->
  NewFile = "/tmp/new_term.appup",
  CurFile = "src/imboy.appup",
  OutFile = "src/imboy.appup",
  {ok, [NewTerm]} = file:consult(NewFile),
  NewV = element(1, NewTerm),

  % 当前文件 terms
  CurTerms = case file:consult(CurFile) of {ok, Ts} -> Ts; _ -> [] end,

  % 读取 OLD_TAG 版本的 imboy.appup terms（若无则为空）
  _ = os:cmd("git show $OLD_TAG:src/imboy.appup > /tmp/imboy_appup_old_tag.appup 2>/dev/null"),
  OldTagTerms = case file:consult("/tmp/imboy_appup_old_tag.appup") of {ok, Ts} -> Ts; _ -> [] end,

  % 合并去重：按版本键合并，NewTerm 覆盖同版本
  Add = fun({V,_,_}=T, M) -> maps:put(V, T, M) end,
  M0 = lists:foldl(Add, #{}, CurTerms),
  M1 = lists:foldl(Add, M0, OldTagTerms),
  M2 = maps:remove(NewV, M1),
  M3 = maps:put(NewV, NewTerm, M2),

  % 排序输出：新版本置顶，其余按字典序降序
  Keys = maps:keys(M3),
  Others = [K || K <- Keys, K =/= NewV],
  Sorted = lists:reverse(lists:sort(Others)),
  TailTerms = [maps:get(K, M3) || K <- Sorted],
  OutTerms = [NewTerm | TailTerms],

  Data = lists:flatmap(fun(T) -> io_lib:format("~p.~n~n", [T]) end, OutTerms),
  file:write_file(OutFile, Data).
EOF
  if erl -noshell -eval "c:c(\"$merge\"), merge_appup:merge(), halt()." 2>/dev/null; then
    rm -f "$merge" "$new_term_file"
    if validate_appup_file "$imboy_appup_file"; then
      echo "  ✓ 生成 imboy.appup（多 term 历史格式，最新置顶）"
    else
      echo "  ✗ 顶层 imboy.appup 验证失败"
      failed_count=$((failed_count+1))
    fi
  else
    rm -f "$merge" "$new_term_file"
    echo "  ✗ 顶层 imboy.appup 合并失败"
    failed_count=$((failed_count+1))
  fi
fi

echo ""
echo "处理完成: 已处理=$processed_count 跳过=$skipped_count 失败=$failed_count"