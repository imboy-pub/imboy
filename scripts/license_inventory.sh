#!/usr/bin/env bash
# 生成第三方依赖许可证清单（D3 发布门）。
#
# 为什么是脚本而不是手写表格：手写清单在第一次 `DEPS +=` 之后就开始撒谎，
# 而许可证清单撒谎的代价是法务的，不是工程的。这里只读磁盘上真实存在的
# LICENSE 文件正文来判定，不读包管理器元数据（元数据可以填错，正文不会）。
#
# 用法:
#   scripts/license_inventory.sh              # 输出 Markdown 到 stdout
#   scripts/license_inventory.sh --check      # 只做门禁：发现强 copyleft 则退出 1
#   scripts/license_inventory.sh --selftest   # 校验 classify 判别正确（10 条样本）
#
# 覆盖范围（明确声明，避免"看起来全扫了"的误读）:
#   ✅ imboy   Erlang 运行时依赖  <- deps/*/         （需先 make deps）
#   ✅ imboyapp Flutter 直接依赖  <- ../imboyapp/pubspec.lock + ~/.pub-cache
#   ❌ imboyadmin (npm) / imboy-sdk-js  —— 未纳入，见 docs/legal/third-party-licenses.md
set -uo pipefail

cd "$(dirname "$0")/.." || exit 1

MODE="${1:-report}"
STRONG_COPYLEFT_FOUND=0

# 按特异性从高到低排列。当前这组模式彼此互斥（"GNU AFFERO GENERAL PUBLIC" 里夹着
# AFFERO，匹配不上 "GNU GENERAL PUBLIC"），所以顺序此刻并不承重——但只要有人往上面
# 插一条更宽的模式（比如 *"GENERAL PUBLIC"*），下面的分支就会被静默吃掉，
# 而静默错分成宽松许可证 = 一次假绿。`--selftest` 就是为了让那种改动立刻变红。
classify() {
    local f="$1" head_txt
    head_txt="$(head -c 4000 "$f" 2>/dev/null)"
    case "$head_txt" in
        *"GNU AFFERO GENERAL PUBLIC"*) echo "AGPL-3.0" ;;
        *"GNU LESSER GENERAL PUBLIC"*|*"GNU LIBRARY GENERAL PUBLIC"*) echo "LGPL" ;;
        *"GNU GENERAL PUBLIC"*) echo "GPL" ;;
        *"Mozilla Public License"*) echo "MPL-2.0" ;;
        *"Apache License"*) echo "Apache-2.0" ;;
        *"MIT License"*) echo "MIT" ;;
        *"Permission is hereby granted, free of charge"*) echo "MIT" ;;
        *"Redistribution and use in source and binary forms"*) echo "BSD" ;;
        *"Permission to use, copy, modify, and"*) echo "ISC" ;;
        *) echo "UNKNOWN" ;;
    esac
}

license_file() {
    local dir="$1" f
    for f in LICENSE LICENSE.md LICENSE.txt LICENCE COPYING COPYING.LIB COPYRIGHT; do
        [ -f "$dir/$f" ] && { echo "$dir/$f"; return 0; }
    done
    return 1
}

# 单条记录：名称 | 版本 | 许可证 | 备注。强 copyleft 顺带置位全局门禁标记。
emit_row() {
    local name="$1" ver="$2" lic="$3" note="${4:-}"
    case "$lic" in
        AGPL-3.0|GPL) STRONG_COPYLEFT_FOUND=1; lic="**$lic** ⛔" ;;
        UNKNOWN) lic="UNKNOWN ⚠️" ;;
    esac
    printf '| %s | %s | %s | %s |\n' "$name" "$ver" "$lic" "$note"
}

# classify 是纯文本模式匹配，改错了不会报错、只会静默错分。这里把每条分支
# 各钉一个样本，其中 mit_bare 是**真实存在**的形态：erlware_commons 的 COPYING 和
# sync 的 LICENSE 都没有 "MIT License" 标题行，只靠授权正文才判得出来。
selftest() {
    local d fail=0 name want got
    d="$(mktemp -d)" || return 1
    printf 'GNU AFFERO GENERAL PUBLIC LICENSE\nVersion 3\n'                  > "$d/agpl"
    printf 'GNU LESSER GENERAL PUBLIC LICENSE\nVersion 2.1\n'                > "$d/lgpl"
    printf 'GNU GENERAL PUBLIC LICENSE\nVersion 3\n'                         > "$d/gpl"
    printf 'MIT License\n\nCopyright\n'                                      > "$d/mit"
    printf 'Copyright (c) 2011 Erlware, LLC\n\nPermission is hereby granted, free of charge, to any person\n' > "$d/mit_bare"
    printf 'Redistribution and use in source and binary forms\nDISCLAIMER\n' > "$d/bsd"
    printf 'ISC License\n\nPermission to use, copy, modify, and/or dist\n'   > "$d/isc"
    printf 'Mozilla Public License Version 2.0\n'                            > "$d/mpl"
    printf 'Apache License\nVersion 2.0\n'                                   > "$d/apache"
    printf 'Copyright 2021'                                                  > "$d/none"
    for pair in agpl:AGPL-3.0 lgpl:LGPL gpl:GPL mit:MIT mit_bare:MIT bsd:BSD isc:ISC \
                mpl:MPL-2.0 apache:Apache-2.0 none:UNKNOWN; do
        name="${pair%%:*}"; want="${pair##*:}"; got="$(classify "$d/$name")"
        if [ "$got" = "$want" ]; then
            echo "ok   $name -> $got"
        else
            echo "FAIL $name -> got=$got want=$want" >&2; fail=1
        fi
    done
    rm -rf "$d"
    return "$fail"
}

scan_erlang() {
    local d name lf lic ver note
    for d in deps/*/; do
        [ -d "$d" ] || continue
        name="$(basename "$d")"
        ver="$(sed -n 's/.*{vsn, *"\([^"]*\)".*/\1/p' "$d"src/"$name".app.src 2>/dev/null | head -1)"
        note=""
        if lf="$(license_file "$d")"; then
            lic="$(classify "$lf")"
            # gpb 的 COPYING.LIB 是 LGPL-2.1，但正文开头写死了链接例外
            # （"ok to link this library with code covered by other licenses"），
            # 且明确不覆盖 gpb 生成的代码——我们只用它编 protobuf，属例外范围内。
            [ "$name" = "gpb" ] && note="LGPL-2.1 + 链接例外；生成代码不受其约束"
        else
            # 无 LICENSE 正文时退回 app.src 的 {licenses,[...]} 元数据。
            # 元数据比正文弱（可以填错、也不构成授权），所以必须显式标注来源，
            # 不能让它冒充成"已核实"。
            lic="$(sed -n 's/.*{licenses, *\["\([^"]*\)".*/\1/p' "$d"src/"$name".app.src 2>/dev/null | head -1)"
            if [ -n "$lic" ]; then
                case "$lic" in
                    *AGPL*) lic="AGPL-3.0" ;;
                    *LGPL*) lic="LGPL" ;;
                    *GPL*) lic="GPL" ;;
                esac
                note="仅 app.src 元数据声明，仓内无许可证正文——须向上游补回 LICENSE"
            else
                lic="UNKNOWN"
                note="既无 LICENSE 正文也无元数据声明，未获授权不得随产品分发"
            fi
        fi
        emit_row "$name" "${ver:-?}" "$lic" "$note"
    done
}

scan_flutter() {
    local lock="../imboyapp/pubspec.lock" name ver src pth ref dir lf base
    if [ ! -f "$lock" ]; then
        echo "| _(跳过)_ | | | 未并排 checkout imboyapp，Flutter 侧未扫描 |"
        return
    fi
    # 只取 direct main（direct dev 不随产品分发给最终用户）。
    # 必须连 source 一起取：仓里 path/git/sdk 三种来源都有，只查 hosted 缓存
    # 会把 20 个包一律记成 UNKNOWN——那种清单看着像扫全了，其实是漏报。
    awk '
        /^  [a-z0-9_]+:$/ { pkg=$1; sub(/:$/,"",pkg); src=""; pth=""; ref="" }
        /dependency: "direct main"/ { want[pkg]=1 }
        /^      path: /          { pth=$2; gsub(/"/,"",pth) }
        /^      resolved-ref: /  { ref=$2; gsub(/"/,"",ref) }
        /^    source: /          { src=$2; gsub(/"/,"",src) }
        /^    version: / {
            if (want[pkg]) { v=$2; gsub(/"/,"",v); print pkg"\t"v"\t"src"\t"pth"\t"ref }
        }
    ' "$lock" | sort | while IFS="$(printf '\t')" read -r name ver src pth ref; do
        dir=""
        case "$src" in
            sdk)
                # Flutter/Dart SDK 自身，随 SDK 分发，BSD-3-Clause
                emit_row "$name" "$ver" "BSD" "Flutter SDK 内置"
                continue
                ;;
            path)
                dir="../imboyapp/$pth"
                ;;
            git)
                # git 缓存目录名用的是仓库名不是包名（IcStorageSpace vs ic_storage_space），
                # 只能拿 resolved-ref 反查——ref 全局唯一，够用。
                for base in "$HOME/.pub-cache/git/"*-"$ref"; do
                    [ -d "$base" ] && { dir="$base"; break; }
                done
                ;;
            *)
                for base in "$HOME/.pub-cache/hosted"/*; do
                    [ -d "$base/$name-$ver" ] && { dir="$base/$name-$ver"; break; }
                done
                ;;
        esac
        if [ -z "$dir" ] || [ ! -d "$dir" ]; then
            emit_row "$name" "$ver" "UNKNOWN" "本机未找到 $src 源副本，未能读取正文"
        elif lf="$(license_file "$dir")"; then
            case "$src" in
                path) emit_row "$name" "$ver" "$(classify "$lf")" "vendored: $pth" ;;
                git)  emit_row "$name" "$ver" "$(classify "$lf")" "git fork" ;;
                *)    emit_row "$name" "$ver" "$(classify "$lf")" "" ;;
            esac
        else
            emit_row "$name" "$ver" "UNKNOWN" "包内无 LICENSE 文件（$src）"
        fi
    done
}

if [ "$MODE" = "--selftest" ]; then
    selftest && { echo "✅ classify 自检 10/10"; exit 0; } || { echo "❌ classify 自检失败" >&2; exit 1; }
fi

REPORT="$(
    echo "| 依赖 | 版本 | 许可证 | 备注 |"
    echo "|------|------|--------|------|"
    scan_erlang
)"
FLUTTER_REPORT="$(
    echo "| 依赖 | 版本 | 许可证 | 备注 |"
    echo "|------|------|--------|------|"
    scan_flutter
)"

# scan_flutter 跑在 while 的子 shell 里，置位的 STRONG_COPYLEFT_FOUND 传不回来，
# 只能回读输出里的 ⛔ 标记。
case "$FLUTTER_REPORT$REPORT" in *"⛔"*) STRONG_COPYLEFT_FOUND=1 ;; esac

if [ "$MODE" = "--check" ]; then
    if [ "$STRONG_COPYLEFT_FOUND" -eq 1 ]; then
        printf '%s\n%s\n' "$REPORT" "$FLUTTER_REPORT" | grep -E '⛔' >&2
        echo "❌ 发现强 copyleft（AGPL/GPL）依赖：分发前必须替换或取得例外授权。" >&2
        exit 1
    fi
    echo "✅ 未发现强 copyleft 依赖"
    exit 0
fi

echo "## Erlang 后端运行时依赖（imboy/deps）"
echo
echo "$REPORT"
echo
echo "## Flutter 客户端直接依赖（imboyapp，direct main）"
echo
echo "$FLUTTER_REPORT"
