#!/usr/bin/env bash
# 插件清单校验 / Plugin manifest validation (P5)
# 校验 priv/plugins/*/plugin.config 能被 file:consult 解析且含必备键。
# 注意：注册中心 index.json 由 imboy-plugin-marketplace/scripts/validate_index.py
# 校验，二者互补 / Registry index.json is validated separately by the
# marketplace repo's validate_index.py; this script covers in-repo manifests.
#
# 用法 / Usage:
#   bash scripts/validate_p5_manifest.sh                 # 校验全部插件
#   bash scripts/validate_p5_manifest.sh channel moment  # 仅校验指定插件
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."
command -v escript >/dev/null || { echo "✗ 缺少 escript（erlang）" >&2; exit 1; }

if [ "$#" -gt 0 ]; then
  PLUGINS=("$@")
else
  PLUGINS=()
  for d in priv/plugins/*/; do
    PLUGINS+=("$(basename "$d")")
  done
fi

# escript 会跳过脚本文件首行（shebang 约定），故首行必须留注释占位
# escript skips the first line of the script file (shebang slot) — keep a comment there
VALIDATOR="$(mktemp -t p5_validate.XXXXXX)"
trap 'rm -f "$VALIDATOR"' EXIT
cat > "$VALIDATOR" <<'ESCRIPT'
%% p5 manifest validator (escript)
main([File, Name]) ->
    Required = [name, version, contract_version, kind, description,
                min_core_version, features, migrations, meta],
    case file:consult(File) of
        {ok, [M]} when is_map(M) ->
            Missing = [K || K <- Required, not maps:is_key(K, M)],
            NameOk = (maps:get(name, M, undefined) =:= list_to_atom(Name)),
            case {Missing, NameOk} of
                {[], true} -> io:format("ok~n");
                {[], false} -> io:format("name 字段 ~p 与目录名 ~s 不一致~n",
                                         [maps:get(name, M), Name]), halt(1);
                _ -> io:format("缺少必备键: ~p~n", [Missing]), halt(1)
            end;
        {ok, _} -> io:format("文件须为单个 map term~n"), halt(1);
        {error, E} -> io:format("解析失败: ~p~n", [E]), halt(1)
    end.
ESCRIPT

FAIL=0
for p in "${PLUGINS[@]}"; do
  f="priv/plugins/${p}/plugin.config"
  if [ ! -f "$f" ]; then
    echo "✗ ${p}: 缺少 ${f}"; FAIL=$((FAIL + 1)); continue
  fi
  # 必备键依据 docs/plugin/contract.md §3 与 imboy_plugin_loader 的消费字段
  if OUT=$(escript "$VALIDATOR" "$f" "$p" 2>&1); then
    echo "✓ ${p}"
  else
    echo "✗ ${p}: ${OUT}"; FAIL=$((FAIL + 1))
  fi
done

echo ""
if [ "$FAIL" -gt 0 ]; then
  echo "✗ ${FAIL}/${#PLUGINS[@]} 个插件清单校验失败"; exit 1
fi
echo "✓ 全部 ${#PLUGINS[@]} 个插件清单有效"
