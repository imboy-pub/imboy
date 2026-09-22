#!/usr/bin/env python3
"""跨仓特性产物一致性门（GZAPP-C1 回归守卫）。

## 为什么需要它

`config/product-feature-manifest.json` 经 `scripts/generate_product_features.py`
生成 11 份产物、分落三仓（imboy 2 份 / imboyapp 7 份 / imboyadmin 2 份）。
每份产物内嵌 `manifest_hash` 与 `compiled_features`。

本轮复核抓到过一次真实漂移：三仓 main 同为 `9f5bc3e8`，但 APP/Admin 侧产物
仍是旧哈希 —— APP 侧因此整体失配（`ensureBuildCompatible` 判定不一致，
特性开关全数误判）。生成器本身能发现这种漂移（每个产物路径都在 `render()`
里），但**没有人跑它**，所以漂移一直挂着。

## 这个脚本做什么

把生成器的 `--check` 作为**唯一真源**调用（不重复实现比对逻辑，避免两套
规则各自漂移），并额外提供：

* 逐仓可见的输出（哪些仓在本次核对范围内）；
* 兄弟仓缺失时的**显式 SKIPPED**（不是 PASS）——单仓 clone / 三仓不同步的
  环境里，"没核对"必须与"核对通过"在输出上可区分，避免假绿；
* 非零退出只在真漂移时出现（缺仓是 SKIPPED，退出码 3）。

## 用法

    python3 scripts/check_product_feature_cross_repo.py \
        [--app-dir ../imboyapp] [--admin-dir ../imboyadmin]

退出码：0 = 三仓一致；1 = 检测到漂移；3 = 兄弟仓缺失（SKIPPED）。
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
GENERATOR = ROOT / "scripts" / "generate_product_features.py"
MANIFEST = ROOT / "config" / "product-feature-manifest.json"


def sibling_ok(path: Path, marker: Path) -> bool:
    return path.is_dir() and marker.is_file()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--app-dir", default=str(ROOT.parent / "imboyapp"))
    parser.add_argument("--admin-dir", default=str(ROOT.parent / "imboyadmin"))
    args = parser.parse_args()

    app_dir = Path(args.app_dir).resolve()
    admin_dir = Path(args.admin_dir).resolve()

    missing: list[str] = []
    if not sibling_ok(
        app_dir, app_dir / "lib" / "app_core" / "feature_flags" / "generated_product_features.dart"
    ):
        missing.append(f"imboyapp（缺少 {app_dir}/lib/app_core/feature_flags/generated_product_features.dart）")
    if not sibling_ok(admin_dir, admin_dir / "src" / "generated" / "productFeatures.ts"):
        missing.append(f"imboyadmin（缺少 {admin_dir}/src/generated/productFeatures.ts）")

    manifest_hash = json.loads(MANIFEST.read_text(encoding="utf-8"))
    del manifest_hash  # 仅确认清单可读；哈希由生成器统一重算

    if missing:
        print("跨仓特性产物一致性：SKIPPED")
        for item in missing:
            print(f"  - 兄弟仓不可用：{item}")
        print(
            "\n本次**没有核对** APP/Admin 侧产物（≠ 一致）。要真正核对，请把\n"
            "imboyapp / imboyadmin 与 imboy 放在同一父目录下，或用\n"
            "--app-dir / --admin-dir 指定路径。"
        )
        return 3

    result = subprocess.run(
        [sys.executable, str(GENERATOR), "--check"],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    stdout = (result.stdout or "").strip()
    stderr = (result.stderr or "").strip()

    if result.returncode != 0:
        print("跨仓特性产物一致性：FAIL")
        if stderr:
            print(f"  {stderr}")
        if stdout:
            print(f"  {stdout}")
        print(
            "\n修复：在 imboy 仓执行\n"
            "  python3 scripts/generate_product_features.py\n"
            "（不带 --check 即按清单重生成全部 11 份产物，含 APP/Admin 侧），\n"
            "然后分别在 imboyapp / imboyadmin 提交被改写的产物。"
        )
        return 1

    print("跨仓特性产物一致性：PASS")
    print(f"  {stdout}")
    print("  已核对三仓（11 份产物逐字节一致）：")
    print(f"    imboy      = {ROOT}")
    print(f"    imboyapp   = {app_dir}")
    print(f"    imboyadmin = {admin_dir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
