#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORKSPACE="$(cd "$ROOT/.." && pwd)"
PRESET="${1:-}"
EVIDENCE_DIR="${FEATURE_EVIDENCE_DIR:-$ROOT/docs/compliance/feature-composition-evidence}"
FLUTTER_PLUGIN_METADATA="$WORKSPACE/imboyapp/.flutter-plugins-dependencies"
FLUTTER_PLUGIN_METADATA_EXISTED=0
SNAPSHOT_READY=0
LOCK_DIR="${TMPDIR:-/tmp}/imboy-product-feature-matrix.$(id -u).lock"

GENERATED_FILES=(
  "$ROOT/include/generated/imboy_product_features.hrl"
  "$ROOT/include/generated/imboy_product_features_erlc.mk"
  "$WORKSPACE/imboyapp/lib/app_core/feature_flags/generated_product_features.dart"
  "$WORKSPACE/imboyapp/lib/config/router/generated_product_feature_routes.dart"
  "$WORKSPACE/imboyapp/lib/app_core/feature_flags/generated_channel_order_widget.dart"
  "$WORKSPACE/imboyadmin/src/generated/productFeatures.ts"
  "$WORKSPACE/imboyadmin/src/generated/generatedFeatureComposition.tsx"
  "$WORKSPACE/imboyapp/android/app/product-features.properties"
  "$WORKSPACE/imboyapp/android/app/src/debug/AndroidManifest.xml"
  "$WORKSPACE/imboyapp/android/app/src/profile/AndroidManifest.xml"
  "$WORKSPACE/imboyapp/android/app/src/release/AndroidManifest.xml"
)

SNAPSHOT_DIR="$(mktemp -d "${TMPDIR:-/tmp}/imboy-feature-matrix.XXXXXX")"

if ! mkdir "$LOCK_DIR" 2>/dev/null; then
  echo "product feature matrix already running or stale lock exists: $LOCK_DIR" >&2
  rmdir "$SNAPSHOT_DIR"
  exit 75
fi

restore_generated() {
  local status=$?
  local file rel restore_failed=0
  trap - EXIT
  if [[ "$SNAPSHOT_READY" -ne 1 ]]; then
    rm -rf "$SNAPSHOT_DIR"
    rmdir "$LOCK_DIR" || status=74
    exit "$status"
  fi
  set +e
  for file in "${GENERATED_FILES[@]}"; do
    rel="${file#/}"
    cp -p "$SNAPSHOT_DIR/$rel" "$file" || restore_failed=1
  done
  rel="${FLUTTER_PLUGIN_METADATA#/}"
  if [[ "$FLUTTER_PLUGIN_METADATA_EXISTED" -eq 1 ]]; then
    cp -p "$SNAPSHOT_DIR/$rel" "$FLUTTER_PLUGIN_METADATA" || restore_failed=1
  else
    rm -f "$FLUTTER_PLUGIN_METADATA" || restore_failed=1
  fi
  if [[ "$restore_failed" -ne 0 ]]; then
    echo "generated-file restore failed; snapshot retained: $SNAPSHOT_DIR" >&2
    status=74
  else
    rm -rf "$SNAPSHOT_DIR"
  fi
  rmdir "$LOCK_DIR" || status=74
  exit "$status"
}

trap restore_generated EXIT
for file in "${GENERATED_FILES[@]}"; do
  [[ -f "$file" ]] || { echo "missing generated file: $file" >&2; exit 2; }
  rel="${file#/}"
  mkdir -p "$SNAPSHOT_DIR/$(dirname "$rel")"
  cp -p "$file" "$SNAPSHOT_DIR/$rel"
done
if [[ -f "$FLUTTER_PLUGIN_METADATA" ]]; then
  rel="${FLUTTER_PLUGIN_METADATA#/}"
  mkdir -p "$SNAPSHOT_DIR/$(dirname "$rel")"
  cp -p "$FLUTTER_PLUGIN_METADATA" "$SNAPSHOT_DIR/$rel"
  FLUTTER_PLUGIN_METADATA_EXISTED=1
fi
SNAPSHOT_READY=1

case "$PRESET" in
  base-only) MANIFEST="$ROOT/test/fixtures/product_features/base-only.json" ;;
  full-selected) MANIFEST="$ROOT/config/product-feature-manifest.json" ;;
  overseas_baseline)
    MANIFEST="$ROOT/config/product-feature-manifests/overseas_baseline.json"
    [ -f "$MANIFEST" ] || { echo "overseas_baseline preset is not available yet" >&2; exit 2; }
    ;;
  agent_hub)
    MANIFEST="$ROOT/config/product-feature-manifests/agent_hub.json"
    [ -f "$MANIFEST" ] || { echo "agent_hub preset is not available yet" >&2; exit 2; }
    ;;
  *) echo "usage: $0 {base-only|full-selected|overseas_baseline|agent_hub}" >&2; exit 2 ;;
esac

python3 "$ROOT/scripts/generate_product_features.py" --manifest "$MANIFEST"

make -C "$ROOT" eunit t=feature_route_http_tests
make -C "$ROOT" rel
(cd "$WORKSPACE/imboyapp" && \
  rm -f android/app/src/main/java/io/flutter/plugins/GeneratedPluginRegistrant.java && \
  flutter --suppress-analytics pub get --offline && \
  python3 - .flutter-plugins-dependencies <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
metadata = json.loads(path.read_text(encoding="utf-8"))
android_plugins = metadata["plugins"]["android"]
metadata["plugins"]["android"] = [
    plugin for plugin in android_plugins if not plugin.get("dev_dependency", False)
]
path.write_text(json.dumps(metadata, separators=(",", ":")) + "\n", encoding="utf-8")
PY
  flutter --suppress-analytics build apk --release --target-platform android-arm64 --no-pub)
(cd "$WORKSPACE/imboyadmin" && bun run build)

BACKEND_BEAM="$(find "$ROOT/_rel/imboy/lib" -path '*/ebin/imboy_feature.beam' -print | sort | tail -1)"
python3 "$ROOT/scripts/verify_product_feature_artifacts.py" \
  --manifest "$MANIFEST" \
  --backend-beam "$BACKEND_BEAM" \
  --flutter-apk "$WORKSPACE/imboyapp/build/app/outputs/flutter-apk/app-release.apk" \
  --admin-dist "$WORKSPACE/imboyadmin/dist" \
  --output "$EVIDENCE_DIR/$PRESET.json"
