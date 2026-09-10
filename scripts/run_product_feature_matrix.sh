#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORKSPACE="$(cd "$ROOT/.." && pwd)"
PRESET="${1:-}"
EVIDENCE_DIR="${FEATURE_EVIDENCE_DIR:-$ROOT/docs/compliance/feature-composition-evidence}"

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

restore_generated() {
  local status=$?
  local file rel restore_failed=0
  trap - EXIT
  set +e
  for file in "${GENERATED_FILES[@]}"; do
    rel="${file#/}"
    cp -p "$SNAPSHOT_DIR/$rel" "$file" || restore_failed=1
  done
  rm -rf "$SNAPSHOT_DIR"
  if [[ "$status" -eq 0 && "$restore_failed" -ne 0 ]]; then
    status=74
  fi
  exit "$status"
}

trap restore_generated EXIT
for file in "${GENERATED_FILES[@]}"; do
  [[ -f "$file" ]] || { echo "missing generated file: $file" >&2; exit 2; }
  rel="${file#/}"
  mkdir -p "$SNAPSHOT_DIR/$(dirname "$rel")"
  cp -p "$file" "$SNAPSHOT_DIR/$rel"
done

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
  R=android/app/src/main/java/io/flutter/plugins/GeneratedPluginRegistrant.java && \
  if [ -f "$R" ]; then \
    sed -i '' -E '/integration_test|PatrolPlugin/d' "$R"; \
  fi && \
  flutter --suppress-analytics build apk --release --target-platform android-arm64 --no-pub)
(cd "$WORKSPACE/imboyadmin" && bun run build)

BACKEND_BEAM="$(find "$ROOT/_rel/imboy/lib" -path '*/ebin/imboy_feature.beam' -print | sort | tail -1)"
python3 "$ROOT/scripts/verify_product_feature_artifacts.py" \
  --manifest "$MANIFEST" \
  --backend-beam "$BACKEND_BEAM" \
  --flutter-apk "$WORKSPACE/imboyapp/build/app/outputs/flutter-apk/app-release.apk" \
  --admin-dist "$WORKSPACE/imboyadmin/dist" \
  --output "$EVIDENCE_DIR/$PRESET.json"
