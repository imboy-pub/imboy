#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORKSPACE="$(cd "$ROOT/.." && pwd)"
PRESET="${1:-}"
EVIDENCE_DIR="${FEATURE_EVIDENCE_DIR:-$ROOT/docs/compliance/feature-composition-evidence}"

case "$PRESET" in
  base-only) MANIFEST="$ROOT/test/fixtures/product_features/base-only.json" ;;
  full-selected) MANIFEST="$ROOT/config/product-feature-manifest.json" ;;
  overseas_baseline)
    MANIFEST="$ROOT/config/product-feature-manifests/overseas_baseline.json"
    [ -f "$MANIFEST" ] || { echo "overseas_baseline preset is not available yet" >&2; exit 2; }
    ;;
  *) echo "usage: $0 {base-only|full-selected|overseas_baseline}" >&2; exit 2 ;;
esac

restore_canonical() {
  python3 "$ROOT/scripts/generate_product_features.py" >/dev/null
}
trap restore_canonical EXIT

python3 "$ROOT/scripts/generate_product_features.py" --manifest "$MANIFEST"

make -C "$ROOT" eunit t=feature_route_http_tests
make -C "$ROOT" rel
(cd "$WORKSPACE/imboyapp" && \
  rm -f android/app/src/main/java/io/flutter/plugins/GeneratedPluginRegistrant.java && \
  flutter --suppress-analytics pub get && \
  flutter --suppress-analytics build apk --release --target-platform android-arm64)
(cd "$WORKSPACE/imboyadmin" && bun run build)

BACKEND_BEAM="$(find "$ROOT/_rel/imboy/lib" -path '*/ebin/imboy_feature.beam' -print | sort | tail -1)"
python3 "$ROOT/scripts/verify_product_feature_artifacts.py" \
  --manifest "$MANIFEST" \
  --backend-beam "$BACKEND_BEAM" \
  --flutter-apk "$WORKSPACE/imboyapp/build/app/outputs/flutter-apk/app-release.apk" \
  --admin-dist "$WORKSPACE/imboyadmin/dist" \
  --output "$EVIDENCE_DIR/$PRESET.json"
