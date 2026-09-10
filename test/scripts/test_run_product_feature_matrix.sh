#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKSPACE="$(cd "$ROOT/.." && pwd)"
SCRIPT="$ROOT/scripts/run_product_feature_matrix.sh"
FILES=(
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

fingerprint() {
  for file in "${FILES[@]}"; do
    stat -f '%Lp %N' "$file"
    shasum -a 256 "$file"
  done | shasum -a 256 | cut -d ' ' -f 1
}

rg -Fq 'flutter --suppress-analytics pub get --offline' "$SCRIPT"
rg -Fq 'flutter --suppress-analytics build apk --release --target-platform android-arm64 --no-pub' "$SCRIPT"

tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/imboy-feature-matrix-test.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT
mkdir -p "$tmp_dir/bin"
cat >"$tmp_dir/bin/make" <<'EOF'
#!/usr/bin/env bash
exit 73
EOF
chmod +x "$tmp_dir/bin/make"

before="$(fingerprint)"
set +e
PATH="$tmp_dir/bin:$PATH" FEATURE_EVIDENCE_DIR="$tmp_dir/evidence" \
  bash "$SCRIPT" base-only >/dev/null 2>&1
status=$?
set -e
after="$(fingerprint)"

[[ "$status" -eq 73 ]]
[[ "$before" == "$after" ]]
