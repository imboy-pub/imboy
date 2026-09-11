#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT/scripts/run_product_feature_matrix.sh"

rg -Fq 'flutter --suppress-analytics pub get --offline' "$SCRIPT"
rg -Fq 'plugin for plugin in android_plugins if not plugin.get("dev_dependency", False)' "$SCRIPT"
rg -Fq 'flutter --suppress-analytics build apk --release --target-platform android-arm64 --no-pub' "$SCRIPT"

tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/imboy-feature-matrix-test.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT
workspace="$tmp_dir/workspace"
backend="$workspace/imboy"
app="$workspace/imboyapp"
admin="$workspace/imboyadmin"
fake_bin="$tmp_dir/bin"
test_tmp="$tmp_dir/tmp"
metadata="$app/.flutter-plugins-dependencies"
lock_dir="$test_tmp/imboy-product-feature-matrix.$(id -u).lock"

generated_files=(
  "$backend/include/generated/imboy_product_features.hrl"
  "$backend/include/generated/imboy_product_features_erlc.mk"
  "$app/lib/app_core/feature_flags/generated_product_features.dart"
  "$app/lib/config/router/generated_product_feature_routes.dart"
  "$app/lib/app_core/feature_flags/generated_channel_order_widget.dart"
  "$admin/src/generated/productFeatures.ts"
  "$admin/src/generated/generatedFeatureComposition.tsx"
  "$app/android/app/product-features.properties"
  "$app/android/app/src/debug/AndroidManifest.xml"
  "$app/android/app/src/profile/AndroidManifest.xml"
  "$app/android/app/src/release/AndroidManifest.xml"
)

mkdir -p "$backend/scripts" "$backend/test/fixtures/product_features" \
  "$backend/_rel/imboy/lib/imboy-1/ebin" "$fake_bin" "$test_tmp"
cp "$SCRIPT" "$backend/scripts/run_product_feature_matrix.sh"
printf '{}\n' >"$backend/test/fixtures/product_features/base-only.json"
printf 'beam\n' >"$backend/_rel/imboy/lib/imboy-1/ebin/imboy_feature.beam"
for file in "${generated_files[@]}"; do
  mkdir -p "$(dirname "$file")"
  printf 'original:%s\n' "${file#$workspace/}" >"$file"
done

cat >"$backend/scripts/generate_product_features.py" <<'PY'
from pathlib import Path

root = Path(__file__).resolve().parents[1]
workspace = root.parent
paths = [
    root / "include/generated/imboy_product_features.hrl",
    root / "include/generated/imboy_product_features_erlc.mk",
    workspace / "imboyapp/lib/app_core/feature_flags/generated_product_features.dart",
    workspace / "imboyapp/lib/config/router/generated_product_feature_routes.dart",
    workspace / "imboyapp/lib/app_core/feature_flags/generated_channel_order_widget.dart",
    workspace / "imboyadmin/src/generated/productFeatures.ts",
    workspace / "imboyadmin/src/generated/generatedFeatureComposition.tsx",
    workspace / "imboyapp/android/app/product-features.properties",
    workspace / "imboyapp/android/app/src/debug/AndroidManifest.xml",
    workspace / "imboyapp/android/app/src/profile/AndroidManifest.xml",
    workspace / "imboyapp/android/app/src/release/AndroidManifest.xml",
]
for path in paths:
    path.write_text("generated\n", encoding="utf-8")
PY
cat >"$backend/scripts/verify_product_feature_artifacts.py" <<'PY'
print("stub artifact verification")
PY
cat >"$fake_bin/make" <<'SH'
#!/usr/bin/env bash
exit "${FAKE_MAKE_STATUS:-0}"
SH
cat >"$fake_bin/bun" <<'SH'
#!/usr/bin/env bash
mkdir -p dist
printf 'admin-dist\n' >dist/index.html
SH
cat >"$fake_bin/flutter" <<'SH'
#!/usr/bin/env bash
case " $* " in
  *" pub get --offline "*)
    cat >.flutter-plugins-dependencies <<'JSON'
{"plugins":{"android":[{"name":"prod_plugin","dev_dependency":false},{"name":"integration_test","dev_dependency":true},{"name":"patrol","dev_dependency":true}]}}
JSON
    ;;
  *" build apk --release "*)
    python3 - .flutter-plugins-dependencies <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as handle:
    plugins = json.load(handle)["plugins"]["android"]
assert [plugin["name"] for plugin in plugins] == ["prod_plugin"]
PY
    mkdir -p build/app/outputs/flutter-apk
    printf 'apk\n' >build/app/outputs/flutter-apk/app-release.apk
    printf 'called\n' >"$FAKE_BUILD_MARKER"
    ;;
  *)
    echo "unexpected flutter command: $*" >&2
    exit 64
    ;;
esac
SH
chmod +x "$fake_bin/make" "$fake_bin/bun" "$fake_bin/flutter"

fingerprint_generated() {
  python3 - "${generated_files[@]}" <<'PY'
import hashlib
import os
import stat
import sys

fingerprint = hashlib.sha256()
for name in sys.argv[1:]:
    mode = stat.S_IMODE(os.stat(name).st_mode)
    fingerprint.update(f"{mode:o} {name}\n".encode())
    with open(name, "rb") as handle:
        fingerprint.update(hashlib.sha256(handle.read()).digest())
print(fingerprint.hexdigest())
PY
}

file_sha256() {
  python3 - "$1" <<'PY'
import hashlib
import sys

with open(sys.argv[1], "rb") as handle:
    print(hashlib.sha256(handle.read()).hexdigest())
PY
}

file_mode() {
  python3 - "$1" <<'PY'
import os
import stat
import sys

print(f"{stat.S_IMODE(os.stat(sys.argv[1]).st_mode):o}")
PY
}

run_matrix() {
  PATH="$fake_bin:$PATH" \
    TMPDIR="$test_tmp" \
    FEATURE_EVIDENCE_DIR="$tmp_dir/evidence" \
    FAKE_BUILD_MARKER="$tmp_dir/build-called" \
    FAKE_MAKE_STATUS="${FAKE_MAKE_STATUS:-0}" \
    bash "$backend/scripts/run_product_feature_matrix.sh" base-only >/dev/null
}

# Existing metadata is filtered for the release build and restored byte-for-byte.
printf 'original metadata\n' >"$metadata"
chmod 600 "$metadata"
before_generated="$(fingerprint_generated)"
before_metadata="$(file_sha256 "$metadata")"
run_matrix
[[ -f "$tmp_dir/build-called" ]]
[[ "$before_generated" == "$(fingerprint_generated)" ]]
[[ "$before_metadata" == "$(file_sha256 "$metadata")" ]]
[[ "$(file_mode "$metadata")" == "600" ]]
[[ ! -d "$lock_dir" ]]

# Metadata generated from an initially clean workspace is removed on exit.
rm -f "$metadata" "$tmp_dir/build-called"
run_matrix
[[ -f "$tmp_dir/build-called" ]]
[[ ! -e "$metadata" ]]
[[ "$before_generated" == "$(fingerprint_generated)" ]]

# A downstream failure still restores both generated files and metadata.
printf 'failure metadata\n' >"$metadata"
before_metadata="$(file_sha256 "$metadata")"
set +e
FAKE_MAKE_STATUS=73 run_matrix
status=$?
set -e
[[ "$status" -eq 73 ]]
[[ "$before_metadata" == "$(file_sha256 "$metadata")" ]]
[[ "$before_generated" == "$(fingerprint_generated)" ]]

# Snapshot setup failures release the lock for the next run.
missing_generated="${generated_files[0]}"
mv "$missing_generated" "$tmp_dir/missing-generated"
set +e
run_matrix
status=$?
set -e
[[ "$status" -eq 2 ]]
[[ ! -d "$lock_dir" ]]
mv "$tmp_dir/missing-generated" "$missing_generated"
[[ "$before_generated" == "$(fingerprint_generated)" ]]

# Concurrent matrices fail closed before mutating the workspace.
mkdir "$lock_dir"
set +e
run_matrix
status=$?
set -e
[[ "$status" -eq 75 ]]
[[ "$before_metadata" == "$(file_sha256 "$metadata")" ]]
[[ "$before_generated" == "$(fingerprint_generated)" ]]
rmdir "$lock_dir"
