#!/usr/bin/env bash
# Export a committed, explicitly listed community installer. Never read customer .env/data.
set -euo pipefail
export LC_ALL=C TZ=UTC
REPO="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }
usage() {
    printf '%s\n' \
        'Usage: bash scripts/package_community.sh --output FILE.zip [--ref HEAD]' \
        'Export only committed files listed in deploy/standalone/files.tsv.' \
        'No images, deployment, customer credentials or working-tree changes are included.'
}
ref=HEAD
output=
while (($#)); do
    case "$1" in
        --ref|--output)
            (($# >= 2)) || fail "$1 requires a value"
            if [[ "$1" == --ref ]]; then ref=$2; else output=$2; fi
            shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) fail "Unknown option: $1" ;;
    esac
done
[[ -n "$output" && "$output" == *.zip ]] || fail '--output FILE.zip is required'
for tool in git zip sha256sum stat find sort touch realpath mktemp; do
    command -v "$tool" >/dev/null 2>&1 || fail "Required build tool: $tool"
done
commit=$(git -C "$REPO" rev-parse --verify --end-of-options "${ref}^{commit}") \
    || fail "Not a local commit: $ref"
[[ ! -e "$output" && ! -L "$output" ]] || fail "Output already exists: $output"
output=$(realpath -m -- "$output")
output_dir=$(dirname -- "$output")
mkdir -p -- "$output_dir"
# Build on the destination filesystem so a hard link can publish without overwriting.
work=$(mktemp -d "$output_dir/.imboy-package.XXXXXX")
trap 'rm -rf -- "$work"' EXIT
manifest=deploy/standalone/files.tsv
git -C "$REPO" show "$commit:$manifest" > "$work/files.tsv" \
    || fail "The selected commit has no $manifest"
version=$(git -C "$REPO" show "$commit:VERSION")
[[ "$version" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]] || fail 'VERSION is not a safe package identifier'
name="IMBoy_Community_Standalone_${version}_${commit:0:12}"
package="$work/$name"
mkdir -p -- "$package/review"
printf '%s\n' "$commit" > "$package/SOURCE_COMMIT"
printf 'source_path\tgit_blob_sha1\tpackage_path\n' > "$package/review/SOURCE_FILES.tsv"

valid_path() {
    [[ "$1" =~ ^[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)*$ ]] || return 1
    case "/$1/" in */../*|*/./*) return 1 ;; esac
}
declare -A destinations=()
file_count=0
while IFS=$'\t' read -r source target extra || [[ -n "${source:-}" ]]; do
    [[ -n "$source" && "$source" != \#* ]] || continue
    [[ -z "$extra" ]] && valid_path "$source" && valid_path "$target" \
        || fail 'Invalid source/target in files.tsv (expected two relative path columns)'
    case "/$target" in
        */.env|*/.git|*/.git/*|*/data/*|*.pem|*.key)
            fail "Runtime credentials/data cannot be packaged: $target" ;;
    esac
    case "$target" in
        SOURCE_COMMIT|MANIFEST.sha256|FILE_MODES.tsv|review/SOURCE_FILES.tsv|review/SOURCE_MIGRATIONS.tsv)
            fail "Reserved generated path: $target" ;;
    esac
    [[ -z "${destinations[$target]+present}" ]] || fail "Duplicate package path: $target"
    destinations[$target]=1
    entry=$(git -C "$REPO" ls-tree "$commit" -- "$source")
    IFS=$' \t' read -r mode type blob tree_path <<< "$entry"
    [[ "$tree_path" == "$source" && "$type" == blob && ( "$mode" == 100644 || "$mode" == 100755 ) ]] \
        || fail "Missing or non-regular committed dependency: $source"
    mkdir -p -- "$package/$(dirname -- "$target")"
    git -C "$REPO" show "$commit:$source" > "$package/$target"
    if [[ "$target" == *.sh ]]; then chmod 755 -- "$package/$target"; else chmod 644 -- "$package/$target"; fi
    printf '%s\t%s\t%s\n' "$source" "$blob" "$target" >> "$package/review/SOURCE_FILES.tsv"
    file_count=$((file_count + 1))
done < "$work/files.tsv"
((file_count > 0)) || fail 'Empty delivery file list'

# Record migration identities without shipping application source or SQL.
printf 'source_path\tgit_blob_sha1\n' > "$package/review/SOURCE_MIGRATIONS.tsv"
git -C "$REPO" ls-tree -r "$commit" -- priv/migrations > "$work/migrations.tsv"
migration_count=0
while IFS=$' \t' read -r mode type blob path; do
    [[ "$path" == *.up.sql ]] || continue
    [[ "$type" == blob && "$mode" == 100644 ]] || fail "Invalid migration source: $path"
    printf '%s\t%s\n' "$path" "$blob" >> "$package/review/SOURCE_MIGRATIONS.tsv"
    migration_count=$((migration_count + 1))
done < "$work/migrations.tsv"
((migration_count > 0)) || fail 'No committed up migrations found for the source manifest'

# records/ is deliberately mutable. Everything else is checked before installation.
touch "$package/FILE_MODES.tsv" "$package/MANIFEST.sha256"
chmod 644 "$package/SOURCE_COMMIT" "$package/review/"*.tsv \
    "$package/FILE_MODES.tsv" "$package/MANIFEST.sha256"
(
    cd "$package"
    find . -type f -printf '%P\n' | sort > "$work/package-files.txt"
    while IFS= read -r path; do
        [[ "$path" != records/* ]] || continue
        printf '%s\t%s\n' "$(stat -c '%a' -- "$path")" "$path"
    done < "$work/package-files.txt" > FILE_MODES.tsv
    while IFS= read -r path; do
        [[ "$path" != MANIFEST.sha256 && "$path" != records/* ]] || continue
        sha256sum -- "$path"
    done < "$work/package-files.txt" > MANIFEST.sha256
)
bash "$package/scripts/check_delivery.sh"

# ZIP's DOS timestamp range starts in 1980; timestamps and entry order are fixed.
stamp=$(git -C "$REPO" show -s --format=%ct "$commit")
((stamp >= 315532800)) || stamp=315532800
((stamp <= 4354819198)) || stamp=4354819198
find "$package" -exec touch -h -d "@$stamp" -- {} +
(
    cd "$work"
    find "$name" -type f -print | sort | zip -X -q "$work/package.zip" -@
)
ln -T -- "$work/package.zip" "$output" || fail "Cannot create output without overwriting: $output"
printf '[PASS] Exported %s source files and %s migration identities from %s\n' "$file_count" "$migration_count" "$commit"
sha256sum -- "$output"
printf '[INFO] No Docker, TLS, database or business acceptance was run.\n'
