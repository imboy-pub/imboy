#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

if ! command -v rg >/dev/null 2>&1; then
  echo "error: rg is required for script/check_module_boundaries.sh" >&2
  exit 1
fi

violations=0

BOUNDARY_HANDLERS="
auth_handler.erl
passport_handler.erl
user_handler.erl
user_collect_handler.erl
user_tag_handler.erl
user_tag_relation_handler.erl
e2ee_handler.erl
report_handler.erl
feedback_handler.erl
channel_handler.erl
moment_handler.erl
group_handler.erl
group_schedule_handler.erl
group_task_handler.erl
group_vote_handler.erl
"

allowed_modules_for_handler() {
  case "$1" in
    auth_handler.erl)
      echo "auth_logic"
      ;;
    passport_handler.erl)
      echo "config_ds passport_logic token_ds user_repo user_setting_ds"
      ;;
    user_handler.erl)
      echo "auth_ds friend_ds fts_user_repo user_ds user_logic user_repo user_setting_ds"
      ;;
    user_collect_handler.erl)
      echo "auth_ds user_collect_logic user_collect_repo"
      ;;
    user_tag_handler.erl)
      echo "auth_ds user_tag_logic"
      ;;
    user_tag_relation_handler.erl)
      echo "auth_ds friend_ds user_collect_repo user_tag_relation_logic"
      ;;
    e2ee_handler.erl)
      echo "auth_ds e2ee_logic e2ee_recovery_logic"
      ;;
    report_handler.erl)
      echo "report_logic"
      ;;
    feedback_handler.erl)
      echo "auth_ds feedback_ds feedback_reply_repo feedback_repo"
      ;;
    channel_handler.erl)
      echo "channel_logic"
      ;;
    moment_handler.erl)
      echo "moment_logic"
      ;;
    group_handler.erl)
      echo "auth_ds config_ds group_ds group_logic group_member_logic group_member_repo group_random_code_repo group_repo msg_c2g_repo msg_s2c_ds user_repo"
      ;;
    group_schedule_handler.erl)
      echo "group_schedule_logic group_schedule_repo"
      ;;
    group_task_handler.erl)
      echo "group_task_logic group_task_repo"
      ;;
    group_vote_handler.erl)
      echo "group_vote_logic"
      ;;
    *)
      echo ""
      ;;
  esac
}

module_allowed_for_file() {
  local module="$1"
  local allowed_line="$2"
  local allowed
  for allowed in $allowed_line; do
    if [[ "$module" == "$allowed" ]]; then
      return 0
    fi
  done
  return 1
}

extract_modules() {
  local file="$1"
  perl -ne 's/%.*$//; print' "$file" \
      | rg -o -N '\b[a-z][a-z0-9_]*(?:_repo|_ds|_logic):' \
      | sed 's/:$//' \
      | sort -u
}

handler_count=0
for handler in $BOUNDARY_HANDLERS; do
  handler_count=$((handler_count + 1))
  file="src/api/$handler"
  if [[ ! -f "$file" ]]; then
    echo "boundary violation: expected boundary-managed handler $file to exist" >&2
    violations=1
    continue
  fi

  allowed_modules="$(allowed_modules_for_handler "$handler")"

  while IFS= read -r module; do
    [[ -z "$module" ]] && continue
    if ! module_allowed_for_file "$module" "$allowed_modules"; then
      echo "boundary violation: $handler directly references unexpected module $module"
      echo "  allowed modules: $allowed_modules"
      violations=1
    fi
  done < <(extract_modules "$file")
done

if [[ "$violations" -ne 0 ]]; then
  echo "backend module boundary check failed" >&2
  exit 1
fi

echo "backend module boundary check passed for $handler_count boundary-managed handlers"
