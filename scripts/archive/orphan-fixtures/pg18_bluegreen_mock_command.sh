#!/usr/bin/env bash
# Synthetic process/file model only. Never forward Docker, SQL, curl or rsync.
set -eu
cmd=${1:?}; shift
: "${MOCK_ROOT:?}" "${MOCK_SCENARIO:?}" "${MOCK_TS:?}"
case "$MOCK_ROOT" in */pg18-bg-test.*/*) ;; *) exit 97 ;; esac
model="$MOCK_ROOT/model"
events="$MOCK_ROOT/events.log"
data="$MOCK_ROOT/data"
event() { printf '%s\n' "$*" >> "$events"; }
get() { cat "$model/$1"; }
put() { printf '%s\n' "$2" > "$model/$1"; }
image_name() { case "$(get image)" in test/new:18) printf new ;; *) printf old ;; esac; }
fail() { event "fault:$*"; printf 'synthetic fault: %s\n' "$*" >&2; exit 23; }
safe_path() { case "$1" in "$MOCK_ROOT"/*) ;; *) event "UNSAFE_PATH:$1"; exit 97 ;; esac; }

case "$cmd" in
  date)
    if [ "${1:-}" = '+%Y%m%dT%H%M%S' ]; then printf '%s\n' "$MOCK_TS"; else /usr/bin/date "$@"; fi
    ;;
  stat)
    # Synthetic cleanup candidate: nine days old on either BSD/GNU stat syntax.
    safe_path "${!#}"
    printf '%s\n' "$(( $(/usr/bin/date +%s) - 9 * 86400 ))"
    ;;
  sleep) : ;;
  curl)
    event "health:$(image_name)"
    [ "$MOCK_SCENARIO" != health_failure ] || fail health
    ;;
  rsync)
    kind=precopy
    checksum=0
    for arg in "$@"; do
      [ "$arg" != --delete ] || kind=delta
      [ "$arg" != --checksum ] || checksum=1
    done
    args=("$@")
    src=${args[${#args[@]}-2]}; dst=${args[${#args[@]}-1]}
    safe_path "$src"; safe_path "$dst"
    event "rsync:$kind"
    [ "$kind" != delta ] || event "rsync:checksum=$checksum"
    if [ "$kind" = precopy ] && [ "$MOCK_SCENARIO" = all_precopy_failure ]; then fail precopy; fi
    if [ "$kind" = delta ] && [ "$MOCK_SCENARIO" = rsync_failure ]; then fail rsync; fi
    mkdir -p "$dst"
    cp -a "$src/." "$dst/"
    if [ "$kind" = delta ] && [ "$MOCK_SCENARIO" = rsync_partial_failure ]; then
      printf 'partial\n' > "$dst/sentinel"; fail rsync_partial
    fi
    if [ "$kind" = delta ] && [ "$MOCK_SCENARIO" = logger_failure ]; then
      # Feed enough output for tee to see it and fail while upstream is running.
      printf 'total size: synthetic stats\n'; exit 0
    fi
    [ "$MOCK_SCENARIO" = rsync_no_stats ] || printf 'Number of files: 1\nNumber of regular files transferred: 1\ntotal size is 9\n'
    ;;
  tee)
    if [ "$MOCK_SCENARIO" = logger_failure ]; then
      payload=$(cat)
      case "$payload" in *'synthetic stats'*) event fault:logger; exit 74 ;; esac
      printf '%s\n' "$payload" | /usr/bin/tee "$@"
    else /usr/bin/tee "$@"; fi
    ;;
  mv)
    operands=()
    for arg in "$@"; do case "$arg" in -*) ;; *) operands+=("$arg") ;; esac; done
    [ "${#operands[@]}" = 2 ] || exit 97
    src=${operands[0]}; dst=${operands[1]}
    safe_path "$src"; safe_path "$dst"
    case "$src:$dst" in
      "$data/pg18:$data/pg18."*)
        case "$dst" in
          *failed*) event mv:new_to_failed ;;
          *) event mv:old_to_anchor; [ "$MOCK_SCENARIO" != first_rename_failure ] || fail first_rename ;;
        esac ;;
      "$data/pg18_new:$data/pg18")
        event mv:new_to_canonical
        [ "$MOCK_SCENARIO" != second_rename_failure ] || fail second_rename ;;
      "$data/pg18."*:"$data/pg18")
        event mv:anchor_to_canonical
        [ "$MOCK_SCENARIO" != restore_rename_failure ] || fail restore_rename ;;
      *:"$data/pg18_upgrade.state")
        event state:publish
        [ "$MOCK_SCENARIO" != state_failure ] || fail state_publish
        if [ "$MOCK_SCENARIO" = rollback_state_publish_failure ] && grep -qx 'PHASE=rolled_back' "$src"; then
          fail rollback_state_publish
        fi
        if [ "$MOCK_SCENARIO" = second_state_failure ]; then
          publishes=$(grep -c '^state:publish$' "$events")
          if [ "$publishes" -ge 2 ]; then
            cmp -s "$data/pg18_upgrade.state" "$MOCK_ROOT/initial-state" || exit 97
            event state:prior_preserved
            fail second_state_publish
          fi
          cp "$src" "$MOCK_ROOT/initial-state"
        fi ;;
    esac
    /usr/bin/mv "$@"
    ;;
  rm|rmdir)
    # Preserve all artifacts. These shims do not delete files or directories.
    event "blocked_delete:$cmd $*"
    exit 97
    ;;
  docker)
    sub=${1:?}; shift
    case "$sub" in
      image) [ "${1:-}" = inspect ] || exit 97 ;;
      inspect)
        fmt="$*"
        case "$fmt" in
          *State.Running*)
            case "$fmt" in *backend*) [ "$(get backend)" = 1 ] && echo true || echo false ;;
              *) [ "$(get pg)" = 1 ] && echo true || echo false ;; esac ;;
          *State.ExitCode*) echo 0 ;;
          *Config.Image*) get image ;;
          *) event "UNSUPPORTED:docker inspect $*"; exit 97 ;;
        esac ;;
      rm)
        event pg:rm
        [ "$MOCK_SCENARIO" != remove_failure ] || fail remove
        put exists 0; put pg 0 ;;
      compose)
        [ "${1:-}" = -f ] || exit 97
        safe_path "$2"; shift 2
        action=${1:?}; shift
        service=${!#}
        case "$action:$service" in
          stop:backend)
            event backend:stop
            case "$MOCK_SCENARIO" in backend_stop_failure|manual_backend_stop_failure) fail backend_stop ;; esac
            put backend 0 ;;
          stop:pg)
            event pg:stop
            if [ "$MOCK_SCENARIO" = rollback_pg_stop_failure ] && [ "$(image_name)" = new ]; then fail pg_stop; fi
            if [ "$MOCK_SCENARIO" = manual_pg_stop_failure ]; then fail pg_stop; fi
            put pg 0 ;;
          start:backend)
            event "backend:start:$(image_name):pg=$(get pg):ready=$(get ready)"
            if [ "$(get pg)" != 1 ] || [ "$(get ready)" != "$(image_name)" ]; then event UNSAFE_BACKEND_START; fi
            put backend 1
            if [ "$(get pg)" = 1 ]; then
              printf '%s\n' "$(image_name)" >> "$data/pg18/writes"
              event "write:$(image_name)"
            fi ;;
          start:pg)
            event "pg:start:$(image_name)"
            [ "$(get exists)" = 1 ] || fail start_missing_container
            put pg 1 ;;
          up:pg)
            next=${PG_IMAGE:-test/old:18}
            case "$next" in test/new:18) next_name=new ;; test/old:18) next_name=old ;; *) exit 97 ;; esac
            event "pg:up:$next_name"
            if [ "$next_name" = new ] && [ "$MOCK_SCENARIO" = new_start_failure ]; then fail new_start; fi
            if [ "$next_name" = old ] && [ "$MOCK_SCENARIO" = old_start_failure ]; then fail old_start; fi
            put image "$next"; put exists 1; put pg 1; put ready none
            if [ "$next_name" = new ]; then
              printf 'NEW\n' > "$data/pg18/sentinel"
              case "$MOCK_SCENARIO" in
                baseline_missing_after_swap|baseline_empty_after_swap|baseline_invalid_after_swap)
                  baseline=""
                  for entry in "$data"/pg18_counts_*; do [ ! -f "$entry" ] || baseline=$entry; done
                  if [ -z "$baseline" ]; then
                    # Exact baseline-script temporary path generated by this
                    # synthetic invocation; verify its known fixture bytes.
                    baseline="/tmp/pgbg_counts_old.$MOCK_TS"
                  fi
                  [ "$(cat "$baseline")" = synthetic_table=7 ] || exit 97
                  cp "$baseline" "$MOCK_ROOT/baseline.retained"
                  case "$MOCK_SCENARIO" in
                    baseline_missing_after_swap) /usr/bin/mv "$baseline" "$MOCK_ROOT/baseline.moved" ;;
                    baseline_empty_after_swap) : > "$baseline" ;;
                    baseline_invalid_after_swap) printf 'synthetic_table=invalid\n' > "$baseline" ;;
                  esac
                  event "fault:$MOCK_SCENARIO"
                  ;;
              esac
            fi ;;
          ps:backend)
            [ "$(get backend)" != 1 ] || printf 'mock_backend\n' ;;
          ps:pg)
            [ "$(get pg)" != 1 ] || printf 'mock_pg\n' ;;
          *) event "UNSUPPORTED:docker compose $action $*"; exit 97 ;;
        esac ;;
      exec)
        container=${1:?}; shift
        [ "$container" = pg ] || exit 97
        operation=${1:?}; shift
        [ "$(get pg)" = 1 ] || fail query_stopped_pg
        name=$(image_name)
        case "$operation" in
          pg_isready)
            event "pg:ready:$name"
            if [ "$name" = old ] && [ "$MOCK_SCENARIO" = old_readiness_failure ]; then fail old_ready; fi
            put ready "$name" ;;
          psql)
            sql=""
            while [ $# -gt 0 ]; do
              if [ "$1" = -c ]; then sql=$2; break; fi
              shift
            done
            case "$sql" in
              *'SELECT count(*) FROM pg_tables'*)
                event "sql:table_exists:$name"
                [ "$MOCK_SCENARIO" != baseline_lookup_failure ] || fail baseline_lookup
                echo 1 ;;
              *'SELECT count(*) FROM timescaledb_information.hypertables'*)
                event "sql:hypertables:$name"
                [ "$MOCK_SCENARIO" != hyper_query_failure ] || fail hyper_query
                echo 1 ;;
              *'SELECT count(*) FROM "'* )
                event "sql:rows:$name"
                if [ "$name" = old ] && [ "$MOCK_SCENARIO" = baseline_rows_failure ]; then fail baseline_rows; fi
                echo 7 ;;
              *'SELECT extversion FROM pg_extension'*)
                event "sql:extension:$name"
                if [ "$name" = new ]; then
                  case "$MOCK_SCENARIO" in
                    verify_query_failure|verify_standalone_query_failure) fail verify_query ;;
                    verify_failure|old_start_failure|old_readiness_failure|rollback_pg_stop_failure|restore_rename_failure|rollback_state_publish_failure) echo 0; exit 0 ;;
                  esac
                fi
                echo 1 ;;
              *'SELECT default_version FROM pg_available_extensions'*) echo 1 ;;
              *'ALTER EXTENSION'*)
                event "sql:alter:$name"
                [ "$MOCK_SCENARIO" != alter_failure ] || fail alter
                echo 'ALTER EXTENSION' ;;
              *'SELECT extname FROM pg_extension'*) echo timescaledb ;;
              *'SELECT 1'*) echo 1 ;;
              *) event "UNSUPPORTED:psql:$sql"; exit 97 ;;
            esac ;;
          *) event "UNSUPPORTED:docker exec:$operation"; exit 97 ;;
        esac ;;
      logs)
        printf 'background worker launcher connected\n'
        [ "$MOCK_SCENARIO" != verify_standalone_logs_failure ] || fail logs ;;
      *) event "UNSUPPORTED:docker:$sub"; exit 97 ;;
    esac ;;
  *) event "UNSUPPORTED:$cmd"; exit 97 ;;
esac
