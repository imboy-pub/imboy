#!/usr/bin/env bash
# Consistent offline snapshot for this dedicated, single-host topology.
declare -a PREVIOUSLY_ACTIVE=()
remember_services() {
    local unit
    PREVIOUSLY_ACTIVE=()
    for unit in imboy-postgresql imboy-garage imboy-backend nginx; do
        if systemctl is-active --quiet "$unit"; then PREVIOUSLY_ACTIVE+=("$unit"); fi
    done
}
restore_service_state() {
    local unit rc=0
    for unit in "${PREVIOUSLY_ACTIVE[@]}"; do
        if [[ $unit == imboy-backend || $unit == nginx ]] && ((rc!=0)); then continue; fi
        systemctl start "$unit" || { rc=1; continue; }
        case $unit in
            imboy-postgresql) wait_postgres || rc=1;;
            imboy-garage) wait_garage || rc=1;;
            imboy-backend) wait_health || rc=1;;
            nginx) systemctl is-active --quiet nginx || rc=1;;
        esac
    done
    if ((rc!=0)); then printf 'ERROR [BACKUP_RESTART] Snapshot may be complete, but previous service readiness was not restored. Check service state before resuming traffic.\n' >&2; fi
    return "$rc"
}
stop_all_writers() {
    # Nginx first: also freezes outstanding S3 presigned PUT traffic.
    systemctl stop nginx imboy-backend.service
    systemctl stop imboy-garage.service
    systemctl stop imboy-postgresql.service
    local unit
    for unit in nginx imboy-backend imboy-garage imboy-postgresql; do
        if systemctl is-active --quiet "$unit"; then die 5 STOP "Service did not stop: $unit"; fi
    done
}
create_backup() {
    local size free work output stamp
    [[ -f $CONF_DIR/release.env && -f $CONF_DIR/secrets.env ]] || die 3 INSTALL 'Missing installed configuration.'
    # No external tablespaces or symlinked secret/data paths in this backup contract.
    [[ -z $(find "$CONF_DIR" "$DATA_DIR" -type l -print -quit) ]] || die 3 BACKUP_LINK 'External/symlinked data requires a separately validated backup contract.'
    install -d -m 700 "$BACKUP_DIR"
    size=$(du -sk "$CONF_DIR" "$DATA_DIR" | awk '{s+=$1} END{print s}')
    free=$(df -Pk "$BACKUP_DIR" | awk 'END{print $4}')
    ((free > size*2+1048576)) || die 3 BACKUP_SPACE 'Need at least twice the current data size plus 1 GiB free in the backup filesystem.'
    stamp=$(date -u +%Y%m%dT%H%M%SZ)
    output=$BACKUP_DIR/imboy-$stamp-$(openssl rand -hex 4).tar.gz
    work=$(mktemp -d "$BACKUP_DIR/.snapshot.XXXXXX")
    BACKUP_WORK=$work
    stop_all_writers
    cp -a -- "$CONF_DIR" "$work/config"
    cp -a -- "$DATA_DIR" "$work/data"
    mkdir "$work/meta"
    cp "$CONF_DIR/release.env" "$work/meta/release.env"
    printf 'FORMAT=1\nCREATED_AT=%s\nCONSISTENCY=all-four-services-stopped\n' "$stamp" > "$work/meta/backup.env"
    (cd "$work" && find config data meta -type f -print0 | sort -z | xargs -0 sha256sum > CONTENTS.sha256)
    tar --numeric-owner -czpf "$output.partial" -C "$work" config data meta CONTENTS.sha256
    chmod 600 "$output.partial"
    mv "$output.partial" "$output"
    sha256sum "$output" > "$output.sha256"
    chmod 600 "$output.sha256"
    BACKUP_RESULT=$output
    # Delete only the newly created private staging directory.
    [[ $work == "$BACKUP_DIR"/.snapshot.* && -d $work && ! -L $work ]] || die 4 BACKUP_PATH 'Unexpected staging path.'
    rm -rf --one-file-system -- "$work"
    BACKUP_WORK=
}

validate_backup_archive() {
    local archive=$1 expected=$2 member entry
    [[ -f $archive && $expected =~ ^[0-9a-f]{64}$ ]] || die 2 BACKUP_INPUT 'Supply the backup archive and its trusted SHA-256.'
    [[ $(sha "$archive") == "$expected" ]] || die 3 BACKUP_CHECKSUM 'Backup checksum mismatch.'
    # This package creates only regular files and directories. Reject links/devices.
    while IFS= read -r entry; do [[ $entry == [-d]* ]] || die 3 BACKUP_TYPE 'Backup contains a link or special file outside the supported format.'; done < <(tar -tvzf "$archive")
    while IFS= read -r member; do
        [[ $member == config/* || $member == data/* || $member == meta/* || $member == CONTENTS.sha256 ]] || die 3 BACKUP_PATH 'Unexpected archive member.'
        [[ $member != *'\'* && $member != *$'\r'* && $member != *$'\t'* ]] || die 3 BACKUP_PATH 'Unsupported control or escaped filename in backup.'
        [[ $member != *'/../'* && $member != */.. && $member != /* ]] || die 3 BACKUP_PATH 'Unsafe archive member.'
    done < <(tar -tzf "$archive")
    tar -tzf "$archive" >/dev/null || die 3 BACKUP_FORMAT 'Unreadable backup archive.'
}
