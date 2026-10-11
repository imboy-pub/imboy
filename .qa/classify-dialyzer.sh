#!/usr/bin/env bash
# DIALYZER_RESULT v1: tab-separated BEGIN, NEW/KNOWN/FIXED fingerprint,
# END new known fixed; or a single ERROR PARSE_ERROR/INFRA_FAILURE.
# Validate the whole frame before publishing any counts (no partial FIXED).
set -euo pipefail
awk -F '\t' -v gate_rc="$2" -v flaky="$3" '
/Could not resolve host|Connection reset by peer|No space left on device|has received a shutdown signal|timed out on the runner|Failed to setup toolchain|error: failed to fetch|fatal: unable to access/ { infra_hint=1 }
/^DIALYZER_RESULT/ {
    records++
    if ($1 != "DIALYZER_RESULT" || $2 != "1") { bad=1; next }
    if ($3 == "BEGIN" && NF == 3 && !begun && !ended && !err) { begun=1; next }
    if ($3 == "ERROR" && NF == 4 && records == 1 &&
        ($4 == "PARSE_ERROR" || $4 == "INFRA_FAILURE")) { err=$4; next }
    if (begun && !ended && ($3 == "NEW" || $3 == "KNOWN" || $3 == "FIXED") &&
        NF == 4 && $4 ~ /^[^|]+[|].+/ && !seen[$4]++) {
        count[$3]++; items[$3]=items[$3] "   " $4 "\n"; next
    }
    if (begun && !ended && $3 == "END" && NF == 6 &&
        $4 ~ /^[0-9]+$/ && $5 ~ /^[0-9]+$/ && $6 ~ /^[0-9]+$/ &&
        $4 == count["NEW"]+0 && $5 == count["KNOWN"]+0 && $6 == count["FIXED"]+0) {
        ended=1; next
    }
    bad=1
}
END {
    if (!records && infra_hint && gate_rc != 0) { err="INFRA_FAILURE"; records=1 }
    n=count["NEW"]+0; k=count["KNOWN"]+0; f=count["FIXED"]+0; fl=0; infra=0
    verdict="PASS"; rc=0
    if (bad || (!err && (!begun || !ended)) || (err && (records != 1 || gate_rc == 0)) ||
        (!err && ((n > 0 && gate_rc == 0) || (n == 0 && gate_rc != 0)))) {
        verdict="PARSE_ERROR"; rc=3; n=k=f=0
    } else if (err) {
        verdict=err; n=k=f=0; rc=3
        if (err == "INFRA_FAILURE") { infra=1; rc=0 }
    } else if (flaky && n > 0) {
        verdict="FLAKY"; fl=n; n=0; f=0
    } else if (n > 0) { verdict="NEW_FAILURE"; rc=1
    } else if (f > 0) { verdict="RATCHET" }
    printf "QA_CLASSIFY gate=dialyze NEW=%d KNOWN=%d FIXED=%d FLAKY=%d INFRA=%d VERDICT=%s\n", n,k,f,fl,infra,verdict
    if (n) { print "-- NEW (dialyzer.baseline fingerprints):"; printf "%s", items["NEW"] }
    if (f) { print "-- FIXED (review before tightening dialyzer.baseline):"; printf "%s", items["FIXED"] }
    exit rc
}' "$1"
