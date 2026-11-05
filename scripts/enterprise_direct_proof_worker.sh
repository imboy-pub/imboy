#!/usr/bin/env bash
# Start under a separately provisioned OS identity with private DB/registry env.
set -euo pipefail
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
worker_script=${EB_PROOF_SCRIPT:-"$script_dir/../priv/workers/enterprise_direct_proof.escript"}
[[ -f "$worker_script" ]] || { printf 'proof worker script unavailable\n' >&2; exit 1; }
for name in EB_PROOF_PG_HOST EB_PROOF_PG_PORT EB_PROOF_PG_DB EB_PROOF_PG_USER EB_PROOF_PG_PASSWORD EB_PROOF_PG_SSL EB_PROOF_STORAGE_NAMESPACE EB_PROOF_STORAGE_ENDPOINT EB_PROOF_STORAGE_CONTAINER EB_PROOF_HEALTH_KEY EB_PROOF_STORAGE_ACCESS_KEY EB_PROOF_STORAGE_SECRET_KEY; do
    [[ -n "${!name:-}" ]] || { printf 'proof worker configuration missing: %s\n' "$name" >&2; exit 1; }
done
exec escript "$worker_script" --poll
