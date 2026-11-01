"""Pure, fail-closed checks for a proposed W6-01 readmission.

This module does not establish an identity, acquire a lock, write an event,
or authorize a launch. Trusted user and reviewer verifiers must be supplied
by the eventual caller; no verifier is provided here.
"""

import copy
import datetime
import hashlib
import json
import math
import posixpath
from itertools import product

OLD_APPLICABILITY = "bare PUT gateway/multipart仅ordinary/internal；ciphertext-only仅ordinary；其余按exact叶四入口。EB/widget使用其已有proxy PUT协议，不伪造multipart或E2EE。W6分别重跑Garage/Disk。"
NEW_APPLICABILITY = ("bare PUT gateway/multipart仅ordinary/internal；ciphertext-only仅ordinary；"
                     "其余按exact叶四入口。EB/widget使用其已有proxy PUT协议，不伪造multipart或E2EE。"
                     "W6分别重跑Garage/Disk。仅W6-01现有8个缺失叶允许使用真实enterprise "
                     "multipart/group_file生产入口，不得用gateway、SQL、mock或替代入口冒充。")
ENTERPRISE_LEAVES = [
    {"ac_id": ac, "domain": "enterprise", "driver": driver,
     "entrypoint": "pending-multipart", "source_case_ref": "W2-01/pending-multipart",
     "source_schema_phase": "expand-queue", "source_scenario": scenario}
    for ac, driver, scenario in product(("AC01", "AC02"), ("garage", "disk"),
                                        ("multipart_success", "group_file_success"))
]


def _sha(data):
    return hashlib.sha256(data).hexdigest()


def _json(data):
    def pairs(values):
        result = {}
        for key, value in values:
            if key in result:
                raise ValueError("duplicate JSON key")
            result[key] = value
        return result

    def invalid_constant(_value):
        raise ValueError("non-finite JSON value")

    return json.loads(data, object_pairs_hook=pairs, parse_constant=invalid_constant)


def _digest_json(value):
    return _sha(json.dumps(value, sort_keys=True, separators=(",", ":"),
                           ensure_ascii=False, allow_nan=False).encode("utf-8"))


def _archive(archive):
    manifest, files = archive["manifest"], archive["files"]
    if not isinstance(manifest, bytes) or not isinstance(files, dict):
        raise ValueError("invalid archive")
    entries = {}
    for line in manifest.decode("utf-8").splitlines():
        digest, name = line.split(None, 1)
        name = name.strip()
        if (len(digest) != 64 or any(c not in "0123456789abcdef" for c in digest)
                or not name or name.startswith("/") or posixpath.normpath(name) != name
                or name == ".." or name.startswith("../") or name in entries):
            raise ValueError("invalid manifest entry")
        entries[name] = digest
    if not entries or set(entries) != set(files):
        raise ValueError("archive file set differs from manifest")
    for name, digest in entries.items():
        if not isinstance(files[name], bytes) or _sha(files[name]) != digest:
            raise ValueError("archive file digest mismatch")
    contract = _json(files["execution-contract.json"])
    if not isinstance(contract, dict):
        raise ValueError("invalid contract")
    return _sha(manifest), contract


def _deadlines(state):
    return {
        "task": state["tasks"]["W6-01"]["task_deadline"],
        "wave": state["time_budgets"]["waves"]["W6"]["deadline"],
        "run": state["time_budgets"]["run"]["deadline"],
    }


def _receipt_time(value):
    if not isinstance(value, str):
        raise ValueError("receipt timestamp must be an ISO string")
    parsed = datetime.datetime.fromisoformat(value.replace("Z", "+00:00"))
    if parsed.utcoffset() is None:
        raise ValueError("receipt timestamp requires timezone")
    return parsed.timestamp()


def _validate_enterprise_leaves(contract):
    leaves = contract["expected_leaf_matrix"]["W6-01/real-pg-garage-disk"]
    if not isinstance(leaves, list):
        raise ValueError("invalid leaf matrix")
    for selector in ENTERPRISE_LEAVES:
        if sum(isinstance(leaf, dict) and all(leaf.get(k) == v for k, v in selector.items())
               for leaf in leaves) != 1:
            raise ValueError("enterprise leaf missing or ambiguous")


def validate_readmission(old_state, new_state, old_archive, new_archive,
                         old_receipt, new_receipt, review, now,
                         verify_user=None, verify_review=None):
    """Return (ok, reason); callers must independently supply trusted verifiers.

    State inputs are pre-event logical snapshots, excluding event revision and
    clock updates. The allowed contract delta is deliberately limited to the
    W6-01 retry override, one-time policy and the explicit upload applicability
    correction. Other contract edits require separately reviewed extension.
    """
    try:
        old_hash, old_contract = _archive(old_archive)
        new_hash, new_contract = _archive(new_archive)
        if not all(isinstance(value, bytes) for value in (old_receipt, new_receipt, review)):
            raise ValueError("invalid receipt bytes")
        previous = _json(old_receipt)
        receipt = _json(new_receipt)
        scope = receipt["readmission"]
        old_times, new_times = _deadlines(old_state), _deadlines(new_state)
        expected_scope = {
            "old_manifest_sha256": old_hash,
            "new_manifest_sha256": new_hash,
            "old_receipt_sha256": _sha(old_receipt),
            "task_id": "W6-01",
            "attempt_limit": 8,
            "old_deadlines": old_times,
            "new_deadlines": new_times,
            "preserve_attempts": True,
            "preserve_required": True,
            "review_sha256": _sha(review),
            "candidate_sha256": _digest_json(old_state["verification_candidate"]),
            "resources_sha256": _digest_json(old_state["resource_allowlist"]),
            "enterprise_leaf_selectors": ENTERPRISE_LEAVES,
        }
        if scope != expected_scope:
            raise ValueError("authorization scope or bindings differ")
        if (old_state.get("reviewed_readmission") is not None
                or old_state.get("active_launch") is not None
                or old_state["tasks"]["W6-01"].get("state") != "RUNNING"
                or old_state["tasks"]["W6-01"].get("attempt") != 5
                or old_state.get("plan_manifest_sha256") != old_hash
                or old_state["verification_candidate"].get("plan_manifest_sha256") != old_hash
                or old_state.get("authorization_sha256") != _sha(old_receipt)
                or previous.get("plan_manifest_sha256") != old_hash
                or receipt.get("plan_manifest_sha256") != new_hash
                or receipt.get("implementation_allowed") is not True
                or receipt.get("local_commit_allowed") is not True
                or receipt.get("allowed_tasks") != previous.get("allowed_tasks")):
            raise ValueError("old state or authorization differs")
        for field in ("repository_paths", "resource_allowlist"):
            if not isinstance(previous.get(field), dict) or not previous[field]:
                raise ValueError("missing original authorization scope")
        if (previous["repository_paths"] != old_state["verification_candidate"]["repository_paths"]
                or previous["resource_allowlist"] != old_state["resource_allowlist"]):
            raise ValueError("original authorization does not match state")
        old_id, new_id = previous.get("receipt_id"), receipt.get("receipt_id")
        if (not isinstance(old_id, str) or not old_id.strip()
                or not isinstance(new_id, str) or not new_id.strip() or old_id == new_id
                or not isinstance(receipt.get("trusted_user_message_reference"), str)
                or not receipt["trusted_user_message_reference"].strip()
                or receipt.get("supersedes") != {"receipt_id": old_id, "sha256": _sha(old_receipt)}):
            raise ValueError("receipt identity or supersession invalid")
        decision = receipt.get("w6_02_decisions")
        if (not isinstance(decision, dict) or set(decision) != {
                "backend_env", "backend_port", "seed_accounts", "device_id", "browser"}
                or decision["backend_env"] != "local"
                or type(decision["backend_port"]) is not int
                or not 1 <= decision["backend_port"] <= 65535 or decision["backend_port"] in (4323, 9800)
                or decision["seed_accounts"] != "synthetic"
                or decision["device_id"] != "XWE6R19916004085"
                or decision["browser"] != "chromium"):
            raise ValueError("W6-02 decisions differ from authorization")
        permitted_receipt_changes = {"plan_manifest_sha256", "issued_at", "expires_at", "readmission",
                                     "receipt_id", "trusted_user_message_reference", "supersedes",
                                     "w6_02_decisions"}
        if ({k: v for k, v in previous.items() if k not in permitted_receipt_changes}
                != {k: v for k, v in receipt.items() if k not in permitted_receipt_changes}):
            raise ValueError("authorization permissions changed")
        issued, expires = _receipt_time(receipt["issued_at"]), _receipt_time(receipt["expires_at"])
        if (type(now) not in (int, float) or not math.isfinite(now)
                or type(issued) not in (int, float) or type(expires) not in (int, float)
                or not math.isfinite(issued) or not math.isfinite(expires)
                or not 0 < issued <= now < expires <= issued + 604800
                or any(type(value) not in (int, float) or not math.isfinite(value)
                       or value <= 0
                       for value in (*old_times.values(), *new_times.values()))
                or any(new_times[key] <= max(old_times[key], now) for key in old_times)
                or not new_times["task"] <= new_times["wave"] <= new_times["run"] <= expires):
            raise ValueError("invalid deadline extension")
        expected_contract = copy.deepcopy(old_contract)
        if expected_contract.get("retry_policy", {}).get("task_attempts_total_max") != 5:
            raise ValueError("unexpected global task limit")
        overrides = expected_contract.setdefault("retry_policy", {}).setdefault("task_attempts_overrides", {})
        if not isinstance(overrides, dict) or "W6-01" in overrides:
            raise ValueError("existing task override")
        overrides["W6-01"] = 8
        if "reviewed_readmission" in expected_contract:
            raise ValueError("existing readmission policy")
        expected_contract["reviewed_readmission"] = {
            "task_id": "W6-01", "attempt_limit": 8, "deadlines": new_times,
        }
        surface = expected_contract["upload_surface_contract"]
        if surface.get("applicability") != OLD_APPLICABILITY:
            raise ValueError("unexpected original upload applicability")
        surface["applicability"] = NEW_APPLICABILITY
        _validate_enterprise_leaves(old_contract)
        if expected_contract != new_contract:
            raise ValueError("contract changed outside reviewed scope")
        if ({k: v for k, v in old_archive["files"].items() if k != "execution-contract.json"}
                != {k: v for k, v in new_archive["files"].items() if k != "execution-contract.json"}):
            raise ValueError("other frozen inputs changed")
        expected_state = copy.deepcopy(old_state)
        expected_state["plan_manifest_sha256"] = new_hash
        expected_state["verification_candidate"]["plan_manifest_sha256"] = new_hash
        expected_state["authorization_sha256"] = _sha(new_receipt)
        new_path = new_state.get("authorization_path")
        new_plan = new_state.get("plan_dir")
        if (not isinstance(new_path, str) or not new_path.startswith("/")
                or new_path == old_state.get("authorization_path")
                or not isinstance(new_plan, str) or not new_plan.startswith("/")
                or new_plan == old_state.get("plan_dir")):
            raise ValueError("new immutable receipt path required")
        expected_state["authorization_path"] = new_path
        expected_state["plan_dir"] = new_plan
        expected_state["tasks"]["W6-01"]["task_deadline"] = new_times["task"]
        expected_state["time_budgets"]["waves"]["W6"]["deadline"] = new_times["wave"]
        expected_state["time_budgets"]["run"]["deadline"] = new_times["run"]
        expected_state["reviewed_readmission"] = _sha(new_receipt)
        if expected_state != new_state:
            raise ValueError("unrelated state changed")
        if not callable(verify_user) or verify_user(new_receipt) is not True:
            raise ValueError("trusted user authorization unverified")
        if not callable(verify_review) or verify_review(review, new_receipt) is not True:
            raise ValueError("independent review unverified")
        return True, "validated proposal only"
    except Exception:
        return False, "invalid or unverified readmission proposal"
