#!/usr/bin/env python3
"""Write honest E2E-01 task evidence from a local Golden Flow run."""

import argparse
import hashlib
import json
import os
from pathlib import Path


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(131072), b""):
            digest.update(chunk)
    return digest.hexdigest()


def artifact(artifact_id, path):
    resolved = Path(path).resolve()
    return {
        "id": artifact_id,
        "path": str(resolved),
        "sha256": sha256_file(resolved),
    }


def acceptance(acceptance_id, status, command_ids, artifact_ids, assertion):
    return {
        "acceptance_id": acceptance_id,
        "status": status,
        "command_ids": command_ids,
        "artifact_ids": artifact_ids,
        "assertions": [assertion],
    }


def build_evidence(args):
    evidence_dir = Path(args.evidence_dir).resolve()
    repo_root = Path(args.repo_root).resolve()
    summary_path = evidence_dir / "run-summary.txt"
    summary_path.write_text(
        "\n".join([
            "task=E2E-01",
            "status=%s" % args.status,
            "final_integrated_base=%d" % args.final_integrated_base,
            "suites_passed=%d" % args.suites_passed,
            "trace_exit=%d" % args.trace_exit,
            "http_smoke_passed=%d" % args.http_smoke_passed,
            "channel_webhook_passed=%d" % args.channel_webhook_passed,
            "agent_dialog_passed=%d" % args.agent_dialog_passed,
            "bot_dialog_passed=%d" % args.bot_dialog_passed,
            "protocol_negatives_passed=%d" % args.protocol_negatives_passed,
            "restart_passed=%d" % args.restart_passed,
            "cleanup_passed=%d" % args.cleanup_passed,
            "sensitive_scan_passed=%d" % args.sensitive_scan_passed,
            "failed_step=%s" % (
                args.failed_step if args.status == "FAIL" and args.failed_step else "none"
            ),
        ]) + "\n",
        encoding="utf-8",
    )

    artifacts = [
        artifact("artifact-run-summary", summary_path),
        artifact("artifact-harness", repo_root / "scripts/agent_hub_golden_flow.sh"),
    ]
    trace_path = evidence_dir / "runtime-correlation-trace.json"
    trace_result_path = evidence_dir / "trace-verifier.json"
    if trace_path.is_file():
        artifacts.append(artifact("artifact-runtime-trace", trace_path))
    if trace_result_path.is_file():
        artifacts.append(artifact("artifact-trace-verifier", trace_result_path))
    trace_exporter_path = repo_root / "scripts/export_agent_hub_correlation_trace.sql"
    if trace_exporter_path.is_file():
        artifacts.append(artifact("artifact-trace-exporter", trace_exporter_path))
    runbook_path = repo_root / "docs/operations/agent-hub-local-golden-flow.md"
    if runbook_path.is_file():
        artifacts.append(artifact("artifact-runbook", runbook_path))
    aead_runbook_path = repo_root / "docs/runbooks/bot-webhook-aead-key.md"
    if aead_runbook_path.is_file():
        artifacts.append(artifact("artifact-aead-runbook", aead_runbook_path))

    runtime_artifact_paths = {
        "artifact-http-mcp-smoke": evidence_dir / "ext01-a02-runtime.json",
        "artifact-http-channel-webhook-smoke": evidence_dir / "channel-webhook-a02-runtime.json",
        "artifact-channel-webhook-db": evidence_dir / "channel-webhook-a02-db.json",
        "artifact-agent-dialog-runtime": evidence_dir / "agent-dialog-a02-runtime.log",
        "artifact-agent-dialog-db": evidence_dir / "agent-dialog-a02-db.json",
        "artifact-bot-dialog-runtime": evidence_dir / "bot-dialog-a02-runtime.json",
        "artifact-bot-dialog-db": evidence_dir / "bot-dialog-a02-db.json",
        "artifact-webhook-5xx-runtime": evidence_dir / "webhook-5xx-a02-runtime.json",
        "artifact-webhook-5xx-db": evidence_dir / "webhook-5xx-a02-db.json",
        "artifact-webhook-4xx-runtime": evidence_dir / "webhook-4xx-a02-runtime.json",
        "artifact-webhook-4xx-db": evidence_dir / "webhook-4xx-a02-db.json",
        "artifact-delivery-replay-runtime": evidence_dir / "delivery-replay-a02-runtime.json",
        "artifact-delivery-replay-db": evidence_dir / "delivery-replay-a02-db.json",
        "artifact-agent-task-e2ee-runtime": evidence_dir / "agent-task-e2ee-a02-runtime.txt",
        "artifact-agent-task-e2ee-db": evidence_dir / "agent-task-e2ee-a02-db.json",
        "artifact-restart-before": evidence_dir / "restart-before.json",
        "artifact-restart-after": evidence_dir / "restart-after.json",
        "artifact-restart-logic-read": evidence_dir / "restart-logic-read.txt",
        "artifact-backend-before-restart": evidence_dir / "runtime-backend-before-restart.log",
        "artifact-backend-after-restart": evidence_dir / "runtime-backend-after-restart.log",
    }
    for artifact_id, path in runtime_artifact_paths.items():
        if path.is_file():
            artifacts.append(artifact(artifact_id, path))

    artifact_ids = {item["id"] for item in artifacts}
    http_artifacts = [
        artifact_id for artifact_id in ["artifact-http-mcp-smoke"]
        if artifact_id in artifact_ids
    ]
    channel_webhook_artifacts = [
        artifact_id
        for artifact_id in [
            "artifact-http-channel-webhook-smoke",
            "artifact-channel-webhook-db",
        ]
        if artifact_id in artifact_ids
    ]
    restart_artifacts = [
        artifact_id for artifact_id in runtime_artifact_paths
        if artifact_id.startswith("artifact-restart-")
        or artifact_id.startswith("artifact-backend-")
        if artifact_id in artifact_ids
    ]
    http_smoke_ok = bool(args.http_smoke_passed and http_artifacts)
    channel_webhook_ok = bool(
        args.channel_webhook_passed and len(channel_webhook_artifacts) == 2
    )
    agent_dialog_artifacts = [
        artifact_id
        for artifact_id in [
            "artifact-agent-dialog-runtime",
            "artifact-agent-dialog-db",
        ]
        if artifact_id in artifact_ids
    ]
    agent_dialog_ok = bool(args.agent_dialog_passed and len(agent_dialog_artifacts) == 2)
    bot_dialog_artifacts = [
        artifact_id
        for artifact_id in [
            "artifact-bot-dialog-runtime",
            "artifact-bot-dialog-db",
        ]
        if artifact_id in artifact_ids
    ]
    bot_dialog_ok = bool(args.bot_dialog_passed and len(bot_dialog_artifacts) == 2)
    protocol_negative_artifacts = [
        artifact_id
        for artifact_id in [
            "artifact-webhook-5xx-runtime",
            "artifact-webhook-5xx-db",
            "artifact-webhook-4xx-runtime",
            "artifact-webhook-4xx-db",
            "artifact-delivery-replay-runtime",
            "artifact-delivery-replay-db",
            "artifact-agent-task-e2ee-runtime",
            "artifact-agent-task-e2ee-db",
        ]
        if artifact_id in artifact_ids
    ]
    protocol_negatives_ok = bool(
        args.protocol_negatives_passed and len(protocol_negative_artifacts) == 8
    )
    restart_ok = bool(args.restart_passed and len(restart_artifacts) == 5)
    trace_ok = bool(
        args.trace_exit == 0
        and trace_path.is_file()
        and trace_result_path.is_file()
    )

    common_artifacts = ["artifact-run-summary", "artifact-harness"]
    if args.status == "FAIL":
        commands = [{
            "id": "cmd-01",
            "command": args.failed_step or "agent_hub_golden_flow",
            "exit_code": args.failed_code,
        }]
        acceptances = [
            acceptance(
                "E2E-01-A%02d" % number,
                "FAIL" if number == 2 else "SKIP",
                ["cmd-01"] if number == 2 else [],
                common_artifacts,
                "Golden Flow stopped before all required checks completed.",
            )
            for number in range(1, 8)
        ]
        failed_tests = 1
        skipped_tests = 7
    else:
        commands = [
            {"id": "cmd-01", "command": "make app", "exit_code": 0},
            {
                "id": "cmd-02",
                "command": "scripts/drill_migrate.escript up against marker scratch DB",
                "exit_code": 0,
            },
            {
                "id": "cmd-03",
                "command": "make eunit-local for the %d Agent Hub suites" % args.suites_passed,
                "exit_code": 0,
            },
            {
                "id": "cmd-04",
                "command": "python3 scripts/verify_agent_hub_correlation_trace.py runtime-correlation-trace.json",
                "exit_code": args.trace_exit,
            },
            {
                "id": "cmd-05",
                "command": "verify marker scratch DB and temporary EUnit config are absent",
                "exit_code": 0 if args.cleanup_passed else 1,
            },
            {
                "id": "cmd-06",
                "command": "run real loopback HTTP MCP credential lifecycle",
                "exit_code": 0 if http_smoke_ok else 1,
            },
            {
                "id": "cmd-07",
                "command": "stop and restart the real backend, then compare persisted state",
                "exit_code": 0 if restart_ok else 1,
            },
            {
                "id": "cmd-08",
                "command": "run real loopback channel incoming-webhook positive and negative checks",
                "exit_code": 0 if channel_webhook_ok else 1,
            },
            {
                "id": "cmd-09",
                "command": "run real WebSocket group mention through the built-in Agent and local fake LLM",
                "exit_code": 0 if agent_dialog_ok else 1,
            },
            {
                "id": "cmd-10",
                "command": "run real WebSocket Bot mention, signed loopback delivery, HTTP reply, and protocol negatives",
                "exit_code": 0 if bot_dialog_ok else 1,
            },
            {
                "id": "cmd-11",
                "command": "run real webhook 5xx exhaustion, 4xx dead-letter, admin replay, and E2EE task negatives",
                "exit_code": 0 if protocol_negatives_ok else 1,
            },
        ]
        if args.final_integrated_base and args.status == "PASS":
            commands.append({
                "id": "cmd-12",
                "command": "verify main branch and candidate paths match HEAD",
                "exit_code": 0,
            })
        trace_artifacts = list(common_artifacts)
        if trace_result_path.is_file():
            trace_artifacts.append("artifact-trace-verifier")
        if trace_path.is_file():
            trace_artifacts.append("artifact-runtime-trace")
        if trace_exporter_path.is_file():
            trace_artifacts.append("artifact-trace-exporter")
        acceptances = [
            acceptance(
                "E2E-01-A01",
                "PASS" if trace_ok else "FAIL",
                ["cmd-04"],
                trace_artifacts,
                "The runtime audit source reconstructs request, task, event, approval, execution, delivery, and outcome under one correlation ID."
                if trace_ok else
                "The required runtime correlation chain or verifier result is missing or failed.",
            ),
            acceptance(
                "E2E-01-A02",
                "PASS" if (
                    http_smoke_ok and channel_webhook_ok and agent_dialog_ok
                    and bot_dialog_ok and protocol_negatives_ok
                ) else "FAIL",
                ["cmd-03", "cmd-06", "cmd-08", "cmd-09", "cmd-10", "cmd-11"],
                common_artifacts + http_artifacts + channel_webhook_artifacts
                + agent_dialog_artifacts + bot_dialog_artifacts
                + protocol_negative_artifacts,
                "The loopback runtime passes MCP authorization and credential lifecycle, incoming webhook, Agent/Bot dialogs, webhook retry/dead/replay, and E2EE fail-closed checks."
                if (
                    http_smoke_ok and channel_webhook_ok and agent_dialog_ok
                    and bot_dialog_ok and protocol_negatives_ok
                ) else
                "The required HTTP Golden Flow and all protocol negatives are not complete.",
            ),
            acceptance(
                "E2E-01-A03", "PASS" if restart_ok else "FAIL", ["cmd-07"],
                common_artifacts + restart_artifacts,
                "A real backend process stop/start preserves the completed task, approval, delivery, and correlation, and the restarted logic reads the terminal state."
                if restart_ok else
                "A complete real backend stop/start recovery check is missing or failed.",
            ),
            acceptance(
                "E2E-01-A04",
                "PASS" if args.sensitive_scan_passed else "FAIL",
                ["cmd-03"], common_artifacts,
                "Generated EUnit, HTTP client, and backend runtime logs were scanned without echoing matched values.",
            ),
            acceptance(
                "E2E-01-A05",
                "PASS" if args.cleanup_passed else "FAIL",
                ["cmd-05"], common_artifacts,
                "Marker scratch resources were checked after cleanup.",
            ),
            acceptance(
                "E2E-01-A06",
                "PASS" if runbook_path.is_file() and aead_runbook_path.is_file() else "FAIL",
                [],
                ["artifact-runbook", "artifact-aead-runbook"]
                if runbook_path.is_file() and aead_runbook_path.is_file()
                else common_artifacts,
                "The runbook documents current automation limits and the AEAD lifecycle drill.",
            ),
            acceptance(
                "E2E-01-A07",
                "PASS" if args.final_integrated_base and args.status == "PASS" else "SKIP",
                ["cmd-12"] if args.final_integrated_base and args.status == "PASS" else [],
                common_artifacts,
                "The Golden Flow passed on main with all candidate paths matching HEAD."
                if args.final_integrated_base and args.status == "PASS" else
                "Final integrated Base rerun is pending.",
            ),
        ]
        failed_tests = 0
        skipped_tests = (2 if args.final_integrated_base else 3) if restart_ok else 4

    residual_risks = [
        "Local fixtures do not replace real device, external MCP, or production acceptance.",
    ]
    if not (args.final_integrated_base and args.status == "PASS"):
        residual_risks.insert(0, "Final integrated Base rerun remains open.")

    return {
        "schema_version": 1,
        "task_id": "E2E-01",
        "status": args.status,
        "verification_status": "verified",
        "base_sha": {
            "imboy": args.imboy_sha,
            "imboyapp": args.imboyapp_sha,
            "imboyadmin": args.imboyadmin_sha,
        },
        "final_diff": [
            "imboy:docs/operations/agent-hub-local-golden-flow.md",
            "imboy:priv/migrations/00000107_agent_hub_runtime_audit.down.sql",
            "imboy:priv/migrations/00000107_agent_hub_runtime_audit.up.sql",
            "imboy:scripts/agent_hub_delivery_replay_smoke.py",
            "imboy:scripts/agent_hub_ext01_mcp_client_smoke.py",
            "imboy:scripts/agent_hub_golden_flow.sh",
            "imboy:scripts/agent_hub_http_status_fixture.py",
            "imboy:scripts/export_agent_hub_correlation_trace.sql",
            "imboy:scripts/write_agent_hub_e2e_evidence.py",
            "imboy:src/lib/bot_webhook_delivery_sender.erl",
            "imboy:src/logic/bot_webhook_delivery_worker.erl",
            "imboy:src/logic/agent_task_logic.erl",
            "imboy:src/logic/mcp_governance_logic.erl",
            "imboy:src/mcp/barrel_mcp_registry.erl",
            "imboy:src/mcp/imboy_mcp_tools.erl",
            "imboy:src/mcp/mcp_authz_gate.erl",
            "imboy:src/repo/agent_hub_audit_repo.erl",
            "imboy:src/repo/agent_task_repo.erl",
            "imboy:src/repo/bot_webhook_delivery_repo.erl",
            "imboy:test/api/mcp_handler_auth_tests.erl",
            "imboy:test/integration/agent_hub_runtime_trace_tests.erl",
            "imboy:test/logic/agent_task_logic_tests.erl",
            "imboy:test/logic/mcp_governance_logic_tests.erl",
            "imboy:test/mcp/barrel_mcp_protocol_tests.erl",
            "imboy:test/mcp/imboy_mcp_task_tools_tests.erl",
            "imboy:test/mcp/mcp_authz_gate_tests.erl",
            "imboy:test/repo/bot_webhook_delivery_repo_tests.erl",
            "imboy:test/scripts/test_agent_hub_golden_flow_db_isolation.sh",
            "imboy:test/scripts/test_agent_hub_http_status_fixture.py",
            "imboy:test/scripts/test_write_agent_hub_e2e_evidence.py",
        ],
        "commands": commands,
        "tests": {
            "passed": args.suites_passed,
            "failed": failed_tests,
            "skipped": skipped_tests,
        },
        "acceptance": acceptances,
        "artifacts": artifacts,
        "residual_risks": residual_risks,
        "commit": args.imboy_sha,
    }


def parse_args(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--evidence-dir", required=True)
    parser.add_argument("--repo-root", required=True)
    parser.add_argument("--status", choices=("FAIL", "PARTIAL", "PASS"), required=True)
    parser.add_argument("--imboy-sha", required=True)
    parser.add_argument("--imboyapp-sha", required=True)
    parser.add_argument("--imboyadmin-sha", required=True)
    parser.add_argument("--suites-passed", type=int, default=0)
    parser.add_argument("--trace-exit", type=int, default=2)
    parser.add_argument("--http-smoke-passed", type=int, choices=(0, 1), default=0)
    parser.add_argument("--channel-webhook-passed", type=int, choices=(0, 1), default=0)
    parser.add_argument("--agent-dialog-passed", type=int, choices=(0, 1), default=0)
    parser.add_argument("--bot-dialog-passed", type=int, choices=(0, 1), default=0)
    parser.add_argument("--protocol-negatives-passed", type=int, choices=(0, 1), default=0)
    parser.add_argument("--restart-passed", type=int, choices=(0, 1), default=0)
    parser.add_argument("--final-integrated-base", type=int, choices=(0, 1), default=0)
    parser.add_argument("--cleanup-passed", type=int, choices=(0, 1), default=0)
    parser.add_argument("--sensitive-scan-passed", type=int, choices=(0, 1), default=0)
    parser.add_argument("--failed-step", default="")
    parser.add_argument("--failed-code", type=int, default=1)
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)
    if args.status == "PASS" and not args.final_integrated_base:
        raise ValueError("PASS requires --final-integrated-base 1")
    evidence_dir = Path(args.evidence_dir).resolve()
    evidence_dir.mkdir(parents=True, exist_ok=True)
    output = evidence_dir / "evidence.json"
    tmp = evidence_dir / "evidence.json.tmp"
    with open(tmp, "w", encoding="utf-8") as handle:
        json.dump(build_evidence(args), handle, ensure_ascii=False, indent=2)
        handle.write("\n")
    os.replace(tmp, output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
