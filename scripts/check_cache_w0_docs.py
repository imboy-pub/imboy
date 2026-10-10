#!/usr/bin/env python3
"""W0 交付文档/证据/验收 ledger 一致性校验。

exit 0 只表示文档、证据与 ledger 相互一致；不表示 W0、生产或 UAT 通过。
--self-test 验证校验器本身 fail-closed：缺证据、缺必需 ID、未知结果、
无证据 PASS、ledger 与计划任务文本不符、状态与 ledger 矛盾一律拒绝。
"""
import argparse
import hashlib
import json
import re
import sys
from pathlib import Path

REQUIRED_IDS = [f"W0-{index:02d}" for index in range(1, 11)]
ALLOWED_RESULTS = {"PASS", "FAIL", "BLOCKED", "BLOCKED_EXTERNAL", "NOT_EXECUTED"}
REQUIRED_UNRESOLVED_FORMS = {"forwarded_key", "derived_string_key", "random_binary_key",
                             "runtime_keys_via_syn", "legacy_set_broadcast"}
DOCS_WHITELIST = {"README.md", "plan-v2.2.1.md", "key-registry.json", "key-registry.md",
                  "confirmed-requirements.md", "architecture-decision.md",
                  "cache-callsite-census.md"}
CALL_PATTERN = re.compile(r"\b(imboy_cache|depcache)\s*:\s*(\w+)\s*\(")


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def plan_w0_tasks(plan_text):
    """从绑定计划逐字提取 W0-01..W0-10 的 ID 与任务描述，不重新定义。"""
    tasks = {}
    for line in plan_text.splitlines():
        match = re.match(r"^\| (W0-\d{2}) \| (.+?) \|$", line.strip())
        if match:
            tasks[match[1]] = match[2]
    return tasks


def evidence_anchor_ok(evidence_path, run_dirs, docs_dir):
    """证据必须落在受 manifest 哈希链约束的 run 目录或仓内白名单交付文档内。"""
    path = Path(evidence_path).resolve()
    for run_dir in run_dirs:
        run_dir = Path(run_dir).resolve()
        if path.parent == run_dir:
            manifest = json.loads((run_dir / "sha256.json").read_text())
            return path.name in manifest
    return path.parent == Path(docs_dir).resolve() and path.name in DOCS_WHITELIST


def validate_ledger(decision, plan_tasks, run_dirs=(), docs_dir=None):
    """ledger 完整性与 fail-closed 规则；违规一律 AssertionError。"""
    ledger = decision["acceptance_ledger"]
    by_id = {}
    for item in ledger:
        assert item["id"] not in by_id, f"ledger 重复 ID: {item['id']}"
        by_id[item["id"]] = item
    for required in REQUIRED_IDS:
        assert required in by_id, f"ledger 缺少必需接受项: {required}"
    assert set(by_id) == set(REQUIRED_IDS), "ledger 出现必需 ID 之外的条目"
    for item_id, item in by_id.items():
        assert item["task"] == plan_tasks[item_id], f"{item_id} 任务文本与绑定计划不符"
        assert item["result"] in ALLOWED_RESULTS, f"{item_id} 未知结果: {item['result']}"
        assert item.get("oracle"), f"{item_id} 缺 oracle"
        assert all(isinstance(gap, str) and gap.strip() for gap in item.get("gaps", [])), \
            f"{item_id} gaps 含空值"
        if item["result"] == "PASS":
            assert item.get("executions", 0) >= 1, f"{item_id} PASS 但 executions<1"
            assert item.get("evidence"), f"{item_id} PASS 但无证据"
            for evidence in item["evidence"]:
                path = Path(evidence["path"])
                assert path.exists(), f"{item_id} 证据不存在: {path}"
                assert digest(path) == evidence["sha256"], f"{item_id} 证据摘要漂移: {path}"
                if run_dirs or docs_dir:
                    assert evidence_anchor_ok(path, run_dirs, docs_dir), \
                        f"{item_id} 证据路径未锚定到 run 目录或白名单文档: {path}"
        else:
            assert item.get("gaps") or item.get("skip_reason"), \
                f"{item_id} 非 PASS 但未列出缺口或 skip 理由"
    not_pass = [item["id"] for item in by_id.values() if item["result"] != "PASS"]
    if not_pass:
        assert decision["status"] == "W0_BLOCKED", \
            f"存在未通过必需项 {not_pass} 但 status={decision['status']}"
    else:
        assert decision["status"] == "W0_PASS", "全部必需项通过但 status 未置 W0_PASS"
    assert decision["status"] in {"W0_PASS", "W0_BLOCKED"}, f"未知 status: {decision['status']}"


def verify_run_files(folder, run):
    manifest = folder / "sha256.json"
    assert digest(manifest) == run["manifest_sha256"], f"manifest 摘要不符: {folder}"
    for filename, expected in json.loads(manifest.read_text()).items():
        assert digest(folder / filename) == expected, f"文件摘要漂移: {folder}/{filename}"


def verify_candidate(candidate, in_repo_registry):
    runs = candidate["runs"]
    assert len(runs) >= 2, f"候选 {candidate['id']} 少于两轮独立运行"
    registries = []
    for run in runs:
        folder = Path(run["path"])
        verify_run_files(folder, run)
        baseline = json.loads((folder / "baseline.json").read_text())
        assert baseline["head"] == candidate["head"], \
            f"候选 {candidate['id']} 的 run 未绑定候选 HEAD"
        for name, expected in candidate["script_hashes"].items():
            assert baseline["script_hashes"][name] == expected, \
                f"候选 {candidate['id']} 脚本摘要不符: {name}"
        assert baseline["compile_exit"] == baseline["run_exit"] == 0
        assert baseline["operations"] == 10000
        result = json.loads((folder / "performance.json").read_text())
        assert result["three_node"]["status"] == "measured"
        assert result["three_node"]["nodes"] == 3
        assert len(result["single_node"]) == 18
        assert len(result["three_node"]["results"]) == 3
        registries.append((folder / "key-registry.json").read_bytes())
    assert all(registry == registries[0] for registry in registries), \
        f"候选 {candidate['id']} 各轮 registry 不一致"
    assert digest(Path(runs[0]["path"]) / "key-registry.json") == candidate["registry_sha256"]
    proof = candidate.get("head_equivalence_proof")
    if candidate["id"].startswith("closeout"):
        # 授权 HEAD 与执行 HEAD 的等价性证明是 closeout 候选的强制锚：
        # 对照轮必须真实出自授权 HEAD（baseline.head 逐字匹配 + manifest 链完整），
        # 否则等价性退化为"两文件相等"而失去绑定含义。
        assert proof, f"候选 {candidate['id']} 缺少强制的 head_equivalence_proof"
        assert candidate.get("authorized_head"), "closeout 候选缺 authorized_head"
    if proof:
        before_dir = Path(proof["run_at_authorized_head"])
        after_dir = Path(proof["run_at_execution_head"])
        before_baseline = json.loads((before_dir / "baseline.json").read_text())
        assert before_baseline["head"] == candidate.get("authorized_head"), \
            "对照轮 baseline.head 与 authorized_head 不符，等价性证明失效"
        verify_run_files(before_dir, {"manifest_sha256": digest(before_dir / "sha256.json")})
        before = before_dir / "source-evidence.json"
        after = after_dir / "source-evidence.json"
        assert before.exists() and after.exists(), "HEAD 等价性证明的轮次目录缺失"
        assert digest(before) == digest(after), \
            "授权 HEAD 与执行 HEAD 的缓存证据面出现漂移，等价性证明失效"
    if candidate["id"].startswith("closeout"):
        assert json.loads(registries[0]) == in_repo_registry, "当前候选 registry 与仓内不一致"
        checks = json.loads((Path(runs[0]["path"]) / "performance.json").read_text())["self_checks"]
        assert checks["loader_executions"] == 1
        assert checks["stale_refill_after_flush"] == "reproduced"
        assert checks["stale_waiter_value_after_flush"] == "stale"
        assert checks["loader_exception_reraised"] is True
        assert checks["premature_exit_waiter"] == "ok"
        assert checks["first_caller_becomes_writer"] == "undefined"
        unresolved = json.loads((Path(runs[0]["path"]) / "unresolved-forms.json").read_text())
        forms = {item["form"] for item in unresolved}
        assert REQUIRED_UNRESOLVED_FORMS <= forms, f"unresolved 形态缺失: {REQUIRED_UNRESOLVED_FORMS - forms}"
        broadcast = [item for item in unresolved if item["form"] == "legacy_set_broadcast"][0]
        assert len(broadcast["broadcast_namespaces"]) >= 10
    else:
        checks = json.loads((Path(runs[0]["path"]) / "performance.json").read_text())["self_checks"]
        assert checks["loader_executions"] == 1
        assert checks["stale_refill_after_flush"] == "reproduced"


def verify_report_rows(report, runs, run_labels):
    for run, label in zip(runs, run_labels):
        result = json.loads((Path(run["path"]) / "performance.json").read_text())
        for row in result["single_node"] + result["three_node"]["results"]:
            values = [label, row["receiver_count"], row["kind"], row["workers"], row["operations"],
                      f'{row["throughput_ops_sec"]:.0f}', row["p50_us"], row["p95_us"], row["p99_us"],
                      f'{row["error_rate"]:.1%}', row["sampled_queue_max"],
                      f'{row["sampled_beam_memory_peak_bytes"] / 1048576:.2f}']
            assert "| " + " | ".join(map(str, values)) + " |" in report, \
                f"报告缺少数据行: {label} {row['kind']} w={row['workers']}"
            assert row["operations"] == row["workers"] * 10000
            assert row["errors"] == 0 and row["error_rate"] == 0
            assert 0 <= row["p50_us"] <= row["p95_us"] <= row["p99_us"]
            assert row["throughput_ops_sec"] > 0


def cache_call_lines(source_text):
    """与 runner census 相同的词法规则，重算调用行集合。"""
    lines = set()
    for number, line in enumerate(source_text.splitlines(), 1):
        code = line.split("%", 1)[0]
        for match in CALL_PATTERN.finditer(code):
            lines.add((number, match.group(0)))
    return lines


def verify_drift(decision):
    candidates = {item["id"]: item for item in decision["candidates"]}
    hist = Path(candidates["hist-578d83ac"]["runs"][0]["path"]) / "source-evidence.json"
    current = Path(candidates["closeout-34858496"]["runs"][0]["path"]) / "source-evidence.json"
    hist_items = {item["path"]: item for item in json.loads(hist.read_text())}
    current_items = {item["path"]: item for item in json.loads(current.read_text())}
    drift = sorted(path for path in current_items if hist_items.get(path) != current_items[path])
    recorded = candidates["closeout-34858496"]["source_drift_vs_hist"]
    assert drift == recorded["files"], f"漂移记录与实际不符: {drift}"
    # 不信任自声明：从两轮 source-evidence 内嵌全文重算缓存调用行差异
    recomputed = 0
    for path in set(hist_items) | set(current_items):
        old = hist_items.get(path, {}).get("source", "")
        new = current_items.get(path, {}).get("source", "")
        recomputed += len(cache_call_lines(old) ^ cache_call_lines(new))
    assert recomputed == recorded["cache_call_line_drift"], \
        f"缓存调用行漂移重算不符: 声明 {recorded['cache_call_line_drift']} vs 重算 {recomputed}"


def main():
    parser = argparse.ArgumentParser(description="校验 W0 交付文档/证据/ledger 一致性")
    parser.add_argument("--self-test", action="store_true",
                        help="验证校验器对损坏 ledger 一律拒绝（fail-closed 自检）")
    parser.add_argument("--plan", type=Path, default=None,
                        help="计划文件回退路径（默认工作区 .Codex/erlang_cache_plan.md）")
    args = parser.parse_args()

    root = Path(__file__).resolve().parents[1]
    evidence = root / "docs/architecture/cache-v2.2.1/w0"
    decision = json.loads((evidence / "decision.json").read_text())
    plan = args.plan or root.parent / ".Codex/erlang_cache_plan.md"
    plan_text = plan.read_text()
    plan_tasks = plan_w0_tasks(plan_text)

    if args.self_test:
        self_test(plan_tasks, evidence)
        print(json.dumps({"status": "SELF_TEST_PASS"}, ensure_ascii=False))
        return

    assert digest(plan) == decision["plan_sha256"]
    assert digest(plan) == digest(evidence / "plan-v2.2.1.md")
    assert decision["version"] == "V2.2.1"
    assert decision["architecture"] == "A"
    assert decision["confirmed"] is False
    assert decision["replicate_whitelist"] == []
    assert decision["w1_status"] == "NOT_STARTED / BLOCKED_ON_W0"
    assert len(plan_tasks) == 10, "计划 W0 任务表提取不足 10 项"

    registry = json.loads((evidence / "key-registry.json").read_text())
    namespaces = [item["namespace"] for item in registry]
    assert len(namespaces) == len(set(namespaces)) == decision["registry_count"]
    assert digest(evidence / "key-registry.json") == decision["registry_sha256"]
    assert not any(item["replication_allowed"] for item in registry)
    assert all(item["mutation_hook"] and item["mutation_hook_reason"] for item in registry)
    matrix = (evidence / "key-registry.md").read_text()
    fields = ["namespace", "key_pattern", "policy", "authority", "ttl_sec", "sensitive", "owner"]
    assert all("| " + " | ".join(str(item[field]).replace("|", "/") for field in fields) + " |" in matrix
               for item in registry)

    candidates = {item["id"]: item for item in decision["candidates"]}
    assert set(candidates) == {"hist-578d83ac", "closeout-34858496"}, "候选绑定不完整"
    for candidate in decision["candidates"]:
        verify_candidate(candidate, registry)

    report = (evidence / "report.md").read_text()
    assert all(value in report for value in ["V2.2.1", "W0_BLOCKED", "confirmed=false", "方案 A",
                                             "hist-578d83ac", "closeout-34858496"])
    verify_report_rows(report, candidates["hist-578d83ac"]["runs"], [1, 2])
    verify_report_rows(report, candidates["closeout-34858496"]["runs"], [7, 8])
    verify_drift(decision)
    probe = decision.get("depcache_timeout_probe")
    if probe:
        probe_data = probe["result"]
        canonical = json.dumps(probe_data, sort_keys=True, ensure_ascii=False)
        assert hashlib.sha256(canonical.encode()).hexdigest() == probe["result_sha256"], "探针结果摘要漂移"
        for key, expected in [("first_caller_became_writer", "undefined"),
                              ("second_writer_after_lock_expiry", "undefined"),
                              ("waiter1_result", "timed_out"),
                              ("final_value_after_last_set", "v2")]:
            assert probe_data[key] == expected, f"探针 oracle 不符: {key}"
        assert probe_data["waiter2_result"] == {"got": "v1"}, f"探针 oracle 不符: waiter2_result"
    run_dirs = [run["path"] for item in decision["candidates"] for run in item["runs"]]
    validate_ledger(decision, plan_tasks, run_dirs, evidence)

    print(json.dumps(dict(status="DOC_CONSISTENCY_PASS", w0_status=decision["status"],
                          registry_count=len(registry),
                          candidates=len(decision["candidates"]),
                          ledger_items=len(decision["acceptance_ledger"]),
                          runs=sum(len(item["runs"]) for item in decision["candidates"])),
                    ensure_ascii=False))


def self_test(plan_tasks, docs_dir):
    """最小自检：缺必需 ID、未知结果、无证据 PASS、证据漂移/缺失、任务文本漂移、
    状态矛盾、重复 ID、多余 ID、证据未锚定 全部拒绝。"""

    def base_item(item_id):
        return {"id": item_id, "task": plan_tasks[item_id], "result": "BLOCKED",
                "oracle": "o", "executions": 0, "evidence": [],
                "gaps": ["g"], "skip_reason": None}

    def expect_reject(decision, message, **kwargs):
        try:
            validate_ledger(decision, plan_tasks, **kwargs)
        except AssertionError:
            return
        raise SystemExit(f"self-test 失败：{message}未被拒绝")

    good = [base_item(item_id) for item_id in REQUIRED_IDS]

    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": good[:-1]}, "缺必需 ID ")
    duplicate = good + [dict(base_item(REQUIRED_IDS[0]))]
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": duplicate}, "重复 ID ")
    extra = good + [dict(base_item(REQUIRED_IDS[0]), id="W0-99")]
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": extra}, "多余 ID ")

    unknown = [base_item(item_id) for item_id in REQUIRED_IDS]
    unknown[0]["result"] = "MAYBE"
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": unknown}, "未知结果 ")

    no_evidence = [base_item(item_id) for item_id in REQUIRED_IDS]
    no_evidence[0].update(result="PASS", executions=1, evidence=[])
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": no_evidence}, "无证据 PASS ")

    stale_evidence = [base_item(item_id) for item_id in REQUIRED_IDS]
    stale_evidence[0].update(result="PASS", executions=1,
                             evidence=[{"path": "/nonexistent/w0.json", "sha256": "0" * 64}])
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": stale_evidence}, "缺失证据 PASS ")

    empty_gaps = [base_item(item_id) for item_id in REQUIRED_IDS]
    empty_gaps[0]["gaps"] = [""]
    empty_gaps[0]["skip_reason"] = None
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": empty_gaps}, "空值 gaps ")

    wrong_task = [base_item(item_id) for item_id in REQUIRED_IDS]
    wrong_task[0]["task"] = "被篡改的任务描述"
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": wrong_task}, "任务文本漂移 ")

    status_conflict = [base_item(item_id) for item_id in REQUIRED_IDS]
    expect_reject({"status": "W0_PASS", "acceptance_ledger": status_conflict}, "状态与 ledger 矛盾 ")

    all_pass = [base_item(item_id) for item_id in REQUIRED_IDS]
    anchor_doc = docs_dir / "confirmed-requirements.md"
    for item in all_pass:
        item.update(result="PASS", executions=1,
                    evidence=[{"path": str(anchor_doc), "sha256": digest(anchor_doc)}])
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": all_pass}, "全 PASS 却保持 BLOCKED ")
    try:
        validate_ledger({"status": "W0_PASS", "acceptance_ledger": all_pass}, plan_tasks)
    except AssertionError:
        raise SystemExit("self-test 失败：全 PASS + W0_PASS 被误拒")

    unanchored = [base_item(item_id) for item_id in REQUIRED_IDS]
    outside = Path(__file__)
    for item in unanchored:
        item.update(result="PASS", executions=1,
                    evidence=[{"path": str(outside), "sha256": digest(outside)}])
    expect_reject({"status": "W0_BLOCKED", "acceptance_ledger": unanchored}, "证据未锚定 ",
                  run_dirs=[], docs_dir=docs_dir)


if __name__ == "__main__":
    main()
