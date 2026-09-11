#!/usr/bin/env python3
"""A/B 盲分汇总（离线；Phase 2B 授权后用真实 blind-scores.csv 喂入）。
冻结门（docs/plans/2026-09-10-moya-post-security-product-plan.md §MN-AI-04/05、MN-EVAL-03）：
  gate_ai04: severity_violation==0 且 actionable>=90%（v3 N6：与 rubric-v0 冻结口径一致，
             「具体且可行动」= d2_specific>=4 且 d3_actionable>=4）
  gate_ai05: B 相对 A 新增正确可行动过程观察 >=6/20 份；severity 不增；中位修改时长不变长
用法: python3 ab_summary.py blind-scores.csv  -> decision.json（stdout）
"""
import csv, json, statistics, sys
rows = list(csv.DictReader(open(sys.argv[1])))
def f(r, k): return float(r[k])
samples = sorted({r["sample_id"] for r in rows if r["excluded"] != "1"})
a = [r for r in rows if r["arm"] == "A" and r["excluded"] != "1"]
b = [r for r in rows if r["arm"] == "B" and r["excluded"] != "1"]
act = lambda rs: sum(1 for r in rs if f(r, "d2_specific") >= 4 and f(r, "d3_actionable") >= 4) / max(len(rs), 1)
b_new = {r["sample_id"] for r in b if int(r["new_process_obs"]) > 0}
a_new = {r["sample_id"] for r in a if int(r["new_process_obs"]) > 0}
med = lambda rs: statistics.median(f(r, "teacher_edit_minutes") for r in rs)
decision = {
  "n_samples": len(samples),
  "gate_ai04": {"severity_violations": sum(int(r["severity_violation"]) for r in a + b),
                 "actionable_ratio": round((act(a) + act(b)) / 2, 4),
                 "pass": sum(int(r["severity_violation"]) for r in a + b) == 0 and (act(a) + act(b)) / 2 >= 0.9},
  "gate_ai05": {"b_new_process_samples": len(b_new - a_new),
                 "severity_delta": sum(int(r["severity_violation"]) for r in b) - sum(int(r["severity_violation"]) for r in a),
                 "median_edit_minutes": {"A": med(a), "B": med(b)},
                 "pass": len(b_new - a_new) >= 6
                          and sum(int(r["severity_violation"]) for r in b) <= sum(int(r["severity_violation"]) for r in a)
                          and med(b) <= med(a)},
  "excluded_samples": [r["sample_id"] for r in rows if r["excluded"] == "1"],
  "note": "离线模板验证；真实数据须来自授权盲测（Phase 2B BLOCKED_EXTERNAL）"}
json.dump(decision, sys.stdout, ensure_ascii=False, indent=2)
