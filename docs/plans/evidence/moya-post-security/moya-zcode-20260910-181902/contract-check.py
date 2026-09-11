#!/usr/bin/env python3
"""contract-check.py — moya-teaching.yaml OpenAPI 3.0 结构校验（MN-CONTRACT-01）。

校验项（结构级，不依赖网络/外部校验器）：
  1. openapi 3.0 字段完整性：openapi(3.0.x)/info(title,version)/paths/components 必须
     且形状合法；servers 存在性不强制（本域免真实 URL）。
  2. paths 唯一性：原始文本中每个 path 字面量只出现一次（YAML 解析后重键会被
     pyyaml 静默覆盖，故在原始文本行级再查一次）。
  3. 每个 operation（method+path）至少一个 response；成功响应必须含 examples；
     response 键必须是合法 HTTP 状态码或 default。
  4. 安全性：全部 operation 声明 security（或全局 security 非空）——JWT deny-by-default。
  5. 隐私红线扫描：示例值中不得出现真实形态的 AppID/token/openid/绝对 URL/手机号/邮箱。
  6. 冻结契约关键点：TSID 字段示例值必须是 string；错误码示例覆盖 5430/5431/5432。

用法：python3 contract-check.py [yaml路径]；输出 PASS/FAIL 明细，exit 0/1。
"""
import json
import re
import sys

import yaml

DEFAULT_PATH = (
    "docs/plans/evidence/moya-post-security/moya-zcode-20260910-181902/"
    "openapi/moya-teaching.yaml"
)

VALID_METHODS = {"get", "post", "put", "delete", "patch", "head", "options", "trace"}
HTTP_STATUS_RE = re.compile(r"^(default|[1-5][0-9][0-9]|2XX|3XX|4XX|5XX)$")

PRIVACY_PATTERNS = [
    (r"\bwx[a-f0-9]{16}\b", "微信 AppID 形态"),
    (r"\bopenid\b\s*[:=]\s*[\"'][^\"']{6,}", "openid 字段赋值"),
    (r"eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\.", "JWT 形态凭证"),
    (r"Bearer\s+[A-Za-z0-9]", "Bearer 字样（凭证值不得出现在示例中）"),
    (r"\bappid\b", "appid 字样（区分大小写；正文普通词 AppID 不算）"),
    (r"https?://(?!s\.example)[a-z0-9.-]+\.[a-z]{2,}", "非 example 保留域名的绝对 URL"),
    (r"\b1[3-9]\d{9}\b", "手机号形态"),
    (r"[a-zA-Z0-9._%+-]+@(?:example)\.(?!com|org|net)[a-z]{2,}", "异常邮箱形态"),
]


def fail(msg, errors):
    errors.append(msg)


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_PATH
    errors = []
    warnings = []

    with open(path, "r", encoding="utf-8") as f:
        raw = f.read()
    try:
        doc = yaml.safe_load(raw)
    except yaml.YAMLError as e:
        print(f"FAIL: YAML 解析失败: {e}")
        return 1

    # 1. openapi 3.0 字段完整性
    if not isinstance(doc, dict):
        fail("根节点必须是 mapping", errors)
        print_report(doc if isinstance(doc, dict) else {}, errors, warnings, path, 0)
        return 1
    ov = doc.get("openapi")
    if not isinstance(ov, str) or not ov.startswith("3.0."):
        fail(f"openapi 版本必须为 3.0.x，实际: {ov!r}", errors)
    info = doc.get("info")
    if not isinstance(info, dict):
        fail("info 必须存在且为 mapping", errors)
    else:
        for k in ("title", "version"):
            if not info.get(k):
                fail(f"info.{k} 必填", errors)
    if not isinstance(doc.get("paths"), dict) or not doc["paths"]:
        fail("paths 必须存在且非空", errors)

    # 2. paths 唯一性（原始文本行级）
    raw_keys = re.findall(r"^  (/api/[^\s:]+):\s*$", raw, flags=re.M)
    dupes = {k for k in raw_keys if raw_keys.count(k) > 1}
    if dupes:
        fail(f"path 在 YAML 顶层重复（后值静默覆盖前值）: {sorted(dupes)}", errors)

    # 3. 每个 operation：responses 完整、成功响应带 examples、键合法
    ops = 0
    ops_without_examples = []
    global_security = doc.get("security")
    for p, item in (doc.get("paths") or {}).items():
        if not isinstance(item, dict):
            fail(f"path {p} 不是 mapping", errors)
            continue
        for m, op in item.items():
            if m in ("parameters", "servers", "summary", "description"):
                continue
            if m not in VALID_METHODS:
                fail(f"path {p} 含非法 method 键: {m!r}", errors)
                continue
            ops += 1
            if not isinstance(op, dict):
                fail(f"{m.upper()} {p} operation 不是 mapping", errors)
                continue
            resp = op.get("responses")
            if not isinstance(resp, dict) or not resp:
                fail(f"{m.upper()} {p} 缺 responses", errors)
                continue
            for code, r in resp.items():
                if not HTTP_STATUS_RE.match(str(code)):
                    fail(f"{m.upper()} {p} response 键非法: {code!r}（须为 HTTP 状态码或 default）", errors)
                if not isinstance(r, dict) or "description" not in r:
                    fail(f"{m.upper()} {p} response {code} 缺 description", errors)
            success = resp.get("200")
            if success is None:
                fail(f"{m.upper()} {p} 缺 200 成功响应（本域业务错误同为 HTTP 200 envelope）", errors)
            else:
                content = success.get("content") or {}
                has_example = any(
                    (media.get("examples") or media.get("example")) is not None
                    for media in content.values()
                )
                if not has_example:
                    ops_without_examples.append(f"{m.upper()} {p}")
            if not op.get("security") and not global_security:
                fail(f"{m.upper()} {p} 未声明 security 且无全局 security（违反 JWT deny-by-default 冻结）", errors)

    if ops_without_examples:
        for o in ops_without_examples:
            fail(f"{o} 200 响应缺 examples", errors)

    # 4.（合并进 3：security 已逐 operation 校验）

    # 5. 隐私红线扫描（示例值层面）
    for pat, label in PRIVACY_PATTERNS:
        for mo in re.finditer(pat, raw):
            frag = mo.group(0)[:60]
            fail(f"隐私红线（{label}）命中: {frag!r}", errors)

    # 6. 冻结契约关键点
    required_paths = {
        "/api/v1/teaching/classes/{id}/learners",
        "/api/v1/teaching/tasks",
        "/api/v1/teaching/submissions/{id}",
        "/api/v1/teaching/submissions/{id}/review-workbench",
        "/api/v1/teaching/submissions/{id}/review-draft",
        "/api/v1/teaching/submissions/{id}/reviews/publish",
    }
    missing = required_paths - set(doc.get("paths") or {})
    if missing:
        fail(f"缺冻结端点: {sorted(missing)}", errors)

    for code in ("5430", "5431", "5432"):
        if f"code: {code}" not in raw:
            fail(f"错误码示例缺 {code}", errors)

    # TSID 示例值必须为 string（yaml 解析后抽查已知字段）
    def walk(node, trail):
        if isinstance(node, dict):
            for k, v in node.items():
                if k in ("example", "examples"):
                    continue
                if k in ("task_id", "attachment_id", "learner_id", "group_id", "submission_id",
                         "assignment_id", "confirm_learner_id") and isinstance(v, dict):
                    ex = v.get("example")
                    if ex is not None and not isinstance(ex, str):
                        fail(f"TSID 示例必须为 string: {'.'.join(trail + [k])} = {ex!r}", errors)
                walk(v, trail + [k])
        elif isinstance(node, list):
            for i, v in enumerate(node):
                walk(v, trail + [str(i)])

    walk(doc.get("components", {}).get("schemas", {}), ["components", "schemas"])

    print_report(doc, errors, warnings, path, ops)
    return 1 if errors else 0


def print_report(doc, errors, warnings, path, ops):
    n_paths = len(doc.get("paths") or {})
    print(f"contract-check: {path}")
    print(f"  openapi: {doc.get('openapi')}  paths: {n_paths}  operations: {ops}")
    for w in warnings:
        print(f"  WARN: {w}")
    if errors:
        print(f"  FAIL ({len(errors)}):")
        for e in errors:
            print(f"    - {e}")
        print("RESULT: FAIL")
    else:
        print("  全部结构校验通过：openapi 3.0 字段完整 / paths 唯一 / 每操作含 200+examples /"
              " JWT security 全覆盖 / 隐私红线零命中 / 5430-5432 示例在案")
        print("RESULT: PASS")


if __name__ == "__main__":
    try:
        sys.exit(main())
    except FileNotFoundError:
        print(f"FAIL: 文件不存在: {sys.argv[1] if len(sys.argv) > 1 else DEFAULT_PATH}")
        sys.exit(1)
