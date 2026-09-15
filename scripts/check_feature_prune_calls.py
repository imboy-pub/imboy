#!/usr/bin/env python3
"""特性裁剪调用门：找出「始终编译的模块**未加保护**地调用被裁剪模块」的地方。

为什么需要（2026-09-15 实证，imboy run enterprise-business-cs 已确认一例）：
  未选中某特性时，该特性的后端模块会被 `ERLC_EXCLUDE` 排除、**根本不编译**。
  Erlang 的远程调用是**运行期**解析的，所以编译期没有任何报错 —— 编译照过，
  一直到**运行期**才炸 `undef`。

  已确认的那一例（`F-EB10-1`，已修）：
    `src/api/auth_middleware_api_v1.erl` 的 `eb_auth_principal:is_tenant_surface_path/1`
    落在 cowboy 中间件入口 `execute/2` 内、**无任何 `-ifdef` 保护**；
    而 `eb_auth_principal` 在未选中 `enterprise_business` 时处于排除集里
    （由生成器实测渲染得到）。
  ⇒ 未选中档下**每一个 `/api/v1/*` 请求**都会在中间件里 `undef`，整条 API 挂掉。
    这不是「企业路由不可用」，而是**全站 API 不可用**。

  单点修完不解决问题类，故做成机械门：**约束是「调用被裁剪模块必须包在
  `-ifdef(IMBOY_FEATURE_<该特性>)` 里」**。未选中时该宏被整条省略（实测），
  所以 `-ifdef` 真的会生效。

判据：
  1. 被裁剪模块集 = 生成器 `FEATURE_BACKEND_MODULES` 的并集（另并入 `src/features/**` 的模块名）
  2. 对每个**不在此集合内**的 `src/**.erl`，去注释、去字符串后找远程调用 `Mod:fun(`
  3. 若 `Mod` 被裁剪，则要求它处在 `-ifdef(IMBOY_FEATURE_<Mod 所属特性>)` 的**活动分支**内
     （跟踪 `-ifdef/-ifndef/-else/-endif` 嵌套；`-else` 视为未保护）
  4. 命中即报 `文件:行 调用者 -> 被调者（所属特性）`，退出码非零

baseline（--baseline F，可选）：
  只豁免 `protection=defensive` 的**既有**条目（catch 包住的降级路径，未选中时
  静默跳过而非崩溃），且要求 caller+callee 精确匹配、当前仍为 defensive。
  `hard`（裸调用）**永不豁免**——它等价于未选中档全站故障，没有「历史原因」可言。

用法：
  python3 scripts/check_feature_prune_calls.py [仓根] [--baseline scripts/feature-prune-baseline.tsv]
  python3 scripts/check_feature_prune_calls.py --selftest
退出码：0 = 无未保护调用；1 = 有（列出全部）；2 = 用法/前提错误。

本文件与 .Codex run 控制台版同源（enterprise-business-cs-20260914T041836Z），
产品仓版是必经门（挂在 scripts/check_feature_architecture.sh，即 make arch-check）。
"""
import importlib.util
import re
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]

CALL_RE = re.compile(r"\b([a-z][a-zA-Z0-9_]*)\s*:\s*([a-z][a-zA-Z0-9_]*)\s*\(")
IFDEF_RE = re.compile(r"^\s*-\s*(ifdef|ifndef|else|endif)\s*\(?\s*([A-Za-z0-9_]*)\s*\)?")


def resolve_wt(arg):
    if arg is None:
        return REPO_ROOT
    p = Path(arg)
    return p if p.is_absolute() else REPO_ROOT / p


def load_feature_modules(wt):
    """(feature -> 模块名集合, 模块名 -> feature)。来自生成器的 FEATURE_BACKEND_MODULES。"""
    gen = wt / "scripts" / "generate_product_features.py"
    if not gen.is_file():
        raise RuntimeError(f"找不到生成器: {gen}")
    spec = importlib.util.spec_from_file_location("genfeat", str(gen))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    table = getattr(m, "FEATURE_BACKEND_MODULES", None)
    if not isinstance(table, dict):
        raise RuntimeError("生成器里没有 FEATURE_BACKEND_MODULES 字典")
    by_feature, mod2feature = {}, {}
    for feat, mods in table.items():
        names = {x for x in mods if isinstance(x, str)}
        by_feature[feat] = names
        for n in names:
            mod2feature.setdefault(n, set()).add(feat)
    return by_feature, mod2feature


def all_src_modules(wt):
    """src/** 下所有模块名 -> 文件路径。"""
    out = {}
    for p in (wt / "src").rglob("*.erl"):
        out[p.stem] = p
    return out


def strip_comments_and_strings(text):
    """去注释与字符串字面量，避免把说明文字/正则里的模块名当成真调用。

    保留行结构（注释替换为等长空白），这样**行号不变** —— 报告要能直接跳到那一行。
    """
    out, i, n = [], 0, len(text)
    while i < n:
        c = text[i]
        if c == "%":
            j = text.find("\n", i)
            j = n if j < 0 else j
            out.append(" " * (j - i))
            i = j
        elif c == '"':
            j = i + 1
            while j < n:
                if text[j] == "\\":
                    j += 2
                    continue
                if text[j] == '"':
                    break
                j += 1
            out.append('""' + " " * max(0, j - i - 1))
            i = min(j + 1, n)
        else:
            out.append(c)
            i += 1
    return "".join(out)


def scan_file(path, mod2feature, feature_of_macro):
    """返回 [(行号, 调用者, 被调者, 所属特性, 防护)] —— 只报**未保护**的调用。

    `防护` 列（用于判严重度与 baseline 豁免资格）：
      * `hard`       —— 裸调用：未选中该特性时 `undef` **直接抛出**，调用方崩
      * `defensive`  —— 同一行里有 `catch`：`undef` 被吞成值，降级为「这条分支静默不做」
                        （既有先例：`adm_moderation_logic.erl` 的 moment 分支就是这么写的）
    两者都要报：`defensive` 只是不崩，行为仍然是**静默失效**。
    这是词法启发式（只看同一行），`try ... catch` 跨行的情况会判成 `hard` —— 宁可报得重，
    不可报得轻（漏报的代价是一次运行期全站故障）。
    """
    raw = path.read_text(encoding="utf-8", errors="replace")
    code = strip_comments_and_strings(raw)
    findings = []
    # ifdef 栈：每层记 (宏名, 是否处于 else 分支)
    stack = []
    for lineno, line in enumerate(code.splitlines(), 1):
        mm = IFDEF_RE.match(line)
        if mm:
            kw, macro = mm.group(1), (mm.group(2) or "")
            if kw in ("ifdef", "ifndef"):
                stack.append([macro, False])
            elif kw == "else":
                if stack:
                    stack[-1][1] = True
            elif kw == "endif":
                if stack:
                    stack.pop()
            continue
        for callee, _fun in CALL_RE.findall(line):
            if callee not in mod2feature:
                continue
            guarded = False
            for macro, in_else in stack:
                feat = feature_of_macro.get(macro)
                if in_else or feat is None:
                    continue
                if feat in mod2feature[callee]:
                    guarded = True
                    break
            if not guarded:
                before = line[:CALL_RE.search(line).start()] if CALL_RE.search(line) else ""
                prot = "defensive" if re.search(r"\bcatch\b", before) else "hard"
                findings.append((lineno, path.stem, callee,
                                 ",".join(sorted(mod2feature[callee])), prot))
    return findings


def load_baseline(path):
    """读 baseline TSV：caller\\tcallee\\tprotection\\treason → {(caller, callee): protection}。"""
    base = {}
    if not path:
        return base
    p = Path(path)
    if not p.is_file():
        raise RuntimeError(f"baseline 文件不存在: {p}")
    lines = [l for l in p.read_text().splitlines() if l.strip() and not l.startswith("#")]
    hdr = lines[0].split("\t")
    if hdr[:3] != ["caller", "callee", "protection"]:
        raise RuntimeError(f"baseline 表头不对: {hdr[:3]}")
    for l in lines[1:]:
        c = l.split("\t")
        if len(c) < 3:
            raise RuntimeError(f"baseline 行字段不足: {l!r}")
        base[(c[0], c[1])] = c[2]
    return base


def apply_baseline(findings, base):
    """豁免与 baseline 精确匹配且双侧均为 defensive 的条目。返回 (剩余, 豁免, 被拒绝的豁免)。"""
    keep, waived, rejected = [], [], []
    for f in findings:
        _lineno, caller, callee, _feats, prot = f
        want = base.get((caller, callee))
        if want is not None:
            # baseline 里登记的防护级别必须与当前一致，且只有 defensive 可豁免；
            # 条目若已改成 hard（比如有人把 catch 删了）必须重新暴露。
            if want == "defensive" and prot == "defensive":
                waived.append(f)
                continue
            rejected.append((f, want))
        keep.append(f)
    return keep, waived, rejected


def run(wt_arg, baseline_arg=None):
    wt = resolve_wt(wt_arg)
    if not (wt / "src").is_dir():
        print(f"[FAIL] 仓库根不可用: {wt}", file=sys.stderr)
        return 2
    by_feature, mod2feature = load_feature_modules(wt)
    feature_of_macro = {"IMBOY_FEATURE_" + f.upper(): f for f in by_feature}
    src = all_src_modules(wt)
    gated = set(mod2feature)
    print("== 特性裁剪调用门 ==")
    print(f"  repo: {wt}")
    print(f"  被裁剪模块集: {len(gated)} 个（来自生成器，{len(by_feature)} 个特性）")
    always = sorted(m for m in src if m not in gated)
    print(f"  始终编译的模块: {len(always)} 个（在 src/** 里）")
    print(f"  认识的 ifdef 宏: {len(feature_of_macro)} 个")
    findings = []
    for m in always:
        findings.extend(scan_file(src[m], mod2feature, feature_of_macro))
    base = load_baseline(baseline_arg) if baseline_arg else {}
    findings, waived, rejected = apply_baseline(findings, base)
    print()
    for _f, want in rejected:
        print(f"  [NOTE] baseline 条目防护级别漂移（登记 {want}），重新计入违规，须复核 baseline")
    for _lineno, caller, callee, _feats, _prot in waived:
        print(f"  [BASELINE] {caller} -> {callee} 按登记豁免（defensive 既有条目）")
    if not findings:
        print("[PASS] 没有任何「始终编译的模块未加保护地调用被裁剪模块」的地方。")
    else:
        hard = [f for f in findings if f[4] == "hard"]
        print(f"[FAIL] 发现 {len(findings)} 处未保护的跨裁剪调用"
              f"（其中 **{len(hard)} 处是裸调用**，未选中该特性时运行期必崩；"
              "其余被 catch 吞掉，降级为静默失效）：")
        for lineno, caller, callee, feats, prot in findings:
            mark = "**裸调用**" if prot == "hard" else "  catch 包住"
            print(f"  {caller}.erl:{lineno}  ->  {callee}  (特性: {feats})  [{mark}]")
    return 1 if findings else 0


# ---------------------------------------------------------------- 自测
def selftest():
    """正反都要证：保护住的调用放行；未保护的调用必红；baseline 只放行 defensive。"""
    results = []

    def case(label, expect, fn):
        got = bool(fn())
        ok = got == expect
        print(f"[{'PASS' if ok else 'FAIL'}] {label} -> {'命中' if got else '无命中'}"
              f"{'' if ok else ' （期望相反）'}")
        results.append(ok)

    mod2feature = {"eb_auth_principal": {"enterprise_business"},
                   "moment_ds": {"moment"}}
    fom = {"IMBOY_FEATURE_ENTERPRISE_BUSINESS": "enterprise_business",
           "IMBOY_FEATURE_MOMENT": "moment"}

    with tempfile.TemporaryDirectory() as d:
        p = Path(d) / "caller.erl"

        p.write_text("-module(caller).\nf(P) -> eb_auth_principal:x(P).\n")
        case("S1 未加保护地调用被裁剪模块 -> 必报（且判为裸调用）",
             True, lambda: len(scan_file(p, mod2feature, fom)) == 1
             and scan_file(p, mod2feature, fom)[0][4] == "hard")

        p.write_text("-module(caller).\n-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).\n"
                     "f(P) -> eb_auth_principal:x(P).\n-endif.\n")
        case("S2 有 -ifdef(该特性) 保护 -> 放行",
             False, lambda: len(scan_file(p, mod2feature, fom)) == 1)

        # 保护了**别的**特性不算保护
        p.write_text("-module(caller).\n-ifdef(IMBOY_FEATURE_MOMENT).\n"
                     "f(P) -> eb_auth_principal:x(P).\n-endif.\n")
        case("S3 只保护了别的特性 -> 仍必报",
             True, lambda: len(scan_file(p, mod2feature, fom)) == 1)

        # -else 分支既不安全也不受保护
        p.write_text("-module(caller).\n-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).\n"
                     "g() -> ok.\n-else.\nf(P) -> eb_auth_principal:x(P).\n-endif.\n")
        case("S4 调用在 -else 分支里 -> 必报（else 不是保护）",
             True, lambda: len(scan_file(p, mod2feature, fom)) == 1)

        # 注释与字符串里的提及不得误报
        p.write_text("-module(caller).\n%% 说明：eb_auth_principal:x/1 由路由侧消费\n"
                     "f() -> \"eb_auth_principal:y\".\n")
        case("S5 注释/字符串里的提及 -> 不得误报",
             False, lambda: len(scan_file(p, mod2feature, fom)) == 1)

        # 同行 catch 包住的裸调用：仍要报，但严重度标为 defensive
        p.write_text("-module(caller).\nf(P) -> case catch eb_auth_principal:x(P) of ok -> ok; _ -> err end.\n")
        fs5 = scan_file(p, mod2feature, fom)
        case("S7 catch 包住的调用仍上报，但标 defensive",
             True, lambda: len(fs5) == 1 and fs5[0][4] == "defensive")

        # 行号必须对得上（报告要能跳到那一行）
        p.write_text("-module(caller).\n\n\nf(P) -> eb_auth_principal:x(P).\n")
        fs = scan_file(p, mod2feature, fom)
        case("S6 报告的行号指向真实调用行",
             True, lambda: len(fs) == 1 and fs[0][0] == 4)

        # baseline：defensive 精确匹配才豁免
        hard1 = [(42, "caller", "eb_auth_principal", "enterprise_business", "hard")]
        def1 = [(43, "caller", "moment_ds", "moment", "defensive")]
        base = {("caller", "moment_ds"): "defensive",
                ("caller", "eb_auth_principal"): "hard"}
        keep, waived, rejected = apply_baseline(def1, base)
        case("S8 baseline 豁免 defensive 既有条目",
             True, lambda: len(keep) == 0 and len(waived) == 1 and not rejected)
        keep, waived, rejected = apply_baseline(hard1, base)
        case("S9 baseline 永不豁免 hard 裸调用",
             True, lambda: len(keep) == 1 and not waived)
        drift = [(50, "adm_moderation_logic", "moment_ds", "moment", "hard")]
        drift_base = {("adm_moderation_logic", "moment_ds"): "defensive"}
        keep, waived, rejected = apply_baseline(drift, drift_base)
        case("S10 baseline 条目防护级别漂移（catch 被删）-> 重新计入违规",
             True, lambda: len(keep) == 1 and rejected and rejected[0][1] == "defensive")

    if not all(results):
        print(f"VERIFIER_FAIL: feature prune calls selftest: {results.count(False)}/{len(results)} 失败")
        return 1
    print(f"VERIFIER_OK: feature prune calls selftest: {len(results)}/{len(results)} 通过"
          "（未保护/已保护/保护错特性/else/注释不误报/catch 标级/行号 + baseline 三态）")
    return 0


def main():
    args = sys.argv[1:]
    if not args:
        print(__doc__)
        return 2
    if args[0] == "--selftest":
        return selftest()
    wt = args[0] if not args[0].startswith("--") else None
    baseline = None
    if "--baseline" in args:
        baseline = args[args.index("--baseline") + 1]
    try:
        return run(wt, baseline)
    except RuntimeError as e:
        print(f"[FAIL] {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
