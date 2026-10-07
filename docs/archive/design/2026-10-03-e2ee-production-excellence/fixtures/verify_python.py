#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""C09（run-20261003-094804）golden vectors 第二实现交叉验证（AC-19）。

按 identity-transparency-profile.md §3 规范用 Python 3 标准库**独立重写**：
  - canonical 编码（key=value\\n，key UTF-8 字节序，末行无换行，fail-closed）
  - RFC 6962 MTH / PATH / SUBPROOF（递归直译）+ 迭代验证算法
  - 域分离前缀 0x00/0x01/0x02 + 交叉签名域前缀 0x03（§3.6，fix-round1）
  - RFC 8032 Ed25519 纯实现（keypair/sign/verify）

与 Erlang（e2ee_kt_merkle 生产 beam + jose_jwa_ed25519）零代码共享。
对每个 vector 做三向断言：Erlang 产出（vectors.json 记录值）== Python 重算 == 预期接受/拒绝。

fix-round1（W1 评审 M1/M2/m5）：
  - 新增 S5 交叉签名 vectors 验证（R1/R2/R3 正例 + 伪造/quorum 不足/凑数/
    域混淆/换叶重放负例），cross-sign 输入按 §3.6 = 0x03‖domain‖0x00‖leaf_bytes。
  - RFC 8032 §7.1 官方测试向量（TEST 1/TEST 3）自检纳入脚本（此前为开发期手工）。

用法：python3 verify_python.py [vectors.json]
退出码：0 全部一致；1 存在不一致。
"""
import hashlib
import json
import sys

# ---------------------------------------------------------------------------
# canonical bytes（profile §3.1）
# ---------------------------------------------------------------------------

class CanonicalError(Exception):
    """fail-closed 拒绝（对齐 e2ee_kt_merkle 的 {error, _}）。"""


def _render(v):
    if isinstance(v, bool):  # bool 是 int 子类，先拒（Erlang 侧无布尔 value）
        raise CanonicalError("boolean_value")
    if isinstance(v, int):
        return str(v)
    if isinstance(v, str):
        return v
    raise CanonicalError("unsupported_value_type")


def canonical(fields):
    if not isinstance(fields, dict):
        raise CanonicalError("not_a_map")
    if len(fields) == 0:
        raise CanonicalError("empty_field_set")
    for k, v in fields.items():
        if not isinstance(k, str):
            raise CanonicalError("non_string_key")
        if "\n" in k or "\r" in k:
            raise CanonicalError("unsafe_field:%s" % k)
        if "=" in k:
            raise CanonicalError("unsafe_field:%s" % k)
        s = _render(v)
        if "\n" in s or "\r" in s:
            raise CanonicalError("unsafe_field:%s" % k)
    pairs = sorted(fields.items(), key=lambda kv: kv[0].encode("utf-8"))
    return "\n".join("%s=%s" % (k, _render(v)) for k, v in pairs).encode("utf-8")


# ---------------------------------------------------------------------------
# RFC 6962 MTH / PATH / SUBPROOF（profile §2/§4，域分离 0x00/0x01）
# ---------------------------------------------------------------------------

def sha(b):
    return hashlib.sha256(b).digest()


def leaf_hash(data):
    return sha(b"\x00" + data)


def node_hash(l, r):
    return sha(b"\x01" + l + r)


def _lp2(n):  # 小于 n 的最大 2 的幂（n>=2）
    k = 1
    while k * 2 < n:
        k *= 2
    return k


def mth(ds):
    n = len(ds)
    if n == 0:
        return sha(b"")
    if n == 1:
        return leaf_hash(ds[0])
    k = _lp2(n)
    return node_hash(mth(ds[:k]), mth(ds[k:]))


def inclusion_path(m, ds):
    """PATH(m, D[n])，RFC 6962 §2.1.1 递归直译。"""
    n = len(ds)
    if n == 1:
        return []
    k = _lp2(n)
    if m < k:
        return inclusion_path(m, ds[:k]) + [mth(ds[k:])]
    return inclusion_path(m - k, ds[k:]) + [mth(ds[:k])]


def consistency_path(m, ds):
    """PROOF(m, D[n])，RFC 6962 §2.1.2 SUBPROOF 递归直译。"""
    return _subproof(m, ds, True)


def _subproof(m, ds, b):
    n = len(ds)
    if m == n:
        return [] if b else [mth(ds)]
    k = _lp2(n)
    if m <= k:
        return _subproof(m, ds[:k], b) + [mth(ds[k:])]
    return _subproof(m - k, ds[k:], False) + [mth(ds[:k])]


def verify_inclusion(leaf, m, n, path, root):
    """RFC 6962 §2.1.1 迭代验证（验证方无整棵树）。"""
    if n <= 0 or m >= n:
        return False
    for p in path:
        if len(p) != 32:
            return False
    fn, sn, r = m, n - 1, leaf
    for p in path:
        if sn == 0:
            return False
        if (fn & 1) == 1 or fn == sn:
            r = node_hash(p, r)
            while (fn & 1) == 0 and fn != 0:
                fn >>= 1
                sn >>= 1
            fn >>= 1
            sn >>= 1
        else:
            r = node_hash(r, p)
            fn >>= 1
            sn >>= 1
    return sn == 0 and r == root


def _shift_while_even(fn, sn):
    while fn != 0 and (fn & 1) == 0:
        fn >>= 1
        sn >>= 1
    return fn, sn


def verify_consistency(m, n, path, root1, root2):
    """RFC 6962 §2.1.2 迭代验证（验证方无整棵树）。"""
    if m <= 0 or n <= 0 or m > n:
        return False
    for p in path:
        if len(p) != 32:
            return False
    if m == n:
        return path == [] and root1 == root2
    # shift while both odd-track（m-1 的最低置位）
    node, last = m - 1, n - 1
    while (node & 1) == 1:
        node >>= 1
        last >>= 1
    if node == 0:
        fr = sr = root1  # m 是 2 的幂：旧根即子树根
        rest = path
    else:
        if not path:
            return False
        fr = sr = path[0]
        rest = path[1:]
    for p in rest:
        if last == 0:
            return False
        if (node & 1) == 1 or node == last:
            node, last = _shift_while_even(node, last)
            fr = node_hash(p, fr)
            sr = node_hash(p, sr)
            node >>= 1
            last >>= 1
        else:
            sr = node_hash(sr, p)
            node >>= 1
            last >>= 1
    return last == 0 and fr == root1 and sr == root2


# ---------------------------------------------------------------------------
# Ed25519（RFC 8032 纯实现）
# ---------------------------------------------------------------------------

_P = 2 ** 255 - 19
_L = 2 ** 252 + 27742317777372353535851937790883648493
_D = (-121665 * pow(121666, _P - 2, _P)) % _P
_I = pow(2, (_P - 1) // 4, _P)


def _inv(x):
    return pow(x, _P - 2, _P)


def _xrecover(y):
    xx = (y * y - 1) * _inv(_D * y * y + 1)
    x = pow(xx, (_P + 3) // 8, _P)
    if (x * x - xx) % _P != 0:
        x = (x * _I) % _P
    if (x * x - xx) % _P != 0:
        raise ValueError("no square root")
    if x % 2 != 0:
        x = _P - x
    return x


_BY = (4 * _inv(5)) % _P
_BX = _xrecover(_BY)
_B = (_BX, _BY, 1, (_BX * _BY) % _P)  # extended coords (X,Y,Z,T)
_IDENT = (0, 1, 1, 0)


def _add(p, q):
    (x1, y1, z1, t1), (x2, y2, z2, t2) = p, q
    a = ((y1 - x1) * (y2 - x2)) % _P
    b = ((y1 + x1) * (y2 + x2)) % _P
    c = (2 * t1 * t2 * _D) % _P
    dd = (2 * z1 * z2) % _P
    e, f, g, h = b - a, dd - c, dd + c, b + a
    return ((e * f) % _P, (g * h) % _P, (f * g) % _P, (e * h) % _P)


def _mul(s, p):
    q = _IDENT
    while s > 0:
        if s & 1:
            q = _add(q, p)
        p = _add(p, p)
        s >>= 1
    return q


def _compress(p):
    x, y, z, _ = p
    zi = _inv(z)
    xi = (x * zi) % _P
    yi = (y * zi) % _P
    return (yi | ((xi & 1) << 255)).to_bytes(32, "little")


def _decompress(b):
    s = int.from_bytes(b, "little")
    y = s & ((1 << 255) - 1)
    x = _xrecover(y)
    if x & 1 != (s >> 255):
        x = _P - x
    p = (x, y, 1, (x * y) % _P)
    if not _on_curve(p):
        raise ValueError("not on curve")
    return p


def _on_curve(p):
    x, y, z, t = p
    return (z % _P != 0 and (x * y - z * t) % _P == 0 and
            (y * y - x * x - z * z - _D * t * t) % _P == 0)


def _clamp(h32):
    a = int.from_bytes(h32, "little")
    a &= (1 << 254) - 8
    a |= 1 << 254
    return a


def ed25519_keypair(seed):
    """RFC 8032 §5.1.5：从 32 字节 seed 派生 (pk, sk_expanded_prefix)。"""
    h = hashlib.sha512(seed).digest()
    a = _clamp(h[:32])
    pk = _compress(_mul(a, _B))
    return pk, h[32:]


def ed25519_sign(msg, seed):
    pk, prefix = ed25519_keypair(seed)
    a = _clamp(hashlib.sha512(seed).digest()[:32])
    r = int.from_bytes(hashlib.sha512(prefix + msg).digest(), "little") % _L
    rr = _compress(_mul(r, _B))
    k = int.from_bytes(hashlib.sha512(rr + pk + msg).digest(), "little") % _L
    s = (r + k * a) % _L
    return rr + s.to_bytes(32, "little")


def ed25519_verify(sig, msg, pk):
    if len(sig) != 64 or len(pk) != 32:
        return False
    try:
        rr = _decompress(sig[:32])
        a = _decompress(pk)
        s = int.from_bytes(sig[32:], "little")
        if s >= _L:
            return False
        k = int.from_bytes(hashlib.sha512(sig[:32] + pk + msg).digest(), "little") % _L
        left = _mul(8 * s, _B)
        right = _add(_mul(8, rr), _mul(8 * k, a))
        return _compress(left) == _compress(right)
    except Exception:
        return False


# ---------------------------------------------------------------------------
# RFC 8032 §7.1 官方测试向量自检（W1 评审 m5：从开发期手工改为脚本内置）
# ---------------------------------------------------------------------------

RFC8032_TEST_VECTORS = [
    # (name, seed_hex, pk_hex, msg_hex, sig_hex)
    ("TEST1",
     "9d61b19deffd5a60ba844af492ec2cc4"
     "4449c5697b326919703bac031cae7f60",
     "d75a980182b10ab7d54bfed3c964073a"
     "0ee172f3daa62325af021a68f707511a",
     "",
     "e5564300c360ac729086e2cc806e828a"
     "84877f1eb8e5d974d873e065224901555"
     "fb8821590a33bacc61e39701cf9b46bd2"
     "5bf5f0595bbe24655141438e7a100b"),
    ("TEST3",
     "c5aa8df43f9f837bedb7442f31dcb7b1"
     "66d38535076f094b85ce3a2e0b4458f7",
     "fc51cd8e6218a1a38da47ed00230f058"
     "0816ed13ba3303ac5deb911548908025",
     "af82",
     "6291d657deec24024827e69c3abe01a3"
     "0ce548a284743a445e3680d7db5ac3ac1"
     "8ff9b538d16f290ae67f760984dc6594a"
     "7c15e9716ed28dc027beceea1ec40a"),
]


def rfc8032_self_test():
    """返回 (ok, failures)：pk 派生与签名均须逐字节命中官方值。"""
    failures = []
    for name, seed_hex, pk_hex, msg_hex, sig_hex in RFC8032_TEST_VECTORS:
        seed, pk_exp = h2b(seed_hex), h2b(pk_hex)
        msg, sig_exp = h2b(msg_hex), h2b(sig_hex)
        pk, _ = ed25519_keypair(seed)
        if pk != pk_exp:
            failures.append("%s:pk" % name)
            continue
        sig = ed25519_sign(msg, seed)
        if sig != sig_exp:
            failures.append("%s:sig" % name)
            continue
        if not ed25519_verify(sig, msg, pk):
            failures.append("%s:verify" % name)
    return (not failures), failures


# ---------------------------------------------------------------------------
# cross-sign 域分离输入（profile §3.6，fix-round1）
# ---------------------------------------------------------------------------

CROSSSIGN_PREFIX = b"\x03"


def crosssign_input(domain, leaf_bytes):
    """0x03 ‖ domain ‖ 0x00 ‖ leaf_bytes（Ed25519 对该原文直接签）。"""
    if "\x00" in domain:
        raise CanonicalError("domain_contains_nul")
    return CROSSSIGN_PREFIX + domain.encode("utf-8") + b"\x00" + leaf_bytes


def quorum_valid_count(leaf_bytes, domain, enrolled_pks, presented_sigs):
    """R3 quorum 判定（§3.2/§3.6）：呈交签名对**登记**公钥集逐一试验签，
    任一登记公钥下通过即计 1 份；有效份数即返回值（调用方与 threshold 比较）。"""
    inp = crosssign_input(domain, leaf_bytes)
    return sum(1 for sig in presented_sigs
               if any(ed25519_verify(sig, inp, pk) for pk in enrolled_pks))


# ---------------------------------------------------------------------------
# vectors 驱动
# ---------------------------------------------------------------------------

class Report:
    def __init__(self):
        self.rows = []   # (vector_id, check, ok, detail)
        self.fails = 0
        self.count = 0

    def check(self, vid, name, ok, detail=""):
        self.count += 1
        if not ok:
            self.fails += 1
        self.rows.append((vid, name, ok, detail))


def h2b(hexstr):
    return bytes.fromhex(hexstr)


def tree_head_signing_input(head_bytes):
    return sha(b"\x02" + head_bytes)


def load_seed_hex(hexstr):
    return h2b(hexstr)


def verify_scenario(rep, sc, meta, all_scenarios):
    sid = sc["id"]
    if sid == "S4":
        verify_s4(rep, sc, meta)
        return
    if sid == "S5":
        verify_s5(rep, sc, meta, all_scenarios)
        return
    events = sc["events"]
    dbytes = [canonical(e) for e in events]
    hashes_hex = sc["tree"]["leaf_hashes_hex"]
    rep.check(sid, "canonical_bytes", [b.hex() for b in dbytes] == sc["tree"]["canonical_bytes_hex"])
    lhashes = [leaf_hash(b) for b in dbytes]
    rep.check(sid, "leaf_hashes", [h.hex() for h in lhashes] == hashes_hex)
    root = mth(dbytes)
    rep.check(sid, "root_hash", root.hex() == sc["tree"]["root_hash_hex"])

    n = len(events)
    seed = load_seed_hex(sc["signing_key"]["seed_sha256_first32_hex"])
    pk, _ = ed25519_keypair(seed)
    rep.check(sid, "signing_pk", pk.hex() == sc["signing_key"]["public_key_hex"])

    head = sc["tree_head"]
    head_map = {k: v for k, v in head["input_fields"].items()}
    head_bytes = canonical(head_map)
    rep.check(sid, "head_canonical", head_bytes.hex() == head["canonical_hex"])
    s_input = tree_head_signing_input(head_bytes)
    rep.check(sid, "head_signing_input", s_input.hex() == head["signing_input_hex"])
    sig = ed25519_sign(s_input, seed)
    rep.check(sid, "head_signature", sig.hex() == head["signature_hex"])
    rep.check(sid, "head_sig_verify", ed25519_verify(sig, s_input, pk))
    rep.check(sid, "head_key_id", sha(pk).hex() == head["key_id_wire"])

    vs = {v["id"]: v for v in sc["vectors"]}
    v1 = vs[sid + "-V1"]
    m = v1["input"]["leaf_index"]
    e1 = v1["expected"]
    rep.check(v1["id"], "leaf_canonical", dbytes[m].hex() == e1["leaf_canonical_hex"])
    rep.check(v1["id"], "leaf_hash", lhashes[m].hex() == e1["leaf_hash_hex"])
    path_py = inclusion_path(m, dbytes)
    rep.check(v1["id"], "audit_path", [p.hex() for p in path_py] == e1["audit_path_hex"])
    rep.check(v1["id"], "verify_inclusion_py",
              verify_inclusion(lhashes[m], m, n, path_py, root) is True)
    rep.check(v1["id"], "verify_inclusion_expected", e1["verify_inclusion"] is True)

    v2 = vs[sid + "-V2"]
    msize = v2["input"]["first_size"]
    nsize = v2["input"]["second_size"]
    e2 = v2["expected"]
    root_m = mth(dbytes[:msize])
    rep.check(v2["id"], "first_root", root_m.hex() == e2["first_root_hex"])
    rep.check(v2["id"], "second_root", root.hex() == e2["second_root_hex"])
    cpath_py = consistency_path(msize, dbytes)
    rep.check(v2["id"], "consistency_path",
              [p.hex() for p in cpath_py] == e2["consistency_path_hex"])
    rep.check(v2["id"], "verify_consistency_py",
              verify_consistency(msize, nsize, cpath_py, root_m, root) is True)
    rep.check(v2["id"], "verify_consistency_expected", e2["verify_consistency"] is True)

    v3 = vs[sid + "-V3"]
    e3 = v3["expected"]
    wrong_from = v3["input"]["wrong_leaf_hash_from_index"]
    claimed = v3["input"]["claimed_leaf_index"]
    rep.check(v3["id"], "wrong_leaf_hash", lhashes[wrong_from].hex() == e3["wrong_leaf_hash_hex"])
    path_claimed = [h2b(x) for x in e3["audit_path_hex"]]
    ok_wrong = verify_inclusion(lhashes[wrong_from], claimed, n, path_claimed, root)
    rep.check(v3["id"], "wrong_leaf_rejected_py", ok_wrong is False)
    rep.check(v3["id"], "wrong_leaf_expected", e3["verify_inclusion_wrong_leaf"] is False)
    # 编码拒绝三例（fail-closed 单射守卫）
    for name, fields in [("newline_value", {"a": "x\ny"}),
                         ("eq_key", {"x=y": "v"}),
                         ("empty_set", {})]:
        try:
            canonical(fields)
            rep.check(v3["id"], "reject_" + name, False, "no exception")
        except CanonicalError:
            rep.check(v3["id"], "reject_" + name, True)

    v4 = vs[sid + "-V4"]
    e4 = v4["expected"]
    tampered_map = dict(head_map)
    if sid == "S1":
        tampered_map["timestamp_ms"] += 1
    elif sid == "S2":
        tampered_map["tree_size"] += 1
    else:
        tampered_map["log_version"] -= 1
    tampered_bytes = canonical(tampered_map)
    rep.check(v4["id"], "tampered_head_canonical",
              tampered_bytes.hex() == e4["tampered_head_canonical_hex"])
    t_input = tree_head_signing_input(tampered_bytes)
    rep.check(v4["id"], "tampered_signing_input",
              t_input.hex() == e4["tampered_signing_input_hex"])
    rep.check(v4["id"], "signing_input_changed", t_input != s_input)
    orig_sig = h2b(head["signature_hex"])
    rep.check(v4["id"], "orig_sig_rejected",
              ed25519_verify(orig_sig, t_input, pk) is False)
    t_sig_py = ed25519_sign(t_input, seed)
    rep.check(v4["id"], "attacker_resign_py", t_sig_py.hex() == e4["tampered_signature_hex"])
    rep.check(v4["id"], "attacker_resign_verifies",
              ed25519_verify(t_sig_py, t_input, pk) is True)

    v5 = vs[sid + "-V5"]
    e5 = v5["expected"]
    perm = v5["input"]["fork_permutation"]
    fork_bytes = [dbytes[i] for i in perm]
    fork_root = mth(fork_bytes)
    rep.check(v5["id"], "fork_root", fork_root.hex() == e5["fork_root_hash_hex"])
    rep.check(v5["id"], "roots_differ", fork_root != root)
    rep.check(v5["id"], "consistency_same_size_fork",
              verify_consistency(n, n, [], root, fork_root) is False)
    rep.check(v5["id"], "consistency_self",
              verify_consistency(n, n, [], root, root) is True)
    rep.check(v5["id"], "split_view_detected", e5["split_view_detected"] is True)

    # 场景内 Erlang self_checks 全 true（生成侧断言）
    for k, v in sc["self_checks"].items():
        rep.check(sid, "erlang_self_check:" + k, v is True)


def verify_s4(rep, sc, meta):
    for v in sc["vectors"]:
        vid, kind, e = v["id"], v["kind"], v["expected"]
        if kind == "empty_tree":
            rep.check(vid, "empty_root", mth([]) == sha(b"") and
                      mth([]).hex() == e["root_hash_hex"])
            rep.check(vid, "self_check", e["self_check"] is True)
        elif kind == "single_leaf_identity":
            x = b"deployment_id=deploy-imboy-demo"
            # 取 S3-L0 真实事件：由 meta 无法直接取，S3 事件重构造
            s3l0 = {
                "deployment_id": "deploy-imboy-demo",
                "event_type": "root_publish",
                "key_id": "a1b2c3d4e5f60718293a4b5c6d7e8f90"
                          "a1b2c3d4e5f60718293a4b5c6d7e8f90",
                "quorum_threshold": 2,
                "root_ed25519": "Um9vdEVkMjU1MTlfREVPXzAx",
                "subject_type": "root",
                "user_id": 1001,
            }
            xb = canonical(s3l0)
            rep.check(vid, "mth_single_eq_leaf", mth([xb]) == leaf_hash(xb))
            rep.check(vid, "leaf_hash", leaf_hash(xb).hex() == e["leaf_hash_hex"])
            rep.check(vid, "mth_hex", mth([xb]).hex() == e["mth_single_hex"])
        elif kind == "canonical_reject":
            cases = {
                "reject_value_lf": {"k": "a\nb"},
                "reject_value_cr": {"k": "a\rb"},
                "reject_key_eq": {"x=y": "v"},
                "reject_empty": {},
            }
            for name, fields in cases.items():
                try:
                    canonical(fields)
                    rep.check(vid, name, False, "no exception")
                except CanonicalError:
                    rep.check(vid, name, e[name] is True)
        elif kind == "type_mapping":
            a = canonical({"a": 2}).hex()
            b = canonical({"a": "2"}).hex()
            rep.check(vid, "int_eq_binary_render", a == b)
            rep.check(vid, "int_canonical", a == e["int_canonical_hex"])
            rep.check(vid, "binary_canonical", b == e["binary_canonical_hex"])
        elif kind == "consistency_same_size":
            # S1 树重构造（与 S1 events 相同）
            s1 = [
                {"curve25519_key": "Y3VydmUx", "deployment_id": "deploy-imboy-demo",
                 "device_id": "dev-A", "ed25519_key": "ZWQyNTUxOTE=",
                 "event_type": "device_publish", "identity_version": 1,
                 "subject_type": "device", "user_id": 1001},
                {"curve25519_key": "Y3VydmUy", "deployment_id": "deploy-imboy-demo",
                 "device_id": "dev-B", "ed25519_key": "ZWQyNTUxOTI=",
                 "event_type": "device_publish", "identity_version": 1,
                 "subject_type": "device", "user_id": 1001},
                {"curve25519_key": "TkVXX0NVUlZFMQ==", "deployment_id": "deploy-imboy-demo",
                 "device_id": "dev-A", "ed25519_key": "TkVXX0VEMjU1MTlfMQ==",
                 "event_type": "device_rotate", "identity_version": 2,
                 "subject_type": "device", "user_id": 1001},
                {"curve25519_key": "", "deployment_id": "deploy-imboy-demo",
                 "device_id": "dev-B", "ed25519_key": "",
                 "event_type": "device_revoke", "identity_version": 2,
                 "subject_type": "device", "user_id": 1001},
            ]
            dbytes = [canonical(ev) for ev in s1]
            r4 = mth(dbytes)
            rep.check(vid, "s1_root", r4.hex() == e["root_hash_hex"])
            rep.check(vid, "verify_self",
                      verify_consistency(4, 4, [], r4, r4) is True)
            fork = mth([dbytes[1], dbytes[0], dbytes[2], dbytes[3]])
            rep.check(vid, "verify_fork_same_size",
                      verify_consistency(4, 4, [], r4, fork) is False)
            rep.check(vid, "expected_fork_false",
                      e["verify_consistency_fork_same_size"] is False)


def verify_s5(rep, sc, meta, all_scenarios):
    """S5 交叉签名（§3.6）：leaf 从既有场景 events 读取（不硬编码），
    全部签名用 Python RFC 8032 实现重算并逐字节比对，负例独立断言拒绝。"""
    sid = sc["id"]
    by_id = {s["id"]: s for s in all_scenarios}
    domains = sc["domains"]
    # 域常量三方一致（Erlang vectors == profile §3.6 表 == 此处常量）
    rep.check(sid, "domain_device",
              domains["device"] == "imboy.kt.v2.crosssign.device.v1"
              == meta["crosssign_domains"]["device"])
    rep.check(sid, "domain_agent",
              domains["agent"] == "imboy.kt.v2.crosssign.agent.v1"
              == meta["crosssign_domains"]["agent"])
    rep.check(sid, "domain_recovery",
              domains["recovery"] == "imboy.kt.v2.crosssign.recovery.v1"
              == meta["crosssign_domains"]["recovery"])
    rep.check(sid, "crosssign_prefix_meta", meta["crosssign_prefix_hex"] == "03")

    # 六把确定性 key：seed → pk 派生逐把一致
    keys = sc["keys"]
    seeds, pks = {}, {}
    for role, kj in keys.items():
        seed = load_seed_hex(kj["seed_sha256_first32_hex"])
        pk, _ = ed25519_keypair(seed)
        rep.check(sid, "key_pk:" + role, pk.hex() == kj["public_key_hex"])
        seeds[role], pks[role] = seed, pk

    # 被签 leaf：从 S1/S2/S3 events 重算 canonical（复用第二实现 canonical）
    leaf = {
        "S1-L2": canonical(by_id["S1"]["events"][2]),
        "S2-L0": canonical(by_id["S2"]["events"][0]),
        "S3-L0": canonical(by_id["S3"]["events"][0]),
        "S1-L0": canonical(by_id["S1"]["events"][0]),
    }
    enrolled_pks = [pks["recovery_1"], pks["recovery_2"], pks["recovery_3"]]
    vs = {v["id"]: v for v in sc["vectors"]}

    # ---- V1 R1 合法 device cross-sign ----
    v1, e1 = vs["S5-V1"], vs["S5-V1"]["expected"]
    rep.check(v1["id"], "leaf_canonical", leaf["S1-L2"].hex() == e1["leaf_canonical_hex"])
    in1 = crosssign_input(domains["device"], leaf["S1-L2"])
    rep.check(v1["id"], "crosssign_input", in1.hex() == e1["crosssign_input_hex"])
    rep.check(v1["id"], "prefix_byte_0x03", in1[0:1] == b"\x03")
    sig1 = ed25519_sign(in1, seeds["root"])
    rep.check(v1["id"], "signature_recalc", sig1.hex() == e1["signature_hex"])
    rep.check(v1["id"], "verify_with_root_pk_py", ed25519_verify(sig1, in1, pks["root"]))
    rep.check(v1["id"], "verify_expected", e1["verify_with_root_pk"] is True)

    # ---- V2 R2 agent 双背书 ----
    v2, e2 = vs["S5-V2"], vs["S5-V2"]["expected"]
    in2 = crosssign_input(domains["agent"], leaf["S2-L0"])
    rep.check(v2["id"], "crosssign_input", in2.hex() == e2["crosssign_input_hex"])
    sig_r = ed25519_sign(in2, seeds["root"])
    sig_d = ed25519_sign(in2, seeds["deploy"])
    sig_a2 = ed25519_sign(in2, seeds["attacker"])
    rep.check(v2["id"], "root_sig_recalc", sig_r.hex() == e2["root_signature_hex"])
    rep.check(v2["id"], "deploy_sig_recalc", sig_d.hex() == e2["deploy_signature_hex"])
    rep.check(v2["id"], "attacker_sig_recalc", sig_a2.hex() == e2["attacker_signature_hex"])
    rep.check(v2["id"], "verify_root_sig_py", ed25519_verify(sig_r, in2, pks["root"]))
    rep.check(v2["id"], "verify_deploy_sig_py", ed25519_verify(sig_d, in2, pks["deploy"]))
    rep.check(v2["id"], "attacker_rejected_by_root_py",
              ed25519_verify(sig_a2, in2, pks["root"]) is False)
    rep.check(v2["id"], "attacker_rejected_expected",
              e2["verify_attacker_sig_with_root_pk"] is False)
    rep.check(v2["id"], "signatures_differ", sig_r != sig_d
              and e2["signatures_differ"] is True)

    # ---- V3 R3 recovery quorum 2-of-3 ----
    v3, e3 = vs["S5-V3"], vs["S5-V3"]["expected"]
    in3 = crosssign_input(domains["recovery"], leaf["S3-L0"])
    rep.check(v3["id"], "crosssign_input", in3.hex() == e3["crosssign_input_hex"])
    sig3a = ed25519_sign(in3, seeds["recovery_1"])
    sig3b = ed25519_sign(in3, seeds["recovery_2"])
    rep.check(v3["id"], "sig_recalc",
              [sig3a.hex(), sig3b.hex()] == e3["signatures_hex"])
    rep.check(v3["id"], "verify_each_py",
              ed25519_verify(sig3a, in3, pks["recovery_1"])
              and ed25519_verify(sig3b, in3, pks["recovery_2"]))
    thr = v3["input"]["quorum_threshold"]
    cnt3 = quorum_valid_count(leaf["S3-L0"], domains["recovery"],
                              enrolled_pks, [sig3a, sig3b])
    rep.check(v3["id"], "valid_count_py", cnt3 == 2 == e3["valid_count"])
    rep.check(v3["id"], "quorum_satisfied_py", cnt3 >= thr)
    rep.check(v3["id"], "quorum_satisfied_expected", e3["quorum_satisfied"] is True)

    # ---- V4 伪造 root 签名（错钥）拒绝 ----
    v4, e4 = vs["S5-V4"], vs["S5-V4"]["expected"]
    sig_atk = ed25519_sign(in1, seeds["attacker"])  # 与 V1 完全相同的输入
    rep.check(v4["id"], "attacker_sig_recalc", sig_atk.hex() == e4["attacker_signature_hex"])
    rep.check(v4["id"], "attacker_selfverify_py", ed25519_verify(sig_atk, in1, pks["attacker"]))
    rep.check(v4["id"], "forged_rejected_by_root_py",
              ed25519_verify(sig_atk, in1, pks["root"]) is False)
    rep.check(v4["id"], "forged_rejected_expected", e4["verify_with_root_pk"] is False)
    rep.check(v4["id"], "sig_differs_legit", sig_atk != sig1)

    # ---- V5 quorum 不足 / 凑数 / 无签 ----
    v5, e5 = vs["S5-V5"], vs["S5-V5"]["expected"]
    sig_atk3 = ed25519_sign(in3, seeds["attacker"])
    rep.check(v5["id"], "forged_sig_recalc", sig_atk3.hex() == e5["forged_sig_hex"])
    rep.check(v5["id"], "first_valid_sig_recalc", sig3a.hex() == e5["first_valid_sig_hex"])
    c1 = quorum_valid_count(leaf["S3-L0"], domains["recovery"], enrolled_pks, [sig3a])
    c2 = quorum_valid_count(leaf["S3-L0"], domains["recovery"],
                            enrolled_pks, [sig3a, sig_atk3])
    c0 = quorum_valid_count(leaf["S3-L0"], domains["recovery"], enrolled_pks, [])
    rep.check(v5["id"], "one_valid_count_py", c1 == 1 == e5["case_one_valid_sig"]["valid_count"])
    rep.check(v5["id"], "one_valid_insufficient_py", c1 < thr
              and e5["case_one_valid_sig"]["quorum_satisfied"] is False)
    rep.check(v5["id"], "forged_not_counted_py", c2 == 1 == e5["case_one_valid_plus_forged"]["valid_count"])
    rep.check(v5["id"], "forged_not_counted_expected",
              e5["case_one_valid_plus_forged"]["quorum_satisfied"] is False)
    rep.check(v5["id"], "empty_count_py", c0 == 0 == e5["case_no_signatures"]["valid_count"])
    rep.check(v5["id"], "empty_insufficient_expected",
              e5["case_no_signatures"]["quorum_satisfied"] is False)

    # ---- V6 域混淆拒绝（device 域签名当 recovery/agent 域用）----
    v6, e6 = vs["S5-V6"], vs["S5-V6"]["expected"]
    conf_r = crosssign_input(domains["recovery"], leaf["S1-L2"])
    conf_a = crosssign_input(domains["agent"], leaf["S1-L2"])
    rep.check(v6["id"], "device_domain_input", in1.hex() == e6["device_domain_input_hex"])
    rep.check(v6["id"], "recovery_domain_input", conf_r.hex() == e6["recovery_domain_input_hex"])
    rep.check(v6["id"], "agent_domain_input", conf_a.hex() == e6["agent_domain_input_hex"])
    rep.check(v6["id"], "inputs_mutually_distinct",
              len({in1, conf_r, conf_a}) == 3 and e6["inputs_mutually_distinct"] is True)
    rep.check(v6["id"], "device_domain_accepts_py", ed25519_verify(sig1, in1, pks["root"]))
    rep.check(v6["id"], "recovery_domain_rejects_py",
              ed25519_verify(sig1, conf_r, pks["root"]) is False)
    rep.check(v6["id"], "agent_domain_rejects_py",
              ed25519_verify(sig1, conf_a, pks["root"]) is False)
    rep.check(v6["id"], "confusion_expected",
              e6["verify_recovery_domain"] is False and e6["verify_agent_domain"] is False)

    # ---- V7 换叶重放拒绝 ----
    v7, e7 = vs["S5-V7"], vs["S5-V7"]["expected"]
    rep.check(v7["id"], "replay_leaf_canonical",
              leaf["S1-L0"].hex() == e7["replay_leaf_canonical_hex"])
    replay_in = crosssign_input(domains["device"], leaf["S1-L0"])
    rep.check(v7["id"], "replay_input", replay_in.hex() == e7["replay_input_hex"])
    rep.check(v7["id"], "leaves_differ", leaf["S1-L0"] != leaf["S1-L2"]
              and e7["leaves_differ"] is True)
    rep.check(v7["id"], "replay_rejected_py",
              ed25519_verify(sig1, replay_in, pks["root"]) is False)
    rep.check(v7["id"], "replay_rejected_expected",
              e7["verify_replay_with_root_pk"] is False)
    rep.check(v7["id"], "original_still_verifies_py",
              ed25519_verify(sig1, in1, pks["root"]))

    # Erlang 侧 self_checks 全 true
    for k, v in sc["self_checks"].items():
        rep.check(sid, "erlang_self_check:" + k, v is True)


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "vectors.json"
    doc = json.load(open(path, encoding="utf-8"))
    meta = doc["meta"]
    rep = Report()

    # RFC 8032 官方向量自检（先于一切 vectors——实现本身不合格则整体无意义）
    ok8032, fails8032 = rfc8032_self_test()
    rep.check("RFC8032", "official_test_vectors_self_check", ok8032, ",".join(fails8032))

    rep.check("META", "kt_module", meta["kt_module"] == "e2ee_kt_merkle")
    rep.check("META", "vectors_version", meta.get("version") == 2)
    rep.check("META", "erlang_self_check", meta["self_check"] is True)
    # 域分离前缀自校验
    rep.check("META", "leaf_prefix", leaf_hash(b"x") == sha(b"\x00x"))
    rep.check("META", "node_prefix",
              node_hash(b"a" * 32, b"b" * 32) == sha(b"\x01" + b"a" * 32 + b"b" * 32))
    rep.check("META", "head_prefix", tree_head_signing_input(b"h") == sha(b"\x02h"))
    rep.check("META", "crosssign_prefix",
              crosssign_input("d", b"l") == b"\x03d\x00l"
              and crosssign_input("d", b"l")[0:1] != leaf_hash(b"x")[0:1])

    for sc in doc["scenarios"]:
        verify_scenario(rep, sc, meta, doc["scenarios"])

    for vid, name, ok, detail in rep.rows:
        if not ok:
            print("FAIL %-8s %-34s %s" % (vid, name, detail))
    print("checks=%d fails=%d" % (rep.count, rep.fails))
    print("RESULT:", "PASS (Erlang 实算 == Python 规范重算 == vector 记录值)"
          if rep.fails == 0 else "MISMATCH")
    sys.exit(0 if rep.fails == 0 else 1)


if __name__ == "__main__":
    main()
