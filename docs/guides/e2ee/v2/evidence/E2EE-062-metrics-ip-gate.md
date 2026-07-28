# `/metrics` 内网 IP 门的接线实证

- **Slice**：上一刀残留 2「HTTP 层未覆盖」
- **会话**：`20260729-1900-claude-code`
- **仓库**：`imboy`
- **状态**：该残留**已闭合**。E2EE-062 整体仍为 `PARTIAL`
- **本刀不改任何生产代码**（只新增测试 + 门禁清单一行）

---

## 1. 先核实两个假设，两个都不成立

动手前先查了两条「可能有洞」的猜想，**都核实为已覆盖**，故不制造伪工作：

### 1.1 IPv4-mapped IPv6 会被误判为外网？

`is_internal_ip/1` 只匹配 IPv4 四元组与 IPv6 loopback `::1`，
不认 `::ffff:127.0.0.1`（`{0,0,0,0,0,16#ffff,...}`）。若监听在 IPv6 双栈上，
IPv4 客户端会以 mapped 形式出现 → 被判外网 → 403（fail-closed，但会打断抓取）。

**核实：不可达。** `imboy_app:start_clear/2` 只传 `[{port, Port}]`，
未指定 `{ip, _}`，ranch 默认 IPv4（`{0,0,0,0}`），peer 恒为 IPv4 四元组。
**认识论状态：已实证（读监听配置）；若将来显式改监听为 IPv6 双栈，此结论失效。**

### 1.2 反代之后 TCP peer 恒为内网，这道门形同虚设？

**核实：已在入口拦截。** `deploy/nginx/templates/imboy.conf.template:43-48`：

```nginx
# 指标端点禁止公网访问：Prometheus 走 docker 内网 imboy_backend:9800/metrics 抓取。
# 后端 is_internal_ip 校验 TCP peer，反代后 peer 是内网 IP 会被放行，故必须入口拦截，
# 否则泄露 License/用户数/系统指标。
location ~ ^/(api/)?(v1/)?metrics$ { return 403; }
```

注释逐字写明了与我推导相同的理由。**已覆盖，不需要改动。**

---

## 2. 真正的缺口：**第二层防线在某些部署里是唯一防线，却没有任何测试**

`deploy/helm/values.yaml` 给 Pod 加了 `prometheus.io/path: "/metrics"` 注解——
**k8s 抓取路径不经过 nginx**。此时 `metrics_handler:init/2` 的
`is_internal_ip` 门就是**唯一**防护。

而这一层此前**没有任何测试**：被重构掉不会有人发现，nginx 部署照常安全，
k8s 部署静默暴露。

暴露的内容不只是系统指标——E2EE-062 刚加的
`olm_otk_exhausted_total` / `olm_prekey_unavailable_total`
**会泄漏攻击活动**（某段时间内耗尽发生得多频繁），
等于把攻击进展反馈给攻击者。

新增 `test/api/e2ee_metrics_ip_gate_tests.erl`（3 例，**已入 e2ee-verify 门禁**）。

---

## 3. RED 记录 —— 空验证

生产代码本来就有这道门，直接跑必然全绿。故 RED 用空验证取得：
把 `init/2` 里的 `is_internal_ip` 分支临时摘除，直接 `serve_metrics`。

```
  Failed: 1.  Skipped: 0.  Passed: 2.
```

**唯独「外网 peer → 403 且不得触达指标读取」变红**，
对照组与正向可用性仍绿。恢复后 `git diff --stat src/api/metrics_handler.erl` 无输出。

### 3.1 三条用例各自的职责

| 用例 | 挡住的失效 |
|---|---|
| **对照组** `is_internal_ip` 分类正确 | 判据本身就错。它红则后两条的结论都不成立。含 `172.15` / `172.32` 两个**紧贴 RFC-1918 边界**的外网地址 |
| 外网 peer → 403 **且不触达指标读取** | 门被摘除 / 被绕过。用 `elib_metric:get_all_metrics` 抛错来断言"一个字节都不读" |
| **正向可用性** 内网 peer 照常拿到指标 | **一个"一律 403"的实现在"不泄漏"上恒得满分**；没有这条，把整个端点关掉也能让上一条通过。且断言响应体**真的含指标内容**，不是空的 200 |

---

## 4. 生产调用方核实

```
路由 /metrics、/api/metrics、/api/v1/metrics → metrics_handler:init/2   ← 被测入口
  → cowboy_req:peer/1 → is_internal_ip/1 → 403 | serve_metrics
```

测试打的是真实 `init/2`，不是内部私有函数。

---

## 5. 验收命令与结果

```
$ IMBOYENV=local make eunit t=e2ee_metrics_ip_gate_tests
  All 3 tests passed.

$ make e2ee-verify
  All 357 tests passed.        # 上一轮基线 354，本刀 +3
=== E2EE verify ALL PASSED ===

$ erlfmt --check test/api/e2ee_metrics_ip_gate_tests.erl
  All matched files use erlfmt code style!

$ git diff --check
  （通过）
```

> 运行中出现 `event: metrics_ranch_info_failed` —— 测试环境没有 ranch listener，
> `collect_system_metrics/0` 已 catch 并降级为 warning，不影响断言。属既有行为。

imboyapp 侧未改动。

---

## 6. 残留风险

1. **告警规则仍未做** —— 指标能导出、端点有门，但没有任何 Prometheus rule /
   阈值 / 通知渠道。运维仍需**主动去看**。**这是"运维不再盲"的最后一段。**
2. **若将来把监听显式改为 IPv6 双栈，§1.1 的结论失效** ——
   `is_internal_ip/1` 不认 IPv4-mapped IPv6，会把内网抓取判成外网（fail-closed，
   但会打断 Prometheus 抓取）。本刀**未加**该分支：当前不可达，
   凭空加分支等于给一个不存在的场景写未被验证的代码。
3. nginx 那一层**本刀只做了读取核实，未实跑验证** ——
   起 nginx 验证 403 属部署侧集成，未做。**认识论状态：配置已实证存在，
   实际行为未实证。**
4. E2EE-062 其余残留不变（fallback 签名非必填、留存期 ≈2 周期、端到端未实证、
   单租户/全局限流、真机等）。

---

## 7. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| `is_internal_ip/1` 对内/外网及 RFC-1918 边界分类正确 | **已实证** |
| 外网 peer → 403 且不读取任何指标 | **已实证** |
| 内网 peer → 照常拿到含内容的指标 | **已实证** |
| 测试非空转（摘掉门即红） | **已实证**（空验证） |
| 监听为 IPv4，IPv4-mapped IPv6 不可达 | **已实证**（`start_clear` 只传 port） |
| nginx 入口已拦截 `/metrics` | **已实证**（配置文件逐行）；**实际行为未实证** |
| k8s 路径不经 nginx，后端门是唯一防线 | **已实证**（helm values 注解） |
| 「运维不再对耗尽攻击盲」 | **仍不成立** —— 缺告警规则 |

---

## 8. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未新增依赖与迁移。
- **未改动任何生产代码**（只加测试与门禁清单一行）。
- **未给 `is_internal_ip/1` 加 IPv4-mapped IPv6 分支**（当前不可达，见 §6.2）。
