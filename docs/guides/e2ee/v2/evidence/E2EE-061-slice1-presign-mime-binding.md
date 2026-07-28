# E2EE-061 Slice 1：presigned PUT 的 MIME 绑定形态 —— 实证

- **Slice**：`27-e2ee-061-attachment-encryption-design.md` §5 切片计划 **Slice 1**
- **会话**：`20260729-0700-claude-code`
- **仓库**：`imboy`
- **状态**：Slice 1 完成；**E2EE-061 整体仍为 `PENDING`**
- **本刀不改任何生产代码**，只新增一个只读探针单测

---

## 1. 做了什么

设计文档 §3.2 原写着：

> ⚠️ **presigned URL 的签名通常覆盖 Content-Type**：只改 PUT 不改 presign 请求会导致
> 签名失配、直传直接失败。两侧必须同一刀改完，否则上传全线中断。
> **认识论状态：Garage presign 是否把 Content-Type 纳入签名，未实证**——
> 实施前必须先在本地 Garage 上验证，不得凭 S3 通例推断。

本刀去验它。

### 1.1 先纠正一个方法论错误：**找错了求证对象**

本地 Garage 未运行（`127.0.0.1:3900` 无监听，`lsof` 零命中）。
但更重要的是：**这个问题的权威来源根本不是 Garage，而是我方的签名实现。**

presigned URL 由 `elib_s3_sign:presign_url/6` 生成，**签名覆盖哪些内容由这段代码
决定**，Garage 只是按 SigV4 规则校验我们签了什么。原设计要求「先验 Garage」
是把求证对象指错了地方——这本身就是「静态阅读只能形成假设」的一个变体：
不仅结论可能错，**连"该去问谁"都可能错**。

---

## 2. 实证结果

新增 `test/lib/e2ee_presign_mime_binding_tests.erl`（5 例），
直接读 `elib_s3_sign:presign_put/5` / `presign_get/4` 的产物。

```
$ IMBOYENV=local make eunit t=e2ee_presign_mime_binding_tests
  All 5 tests passed.
```

| # | 性质 | 结果 |
|---|---|---|
| 1 | **对照组**：签名随输入变化（不同 object_key → 不同签名） | **成立** |
| 2 | `X-Amz-SignedHeaders` 内容 | **只有 `host`** —— PUT 请求的 Content-Type **请求头不被签名覆盖** |
| 3 | MIME 在何处进签名 | 以 **query 参数** `Content-Type=<mime>` 进入 canonical query string，**因而被绑进签名** |
| 4 | 不同 MIME → 不同签名 | **成立**（改 MIME 必须重新 presign） |
| 5 | **正向可用性**：空 MIME 不产出 `Content-Type=` 参数 | **成立**（`presign_get`/`presign_delete` 走此路，未被本查证破坏） |

对照组（#1）改前改后都绿；若它红，说明探针没取到真签名（如 URL 形状变了、
`binary:split` 取错段），后面四条的任何结论都不成立。

---

## 3. 结论：原表述被推翻，且**修正后的结论更强**

1. **只改客户端 PUT 请求头不会导致签名失配**——请求头不在 `SignedHeaders` 里。
   原文「两侧必须同一刀改完，否则上传全线中断」**不成立**。
2. **但只改请求头也毫无用处**：MIME 已经写在 presigned URL 的 query 参数里。
3. **真正的问题范围比原设计大**：不是「PUT 与 presign 同刀改」，而是**整个
   presign/confirm 契约**——服务端在 `presign` 请求里收 `mime_type`、
   在 `confirm` 里又存一次（`attachment_api.dart` 的 `confirmBody`）。
   **服务端本来就知道真实 MIME。** 隐藏 MIME 必须改这两处契约。

对切片计划的直接影响：**Slice 4（上传侧接线）必须连带改 presign 与 confirm 的
MIME 契约**，而不只是改客户端请求头。已写回设计 §3.2 与 §5。

---

## 4. RED 记录

**不适用**——本刀是**只读查证**，不改生产代码，因此没有可复现的行为缺陷。

替代验收：**对照组**（#1）承担 harness 有效性的职责；
**正向可用性用例**（#5）承担「不因查证而收紧坏既有形态」的职责。
两者都不是「拒收即满分」的指标。

---

## 5. 验收命令与结果

```
$ IMBOYENV=local make eunit t=e2ee_presign_mime_binding_tests
  All 5 tests passed.

$ make e2ee-verify
  All 338 tests passed.        # 上一轮基线 333，本刀 +5
=== E2EE verify ALL PASSED ===

$ erlfmt --check test/lib/e2ee_presign_mime_binding_tests.erl
  All matched files use erlfmt code style!

$ git diff --check
  （通过）
```

新模块 `e2ee_presign_mime_binding_tests` 是**后端单测**，
按规定**已加进 Makefile 的 e2ee-verify Modules 清单**。

---

## 6. 残留风险

1. **Garage 是否校验请求头与 query 参数一致，仍未实证** —— 本地 Garage 未运行。
   该问题只影响「改了 presign 后客户端要不要同步改请求头」这一细节，
   **不影响 §3 的三条结论**。解除条件：启动本地 Garage
   （`scripts/garage-local-setup.sh`）。**loop 未自行启动它**——
   建 bucket / key / layout 是有副作用的基础设施操作，不属只读查证。
2. **MIME 隐藏的完整方案未设计** —— 只确认了问题范围（presign + confirm 契约），
   具体改法属 Slice 4/5，且牵涉 §6 的人工拍板项（服务端失去附件元数据能力）。
3. E2EE-061 其余八刀未动；实施仍需人工确认（队列规定）。
4. E2EE-062 既有残留不变。

---

## 7. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 签名随输入变化（对照组） | **已实证** |
| `SignedHeaders` 只含 `host` | **已实证** |
| MIME 以 query 参数被绑进签名 | **已实证** |
| 不同 MIME → 不同签名，改 MIME 必须重新 presign | **已实证** |
| 空 MIME 不产出 `Content-Type=` 参数 | **已实证** |
| 服务端在 presign 与 confirm 两处都拿到真实 MIME | **已实证**（`elib_s3_sign` + `attachment_api.dart` 逐行） |
| 原「签名覆盖 Content-Type 请求头」表述 | **已被推翻** |
| Garage 是否校验请求头与 query 参数一致 | **未实证**（不影响上述结论） |

---

## 8. 未做

- **未实施附件加密的任何生产代码**；未新增迁移、依赖、配置项。
- **未启动本地 Garage**（有副作用的基础设施操作，不属只读查证）。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除或 skip 任何测试。
- 不 push、不部署、不访问生产、不通知第三方。
