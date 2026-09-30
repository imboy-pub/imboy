# 计划合同修订 #1 — SC-BE-A05 状态码口径

> 修订日期：2026-09-30
> 修订对象：`2026-09-28-customer-service-seat-console-embed-mall-admin-implementation-plan-v2.md` §9 SC-BE-A05
> 被修订计划 SHA-256：`580a1b16a17e61f42bb4982a82a4c4b6ce963ca71943f943d356d410608793fe`（计划原文与其 `.sha256` 文件保持不动，以维持冻结证据链——SC-00-A03 快照与 candidate-manifest 的 `plan_sha256` 绑定；本文件为该计划的正式 change-order，由 acceptance-ledger SC-BE-A05 条目引用）
> 授权来源：用户 2026-09-30 第二轮独立评审（"就 API 设计而言，我更推荐 B：路径语法错误返回 400，合法但不可用的 ID 统一 404……必须修改计划合同，不能静默偏离"）及同日执行指令"按上面评审改进计划"

## 1. 原文（被修订）

> | SC-BE-A05 | `/seat/:public_id` 对合法 active 返回 200；missing/invalid/revoked 统一 404；非 GET 405；credential query 400 |

## 2. 修订后

> | SC-BE-A05 | `/seat/:public_id` 对合法 active 返回 200；合法形状但不存在（missing）与已停用（revoked）统一 404；URL 形状非法（非合法 ID 语法）返回 400 `invalid_public_seat_console_id`；非 GET 405；credential query 400 |

## 3. 理由

1. 防枚举要求的是"合法形状但不可用"的 ID 响应不可区分（missing/revoked 统一 404）；该目标不要求把调用方的路径语法错误也伪装成 404。
2. 非法形状是调用方缺陷：400 + 明确错误码 `invalid_public_seat_console_id` 在 REST 语义与运维定位上均优于 404。
3. 现行实现与测试已锁定该行为（`src/features/customer_service/interfaces/cs_seat_console_handler.erl:53`、`test/features/customer_service/interfaces/cs_seat_console_handler_tests.erl:214`）；本修订使合同与实现一致，而非放松任何 oracle。
4. 公开 ID 非凭证（计划 §1 决策 2），非法形状路径不泄露任何存在性信息。

## 4. 影响范围

- 仅 SC-BE-A05 按修订口径验收。missing→404（SC-INT-A03 T1 `unknown→404 seat_console_unavailable`）、revoked→404（T3）、合法 active→200（T2）、非 GET 405 与 credential query 400（route contract 22/22 + handler eunit）均已实证。
- 其余 66 条 Required Acceptance 与计划其余条款不受影响。
- 本修订不降低任何 oracle 强度，仅消除合同文本与实现之间的口径冲突。
