# Tech-Debt Audit — VM 时钟写入（elib_dt:now()）全库约束竞态核查

> 背景：EWS-BE-D2 修复（commit 4c973499，main 27e85f76）时发现 src/repo/ 下
> 10+ 模块与 D2 同构使用 VM 时钟写时间列。本审计回答：除 D2 外，全库还存在
> 多少**会产生 CHECK 23514 硬失败的跨时钟竞态面**。
> 方法：从约束侧反查（非从 128 个写入文件正查），语句级跨行兜底防漏检。
> Executed: 2026-10-04，基于 main 27e85f76。

## 结论（TL;DR）

**D2 是全库唯一一例跨时钟 CHECK 竞态，已修复；残留硬竞态面 = 0。**
其余约 128 个 VM 时钟写入文件构成"双时钟债务"：只会产生毫秒级审计时间戳偏差
与应用层 WHERE 逻辑噪声，不触发任何数据库约束硬失败。建议作为长期规范演进
（新代码统一 DB 时钟写入），不回溯改造。

## 审计范围与方法

- 迁移 DDL：`priv/migrations/` 共 **163 个 up 迁移、530 个 CHECK 约束**。
- 扫描面：①逐行 CHECK；②语句级（RS=";"）跨行 CHECK 兜底；③partial index
  WHERE 谓词；④触发器/函数体内时间列共现点。时间列词表：
  created_at/updated_at/consumed_at/expired_at/expires_at/issued_at/revoked_at/
  completed_at/acked_at/deleted_at/start_time/end_time/*_time。

## 跨时间列比较 CHECK：全库有且仅有 4 个

| 约束 | 表 | 迁移 | 判定 |
|---|---|---|---|
| ck_eosc_consumed_after_created | enterprise_oa_sso_code | 00000136 | **已修复**（D2，4c973499：created_at 改 DB CURRENT_TIMESTAMP 单一时钟源） |
| ck_cswk_expiry (expires_at > created_at) | customer_service_widget_identity_key | 00000132 | 安全（见下） |
| ck_cswn_expiry (expires_at > created_at) | customer_service_widget_nonce | 00000132 | 安全（见下） |
| ck_csvt_expiry (expires_at > created_at) | customer_service_visit_token | 00000125 | 安全（见下） |

**客服 3 约束安全论证**：
- created_at：三表 DDL 均 `DEFAULT CURRENT_TIMESTAMP NOT NULL`（DB 事务时钟）；
  repo INSERT（cs_pg_widget.erl / cs_pg_token.erl）列清单均不含 created_at——
  即写入侧已经是 DB 单时钟，与 D2 修复后形态同构。
- expires_at：应用传入，`cs_widget_support:token_ttl/1` 有 `Ttl > 0` 整数秒
  守卫，默认 `?DEFAULT_TOKEN_TTL = 3600`（1 小时）；identity key 为 epoch 秒、
  天级。约束翻转需要 TTL ≤ VM−DB 时钟差；时钟差为毫秒级（NTP 同步域），
  TTL ≥ 1 秒即不可能翻转。唯一残余前提是系统时钟大漂移（运维域，非代码缺陷）。

## partial index 谓词

全部命中的 WHERE 均为单时间列谓词或非时间谓词
（`client_ack=false`、`status=0`、`pinned=true`、`IS NOT NULL` 等），
**无跨列时间比较**——partial index 不会因写入时钟差异产生建索引冲突。

## 触发器/函数体（4 处命中，逐一判定）

| 位置 | 形态 | 判定 |
|---|---|---|
| 00000001_foundation:474-483（dc.updated_at） | 视图/函数 SELECT 列清单 | 非比较，安全 |
| 00000001_foundation:440（s.expired_at < CURRENT_TIMESTAMP） | 单列 vs DB 时钟的状态判定（EXPIRED 标记） | DB 单时钟，安全 |
| 00000009_constraints_views_triggers:99 | 视图列清单 | 非比较，安全 |
| 00000113_organization_member:42-58 | INSERT created_at/updated_at 全 CURRENT_TIMESTAMP | DB 单时钟自写，安全 |

## 双时钟债务清单（不修，记录性质）

`elib_dt:now()` 写入文件约 128 个，分层：src/repo 45、src/ds 29、src/logic 39、
src/api 7、src/adm 5、其余 features/lib 等。典型风险仅两类、均为毫秒级噪声：
1. 审计时间戳（created_at/updated_at）与 DB 端时间列并读时可能有毫秒偏差；
2. 应用层把 VM now 参数化进 WHERE 与 DB 写入列比较（过期/超时判定）可提前/
   滞后毫秒级——业务 TTL 均为秒级以上，无实际影响。
长期方向：新代码时间列写入统一 DB 端（DEFAULT CURRENT_TIMESTAMP / 语句内
CURRENT_TIMESTAMP），存量不回溯；如后续某表新增跨时间列 CHECK，写入侧必须
单一 DB 时钟（D2 模式）。
