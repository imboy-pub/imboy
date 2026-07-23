# IMBoy 安全演进路线（Security Roadmap）

> 基于 `docs/review/security-review.md` · 渐进升级,不破坏兼容 · 日期 2026-07-22
> 核心判断：密码学基本面扎实（JWT exp 强制、支付 owner_uid 红线、E2EE 零明文私钥、SQL 全参数化、密钥未入库）；缺口集中在**授权粒度**（对象级越权）与**会话/密钥卫生**（管理端）。安全债多为"结构未封死",非"防线缺失"。

---

## SEC-00 · AGPL 法务裁决 【P0 · 产品决策非代码】
- **目标**：为 `flutter_vodozemac`/`vodozemac`（AGPL-3.0）定授权路径：开源本体 / 购商业授权 / 换非 AGPL 绑定,三选一。
- **原因**：评审 P0-4,与闭源商业售卖冲突,7 文件直接 import,发售即触发。
- **收益**：解除发布一票否决闸门。
- **风险**：高（商业/法务）;技术无风险。
- **影响范围**：`imboyapp/pubspec.yaml:223-224` + 商业条款。
- **工作量**：产品决策（非工程 pd）。**PR 数**：0–1（若换绑定另立技术任务）。
- **验收**：授权路径书面定案,发布合规。

## SEC-01 · 计费 API 对象级授权 【P0】
- **目标**：billing 9 端点补 `current_uid` 归属校验,`invoice_pay` 优先。
- **原因**：`billing_handler.erl:70-253` 全族忽略 current_uid,任意 JWT 可操作任意租户账单,invoice_pay 走真实扣款（评审 P1-A1,安全 H-01,唯一可被任意登录用户利用的高危面）。
- **收益**：恢复 SaaS 租户隔离与账单完整性。
- **风险**：低；照支付 owner_uid 红线范式即可。
- **影响范围**：`src/api/billing_handler.erl`。
- **工作量**：S。**PR 数**：1–2。
- **验收**：跨租户操作被拒;每端点有归属校验测试。

## SEC-02 · 钱包冻结资金守卫 【P0】
- **目标**：借记守卫补 `frozen`/`status` 校验（照 `recharge_order_repo:271`）,加表级 CHECK `frozen<=balance`。
- **原因**：`wallet_repo.erl:117-120,193-203` 冻结资金可被转账/红包花掉,不变量无 schema 兜底（评审 P1-D1）;同库已有正确范本。
- **收益**：资金不变量下沉 schema,冻结资金不可花。
- **风险**：低；范本现成。
- **影响范围**：`src/repo/wallet_repo.erl` + 迁移（加 CHECK）。
- **工作量**：S。**PR 数**：1–2。
- **验收**：冻结态借记被拒;CHECK 拦截 frozen>balance;回归覆盖转账/红包。

## SEC-03 · 首启向导可达性 + 鉴权声明式化 【P0/P1】
- **目标**：修 `/api/adm/setup/*` 401 不可达（P1-A2）;随 ARCH-01 把鉴权豁免收敛为路由声明。
- **原因**：`adm_auth_middleware:19-44` 不查 open/0 无 setup 分支,全新部署 401（路由收口回归）;根因是横切鉴权无单一真相源。
- **收益**：全新部署可初始化;消除整类鉴权静默失效。
- **风险**：中（触鉴权入口）。缓解:见 ARCH-01 豁免矩阵测试。
- **影响范围**：`adm_auth_middleware`、`auth_middleware*`、`imboy_router`。
- **工作量**：急修 S + 声明式化 L。**PR 数**：4–6（并入 ARCH-01）。
- **验收**：全新部署 setup 可达;鉴权属性路由单一声明。

## SEC-04 · 会话与密钥卫生 【P1】
- **目标**：① JWT 吊销通道（封禁即失效）② 口令改记忆硬 KDF（当前单轮 HMAC-SHA512）③ admin cookie 去硬编码默认密钥 + 加过期/可吊销 ④ `jwt_key` 与 `postgre_aes_key` 拆分独立值。
- **原因**：评审 P2-1/P2-2/P1-A3/P2-3——封禁用户存量 token 到期前全 API 可用;快哈希抗暴力破解弱;cookie 硬编码默认（P1 裁决）永不过期;密钥复用泄露放大。
- **收益**：会话可吊销,口令抗暴力破解,密钥隔离,cookie 不可伪造/可登出。
- **风险**：中；改认证核心。缓解:KDF 升级用双读兼容（旧哈希验证时透明升级）;cookie 加过期不影响存量（渐进）。
- **影响范围**：`token_ds`、`elib_password`、`adm_auth_middleware`、密钥配置。
- **工作量**：L。**PR 数**：4–5。
- **验收**：封禁用户 token 立即失效;新口令走硬 KDF 且旧口令透明升级;cookie 有 exp 且登出吊销;两密钥不同值（强制校验）。

## SEC-05 · 多租户隔离审计 + 归档链治理 【P3】
- **目标**：全 API 面对象级授权审计（BOLA 系统排查,不止 billing）;TimescaleDB 生命周期链（队列 1 年/timeline 30 天/msg_store 永久）与 `msg_archive_enabled` 配置文档对齐。
- **原因**：billing 越权暴露的对象级授权缺口可能不止一处;归档链依赖配置且文档已漂移（P3-1）。
- **收益**：租户隔离达标;数据生命周期可控可审计。
- **风险**：低（审计为主）。
- **影响范围**：全 API handler + TimescaleDB 配置。
- **工作量**：L。**PR 数**：3–5。
- **验收**：对象级授权审计报告零高危遗留;归档链配置与文档一致,有生命周期测试。

## SEC-06 · 客户端安全兜底 【P1/P2】
- **目标**：① Flutter 阅后即焚静默吞错补审计（焚毁失败可见）② raw SQL 逃生门结构封死（`elib_pg_sql` 加标识符断言/删死代码）。
- **原因**：`chat_burn_service.dart` 7 处 `catch(_){}` 焚毁失败无痕（P1-Q1,安全语义敏感）;`elib_pg_sql:335-338` raw 逃生门注入防线靠约定（P1-D3）。
- **收益**：焚毁失败可观测;注入结构性封死。
- **风险**：低。
- **影响范围**：`chat_burn_service.dart`、`src/lib/elib_pg_sql.erl`。
- **工作量**：S–M。**PR 数**：2。
- **验收**：焚毁失败有日志/上报;raw 路径有标识符白名单断言,无未校验拼接。

---

## 兼容性说明

SEC-01/02 是"补校验",拒绝的本就是越权请求,合法流量零影响。SEC-04 KDF/cookie 用双读兼容渐进升级,不强制存量用户重登。SEC-03 随 ARCH-01 走豁免矩阵回归保行为等价。

## 汇总表

| 编号 | 任务 | 优先级 | 工作量 | PR | 关键证据 |
|---|---|---|---|---|---|
| SEC-00 | AGPL 裁决 | P0 | 决策 | 0–1 | imboyapp/pubspec.yaml:221 |
| SEC-01 | 计费授权 | P0 | S | 1–2 | billing_handler.erl:70 |
| SEC-02 | 钱包冻结守卫 | P0 | S | 1–2 | wallet_repo.erl:117 |
| SEC-03 | 首启+鉴权声明 | P0/P1 | L | 4–6 | adm_auth_middleware:19 |
| SEC-04 | 会话密钥卫生 | P1 | L | 4–5 | token_ds.erl:55 |
| SEC-05 | 多租户审计 | P3 | L | 3–5 | 全 API 面 |
| SEC-06 | 客户端兜底 | P1/P2 | S–M | 2 | chat_burn_service.dart |
