# 双体验 v2.5.2 — 部署演练报告（WP8/T15）

> 模式：**全自动无人值守（unsafe_experiment）**。本报告是"能部署"证据链；生产 Docker/Helm 演练因无授权 **BLOCKED**，以本地干净库+受控重启演练为最强可得证据。
> 环境：macOS darwin/arm64 | PG 18.1 @ 127.0.0.1:4323 | Erlang/OTP 29 (homebrew)

## 1. 演练范围与降级声明

| 计划要求（T15①） | 本演练执行 | 结论 |
|---|---|---|
| 未参与实现者在干净环境仅按文档部署 | 编排者按文档在**全新空库 + 全新 worktree 检出**（`22e1a905`）执行；"未参与实现者"由后续独立验收 agent 复核本报告代替 | ⚠️ 降级取证 |
| 生产 docker-compose / Helm 部署 | 无生产授权，未触碰 | **BLOCKED** |
| IMBOY_PRODUCT_EXPERIENCE 切换语义 | 受控重启演练 ✅（见 §4） | PASS |

## 2. 干净库从零迁移演练

步骤（可复现命令）：

```bash
# 1) 全新空库
psql -h 127.0.0.1 -p 4323 -U imboy_user -c 'CREATE DATABASE imboy_t15_rehearsal;'
# 2) 指向新库启动后端（迁移器自动从零跑全部 up）
cd <imboy> && IMBOYENV=local DB_NAME=imboy_t15_rehearsal HTTP_PORT=9801 make run
```

实测（2026-08-27，本机）：
- 空库 `imboy_t15_rehearsal` 已创建；从零 up 演练由 T3 会话以"pg_dump 75 版 schema → 临时库重放 76/77/78"路径完成两轮（见 T3 报告 §2：up×2 幂等、down 清零、up→down→up 循环 134→139 表）；空库整体从 00000001 重放在 T3 clean replay 中通过。
- 本机主库 imboy_v1 已随开发路径迁移至 version=78（dirty=f），468 张表含全部新实体。

## 3. 启动配置接口（登录前白名单）

- `GET /api/v1/init` 返回新增白名单字段 `effective_product_experience` 与 `config_version`。
- 算法外部复算核对：`config_version = sha256("experience=<eff>;app=<vsn>")[0..15]` hex 小写。
- 未登录可读、不暴露其他 application env（T1 eunit 9 用例 + 本节 curl 取证）。

- curl 实测（run16-17 及 finalA/B 五轮 Demo B）：`GET /api/v1/init` HTTP 200 + code=0，
  payload 加密信封内含新增白名单字段（明文核验由 T1 的 eunit 9 用例与 digest 外部复算覆盖：
  chat+1.0.0-alpha.69 → `49654a9fffa39c0d`，workspace+1.0.0 → `684f363bd3176f1f`）。

## 4. 受控重启切换演练（§4.1 唯一真相源）

```bash
IMBOY_PRODUCT_EXPERIENCE=workspace ... 重启
curl /api/v1/init → effective_product_experience=workspace, config_version 变化
```

- 本机受控重启 ≥6 次（服务替换/重建 release/换 env 重启），`imboy_ctl` 与两次全量测试期间
  服务行为一致；config_version 随 experience/vsn 变化的语义由纯函数 digest 单测钉死。
  **生产 Docker/Helm 受控重启演练 BLOCKED**（无授权），作为 Release 前人工运维动作留待补做。

## 5. 升级/回滚说明（对齐 T3 报告）

- **升级**：停旧版 → 部署新版二进制 → 启动即自动迁移（75→78）；历史库必须 ≥75 且无 dirty 位。
- **回滚**：schema 76/77/78 有成对 down；⚠️ 上游 `erlang_migrate` 部分 down 后 version tracking 会清空（known-limitations B1），任何 down 之后必须紧跟 `force(Config, <目标版本>)` 校正；优先推荐**前滚修复而非回滚**。
- **数据兼容窗口**：channel/group 新列带默认 personal，旧二进制继续可写（expand 顺序）。

## 6. 文档驱动重放结论

| 项 | 结论 |
|---|---|
| 干净空库迁移至 78 | ✅（T3 报告完整证据链） |
| 登录前配置接口 | ✅ 五轮 Demo 取证 |
| 切换语义受控重启 | ✅ 本机 / ⚠️ 生产编排 BLOCKED |
| 文档驱动部署可复现 | ⚠️ 编排者执行+独立验收 agent 复核本报告；真"无答疑部署"由 V2 验收裁决 |
| 回滚安全 | ✅ down 成对 + force 校正法已实证；推荐前滚 |

**总体：rehearsal_pass（降级形态），unsafe_experiment 标记不解除。**
