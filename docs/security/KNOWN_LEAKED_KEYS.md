# 已知泄漏密钥台账 / Known Leaked Keys Ledger

> 维护者 / Owner: leeyi
> 创建: 2026-09-27（crossplan-v12-20260927T000125Z-b7fd6733 / CP-SEC-02）
> 密级: 内部。本文档只登记指纹与处置状态，不包含任何密钥明文或专属端点全文。

## 状态总览

`SECURITY_INCIDENT_OPEN` — 本地围堵已完成，第三方轮换未执行（见 PP-SEC-ROTATE）。

## 泄漏条目

### 条目 1：阿里云百炼（Bailian）API Key — pro 配置实例

| 字段 | 值 |
|---|---|
| 指纹 (SHA-256 of key) | `5a95c8191c50d15bddd9ce7bbae684a3c88e4ca5f9fb0f12d21b083edff91865` |
| 键前缀 | `sk-ws-`（MaaS 专属网关形态） |
| 泄漏位置 | 本地 ignored 文件 `config/sys.pro.config`（`llm_providers` → bailian → `api_key`） |
| 引入 commit | 无 git commit —— 该文件为 ignored 本地配置，key 从未被 git 跟踪（全部历史版本扫描零命中） |
| 引入途径 | Bailian provider 落地（功能 commit `a83648e2`，2026-08-08，该 commit 本身使用 `{env, ...}` 占位）后，本地手工将真实值写入 ignored pro 配置 |
| 删除 commit | N/A —— 2026-09-27 由 run `crossplan-v12-20260927T000125Z-b7fd6733` 卡 CP-SEC-01 就地替换为 `{env, <<"BAILIAN_API_KEY">>}`（ignored 文件，无 git 提交） |
| 当前轮换状态 | **未轮换** —— 等待用户在阿里云百炼控制台吊销并重签（PP-SEC-ROTATE，BLOCKED_USER_ACTION） |

### 条目 2：阿里云百炼（Bailian）API Key — dev 配置实例

| 字段 | 值 |
|---|---|
| 指纹 (SHA-256 of key) | `5a95c8191c50d15bddd9ce7bbae684a3c88e4ca5f9fb0f12d21b083edff91865`（与条目 1 为**同一把 key**，两个文件实例） |
| 泄漏位置 | 本地 ignored 文件 `config/sys.dev.config`（同上结构） |
| 引入 commit | 无（同条目 1，从未被 git 跟踪） |
| 删除 commit | N/A —— 同条目 1，CP-SEC-01 一并替换 |
| 当前轮换状态 | **未轮换** —— 同条目 1 |

## 暴露面评估（2026-09-27）

- Git 历史：干净。`git log --all` + 全历史版本内容扫描，`sk-ws-` 真实值（长度 ≥20）零命中；
  `config/sys.config.example` 中的 `sk-ws-` 仅为注释性前缀说明，非真实值。
- 本地磁盘：两个 ignored 文件中的明文已被 `{env, <<"BAILIAN_API_KEY">>}` 占位替换
  （替换后前缀扫描零命中，指纹不可再提取）。
- 生产服务器：本 run 无生产访问权限，生产配置中是否存在同把 key **未知**；
  需 PP-SEC-ROTATE 或独立生产子计划核验。在核验+轮换完成前保持 `SECURITY_INCIDENT_OPEN`。

## 后续动作

1. 用户在百炼控制台吊销指纹对应的 key 并重签（PP-SEC-ROTATE，需要用户操作）。
2. 新 key 仅进入 secret/env（`BAILIAN_API_KEY`），禁止回填配置文件。
3. 生产侧同把 key 的存在性核验与替换，纳入独立生产子计划。
4. 外部 LLM 冒烟（可能产生费用）需单独授权。
