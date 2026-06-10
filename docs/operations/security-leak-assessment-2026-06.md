# git 历史密钥泄露评估 / Git History Secret-Leak Assessment

> **日期 / Date**: 2026-06-10 | **范围 / Scope**: imboy 仓库全历史（721 commits, 39.3MB）
> **工具 / Tool**: `gitleaks detect --log-opts="--all"` | **结果 / Result**: 9 findings, **全部仅存在于历史 commit，HEAD 零泄露**
> 本文档是商业化计划 Task A1 的验证产物，供买家尽调使用。
> This document is the validation artifact of commercialization Task A1, intended for buyer due diligence.

---

## 结论 / Conclusion (TL;DR)

1. **HEAD 干净 / HEAD is clean**：9 条泄露所在文件均已不在当前代码树（`git cat-file -e HEAD:<path>` 逐一验证失败）。
2. **泄露的唯一真私钥已停用 / The only real private key leaked is retired**：历史 DML 中的 `login_rsa_priv_key`（前缀 `MIICXQ…`，1024-bit）源自 py_admin 模板项目的**公开样板密钥**（同行 `site_name='py_admin'` 佐证）。现行系统：① 已改为文件加载（`imboy_app.erl: login_rsa_priv_key_file`）；② 现用密钥前缀 `MIIEpA…`（2048-bit+），与泄露 key 不同；③ 现用密钥文件 `priv/dev_keys/login_rsa_priv.pem` **未被 git 跟踪**；④ 当前数据库 `config` 表中已无该键。
3. **现行敏感配置从未入库 / Live secrets never entered git**：`config/sys.local.config` 被 `.gitignore:11 *local*.config` 排除，`git ls-files` 与全历史 `git log` 均无记录（2026-06-10 复核）。

## 逐条定性 / Finding-by-Finding Triage

| # | 规则 | 历史文件（HEAD 均不存在） | commit | 定性 / Triage |
|---|------|--------------------------|--------|---------------|
| 1 | private-key | `.claude/plan/e2ee_plus_plan/phase-04-local-backup.md` | 766c54f3 | 计划文档中的**示例**密钥块 / Example key in planning doc |
| 2-5 | jwt ×4 | `doc/message_ack.md` | 35342fbd | 文档中的演示 JWT；token 非密钥，且签发密钥后续已更换 / Demo JWTs in docs; tokens, not keys |
| 6 | private-key | `doc/postgressql/vsn0.1/public.config.csv` | df9b479b | py_admin 模板私钥（公开样板）/ py_admin template key (public boilerplate) |
| 7 | private-key | `doc/postgressql/vsn0.1/dml.sql` | 52940757 | 同上（同一把 `MIICXQ…`）/ Same template key |
| 8 | private-key | `priv/sql/v1_dml.sql` | 8598a0fc | 同上 / Same template key |
| 9 | generic-api-key | `priv/doc/CentOS优化.md` | d54bc5b9 | 运维笔记中的匹配串，非现行凭证 / Pattern match in ops notes, not a live credential |

## 现行密钥健康度 / Current Key Hygiene

| 项 | 状态 |
|----|------|
| `config/sys.local.config`（全部运行时密钥） | gitignore 排除，从未入库 ✅ |
| `priv/dev_keys/*.pem`（登录 RSA 对） | 未跟踪，与泄露 key 不同把 ✅ |
| 生产凭证注入方式 | `IMBOY_*` 环境变量（`sys.pro.config` 仅结构性覆盖）✅ |
| Garage S3 key | 2026-06 已轮换（GK26bf…，旧 key 随容器重建废弃）✅ |

## 处置选项 / Remediation Options（待决策 / Decision Pending）

| 选项 | 做法 | 代价 | 收益 |
|------|------|------|------|
| **A. 历史清洗** | `git filter-repo` 删除 6 个历史文件路径，4 个 remote（gitee/github/gitcode/leeyi）同步强推 | 全部 commit hash 改写；协作 clone 须重拉；先打 tag 备份旧 HEAD | 尽调时 `gitleaks --all` 零结果 |
| **B. 披露式基线** | 本文档 + `.gitleaksignore`（按 commit+path 精确豁免 9 条），CI ratchet 基线锁定、新增即红 | 历史仍可被翻出（但均为模板/示例，已逐条定性） | 不改写历史；诚实披露本身是尽调加分项 |

> 建议 / Recommendation：**B 优先**。9 条均非现行凭证，定性清楚；历史改写的协调成本与风险（4 remote、外部 clone）高于收益。若买家尽调政策强制"全历史零泄露"，再执行 A（操作步骤见商业化计划 Task A1）。

## 复核命令 / Reproduce

```bash
cd imboy
gitleaks detect --source . --log-opts="--all" --no-banner   # 9 findings, all historical
git cat-file -e HEAD:priv/sql/v1_dml.sql || echo "not in HEAD"
git ls-files priv/dev_keys/                                  # empty = untracked
docker exec imboy_pg18 psql -U imboy_user -d imboy_v1 -tA \
  -c "SELECT \"key\" FROM public.config WHERE \"key\"='login_rsa_priv_key'"  # empty
```
