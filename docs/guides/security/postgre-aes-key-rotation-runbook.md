# `postgre_aes_key` 轮换 Runbook（A-06）

> 状态：**已在测试库演练通过，生产尚未执行**（需单独授权）
> 关联：审计缺陷 #2 / #26 · 计划 `sellable-hardening-2026-q3.plan.md` A-05/A-06/A-07
> 最后更新：2026-07-31

---

## 1. 为什么必须轮换

`elib_hasher:encoded_val/1` 在 A-05 之前拼的是一段含**真实 `postgre_aes_key`** 的
pgcrypto SQL 表达式字符串：

```
encode(encrypt('<base64(明文)>', '<主密钥>', 'aes-cbc/pad:pkcs'), 'base64')
```

而 `elib_pg_sql:unzip_map/1` 只把 `{raw, Sql}` 元组拼进 SQL、普通 binary 一律走绑定参数，
所以这段字符串被当作**字面值**写进了 `user_collect.info`（调用点
`user_collect_logic:add_kind/8`）。

后果：

1. **主密钥泄露** —— 任意用户收藏一次内容，`SELECT info FROM user_collect` 即得全站
   配置加密主密钥。该密钥同时用于 SSO 凭据、`config` 表、消息载荷。
2. **加密静默失效** —— 存进去的一直是明文表达式，从未真正加密过。

因此**只改代码不够**：凡是这张表被任何非授权方读过一次，旧密钥即视为已泄露，必须轮换。

---

## 2. 变更清单（代码侧，已完成）

| 项 | 位置 | 处置 |
|---|---|---|
| A-05 写入 | `elib_hasher:encoded_val/1` | 改应用层 AES-256-GCM（`elib_cipher:aes_gcm_encrypt/2`），产出 `aesg1_<base64>`；密钥缺失/长度非 32 时 `error`（fail-closed） |
| A-05 读取 | `elib_hasher:decoded_val/1`、`decode_list_field/2` | 兼容三形态：`aesg1_` 密文 / 历史 SQL 字面值 / 明文 |
| A-05 调用点 | `user_collect_handler:page/2`、`user_tag_relation_logic`、`adm_user_handler` | `decoded_field/1` 换成取原始列 + 应用层解密 |
| A-07 config | `config_ds:pluck_decrypted_value/2` | 主密钥改走绑定参数 `$1`，不再进 SQL 文本 / 慢查询日志 / `pg_stat_statements` |
| A-06 清洗 | `priv/migrations/00000053_user_collect_info_recrypt.{up,down}.sql` | 抹掉列里的密钥（随发布自动执行，幂等） |
| A-06 重加密 | `scripts/recrypt_user_collect.escript` | 明文 → 新密钥密文，支持 `--dry-run` |

> `elib_hasher:decoded_field/1` 与 `decoded_payload/0` 已删除（前者是密钥内联进 SQL 的源头，
> 后者无任何生产调用方）。

---

## 3. 过渡期策略（旧密钥保留一个发布周期）

`user_collect.info` 的历史数据**从未真正加密**，所以清洗它不需要旧密钥 —— 明文就在
字面值里。但旧密钥仍被 `config` 表（pgcrypto 真加密）和历史消息载荷使用，因此：

| 阶段 | 旧密钥 | 新密钥 | 说明 |
|---|---|---|---|
| T0 发布当天 | 保留在 `IMBOY_POSTGRE_AES_KEY_OLD`（只读） | `IMBOY_POSTGRE_AES_KEY` 生效 | 新写入全部走新密钥 |
| T0 → T+1 个发布周期 | 只读，不用于任何新写入 | 正常 | 观察是否有遗漏的旧密文读取路径报错 |
| T+1 个发布周期后 | **从所有 config / env / 密钥管理器中彻底移除** | 正常 | 完成轮换 |

> ⚠️ 当前代码**没有**读取 `IMBOY_POSTGRE_AES_KEY_OLD` 的分支 —— 因为 `user_collect`
> 不需要它。若 `config` 表也要轮换，需另立任务并补双密钥读取逻辑；本 runbook 不含该步。

---

## 4. 生产执行步骤

> 全程在维护窗口执行。**每一步的输出都要留档。**

### 4.0 前置

```bash
# 生成新密钥（32 字节）
openssl rand -base64 24 | head -c 32; echo
```

- [ ] 新密钥已写入密钥管理器 / 部署环境变量 `IMBOY_POSTGRE_AES_KEY`，**未**进 git
- [ ] 已确认目标库不是测试库（`SELECT current_database(), inet_server_addr();`）

### 4.1 备份（必做，回滚唯一依据）

```bash
bash scripts/backup_pg.sh          # 全库备份
# 另外单独导一份该表，回滚更快：
pg_dump -h <HOST> -p <PORT> -U <USER> -d <DB> -t public.user_collect -Fc \
  -f user_collect_pre_a06_$(date +%Y%m%d%H%M).dump
```

### 4.2 dry-run（只读，不写库）

```bash
PGPASSWORD=<口令> escript scripts/recrypt_user_collect.escript --dry-run \
  --host <HOST> --port <PORT> --user <USER> --db <DB> \
  --key '<新密钥>'
```

输出示例：

```
模式: dry_run
user_collect 统计: 总计=N  含主密钥脏数据=M  已密文=0  明文=0
待处理行数: M
抽样（最多 5 行，只显示明文前 120 字节）:
  id=...  明文={"title":"..."}
[dry-run] 未写入任何数据。
```

- [ ] 记录 `总计=N`、`含主密钥脏数据=M`
- [ ] 抽样明文肉眼可读（说明提取逻辑正确）
- [ ] **退出码为 0**（预检通过）

#### dry-run 是硬门禁，退出码 5 表示有阻塞行

预检会点名两类数据，发现任一类即以 **5** 退出：

| 类型 | 后果 |
|---|---|
| 非法 UTF-8 | 迁移 `00000053` 的 `convert_from(..., 'UTF8')` 会**抛错**。迁移是启动期 fail-fast，**一行坏数据就挡住整个部署**，只能手工改库恢复。 |
| 无法提取明文 | 迁移的 base64 字符集守卫同样跳过，**主密钥继续留在这些行里且无任何报错**。 |

**退出码 5 时不要直接进 4.3。** 处理办法二选一：

1. **先跑 `--apply`（推荐）** —— 这些行会被转成 `aesg1_` 密文，迁移随后不再命中。
   注意此时用的是**新**密钥，等价于把 4.4 提到 4.3 之前，顺序无副作用（两者幂等）。
2. 人工核实这些 id 后清理。

> 非法 UTF-8 的行被 `--apply` 加密后仍可存取，只是读取时 `json_decode_field` 解 JSON 会
> 失败并降级为原样返回（有 warning 日志），不影响其它行。

### 4.3 部署新代码（含迁移 00000053）

迁移随启动 fail-fast 自动执行，把字面值就地还原成明文 JSON，**一条 UPDATE 抹掉密钥**。

- [ ] 部署完成后立即验证：

```sql
SELECT count(*) FROM public.user_collect;                                  -- 必须 = N
SELECT count(*) FROM public.user_collect WHERE info LIKE 'encode(encrypt(%'; -- 必须 = 0
SELECT info FROM public.user_collect LIMIT 20;                             -- 不得出现 encrypt(
```

### 4.4 重加密

```bash
PGPASSWORD=<口令> escript scripts/recrypt_user_collect.escript --apply \
  --host <HOST> --port <PORT> --user <USER> --db <DB> \
  --key '<新密钥>' --batch 500
```

行数超过 `--batch` 时重复执行直到 `待处理行数: 0`（脚本幂等）。

- [ ] `SELECT count(*) FROM public.user_collect;` 仍 = N
- [ ] `SELECT count(*) FROM public.user_collect WHERE info NOT LIKE 'aesg1\_%' AND info <> '';` = 0

### 4.5 功能验收

- [ ] `GET /api/v1/user_collect/page` 返回历史收藏，`info` 字段内容正确（至少比对 5 条）
- [ ] 管理后台「用户收藏」列表正常
- [ ] 新增一条收藏后，`SELECT info` 是 `aesg1_` 开头且不含明文
- [ ] 应用日志无 `decoded_val failed`

### 4.6 收尾

- [ ] 旧密钥标记为「只读过渡」，记录移除截止日期（T+1 个发布周期）
- [ ] 审计：旧密钥可能已被读取，评估是否需要同步轮换 SSO 凭据等下游

---

## 5. 回滚路径

| 故障点 | 回滚动作 |
|---|---|
| 4.3 迁移后发现问题 | 回滚代码版本；数据层执行 `00000053_..._recrypt.down.sql`（**只还原形态，密钥位置写 `REVOKED_KEY` 占位，绝不重新植入真实密钥**）。实践中更推荐直接前滚到 4.4。 |
| 4.4 重加密中途失败 | 脚本按行提交，无中间态；直接重跑 `--apply`（幂等）。 |
| 4.5 收藏读不出来 | 优先查 `IMBOY_POSTGRE_AES_KEY` 是否与 4.4 使用的一致（不一致时 `decoded_val` 返回 `<<>>`，日志有 `decoded_val failed`）。改对后无需重跑数据。 |
| 全面失败 | 停服 → `pg_restore` 4.1 的 `user_collect` 单表 dump → 回滚代码版本 → 旧密钥继续生效。**注意：此路径等于把密钥泄露装回去，仅作最后手段且必须限时。** |

---

## 6. 测试库演练记录（2026-07-31）

环境：`127.0.0.1:4323 / imboy_v1`（docker `imboy_pg18`）

| 检查项 | 结果 |
|---|---|
| 构造 6 行 A-05 之前形态的脏数据 | 复现成功，`SELECT info` 直接读出主密钥 |
| `--dry-run` 前后表指纹一致 | ✅ 未写库 |
| 迁移 00000053 up | `UPDATE 6`，行数 6→6，密钥残留行数 = 0 |
| 迁移幂等（二次执行） | `UPDATE 0` |
| `--apply`（新密钥） | 重加密 6 行，全部 `aesg1_` 前缀 |
| `--apply` 幂等（二次执行） | 待处理 0 行 |
| `SELECT info LIMIT 20` 含 `encrypt(` 行数 | 0 |
| `decoded_val` 解回明文 6/6 与原文逐字节一致 | ✅ |
| 老格式字面值兼容读取 | ✅ |
| 错误密钥 → 返回 `<<>>`（不回落密文） | ✅ |
| 密钥缺失 → `error:invalid_postgre_aes_key` | ✅ |
| 同明文两次加密密文不同（随机 IV） | ✅ |
| `config_ds` 参数化解密在真库可用 | ✅ 明文往返一致 |
