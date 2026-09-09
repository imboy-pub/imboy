# Step 17 后端 boot 冒烟（R7，Coordinator 亲跑）

日期：2026-09-09 22:45–22:52 | 执行者：Coordinator | 结论：**PASS（含 1 个跨项目真 bug 发现：ecron `jobs` 键失效）**

## 目的

Step 17 唯一本地可闭合子项：真实 boot 路径（空库 + 全链自动迁移 + 完整应用启动 + 路由/ecron 活性）验证，
不触碰用户运行中的 9800/9801 节点与其 imboy_v1 开发库。

## 环境配方（可复现）

```bash
# 1. 专用库（扩展清单从 imboy_v1 只读复刻，13 个：含 postgis 家族/timescaledb/pg_jieba/roaringbitmap/vector）
PGPASSWORD=*** psql -h 127.0.0.1 -p 4323 -U imboy_user -d postgres -c "CREATE DATABASE moya_boot_smoke"
# pgrouting 需在 postgis 之后装（字母序坑）
# 2. 冒烟配置 = sys.local.config 副本（/tmp/smoke_sys.config）：
#    sed imboy_v1→moya_boot_smoke（2 处）+ 注入 {ecron, [{time_zone,local},{global_quorum_size,1},{local_jobs,[教学×2]}]}
#    file:consult OK
# 3. 直连 boot（不 make run——那会重建 _rel/imboy 即用户运行节点的家目录）：
HTTP_PORT=9811 erl -noshell -pa ebin -pa deps/*/ebin \
  -name imboy_smoke@127.0.0.1 -setcookie imboy_smoke_ck \
  -config /tmp/smoke_sys.config \
  -eval 'R = application:ensure_all_started(imboy), io:format("SMOKE_BOOT_RESULT: ~p~n", [R])'
# 4. 远程探针必须用文件模块（CLI -eval 里未加引号的 '@' 节点原子必炸语法错误）
```

## 断言与结果

| # | 断言 | 结果 |
|---|---|---|
| B1 | 全应用启动（含全部 dep + imboy） | `SMOKE_BOOT_RESULT: {ok,[...38 app...,imboy]}`，无 crash report |
| B2 | 空库全链自动迁移 1→99（真实首次部署路径，auto_migrate 默认 true） | schema_migrations=99；教学 6 表（learner/class_profile/guardian_learner/homework_submission/teacher_review/teaching_admin_audit）全在 |
| B3 | Step 16 新路由挂载 + 鉴权边界 | POST /api/v1/teaching/learners/1/bind 与 /unbind → **401** + envelope `{"code":401,"msg":"未登录，请先登录","payload":{},"sv_ts":...}` |
| B4 | 路由 404 语义 | GET /api/v1/definitely-not-a-route → 404 |
| B5 | 监听器 fail-fast | （既有行为确认：imboy_app.erl 确保绑定失败阻断启动） |
| B6 | ecron 作业激活 + 实际触发 | 见下节（修复后）：双作业 status=activate；teaching_ai_worker ok 计数 1→2（每分钟真实执行，零 crashed/aborted/skipped） |
| B7 | imboy_v1 零接触 | pg_stat_activity：moya_boot_smoke=5（我的池）、imboy_v1=14（用户自有节点连接，配置层即隔离——副本内 2 处库名全替换） |
| B8 | 节点隔离 | 独立节点名 imboy_smoke@127.0.0.1 / 独立 cookie / 端口 9811；用户 9800/9801 与 epmd 既有注册零干扰；退出后端口与 epmd 注册干净释放 |

## 真 bug：ecron v1.1.0 不消费 {jobs,...} 配置键（跨项目，pre-existing）

- **现象**：注入 sys.config.example 同款 `{ecron,[...,{jobs,[...]}]}` 配置 boot 后 `ecron:statistic()` 返回 `[]`——作业零激活。
- **根因**：依赖 pin ecron v1.1.0（include/deps.mk:87），`ecron_sup` 只读 `global_jobs`/`local_jobs` 两个 env 键
  （deps/ecron/src/ecron_sup.erl:18/44/45/60）；全 dep 源码无任何 `{jobs,` 消费者。
- **影响面**：sys.config.example:523 起整个 ecron 块从未生效——含**既有**的 attachment_orphan_cleanup /
  attachment_pending_cleanup / **B-06 支付对账**，与教学域两条新作业。用户运行中的 9800/9801 节点
  （sys.runtime.config ← sys.config.example）同样中招：**B-06 支付对账从未被真正调度**。pre-existing，非本任务引入。
- **修复证明（本冒烟内）**：/tmp 配置 `{jobs,[` → `{local_jobs,[` 重启后 statistic() 双作业 activate，
  teaching_ai_worker 每分钟实跑（ok 1→2）。仓内修复（sys.config.example 键名改 local_jobs 或 global_jobs——
  行为选择：每节点各跑 vs 全局单实例）移交 Agent B' 接管落地，用户节点重启前不会自愈。
- **教训**：配置块「格式看着对 + file:consult OK」≠ 生效；ecron 必须用 statistic() 验活。

## 足迹

- 运行中：imboy_smoke@127.0.0.1（9811）保留至本轮收口后停止；DB moya_boot_smoke 保留作证据/复用。
- /tmp/smoke_sys.config、/tmp/ec_probe*.erl、/tmp/halt_smoke.erl 保留（复现用）；两枚误入仓根的 erl_crash.dump 已即时清理。
- 用户资产零接触：9800/9801、imboy_v1、_rel/、sys.local.config 均未动。
