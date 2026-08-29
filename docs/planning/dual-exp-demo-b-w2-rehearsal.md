# Demo B W2 演练报告（ZC-08：Channel-first-class W2 全链 + 迁移演练工具）

> 版本：1.0 | 日期：2026-08-29
> 执行角色：IMBoy W2 发布计划 ZC-08 Backend Test Agent
> 基线：main @ `db41789a` + W2 未提交产物（迁移 00000081 已应用、三卡四层模块与 ZC-05 整合视为前置，未改动）

---

## 1. 交付物

| 文件 | 说明 |
|---|---|
| `scripts/demo/dual_exp_demo_b_w2.sh` | W2 演练脚本（新增，13 步 / 固定 66 断言，自带 teardown） |
| `docs/planning/dual-exp-demo-b-w2-transcripts.md` | 两遍完整输出存档（md 入仓版；`*.log` 被根 .gitignore 忽略，与 W0 transcript 惯例一致） |
| `docs/planning/dual-exp-demo-b-w2-transcript-run1.log` | run1 原始输出（本地存档，ALL PASS 66/66，退出码 0；git 忽略） |
| `docs/planning/dual-exp-demo-b-w2-transcript-run2.log` | run2 原始输出（本地存档，ALL PASS 66/66，退出码 0；git 忽略） |
| `docs/planning/dual-exp-demo-b-w2-rehearsal.md` | 本报告 |

未修改任何 src/、test/、priv/migrations/、既有 dual_exp_demo_b.sh、执行台账、imboyapp/imboyadmin 文件。

## 2. 运行方式（决策记录）

- **后端实例**：`IMBOYENV=local` release（`_rel/imboy`），经 `bash scripts/start_node.sh imboy imboy 9800 "" daemon` 以 daemon 模式启动（`make run` 前台交互式 Eshell 在无 TTY 后台会因 stdin EOF 立即退出，daemon 模式等价且可无人值守）。HTTP 9800，与 sys.local.config 一致。
- **认证**：与 W0 不同，不使用 demo 登录，而是一律经 `scripts/imboy_ctl user create + user token` 创建唯一前缀账号并直接取得 JWT。原因：本地共享库 license 用户数上限使 `passport/signup` 稳定返回 402（W0 已登记该现象，imboy_ctl 正是 W0 的无人值守降级通道，此处升为主路径）。`imboy_ctl user create` 将 account 同值写入 `user.mobile`（varchar(40)），因此账号前缀固定为 `w2d<14位秒级时间戳><2位随机>`，全账号 ≤40 字符（见 §5 根因 R1）。
- **外向动作**：无。注册链路走 imboy_ctl 直写（不触发验证码通知）；local config `sms.switch=off`，email 类型注册不外发。
- **命令**：
  ```
  PGPASSWORD=<本地库口令> bash scripts/demo/dual_exp_demo_b_w2.sh
  ```
  PGPASSWORD 经环境变量注入（与 config/sys.local.config 一致，不入库不入脚本）。
- **两遍策略**：选择"每遍结束跑完整 teardown + 各用新时间戳前缀"（两遍前缀天然不同，teardown 自验证作为脚本自身步骤输出）。两遍均为完整独立演练。

## 3. 验收结果

| 验收项 | 结果 |
|---|---|
| 连续两遍 ALL PASS | 通过：run1 EXIT=0（66/66），run2 EXIT=0（66/66） |
| 断言数固定 | 66（脚本内 `TOTAL_ASSERTIONS=66`，summary 行输出"断言 66/66 通过"） |
| 失败退出码非零 | 通过：任一断言 FAIL → HAVE_FAIL=1 → `exit 1`（调试期多次复现） |
| transcript 与实际一致 | 两遍完整输出存档（§4 摘录与 log 一一对应） |
| 共享库无残留 | 通过：teardown 后按前缀独立复查 user/workspace/project/milestone/channel 五项全 0 |

残留终验（两遍结束、脚本 teardown 之后，从库外独立执行）：

```sql
SELECT (SELECT count(*) FROM "user" WHERE account LIKE 'w2d%'),        -- 0
       (SELECT count(*) FROM workspace WHERE name LIKE 'W2Demo-W2-%'), -- 0
       (SELECT count(*) FROM project WHERE name LIKE 'W2Demo-Project-%'), -- 0
       (SELECT count(*) FROM project_milestone WHERE name LIKE 'W2Demo-M1-%'), -- 0
       (SELECT count(*) FROM channel WHERE name LIKE 'W2Demo%');       -- 0
```

## 4. 场景 → 证据对照（transcript 关键行）

完整输出见两份 transcript log；以下摘自 run1（run2 同构，前缀不同）。

1. **注册（唯一前缀账号）** — `PASS: 用户 A 就绪(uid=178800918139721)` / `PASS: 用户 B 就绪(...722)` / `PASS: 前置校验：u1/u2 账号已落库`
2. **Workspace Template** — `PASS: Template 返回 status=created` + workspace/General 群/Announcements 频道 id 三项取得
3. **四关系之 workspace 成员邀请** — `PASS: workspace 成员邀请成功` + `PASS: DB 核查：B 是 active workspace_member`
4. **Project 创建（Owner 自动入项目）** — `PASS: Owner 自动入项目（members 含 owner uid=...)` / `PASS: 初始 members 仅 Owner 一人（total=1）` / `PASS: B 直访 members 被拒 code=403（非 Project Member）`（W2 项目隔离）
5. **Task** — `PASS: 任务创建并指派成功`（workspace member 可指派）+ task id 取得
6. **Milestone** — `PASS: 里程碑创建成功` / `PASS: 初始 status=planned` / `PASS: create/update 携带 status 字段被 400 拒绝` / `PASS: reach → status_flag=reached` / `PASS: 重复 reach 幂等 → status_flag=already_reached` / `PASS: list?status=reached 过滤含已达成里程碑`
7. **Channel 关联** — `PASS: link 频道 → status_flag=created` / `PASS: 重复 link 幂等 → status_flag=existing` / 关联列表含 Announcements / 发帖成功 / `PASS: related_posts 非空` + 无正文字段 / `PASS: unlink → status_flag=unlinked` / `PASS: unlink 缺失关联 → 404（定向删除语义）` / 重连 created
8. **四聚合空态与非空** — `PASS: pinned 可用且空态（total=0）` / `PASS: resources 空态` / `PASS: update_links 全量替换成功` / `PASS: resources 非空` / `PASS: activity 可用且非空（total=…）`（含 channel_linked、links_updated 事件）/ `PASS: activity 仅元数据（payload 无正文字段）` / B 入项目后 `PASS: B（active Project Member）可读 pinned 聚合`
9. **归档写拒读通** — `PASS: 归档成功`；五个写端点逐一 `code=980`（milestone create / reach / channel link / links update / member invite）；三个读端点可通（pinned / members / activity）
10. **恢复写放行** — `PASS: 恢复成功` + `PASS: 恢复后 links/update 放行`
11. **移除成员后再邀（重邀语义）** — `PASS: 移除 B → status_flag=removed` / `PASS: 移除后 B 直访 members → 403` / `PASS: DB 核查：B 的 project_member 行 status=removed` / `PASS: 重邀 → status_flag=created（覆盖激活，非静默 existing）` / `PASS: DB 核查：invited_by 重置为本次邀请人 A（历史不保留）` / `PASS: DB 核查：重邀不改变项目 Owner（仍为 A）` / `PASS: 重邀后 B 直访 members 恢复 200`
12. **teardown 自验证** — `PASS: teardown DELETE 单事务执行成功（ON_ERROR_STOP）` + 五项残留=0（workspace/project/user/孤儿频道/孤儿 project_member）

## 5. 调试期根因记录（对后续演练有复用价值）

- **R1 账号静默创建失败 → workspace 500**：`imboy_ctl user create` 把 account 值同时写入 `user.mobile`（varchar(40)）；账号串 >40 字符时 INSERT 报 22001，escript 输出丢弃后只见 `user token` 仍能签出 JWT（纯计算不查库），直到 Template 创建时 `fk_workspace_owner` 外键违规返回 500。修复：账号前缀固定 `w2d<14位><2位随机>`（≤40 字符）+ P1 增加"账号已落库 count=2"前置断言。
- **R2 teardown 静默失败**：初版每条 DELETE 独立 psql 会话且 `|| true` 吞错，间歇性失败导致"自验证绿但库有残留"。修复：单 `-c` 多语句（同一隐式事务）+ `ON_ERROR_STOP=1`，成败显式计入断言与退出码。
- **R3 bash 全角相邻变量**：`$VAR）`（中文括号紧随）会被 bash 解析为超长变量名导致 unbound variable。全脚本已统一 `${VAR}`。
- **R4 `make run` 后台不可用**：前台交互 Eshell 随 stdin EOF 退出（heart 记录 "Would reboot"）。演练环境用 `start_node.sh daemon`。

## 6. 语义假设与残余风险

- **重邀语义口径**：W2 的"移除后重邀不自动恢复历史状态"断言为——重邀返回 `status_flag=created`（覆盖激活而非静默 existing）、`invited_by` 重置为本次邀请人、项目 Owner 不变、成员身份本身恢复（200）而移除期间丧失的访问权（403→200）即成员身份，不存在成员级"下级历史"（W2 中 milestone/channel 关联均为项目级而非成员级）。与执行台账"重新邀请由应用层覆盖激活，不自动恢复历史下级"一致。
- **归档写拒覆盖面**：演练断言了 W2 新增的五个写端点 980（milestone create/reach、link、links/update、member invite）；member remove 同守卫未单独断言（归档窗口内移除场景无产品语义）。
- **observation（非阻塞）**：milestone create 携带 `due_date` 时响应体中该字段序列化为 Erlang date 元组形态字符串（如 `"{2026,9,30}"`），不影响本次断言（断言只看 name/status），建议后续卡核对 jsone 对 calendar date 的编码口径。
- **teardown 覆盖面**：按 workspace 前缀级联 + 账号前缀直删；未覆盖演练账号可能产生的 e2ee/olm 行（本演练未触达这些端点，无行产生）。若未来演练扩面，需同步扩 teardown 清单。
- **共享库白名单**：W2 演练前缀 `W2Demo-W2-`/`w2d…` 不在 `w0_schema_contract_tests.erl` 白名单内——这是有意为之：脚本必须自己清干净（已验证）。但若 teardown 被中途打断（Ctrl-C），可能留下触发 W0 contract test 的非白名单行；需手工按前缀清理（§2 命令可复用）。
- **imboy_ctl token 不校验用户存在**（R1 的诱因之一），以其做认证源的脚本都应加"账号已落库"前置校验（本脚本已内置）。

## 7. 建议 commit message + pathspec（不执行）

```
test(demo): add W2 Demo B rehearsal script with fixed 66 assertions and prefix teardown

- scripts/demo/dual_exp_demo_b_w2.sh: full channel-firstclass W2 drill
  (template -> ws member -> project owner auto-member -> task -> milestone
  reach idempotency -> channel link/unlink idempotency -> 4 aggregations
  empty/non-empty -> archived 980 write-reject reads-ok -> restore -> remove
  & re-invite semantics -> single-tx teardown with residual=0 self-check)
- docs/planning/dual-exp-demo-b-w2-rehearsal.md: rehearsal report
- docs/planning/dual-exp-demo-b-w2-transcript-run{1,2}.log: archived runs

Pathspec:
  scripts/demo/dual_exp_demo_b_w2.sh
  docs/planning/dual-exp-demo-b-w2-rehearsal.md
  docs/planning/dual-exp-demo-b-w2-transcripts.md
```

## 8. 复现指引

```bash
# 终端 1：启动本地后端（如未运行）
IMBOYENV=local bash scripts/start_node.sh imboy imboy 9800 "" daemon

# 终端 2：跑演练（连跑两遍即验收形态）
PGPASSWORD=<本地库口令> bash scripts/demo/dual_exp_demo_b_w2.sh; echo "exit=$?"
```
