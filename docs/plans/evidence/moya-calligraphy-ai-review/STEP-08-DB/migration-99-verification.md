# 00000099_teaching_admin_audit 验证证据（R6 续轮）

Owner: Agent C (DATABASE 泳道) | 日期: 2026-09-09 | 环境: docker PG `imboy_pg18`（127.0.0.1:4323，scratch 库 `moya_mig_test`，00000001→00000098 全量态叠加 99）

> R5 中断点回顾：SQL up/down 与 moya_teaching_migration_tests 的 99 断言在中断前已写完，行为测试当时 PASSED；
> R6 在重建取证时发现并修复了两处问题（见"R6 修复"），全部验证重新跑通。

## 1. 最终结构摘要

```sql
teaching_admin_audit (
    id             bigint   NOT NULL,              -- TSID；pk_teaching_admin_audit
    action         text     NOT NULL,              -- ck_..._action 枚举 5 值
    operator_uid   bigint   NOT NULL,              -- 裸列无 FK；0=sentinel；ck_..._uids (>=0)
    learner_id     bigint   NOT NULL,              -- 裸列无 FK
    target_user_id bigint   NULL,                  -- NULL=无目标用户；0=sentinel（目标已注销）
    detail         jsonb    DEFAULT '{}' NOT NULL,
    created_at     timestamptz DEFAULT now() NOT NULL
)
-- action CHECK 集合：bind_learner / unbind_learner / learner_archive / data_export / data_delete
-- 索引：i_teaching_admin_audit_learner  (learner_id, created_at DESC)
--       i_teaching_admin_audit_operator (operator_uid, created_at DESC)
-- 触发器：无（审计只插入，无状态机）；down = 索引→表逆序 DROP
```

## 2. sentinel uid 0 决策（任务书留给 C 的决策点）

**选择：取消 FK 改裸列**（STEP-16 handoff #2 给出的二选一），不预插 uid=0 占位行。理由：

1. 审计是不可变合规日志，必须比被引用实体活得更久——任何 FK（CASCADE 删行 / SET NULL 抹操作人 / RESTRICT 反向阻塞用户删除）都与审计意图相悖（行为测试 T5 实证：用户物理删除不触碰审计行）。
2. FK + sentinel 需在 "user" 表预插 uid=0 占位行：会触发 `sync_fts_user()` 等账号触发器、污染账号空间与序列语义，跨环境保证脆弱。
3. 裸列使本表天然不进 user_deletion_executor 的级联清单。
4. sentinel 语义：operator_uid / target_user_id 用 0 表示"账号已删除/匿名化"，严禁 NULL 抹操作主体（NULL 仅保留给 target_user_id=该动作本无目标用户的正交语义）。应用层匿名化路径 = 注销前 `UPDATE ... SET operator_uid=0 WHERE operator_uid=$uid`（T5b 实证生效）。

已知后续（不在本波）：00000097/98 的 homework_submission.withdrawn_by / teacher_review.reviewer_uid 仍是 FK SET NULL + CHECK fail-closed，若要统一 sentinel 策略需另立迁移——已在 up.sql 头部注释登记。

## 3. up / down / up 幂等与行为断言（R6 重跑）

连接形态：`export PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user`（沿用 STEP-05 既有配方）。

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 --single-transaction -q -f $MIG/00000099_teaching_admin_audit.up.sql
UP2_IDEMPOTENT_OK   # exit 0（NOTICE: already exists skipping ×3，幂等符合预期）

$ psql -d moya_mig_test -v ON_ERROR_STOP=1 -f /tmp/moya_mig/step99_behavior_test.sql
STEP99_AUDIT_TESTS_PASSED   # exit 0（BEGIN..ROLLBACK 不留数据）

$ psql ... -f $MIG/00000099_teaching_admin_audit.down.sql   → DOWN_OK exit 0
$ psql -d moya_mig_test -tAc "SELECT count(*) FROM pg_tables WHERE tablename='teaching_admin_audit';"  → 0
$ psql ... -f $MIG/00000099_teaching_admin_audit.up.sql     → UP_AFTER_DOWN_OK exit 0
$ psql ... -f /tmp/moya_mig/step99_behavior_test.sql        → STEP99_AUDIT_TESTS_PASSED（回补后再证）
```

行为断言矩阵（/tmp/moya_mig/step99_behavior_test.sql，R6 在中断前版本上补 T2b）：

| # | 用例 | 期望 | 结果 |
|---|---|---|---|
| T1 | bind/unbind/archive 三动作合法插入 + detail jsonb 结构化 | 3 行落库 | PASS |
| T2 | 非法 action（'delete_learner'，未枚举） | check_violation（ck_..._action） | PASS |
| T2b | 重复 id（PK 冲突，任务书"重复行"断言；R6 新增） | unique_violation（pk_teaching_admin_audit） | PASS |
| T3 | operator_uid=0 sentinel 合法；-1 拒绝 | 0 可插入；负数 check_violation（ck_..._uids） | PASS |
| T4 | operator/target 指向不存在账号仍可插入（裸列无 FK 语义，任务书"非法 FK"断言的**有意反向**：99 设计即无 FK） | 插入成功 | PASS |
| T5 | 用户物理删除不删/不改审计行；匿名化 UPDATE 改写为 sentinel 0 | 审计行存活；改写生效 | PASS |
| T6 | learner/operator 双复合索引存在且列序正确 | pg_indexes 命中 | PASS |
| T7 | detail 默认 '{}' | 命中 | PASS |

说明：任务书要求"重复行/非法 FK/CHECK 违反各至少一条"——99 的裸列设计使"非法 FK 拒绝"不适用（有意决策，见 §2）；
以 T4（FK 缺席的正例）+ T5（用户删除零影响）组合证明裸列语义，T2/T3/T2b 覆盖 CHECK 与 PK。

## 4. 全链完整性（95→96→97→98→99）

- 库态确认：96 五表（class_profile/class_staff/learner/class_enrollment/guardian_learner）+
  97 四表（homework_submission/submission_asset/calligraphy_review_draft/teacher_review）+
  98 四列（idempotency_key/request_digest/withdrawn_at/withdrawn_by）+ 99 表均在，158 表全量态。
- 99 down/up 轮换不影响 95-98 对象（down 仅触碰自身索引与表）。
- 环境插曲（诚实记录）：R6 初查时误用无 -h 的 psql（走本机 socket 连到另一个同名空库），差点误判"库被清空"；
  改用 4323 显式参数后确认全量态在位，未做任何重建。此坑已在上表命令形态中固定规避（显式 PGHOST/PGPORT）。

## 5. moya_teaching_migration_tests（EUnit）

```
$ erlc -I include -o test/ test/repo/moya_teaching_migration_tests.erl   # ERLC_OK
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval 'R = eunit:test([moya_teaching_migration_tests], [no_tty]), ...'
RESULT: ok   # EXIT=0；verbose: 18 passed / 0 failed / 0 skipped
```

### R6 修复：中文断言 binary 字面量截断 bug（自找的 WIP 缺陷，非历史问题）

中断前写的 99 断言 `<<"禁置NULL">>` / `<<"物理删除审计历史">>` 在 step99_sentinel_no_fk_test 失败。
根因实验（/tmp 探针，已清理）：epp 按 UTF-8 读源码（list 字面量码点正确 [31105,32622,...]），
但 **binary 字面量按 latin1 语义把 >255 码点截断为低 8 位**（31105 band 255 = 129），
编译产物 `<<129,110,"NULL">>` 永远匹配不到 SQL 文件的 UTF-8 字节序列。
修复：两处断言加 `/utf8` 修饰符（`<<"禁置NULL"/utf8>>`），编译产物字节与文件一致，断言强度不变。
这是本套件**首个含中文的断言**（step5-98 全是 ASCII），故此前未暴露。

## 6. 写入接线状态（非 C 缺口）

bind/unbind 的 repo 层写审计行属 B 泳道文件（不在 C 可写清单）。**schema 就绪、写入接线待 B**：
B 在 teaching_learner_bind_repo（或等价位置）的 bind/unbind 事务内各插一行
`teaching_admin_audit(action='bind_learner'|'unbind_learner', operator_uid, learner_id, target_user_id, detail)` 即可，
detail 建议含 role/why/request_id，禁止 display_name/出生年/视频 URL 等儿童 PII（§9.3）。

## 7. 结论

00000099 结构/幂等/行为断言/EUnit 四层验证全部通过，**PASS**。
