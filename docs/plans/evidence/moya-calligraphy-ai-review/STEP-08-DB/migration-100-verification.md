# 00000100_teaching_sentinel_unify 验证证据（R8）

Owner: Agent C (DATABASE 泳道) | 日期: 2026-09-09 | 环境: docker PG `imboy_pg18`（显式 PGHOST=127.0.0.1 PGPORT=4323）/ scratch 库 `moya_mig_test`（R6 时已升 99 态，本轮升 100）/ 临时全链库 `moya_r8_fullchain`（验证后已 DROP）

## 1. 结构 diff（对 97/98 既有列）

来源核对（动手前先读原定义）：
- `00000098.up.sql:23,35-37`：`withdrawn_by bigint` + `fk_hs_withdrawn_by FOREIGN KEY (withdrawn_by) REFERENCES "user"(id) ON DELETE SET NULL`
- `00000097.up.sql:256,287-289`：`reviewer_uid bigint` + `fk_tr_reviewer FOREIGN KEY (reviewer_uid) REFERENCES "user"(id) ON DELETE SET NULL`
- 上库实态确认（pg_constraint）：两 FK 均在 moya_mig_test 生效中。

00000100 up 做的事（存量数据零改写）：

```sql
-- ① 摘 FK（幂等）
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_withdrawn_by;
ALTER TABLE teacher_review      DROP CONSTRAINT IF EXISTS fk_tr_reviewer;
-- ② sentinel CHECK（99 同款风格）
ALTER TABLE homework_submission ADD CONSTRAINT ck_homework_submission_withdrawn_by_sentinel
    CHECK (withdrawn_by IS NULL OR withdrawn_by >= 0);
ALTER TABLE teacher_review ADD CONSTRAINT ck_teacher_review_reviewer_uid_sentinel
    CHECK (reviewer_uid IS NULL OR reviewer_uid >= 0);
-- ③ 列注释对齐 sentinel 语义（0=已注销/匿名化；禁置 NULL）
```

新状态（行为测试 T6 + 上库抽检双确认）：两 FK 不存在、两 sentinel CHECK 存在、`withdrawn_audit`/`published` 既有 CHECK 原样保留（与 sentinel 0 兼容：0 满足 IS NOT NULL）。

## 2. FK→裸列理由

1. **统一 99 基准**（migration-99-verification.md §2）：撤回/回评是审计动作，操作人账号注销后审计行必须保留原 uid，由应用层匿名化显式 `UPDATE→0`，而非 FK 静默 SET NULL 抹痕。
2. **解除 fail-closed 死锁**：00000098 时代 withdrawn_by 的 SET NULL 撞 `ck_homework_submission_withdraw_audit`（withdrawn 态强制非空）使用户删除事务失败（当时的故意设计）；`ck_teacher_review_published`（published 态 reviewer_uid 非空）同理。裸列后**用户删除不再被审计行阻塞，痕迹也不丢**——两面同时成立（行为测试 T2 实证：DELETE user 成功且两列保留原 uid）。
3. 替代方案「FK + 预插 user(id)=0 占位行」否决：触发 sync_fts_user() 等账号触发器（同 99 理由）。

## 3. down 决策：预检 fail-fast（两个候选均不采纳，理由）

- **候选 b（UPDATE 0→NULL 再重建 FK）链走不通**：sentinel 0 的 withdrawn 行改 NULL 直接违反 `ck_homework_submission_withdraw_audit`（withdrawn 态强制 withdrawn_by 非空）；published 回评改 NULL 违反 `ck_teacher_review_published`。UPDATE check_violation，不是可行的 down。
- **候选 a（直接重建 FK）在有 0 值行时失败**：`ADD CONSTRAINT ... FOREIGN KEY` 校验存量行，0 无对应 user → FK violation，且错误信息对运维不友好。
- **采纳：DO 块预检**——存在 sentinel 0 行（任一表）则 RAISE EXCEPTION 拒绝回滚（须先导出/处置，与 99 down 的「先导出再删」同口径）；无 0 值行时干净重建 97/98 同名 FK + 恢复原注释。该限制是审计不可变设计的必然：0 行本身就是必须保留的操作人痕迹。

## 4. 验证输出（moya_mig_test@4323）

```
UP1 OK / UP2_IDEMPOTENT_OK（NOTICE skipping 均幂等预期）
行为断言 /tmp/moya_mig/step100_behavior_test.sql → STEP100_SENTINEL_TESTS_PASSED（exit 0）
  T1  种子：withdrawn 行(withdrawn_by=996101) + published review(reviewer_uid=996102) 落值
      （withdrawn+published 并存被 98 互斥触发器正确拒绝——测试改双 submission 设计，守卫行为反证）
  T2  核心断言：DELETE 两操作人账号 → withdrawn_by/reviewer_uid 均保留原 uid（FK 时代被 SET NULL）
  T3  匿名化 UPDATE→0 成功（99 同款路径）
  T4  负数拒收：withdrawn_by=-1 / reviewer_uid=-1 各 check_violation
  T5  裸列语义：withdrawn_by=777777777（不存在账号）可插入
  T6  约束实态：两 FK 无、两 sentinel CHECK 在
down 矩阵：
  有 0 值行 → down EXIT=3 拒绝（"sentinel 0 audit rows exist"），事务回滚约束态不变
  清 0 行   → down OK；fk_hs_withdrawn_by/fk_tr_reviewer 恢复、sentinel CHECK 摘除
  up 复原   → UP_FINAL_OK，回 100 态收尾
全链 1→100（临时库 moya_r8_fullchain：6 扩展 + run_all_up.sh）→ EXIT=0，OK 99/99 文件，00000100 在列；抽检 100 态正确；临时库已 DROP
```

## 5. EUnit

```
$ erlc -I include -o test/ test/repo/moya_teaching_migration_tests.erl   # ERLC_OK
$ erl ... eunit:test([moya_teaching_migration_tests], [no_tty]) → RESULT: ok（EXIT=0）
verbose：21 passed / 0 failed（原 18 + 新 3）
  step100_up_drops_fk_adds_sentinel_check_test（摘 FK/无新 FK/CHECK 文本/禁置NULL /utf8）
  step100_down_failfast_guard_test（0 行预检两处/RAISE EXCEPTION/FK 同名重建/CHECK 摘除）
  step100_no_touch_history_test（up 无 DROP COLUMN/ALTER COLUMN/数据 UPDATE）
中文断言全部带 /utf8 修饰（R6 教训落实）。
```

make compile EXIT=0。

## 6. 历史迁移铁律取证

- mtime：95-99 十个文件全部停在 R5/R6 时点（18:26-20:16），本轮唯一新增为 00000100 两文件（23:08）。
- 95-97 工作区 vs 暂存区 `git diff --quiet`（只读）：全部 UNCHANGED；98 vs HEAD：CLEAN。
- 暂存区 `A` 状态为旧批次历史污染，未做任何 git 写操作。

## 7. 与并行 B 泳道的边界

- 未触碰 moya_boot_smoke 库与 9800/9801/9811 节点。
- B boot 时 auto-migrate 会把 100 应用到 boot 库——预期行为（boot 路径实测），未干预。
- 上层配套（绑定/撤回/回评 repo 的匿名化 UPDATE→0 调用点）属 B 泳道文件，本迁移只保证 schema 语义就绪：删除不再阻塞、痕迹保留、sentinel 写入路径开放。

## 8. 结论

00000100 结构/幂等/行为断言/down 矩阵/全链复跑/EUnit 六层验证全通过，**PASS**。
