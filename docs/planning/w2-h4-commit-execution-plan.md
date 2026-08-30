# W2 H4 提交执行清单（仅剩 push 阶段待授权）

> 生成：2026-08-29 | **2026-08-30 刷新：§一~§三 commit 序列已全部执行完毕**（其间另有 ZC-08、发现①②③④等修复与台账多笔提交，现状见 §五），**仅剩 §五 push 终版待用户指定远端后执行**；§四保留作历史。
> **执行前置（H4 人工确认项）**：① git author/committer 身份 ② 逐仓目标远端 ③ push 批次单独授权。
> 顺序遵循计划固定合并顺序：DB → Backend Domain → Contract → Flutter/Admin → Tests/Demo → Docs/Version。

## 一、imboy 后端（9 笔，HEAD=db41789a，45 项未提交）

```bash
cd /Users/leeyi/project/imboy.pub/imboy

# 1. ZC-01 W2 schema
git add priv/migrations/00000081_project_w2_foundation.up.sql \
        priv/migrations/00000081_project_w2_foundation.down.sql \
        test/integration/w2_schema_contract_tests.erl \
        test/integration/w0_schema_contract_tests.erl
git commit -m "feat(db): add W2 project membership and channel-firstclass schema"

# 2. ZC-02 成员管理
git add src/api/project_member_handler.erl src/logic/project_member_logic.erl \
        src/ds/project_member_ds.erl src/repo/project_member_repo.erl \
        test/api/project_member_handler_tests.erl test/logic/project_member_logic_tests.erl \
        test/ds/project_member_ds_tests.erl test/repo/project_member_repo_tests.erl \
        test/integration/project_member_concurrency_tests.erl
git commit -m "feat(project): W2 project member management with workspace-subset authorization"

# 3. ZC-03 里程碑
git add src/api/project_milestone_handler.erl src/logic/project_milestone_logic.erl \
        src/ds/project_milestone_ds.erl src/repo/project_milestone_repo.erl \
        test/api/project_milestone_handler_tests.erl test/logic/project_milestone_logic_tests.erl \
        test/ds/project_milestone_ds_tests.erl test/repo/project_milestone_repo_tests.erl \
        test/integration/project_milestone_integration_tests.erl \
        test/integration/project_milestone_concurrency_tests.erl
git commit -m "feat(project): W2 milestone CRUD + planned->reached state machine with same-tx events"

# 4. ZC-04 关联与聚合
git add src/api/project_channel_handler.erl src/logic/project_channel_logic.erl \
        src/ds/project_channel_ds.erl src/repo/project_channel_rel_repo.erl \
        src/repo/project_channel_agg_repo.erl \
        test/logic/project_channel_logic_tests.erl \
        test/integration/project_channel_rel_integration_tests.erl \
        test/integration/project_channel_agg_integration_tests.erl
git commit -m "feat(project): channel association + pinned/resources/activity/related-posts aggregations"

# 5. ZC-05 整合（含 ZC-09R 的 src 侧修复同文件）
git add src/imboy_router.erl src/ds/project_ds.erl src/repo/project_member_repo.erl \
        src/repo/project_milestone_repo.erl src/repo/project_channel_rel_repo.erl \
        src/logic/project_member_logic.erl src/logic/project_milestone_logic.erl \
        src/logic/project_channel_logic.erl src/adm/adm_workspace_handler.erl \
        .contract/api_contract.json
git commit -m "feat(project): integrate W2 REST surface, owner auto-join and admin read APIs"

# 6. ZC-08 Demo 演练
git add scripts/demo/dual_exp_demo_b_w2.sh \
        docs/planning/dual-exp-demo-b-w2-rehearsal.md \
        docs/planning/dual-exp-demo-b-w2-transcripts.md
git commit -m "test(demo): add W2 Demo B rehearsal script with fixed 66 assertions and prefix teardown"

# 7. ZC-09 独立审查报告
git add docs/planning/w2-backend-review-2026-08-29.md
git commit -m "docs(review): W2 backend independent security and quality review"

# 8. ZC-11 发布文档与版本
git add VERSION CHANGELOG.md \
        docs/planning/w2-alpha-release-acceptance-2026-08-29.md \
        docs/planning/w2-zc12-manual-execution-handbook.md \
        docs/planning/channel-firstclass-w2-execution-ledger.md \
        docs/planning/imboy-channel-firstclass-w2-alpha-release-execution-plan.md
git commit -m "docs(release): prepare alpha.70 acceptance evidence, W2 changelog and manual handbook"

# 复核
git log --oneline -8 && git status --short
```

> 注：ledger 建议执行计划文档（imboy-channel-firstclass-w2-alpha-release-execution-plan.md）随第 8 笔入库；若您倾向不将计划/手册入公共仓，从第 8 笔 pathspec 中移除即可。

## 二、imboyapp（2 笔，HEAD=a18e89e5，29 项未提交；⚠️ 26 文件在 index-staged 状态）

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp

# 1. ZC-06 W2 功能全量（staged 状态可直接 commit；route_registry 未 staged 需一并加入）
git add lib/ assets/i18n/ test/unit_test/
git commit -m "feat(workspace): W2 project collaboration — members/milestones/channels/aggregations (ZC-06)"

# 2. ZC-11 版本与错误码再生
git add pubspec.yaml lib/config/error_code.dart
git commit -m "chore(release): bump 1.0.0-alpha.16+6; regen error_code from contract (ZC-11)"

git log --oneline -2 && git status --short
```

## 三、imboyadmin（2 笔 + 1 清理决策，HEAD=46c7e10）

```bash
cd /Users/leeyi/project/imboy.pub/imboyadmin

# 0.（决策项）E2E evidence 副产物 233 项——建议不入库：
#    确认 .gitignore 覆盖 tests/auto_test/evidence/，或手动清理
git add src/services/api/workspaces.ts src/services/api/workspaces.test.ts \
        src/pages/workspaces/ProjectDetailPage.tsx src/pages/workspaces/ProjectDetailPage.test.tsx
git commit -m "feat(admin): W2 project governance read-only panels on ProjectDetailPage (ZC-07)"

git add package.json
git commit -m "chore(release): bump 1.0.0-alpha.16 (ZC-11)"

git log --oneline -2 && git status --short | head -5
```

## 四、push 批次（第三重授权，与 commit 分离）

```bash
# 逐仓逐远端，由用户明确指定后执行；示例（以 gitee origin 为例）：
# git -C imboy push origin main
# git -C imboyapp push origin main
# git -C imboyadmin push origin main
# tag（可选）：
# git -C imboy tag v1.0.0-alpha.70 && git -C imboy push origin v1.0.0-alpha.70
# git -C imboyapp tag v1.0.0-alpha.16 && git -C imboyapp push origin v1.0.0-alpha.16
# git -C imboyadmin tag v1.0.0-alpha.16 && git -C imboyadmin push origin v1.0.0-alpha.16
# ⚠️ 三仓历史领先提交（156/99/13+）将随 main 一并推送——push 前请知悉。
```

---

## 五、push 终版（2026-08-30 刷新；§一~§四为历史记录）

### 5.1 现状

| 仓 | HEAD | 领先/分叉 | tag（均未推） | push 方式 |
|---|---|---|---|---|
| imboy | `d8b98253` | 领先 origin/main **180** | `v1.0.0-alpha.70`→`0e7618d5` | 普通 push |
| imboyapp | `55cb1021` | 领先 origin/main **102** | `v1.0.0-alpha.16`→`122652b4` | 普通 push |
| imboyadmin | `9911a28`（历史已重写：evidence 全清，`.git` 165M→1.7M） | 与三远端完全分叉（旧 `1088e7b`） | `v1.0.0-alpha.16`=`9911a28`（重写后） | **必须 --force** |

admin 重写前完整备份：工作区 `.Codex/backups/imboyadmin-pre-evidence-purge-20260830.bundle`（134MB，含全部旧历史与 9 tag）。

### 5.2 命令（`<remote>` 由用户指定：`origin`(gitee) / `github` / `gitcode`）

```bash
# imboy / imboyapp：普通 push + tag
git -C imboy    push <remote> main && git -C imboy    push <remote> v1.0.0-alpha.70
git -C imboyapp push <remote> main && git -C imboyapp push <remote> v1.0.0-alpha.16

# imboyadmin：先 fetch 重建 tracking 引用，再 force-with-lease（比裸 --force 安全）
git -C imboyadmin fetch <remote>
git -C imboyadmin push --force-with-lease <remote> main
git -C imboyadmin push --force-with-lease <remote> v1.0.0-alpha.16
```

### 5.3 push 后验证

1. 每仓 `git log -1` 确认远端 HEAD 与 5.1 表一致；tag `git ls-remote --tags <remote>` 到位。
2. 全新 clone 冒烟：admin clone 体积降至个位数 MB，且 `git ls-files | grep -c tests/auto_test/evidence` = **0**。
3. admin 另外两个远端是否同步 force-push 由用户决定——不同步则各远端历史不一致（gitee 旧历史含 evidence 仍公开可见）。
4. push 将触发各远端已配置的 CI。

### 5.4 不变的前置（H4 人工门）

① git author/committer 身份人工确认 ② 逐仓远端指定 ③ push 批次单独授权（admin 的 force 属历史覆盖，须明确知悉）。**H2/H3 未收齐前 push≠Release**（计划规则 10）。
