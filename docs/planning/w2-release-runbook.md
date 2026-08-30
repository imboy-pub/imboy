# W2 Release 蓝绿发布执行卡（alpha.71 候选；⛔ 待用户明确指令）

> 授权在册：2026-08-30 用户条件授权「如果有必要授权你蓝绿发布 imboy 到 prod」。
> 总控判定按门序 H2/H3 未收齐前**不执行**；本卡仅为执行准备——实际发布须用户明确下达，
> 并一并确认届时哪些人工门收齐/豁免。

## 一、前置清单（逐项勾选后方可执行）

- [ ] **H4**：三仓按 `w2-h4-commit-execution-plan.md` §五 push（imboy / imboyapp 普通推，admin `fetch` + `--force-with-lease`）；或明确选择 deploy.sh `-l` 本地 rsync 模式（不依赖 push）
- [ ] **版本物**：alpha.71 定版——VERSION 文件 + CHANGELOG `[Unreleased]` 转正 + 三仓 tag（tag 打制随 H4 授权）
- [ ] **生产备份**：`scripts/backup_pg.sh` + `backup_imboy_db.sh`（恢复路径已被 H3 预演实证）
- [ ] **H3**：生产库跑 `scripts/sanitized_snapshot.sh`（维护窗口）+ 演练复核，或用户明示豁免
- [ ] **密钥**：生产五密钥（jwt/postgre_aes/adm_cookie/solidified+iv/password_salt）经 `IMBOY_*` env 注入，不落任何文件
- [ ] **H2 残余**：双机收发 / 3 人真人 / JPush / LiveKit / release 签名——未收齐则本次发布定名「**alpha 发布**」，公告措辞遵守计划规则 10（不得声称正式 Release）

## 二、执行序列

```bash
# 0) 前置检查
bash deploy/preflight.sh

# 1) 蓝绿部署（服务器拉代码模式；-l = rsync 本地源码，无需 tag 已推）
bash scripts/deploy.sh <SERVER_HOST> <VSN> <NODE_NAME>
#    内部：检测运行色 → 部署对色 → expand 迁移 → 禁启动迁移起新节点
#         → 切流 + 排空旧节点长连接 → 显式执行完整迁移

# 2) 冒烟（每项按手册 §六 证据模板记录）
#    healthz 200；测试号登录全链；WS 连接；/init 校验 ws_url 派生值（发现①修复的线上验证点）

# 3) 观察窗口 ≥30min：错误率 / lager 日志 / 在线连接数；异常立即走 §三
```

## 三、回滚路径

```bash
# 应用回退（切回另一色，不回滚迁移）：
bash scripts/deploy.sh --rollback <SERVER_HOST> <VSN> <NODE_NAME>
# 迁移级回滚（确认需撤销 schema 才用；81 的 down 拒收 W2 事件行=安全特性）：
PGDATABASE=<prod> scripts/drill_migrate.escript down    # 81 → 80
```
预演基线（本地脱敏库）：down 0.27s / up 0.28s，锁窗口秒级；生产规模时长以实测为准并记台账。

## 四、证据归档

- 逐项按 `w2-zc12-manual-execution-handbook.md` §六 模板记录（含迁移时长/锁窗口/回滚演练行数对账）。
- 完成后判定：全 PASS 且无阻断缺陷 → `READY_FOR_ALPHA_RELEASE`；任一缺失 → `BLOCKED` + 精确缺口。

## 五、红线

- 生产 IP / SSH 端口**不入任何仓库文档**（记忆 imboy-pub-readme-server-info）。
- push 与远端选择归 H4，本卡不代行。
- 本卡不消除任何人工门——只把执行成本降为零。
