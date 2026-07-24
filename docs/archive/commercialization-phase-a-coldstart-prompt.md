
  1. 读计划：.claude/PRPs/plans/commercialization-readiness.plan.md
     （XL 级蓝图：Phase A 止血 / B 安全 / C 交付 / D 商业包装 / E 丝滑E2EE双轨）
  2. 读上轮报告：.claude/PRPs/reports/commercialization-readiness-A-subset-report.md
  3. 读项目记忆：~/.claude/projects/-Users-leeyi-project-imboy-pub/memory/MEMORY.md
     重点：project_security_audit_corrections.md（密钥从未入库、勿重复"密钥在git历史"错误结论）

  【已完成】Phase A 安全子集（双语，全部可回滚）：
  - imboy/config/sys.local.config.example（密钥模板）
  - script/backup_pg.sh + restore_pg.sh + backup_garage.sh（语法验证通过）
  - docs/guides/operations/deployment/BACKUP-RESTORE.md（新建）
  - README.md / README.en.md：E2EE 默认禁用披露 + 百万并发降级为设计目标 + 表格
  - imboy/CLAUDE.md 根表格 + logic/lib 面包屑计数修正

  【关键事实，先验证勿盲信旧审查】
  - imboy 在 dev 分支；imboyapp 在 dev 分支
  - sys.local.config 已被 .gitignore（*local*.config）排除，从未入库——不需要 git 历史清洗
  - 本地配置仍含真实 secret（Garage/postgre_aes_key/千帆/SMTP/PG/Redis），但仅本地未泄露
  - E2EE 客户端 e2ee_settings.dart:25-30 isEnabled()=>false 硬编码，消息默认明文

  【本次任务，按投入产出排序，请先只做第 1 项并报告】
  1. 运行 codemap 自动化（codemap.yml/对应 make 目标）一次性重生成所有子目录
     src/*/CLAUDE.md 文件清单——api/adm/repo 的计数与文件列表都过期，手工改会引入
     新矛盾，必须用自动化。真实计数：api 54/adm 27/logic 76/ds 77/repo 72/lib 61
  2. BACKUP-RESTORE 真实演练：需 docker PG 容器(imboy_pg18)运行，跑 backup_pg.sh→
     restore_pg.sh --target imboy_restore_test，生成 docs/guides/operations/deployment/RESTORE-DRILL-2026-06.md
  3. A2：backend-ci.yml 改为全量 make eunit + make dialyze + OTP 28（注意可能暴露历史失败）

  【不要自动执行】
  - A1 密钥轮换（不可逆，需单独会话逐步人工确认）
  - Phase E PFS（数月级，先做 libsignal_protocol_dart 选型 PoC）
  - 任何 git commit/push、生产环境操作——除非我明确要求

  【约束】严格 Handler→Logic→DS→Repo 分层；不改 erlang.mk/ios/macos/r_upgrade；
  双语文档强制；遇密钥/git 类结论先 git ls-files/git log 验证再行动。
