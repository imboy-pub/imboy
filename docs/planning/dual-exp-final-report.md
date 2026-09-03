# IMBoy 双体验架构实施 — 最终统一汇报（unsafe_experiment）

> **历史快照说明（2026-09-03）**：§一至§七记录 2026-08-26 至 27 日的 W0 实施，不代表当前 main 的 W2 能力和工作树状态；当前增量见 §八。
>
> 计划：`.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md` v2.5.2
> 执行模式：**全自动无人值守（unsafe_experiment）**——Gate 0 / Gate W 由用户 goal 授权跳过人工确认；
> 本报告一切结论为 **local / rehearsal 级**，≠客户验收 ≠Release ≠工程 DoD。
> 执行窗口：2026-08-26 19:27 → 2026-08-27（分三段） | 分支：三仓 `dual-exp-v21`（未 push、未合并 main）

---

## 一、任务总表 ✅/❌

| 阶段/工作包 | 任务 | 状态 | 交付锚点 |
|---|---|---|---|
| Preflight 0 | 三仓基线/worktree 隔离/合并责任人 | ✅ | imboy `docs/planning/dual-exp-preflight-baseline.md` |
| WP0 | Decision Brief（R1-R6）+ W0 自动代行 | ✅ | 84c5ec2d · brief 六节+Gate W 十二项 Scope Contract |
| WP1 | T1 Product Experience 服务端 | ✅ | f7f668a0 · product_experience.erl + /api/v1/init 白名单字段 + eunit 9/9 |
| WP1 | T2 ChatShell 纯包壳 | ✅ | worktree e6a71caf · experience_provider+降级矩阵测试 64/64 |
| WP2 | T3 迁移批次（3 对文件 10 逻辑变更，W0 裁剪） | ✅ | 6dc78492 · 库 version=78；up/down 演练；发现 erlang_migrate 部分 down 缺陷（上游库） |
| WP3 | T4 workspace_logic/API + T5 scope 边界 | ✅ | 3073ef4a · 新测 40 绿；Template 原子/幂等有故障注入证据 |
| WP4 | T6a/T6b project+task + T7 归档守卫 + 路由整合 | ✅ | c132da43 · 守卫接入矩阵逐条入册；并发线性化 PG 实证 2/2 |
| WP5 | T8/T9/T12 WorkspaceShell+视图+Branding | ✅ | worktree e807e694 · 47 测试绿；Channel 无聊天输入框断言 |
| WP7 | T11/T11b Admin 两页+后端 admin 端点 | ✅ | imboy 15826dd9 + admin 68396eb · bun test 1374/0；鉴权 fail-closed 测试 |
| WP6 | T10a/T10b Project 详情+Tasks 视图（W0 裁剪） | ✅ | a84d76bd · 37 测试绿；defer 无占位 UI 有断言 |
| WP8 | T13 对账 + T14 Demo B ×2 + T15 三报告 | ✅ | ff648951 + 79bc2ee8 · Demo B **双遍 ALL PASS（22 步 54 断言×2）** |
| V1 review | 三仓代码质量（独立会话） | ✅ | HIGH×1+MEDIUM×3+LOW×6，无 CRITICAL |
| V3 review | 安全专项（独立会话） | ✅ | HIGH×2+MEDIUM×1+LOW×2；正面取证 7 组全过 |
| 修复批 | HIGH 全清零 + MEDIUM 关键项 | ✅ | 9078b4c5（6 文件）；HIGH×3 与 join 980 守卫见下文 |
| V2 验收 | 最终验收（第三个独立会话） | ✅ | b8b93466 `dual-exp-acceptance.md` · **ACCEPTED(local/rehearsal)** |
| 治理批 | 验收 F-N1/N3 落实 | ✅ | 5a33f754 · transcript 换 ALL PASS 源；当前 main 移除共享库名称白名单断言 |
| 归档+验收尾批 | review 报告入仓 + worktree 回归修复 | ✅ | c6bc0444 + worktree 807d330c |
| 契约同步 | api_contract.json(+195 行新路由) + error_code.dart(980) 双仓再生；Contract Gate PASS | ✅ | e8a375d3 + worktree c72ab6b5 |
| 守卫域回归复跑 | subset/boundary/scope 三组套件全绿（join 980 改动域） | ✅ | HEAD e8a375d3 实测 |
| 上游修复 | erlang_migrate 分支 fix/partial-down-tracking：down(N)/goto 不再清空升级历史；110/110 绿含 3 新钉子用例（未 push，imboy 切换需 bump deps pin） | ✅ | 0054908 |

❌ 未完成 / BLOCKED 清单：
1. **真机 Demo A/B 的 UI+媒体链路**（两台真机不可得）——API 层全链替代取证；
2. **生产规模迁移窗口终判**（无生产授权，R2 已 BLOCKED 声明）；
3. **生产 Docker/Helm 受控重启演练**、升级提示真机端到端、30 秒真人理解测试——T15 如实 BLOCKED。

## 二、验收结果摘要

`dual-exp-acceptance.md`（V2 独立取证）：**ACCEPTED（local/rehearsal 级）**。
- §9.2 P0：✅15 / ⚠️1（能部署=本机干净库+重建 release 多轮冷启动实录，生产编排 BLOCKED）/ N/A(W0)1
- §9.3 八条自检全部书面回答（①Chat 兼容 ②双体验可运行零第二实现 ③单配置切换 ④Scope Contract 无偷建 ⑤Archive tx 线性化 ⑥四元语义无混用(理解测试人工补) ⑦W0 成员深度一致 ⑧首日缺陷闭环）
- §9.4：拿得出手✅ / 能让别人真实使用✅(API 层) / 首日缺陷✅ / 能部署⚠️降级
- 安全探针 13 步全过：非成员 403、子集约束三层证据、归档后写 980×4+join 980（修复后）、恢复放行、personal 对照
- 后端关键子集复跑全绿（w0 断言 5/5、并发 2/2、template 5/5 等）；Flutter 子集 +189/0

## 三、变更清单（三仓 dual-exp-v21 分支）

**imboy**（10 提交计划内 + 用户 docs 提交，diff main...dual-exp-v21 ≈89 文件 +12.4k/-442 起）：
e29f6fdf preflight → 84c5ec2d WP0 → f7f668a0 T1 → 6dc78492 T3 → 3073ef4a T4+T5 → c132da43 T6a/b+T7 → 15826dd9 T11b → ff648951 WP8 → 79bc2ee8 w0 断言 → 9078b4c5 review 修复 → b8b93466 acceptance → 5a33f754 治理批

**imboyapp worktree**（62 文件 +10.5k）：e6a71caf T2 → e807e694 WP5 →（merge main b80634b7，用户并行推进了 main）→ a84d76bd WP6 → 807d330c 验收轮修复（a11y Cupertino 图标×3 + route_registry 补登记 WP6 四路由，smoke 53/0 绿）

**imboyadmin**（15 文件 +1714）：68396eb T11+T11b

## 四、执行中抓获并修复的真缺陷（全部为单元测试 mock 盲区，仅端到端可见）

| # | 缺陷 | 定级 |
|---|---|---|
| D1 | create_template 响应 case 错配 with_tx 直通契约 + 事务外回读空行 → Template 创建 500 且幂等键占用 | CRITICAL |
| D2 | resolver 便捷门透传 {ok,Role} → workspace 资源**合法成员**访问全体 500（403 反而正常，无越权泄漏） | CRITICAL |
| D3 | elib_pg_sql:update/3 不存在（真实签名 /4），project/workspace/task 四 repo 调用点全雷 → 任务流转等 500 | CRITICAL |
| D4 | remove/archive/restore/admin_archive/admin_restore 五处 {ok,_} 错配 → 移除与归档恢复 500 | HIGH |
| D5 | 归档后 join_group 无 980 守卫（V3-F3）→ 补 DS 层同事务守卫+handler 翻译 | HIGH |
| D6 | invite 响应 atom status 键与 binary "status" 键冲突产生重复 JSON 键（V1-F1） | HIGH |

根因共性：WP3/WP4 单元测试 mock 了 repo/with_tx 边界，**Demo B 端到端是这些缺陷的唯一暴露面**——验证了计划坚持 T14 Golden Demo 的价值。change_role Owner 并发窗口已于 2026-09-03 由 `d4cbe404` 结构性修复；频道首帖 E4 已确认由创建者角色兜底覆盖并补充发布回归测试。

## 五、已知限制（Top，全文见 dual-exp-known-limitations.md A-F 节）
- Workspace 数据库写最终入口已事务化；对象存储上传与 PostgreSQL 归档事务之间仍可能留下未引用对象
- erlang_migrate 上游缺陷：部分 down 后 version tracking 清空，必须 force 校正（B1）
- 4 个部分索引需发布窗口；生产行数量级取证 BLOCKED（B3/D）
- Flutter 基线红灯随上游 main 演进扩大至 ~165 条（归属实验证实与本分支无关；dual-exp 命名空间 265 用例全绿）（C1）
- 后端基线 59 失败 + 本计划新增 0（对账子集与命名空间归属分析见 known-limitations C2 与最终 T13 记录）（C2）
- license 社区版限制 signup（Demo 用演示账号降级路径并已写明）（F）
- CI 三仓镜像大面积红灯既有问题，未在本计划处理（C3）

## 六、无人值守模式的两笔代价（用户开局知悉项的兑现记录）
1. **Gate 0 被跳过**：全部工程投入建立在假设上。缓解已做：Scope Contract 按 W0 最小重量收敛（省掉 member/milestone/pinned/resources/activity 五大件），Gate 1 付费 PoC 前不扩张。
2. **Recon 无人复核**：R2/R3 结论质量经后续波次实证良好（25 条写路径穷举在 T7 全部对上；EXPLAIN 结论被 V2 复核）。错误地基风险实际上转移到了 Wave 编排层并被 Demo B/V2 双层拦截。

## 七、下一步建议（人工决策）
1. **复核并签认**：Gate W W0 决策表（decision-brief 末尾四问）、acceptance 报告；推翻任何一项则按文档声明的范围作废重签。
2. 三仓 `dual-exp-v21` 是否 push / 合回 main：由用户拍板（红线约束未 push）。
3. 生产前必办：生产行数画像补 R2 判据 → 发布窗口决策；erlang_migrate 上游 issue；CI 红灯专项。
4. Gate 1 商业验证按 §9.5：同一试点环境跑付费 PoC（建议场景见 acceptance 报告第⑧条：归档留档+周报流）。

## 八、2026-09-03 当前 main 修复补记

- `197ff0cb`：邀请目标成员改为发送 Announcements 频道邀请，不再误订阅操作者；
- `d4cbe404`：最后 Owner 角色变更事务内串行化；
- `07312d0c`：频道/群公告派生已读写入事务化；
- `b175879e`：Personal/Workspace 改称首页布局，并支持恢复部署默认值；
- `eb9dae04`：Workspace 全局会话统一标记为“全部消息”；
- `8feee84d`、`49e58b76`：无工作区和加载失败均可返回 Personal，创建/加入入口闭环。

以上均为本地代码与自动测试证据，不解除真机、真人理解、生产迁移或发布验收门禁。
