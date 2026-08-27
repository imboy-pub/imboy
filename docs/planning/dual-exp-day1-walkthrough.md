# 双体验 v2.5.2 — 首日体验走查报告（WP8/T15 Day-1 Bar）

> 模式：**全自动无人值守（unsafe_experiment）**。UI/真机/真人理解测试不可自动化执行的部分**如实标 BLOCKED**，不伪造截图、不把 API 级证据说成 UI 证据。

## 1. 走查清单与取证状态

| # | 旅程步骤（计划 T15②） | 取证方式 | 状态 |
|---|---|---|---|
| 1 | 注册新账号 | Demo B 脚本 P0 步骤（API） | ✅ API 级 |
| 2 | Template 建站（Workspace+Owner+Announcements+General 原子） | Demo B P1/P1b（含幂等重试） | ✅ API 级 + DB 核查 |
| 3 | Workspace Member 三角色（邀请/角色/转移/Guest 只读） | WP3 workspace_logic_tests 16 用例矩阵 + Demo B P2 | ✅ 测试+API 级；UI 走查 BLOCKED |
| 4 | 显式 Group Member / Channel Subscriber（三关系独立、部分接受、可重试） | Demo B P2b-P2d + DB 核查（未自动入群/订阅断言） | ✅ API+DB 级；Flutter 单测 47 用例（ invite_controller_test 三关系独立）佐证 UI 逻辑 |
| 5 | 子集约束（Group⊆Workspace 两档强制） | DB 触发器实测（T3 报告）+ group_member_workspace_subset_tests + Demo B 移除/重邀流程 | ✅ |
| 6 | Scope Contract now 能力闭环（Tasks 四态等） | Demo B P5/P5b（创建→指派→流转→回退→done） | ✅ API 级 |
| 7 | 归档/恢复联动（980 拒写、读取保留、personal 不受影响、恢复放行） | Demo B P7-P7e 全链 | ✅ API 级 |
| 8 | Chat 回归（登录→会话→消息→附件） | R5 矩阵既有自动化子集 + T13 对账数字 | ✅ 测试级；真机 BLOCKED |
| 9 | 音视频通话 | 需两台真机+人工操作 | **BLOCKED** |
| 10 | Push 推送（厂商通道真机端到端） | 需真机 | **BLOCKED** |
| 11 | 升级提示链路（app_version 记录存在性） | 只读查询本地库 app_version 表 vsn 与客户端版本比较 | 见 §3 |
| 12 | 30 秒理解测试（4 个术语差异） | 需真实用户 3 人 | **BLOCKED** |

## 2. 阻断性缺陷

走查过程中发现并修复的缺陷（全部闭环后 Demo B 才达成 ALL PASS）：

| # | 缺陷 | 定级 | 修复 |
|---|---|---|---|
| D1 | Template 创建成功但响应构造 case 错配 with_tx 裸值契约 + 事务内回读未用 Conn 读到空行 → HTTP 500 且幂等键占用 | **CRITICAL** | workspace_ds:create_template 改裸值守卫 + 同事务 Conn 回读（新增防回归用例）|
| D2 | resolver 便捷门把 ensure_member 的 {ok,Role} 直接透传 → workspace 资源合法成员访问全体 case_clause 500 | **CRITICAL** | ensure_member_ok 归一化 + 3 用例回归 |
| D3 | elib_pg_sql:update/3 不存在（真实签名 update/4）→ task 流转 500 | **CRITICAL** | project_task/project/workspace 三 repo 四调用点改正确签名 |
| D4 | remove_member_checked/archive/restore/admin_* 五处同款 {ok,_} 错配 → 移除与归档恢复 500 | **HIGH** | 全部改直通契约匹配 |
| D5 | 非法流转错误消息含 Unicode 箭头在响应编码层崩成 500 | LOW | 消息改 ASCII 箭头（400 语义不变） |
| D6 | demo 脚本自身断言口径 4 处修正（HTTP码 vs envelope码、软删订阅行、表名、参数缺失） | — | 脚本侧 |

修复后 T14 双遍连续 **ALL PASS（22 步 54 断言 ×2，transcript 见 run1/run2 存档）**。
新登记已知限制：频道创建后首帖存在角色读缓存竞态窗口（重试成功；详见 known-limitations §E4）。

## 3. 升级提示链路检查（只读）

本地库 `app_version` 表存在且含 3 条 status=1 记录（CN/android/com.imboy.app：vsn 1.0.1/1.0.2/1.0.3，min_supported_vsn=0.0.0，grayscale_percent=100）——后台存在 ≥ 客户端版本的 `app_version` 记录这一前置成立，升级提示不会因 vsn 落后而恒 none。真机端到端弹出验证 BLOCKED。未改动任何表内容（运维决策属人工）。

## 4. 30 秒理解测试设计（供人工补做）

问题集（W0 口径，比计划原文缩减了 Project Member 问项）：
1. 工作区成员和群成员有什么区别？（答：前者是组织访问边界 Owner/Member/Guest，后者只管某个群的聊天）
2. 频道订阅者是什么？（答：关注某频道内容更新的人）
3. 加入工作区会自动进群吗？（答：不会——必须显式加入 General/订阅 Announcements）
4. 为什么用 Project 不用群聊跟进度？（答：任务不会沉底，"做完没"一眼看清）

判定：全部答对=通过；无人值守下未执行 → 计划 §9.3 自检第 6 条与 Gate 0 商业验证一并由人工补测。

## 5. 结论

API 层首日旅程全程闭环（注册受限走演示账号降级路径的说明见 known-limitations F）；
阻断性缺陷 5 项（D1-D5）全部修复并带回归测试；
UI 层走查、真机媒体链路、真人理解测试三项 BLOCKED——按 §9.4 由 Gate 1 前的人工验收补齐。
**Day-1 Bar 结论：API 层达标；完整 Day-1 Bar 未闭环（BLOCKED 项如实披露）。**
