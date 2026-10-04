# W3 分域汇总报告（W4 冻结准备版）

> 版本 1.0 | 2026-10-04 | runner: ZCode（两计划验收协调）
> 依据：两计划交接合同 + `acceptance.tsv`（97-ID exact-set，98 行含 header）+
> `f00-group-batch-reconciliation.tsv`（group 域 271 行判定）+
> evidence/zcode-acceptance-20261004/ 全部 receipt 与校准注记。
> 合同不变量：**acceptance.tsv 全部 97 行保持 PENDING**——本报告只做证据就绪度
> 汇总，不翻转任何格状态；格翻转以 W4 冻结终审的正式判定为准。

---

## 1. 账本总貌

| 计划段 | ID 段 | 行数 | 格状态 | 证据就绪度 |
|---|---|---|---|---|
| E2EE | AC-00…AC-35 | 36 | 全 PENDING | W1 已交付设备侧 E2EE 旅程与备份域证据（w2-journeys-calibration.json 等）；AC-19/20/21、AC-30/31、AC-32/33、AC-26 被 D-02…D-07 决策阻塞 |
| 全产品 | F00-A01…（F00…F17） | 61 | 全 PENDING | group 域（F00）证据最厚（见 §3）；contact 域高风险流程已落地（7c7e0ae7 等）；其余域按 W2 校准注记在案 |

## 2. W3 已执行（本会话累计提交）

**imboy 仓**（receipt/注记链）：36d1be64（邀请链+notice 契约）、66ae4ab6（moment ACL 缺陷修复+6/6 矩阵）、f62c7614、79efef49、e9053461、33ff2842、f00 校准注记五连（f2f/add_member/batch2/batch3/batch4）、57e29427。
**imboyapp 仓**（测试与修复）：4d08d51e、c50bcfa、5423a917、4c4bfd5d、4bc0603b、d05578b9、ce797c02、1899a5c7、a95f0e4b、f313fbe7、e8a06394。

## 3. group 域（F00）逐页收敛表

判定 TSV 共 271 行：67 COVERED_BY_EXISTING_JOURNEY / 25 PARTIAL / 91 TARGET_MISSING_WIDGET_LEVEL / 70 TARGET_MISSING_NEW_CASE_REQUIRED / 18 TARGET_MISSING_PLATFORM_SENSITIVE。

| 页面/线 | 判定行数 | 已收敛（校准注记登记） | 证据形态 |
|---|---|---|---|
| face_to_face_page + confirm_page（f2f 设备线） | 21 | 14 行映射；真机 9 PASS / 1 skip-by-design（GF6）/ 1 FAIL-ENV（GF4） | 真机集成测试 + DB 断言（j-f2f-*.log ×13） |
| add_member_page | 11 | 9 行（:001/003/004/005/006/007 未含——实收敛 :001,003,004,005,006,008,010,011 共 8 行 + :002 部分注记） | widget 套件 9 用例（4bc0603b→a95f0e4b） |
| remove_member_page | 10 | **10/10 全收敛**（:009 toast 文案部分留注） | widget 套件 7 用例（a95f0e4b） |
| launch_chat_page | 12 | 4 行（:005/006/009/011） | widget 套件 4 用例（ce797c02） |
| group_select_page | 8 | 3 行（:001 空态分支/:002/:004） | widget 套件 2 用例（d05578b9） |
| group_vote_page | 12 | 3 行（:002 前半/:003/:005） | widget 套件 3 用例（f313fbe7） |
| contact 域（f00-contact TSV 86 行） | 86 | **漂移核对 19 行**（W2 套件/纯函数单测/assistant_plaza provider 单测早于判定）+ **新闭 8 行**（tag 页 :002/:006/:007、people_info_more :002/:003、assistant_plaza :004/:006/:010） | contact 三套件 20 用例（7c7e0ae7→ab306ddf）+ f00-contact-batch-calibration.json |
| group_task / task_detail / schedule_detail / file / album_photo / album_photo_detail / vote_detail | 各 11-12 | 0（FIND-GROUP-07 阻塞） | — |
| 其余（group_tag/category/notice 等） | — | 未启动批量 | — |

> 上表为页内计数；跨注记的同一 fn 行（如 add_member:010 空态同时被设备线与 widget 线覆盖）不重复计。

## 4. 证据索引（evidence/zcode-acceptance-20261004/）

**校准注记（7 份，伴生文件，不翻格）**：
1. `acceptance-calibration-notes.json`（W2：contact/fn009 校准）
2. `w2-journeys-calibration.json`（W2 六旅程）
3. `f00-f2f-device-calibration.json`（f2f 设备线 14 行映射）
4. `f00-add-member-widget-calibration.json`（6 行）
5. `f00-group-widget-calibration-batch2.json`（5 行：add_member :002/:003/:009 + group_select :004/:002）
6. `f00-launch-chat-widget-calibration.json`（4 行）
7. `f00-group-widget-calibration-batch3.json`（11 行：add_member:011 + remove_member 10 行）
8. `f00-group-widget-calibration-batch4.json`（vote 3 行 + FIND-GROUP-07 受阻定性）
9. `f00-contact-batch-calibration.json`（contact 域：漂移核对 19 行 + 新闭 8 行 + harness 规则）

**判定/对账 TSV**：f00-group-batch-reconciliation.{tsv,json}、f00-contact-batch-reconciliation.{tsv,json}、f00-group-domain-reconciliation-start.json。
**关键 receipt**：moment-acl-first-closure.json（FIND-MOMENT-01 修复+6/6 矩阵）、group-integration-first-batch.json、w3-second-batch-group-contracts.json、moment-acl-device-journey-v1.json（run6 全绿）、find-group-04-carrier-parameterization.json、f2f-suite-gf1-gf11-device-results.json。

**测试套件（imboyapp，10 套件 70+ 用例）**：
- widget：`test/unit_test/page/group/{add_member,group_select,launch_chat}_widget_flow_test.dart`、`group_detail/remove_member_widget_flow_test.dart`、`vote/group_vote_widget_flow_test.dart`（合计 31 用例，dart analyze 全清）
- contact：`test/unit_test/page/contact/{confirm_new_friend,contact_setting_tag}_widget_flow_test.dart`、`contact_setting_destructive_flow_test.dart`、`assistant_plaza/assistant_plaza_widget_flow_test.dart`、`contact_page_search_test.dart`、`assistant_plaza/assistant_plaza_provider_test.dart`
- 集成（真机）：`integration_test/moment/moment_visibility_device_test.dart`、`integration_test/group/face_to_face_acceptance_test.dart`（单场景 --plain-name 跑法）

## 5. 阻断与待决清单

| 编号 | 内容 | 状态 |
|---|---|---|
| GF4 | 真机 3 秒节流窗口 vs 定位耗时（f2f :008 场景） | FAIL-ENV 留证；需服务端测试钩子或更快定位路径 |
| GF6 | NetworkMonitor 无测试注入通道（f2f :009） | 设计内 SKIP |
| FIND-GROUP-07 | 群应用页数据依赖行（vote×8、task/schedule/file/album/vote_detail 各 11-12 行）被 service 单例不可注入阻塞 | 解阻选项 (a) 产品侧 @visibleForTesting 注入钩子（**待批准**）或 (b) 真机逐行 |
| D-02…D-07 | MLS/PQ/见证/SLO/信任根/条件 AC | **等用户逐项拍板**；阻塞 AC-19/20/21、AC-30/31、AC-32/33、AC-26 |
| RES-L3-RERUN | 冻结候选成立后执行 | 未开始 |
| RES-MITM | NOT_AUTHORIZED | 等明确批准 |

## 6. W4 冻结前置条件（gap list）

1. D-02…D-07 逐项拍板 → 启动对应 AC 门；
2. FIND-GROUP-07 解阻决策（选项 a/b）→ 群应用页数据依赖行收敛；
3. GF4 方案定夺（服务端测试钩子 or 接受 FAIL-ENV 终审定性）；
4. contact 域与其余 F 域（F01…F17）按同一 TSV 模式补齐批量证据；
5. 上述收敛后执行 RES-L3-RERUN（L3 全量回归）→ 进入 W4 冻结终审，届时按判定逐格翻转 acceptance.tsv 并记 reviewer/completed_at。
