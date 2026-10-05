# 企业 App Case Manifest（交全产品线 owner 执行）

> 来源：ews-20261004T065500Z 只读扫描（imboyapp main d17bb020）。本文件不执行任何 App 测试/设备操作。
> 平台要求：Android 真机 + macOS 桌面（同 shell，NavigationRail）。OA 入口仅 mobile。
> 继承：非OA计划 N2/N3 未闭合项 + final-closure FC-06（Android/macOS 各两轮）。

## 已闭合 vs 待原生重验（继承非OA PARTIAL 的开放项）

| 开放项（corrective-review） | 建议 case | 锚点测试 |
|---|---|---|
| 跨账号图读取 | 账号A/OrgA 组织图加载→切账号B/OrgB，断言不出现A节点/成员 | human_directory_access_revocation_test, organization_picker_page_widget_test |
| 确定性迟到响应 | 可控延迟 transport：A慢B快，旧请求/finally 不污染新状态 | human_directory_controller_test（原生重放） |
| 故障恢复 | 加载失败→重试成功；部分页失败不清空已有数据 | 同上 + organization_chart_widget_test |
| 管理员管理及直达 UI | Admin/Member 双角色原生登录，管理入口可见性 + 直达路由服务端拒绝 | organization_manage_page_widget_test, enterprise_shell_page_test:231 |
| 真实窗口+触控板 | macOS 真实窗口拖动/双指缩放/适应视图/折叠 | organization_chart（原生 N2-A02 原oracle） |

## 企业旅程点（16 项，见 scope-inventory app_surface_readonly）

1. 企业四栏进入与壳分发（workspace_shell_page.dart:48-57）
2. 切换企业 A→B（指针提交+默认WS落地+失败回滚 organization_picker_page.dart:250-282）
3. 切回个人四栏（_selectPersonal :302）
4. 企业消息 scope（群聊/频道分段、未读角标、换账号旧响应不覆盖）
5. 通讯录浏览（根→子部门→本级成员分页→成员详情含有权WS）
6. 搜索（300ms debounce ≥2字符）+我的部门 /directory/me
7. 组织架构图（按层展开、403 分型）
8. 部门治理（建/改名CAS 409/移动成环/归档/兼职/局部管理员）
9. 邀请旅程（token一次性/接受/拒绝/撤销/邀请码 981/982）
10. 成员生命周期（suspend/restore/offboard/角色/transfer_owner/移除）
11. 企业管理面角色门 + 归档 fail-closed
12. 我页企业身份（canGovern 入口 + 切换企业 tile）
13. OA 工作台授权→SSO→撤权清退（仅 Android）
14. macOS 桌面侧栏（OA 不读取）
15. 撤权退场（指针 forget、导航清退、无鉴权外请求）
16. 【缺口】auto_test specs 无企业端到端旅程 spec（仅 organization-home-device-preparation.json）——建议全产品线补 F09 系列

## 缺陷卡（本线只读发现，交 owner 裁决）

- **EWS-APP-D1（信息性）**：`OrganizationEntryCard` 在 lib 内无宿主引用、个人 ContactPage 无企业入口——个人通讯录→企业直入路径不存在。当前产品主路径=我页切换企业。若原 V2.1 §14.1 "ContactPage 企业入口" 仍是产品要求，则为 GAP_IMPLEMENTATION；若已被 mine 页路径替代，请 owner 出具替代决定以关闭继承 ORG/APP-01 P0 口径。
- **EWS-APP-D2（旅程缺口）**：auto_test specs 仅 1 条企业准备 spec，无企业四栏/切换/通讯录端到端旅程 spec；FC-06 两轮真机证据须由全产品线在其候选 SHA 上生成。
- **EWS-APP-D3（平台）**：macOS OA 入口为空（enterprise_shell_page.dart:81-89 仅 mobile），N3-A01 "企业工作台始终存在"在 macOS 语境=骨架+资源入口可见、无 OA 条目——请 owner 在视觉评审时确认此为预期裁剪。

## 证据消费边界

- 本 manifest 基于 d17bb020 只读扫描；任何 App PASS 须绑定全产品线候选 SHA 与真实设备指纹。
- 企业线不租用设备、不修改 App 源码；App 相关共享路径（app_router.dart、i18n、Token）改动须唯一 owner 登记。
