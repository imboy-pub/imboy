# 企业通讯录权限撤销与分页去重

App 候选 b8f171eee4639f7aeb990c3d90fd9f3ff88621af，基线 cf2573b9。复用既有企业通讯录、部门层级浏览、成员详情和 Human Directory 状态机，未新增导航或请求通道。

部门、成员、搜索和「我的部门」四个真实数据源调用入口，当前请求返回 OrganizationApiException 403/404 时，统一清空当前目录、搜索、我的部门及分页游标，递增 generation 作废同代在途请求，回到全屏错误状态。组织指针不擅自切换；用户可返回根级、切换企业或重新获权后手动重试。旧企业迟到的 403 先通过 generation/request_key 校验并被忽略，不能清空新企业。普通 500 网络错误保留成功数据与内联重试行为。错误只显示 API message，不再附加内部 code 数值。

_applyPage 追加模式改用 seen.add，一次性消除新页内部重复和前页重复；部门、成员、混排搜索共享修复，保持首次出现的原有顺序。

## 证据

- 先增加回归检查，在未修改产品代码时有 11 项失败：四个入口各自的 403/404 共八项，以及部门/成员/搜索三个同页重复；证明旧数据保留及去重缺陷。
- 修复后 56/56 Controller/Widget/API 合同测试通过；覆盖每个入口 403/404 清空、迟到兄弟响应不得补回、重新获权后的重试成功、旧企业 403 不影响新企业、三个分页节同页去重。
- Widget 层实证：刷新返回 403 后部门名和成员名从内容区撤下，展示服务端权限提示，不展示 code=403。既有账号切换、搜索 debounce、层级浏览、成员路由和两阶段企业切换检查继续通过。
- 定向 flutter analyze 零问题，格式、design token、gitleaks、conventional hooks 通过。初次新测试存在三处流程控制格式提示，已修复并重新验证，未压制检查。
- 人工复查四个 catch 中先校验请求代次，再处理权限拒绝；统一状态失效不触发新的自动请求或外向写操作。未执行子代理审查。

[源代码摘要与日志](evidence/directory-access-revocation-2026-10-01/sha256.json)。这些是可控 transport 与 Widget 回归证据，不声称真实设备、真实后端撤权旅程或生产资格。本轮未改后端权限规则，没有部署或生产数据操作。

完整六项目标仍未交付；坐席全旅程、企业治理与资料归属、OA/API 全面资格以及真机/外部/生产验收继续推进。

English summary: The existing Human Directory controller clears all current directory state on 403/404 from any of four endpoints and invalidates sibling requests. Stale errors cannot clear a newer organization; ordinary network failure retains existing data. Shared append deduplication now removes duplicate IDs within a new page. Eleven failing baseline regressions become green, and all 56 directory checks plus focused analysis pass. This is local client evidence, not device or complete production delivery.
